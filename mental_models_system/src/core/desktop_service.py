import os
import sys
import json
import logging
import asyncio
import signal
import threading
import time
import platform
import subprocess
from pathlib import Path
from dataclasses import dataclass, field
from typing import Dict, Any, Optional, List, Callable
from datetime import datetime
from enum import Enum
import socket
import uuid

logger = logging.getLogger(__name__)


class ServiceStatus(Enum):
    STOPPED = "stopped"
    STARTING = "starting"
    RUNNING = "running"
    STOPPING = "stopping"
    ERROR = "error"


@dataclass
class ServiceConfig:
    service_name: str = "MentalModelsService"
    display_name: str = "Mental Models System"
    description: str = "24/7 Mental Models Analysis Service"
    auto_start: bool = True
    restart_on_failure: bool = True
    max_restart_attempts: int = 5
    restart_delay_seconds: float = 5.0
    api_port: int = 8001
    web_sync_url: str = ""
    watch_directories: List[str] = field(default_factory=list)
    lm_studio_url: str = "http://localhost:1234"
    log_file: str = ""
    pid_file: str = ""
    data_dir: str = ""


@dataclass
class ServiceState:
    status: ServiceStatus = ServiceStatus.STOPPED
    started_at: Optional[datetime] = None
    uptime_seconds: float = 0.0
    restart_count: int = 0
    last_error: str = ""
    tasks_processed: int = 0
    files_processed: int = 0
    active_watchers: int = 0
    connected_to_web: bool = False
    lm_studio_connected: bool = False


class DesktopService:
    def __init__(self, config: ServiceConfig = None):
        self.config = config or ServiceConfig()
        self.state = ServiceState()
        self._running = False
        self._shutdown_event = asyncio.Event()
        self._components: Dict[str, Any] = {}
        self._callbacks: Dict[str, List[Callable]] = {
            "on_start": [],
            "on_stop": [],
            "on_error": [],
            "on_file_processed": [],
            "on_task_completed": [],
        }
        
        self._setup_paths()
        self._setup_logging()

    def _setup_paths(self):
        if not self.config.data_dir:
            self.config.data_dir = os.path.expanduser("~/.mental_models")
        
        Path(self.config.data_dir).mkdir(parents=True, exist_ok=True)
        
        if not self.config.log_file:
            self.config.log_file = os.path.join(self.config.data_dir, "service.log")
        
        if not self.config.pid_file:
            self.config.pid_file = os.path.join(self.config.data_dir, "service.pid")

    def _setup_logging(self):
        log_dir = os.path.dirname(self.config.log_file)
        if log_dir:
            Path(log_dir).mkdir(parents=True, exist_ok=True)
        
        file_handler = logging.FileHandler(self.config.log_file)
        file_handler.setLevel(logging.INFO)
        file_handler.setFormatter(logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        ))
        
        root_logger = logging.getLogger()
        root_logger.addHandler(file_handler)

    def _write_pid_file(self):
        with open(self.config.pid_file, 'w') as f:
            f.write(str(os.getpid()))

    def _remove_pid_file(self):
        if os.path.exists(self.config.pid_file):
            os.remove(self.config.pid_file)

    def _is_already_running(self) -> bool:
        if not os.path.exists(self.config.pid_file):
            return False
        
        try:
            with open(self.config.pid_file, 'r') as f:
                pid = int(f.read().strip())
            
            os.kill(pid, 0)
            return True
        except (OSError, ValueError):
            self._remove_pid_file()
            return False

    def add_callback(self, event: str, callback: Callable):
        if event in self._callbacks:
            self._callbacks[event].append(callback)

    def _trigger_callbacks(self, event: str, *args, **kwargs):
        for callback in self._callbacks.get(event, []):
            try:
                callback(*args, **kwargs)
            except Exception as e:
                logger.error(f"Callback error for {event}: {e}")

    async def _check_lm_studio_connection(self) -> bool:
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(
                    f"{self.config.lm_studio_url}/v1/models",
                    timeout=aiohttp.ClientTimeout(total=5)
                ) as response:
                    return response.status == 200
        except Exception:
            return False

    async def _sync_with_web(self) -> bool:
        if not self.config.web_sync_url:
            return False
        
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                payload = {
                    "node_id": self._get_node_id(),
                    "hostname": socket.gethostname(),
                    "status": self.state.status.value,
                    "uptime": self.state.uptime_seconds,
                    "tasks_processed": self.state.tasks_processed,
                    "files_processed": self.state.files_processed,
                }
                
                async with session.post(
                    f"{self.config.web_sync_url}/api/nodes/heartbeat",
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=10)
                ) as response:
                    return response.status == 200
        except Exception as e:
            logger.debug(f"Web sync failed: {e}")
            return False

    def _get_node_id(self) -> str:
        node_id_file = os.path.join(self.config.data_dir, "node_id")
        
        if os.path.exists(node_id_file):
            with open(node_id_file, 'r') as f:
                return f.read().strip()
        
        node_id = f"node-{uuid.uuid4().hex[:12]}"
        with open(node_id_file, 'w') as f:
            f.write(node_id)
        
        return node_id

    async def _main_loop(self):
        logger.info("Main service loop started")
        
        while self._running:
            try:
                self.state.lm_studio_connected = await self._check_lm_studio_connection()
                self.state.connected_to_web = await self._sync_with_web()
                
                if self.state.started_at:
                    self.state.uptime_seconds = (datetime.now() - self.state.started_at).total_seconds()
                
                await asyncio.sleep(30)
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Main loop error: {e}")
                self.state.last_error = str(e)
                await asyncio.sleep(5)

    async def _start_api_server(self):
        try:
            from ..api.main import app
            import uvicorn
            
            config = uvicorn.Config(
                app,
                host="0.0.0.0",
                port=self.config.api_port,
                log_level="info",
            )
            server = uvicorn.Server(config)
            self._components["api_server"] = server
            
            await server.serve()
            
        except Exception as e:
            logger.error(f"API server failed: {e}")
            self.state.last_error = str(e)

    async def _start_file_watchers(self):
        if not self.config.watch_directories:
            return
        
        try:
            from ..ingestion.batch_processor import DistributedBatchProcessor, ProgressTracker
            
            tracker = ProgressTracker()
            processor = DistributedBatchProcessor(
                tracker=tracker,
                max_workers=4,
            )
            
            for directory in self.config.watch_directories:
                if os.path.exists(directory):
                    processor.add_directory(directory)
                    self.state.active_watchers += 1
            
            processor.start()
            self._components["batch_processor"] = processor
            
            logger.info(f"Started watching {self.state.active_watchers} directories")
            
        except Exception as e:
            logger.error(f"File watcher failed: {e}")
            self.state.last_error = str(e)

    async def start(self):
        if self._is_already_running():
            logger.error("Service is already running")
            return False
        
        self.state.status = ServiceStatus.STARTING
        self._running = True
        self._write_pid_file()
        
        try:
            self.state.started_at = datetime.now()
            self.state.status = ServiceStatus.RUNNING
            
            self._trigger_callbacks("on_start")
            logger.info(f"Service started: {self.config.display_name}")
            
            tasks = [
                asyncio.create_task(self._main_loop()),
                asyncio.create_task(self._start_file_watchers()),
            ]
            
            await asyncio.gather(*tasks, return_exceptions=True)
            
        except Exception as e:
            self.state.status = ServiceStatus.ERROR
            self.state.last_error = str(e)
            self._trigger_callbacks("on_error", e)
            logger.error(f"Service error: {e}")
            return False
        
        finally:
            self._remove_pid_file()
        
        return True

    async def stop(self):
        logger.info("Stopping service...")
        self.state.status = ServiceStatus.STOPPING
        self._running = False
        
        if "batch_processor" in self._components:
            self._components["batch_processor"].stop()
        
        if "api_server" in self._components:
            self._components["api_server"].should_exit = True
        
        self.state.status = ServiceStatus.STOPPED
        self._trigger_callbacks("on_stop")
        self._remove_pid_file()
        
        logger.info("Service stopped")

    def get_status(self) -> Dict[str, Any]:
        return {
            "service_name": self.config.service_name,
            "display_name": self.config.display_name,
            "status": self.state.status.value,
            "started_at": self.state.started_at.isoformat() if self.state.started_at else None,
            "uptime_seconds": self.state.uptime_seconds,
            "restart_count": self.state.restart_count,
            "last_error": self.state.last_error,
            "tasks_processed": self.state.tasks_processed,
            "files_processed": self.state.files_processed,
            "active_watchers": self.state.active_watchers,
            "connected_to_web": self.state.connected_to_web,
            "lm_studio_connected": self.state.lm_studio_connected,
            "node_id": self._get_node_id(),
            "hostname": socket.gethostname(),
            "platform": platform.system(),
        }


class ServiceInstaller:
    def __init__(self, config: ServiceConfig):
        self.config = config
        self.system = platform.system()

    def install(self) -> bool:
        if self.system == "Windows":
            return self._install_windows()
        elif self.system == "Darwin":
            return self._install_macos()
        elif self.system == "Linux":
            return self._install_linux()
        else:
            logger.error(f"Unsupported platform: {self.system}")
            return False

    def uninstall(self) -> bool:
        if self.system == "Windows":
            return self._uninstall_windows()
        elif self.system == "Darwin":
            return self._uninstall_macos()
        elif self.system == "Linux":
            return self._uninstall_linux()
        else:
            return False

    def _install_windows(self) -> bool:
        try:
            script_path = os.path.abspath(sys.argv[0])
            python_path = sys.executable
            
            startup_dir = os.path.join(
                os.environ.get("APPDATA", ""),
                "Microsoft", "Windows", "Start Menu", "Programs", "Startup"
            )
            
            shortcut_path = os.path.join(startup_dir, f"{self.config.service_name}.bat")
            
            with open(shortcut_path, 'w') as f:
                f.write(f'@echo off\n')
                f.write(f'start /min "" "{python_path}" "{script_path}" --service\n')
            
            logger.info(f"Windows startup script created: {shortcut_path}")
            return True
            
        except Exception as e:
            logger.error(f"Windows installation failed: {e}")
            return False

    def _uninstall_windows(self) -> bool:
        try:
            startup_dir = os.path.join(
                os.environ.get("APPDATA", ""),
                "Microsoft", "Windows", "Start Menu", "Programs", "Startup"
            )
            shortcut_path = os.path.join(startup_dir, f"{self.config.service_name}.bat")
            
            if os.path.exists(shortcut_path):
                os.remove(shortcut_path)
            
            return True
        except Exception as e:
            logger.error(f"Windows uninstallation failed: {e}")
            return False

    def _install_macos(self) -> bool:
        try:
            script_path = os.path.abspath(sys.argv[0])
            python_path = sys.executable
            
            plist_content = f'''<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.mentalmodels.{self.config.service_name}</string>
    <key>ProgramArguments</key>
    <array>
        <string>{python_path}</string>
        <string>{script_path}</string>
        <string>--service</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>StandardOutPath</key>
    <string>{self.config.log_file}</string>
    <key>StandardErrorPath</key>
    <string>{self.config.log_file}</string>
</dict>
</plist>'''
            
            launch_agents_dir = os.path.expanduser("~/Library/LaunchAgents")
            os.makedirs(launch_agents_dir, exist_ok=True)
            
            plist_path = os.path.join(
                launch_agents_dir,
                f"com.mentalmodels.{self.config.service_name}.plist"
            )
            
            with open(plist_path, 'w') as f:
                f.write(plist_content)
            
            subprocess.run(["launchctl", "load", plist_path], check=True)
            
            logger.info(f"macOS LaunchAgent created: {plist_path}")
            return True
            
        except Exception as e:
            logger.error(f"macOS installation failed: {e}")
            return False

    def _uninstall_macos(self) -> bool:
        try:
            plist_path = os.path.expanduser(
                f"~/Library/LaunchAgents/com.mentalmodels.{self.config.service_name}.plist"
            )
            
            if os.path.exists(plist_path):
                subprocess.run(["launchctl", "unload", plist_path], check=False)
                os.remove(plist_path)
            
            return True
        except Exception as e:
            logger.error(f"macOS uninstallation failed: {e}")
            return False

    def _install_linux(self) -> bool:
        try:
            script_path = os.path.abspath(sys.argv[0])
            python_path = sys.executable
            
            service_content = f'''[Unit]
Description={self.config.description}
After=network.target

[Service]
Type=simple
ExecStart={python_path} {script_path} --service
Restart=always
RestartSec={self.config.restart_delay_seconds}
User={os.environ.get("USER", "root")}
WorkingDirectory={os.path.dirname(script_path)}
Environment=PYTHONUNBUFFERED=1

[Install]
WantedBy=multi-user.target
'''
            
            user_systemd_dir = os.path.expanduser("~/.config/systemd/user")
            os.makedirs(user_systemd_dir, exist_ok=True)
            
            service_path = os.path.join(
                user_systemd_dir,
                f"{self.config.service_name}.service"
            )
            
            with open(service_path, 'w') as f:
                f.write(service_content)
            
            subprocess.run(["systemctl", "--user", "daemon-reload"], check=True)
            subprocess.run(["systemctl", "--user", "enable", self.config.service_name], check=True)
            subprocess.run(["systemctl", "--user", "start", self.config.service_name], check=True)
            
            logger.info(f"Linux systemd service created: {service_path}")
            return True
            
        except Exception as e:
            logger.error(f"Linux installation failed: {e}")
            return False

    def _uninstall_linux(self) -> bool:
        try:
            subprocess.run(
                ["systemctl", "--user", "stop", self.config.service_name],
                check=False
            )
            subprocess.run(
                ["systemctl", "--user", "disable", self.config.service_name],
                check=False
            )
            
            service_path = os.path.expanduser(
                f"~/.config/systemd/user/{self.config.service_name}.service"
            )
            
            if os.path.exists(service_path):
                os.remove(service_path)
            
            subprocess.run(["systemctl", "--user", "daemon-reload"], check=False)
            
            return True
        except Exception as e:
            logger.error(f"Linux uninstallation failed: {e}")
            return False


class ServiceController:
    def __init__(self, config: ServiceConfig = None):
        self.config = config or ServiceConfig()
        self.system = platform.system()

    def start(self) -> bool:
        if self.system == "Linux":
            result = subprocess.run(
                ["systemctl", "--user", "start", self.config.service_name],
                capture_output=True
            )
            return result.returncode == 0
        elif self.system == "Darwin":
            plist_path = os.path.expanduser(
                f"~/Library/LaunchAgents/com.mentalmodels.{self.config.service_name}.plist"
            )
            result = subprocess.run(["launchctl", "load", plist_path], capture_output=True)
            return result.returncode == 0
        else:
            return False

    def stop(self) -> bool:
        if self.system == "Linux":
            result = subprocess.run(
                ["systemctl", "--user", "stop", self.config.service_name],
                capture_output=True
            )
            return result.returncode == 0
        elif self.system == "Darwin":
            plist_path = os.path.expanduser(
                f"~/Library/LaunchAgents/com.mentalmodels.{self.config.service_name}.plist"
            )
            result = subprocess.run(["launchctl", "unload", plist_path], capture_output=True)
            return result.returncode == 0
        else:
            return False

    def restart(self) -> bool:
        self.stop()
        time.sleep(2)
        return self.start()

    def status(self) -> Dict[str, Any]:
        if self.system == "Linux":
            result = subprocess.run(
                ["systemctl", "--user", "status", self.config.service_name],
                capture_output=True,
                text=True
            )
            return {
                "running": "active (running)" in result.stdout,
                "output": result.stdout,
            }
        elif self.system == "Darwin":
            result = subprocess.run(
                ["launchctl", "list"],
                capture_output=True,
                text=True
            )
            service_name = f"com.mentalmodels.{self.config.service_name}"
            return {
                "running": service_name in result.stdout,
                "output": result.stdout,
            }
        else:
            return {"running": False, "output": "Unsupported platform"}


def create_cli():
    import argparse
    
    parser = argparse.ArgumentParser(description="Mental Models Desktop Service")
    parser.add_argument("--service", action="store_true", help="Run as service")
    parser.add_argument("--install", action="store_true", help="Install service")
    parser.add_argument("--uninstall", action="store_true", help="Uninstall service")
    parser.add_argument("--start", action="store_true", help="Start service")
    parser.add_argument("--stop", action="store_true", help="Stop service")
    parser.add_argument("--restart", action="store_true", help="Restart service")
    parser.add_argument("--status", action="store_true", help="Show service status")
    parser.add_argument("--port", type=int, default=8001, help="API port")
    parser.add_argument("--watch", nargs="+", help="Directories to watch")
    parser.add_argument("--web-sync", type=str, help="Web sync URL")
    parser.add_argument("--lm-studio", type=str, default="http://localhost:1234", help="LM Studio URL")
    
    return parser


async def main():
    parser = create_cli()
    args = parser.parse_args()
    
    config = ServiceConfig(
        api_port=args.port,
        watch_directories=args.watch or [],
        web_sync_url=args.web_sync or "",
        lm_studio_url=args.lm_studio,
    )
    
    if args.install:
        installer = ServiceInstaller(config)
        success = installer.install()
        print("Service installed" if success else "Installation failed")
        return
    
    if args.uninstall:
        installer = ServiceInstaller(config)
        success = installer.uninstall()
        print("Service uninstalled" if success else "Uninstallation failed")
        return
    
    controller = ServiceController(config)
    
    if args.start:
        success = controller.start()
        print("Service started" if success else "Start failed")
        return
    
    if args.stop:
        success = controller.stop()
        print("Service stopped" if success else "Stop failed")
        return
    
    if args.restart:
        success = controller.restart()
        print("Service restarted" if success else "Restart failed")
        return
    
    if args.status:
        status = controller.status()
        print(json.dumps(status, indent=2))
        return
    
    if args.service:
        service = DesktopService(config)
        
        def signal_handler(sig, frame):
            asyncio.create_task(service.stop())
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)
        
        await service.start()


if __name__ == "__main__":
    asyncio.run(main())
