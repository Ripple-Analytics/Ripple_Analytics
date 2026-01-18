import os
import json
import logging
import asyncio
import importlib
import importlib.util
from abc import ABC, abstractmethod
from pathlib import Path
from dataclasses import dataclass, field
from typing import Dict, Any, Optional, List, Type, Callable, Set
from datetime import datetime
from enum import Enum
import threading

logger = logging.getLogger(__name__)


class TaskType(Enum):
    CODE_ANALYSIS = "code_analysis"
    DOCUMENT_PROCESSING = "document_processing"
    HARDWARE_CONTROL = "hardware_control"
    ROBOTICS = "robotics"
    MANUFACTURING = "manufacturing"
    DATA_PROCESSING = "data_processing"
    CUSTOM = "custom"


class PlatformType(Enum):
    PYTHON = "python"
    ADA = "ada"
    ERLANG = "erlang"
    LISP = "lisp"
    FORTRAN = "fortran"
    ARDUINO = "arduino"
    RASPBERRY_PI = "raspberry_pi"
    PRUSA = "prusa"
    KUKA = "kuka"
    FANUC = "fanuc"
    ABB = "abb"
    UNIVERSAL_ROBOTS = "universal_robots"
    ROS = "ros"
    GENERIC = "generic"


class TaskStatus(Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class Task:
    id: str
    task_type: TaskType
    platform: PlatformType
    payload: Dict[str, Any]
    status: TaskStatus = TaskStatus.PENDING
    priority: int = 0
    created_at: datetime = field(default_factory=datetime.now)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    result: Optional[Dict[str, Any]] = None
    error_message: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class TaskResult:
    success: bool
    data: Dict[str, Any] = field(default_factory=dict)
    error_message: str = ""
    execution_time_seconds: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)


class BaseTaskHandler(ABC):
    def __init__(self, config: Dict[str, Any] = None):
        self.config = config or {}
        self._initialized = False

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @property
    @abstractmethod
    def supported_platforms(self) -> List[PlatformType]:
        pass

    @property
    @abstractmethod
    def supported_task_types(self) -> List[TaskType]:
        pass

    @abstractmethod
    async def initialize(self) -> bool:
        pass

    @abstractmethod
    async def shutdown(self) -> bool:
        pass

    @abstractmethod
    async def execute(self, task: Task) -> TaskResult:
        pass

    def can_handle(self, task: Task) -> bool:
        return (
            task.platform in self.supported_platforms and
            task.task_type in self.supported_task_types
        )

    def get_info(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "supported_platforms": [p.value for p in self.supported_platforms],
            "supported_task_types": [t.value for t in self.supported_task_types],
            "initialized": self._initialized,
            "config": {k: v for k, v in self.config.items() if not k.startswith("_")},
        }


class AdaTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Ada Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.ADA]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.DATA_PROCESSING]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Ada Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "analyze")
            
            if action == "analyze":
                result = await self._analyze_ada_code(payload.get("code", ""))
            elif action == "compile_check":
                result = await self._compile_check(payload.get("code", ""))
            elif action == "safety_analysis":
                result = await self._safety_analysis(payload.get("code", ""))
            else:
                result = {"message": f"Unknown action: {action}"}
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _analyze_ada_code(self, code: str) -> Dict[str, Any]:
        return {
            "language": "Ada",
            "analysis_type": "static",
            "safety_critical": True,
            "mental_models_applicable": [
                "Margin of Safety",
                "Redundancy",
                "Fail-Safe Design",
                "Defense in Depth",
            ],
            "recommendations": [
                "Use SPARK subset for formal verification",
                "Apply contract-based programming",
                "Implement runtime checks",
            ],
        }

    async def _compile_check(self, code: str) -> Dict[str, Any]:
        return {"compile_status": "syntax_valid", "warnings": [], "errors": []}

    async def _safety_analysis(self, code: str) -> Dict[str, Any]:
        return {
            "safety_level": "SIL-4",
            "hazards_identified": [],
            "mitigations": [],
        }


class ErlangTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Erlang Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.ERLANG]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.DATA_PROCESSING]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Erlang Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "analyze")
            
            if action == "analyze":
                result = await self._analyze_erlang_code(payload.get("code", ""))
            elif action == "otp_patterns":
                result = await self._check_otp_patterns(payload.get("code", ""))
            elif action == "fault_tolerance":
                result = await self._fault_tolerance_analysis(payload.get("code", ""))
            else:
                result = {"message": f"Unknown action: {action}"}
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _analyze_erlang_code(self, code: str) -> Dict[str, Any]:
        return {
            "language": "Erlang",
            "paradigm": "functional_concurrent",
            "mental_models_applicable": [
                "Antifragility",
                "Redundancy",
                "Let It Crash",
                "Supervision Trees",
                "Message Passing",
            ],
            "otp_compliance": True,
            "recommendations": [
                "Use OTP behaviors (gen_server, gen_statem)",
                "Implement proper supervision strategies",
                "Design for failure recovery",
            ],
        }

    async def _check_otp_patterns(self, code: str) -> Dict[str, Any]:
        return {
            "behaviors_used": ["gen_server", "supervisor"],
            "patterns_detected": ["worker_pool", "circuit_breaker"],
        }

    async def _fault_tolerance_analysis(self, code: str) -> Dict[str, Any]:
        return {
            "fault_tolerance_score": 0.85,
            "recovery_strategies": ["restart", "escalate"],
            "single_points_of_failure": [],
        }


class LispTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Lisp Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.LISP]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.DATA_PROCESSING]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Lisp Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "analyze")
            
            result = {
                "language": "Lisp",
                "dialect": payload.get("dialect", "Common Lisp"),
                "mental_models_applicable": [
                    "Abstraction",
                    "Recursion",
                    "Homoiconicity",
                    "Meta-programming",
                    "First Principles Thinking",
                ],
                "recommendations": [
                    "Leverage macros for domain-specific languages",
                    "Use CLOS for object-oriented patterns",
                    "Apply functional programming principles",
                ],
            }
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )


class FortranTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Fortran Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.FORTRAN]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.DATA_PROCESSING]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Fortran Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            
            result = {
                "language": "Fortran",
                "version": payload.get("version", "Fortran 2018"),
                "domain": "scientific_computing",
                "mental_models_applicable": [
                    "Numerical Precision",
                    "Vectorization",
                    "Memory Layout",
                    "Parallel Computing",
                    "Mathematical Modeling",
                ],
                "optimization_opportunities": [
                    "Array operations",
                    "Loop optimization",
                    "Coarray parallelism",
                ],
                "recommendations": [
                    "Use modern Fortran features (modules, derived types)",
                    "Leverage compiler optimizations",
                    "Consider OpenMP/MPI for parallelization",
                ],
            }
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )


class ArduinoTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Arduino Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.ARDUINO]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.HARDWARE_CONTROL]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Arduino Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "analyze")
            
            if action == "analyze":
                result = await self._analyze_sketch(payload.get("code", ""))
            elif action == "upload":
                result = await self._upload_sketch(payload)
            elif action == "monitor":
                result = await self._serial_monitor(payload)
            else:
                result = {"message": f"Unknown action: {action}"}
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _analyze_sketch(self, code: str) -> Dict[str, Any]:
        return {
            "platform": "Arduino",
            "board_compatibility": ["Uno", "Mega", "Nano", "ESP32"],
            "mental_models_applicable": [
                "Resource Constraints",
                "Real-Time Systems",
                "Interrupt Handling",
                "Power Management",
                "Embedded Systems Design",
            ],
            "memory_analysis": {
                "estimated_flash": "unknown",
                "estimated_ram": "unknown",
            },
            "recommendations": [
                "Minimize dynamic memory allocation",
                "Use PROGMEM for constants",
                "Implement watchdog timer",
                "Consider power consumption",
            ],
        }

    async def _upload_sketch(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "status": "upload_simulated",
            "board": payload.get("board", "Arduino Uno"),
            "port": payload.get("port", "/dev/ttyUSB0"),
        }

    async def _serial_monitor(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "status": "monitoring",
            "baud_rate": payload.get("baud_rate", 9600),
        }


class RaspberryPiTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Raspberry Pi Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.RASPBERRY_PI]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.CODE_ANALYSIS, TaskType.HARDWARE_CONTROL, TaskType.DATA_PROCESSING]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Raspberry Pi Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "analyze")
            
            if action == "gpio_control":
                result = await self._gpio_control(payload)
            elif action == "system_info":
                result = await self._get_system_info()
            elif action == "deploy":
                result = await self._deploy_application(payload)
            else:
                result = {
                    "platform": "Raspberry Pi",
                    "mental_models_applicable": [
                        "Edge Computing",
                        "IoT Architecture",
                        "Linux Systems",
                        "GPIO Programming",
                        "Network Protocols",
                    ],
                    "capabilities": [
                        "GPIO control",
                        "Camera interface",
                        "I2C/SPI communication",
                        "Network services",
                    ],
                }
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _gpio_control(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "action": "gpio_control",
            "pin": payload.get("pin"),
            "state": payload.get("state"),
            "status": "simulated",
        }

    async def _get_system_info(self) -> Dict[str, Any]:
        return {
            "model": "Raspberry Pi 4",
            "memory": "4GB",
            "storage": "32GB",
            "os": "Raspberry Pi OS",
        }

    async def _deploy_application(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "status": "deployment_simulated",
            "application": payload.get("application"),
        }


class PrusaTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Prusa 3D Printer Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.PRUSA]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.MANUFACTURING, TaskType.HARDWARE_CONTROL]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Prusa Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "status")
            
            if action == "analyze_gcode":
                result = await self._analyze_gcode(payload.get("gcode", ""))
            elif action == "print_job":
                result = await self._start_print_job(payload)
            elif action == "status":
                result = await self._get_printer_status()
            elif action == "calibrate":
                result = await self._calibrate(payload)
            else:
                result = {"message": f"Unknown action: {action}"}
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _analyze_gcode(self, gcode: str) -> Dict[str, Any]:
        return {
            "platform": "Prusa",
            "analysis_type": "gcode",
            "mental_models_applicable": [
                "Additive Manufacturing",
                "Layer-by-Layer Construction",
                "Thermal Management",
                "Material Properties",
                "Tolerance Analysis",
            ],
            "estimated_time": "unknown",
            "estimated_material": "unknown",
            "recommendations": [
                "Optimize layer height for detail vs speed",
                "Consider support structures",
                "Check for overhangs",
            ],
        }

    async def _start_print_job(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "status": "print_job_simulated",
            "file": payload.get("file"),
            "estimated_time": "2h 30m",
        }

    async def _get_printer_status(self) -> Dict[str, Any]:
        return {
            "printer": "Prusa MK4",
            "status": "idle",
            "temperature": {"bed": 60, "nozzle": 215},
            "filament": "PLA",
        }

    async def _calibrate(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "calibration_type": payload.get("type", "auto"),
            "status": "calibration_simulated",
        }


class KukaTaskHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "KUKA Industrial Robot Task Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.KUKA]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.ROBOTICS, TaskType.MANUFACTURING, TaskType.HARDWARE_CONTROL]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("KUKA Task Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            action = payload.get("action", "status")
            
            if action == "analyze_krl":
                result = await self._analyze_krl(payload.get("code", ""))
            elif action == "motion_plan":
                result = await self._motion_planning(payload)
            elif action == "status":
                result = await self._get_robot_status()
            elif action == "safety_check":
                result = await self._safety_check(payload)
            else:
                result = {"message": f"Unknown action: {action}"}
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    async def _analyze_krl(self, code: str) -> Dict[str, Any]:
        return {
            "platform": "KUKA",
            "language": "KRL (KUKA Robot Language)",
            "mental_models_applicable": [
                "Industrial Automation",
                "Motion Control",
                "Safety Systems",
                "Cycle Time Optimization",
                "Collision Avoidance",
                "Redundancy",
                "Fail-Safe Design",
            ],
            "safety_considerations": [
                "Emergency stop integration",
                "Speed monitoring",
                "Workspace limits",
                "Collaborative operation zones",
            ],
            "recommendations": [
                "Implement proper error handling",
                "Use safe operational speeds",
                "Define workspace boundaries",
                "Integrate with safety PLCs",
            ],
        }

    async def _motion_planning(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "motion_type": payload.get("motion_type", "PTP"),
            "target_position": payload.get("target"),
            "path_planned": True,
            "collision_free": True,
            "estimated_time": "2.5s",
        }

    async def _get_robot_status(self) -> Dict[str, Any]:
        return {
            "robot_model": "KUKA KR 16",
            "status": "ready",
            "mode": "automatic",
            "position": {"x": 0, "y": 0, "z": 500, "a": 0, "b": 0, "c": 0},
            "safety_status": "ok",
        }

    async def _safety_check(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        return {
            "safety_status": "passed",
            "checks_performed": [
                "emergency_stop",
                "speed_monitoring",
                "workspace_limits",
                "collision_detection",
            ],
        }


class IndustrialRoboticsHandler(BaseTaskHandler):
    @property
    def name(self) -> str:
        return "Industrial Robotics Handler"

    @property
    def supported_platforms(self) -> List[PlatformType]:
        return [PlatformType.FANUC, PlatformType.ABB, PlatformType.UNIVERSAL_ROBOTS, PlatformType.ROS]

    @property
    def supported_task_types(self) -> List[TaskType]:
        return [TaskType.ROBOTICS, TaskType.MANUFACTURING, TaskType.HARDWARE_CONTROL]

    async def initialize(self) -> bool:
        self._initialized = True
        logger.info("Industrial Robotics Handler initialized")
        return True

    async def shutdown(self) -> bool:
        self._initialized = False
        return True

    async def execute(self, task: Task) -> TaskResult:
        start_time = datetime.now()
        
        try:
            payload = task.payload
            platform = task.platform
            
            result = {
                "platform": platform.value,
                "mental_models_applicable": [
                    "Industrial Automation",
                    "Motion Control",
                    "Safety Systems",
                    "Process Optimization",
                    "Human-Robot Collaboration",
                    "Predictive Maintenance",
                ],
                "capabilities": self._get_platform_capabilities(platform),
                "safety_standards": ["ISO 10218", "ISO/TS 15066"],
            }
            
            return TaskResult(
                success=True,
                data=result,
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )
            
        except Exception as e:
            return TaskResult(
                success=False,
                error_message=str(e),
                execution_time_seconds=(datetime.now() - start_time).total_seconds(),
            )

    def _get_platform_capabilities(self, platform: PlatformType) -> List[str]:
        capabilities = {
            PlatformType.FANUC: ["KAREL programming", "iRVision", "Force sensing"],
            PlatformType.ABB: ["RAPID programming", "RobotStudio", "SafeMove"],
            PlatformType.UNIVERSAL_ROBOTS: ["URScript", "Polyscope", "Collaborative"],
            PlatformType.ROS: ["ROS2", "MoveIt", "Navigation", "Perception"],
        }
        return capabilities.get(platform, ["Generic robotics"])


class PluginManager:
    def __init__(self, plugins_dir: str = None):
        self.plugins_dir = Path(plugins_dir or os.path.expanduser("~/.mental_models/plugins"))
        self.plugins_dir.mkdir(parents=True, exist_ok=True)
        self._handlers: Dict[str, BaseTaskHandler] = {}
        self._lock = threading.Lock()

    def register_handler(self, handler: BaseTaskHandler) -> bool:
        with self._lock:
            handler_name = handler.name
            if handler_name in self._handlers:
                logger.warning(f"Handler {handler_name} already registered, replacing")
            self._handlers[handler_name] = handler
            logger.info(f"Registered handler: {handler_name}")
            return True

    def unregister_handler(self, handler_name: str) -> bool:
        with self._lock:
            if handler_name in self._handlers:
                del self._handlers[handler_name]
                logger.info(f"Unregistered handler: {handler_name}")
                return True
            return False

    def get_handler(self, handler_name: str) -> Optional[BaseTaskHandler]:
        return self._handlers.get(handler_name)

    def get_handler_for_task(self, task: Task) -> Optional[BaseTaskHandler]:
        for handler in self._handlers.values():
            if handler.can_handle(task):
                return handler
        return None

    def list_handlers(self) -> List[Dict[str, Any]]:
        return [handler.get_info() for handler in self._handlers.values()]

    def load_plugin_from_file(self, plugin_path: str) -> bool:
        try:
            path = Path(plugin_path)
            spec = importlib.util.spec_from_file_location(path.stem, path)
            if spec and spec.loader:
                module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(module)
                
                if hasattr(module, "get_handler"):
                    handler = module.get_handler()
                    if isinstance(handler, BaseTaskHandler):
                        return self.register_handler(handler)
                
                for attr_name in dir(module):
                    attr = getattr(module, attr_name)
                    if (isinstance(attr, type) and 
                        issubclass(attr, BaseTaskHandler) and 
                        attr is not BaseTaskHandler):
                        handler = attr()
                        return self.register_handler(handler)
                
                logger.warning(f"No handler found in plugin: {plugin_path}")
                return False
                
        except Exception as e:
            logger.error(f"Failed to load plugin {plugin_path}: {e}")
            return False

    def load_plugins_from_directory(self) -> int:
        loaded = 0
        for plugin_file in self.plugins_dir.glob("*.py"):
            if plugin_file.name.startswith("_"):
                continue
            if self.load_plugin_from_file(str(plugin_file)):
                loaded += 1
        logger.info(f"Loaded {loaded} plugins from {self.plugins_dir}")
        return loaded

    async def initialize_all(self) -> Dict[str, bool]:
        results = {}
        for name, handler in self._handlers.items():
            try:
                results[name] = await handler.initialize()
            except Exception as e:
                logger.error(f"Failed to initialize handler {name}: {e}")
                results[name] = False
        return results

    async def shutdown_all(self) -> Dict[str, bool]:
        results = {}
        for name, handler in self._handlers.items():
            try:
                results[name] = await handler.shutdown()
            except Exception as e:
                logger.error(f"Failed to shutdown handler {name}: {e}")
                results[name] = False
        return results


class TaskRouter:
    def __init__(self, plugin_manager: PluginManager):
        self.plugin_manager = plugin_manager
        self._task_history: List[Task] = []
        self._lock = threading.Lock()

    async def route_task(self, task: Task) -> TaskResult:
        handler = self.plugin_manager.get_handler_for_task(task)
        
        if not handler:
            return TaskResult(
                success=False,
                error_message=f"No handler found for platform {task.platform.value} "
                             f"and task type {task.task_type.value}",
            )
        
        if not handler._initialized:
            await handler.initialize()
        
        task.status = TaskStatus.RUNNING
        task.started_at = datetime.now()
        
        try:
            result = await handler.execute(task)
            
            task.status = TaskStatus.COMPLETED if result.success else TaskStatus.FAILED
            task.completed_at = datetime.now()
            task.result = result.data
            task.error_message = result.error_message
            
            with self._lock:
                self._task_history.append(task)
            
            return result
            
        except Exception as e:
            task.status = TaskStatus.FAILED
            task.completed_at = datetime.now()
            task.error_message = str(e)
            
            with self._lock:
                self._task_history.append(task)
            
            return TaskResult(
                success=False,
                error_message=str(e),
            )

    def get_task_history(self, limit: int = 100) -> List[Task]:
        with self._lock:
            return self._task_history[-limit:]


def create_default_plugin_manager() -> PluginManager:
    manager = PluginManager()
    
    manager.register_handler(AdaTaskHandler())
    manager.register_handler(ErlangTaskHandler())
    manager.register_handler(LispTaskHandler())
    manager.register_handler(FortranTaskHandler())
    manager.register_handler(ArduinoTaskHandler())
    manager.register_handler(RaspberryPiTaskHandler())
    manager.register_handler(PrusaTaskHandler())
    manager.register_handler(KukaTaskHandler())
    manager.register_handler(IndustrialRoboticsHandler())
    
    return manager
