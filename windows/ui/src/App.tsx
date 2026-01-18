import { useState, useEffect } from 'react';

interface SystemStats {
  filesProcessed: number;
  bytesProcessed: number;
  patternsDetected: number;
  activeWorkers: number;
  cpuUsage: number;
  gpuUsage: number;
  uptime: number;
  lastSync: string | null;
}

interface HarvestStatus {
  isRunning: boolean;
  currentPath: string | null;
  queueSize: number;
}

export default function App() {
  const [stats, setStats] = useState<SystemStats>({
    filesProcessed: 0,
    bytesProcessed: 0,
    patternsDetected: 0,
    activeWorkers: 0,
    cpuUsage: 0,
    gpuUsage: 0,
    uptime: 0,
    lastSync: null,
  });

  const [harvest, setHarvest] = useState<HarvestStatus>({
    isRunning: false,
    currentPath: null,
    queueSize: 0,
  });

  const [connected, setConnected] = useState(false);

  // Connect to backend and get real stats
  useEffect(() => {
    const fetchStats = async () => {
      try {
        const response = await fetch('http://localhost:8080/api/stats');
        if (response.ok) {
          const data = await response.json();
          setStats(data);
          setConnected(true);
        }
      } catch {
        setConnected(false);
      }
    };

    fetchStats();
    const interval = setInterval(fetchStats, 1000);
    return () => clearInterval(interval);
  }, []);

  const formatBytes = (bytes: number): string => {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB', 'TB', 'PB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
  };

  const formatUptime = (seconds: number): string => {
    if (seconds === 0) return '—';
    const h = Math.floor(seconds / 3600);
    const m = Math.floor((seconds % 3600) / 60);
    const s = seconds % 60;
    return `${h}h ${m}m ${s}s`;
  };

  return (
    <div className="min-h-screen bg-white text-gray-900 font-sans">
      {/* Header */}
      <header className="border-b border-gray-200 px-6 py-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="w-8 h-8 bg-gray-900 rounded flex items-center justify-center">
              <span className="text-white font-bold text-sm">MM</span>
            </div>
            <div>
              <h1 className="font-bold text-lg">Mental Models</h1>
              <p className="text-xs text-gray-500">Data Harvester</p>
            </div>
          </div>
          <div className="flex items-center gap-2">
            <span className={`w-2 h-2 rounded-full ${connected ? 'bg-green-500' : 'bg-gray-300'}`} />
            <span className="text-sm text-gray-500">
              {connected ? 'Connected' : 'Disconnected'}
            </span>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="p-6 space-y-6">
        {/* Status Banner */}
        {!connected && (
          <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
            <p className="text-sm text-gray-600">
              <strong>Not connected to backend.</strong> Start the server to see real metrics.
            </p>
          </div>
        )}

        {/* Stats Grid */}
        <div className="grid grid-cols-4 gap-4">
          <div className="bg-white border border-gray-200 rounded-lg p-4">
            <span className="text-sm text-gray-500">Files Processed</span>
            <div className="text-2xl font-bold font-mono mt-1">
              {stats.filesProcessed.toLocaleString()}
            </div>
          </div>
          <div className="bg-white border border-gray-200 rounded-lg p-4">
            <span className="text-sm text-gray-500">Data Processed</span>
            <div className="text-2xl font-bold font-mono mt-1">
              {formatBytes(stats.bytesProcessed)}
            </div>
          </div>
          <div className="bg-white border border-gray-200 rounded-lg p-4">
            <span className="text-sm text-gray-500">Patterns Found</span>
            <div className="text-2xl font-bold font-mono mt-1">
              {stats.patternsDetected.toLocaleString()}
            </div>
          </div>
          <div className="bg-white border border-gray-200 rounded-lg p-4">
            <span className="text-sm text-gray-500">Uptime</span>
            <div className="text-2xl font-bold font-mono mt-1">
              {formatUptime(stats.uptime)}
            </div>
          </div>
        </div>

        {/* Resource Usage */}
        <div>
          <h2 className="text-sm font-semibold text-gray-700 mb-3">Resource Usage</h2>
          <div className="grid grid-cols-2 gap-4">
            <div className="bg-white border border-gray-200 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <span className="text-sm text-gray-500">CPU</span>
                <span className="text-sm font-mono">{stats.cpuUsage}%</span>
              </div>
              <div className="h-2 bg-gray-100 rounded-full overflow-hidden">
                <div 
                  className="h-full bg-gray-700 rounded-full transition-all"
                  style={{ width: `${stats.cpuUsage}%` }}
                />
              </div>
            </div>
            <div className="bg-white border border-gray-200 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <span className="text-sm text-gray-500">GPU</span>
                <span className="text-sm font-mono">{stats.gpuUsage}%</span>
              </div>
              <div className="h-2 bg-gray-100 rounded-full overflow-hidden">
                <div 
                  className="h-full bg-gray-700 rounded-full transition-all"
                  style={{ width: `${stats.gpuUsage}%` }}
                />
              </div>
            </div>
          </div>
        </div>

        {/* Workers */}
        <div>
          <h2 className="text-sm font-semibold text-gray-700 mb-3">Workers</h2>
          <div className="bg-white border border-gray-200 rounded-lg p-4">
            <div className="flex items-center justify-between">
              <span className="text-sm text-gray-500">Active Workers</span>
              <span className="text-lg font-bold font-mono">{stats.activeWorkers}</span>
            </div>
            {harvest.isRunning && harvest.currentPath && (
              <div className="mt-3 pt-3 border-t border-gray-100">
                <span className="text-xs text-gray-500">Currently processing:</span>
                <p className="text-sm font-mono text-gray-700 truncate mt-1">
                  {harvest.currentPath}
                </p>
              </div>
            )}
            {harvest.queueSize > 0 && (
              <div className="mt-2">
                <span className="text-xs text-gray-500">
                  {harvest.queueSize.toLocaleString()} items in queue
                </span>
              </div>
            )}
          </div>
        </div>

        {/* Last Sync */}
        {stats.lastSync && (
          <div className="text-xs text-gray-400 text-center">
            Last synced: {stats.lastSync}
          </div>
        )}
      </main>

      {/* Footer */}
      <footer className="fixed bottom-0 left-0 right-0 border-t border-gray-200 bg-white px-6 py-3">
        <div className="flex items-center justify-between">
          <span className="text-xs text-gray-500">
            Mental Models System v1.0
          </span>
          <button 
            className={`px-4 py-2 rounded text-sm font-medium transition-colors ${
              harvest.isRunning 
                ? 'bg-gray-200 text-gray-700 hover:bg-gray-300' 
                : 'bg-gray-900 text-white hover:bg-gray-800'
            }`}
            onClick={() => setHarvest(h => ({ ...h, isRunning: !h.isRunning }))}
          >
            {harvest.isRunning ? 'Stop Harvesting' : 'Start Harvesting'}
          </button>
        </div>
      </footer>
    </div>
  );
}
