/**
 * Mental Models System - Preload Script
 * 
 * Exposes secure APIs to the renderer process
 */

const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  // App info
  getVersion: () => ipcRenderer.invoke('get-version'),
  
  // Settings
  getSettings: () => ipcRenderer.invoke('get-settings'),
  saveSettings: (settings) => ipcRenderer.invoke('save-settings', settings),
  
  // Updates
  checkForUpdates: () => ipcRenderer.invoke('check-for-updates'),
  onUpdateStatus: (callback) => ipcRenderer.on('update-status', (event, data) => callback(data)),
  
  // API calls
  apiCall: (method, endpoint, data) => ipcRenderer.invoke('api-call', { method, endpoint, data }),
  
  // Cluster operations
  getClusterStatus: () => ipcRenderer.invoke('get-cluster-status'),
  startContinuous: () => ipcRenderer.invoke('start-continuous'),
  stopContinuous: () => ipcRenderer.invoke('stop-continuous'),
  submitWork: (type, data, priority) => ipcRenderer.invoke('submit-work', { type, data, priority }),
  
  // Harvester operations
  startHarvester: (config) => ipcRenderer.invoke('start-harvester', config),
  stopHarvester: () => ipcRenderer.invoke('stop-harvester'),
  getHarvesterStatus: () => ipcRenderer.invoke('get-harvester-status'),
  addWatchDirectory: (dir) => ipcRenderer.invoke('add-watch-directory', dir),
  removeWatchDirectory: (dir) => ipcRenderer.invoke('remove-watch-directory', dir),
  addScrapeUrl: (url) => ipcRenderer.invoke('add-scrape-url', url),
  removeScrapeUrl: (url) => ipcRenderer.invoke('remove-scrape-url', url),
  
  // Events from main process
  onNavigate: (callback) => ipcRenderer.on('navigate', (event, page) => callback(page)),
  onStartProcessing: (callback) => ipcRenderer.on('start-processing', () => callback()),
  onStopProcessing: (callback) => ipcRenderer.on('stop-processing', () => callback()),
  onHarvesterUpdate: (callback) => ipcRenderer.on('harvester-update', (event, data) => callback(data)),
  
  // File dialogs
  selectDirectory: () => ipcRenderer.invoke('select-directory'),
  selectFiles: (filters) => ipcRenderer.invoke('select-files', filters),
  
  // External links
  openExternal: (url) => ipcRenderer.invoke('open-external', url)
});
