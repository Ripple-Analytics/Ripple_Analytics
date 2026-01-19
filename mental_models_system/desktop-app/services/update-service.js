/**
 * Update Service Module
 * 
 * Manages hot-loading updates for service modules.
 * Checks for updates from remote sources and applies them.
 */

const fs = require('fs');
const path = require('path');
const https = require('https');
const http = require('http');
const { EventEmitter } = require('events');

const VERSION = '1.0.0';

let eventEmitter = new EventEmitter();
let updateCheckInterval = null;
let moduleLoader = null;

// Update configuration
const config = {
  updateUrl: null,  // URL to check for updates (disabled by default)
  checkInterval: 3600000,  // 1 hour
  autoUpdate: false,
  lastCheck: null
};

/**
 * Initialize the update service
 * @param {object} loader - Module loader instance
 */
function init(loader = null) {
  moduleLoader = loader;
  console.log('[UpdateService] Initialized v' + VERSION);
}

/**
 * Cleanup the update service
 */
function cleanup() {
  if (updateCheckInterval) {
    clearInterval(updateCheckInterval);
    updateCheckInterval = null;
  }
  eventEmitter.removeAllListeners();
  console.log('[UpdateService] Cleaned up');
}

/**
 * Set the module loader
 * @param {object} loader - Module loader instance
 */
function setModuleLoader(loader) {
  moduleLoader = loader;
}

/**
 * Configure update settings
 * @param {object} options - Configuration options
 */
function configure(options) {
  Object.assign(config, options);
}

/**
 * Start automatic update checking
 */
function startAutoCheck() {
  if (updateCheckInterval) {
    clearInterval(updateCheckInterval);
  }
  
  if (config.updateUrl && config.checkInterval > 0) {
    updateCheckInterval = setInterval(() => {
      checkForUpdates().catch(err => {
        console.error('[UpdateService] Auto-check failed:', err);
      });
    }, config.checkInterval);
    
    console.log('[UpdateService] Auto-check started');
  }
}

/**
 * Stop automatic update checking
 */
function stopAutoCheck() {
  if (updateCheckInterval) {
    clearInterval(updateCheckInterval);
    updateCheckInterval = null;
    console.log('[UpdateService] Auto-check stopped');
  }
}

/**
 * Check for updates
 * @returns {Promise<object>} Update check result
 */
async function checkForUpdates() {
  if (!config.updateUrl) {
    return {
      success: true,
      message: 'Update checking disabled - no update URL configured',
      updates: []
    };
  }
  
  config.lastCheck = new Date().toISOString();
  eventEmitter.emit('checking');
  
  try {
    const manifest = await fetchManifest(config.updateUrl);
    const updates = [];
    
    if (moduleLoader) {
      const loadedModules = moduleLoader.getStatus();
      
      for (const [name, info] of Object.entries(manifest.modules || {})) {
        const current = loadedModules[name];
        if (!current || compareVersions(info.version, current.version) > 0) {
          updates.push({
            name,
            currentVersion: current ? current.version : null,
            newVersion: info.version,
            url: info.url,
            changelog: info.changelog
          });
        }
      }
    }
    
    const result = {
      success: true,
      updates,
      hasUpdates: updates.length > 0,
      timestamp: config.lastCheck
    };
    
    eventEmitter.emit('checked', result);
    return result;
  } catch (error) {
    const result = {
      success: false,
      error: error.message,
      timestamp: config.lastCheck
    };
    
    eventEmitter.emit('error', error);
    return result;
  }
}

/**
 * Fetch update manifest from URL
 * @param {string} url - Manifest URL
 * @returns {Promise<object>} Manifest data
 */
function fetchManifest(url) {
  return new Promise((resolve, reject) => {
    const protocol = url.startsWith('https') ? https : http;
    
    protocol.get(url, (res) => {
      let data = '';
      
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve(JSON.parse(data));
        } catch (e) {
          reject(new Error('Invalid manifest format'));
        }
      });
    }).on('error', reject);
  });
}

/**
 * Download and apply an update
 * @param {object} update - Update info
 * @returns {Promise<object>} Update result
 */
async function applyUpdate(update) {
  if (!moduleLoader) {
    throw new Error('Module loader not configured');
  }
  
  eventEmitter.emit('downloading', update);
  
  try {
    // Download the module
    const moduleCode = await downloadModule(update.url);
    
    // Write to services directory
    const modulePath = path.join(__dirname, `${update.name}.js`);
    fs.writeFileSync(modulePath, moduleCode);
    
    // Hot reload the module
    eventEmitter.emit('applying', update);
    moduleLoader.reload(update.name);
    
    const result = {
      success: true,
      name: update.name,
      version: update.newVersion
    };
    
    eventEmitter.emit('applied', result);
    return result;
  } catch (error) {
    eventEmitter.emit('error', { update, error });
    throw error;
  }
}

/**
 * Download module from URL
 * @param {string} url - Module URL
 * @returns {Promise<string>} Module code
 */
function downloadModule(url) {
  return new Promise((resolve, reject) => {
    const protocol = url.startsWith('https') ? https : http;
    
    protocol.get(url, (res) => {
      let data = '';
      
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(data));
    }).on('error', reject);
  });
}

/**
 * Compare version strings
 * @param {string} v1 - Version 1
 * @param {string} v2 - Version 2
 * @returns {number} -1, 0, or 1
 */
function compareVersions(v1, v2) {
  const parts1 = v1.split('.').map(Number);
  const parts2 = v2.split('.').map(Number);
  
  for (let i = 0; i < Math.max(parts1.length, parts2.length); i++) {
    const p1 = parts1[i] || 0;
    const p2 = parts2[i] || 0;
    
    if (p1 > p2) return 1;
    if (p1 < p2) return -1;
  }
  
  return 0;
}

/**
 * Subscribe to update events
 * @param {string} event - Event name
 * @param {function} callback - Callback function
 * @returns {function} Unsubscribe function
 */
function subscribe(event, callback) {
  eventEmitter.on(event, callback);
  return () => eventEmitter.off(event, callback);
}

/**
 * Get service status
 * @returns {object} Service status
 */
function getStatus() {
  return {
    version: VERSION,
    updateUrl: config.updateUrl,
    autoUpdate: config.autoUpdate,
    lastCheck: config.lastCheck,
    autoCheckRunning: updateCheckInterval !== null
  };
}

module.exports = {
  VERSION,
  init,
  cleanup,
  setModuleLoader,
  configure,
  startAutoCheck,
  stopAutoCheck,
  checkForUpdates,
  applyUpdate,
  compareVersions,
  subscribe,
  getStatus
};
