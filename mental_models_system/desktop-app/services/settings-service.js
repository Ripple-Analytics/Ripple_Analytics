/**
 * Settings Service Module
 * 
 * Manages application settings and user preferences.
 * Provides hot-loadable configuration management.
 */

const Store = require('electron-store');
const { EventEmitter } = require('events');

const VERSION = '1.0.0';

// Default settings
const DEFAULTS = {
  // Connection
  apiUrl: 'http://localhost:8000',
  
  // Application
  autoStart: false,
  minimizeToTray: true,
  checkUpdatesOnStart: false,
  
  // Theme
  theme: 'light',
  
  // Window
  windowBounds: { width: 1200, height: 800 },
  
  // Data
  watchedDirs: [],
  scrapeUrls: [],
  
  // Analysis
  offlineMode: true,
  maxResults: 5,
  
  // Notifications
  enableNotifications: true,
  notificationSound: true
};

let store = null;
let eventEmitter = new EventEmitter();

/**
 * Initialize the settings service
 */
function init() {
  store = new Store({ defaults: DEFAULTS });
  console.log('[SettingsService] Initialized v' + VERSION);
}

/**
 * Cleanup the settings service
 */
function cleanup() {
  eventEmitter.removeAllListeners();
  console.log('[SettingsService] Cleaned up');
}

/**
 * Get a setting value
 * @param {string} key - Setting key
 * @param {*} defaultValue - Default value if not found
 * @returns {*} The setting value
 */
function get(key, defaultValue = undefined) {
  if (!store) init();
  return store.get(key, defaultValue !== undefined ? defaultValue : DEFAULTS[key]);
}

/**
 * Set a setting value
 * @param {string} key - Setting key
 * @param {*} value - Setting value
 */
function set(key, value) {
  if (!store) init();
  const oldValue = store.get(key);
  store.set(key, value);
  eventEmitter.emit('change', { key, value, oldValue });
  eventEmitter.emit(`change:${key}`, { value, oldValue });
}

/**
 * Get all settings
 * @returns {object} All settings
 */
function getAll() {
  if (!store) init();
  return store.store;
}

/**
 * Set multiple settings at once
 * @param {object} settings - Settings object
 */
function setAll(settings) {
  if (!store) init();
  for (const [key, value] of Object.entries(settings)) {
    set(key, value);
  }
}

/**
 * Reset a setting to default
 * @param {string} key - Setting key
 */
function reset(key) {
  if (DEFAULTS.hasOwnProperty(key)) {
    set(key, DEFAULTS[key]);
  }
}

/**
 * Reset all settings to defaults
 */
function resetAll() {
  if (!store) init();
  store.clear();
  store.set(DEFAULTS);
  eventEmitter.emit('reset');
}

/**
 * Check if a setting exists
 * @param {string} key - Setting key
 * @returns {boolean}
 */
function has(key) {
  if (!store) init();
  return store.has(key);
}

/**
 * Delete a setting
 * @param {string} key - Setting key
 */
function remove(key) {
  if (!store) init();
  store.delete(key);
  eventEmitter.emit('delete', { key });
}

/**
 * Subscribe to setting changes
 * @param {string} event - Event name ('change', 'change:key', 'reset', 'delete')
 * @param {function} callback - Callback function
 * @returns {function} Unsubscribe function
 */
function subscribe(event, callback) {
  eventEmitter.on(event, callback);
  return () => eventEmitter.off(event, callback);
}

/**
 * Get the path to the settings file
 * @returns {string} Settings file path
 */
function getPath() {
  if (!store) init();
  return store.path;
}

/**
 * Export settings to JSON
 * @returns {string} JSON string of settings
 */
function exportSettings() {
  return JSON.stringify(getAll(), null, 2);
}

/**
 * Import settings from JSON
 * @param {string} json - JSON string of settings
 */
function importSettings(json) {
  try {
    const settings = JSON.parse(json);
    setAll(settings);
    return { success: true };
  } catch (error) {
    return { success: false, error: error.message };
  }
}

/**
 * Get service status
 * @returns {object} Service status
 */
function getStatus() {
  return {
    version: VERSION,
    path: store ? store.path : null,
    settingsCount: Object.keys(getAll()).length
  };
}

module.exports = {
  VERSION,
  DEFAULTS,
  init,
  cleanup,
  get,
  set,
  getAll,
  setAll,
  reset,
  resetAll,
  has,
  remove,
  subscribe,
  getPath,
  exportSettings,
  importSettings,
  getStatus
};
