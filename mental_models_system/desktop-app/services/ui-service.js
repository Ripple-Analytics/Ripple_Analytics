/**
 * UI Service Module
 * 
 * Manages UI state and theme configuration.
 * Provides hot-loadable UI customization.
 */

const VERSION = '1.0.0';

// Theme definitions
const THEMES = {
  light: {
    name: 'Light',
    colors: {
      bgPrimary: '#f8f9fa',
      bgSecondary: '#ffffff',
      bgCard: '#ffffff',
      textPrimary: '#1a1a2e',
      textSecondary: '#6c757d',
      accent: '#0066cc',
      accentHover: '#0052a3',
      success: '#28a745',
      warning: '#ffc107',
      error: '#dc3545',
      borderColor: '#dee2e6',
      shadow: '0 2px 8px rgba(0,0,0,0.1)'
    }
  },
  dark: {
    name: 'Dark',
    colors: {
      bgPrimary: '#1a1a2e',
      bgSecondary: '#16213e',
      bgCard: '#0f3460',
      textPrimary: '#eaeaea',
      textSecondary: '#a0a0a0',
      accent: '#e94560',
      accentHover: '#ff6b6b',
      success: '#4ade80',
      warning: '#fbbf24',
      error: '#ef4444',
      borderColor: 'rgba(255,255,255,0.1)',
      shadow: '0 2px 8px rgba(0,0,0,0.3)'
    }
  }
};

let currentTheme = 'light';
let webContents = null;

/**
 * Initialize the UI service
 */
function init() {
  console.log('[UIService] Initialized v' + VERSION);
}

/**
 * Cleanup the UI service
 */
function cleanup() {
  webContents = null;
  console.log('[UIService] Cleaned up');
}

/**
 * Set the web contents reference for IPC
 * @param {object} contents - Electron webContents
 */
function setWebContents(contents) {
  webContents = contents;
}

/**
 * Get available themes
 * @returns {object} Theme definitions
 */
function getThemes() {
  return THEMES;
}

/**
 * Get current theme name
 * @returns {string} Current theme name
 */
function getCurrentTheme() {
  return currentTheme;
}

/**
 * Get theme colors
 * @param {string} themeName - Theme name (optional, defaults to current)
 * @returns {object} Theme colors
 */
function getThemeColors(themeName = null) {
  const name = themeName || currentTheme;
  return THEMES[name] ? THEMES[name].colors : THEMES.light.colors;
}

/**
 * Set the current theme
 * @param {string} themeName - Theme name
 * @returns {boolean} Success
 */
function setTheme(themeName) {
  if (!THEMES[themeName]) {
    console.error(`[UIService] Unknown theme: ${themeName}`);
    return false;
  }
  
  currentTheme = themeName;
  
  // Send theme update to renderer
  if (webContents) {
    webContents.send('theme-changed', {
      theme: themeName,
      colors: THEMES[themeName].colors
    });
  }
  
  console.log(`[UIService] Theme changed to: ${themeName}`);
  return true;
}

/**
 * Generate CSS variables for a theme
 * @param {string} themeName - Theme name (optional)
 * @returns {string} CSS variables string
 */
function generateCssVariables(themeName = null) {
  const colors = getThemeColors(themeName);
  
  return `:root {
    --bg-primary: ${colors.bgPrimary};
    --bg-secondary: ${colors.bgSecondary};
    --bg-card: ${colors.bgCard};
    --text-primary: ${colors.textPrimary};
    --text-secondary: ${colors.textSecondary};
    --accent: ${colors.accent};
    --accent-hover: ${colors.accentHover};
    --success: ${colors.success};
    --warning: ${colors.warning};
    --error: ${colors.error};
    --border-color: ${colors.borderColor};
    --shadow: ${colors.shadow};
  }`;
}

/**
 * Get window background color for current theme
 * @returns {string} Background color hex
 */
function getWindowBackgroundColor() {
  return getThemeColors().bgPrimary;
}

/**
 * Send notification to UI
 * @param {string} type - Notification type (info, success, warning, error)
 * @param {string} message - Notification message
 * @param {number} duration - Duration in ms (optional)
 */
function notify(type, message, duration = 5000) {
  if (webContents) {
    webContents.send('notification', { type, message, duration });
  }
}

/**
 * Update UI status indicator
 * @param {string} status - Status (running, stopped, warning)
 * @param {string} message - Status message
 */
function updateStatus(status, message) {
  if (webContents) {
    webContents.send('status-update', { status, message });
  }
}

/**
 * Send data to UI
 * @param {string} channel - IPC channel
 * @param {*} data - Data to send
 */
function sendToUI(channel, data) {
  if (webContents) {
    webContents.send(channel, data);
  }
}

/**
 * Get service status
 * @returns {object} Service status
 */
function getStatus() {
  return {
    version: VERSION,
    currentTheme,
    availableThemes: Object.keys(THEMES),
    hasWebContents: webContents !== null
  };
}

module.exports = {
  VERSION,
  THEMES,
  init,
  cleanup,
  setWebContents,
  getThemes,
  getCurrentTheme,
  getThemeColors,
  setTheme,
  generateCssVariables,
  getWindowBackgroundColor,
  notify,
  updateStatus,
  sendToUI,
  getStatus
};
