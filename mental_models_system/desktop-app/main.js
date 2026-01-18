/**
 * Mental Models System - Desktop Application
 * 
 * Electron-based desktop app with:
 * - GitHub auto-updates (hot loading)
 * - System tray integration
 * - Background processing
 * - Connection to local/remote Mental Models API
 */

const { app, BrowserWindow, Tray, Menu, ipcMain, nativeImage, dialog, shell } = require('electron');
const { autoUpdater } = require('electron-updater');
const log = require('electron-log');
const Store = require('electron-store');
const path = require('path');
const axios = require('axios');
const DataHarvester = require('./harvester');

// Configure logging
log.transports.file.level = 'info';
autoUpdater.logger = log;
autoUpdater.logger.transports.file.level = 'info';

// Persistent storage
const store = new Store({
  defaults: {
    apiUrl: 'http://localhost:8000',
    autoStart: true,
    minimizeToTray: true,
    checkUpdatesOnStart: true,
    updateChannel: 'latest',
    theme: 'system',
    windowBounds: { width: 1200, height: 800 },
    watchedDirs: [],
    scrapeUrls: []
  }
});

// Global references
let mainWindow = null;
let tray = null;
let isQuitting = false;
let harvester = null;

// ============================================
// Auto-Updater Configuration
// ============================================

function setupAutoUpdater() {
  // Check for updates on startup
  if (store.get('checkUpdatesOnStart')) {
    autoUpdater.checkForUpdatesAndNotify();
  }

  // Auto-updater events
  autoUpdater.on('checking-for-update', () => {
    log.info('Checking for updates...');
    sendStatusToWindow('checking-for-update');
  });

  autoUpdater.on('update-available', (info) => {
    log.info('Update available:', info.version);
    sendStatusToWindow('update-available', info);
    
    // Show notification
    if (mainWindow) {
      dialog.showMessageBox(mainWindow, {
        type: 'info',
        title: 'Update Available',
        message: `A new version (${info.version}) is available. It will be downloaded in the background.`,
        buttons: ['OK']
      });
    }
  });

  autoUpdater.on('update-not-available', (info) => {
    log.info('Update not available');
    sendStatusToWindow('update-not-available', info);
  });

  autoUpdater.on('error', (err) => {
    log.error('Error in auto-updater:', err);
    sendStatusToWindow('update-error', err.message);
  });

  autoUpdater.on('download-progress', (progressObj) => {
    let logMessage = `Download speed: ${progressObj.bytesPerSecond}`;
    logMessage += ` - Downloaded ${progressObj.percent.toFixed(1)}%`;
    logMessage += ` (${progressObj.transferred}/${progressObj.total})`;
    log.info(logMessage);
    sendStatusToWindow('download-progress', progressObj);
  });

  autoUpdater.on('update-downloaded', (info) => {
    log.info('Update downloaded:', info.version);
    sendStatusToWindow('update-downloaded', info);
    
    // Prompt user to restart
    dialog.showMessageBox(mainWindow, {
      type: 'info',
      title: 'Update Ready',
      message: `Version ${info.version} has been downloaded. Restart now to apply the update?`,
      buttons: ['Restart Now', 'Later']
    }).then((result) => {
      if (result.response === 0) {
        isQuitting = true;
        autoUpdater.quitAndInstall();
      }
    });
  });
}

function sendStatusToWindow(status, data = null) {
  if (mainWindow && mainWindow.webContents) {
    mainWindow.webContents.send('update-status', { status, data });
  }
}

// ============================================
// Window Management
// ============================================

function createWindow() {
  const bounds = store.get('windowBounds');
  
  mainWindow = new BrowserWindow({
    width: bounds.width,
    height: bounds.height,
    minWidth: 800,
    minHeight: 600,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, 'preload.js')
    },
    icon: path.join(__dirname, 'build', 'icon.png'),
    show: false,
    backgroundColor: '#1a1a2e'
  });

  // Load the app
  mainWindow.loadFile('index.html');

  // Show when ready
  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
  });

  // Save window bounds on resize
  mainWindow.on('resize', () => {
    const bounds = mainWindow.getBounds();
    store.set('windowBounds', { width: bounds.width, height: bounds.height });
  });

  // Handle close to tray
  mainWindow.on('close', (event) => {
    if (!isQuitting && store.get('minimizeToTray')) {
      event.preventDefault();
      mainWindow.hide();
      return false;
    }
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });

  // Open external links in browser
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });
}

// ============================================
// System Tray
// ============================================

function createTray() {
  // Create tray icon (use a simple icon for now)
  const iconPath = path.join(__dirname, 'build', 'tray-icon.png');
  let trayIcon;
  
  try {
    trayIcon = nativeImage.createFromPath(iconPath);
  } catch (e) {
    // Create a simple colored icon if file doesn't exist
    trayIcon = nativeImage.createEmpty();
  }
  
  tray = new Tray(trayIcon);
  
  const contextMenu = Menu.buildFromTemplate([
    {
      label: 'Open Mental Models System',
      click: () => {
        if (mainWindow) {
          mainWindow.show();
          mainWindow.focus();
        } else {
          createWindow();
        }
      }
    },
    { type: 'separator' },
    {
      label: 'Check for Updates',
      click: () => {
        autoUpdater.checkForUpdatesAndNotify();
      }
    },
    {
      label: 'Settings',
      click: () => {
        if (mainWindow) {
          mainWindow.show();
          mainWindow.webContents.send('navigate', 'settings');
        }
      }
    },
    { type: 'separator' },
    {
      label: 'Start Processing',
      click: () => {
        if (mainWindow) {
          mainWindow.webContents.send('start-processing');
        }
      }
    },
    {
      label: 'Stop Processing',
      click: () => {
        if (mainWindow) {
          mainWindow.webContents.send('stop-processing');
        }
      }
    },
    { type: 'separator' },
    {
      label: 'Quit',
      click: () => {
        isQuitting = true;
        app.quit();
      }
    }
  ]);
  
  tray.setToolTip('Mental Models System');
  tray.setContextMenu(contextMenu);
  
  tray.on('double-click', () => {
    if (mainWindow) {
      mainWindow.show();
      mainWindow.focus();
    } else {
      createWindow();
    }
  });
}

// ============================================
// IPC Handlers
// ============================================

function setupIpcHandlers() {
  // Get app version
  ipcMain.handle('get-version', () => {
    return app.getVersion();
  });

  // Get settings
  ipcMain.handle('get-settings', () => {
    return {
      apiUrl: store.get('apiUrl'),
      autoStart: store.get('autoStart'),
      minimizeToTray: store.get('minimizeToTray'),
      checkUpdatesOnStart: store.get('checkUpdatesOnStart'),
      theme: store.get('theme')
    };
  });

  // Save settings
  ipcMain.handle('save-settings', (event, settings) => {
    Object.keys(settings).forEach(key => {
      store.set(key, settings[key]);
    });
    return true;
  });

  // Check for updates manually
  ipcMain.handle('check-for-updates', () => {
    autoUpdater.checkForUpdatesAndNotify();
  });

  // API calls to Mental Models backend
  ipcMain.handle('api-call', async (event, { method, endpoint, data }) => {
    const apiUrl = store.get('apiUrl');
    try {
      const response = await axios({
        method,
        url: `${apiUrl}${endpoint}`,
        data,
        timeout: 30000
      });
      return { success: true, data: response.data };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Get cluster status
  ipcMain.handle('get-cluster-status', async () => {
    const apiUrl = store.get('apiUrl');
    try {
      const response = await axios.get(`${apiUrl}/api/distributed/status`, { timeout: 5000 });
      return { success: true, data: response.data };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Start continuous processing
  ipcMain.handle('start-continuous', async () => {
    const apiUrl = store.get('apiUrl');
    try {
      const response = await axios.post(`${apiUrl}/api/continuous/start`, {}, { timeout: 10000 });
      return { success: true, data: response.data };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Stop continuous processing
  ipcMain.handle('stop-continuous', async () => {
    const apiUrl = store.get('apiUrl');
    try {
      const response = await axios.post(`${apiUrl}/api/continuous/stop`, {}, { timeout: 10000 });
      return { success: true, data: response.data };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Submit work to cluster
  ipcMain.handle('submit-work', async (event, { type, data, priority }) => {
    const apiUrl = store.get('apiUrl');
    try {
      const response = await axios.post(`${apiUrl}/api/distributed/submit`, {
        type,
        data,
        priority
      }, { timeout: 10000 });
      return { success: true, data: response.data };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Open external URL
  ipcMain.handle('open-external', (event, url) => {
    shell.openExternal(url);
  });

  // ============================================
  // Harvester IPC Handlers
  // ============================================

  // Start harvester
  ipcMain.handle('start-harvester', (event, config) => {
    try {
      if (!harvester) {
        harvester = new DataHarvester({
          apiUrl: config.apiUrl || store.get('apiUrl'),
          batchSize: 100,
          scrapeInterval: 3600000,
          syncInterval: 30000
        });

        // Setup update callback
        harvester.setOnUpdate((status) => {
          if (mainWindow && mainWindow.webContents) {
            mainWindow.webContents.send('harvester-update', status);
          }
        });

        // Restore saved directories and URLs
        const savedDirs = store.get('watchedDirs') || [];
        const savedUrls = store.get('scrapeUrls') || [];
        
        savedDirs.forEach(dir => harvester.addWatchDirectory(dir));
        savedUrls.forEach(url => harvester.addScrapeUrl(url));
      }

      harvester.start();
      return { success: true };
    } catch (error) {
      log.error('Failed to start harvester:', error);
      return { success: false, error: error.message };
    }
  });

  // Stop harvester
  ipcMain.handle('stop-harvester', () => {
    try {
      if (harvester) {
        harvester.stop();
      }
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Get harvester status
  ipcMain.handle('get-harvester-status', () => {
    if (harvester) {
      return harvester.getStatus();
    }
    return {
      running: false,
      watchedDirs: store.get('watchedDirs') || [],
      scrapeUrls: store.get('scrapeUrls') || [],
      pendingQueue: 0,
      stats: {
        filesProcessed: 0,
        bytesProcessed: 0,
        urlsScraped: 0,
        itemsSynced: 0,
        errors: 0
      }
    };
  });

  // Add watch directory
  ipcMain.handle('add-watch-directory', (event, dir) => {
    try {
      const dirs = store.get('watchedDirs') || [];
      if (!dirs.includes(dir)) {
        dirs.push(dir);
        store.set('watchedDirs', dirs);
      }

      if (harvester) {
        harvester.addWatchDirectory(dir);
      }

      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Remove watch directory
  ipcMain.handle('remove-watch-directory', (event, dir) => {
    try {
      const dirs = store.get('watchedDirs') || [];
      const index = dirs.indexOf(dir);
      if (index > -1) {
        dirs.splice(index, 1);
        store.set('watchedDirs', dirs);
      }

      if (harvester) {
        harvester.removeWatchDirectory(dir);
      }

      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Add scrape URL
  ipcMain.handle('add-scrape-url', (event, url) => {
    try {
      const urls = store.get('scrapeUrls') || [];
      if (!urls.includes(url)) {
        urls.push(url);
        store.set('scrapeUrls', urls);
      }

      if (harvester) {
        harvester.addScrapeUrl(url);
      }

      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Remove scrape URL
  ipcMain.handle('remove-scrape-url', (event, url) => {
    try {
      const urls = store.get('scrapeUrls') || [];
      const index = urls.indexOf(url);
      if (index > -1) {
        urls.splice(index, 1);
        store.set('scrapeUrls', urls);
      }

      if (harvester) {
        harvester.removeScrapeUrl(url);
      }

      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  });

  // Select directory dialog
  ipcMain.handle('select-directory', async () => {
    const result = await dialog.showOpenDialog(mainWindow, {
      properties: ['openDirectory']
    });
    
    if (!result.canceled && result.filePaths.length > 0) {
      return result.filePaths[0];
    }
    return null;
  });

  // Select files dialog
  ipcMain.handle('select-files', async (event, filters) => {
    const result = await dialog.showOpenDialog(mainWindow, {
      properties: ['openFile', 'multiSelections'],
      filters: filters || [
        { name: 'Text Files', extensions: ['txt', 'md', 'json', 'csv'] },
        { name: 'All Files', extensions: ['*'] }
      ]
    });
    
    if (!result.canceled) {
      return result.filePaths;
    }
    return [];
  });
}

// ============================================
// App Lifecycle
// ============================================

// Single instance lock
const gotTheLock = app.requestSingleInstanceLock();

if (!gotTheLock) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.show();
      mainWindow.focus();
    }
  });

  app.whenReady().then(() => {
    createWindow();
    createTray();
    setupIpcHandlers();
    setupAutoUpdater();

    app.on('activate', () => {
      if (BrowserWindow.getAllWindows().length === 0) {
        createWindow();
      }
    });
  });
}

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    // On Windows/Linux, keep running in tray
    if (!store.get('minimizeToTray')) {
      app.quit();
    }
  }
});

app.on('before-quit', () => {
  isQuitting = true;
});

// Handle certificate errors (for development)
app.on('certificate-error', (event, webContents, url, error, certificate, callback) => {
  if (url.startsWith('https://localhost')) {
    event.preventDefault();
    callback(true);
  } else {
    callback(false);
  }
});
