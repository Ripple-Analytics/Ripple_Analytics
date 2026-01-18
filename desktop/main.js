// Electron Main Process - Mental Models Desktop App
// Replaces Rust/Tauri main.rs
// Provides window management, IPC, and system integration

const { app, BrowserWindow, Menu, ipcMain, dialog } = require('electron');
const path = require('path');
const isDev = require('electron-is-dev');
const Store = require('electron-store');

// Persistent storage
const store = new Store();

let mainWindow;
let webAppWindow;

// Create main window
function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 800,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      nodeIntegration: false,
      contextIsolation: true,
      enableRemoteModule: false,
      sandbox: true,
    },
  });

  const startUrl = isDev
    ? 'http://localhost:3000'
    : `file://${path.join(__dirname, '../build/index.html')}`;

  mainWindow.loadURL(startUrl);

  if (isDev) {
    mainWindow.webContents.openDevTools();
  }

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
}

// Open web app in separate window
function openWebApp() {
  if (webAppWindow) {
    webAppWindow.focus();
    return;
  }

  webAppWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
    },
  });

  const webAppUrl = store.get('webAppUrl', 'http://localhost:3000');
  webAppWindow.loadURL(webAppUrl);

  webAppWindow.on('closed', () => {
    webAppWindow = null;
  });
}

// IPC Handlers

// Add folder to watch
ipcMain.handle('add-folder', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    properties: ['openDirectory'],
  });

  if (!result.canceled) {
    const folderPath = result.filePaths[0];
    const watchedFolders = store.get('watchedFolders', []);
    
    if (!watchedFolders.includes(folderPath)) {
      watchedFolders.push(folderPath);
      store.set('watchedFolders', watchedFolders);
      
      mainWindow.webContents.send('add-folder', { path: folderPath });
    }
  }
});

// Remove folder from watch
ipcMain.handle('remove-folder', async (event, folderPath) => {
  const watchedFolders = store.get('watchedFolders', []);
  const updated = watchedFolders.filter(f => f !== folderPath);
  store.set('watchedFolders', updated);
  
  mainWindow.webContents.send('remove-folder', { path: folderPath });
});

// Get watched folders
ipcMain.handle('get-folders', async () => {
  return store.get('watchedFolders', []);
});

// Sync results
ipcMain.handle('sync-results', async (event, results) => {
  try {
    const apiUrl = store.get('apiUrl', 'http://localhost:3000');
    const response = await fetch(`${apiUrl}/api/v1/analyze/batch`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ analyses: results }),
    });
    
    if (response.ok) {
      return { success: true };
    } else {
      return { success: false, error: 'Sync failed' };
    }
  } catch (error) {
    console.error('Sync error:', error);
    return { success: false, error: error.message };
  }
});

// Open web app
ipcMain.handle('open-web-app', async () => {
  openWebApp();
});

// Settings
ipcMain.handle('get-settings', async () => {
  return {
    apiUrl: store.get('apiUrl', 'http://localhost:3000'),
    watchedFolders: store.get('watchedFolders', []),
    autoSync: store.get('autoSync', true),
  };
});

ipcMain.handle('set-settings', async (event, settings) => {
  Object.entries(settings).forEach(([key, value]) => {
    store.set(key, value);
  });
  return { success: true };
});

// File analysis notification
ipcMain.handle('file-analyzed', async (event, result) => {
  // Could show notification here
  console.log('File analyzed:', result.source_url);
});

// App Menu
const template = [
  {
    label: 'File',
    submenu: [
      {
        label: 'Add Folder',
        accelerator: 'CmdOrCtrl+O',
        click: () => mainWindow.webContents.send('menu-add-folder'),
      },
      { type: 'separator' },
      {
        label: 'Exit',
        accelerator: 'CmdOrCtrl+Q',
        click: () => app.quit(),
      },
    ],
  },
  {
    label: 'View',
    submenu: [
      {
        label: 'Open Web App',
        click: openWebApp,
      },
      { type: 'separator' },
      { role: 'reload' },
      { role: 'forceReload' },
      { role: 'toggleDevTools' },
    ],
  },
  {
    label: 'Help',
    submenu: [
      {
        label: 'About',
        click: () => {
          dialog.showMessageBox(mainWindow, {
            type: 'info',
            title: 'About Mental Models Desktop',
            message: 'Mental Models Desktop App',
            detail: 'Version 1.0.0\n\nUnified Electric Clojure Stack',
          });
        },
      },
    ],
  },
];

const menu = Menu.buildFromTemplate(template);
Menu.setApplicationMenu(menu);

// App lifecycle
app.on('ready', () => {
  createWindow();
  
  // Load watched folders from storage
  const watchedFolders = store.get('watchedFolders', []);
  watchedFolders.forEach(folder => {
    mainWindow.webContents.send('add-folder', { path: folder });
  });
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  if (mainWindow === null) {
    createWindow();
  }
});

// Auto-update check (optional)
// const { autoUpdater } = require('electron-updater');
// autoUpdater.checkForUpdatesAndNotify();
