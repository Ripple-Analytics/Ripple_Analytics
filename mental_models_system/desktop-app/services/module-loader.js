/**
 * Module Loader Service
 * 
 * Provides hot-loading capability for microservices architecture.
 * Each service module can be loaded, unloaded, and reloaded independently.
 */

const fs = require('fs');
const path = require('path');
const { EventEmitter } = require('events');

class ModuleLoader extends EventEmitter {
  constructor(options = {}) {
    super();
    this.modules = new Map();
    this.modulePaths = new Map();
    this.watchers = new Map();
    this.servicesDir = options.servicesDir || path.join(__dirname);
    this.hotReloadEnabled = options.hotReload !== false;
  }

  /**
   * Load a service module
   * @param {string} moduleName - Name of the module to load
   * @param {string} modulePath - Path to the module file (optional)
   * @returns {object} The loaded module
   */
  load(moduleName, modulePath = null) {
    const fullPath = modulePath || path.join(this.servicesDir, `${moduleName}.js`);
    
    try {
      // Clear require cache to enable hot reload
      if (require.cache[require.resolve(fullPath)]) {
        delete require.cache[require.resolve(fullPath)];
      }
      
      const module = require(fullPath);
      this.modules.set(moduleName, module);
      this.modulePaths.set(moduleName, fullPath);
      
      // Initialize module if it has an init function
      if (typeof module.init === 'function') {
        module.init();
      }
      
      // Setup file watcher for hot reload
      if (this.hotReloadEnabled) {
        this.setupWatcher(moduleName, fullPath);
      }
      
      this.emit('module-loaded', { name: moduleName, module });
      console.log(`[ModuleLoader] Loaded module: ${moduleName}`);
      
      return module;
    } catch (error) {
      console.error(`[ModuleLoader] Failed to load module ${moduleName}:`, error);
      this.emit('module-error', { name: moduleName, error });
      throw error;
    }
  }

  /**
   * Unload a service module
   * @param {string} moduleName - Name of the module to unload
   */
  unload(moduleName) {
    const module = this.modules.get(moduleName);
    const modulePath = this.modulePaths.get(moduleName);
    
    if (module) {
      // Call cleanup if available
      if (typeof module.cleanup === 'function') {
        module.cleanup();
      }
      
      // Remove from cache
      if (modulePath && require.cache[require.resolve(modulePath)]) {
        delete require.cache[require.resolve(modulePath)];
      }
      
      // Stop watcher
      const watcher = this.watchers.get(moduleName);
      if (watcher) {
        watcher.close();
        this.watchers.delete(moduleName);
      }
      
      this.modules.delete(moduleName);
      this.modulePaths.delete(moduleName);
      
      this.emit('module-unloaded', { name: moduleName });
      console.log(`[ModuleLoader] Unloaded module: ${moduleName}`);
    }
  }

  /**
   * Reload a service module (hot reload)
   * @param {string} moduleName - Name of the module to reload
   * @returns {object} The reloaded module
   */
  reload(moduleName) {
    const modulePath = this.modulePaths.get(moduleName);
    
    if (!modulePath) {
      throw new Error(`Module ${moduleName} not found`);
    }
    
    console.log(`[ModuleLoader] Hot reloading module: ${moduleName}`);
    this.emit('module-reloading', { name: moduleName });
    
    // Unload and reload
    this.unload(moduleName);
    return this.load(moduleName, modulePath);
  }

  /**
   * Get a loaded module
   * @param {string} moduleName - Name of the module
   * @returns {object} The module or undefined
   */
  get(moduleName) {
    return this.modules.get(moduleName);
  }

  /**
   * Check if a module is loaded
   * @param {string} moduleName - Name of the module
   * @returns {boolean}
   */
  isLoaded(moduleName) {
    return this.modules.has(moduleName);
  }

  /**
   * Get list of all loaded modules
   * @returns {string[]}
   */
  getLoadedModules() {
    return Array.from(this.modules.keys());
  }

  /**
   * Setup file watcher for hot reload
   * @param {string} moduleName - Name of the module
   * @param {string} modulePath - Path to the module file
   */
  setupWatcher(moduleName, modulePath) {
    // Close existing watcher if any
    const existingWatcher = this.watchers.get(moduleName);
    if (existingWatcher) {
      existingWatcher.close();
    }
    
    let debounceTimer = null;
    
    const watcher = fs.watch(modulePath, (eventType) => {
      if (eventType === 'change') {
        // Debounce to avoid multiple reloads
        if (debounceTimer) {
          clearTimeout(debounceTimer);
        }
        
        debounceTimer = setTimeout(() => {
          try {
            this.reload(moduleName);
            this.emit('hot-reload', { name: moduleName, success: true });
          } catch (error) {
            console.error(`[ModuleLoader] Hot reload failed for ${moduleName}:`, error);
            this.emit('hot-reload', { name: moduleName, success: false, error });
          }
        }, 100);
      }
    });
    
    this.watchers.set(moduleName, watcher);
  }

  /**
   * Load all modules from services directory
   */
  loadAll() {
    const files = fs.readdirSync(this.servicesDir);
    
    for (const file of files) {
      if (file.endsWith('.js') && file !== 'module-loader.js') {
        const moduleName = file.replace('.js', '');
        try {
          this.load(moduleName);
        } catch (error) {
          console.error(`[ModuleLoader] Failed to load ${moduleName}:`, error);
        }
      }
    }
  }

  /**
   * Unload all modules
   */
  unloadAll() {
    for (const moduleName of this.modules.keys()) {
      this.unload(moduleName);
    }
  }

  /**
   * Get module status
   * @returns {object} Status of all modules
   */
  getStatus() {
    const status = {};
    for (const [name, module] of this.modules) {
      status[name] = {
        loaded: true,
        path: this.modulePaths.get(name),
        hasInit: typeof module.init === 'function',
        hasCleanup: typeof module.cleanup === 'function',
        version: module.VERSION || '1.0.0'
      };
    }
    return status;
  }
}

module.exports = ModuleLoader;
