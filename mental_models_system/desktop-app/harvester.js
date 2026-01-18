/**
 * Mental Models System - Data Harvester
 * 
 * Continuous data harvesting system that:
 * - Monitors file system directories for new/changed files
 * - Scrapes web pages on a schedule
 * - Processes and pipes data to the web app API
 * - Runs at maximum capacity continuously
 */

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const axios = require('axios');
const log = require('electron-log');

class DataHarvester {
  constructor(options = {}) {
    this.apiUrl = options.apiUrl || 'http://localhost:8000';
    this.batchSize = options.batchSize || 100;
    this.scrapeInterval = options.scrapeInterval || 3600000; // 1 hour
    this.syncInterval = options.syncInterval || 30000; // 30 seconds
    this.maxConcurrent = options.maxConcurrent || 10;
    
    // State
    this.running = false;
    this.watchedDirs = new Set();
    this.scrapeUrls = new Set();
    this.fileWatchers = new Map();
    this.processedHashes = new Set();
    this.pendingQueue = [];
    this.stats = {
      filesProcessed: 0,
      bytesProcessed: 0,
      urlsScraped: 0,
      itemsSynced: 0,
      errors: 0,
      startTime: null,
      lastSync: null
    };
    
    // Intervals
    this.scrapeIntervalId = null;
    this.syncIntervalId = null;
    this.statusIntervalId = null;
    
    // Callbacks
    this.onUpdate = null;
  }

  // ============================================
  // Lifecycle
  // ============================================

  start() {
    if (this.running) return;
    
    log.info('Starting data harvester...');
    this.running = true;
    this.stats.startTime = new Date();
    
    // Start file watchers for all watched directories
    this.watchedDirs.forEach(dir => this._startWatching(dir));
    
    // Start scraping interval
    this.scrapeIntervalId = setInterval(() => this._scrapeAll(), this.scrapeInterval);
    
    // Start sync interval
    this.syncIntervalId = setInterval(() => this._syncToApi(), this.syncInterval);
    
    // Start status update interval
    this.statusIntervalId = setInterval(() => this._emitStatus(), 5000);
    
    // Initial scrape
    setTimeout(() => this._scrapeAll(), 1000);
    
    this._emitStatus();
    log.info('Data harvester started');
  }

  stop() {
    if (!this.running) return;
    
    log.info('Stopping data harvester...');
    this.running = false;
    
    // Stop all file watchers
    this.fileWatchers.forEach((watcher, dir) => {
      watcher.close();
      log.info(`Stopped watching: ${dir}`);
    });
    this.fileWatchers.clear();
    
    // Clear intervals
    if (this.scrapeIntervalId) clearInterval(this.scrapeIntervalId);
    if (this.syncIntervalId) clearInterval(this.syncIntervalId);
    if (this.statusIntervalId) clearInterval(this.statusIntervalId);
    
    this._emitStatus();
    log.info('Data harvester stopped');
  }

  getStatus() {
    return {
      running: this.running,
      watchedDirs: Array.from(this.watchedDirs),
      scrapeUrls: Array.from(this.scrapeUrls),
      pendingQueue: this.pendingQueue.length,
      stats: { ...this.stats }
    };
  }

  // ============================================
  // Directory Watching
  // ============================================

  addWatchDirectory(dir) {
    if (!fs.existsSync(dir)) {
      log.warn(`Directory does not exist: ${dir}`);
      return false;
    }
    
    this.watchedDirs.add(dir);
    
    if (this.running) {
      this._startWatching(dir);
    }
    
    // Process existing files
    this._processDirectory(dir);
    
    log.info(`Added watch directory: ${dir}`);
    return true;
  }

  removeWatchDirectory(dir) {
    this.watchedDirs.delete(dir);
    
    if (this.fileWatchers.has(dir)) {
      this.fileWatchers.get(dir).close();
      this.fileWatchers.delete(dir);
    }
    
    log.info(`Removed watch directory: ${dir}`);
    return true;
  }

  _startWatching(dir) {
    if (this.fileWatchers.has(dir)) return;
    
    try {
      const watcher = fs.watch(dir, { recursive: true }, (eventType, filename) => {
        if (filename && eventType === 'change') {
          const fullPath = path.join(dir, filename);
          this._processFile(fullPath);
        }
      });
      
      this.fileWatchers.set(dir, watcher);
      log.info(`Started watching: ${dir}`);
    } catch (error) {
      log.error(`Error watching directory ${dir}:`, error);
      this.stats.errors++;
    }
  }

  _processDirectory(dir) {
    try {
      const files = this._getAllFiles(dir);
      log.info(`Processing ${files.length} files from ${dir}`);
      
      files.forEach(file => this._processFile(file));
    } catch (error) {
      log.error(`Error processing directory ${dir}:`, error);
      this.stats.errors++;
    }
  }

  _getAllFiles(dir, files = []) {
    const items = fs.readdirSync(dir);
    
    for (const item of items) {
      const fullPath = path.join(dir, item);
      const stat = fs.statSync(fullPath);
      
      if (stat.isDirectory()) {
        // Skip hidden directories and node_modules
        if (!item.startsWith('.') && item !== 'node_modules') {
          this._getAllFiles(fullPath, files);
        }
      } else if (this._isSupportedFile(fullPath)) {
        files.push(fullPath);
      }
    }
    
    return files;
  }

  _isSupportedFile(filePath) {
    const supportedExtensions = ['.txt', '.md', '.json', '.csv', '.html', '.xml', '.pdf'];
    const ext = path.extname(filePath).toLowerCase();
    return supportedExtensions.includes(ext);
  }

  _processFile(filePath) {
    try {
      if (!fs.existsSync(filePath)) return;
      if (!this._isSupportedFile(filePath)) return;
      
      const stat = fs.statSync(filePath);
      if (stat.size > 100 * 1024 * 1024) {
        // Skip files larger than 100MB
        log.warn(`Skipping large file: ${filePath}`);
        return;
      }
      
      const content = fs.readFileSync(filePath, 'utf-8');
      const hash = this._hashContent(content);
      
      // Skip if already processed
      if (this.processedHashes.has(hash)) return;
      this.processedHashes.add(hash);
      
      // Add to queue
      this.pendingQueue.push({
        type: 'file',
        path: filePath,
        content: content,
        hash: hash,
        size: stat.size,
        timestamp: new Date().toISOString()
      });
      
      this.stats.filesProcessed++;
      this.stats.bytesProcessed += stat.size;
      
      log.debug(`Processed file: ${filePath}`);
    } catch (error) {
      log.error(`Error processing file ${filePath}:`, error);
      this.stats.errors++;
    }
  }

  // ============================================
  // Web Scraping
  // ============================================

  addScrapeUrl(url) {
    this.scrapeUrls.add(url);
    log.info(`Added scrape URL: ${url}`);
    
    // Scrape immediately
    if (this.running) {
      this._scrapeUrl(url);
    }
    
    return true;
  }

  removeScrapeUrl(url) {
    this.scrapeUrls.delete(url);
    log.info(`Removed scrape URL: ${url}`);
    return true;
  }

  async _scrapeAll() {
    if (!this.running) return;
    
    log.info(`Scraping ${this.scrapeUrls.size} URLs...`);
    
    const urls = Array.from(this.scrapeUrls);
    const chunks = this._chunkArray(urls, this.maxConcurrent);
    
    for (const chunk of chunks) {
      await Promise.all(chunk.map(url => this._scrapeUrl(url)));
    }
  }

  async _scrapeUrl(url) {
    try {
      const response = await axios.get(url, {
        timeout: 30000,
        headers: {
          'User-Agent': 'MentalModelsBot/1.0 (Desktop Harvester)'
        }
      });
      
      const content = response.data;
      const hash = this._hashContent(typeof content === 'string' ? content : JSON.stringify(content));
      
      // Skip if already processed
      if (this.processedHashes.has(hash)) return;
      this.processedHashes.add(hash);
      
      // Extract text from HTML
      let text = content;
      if (typeof content === 'string' && content.includes('<html')) {
        text = this._extractTextFromHtml(content);
      }
      
      // Add to queue
      this.pendingQueue.push({
        type: 'url',
        url: url,
        content: text,
        hash: hash,
        timestamp: new Date().toISOString()
      });
      
      this.stats.urlsScraped++;
      log.debug(`Scraped URL: ${url}`);
    } catch (error) {
      log.error(`Error scraping URL ${url}:`, error.message);
      this.stats.errors++;
    }
  }

  _extractTextFromHtml(html) {
    // Simple HTML to text extraction
    return html
      .replace(/<script[^>]*>[\s\S]*?<\/script>/gi, '')
      .replace(/<style[^>]*>[\s\S]*?<\/style>/gi, '')
      .replace(/<[^>]+>/g, ' ')
      .replace(/\s+/g, ' ')
      .trim();
  }

  // ============================================
  // API Sync
  // ============================================

  async _syncToApi() {
    if (!this.running || this.pendingQueue.length === 0) return;
    
    const batch = this.pendingQueue.splice(0, this.batchSize);
    log.info(`Syncing ${batch.length} items to API...`);
    
    try {
      // Submit each item as work to the distributed system
      for (const item of batch) {
        await this._submitToApi(item);
      }
      
      this.stats.itemsSynced += batch.length;
      this.stats.lastSync = new Date();
      
      log.info(`Synced ${batch.length} items successfully`);
    } catch (error) {
      log.error('Error syncing to API:', error.message);
      // Put items back in queue
      this.pendingQueue.unshift(...batch);
      this.stats.errors++;
    }
  }

  async _submitToApi(item) {
    try {
      const response = await axios.post(`${this.apiUrl}/api/distributed/submit`, {
        type: 'analyze',
        data: {
          source: item.type === 'file' ? item.path : item.url,
          content: item.content.substring(0, 50000), // Limit content size
          hash: item.hash,
          harvested_at: item.timestamp
        },
        priority: 'normal'
      }, {
        timeout: 10000
      });
      
      return response.data;
    } catch (error) {
      throw error;
    }
  }

  // ============================================
  // Utilities
  // ============================================

  _hashContent(content) {
    return crypto.createHash('sha256').update(content).digest('hex');
  }

  _chunkArray(array, size) {
    const chunks = [];
    for (let i = 0; i < array.length; i += size) {
      chunks.push(array.slice(i, i + size));
    }
    return chunks;
  }

  _emitStatus() {
    if (this.onUpdate) {
      this.onUpdate(this.getStatus());
    }
  }

  // ============================================
  // Configuration
  // ============================================

  setApiUrl(url) {
    this.apiUrl = url;
  }

  setOnUpdate(callback) {
    this.onUpdate = callback;
  }
}

module.exports = DataHarvester;
