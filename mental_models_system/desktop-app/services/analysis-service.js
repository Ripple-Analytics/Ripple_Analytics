/**
 * Analysis Service Module
 * 
 * Provides mental model analysis capabilities.
 * Works offline with local keyword-based analysis.
 * Can connect to remote API when available.
 */

const VERSION = '1.0.0';

// Mental Models Database (embedded for offline use)
const MENTAL_MODELS = [
  {
    id: 1,
    name: 'Incentive-Caused Bias',
    category: 'Psychology',
    keywords: ['incentive', 'reward', 'punishment', 'motivation', 'bonus', 'commission', 'salary', 'pay'],
    description: 'People tend to act in ways that serve their incentives, even unconsciously.'
  },
  {
    id: 2,
    name: 'Confirmation Bias',
    category: 'Psychology',
    keywords: ['confirm', 'belief', 'evidence', 'ignore', 'dismiss', 'agree', 'disagree', 'opinion'],
    description: 'The tendency to search for, interpret, and recall information that confirms pre-existing beliefs.'
  },
  {
    id: 3,
    name: 'Availability Heuristic',
    category: 'Psychology',
    keywords: ['recent', 'memorable', 'vivid', 'news', 'media', 'remember', 'recall', 'example'],
    description: 'Overweighting information that comes easily to mind.'
  },
  {
    id: 4,
    name: 'Sunk Cost Fallacy',
    category: 'Economics',
    keywords: ['sunk', 'cost', 'invested', 'spent', 'waste', 'continue', 'quit', 'abandon'],
    description: 'Continuing a behavior due to previously invested resources rather than future value.'
  },
  {
    id: 5,
    name: 'Opportunity Cost',
    category: 'Economics',
    keywords: ['opportunity', 'alternative', 'trade-off', 'choice', 'option', 'instead', 'rather'],
    description: 'The cost of the next best alternative foregone.'
  },
  {
    id: 6,
    name: 'Margin of Safety',
    category: 'Engineering',
    keywords: ['margin', 'safety', 'buffer', 'redundancy', 'backup', 'cushion', 'reserve'],
    description: 'Building in a buffer to account for errors, unknowns, and variability.'
  },
  {
    id: 7,
    name: 'Feedback Loops',
    category: 'Systems',
    keywords: ['feedback', 'loop', 'cycle', 'reinforce', 'amplify', 'dampen', 'stabilize'],
    description: 'Systems where outputs become inputs, creating reinforcing or balancing dynamics.'
  },
  {
    id: 8,
    name: 'Second-Order Thinking',
    category: 'Thinking Tools',
    keywords: ['consequence', 'effect', 'result', 'then what', 'downstream', 'ripple', 'chain'],
    description: 'Considering the consequences of consequences.'
  },
  {
    id: 9,
    name: 'Inversion',
    category: 'Thinking Tools',
    keywords: ['invert', 'reverse', 'opposite', 'avoid', 'prevent', 'failure', 'mistake'],
    description: 'Approaching problems backwards - thinking about what to avoid rather than what to do.'
  },
  {
    id: 10,
    name: 'Circle of Competence',
    category: 'Thinking Tools',
    keywords: ['competence', 'expertise', 'knowledge', 'understand', 'limit', 'boundary', 'edge'],
    description: 'Understanding the boundaries of your knowledge and staying within them.'
  },
  {
    id: 11,
    name: 'First Principles',
    category: 'Thinking Tools',
    keywords: ['first principle', 'fundamental', 'basic', 'assumption', 'ground up', 'foundation'],
    description: 'Breaking down problems to their most basic elements and building up from there.'
  },
  {
    id: 12,
    name: 'Occam\'s Razor',
    category: 'Thinking Tools',
    keywords: ['simple', 'simplest', 'complexity', 'complicated', 'explanation', 'assumption'],
    description: 'The simplest explanation is usually the correct one.'
  },
  {
    id: 13,
    name: 'Hanlon\'s Razor',
    category: 'Psychology',
    keywords: ['malice', 'stupidity', 'incompetence', 'mistake', 'intention', 'deliberate'],
    description: 'Never attribute to malice that which can be explained by incompetence.'
  },
  {
    id: 14,
    name: 'Network Effects',
    category: 'Economics',
    keywords: ['network', 'user', 'platform', 'value', 'growth', 'viral', 'adoption'],
    description: 'The value of a product increases as more people use it.'
  },
  {
    id: 15,
    name: 'Compounding',
    category: 'Mathematics',
    keywords: ['compound', 'exponential', 'growth', 'interest', 'accumulate', 'snowball'],
    description: 'Small consistent gains accumulate into large results over time.'
  }
];

let apiUrl = 'http://localhost:8000';
let isOnline = false;

/**
 * Initialize the analysis service
 */
function init() {
  console.log('[AnalysisService] Initialized v' + VERSION);
}

/**
 * Cleanup the analysis service
 */
function cleanup() {
  console.log('[AnalysisService] Cleaned up');
}

/**
 * Set the API URL for remote analysis
 * @param {string} url - The API URL
 */
function setApiUrl(url) {
  apiUrl = url;
}

/**
 * Check if remote API is available
 * @returns {Promise<boolean>}
 */
async function checkApiStatus() {
  try {
    const axios = require('axios');
    const response = await axios.get(`${apiUrl}/health`, { timeout: 3000 });
    isOnline = response.status === 200;
    return isOnline;
  } catch (error) {
    isOnline = false;
    return false;
  }
}

/**
 * Analyze text using mental models (local keyword-based analysis)
 * @param {string} text - The text to analyze
 * @returns {object} Analysis results
 */
function analyzeLocal(text) {
  const lowerText = text.toLowerCase();
  const results = [];
  
  for (const model of MENTAL_MODELS) {
    let matchCount = 0;
    const matchedKeywords = [];
    
    for (const keyword of model.keywords) {
      if (lowerText.includes(keyword.toLowerCase())) {
        matchCount++;
        matchedKeywords.push(keyword);
      }
    }
    
    if (matchCount > 0) {
      const relevance = Math.min(matchCount / model.keywords.length * 2, 1);
      results.push({
        id: model.id,
        name: model.name,
        category: model.category,
        description: model.description,
        relevance: Math.round(relevance * 100) / 100,
        matchedKeywords
      });
    }
  }
  
  // Sort by relevance
  results.sort((a, b) => b.relevance - a.relevance);
  
  return {
    success: true,
    mode: 'offline',
    text: text.substring(0, 100) + (text.length > 100 ? '...' : ''),
    models: results.slice(0, 5),
    totalModelsChecked: MENTAL_MODELS.length,
    timestamp: new Date().toISOString()
  };
}

/**
 * Analyze text using remote API
 * @param {string} text - The text to analyze
 * @returns {Promise<object>} Analysis results
 */
async function analyzeRemote(text) {
  try {
    const axios = require('axios');
    const response = await axios.post(`${apiUrl}/api/analysis/latticework`, {
      text,
      top_n: 5
    }, { timeout: 30000 });
    
    return {
      success: true,
      mode: 'online',
      ...response.data
    };
  } catch (error) {
    throw new Error(`Remote analysis failed: ${error.message}`);
  }
}

/**
 * Analyze text (auto-selects local or remote based on availability)
 * @param {string} text - The text to analyze
 * @returns {Promise<object>} Analysis results
 */
async function analyze(text) {
  if (!text || text.trim().length === 0) {
    return {
      success: false,
      error: 'No text provided for analysis'
    };
  }
  
  // Try remote first if we think we're online
  if (isOnline) {
    try {
      return await analyzeRemote(text);
    } catch (error) {
      console.log('[AnalysisService] Remote analysis failed, falling back to local');
      isOnline = false;
    }
  }
  
  // Fall back to local analysis
  return analyzeLocal(text);
}

/**
 * Get all available mental models
 * @returns {object[]} List of mental models
 */
function getModels() {
  return MENTAL_MODELS.map(m => ({
    id: m.id,
    name: m.name,
    category: m.category,
    description: m.description
  }));
}

/**
 * Get model by ID
 * @param {number} id - Model ID
 * @returns {object|null} The model or null
 */
function getModelById(id) {
  return MENTAL_MODELS.find(m => m.id === id) || null;
}

/**
 * Get models by category
 * @param {string} category - Category name
 * @returns {object[]} List of models in category
 */
function getModelsByCategory(category) {
  return MENTAL_MODELS.filter(m => m.category.toLowerCase() === category.toLowerCase());
}

/**
 * Get all categories
 * @returns {string[]} List of categories
 */
function getCategories() {
  return [...new Set(MENTAL_MODELS.map(m => m.category))];
}

/**
 * Get service status
 * @returns {object} Service status
 */
function getStatus() {
  return {
    version: VERSION,
    isOnline,
    apiUrl,
    modelsCount: MENTAL_MODELS.length,
    categories: getCategories()
  };
}

module.exports = {
  VERSION,
  init,
  cleanup,
  setApiUrl,
  checkApiStatus,
  analyze,
  analyzeLocal,
  analyzeRemote,
  getModels,
  getModelById,
  getModelsByCategory,
  getCategories,
  getStatus
};
