(ns mental-models.ui.theme
  "Theme System - Dark Mode with M&S + Costco Design Language
   Clean typography, high information density, monochrome with red accents"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

;; -- Design Tokens -----------------------------------------------------------

(def colors
  {:light {:bg-primary "#ffffff"
           :bg-secondary "#fafafa"
           :bg-tertiary "#f5f5f5"
           :bg-elevated "#ffffff"
           :text-primary "#171717"
           :text-secondary "#525252"
           :text-tertiary "#737373"
           :text-muted "#a3a3a3"
           :border "#e5e5e5"
           :border-subtle "#f0f0f0"
           :accent "#dc2626"        ; Costco red
           :accent-hover "#b91c1c"
           :accent-muted "#fef2f2"
           :success "#059669"
           :warning "#d97706"
           :error "#dc2626"
           :info "#0284c7"}
   
   :dark {:bg-primary "#0a0a0a"
          :bg-secondary "#171717"
          :bg-tertiary "#262626"
          :bg-elevated "#1f1f1f"
          :text-primary "#fafafa"
          :text-secondary "#a3a3a3"
          :text-tertiary "#737373"
          :text-muted "#525252"
          :border "#262626"
          :border-subtle "#1f1f1f"
          :accent "#ef4444"
          :accent-hover "#f87171"
          :accent-muted "#450a0a"
          :success "#10b981"
          :warning "#f59e0b"
          :error "#ef4444"
          :info "#3b82f6"}})

(def typography
  {:font-family "'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
   :font-mono "'JetBrains Mono', 'SF Mono', Consolas, monospace"
   
   ;; M&S-inspired type scale - clean, readable
   :text-xs "11px"
   :text-sm "12px"
   :text-base "13px"
   :text-lg "14px"
   :text-xl "16px"
   :text-2xl "18px"
   :text-3xl "24px"
   
   ;; Costco-inspired density - compact line heights
   :leading-tight "1.2"
   :leading-normal "1.4"
   :leading-relaxed "1.6"
   
   ;; Font weights
   :font-normal "400"
   :font-medium "500"
   :font-semibold "600"
   :font-bold "700"})

(def spacing
  {:space-0 "0"
   :space-1 "4px"
   :space-2 "8px"
   :space-3 "12px"
   :space-4 "16px"
   :space-5 "20px"
   :space-6 "24px"
   :space-8 "32px"
   :space-10 "40px"
   :space-12 "48px"})

(def radii
  {:radius-none "0"
   :radius-sm "2px"
   :radius-md "4px"
   :radius-lg "6px"
   :radius-xl "8px"})

(def shadows
  {:shadow-none "none"
   :shadow-sm "0 1px 2px rgba(0,0,0,0.04)"
   :shadow-md "0 2px 4px rgba(0,0,0,0.06)"
   :shadow-lg "0 4px 8px rgba(0,0,0,0.08)"
   :shadow-xl "0 8px 16px rgba(0,0,0,0.1)"})

;; -- CSS Generation ----------------------------------------------------------

(defn generate-css-variables
  "Generate CSS custom properties for a theme"
  [theme-name]
  (let [theme-colors (get colors theme-name)]
    (str
     (apply str
            (for [[k v] theme-colors]
              (str "  --color-" (name k) ": " v ";\n")))
     (apply str
            (for [[k v] typography]
              (str "  --" (name k) ": " v ";\n")))
     (apply str
            (for [[k v] spacing]
              (str "  --" (name k) ": " v ";\n")))
     (apply str
            (for [[k v] radii]
              (str "  --" (name k) ": " v ";\n")))
     (apply str
            (for [[k v] shadows]
              (str "  --" (name k) ": " v ";\n"))))))

(def base-styles
  "/* Base Reset & Typography */
  *, *::before, *::after {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }
  
  html {
    font-size: 16px;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }
  
  body {
    font-family: var(--font-family);
    font-size: var(--text-base);
    line-height: var(--leading-normal);
    color: var(--color-text-primary);
    background: var(--color-bg-primary);
    transition: background-color 0.2s, color 0.2s;
  }
  
  /* Typography */
  h1, h2, h3, h4, h5, h6 {
    font-weight: var(--font-semibold);
    line-height: var(--leading-tight);
    color: var(--color-text-primary);
  }
  
  h1 { font-size: var(--text-3xl); }
  h2 { font-size: var(--text-2xl); }
  h3 { font-size: var(--text-xl); }
  h4 { font-size: var(--text-lg); }
  
  p { margin-bottom: var(--space-2); }
  
  a {
    color: var(--color-accent);
    text-decoration: none;
    transition: color 0.15s;
  }
  a:hover { color: var(--color-accent-hover); }
  
  code, pre {
    font-family: var(--font-mono);
    font-size: var(--text-sm);
  }
  
  /* Utility Classes */
  .text-primary { color: var(--color-text-primary); }
  .text-secondary { color: var(--color-text-secondary); }
  .text-tertiary { color: var(--color-text-tertiary); }
  .text-muted { color: var(--color-text-muted); }
  .text-accent { color: var(--color-accent); }
  .text-success { color: var(--color-success); }
  .text-warning { color: var(--color-warning); }
  .text-error { color: var(--color-error); }
  
  .bg-primary { background: var(--color-bg-primary); }
  .bg-secondary { background: var(--color-bg-secondary); }
  .bg-tertiary { background: var(--color-bg-tertiary); }
  .bg-elevated { background: var(--color-bg-elevated); }
  .bg-accent { background: var(--color-accent); }
  .bg-accent-muted { background: var(--color-accent-muted); }
  
  .border { border: 1px solid var(--color-border); }
  .border-subtle { border: 1px solid var(--color-border-subtle); }
  
  /* Layout */
  .container {
    width: 100%;
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 var(--space-4);
  }
  
  .flex { display: flex; }
  .flex-col { flex-direction: column; }
  .items-center { align-items: center; }
  .justify-between { justify-content: space-between; }
  .gap-1 { gap: var(--space-1); }
  .gap-2 { gap: var(--space-2); }
  .gap-3 { gap: var(--space-3); }
  .gap-4 { gap: var(--space-4); }
  
  /* Cards - M&S clean style */
  .card {
    background: var(--color-bg-elevated);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-md);
    padding: var(--space-4);
    transition: box-shadow 0.15s, border-color 0.15s;
  }
  .card:hover {
    border-color: var(--color-border);
    box-shadow: var(--shadow-md);
  }
  
  /* Buttons - Costco bold style */
  .btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: var(--space-2);
    padding: var(--space-2) var(--space-4);
    font-size: var(--text-sm);
    font-weight: var(--font-medium);
    border-radius: var(--radius-md);
    border: 1px solid transparent;
    cursor: pointer;
    transition: all 0.15s;
  }
  
  .btn-primary {
    background: var(--color-accent);
    color: white;
  }
  .btn-primary:hover {
    background: var(--color-accent-hover);
  }
  
  .btn-secondary {
    background: transparent;
    border-color: var(--color-border);
    color: var(--color-text-primary);
  }
  .btn-secondary:hover {
    background: var(--color-bg-tertiary);
  }
  
  .btn-ghost {
    background: transparent;
    color: var(--color-text-secondary);
  }
  .btn-ghost:hover {
    background: var(--color-bg-tertiary);
    color: var(--color-text-primary);
  }
  
  /* Tables - Costco information density */
  .table {
    width: 100%;
    border-collapse: collapse;
    font-size: var(--text-sm);
  }
  .table th {
    text-align: left;
    font-weight: var(--font-semibold);
    color: var(--color-text-secondary);
    padding: var(--space-2) var(--space-3);
    border-bottom: 1px solid var(--color-border);
    font-size: var(--text-xs);
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  .table td {
    padding: var(--space-2) var(--space-3);
    border-bottom: 1px solid var(--color-border-subtle);
  }
  .table tr:hover td {
    background: var(--color-bg-secondary);
  }
  
  /* Metrics - Value Line density */
  .metric {
    display: flex;
    flex-direction: column;
    gap: var(--space-1);
  }
  .metric-label {
    font-size: var(--text-xs);
    color: var(--color-text-tertiary);
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  .metric-value {
    font-size: var(--text-xl);
    font-weight: var(--font-semibold);
    color: var(--color-text-primary);
    font-variant-numeric: tabular-nums;
  }
  .metric-delta {
    font-size: var(--text-xs);
    font-weight: var(--font-medium);
  }
  .metric-delta.positive { color: var(--color-success); }
  .metric-delta.negative { color: var(--color-error); }
  
  /* Badges */
  .badge {
    display: inline-flex;
    align-items: center;
    padding: 2px 6px;
    font-size: var(--text-xs);
    font-weight: var(--font-medium);
    border-radius: var(--radius-sm);
    background: var(--color-bg-tertiary);
    color: var(--color-text-secondary);
  }
  .badge-accent {
    background: var(--color-accent-muted);
    color: var(--color-accent);
  }
  .badge-success {
    background: rgba(5, 150, 105, 0.1);
    color: var(--color-success);
  }
  .badge-warning {
    background: rgba(217, 119, 6, 0.1);
    color: var(--color-warning);
  }
  .badge-error {
    background: rgba(220, 38, 38, 0.1);
    color: var(--color-error);
  }
  
  /* Inputs */
  .input {
    width: 100%;
    padding: var(--space-2) var(--space-3);
    font-size: var(--text-sm);
    background: var(--color-bg-primary);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-md);
    color: var(--color-text-primary);
    transition: border-color 0.15s, box-shadow 0.15s;
  }
  .input:focus {
    outline: none;
    border-color: var(--color-accent);
    box-shadow: 0 0 0 2px var(--color-accent-muted);
  }
  .input::placeholder {
    color: var(--color-text-muted);
  }
  
  /* Sidebar */
  .sidebar {
    width: 240px;
    height: 100vh;
    background: var(--color-bg-secondary);
    border-right: 1px solid var(--color-border);
    padding: var(--space-4);
    position: fixed;
    left: 0;
    top: 0;
    overflow-y: auto;
  }
  
  .sidebar-nav-item {
    display: flex;
    align-items: center;
    gap: var(--space-2);
    padding: var(--space-2) var(--space-3);
    font-size: var(--text-sm);
    color: var(--color-text-secondary);
    border-radius: var(--radius-md);
    cursor: pointer;
    transition: all 0.15s;
  }
  .sidebar-nav-item:hover {
    background: var(--color-bg-tertiary);
    color: var(--color-text-primary);
  }
  .sidebar-nav-item.active {
    background: var(--color-accent-muted);
    color: var(--color-accent);
  }
  
  /* Main Content */
  .main-content {
    margin-left: 240px;
    padding: var(--space-6);
    min-height: 100vh;
  }
  
  /* Scrollbar */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }
  ::-webkit-scrollbar-track {
    background: var(--color-bg-secondary);
  }
  ::-webkit-scrollbar-thumb {
    background: var(--color-border);
    border-radius: 4px;
  }
  ::-webkit-scrollbar-thumb:hover {
    background: var(--color-text-muted);
  }
  
  /* Animations */
  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }
  
  @keyframes slideUp {
    from { opacity: 0; transform: translateY(8px); }
    to { opacity: 1; transform: translateY(0); }
  }
  
  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.5; }
  }
  
  .animate-fadeIn { animation: fadeIn 0.2s ease-out; }
  .animate-slideUp { animation: slideUp 0.2s ease-out; }
  .animate-pulse { animation: pulse 2s infinite; }
  
  /* Transitions */
  .transition-colors {
    transition: color 0.15s, background-color 0.15s, border-color 0.15s;
  }
  .transition-all {
    transition: all 0.15s;
  }")

;; -- Theme Stylesheet --------------------------------------------------------

(defn theme-stylesheet
  "Generate complete stylesheet for a theme"
  [theme-name]
  (str
   ":root {\n"
   (generate-css-variables :light)
   "}\n\n"
   "[data-theme='dark'] {\n"
   (generate-css-variables :dark)
   "}\n\n"
   "@media (prefers-color-scheme: dark) {\n"
   "  :root:not([data-theme='light']) {\n"
   (generate-css-variables :dark)
   "  }\n"
   "}\n\n"
   base-styles))

;; -- Electric Components -----------------------------------------------------

(e/defn ThemeStyles []
  (dom/style (dom/text (theme-stylesheet :light))))

(e/defn ThemeToggle [current-theme on-change]
  (dom/button
    (dom/props {:class "btn btn-ghost"
                :title (if (= current-theme :dark) "Switch to light mode" "Switch to dark mode")})
    (dom/on "click" (fn [_] (on-change (if (= current-theme :dark) :light :dark))))
    (if (= current-theme :dark)
      (dom/text "☀")
      (dom/text "☾"))))

(e/defn ThemeProvider [theme children]
  (dom/div
    (dom/props {:data-theme (name theme)})
    (ThemeStyles.)
    children))
