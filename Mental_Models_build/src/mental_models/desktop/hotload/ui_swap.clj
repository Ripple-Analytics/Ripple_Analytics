(ns mental-models.desktop.hotload.ui-swap
  "UI Hot-Swap System for Mental Models Desktop
   
   Enables updating UI components without restarting the application.
   Based on Munger's principle of 'continuous improvement' - 
   the system should get better without interruption.
   
   Features:
   - Component-level hot swap (replace individual panels)
   - Theme hot reload (change colors/fonts instantly)
   - Layout hot swap (change layouts without restart)
   - State preservation across UI updates
   - Smooth transitions during swap"
  (:require [clojure.string :as str])
  (:import [javax.swing SwingUtilities JPanel JFrame Timer]
           [java.awt CardLayout BorderLayout Color Font Component Container]
           [java.awt.event ActionListener]))

;; =============================================================================
;; UI Component Registry
;; =============================================================================

(def *ui-registry 
  "Registry of swappable UI components"
  (atom {:components {}      ;; name -> {:component JComponent :factory fn :parent Container}
         :themes {}          ;; name -> theme-map
         :current-theme nil
         :swap-history []}))

(defn register-component! 
  "Register a UI component for hot-swapping
   
   Parameters:
   - name: Unique identifier for the component
   - component: The actual Swing component
   - factory-fn: Function that creates a new instance of the component
   - parent: Parent container holding the component"
  [name component factory-fn parent]
  (swap! *ui-registry assoc-in [:components name]
         {:component component
          :factory factory-fn
          :parent parent
          :created-at (System/currentTimeMillis)}))

(defn unregister-component! [name]
  "Remove a component from the registry"
  (swap! *ui-registry update :components dissoc name))

;; =============================================================================
;; Theme System
;; =============================================================================

(def default-theme
  {:name "default"
   :colors {:bg-primary (Color. 255 255 255)
            :bg-secondary (Color. 248 250 252)
            :bg-tertiary (Color. 241 245 249)
            :text-primary (Color. 15 23 42)
            :text-secondary (Color. 71 85 105)
            :text-muted (Color. 148 163 184)
            :border (Color. 226 232 240)
            :primary (Color. 99 102 241)
            :success (Color. 34 197 94)
            :warning (Color. 245 158 11)
            :danger (Color. 239 68 68)
            :accent (Color. 220 38 38)}  ;; Red accent for M&S/Costco style
   :fonts {:title (Font. "Segoe UI" Font/BOLD 12)
           :subtitle (Font. "Segoe UI" Font/BOLD 10)
           :heading (Font. "Segoe UI" Font/BOLD 9)
           :body (Font. "Segoe UI" Font/PLAIN 9)
           :small (Font. "Segoe UI" Font/PLAIN 8)
           :mono (Font. "Consolas" Font/PLAIN 9)}})

(def dark-theme
  {:name "dark"
   :colors {:bg-primary (Color. 17 24 39)
            :bg-secondary (Color. 31 41 55)
            :bg-tertiary (Color. 55 65 81)
            :text-primary (Color. 249 250 251)
            :text-secondary (Color. 209 213 219)
            :text-muted (Color. 156 163 175)
            :border (Color. 75 85 99)
            :primary (Color. 129 140 248)
            :success (Color. 74 222 128)
            :warning (Color. 251 191 36)
            :danger (Color. 248 113 113)
            :accent (Color. 239 68 68)}
   :fonts {:title (Font. "Segoe UI" Font/BOLD 12)
           :subtitle (Font. "Segoe UI" Font/BOLD 10)
           :heading (Font. "Segoe UI" Font/BOLD 9)
           :body (Font. "Segoe UI" Font/PLAIN 9)
           :small (Font. "Segoe UI" Font/PLAIN 8)
           :mono (Font. "Consolas" Font/PLAIN 9)}})

(defn register-theme! [theme]
  "Register a theme for hot-swapping"
  (swap! *ui-registry assoc-in [:themes (:name theme)] theme))

(defn get-current-theme []
  "Get the currently active theme"
  (or (get-in @*ui-registry [:themes (:current-theme @*ui-registry)])
      default-theme))

;; =============================================================================
;; Component Hot-Swap
;; =============================================================================

(defn swap-component! 
  "Hot-swap a registered component with a new instance
   
   Parameters:
   - name: Name of the registered component
   - new-factory-fn: (optional) New factory function, uses existing if not provided
   
   Returns:
   - {:success true/false :old-component :new-component}"
  ([name] (swap-component! name nil))
  ([name new-factory-fn]
   (SwingUtilities/invokeAndWait
    (fn []
      (when-let [reg (get-in @*ui-registry [:components name])]
        (let [{:keys [component factory parent]} reg
              factory-fn (or new-factory-fn factory)
              new-component (factory-fn)]
          
          ;; Find and replace the component in parent
          (when (and parent component)
            (let [layout (.getLayout parent)
                  constraints (when (instance? BorderLayout layout)
                                ;; Try to preserve BorderLayout constraints
                                (cond
                                  (= component (.getComponent parent BorderLayout/CENTER)) BorderLayout/CENTER
                                  (= component (.getComponent parent BorderLayout/NORTH)) BorderLayout/NORTH
                                  (= component (.getComponent parent BorderLayout/SOUTH)) BorderLayout/SOUTH
                                  (= component (.getComponent parent BorderLayout/EAST)) BorderLayout/EAST
                                  (= component (.getComponent parent BorderLayout/WEST)) BorderLayout/WEST
                                  :else nil))]
              
              ;; Remove old component
              (.remove parent component)
              
              ;; Add new component
              (if constraints
                (.add parent new-component constraints)
                (.add parent new-component))
              
              ;; Update registry
              (swap! *ui-registry assoc-in [:components name :component] new-component)
              (when new-factory-fn
                (swap! *ui-registry assoc-in [:components name :factory] new-factory-fn))
              
              ;; Record in history
              (swap! *ui-registry update :swap-history conj
                     {:name name
                      :timestamp (System/currentTimeMillis)
                      :success true})
              
              ;; Refresh UI
              (.revalidate parent)
              (.repaint parent)))
          
          {:success true
           :old-component component
           :new-component new-component}))))))

(defn swap-all-components! 
  "Swap all registered components (useful after theme change)"
  []
  (doseq [name (keys (:components @*ui-registry))]
    (swap-component! name)))

;; =============================================================================
;; Theme Hot-Swap
;; =============================================================================

(defn apply-theme-to-component! [component theme]
  "Recursively apply theme to a component and its children"
  (let [colors (:colors theme)
        fonts (:fonts theme)]
    
    ;; Apply to this component
    (when (instance? JPanel component)
      (.setBackground component (:bg-primary colors)))
    
    (.setForeground component (:text-primary colors))
    (.setFont component (:body fonts))
    
    ;; Recursively apply to children
    (when (instance? Container component)
      (doseq [child (.getComponents component)]
        (apply-theme-to-component! child theme)))))

(defn swap-theme! 
  "Hot-swap the current theme
   
   Parameters:
   - theme-name: Name of the registered theme to switch to
   
   Returns:
   - {:success true/false :old-theme :new-theme}"
  [theme-name]
  (SwingUtilities/invokeAndWait
   (fn []
     (when-let [new-theme (get-in @*ui-registry [:themes theme-name])]
       (let [old-theme-name (:current-theme @*ui-registry)]
         
         ;; Update current theme
         (swap! *ui-registry assoc :current-theme theme-name)
         
         ;; Apply theme to all registered components
         (doseq [[_ reg] (:components @*ui-registry)]
           (when-let [component (:component reg)]
             (apply-theme-to-component! component new-theme)))
         
         ;; Record in history
         (swap! *ui-registry update :swap-history conj
                {:type :theme-swap
                 :from old-theme-name
                 :to theme-name
                 :timestamp (System/currentTimeMillis)})
         
         {:success true
          :old-theme old-theme-name
          :new-theme theme-name})))))

;; =============================================================================
;; Animated Transitions
;; =============================================================================

(defn fade-transition! 
  "Perform a fade transition when swapping components
   
   Parameters:
   - old-component: Component being replaced
   - new-component: New component
   - duration-ms: Transition duration in milliseconds
   - on-complete: Callback when transition completes"
  [old-component new-component duration-ms on-complete]
  (let [steps 20
        step-delay (/ duration-ms steps)
        step-alpha (/ 1.0 steps)]
    
    ;; Start with new component invisible
    (.setVisible new-component false)
    
    ;; Fade out old, fade in new
    (let [timer (Timer. step-delay nil)
          step-count (atom 0)]
      (.addActionListener timer
        (reify ActionListener
          (actionPerformed [_ _]
            (swap! step-count inc)
            (let [progress (/ @step-count steps)]
              (if (>= progress 1.0)
                (do
                  (.stop timer)
                  (.setVisible old-component false)
                  (.setVisible new-component true)
                  (when on-complete (on-complete)))
                (do
                  ;; Cross-fade effect would go here
                  ;; For now, just do a simple swap at midpoint
                  (when (>= progress 0.5)
                    (.setVisible old-component false)
                    (.setVisible new-component true))))))))
      (.start timer))))

;; =============================================================================
;; Panel Factory Helpers
;; =============================================================================

(defn create-swappable-panel 
  "Create a panel that can be hot-swapped
   
   Parameters:
   - name: Unique name for the panel
   - content-fn: Function that creates the panel content
   - parent: Parent container
   
   Returns:
   - The created panel (also registered for hot-swap)"
  [name content-fn parent]
  (let [panel (content-fn)]
    (register-component! name panel content-fn parent)
    panel))

(defn with-hot-swap 
  "Wrapper that makes a component factory hot-swappable
   
   Usage:
   (with-hot-swap \"my-panel\" 
     (fn [] (create-my-panel))
     parent-container)"
  [name factory-fn parent]
  (let [component (factory-fn)]
    (register-component! name component factory-fn parent)
    component))

;; =============================================================================
;; Live Code Reload Integration
;; =============================================================================

(defn reload-ui-from-source! 
  "Reload UI components from updated source code
   This is called after namespace reload to refresh UI"
  [namespace-sym]
  (try
    ;; Reload the namespace
    (require namespace-sym :reload)
    
    ;; Swap all components that might have changed
    (swap-all-components!)
    
    {:success true :namespace namespace-sym}
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; State Preservation
;; =============================================================================

(defn capture-component-state [component]
  "Capture the state of a component for preservation
   Handles common Swing component states"
  (cond
    (instance? javax.swing.JTextField component)
    {:type :text-field :text (.getText component) :caret (.getCaretPosition component)}
    
    (instance? javax.swing.JTextArea component)
    {:type :text-area :text (.getText component) :caret (.getCaretPosition component)}
    
    (instance? javax.swing.JList component)
    {:type :list :selected-index (.getSelectedIndex component)}
    
    (instance? javax.swing.JComboBox component)
    {:type :combo-box :selected-index (.getSelectedIndex component)}
    
    (instance? javax.swing.JScrollPane component)
    (let [vp (.getViewport component)
          pos (.getViewPosition vp)]
      {:type :scroll-pane :scroll-x (.x pos) :scroll-y (.y pos)})
    
    :else nil))

(defn restore-component-state! [component state]
  "Restore captured state to a component"
  (when state
    (case (:type state)
      :text-field (do (.setText component (:text state))
                      (.setCaretPosition component (min (:caret state) (count (:text state)))))
      :text-area (do (.setText component (:text state))
                     (.setCaretPosition component (min (:caret state) (count (:text state)))))
      :list (.setSelectedIndex component (:selected-index state))
      :combo-box (.setSelectedIndex component (:selected-index state))
      :scroll-pane (let [vp (.getViewport component)]
                     (.setViewPosition vp (java.awt.Point. (:scroll-x state) (:scroll-y state))))
      nil)))

(defn swap-with-state-preservation! 
  "Swap a component while preserving user state (scroll position, text, selections)"
  [name new-factory-fn]
  (when-let [reg (get-in @*ui-registry [:components name])]
    (let [old-component (:component reg)
          ;; Capture state from old component and its children
          old-states (into {} 
                          (for [c (tree-seq #(instance? Container %) 
                                           #(.getComponents %) 
                                           old-component)
                                :when (some? (capture-component-state c))]
                            [(.getName c) (capture-component-state c)]))]
      
      ;; Perform the swap
      (let [result (swap-component! name new-factory-fn)]
        
        ;; Restore states to new component
        (when (:success result)
          (let [new-component (:new-component result)]
            (doseq [c (tree-seq #(instance? Container %) 
                               #(.getComponents %) 
                               new-component)]
              (when-let [state (get old-states (.getName c))]
                (restore-component-state! c state)))))
        
        result))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-ui-swap! []
  "Initialize the UI hot-swap system"
  (register-theme! default-theme)
  (register-theme! dark-theme)
  (swap! *ui-registry assoc :current-theme "default")
  (println "[UI-SWAP] UI hot-swap system initialized"))

(defn get-swap-stats []
  "Get statistics about UI swaps"
  (let [history (:swap-history @*ui-registry)]
    {:total-swaps (count history)
     :component-count (count (:components @*ui-registry))
     :theme-count (count (:themes @*ui-registry))
     :current-theme (:current-theme @*ui-registry)
     :last-swap (last history)}))
