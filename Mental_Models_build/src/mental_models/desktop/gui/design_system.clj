(ns mental-models.desktop.gui.design-system
  "Design System for Mental Models Desktop
   
   Principles:
   1. Steve Jobs attention to detail - Every pixel matters
   2. Value Line information density - Maximum data per square inch
   3. M&S + Costco design language - Clean, trustworthy, no-nonsense
   4. Munger's latticework - Organized by mental model categories
   5. Light theme (per user preference) with proper contrast
   
   This module provides a consistent design language across the entire application."
  (:import [java.awt Color Font Dimension Insets RenderingHints Graphics2D
                     GradientPaint BasicStroke LinearGradientPaint]
           [java.awt.geom RoundRectangle2D$Double Path2D$Double]
           [javax.swing JPanel JLabel JButton JTextField JTextArea JScrollPane
                        BorderFactory UIManager SwingConstants Timer]
           [javax.swing.border EmptyBorder CompoundBorder LineBorder AbstractBorder]
           [javax.swing.plaf.basic BasicButtonUI BasicScrollBarUI]))

;; =============================================================================
;; COLOR SYSTEM - M&S + Costco inspired (trustworthy, clean, professional)
;; =============================================================================

(def color-palette
  "Light theme color palette - designed for maximum readability and trust"
  {;; Primary backgrounds
   :bg-primary     (Color. 255 255 255)      ; Pure white - main content
   :bg-secondary   (Color. 250 251 252)      ; Slight gray - panels
   :bg-tertiary    (Color. 245 247 250)      ; Light gray - cards
   :bg-elevated    (Color. 255 255 255)      ; Elevated surfaces
   
   ;; Sidebar - M&S dark navy inspired
   :sidebar-bg     (Color. 24 32 44)         ; Dark navy
   :sidebar-hover  (Color. 35 45 60)         ; Hover state
   :sidebar-active (Color. 45 58 78)         ; Active/selected
   :sidebar-text   (Color. 220 225 235)      ; Light text on dark
   :sidebar-muted  (Color. 140 150 170)      ; Muted text
   
   ;; Text hierarchy
   :text-primary   (Color. 17 24 39)         ; Near black - headings
   :text-secondary (Color. 55 65 81)         ; Dark gray - body
   :text-tertiary  (Color. 107 114 128)      ; Medium gray - labels
   :text-muted     (Color. 156 163 175)      ; Light gray - hints
   :text-disabled  (Color. 209 213 219)      ; Disabled text
   
   ;; Borders and dividers
   :border-light   (Color. 243 244 246)      ; Subtle dividers
   :border-default (Color. 229 231 235)      ; Default borders
   :border-strong  (Color. 209 213 219)      ; Strong borders
   :border-focus   (Color. 99 102 241)       ; Focus rings
   
   ;; Accent colors - Costco-inspired (trustworthy, action-oriented)
   :primary        (Color. 37 99 235)        ; Blue - primary actions
   :primary-hover  (Color. 29 78 216)        ; Blue hover
   :primary-light  (Color. 239 246 255)      ; Blue tint background
   
   :success        (Color. 22 163 74)        ; Green - positive
   :success-light  (Color. 240 253 244)      ; Green tint
   
   :warning        (Color. 234 179 8)        ; Yellow - caution
   :warning-light  (Color. 254 252 232)      ; Yellow tint
   
   :danger         (Color. 220 38 38)        ; Red - errors/critical
   :danger-light   (Color. 254 242 242)      ; Red tint
   
   :info           (Color. 59 130 246)       ; Info blue
   :info-light     (Color. 239 246 255)      ; Info tint
   
   ;; Category colors (for mental model categories)
   :cat-psychology (Color. 139 92 246)       ; Purple
   :cat-economics  (Color. 34 197 94)        ; Green
   :cat-biology    (Color. 236 72 153)       ; Pink
   :cat-physics    (Color. 59 130 246)       ; Blue
   :cat-math       (Color. 245 158 11)       ; Amber
   :cat-systems    (Color. 20 184 166)       ; Teal
   :cat-strategy   (Color. 239 68 68)        ; Red
   :cat-learning   (Color. 168 85 247)})     ; Violet

;; =============================================================================
;; TYPOGRAPHY SYSTEM - Value Line inspired (dense but readable)
;; =============================================================================

(def typography
  "Font definitions optimized for information density"
  {;; Display fonts (rare use - main titles only)
   :display-lg     (Font. "Segoe UI" Font/BOLD 18)
   :display-md     (Font. "Segoe UI" Font/BOLD 14)
   
   ;; Headings
   :heading-lg     (Font. "Segoe UI Semibold" Font/BOLD 12)
   :heading-md     (Font. "Segoe UI Semibold" Font/BOLD 11)
   :heading-sm     (Font. "Segoe UI Semibold" Font/BOLD 10)
   
   ;; Body text - Value Line uses 8-9pt for density
   :body-lg        (Font. "Segoe UI" Font/PLAIN 10)
   :body-md        (Font. "Segoe UI" Font/PLAIN 9)
   :body-sm        (Font. "Segoe UI" Font/PLAIN 8)
   
   ;; Labels and captions
   :label          (Font. "Segoe UI" Font/PLAIN 9)
   :label-sm       (Font. "Segoe UI" Font/PLAIN 8)
   :caption        (Font. "Segoe UI" Font/PLAIN 7)
   
   ;; Data/monospace - for tables and numbers
   :data-lg        (Font. "Consolas" Font/PLAIN 10)
   :data-md        (Font. "Consolas" Font/PLAIN 9)
   :data-sm        (Font. "Consolas" Font/PLAIN 8)
   :data-xs        (Font. "Consolas" Font/PLAIN 7)
   
   ;; Sidebar
   :sidebar-title  (Font. "Segoe UI Semibold" Font/BOLD 11)
   :sidebar-item   (Font. "Segoe UI" Font/PLAIN 10)
   :sidebar-badge  (Font. "Segoe UI" Font/BOLD 8)})

;; =============================================================================
;; SPACING SYSTEM - Consistent spacing scale
;; =============================================================================

(def spacing
  "Spacing scale in pixels - based on 4px grid"
  {:none   0
   :xs     2
   :sm     4
   :md     8
   :lg     12
   :xl     16
   :xxl    24
   :xxxl   32})

(def border-radius
  "Border radius scale"
  {:none   0
   :sm     2
   :md     4
   :lg     6
   :xl     8
   :full   9999})

;; =============================================================================
;; COMPONENT STYLES - Reusable style definitions
;; =============================================================================

(defn create-insets [top right bottom left]
  (Insets. top right bottom left))

(def button-styles
  "Button style presets"
  {:primary   {:bg (:primary color-palette)
               :fg Color/WHITE
               :hover-bg (:primary-hover color-palette)
               :border nil
               :font (:body-md typography)
               :padding (create-insets 4 12 4 12)
               :radius (:md border-radius)}
   
   :secondary {:bg (:bg-secondary color-palette)
               :fg (:text-primary color-palette)
               :hover-bg (:bg-tertiary color-palette)
               :border (:border-default color-palette)
               :font (:body-md typography)
               :padding (create-insets 4 12 4 12)
               :radius (:md border-radius)}
   
   :ghost     {:bg (Color. 0 0 0 0)
               :fg (:text-secondary color-palette)
               :hover-bg (:bg-tertiary color-palette)
               :border nil
               :font (:body-md typography)
               :padding (create-insets 4 8 4 8)
               :radius (:md border-radius)}
   
   :danger    {:bg (:danger color-palette)
               :fg Color/WHITE
               :hover-bg (Color. 185 28 28)
               :border nil
               :font (:body-md typography)
               :padding (create-insets 4 12 4 12)
               :radius (:md border-radius)}
   
   :compact   {:bg (:bg-secondary color-palette)
               :fg (:text-secondary color-palette)
               :hover-bg (:bg-tertiary color-palette)
               :border (:border-light color-palette)
               :font (:body-sm typography)
               :padding (create-insets 2 6 2 6)
               :radius (:sm border-radius)}
   
   :sidebar   {:bg (Color. 0 0 0 0)
               :fg (:sidebar-text color-palette)
               :hover-bg (:sidebar-hover color-palette)
               :active-bg (:sidebar-active color-palette)
               :border nil
               :font (:sidebar-item typography)
               :padding (create-insets 6 12 6 12)
               :radius (:md border-radius)}})

;; =============================================================================
;; CUSTOM BORDERS - Steve Jobs attention to detail
;; =============================================================================

(defn create-rounded-border 
  "Create a rounded border with optional shadow effect"
  ([radius color]
   (create-rounded-border radius color 1))
  ([radius color thickness]
   (proxy [AbstractBorder] []
     (paintBorder [c g x y width height]
       (let [g2 (.create g)]
         (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
         (.setColor g2 color)
         (.setStroke g2 (BasicStroke. thickness))
         (.draw g2 (RoundRectangle2D$Double. (+ x 0.5) (+ y 0.5) (- width 1) (- height 1) radius radius))
         (.dispose g2)))
     (getBorderInsets [c]
       (create-insets (inc thickness) (inc thickness) (inc thickness) (inc thickness)))
     (isBorderOpaque [] false))))

(defn create-card-border
  "Create a subtle card border with shadow effect"
  []
  (CompoundBorder.
    (create-rounded-border (:lg border-radius) (:border-default color-palette))
    (EmptyBorder. (:sm spacing) (:md spacing) (:sm spacing) (:md spacing))))

(defn create-section-border
  "Create a section border with title"
  [title]
  (BorderFactory/createTitledBorder
    (BorderFactory/createLineBorder (:border-default color-palette) 1)
    title
    javax.swing.border.TitledBorder/LEFT
    javax.swing.border.TitledBorder/TOP
    (:label-sm typography)
    (:text-tertiary color-palette)))

;; =============================================================================
;; STYLED COMPONENTS - Factory functions for consistent UI
;; =============================================================================

(defn styled-button
  "Create a styled button with hover effects"
  ([text] (styled-button text :secondary))
  ([text style-key]
   (let [style (get button-styles style-key (:secondary button-styles))
         btn (proxy [JButton] [text]
               (paintComponent [g]
                 (let [g2 (.create g)
                       w (.getWidth this)
                       h (.getHeight this)
                       model (.getModel this)
                       bg (cond
                            (.isPressed model) (or (:active-bg style) (:hover-bg style))
                            (.isRollover model) (:hover-bg style)
                            :else (:bg style))]
                   (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                   (when bg
                     (.setColor g2 bg)
                     (.fill g2 (RoundRectangle2D$Double. 0 0 w h (:radius style) (:radius style))))
                   (when (:border style)
                     (.setColor g2 (:border style))
                     (.draw g2 (RoundRectangle2D$Double. 0.5 0.5 (- w 1) (- h 1) (:radius style) (:radius style))))
                   (.setColor g2 (:fg style))
                   (.setFont g2 (:font style))
                   (let [fm (.getFontMetrics g2)
                         text-width (.stringWidth fm text)
                         text-height (.getHeight fm)
                         x (/ (- w text-width) 2)
                         y (+ (/ (- h text-height) 2) (.getAscent fm))]
                     (.drawString g2 text (int x) (int y)))
                   (.dispose g2))))]
     (.setContentAreaFilled btn false)
     (.setBorderPainted btn false)
     (.setFocusPainted btn false)
     (.setFont btn (:font style))
     (.setForeground btn (:fg style))
     (.setMargin btn (:padding style))
     (.setCursor btn (java.awt.Cursor/getPredefinedCursor java.awt.Cursor/HAND_CURSOR))
     btn)))

(defn styled-label
  "Create a styled label"
  ([text] (styled-label text :body-md :text-primary))
  ([text font-key] (styled-label text font-key :text-primary))
  ([text font-key color-key]
   (let [lbl (JLabel. text)]
     (.setFont lbl (get typography font-key (:body-md typography)))
     (.setForeground lbl (get color-palette color-key (:text-primary color-palette)))
     lbl)))

(defn styled-text-field
  "Create a styled text field with focus effects"
  ([] (styled-text-field 20))
  ([columns]
   (let [tf (JTextField. columns)]
     (.setFont tf (:body-md typography))
     (.setForeground tf (:text-primary color-palette))
     (.setBackground tf (:bg-primary color-palette))
     (.setBorder tf (CompoundBorder.
                      (create-rounded-border (:md border-radius) (:border-default color-palette))
                      (EmptyBorder. 4 8 4 8)))
     (.setCaretColor tf (:primary color-palette))
     tf)))

(defn styled-text-area
  "Create a styled text area"
  ([] (styled-text-area 5 40))
  ([rows cols]
   (let [ta (JTextArea. rows cols)]
     (.setFont ta (:body-md typography))
     (.setForeground ta (:text-primary color-palette))
     (.setBackground ta (:bg-primary color-palette))
     (.setBorder ta (EmptyBorder. 8 8 8 8))
     (.setCaretColor ta (:primary color-palette))
     (.setLineWrap ta true)
     (.setWrapStyleWord ta true)
     ta)))

(defn styled-scroll-pane
  "Create a styled scroll pane with custom scrollbars"
  [component]
  (let [sp (JScrollPane. component)]
    (.setBorder sp (create-rounded-border (:md border-radius) (:border-default color-palette)))
    (.getVerticalScrollBar sp)
    sp))

;; =============================================================================
;; INFORMATION-DENSE COMPONENTS - Value Line style
;; =============================================================================

(defn create-data-cell
  "Create a compact data cell for tables"
  [value & {:keys [align font color width]
            :or {align SwingConstants/LEFT
                 font :data-sm
                 color :text-primary
                 width nil}}]
  (let [lbl (JLabel. (str value))]
    (.setFont lbl (get typography font))
    (.setForeground lbl (get color-palette color))
    (.setHorizontalAlignment lbl align)
    (when width
      (.setPreferredSize lbl (Dimension. width 14)))
    lbl))

(defn create-stat-cell
  "Create a compact statistic cell with label and value"
  [label value & {:keys [value-color]
                  :or {value-color :text-primary}}]
  (let [panel (JPanel. (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 2 0))]
    (.setOpaque panel false)
    (.add panel (styled-label (str label ":") :caption :text-muted))
    (.add panel (styled-label (str value) :data-sm value-color))
    panel))

(defn create-dense-row
  "Create an information-dense row with multiple data points"
  [& items]
  (let [panel (JPanel. (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 4 0))]
    (.setOpaque panel false)
    (doseq [item items]
      (cond
        (string? item) (.add panel (styled-label item :data-sm :text-secondary))
        (vector? item) (.add panel (apply create-stat-cell item))
        :else (.add panel item)))
    panel))

(defn create-mini-chart
  "Create a mini sparkline-style indicator"
  [values & {:keys [width height color]
             :or {width 40 height 12 color :primary}}]
  (let [chart-color (get color-palette color)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (let [g2 (.create g)
                        w (.getWidth this)
                        h (.getHeight this)
                        max-val (apply max values)
                        min-val (apply min values)
                        range (max 1 (- max-val min-val))
                        n (count values)
                        step (/ w (max 1 (dec n)))]
                    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                    (.setColor g2 chart-color)
                    (.setStroke g2 (BasicStroke. 1.5))
                    (let [path (Path2D$Double.)]
                      (doseq [[i v] (map-indexed vector values)]
                        (let [x (* i step)
                              y (- h (* (/ (- v min-val) range) (- h 2)))]
                          (if (zero? i)
                            (.moveTo path x y)
                            (.lineTo path x y))))
                      (.draw g2 path))
                    (.dispose g2))))]
    (.setPreferredSize panel (Dimension. width height))
    (.setOpaque panel false)
    panel))

;; =============================================================================
;; SIDEBAR COMPONENTS - Munger's latticework organization
;; =============================================================================

(defn create-sidebar-item
  "Create a sidebar navigation item with icon and badge support"
  [label icon-char & {:keys [badge on-click active?]
                      :or {badge nil on-click nil active? false}}]
  (let [style (:sidebar button-styles)
        item (proxy [JPanel] []
               (paintComponent [g]
                 (proxy-super paintComponent g)
                 (let [g2 (.create g)
                       w (.getWidth this)
                       h (.getHeight this)]
                   (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                   (when active?
                     (.setColor g2 (:sidebar-active color-palette))
                     (.fill g2 (RoundRectangle2D$Double. 4 0 (- w 8) h 4 4)))
                   (.dispose g2))))]
    (.setLayout item (java.awt.BorderLayout. 8 0))
    (.setOpaque item false)
    (.setPreferredSize item (Dimension. 180 32))
    (.setCursor item (java.awt.Cursor/getPredefinedCursor java.awt.Cursor/HAND_CURSOR))
    
    ;; Icon
    (let [icon-lbl (JLabel. (str icon-char))]
      (.setFont icon-lbl (Font. "Segoe UI Symbol" Font/PLAIN 14))
      (.setForeground icon-lbl (if active? 
                                 (:primary color-palette) 
                                 (:sidebar-muted color-palette)))
      (.add item icon-lbl java.awt.BorderLayout/WEST))
    
    ;; Label
    (let [text-lbl (JLabel. label)]
      (.setFont text-lbl (:sidebar-item typography))
      (.setForeground text-lbl (if active?
                                 Color/WHITE
                                 (:sidebar-text color-palette)))
      (.add item text-lbl java.awt.BorderLayout/CENTER))
    
    ;; Badge (optional)
    (when badge
      (let [badge-lbl (JLabel. (str badge))]
        (.setFont badge-lbl (:sidebar-badge typography))
        (.setForeground badge-lbl (:primary color-palette))
        (.setOpaque badge-lbl true)
        (.setBackground badge-lbl (:primary-light color-palette))
        (.setBorder badge-lbl (EmptyBorder. 1 4 1 4))
        (.add item badge-lbl java.awt.BorderLayout/EAST)))
    
    ;; Click handler
    (when on-click
      (.addMouseListener item
        (proxy [java.awt.event.MouseAdapter] []
          (mouseClicked [e] (on-click))
          (mouseEntered [e] 
            (.setBackground item (:sidebar-hover color-palette))
            (.setOpaque item true)
            (.repaint item))
          (mouseExited [e]
            (.setOpaque item false)
            (.repaint item)))))
    
    item))

(defn create-sidebar-section
  "Create a sidebar section with title and items"
  [title items]
  (let [section (JPanel.)]
    (.setLayout section (javax.swing.BoxLayout. section javax.swing.BoxLayout/Y_AXIS))
    (.setOpaque section false)
    
    ;; Section title
    (let [title-lbl (JLabel. (.toUpperCase title))]
      (.setFont title-lbl (:caption typography))
      (.setForeground title-lbl (:sidebar-muted color-palette))
      (.setBorder title-lbl (EmptyBorder. 12 16 4 16))
      (.setAlignmentX title-lbl 0.0)
      (.add section title-lbl))
    
    ;; Items
    (doseq [item items]
      (.setAlignmentX item 0.0)
      (.add section item))
    
    section))

;; =============================================================================
;; CARD COMPONENTS - M&S clean card design
;; =============================================================================

(defn create-card
  "Create a card container with optional title"
  [& {:keys [title content padding]
      :or {title nil padding :md}}]
  (let [card (JPanel. (java.awt.BorderLayout. 0 (:sm spacing)))]
    (.setBackground card (:bg-elevated color-palette))
    (.setBorder card (create-card-border))
    
    (when title
      (let [header (JPanel. (java.awt.BorderLayout.))]
        (.setOpaque header false)
        (.setBorder header (EmptyBorder. 0 0 (:sm spacing) 0))
        (.add header (styled-label title :heading-sm :text-primary) java.awt.BorderLayout/WEST)
        (.add card header java.awt.BorderLayout/NORTH)))
    
    (when content
      (.add card content java.awt.BorderLayout/CENTER))
    
    card))

(defn create-stat-card
  "Create a compact statistics card"
  [title value & {:keys [subtitle trend icon]
                  :or {subtitle nil trend nil icon nil}}]
  (let [card (JPanel. (java.awt.BorderLayout. 4 2))]
    (.setBackground card (:bg-elevated color-palette))
    (.setBorder card (CompoundBorder.
                       (create-rounded-border (:md border-radius) (:border-light color-palette))
                       (EmptyBorder. 6 8 6 8)))
    (.setPreferredSize card (Dimension. 100 50))
    
    ;; Title row
    (let [title-row (JPanel. (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 4 0))]
      (.setOpaque title-row false)
      (when icon
        (.add title-row (styled-label icon :body-sm :text-muted)))
      (.add title-row (styled-label title :caption :text-tertiary))
      (.add card title-row java.awt.BorderLayout/NORTH))
    
    ;; Value
    (.add card (styled-label (str value) :heading-lg :text-primary) java.awt.BorderLayout/CENTER)
    
    ;; Trend/subtitle
    (when (or subtitle trend)
      (let [footer (JPanel. (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 2 0))]
        (.setOpaque footer false)
        (when trend
          (let [[direction pct] trend
                trend-color (if (= direction :up) :success :danger)
                trend-icon (if (= direction :up) "↑" "↓")]
            (.add footer (styled-label (str trend-icon pct "%") :caption trend-color))))
        (when subtitle
          (.add footer (styled-label subtitle :caption :text-muted)))
        (.add card footer java.awt.BorderLayout/SOUTH)))
    
    card))

;; =============================================================================
;; STATUS INDICATORS - Clear visual feedback
;; =============================================================================

(defn create-status-dot
  "Create a colored status indicator dot"
  [status]
  (let [color (case status
                :connected (:success color-palette)
                :disconnected (:danger color-palette)
                :connecting (:warning color-palette)
                :unknown (:text-muted color-palette)
                (:text-muted color-palette))
        dot (proxy [JPanel] []
              (paintComponent [g]
                (proxy-super paintComponent g)
                (let [g2 (.create g)]
                  (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                  (.setColor g2 color)
                  (.fillOval g2 2 2 6 6)
                  (.dispose g2))))]
    (.setPreferredSize dot (Dimension. 10 10))
    (.setOpaque dot false)
    dot))

(defn create-connection-indicator
  "Create a labeled connection status indicator"
  [label status]
  (let [panel (JPanel. (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 4 0))]
    (.setOpaque panel false)
    (.add panel (create-status-dot status))
    (.add panel (styled-label label :caption :text-tertiary))
    panel))

(defn create-progress-indicator
  "Create a compact progress indicator"
  [progress & {:keys [width height color show-text?]
               :or {width 60 height 4 color :primary show-text? false}}]
  (let [bar-color (get color-palette color)
        bg-color (:border-light color-palette)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (let [g2 (.create g)
                        w (.getWidth this)
                        h (.getHeight this)
                        fill-width (* w (/ (min 100 (max 0 progress)) 100.0))]
                    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                    ;; Background
                    (.setColor g2 bg-color)
                    (.fill g2 (RoundRectangle2D$Double. 0 0 w h 2 2))
                    ;; Progress
                    (.setColor g2 bar-color)
                    (.fill g2 (RoundRectangle2D$Double. 0 0 fill-width h 2 2))
                    (.dispose g2))))]
    (.setPreferredSize panel (Dimension. width height))
    (.setOpaque panel false)
    panel))

;; =============================================================================
;; MICRO-INTERACTIONS - Subtle animations for polish
;; =============================================================================

(defn animate-fade-in
  "Animate a component fading in"
  [component duration-ms]
  (let [start-time (System/currentTimeMillis)
        timer (Timer. 16 nil)]
    (.addActionListener timer
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [e]
          (let [elapsed (- (System/currentTimeMillis) start-time)
                progress (min 1.0 (/ elapsed duration-ms))]
            (if (>= progress 1.0)
              (do
                (.stop timer)
                (.setVisible component true))
              (.repaint component))))))
    (.start timer)))

(defn create-hover-effect
  "Add hover effect to a component"
  [component normal-bg hover-bg]
  (.addMouseListener component
    (proxy [java.awt.event.MouseAdapter] []
      (mouseEntered [e]
        (.setBackground component hover-bg)
        (.repaint component))
      (mouseExited [e]
        (.setBackground component normal-bg)
        (.repaint component)))))

(defn create-ripple-button
  "Create a button with material-design ripple effect"
  [text on-click & {:keys [style]
                    :or {style :primary}}]
  (let [btn (styled-button text style)
        ripple-x (atom 0)
        ripple-y (atom 0)
        ripple-size (atom 0)
        animating? (atom false)]
    (.addMouseListener btn
      (proxy [java.awt.event.MouseAdapter] []
        (mousePressed [e]
          (reset! ripple-x (.getX e))
          (reset! ripple-y (.getY e))
          (reset! ripple-size 0)
          (reset! animating? true)
          (let [timer (Timer. 16 nil)
                max-size (* 2 (max (.getWidth btn) (.getHeight btn)))]
            (.addActionListener timer
              (proxy [java.awt.event.ActionListener] []
                (actionPerformed [_]
                  (if (< @ripple-size max-size)
                    (do
                      (swap! ripple-size + 20)
                      (.repaint btn))
                    (do
                      (.stop timer)
                      (reset! animating? false)
                      (.repaint btn))))))
            (.start timer)))
        (mouseReleased [e]
          (when on-click (on-click)))))
    btn))

;; =============================================================================
;; CATEGORY BADGE - For mental model categories
;; =============================================================================

(defn create-category-badge
  "Create a colored badge for mental model categories"
  [category]
  (let [cat-colors {"Psychology" :cat-psychology
                    "Economics" :cat-economics
                    "Biology" :cat-biology
                    "Physics" :cat-physics
                    "Mathematics" :cat-math
                    "Systems" :cat-systems
                    "Strategy" :cat-strategy
                    "Learning" :cat-learning}
        color-key (get cat-colors category :text-muted)
        color (get color-palette color-key)
        badge (proxy [JLabel] [(subs category 0 (min 3 (count category)))]
                (paintComponent [g]
                  (let [g2 (.create g)
                        w (.getWidth this)
                        h (.getHeight this)]
                    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                    (.setColor g2 (Color. (.getRed color) (.getGreen color) (.getBlue color) 30))
                    (.fill g2 (RoundRectangle2D$Double. 0 0 w h 3 3))
                    (.setColor g2 color)
                    (.setFont g2 (:caption typography))
                    (let [fm (.getFontMetrics g2)
                          text (subs category 0 (min 3 (count category)))
                          x (/ (- w (.stringWidth fm text)) 2)
                          y (+ (/ (- h (.getHeight fm)) 2) (.getAscent fm))]
                      (.drawString g2 text (int x) (int y)))
                    (.dispose g2))))]
    (.setPreferredSize badge (Dimension. 28 14))
    (.setOpaque badge false)
    badge))

;; =============================================================================
;; TOOLTIP SYSTEM - Informative tooltips
;; =============================================================================

(defn set-tooltip
  "Set a styled tooltip on a component"
  [component text]
  (.setToolTipText component 
    (str "<html><body style='font-family:Segoe UI;font-size:9px;padding:4px;'>" 
         text 
         "</body></html>")))

;; =============================================================================
;; INITIALIZATION - Apply global UI settings
;; =============================================================================

(defn init-design-system!
  "Initialize the design system and apply global UI settings"
  []
  ;; Set system look and feel properties
  (UIManager/put "Button.arc" 6)
  (UIManager/put "Component.arc" 6)
  (UIManager/put "TextComponent.arc" 6)
  (UIManager/put "ScrollBar.width" 8)
  (UIManager/put "ScrollBar.thumbArc" 4)
  (UIManager/put "TabbedPane.selectedBackground" (:bg-primary color-palette))
  (UIManager/put "Table.rowHeight" 18)
  (UIManager/put "Table.intercellSpacing" (Dimension. 4 2))
  (UIManager/put "ToolTip.background" (:bg-elevated color-palette))
  (UIManager/put "ToolTip.foreground" (:text-primary color-palette))
  (UIManager/put "ToolTip.border" (create-rounded-border 4 (:border-default color-palette)))
  
  ;; Enable anti-aliasing globally
  (System/setProperty "awt.useSystemAAFontSettings" "on")
  (System/setProperty "swing.aatext" "true"))
