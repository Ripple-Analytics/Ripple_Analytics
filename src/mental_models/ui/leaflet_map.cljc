(ns mental-models.ui.leaflet-map
  "World Map Visualization with Leaflet.js
   Spatial-temporal analysis of mental model usage and decisions"
  #?(:cljs (:require-macros [mental-models.ui.leaflet-map :refer [e-fn]]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:cljs [goog.object :as gobj])))

;; -- Leaflet Integration (ClojureScript) -------------------------------------

#?(:cljs
   (defn init-leaflet-map
     "Initialize Leaflet map in a container element"
     [container-id {:keys [center zoom] :or {center [20 0] zoom 2}}]
     (when-let [L (gobj/get js/window "L")]
       (let [map-instance (.map L container-id #js {:center (clj->js center)
                                                     :zoom zoom
                                                     :zoomControl true
                                                     :scrollWheelZoom true})]
         ;; Add tile layer (OpenStreetMap)
         (.addTo
          (.tileLayer L "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
                      #js {:attribution "&copy; OpenStreetMap contributors"
                           :maxZoom 18})
          map-instance)
         map-instance))))

#?(:cljs
   (defn add-marker
     "Add a marker to the map"
     [map-instance {:keys [lat lng popup icon-color]
                    :or {icon-color "#dc2626"}}]
     (when-let [L (gobj/get js/window "L")]
       (let [icon (.divIcon L #js {:className "custom-marker"
                                    :html (str "<div style='background:" icon-color ";width:12px;height:12px;border-radius:50%;border:2px solid white;box-shadow:0 2px 4px rgba(0,0,0,0.3);'></div>")
                                    :iconSize #js [12 12]
                                    :iconAnchor #js [6 6]})
             marker (.addTo (.marker L #js [lat lng] #js {:icon icon}) map-instance)]
         (when popup
           (.bindPopup marker popup))
         marker))))

#?(:cljs
   (defn add-circle
     "Add a circle to the map (for heatmap-like visualization)"
     [map-instance {:keys [lat lng radius color opacity]
                    :or {radius 50000 color "#dc2626" opacity 0.3}}]
     (when-let [L (gobj/get js/window "L")]
       (.addTo
        (.circle L #js [lat lng]
                 #js {:radius radius
                      :color color
                      :fillColor color
                      :fillOpacity opacity
                      :weight 1})
        map-instance))))

#?(:cljs
   (defn add-polyline
     "Add a polyline connecting points"
     [map-instance points {:keys [color weight opacity]
                           :or {color "#525252" weight 2 opacity 0.6}}]
     (when-let [L (gobj/get js/window "L")]
       (.addTo
        (.polyline L (clj->js points)
                   #js {:color color
                        :weight weight
                        :opacity opacity})
        map-instance))))

;; -- Data Structures ---------------------------------------------------------

(def sample-locations
  "Sample decision/model usage locations for visualization"
  [{:id 1 :lat 40.7128 :lng -74.0060 :city "New York" :decisions 45 :top-model "First Principles"}
   {:id 2 :lat 51.5074 :lng -0.1278 :city "London" :decisions 38 :top-model "Second-Order Thinking"}
   {:id 3 :lat 35.6762 :lng 139.6503 :city "Tokyo" :decisions 32 :top-model "Inversion"}
   {:id 4 :lat -33.8688 :lng 151.2093 :city "Sydney" :decisions 28 :top-model "Margin of Safety"}
   {:id 5 :lat 37.7749 :lng -122.4194 :city "San Francisco" :decisions 52 :top-model "Network Effects"}
   {:id 6 :lat 52.5200 :lng 13.4050 :city "Berlin" :decisions 25 :top-model "Feedback Loops"}
   {:id 7 :lat 1.3521 :lng 103.8198 :city "Singapore" :decisions 41 :top-model "Comparative Advantage"}
   {:id 8 :lat 55.7558 :lng 37.6173 :city "Moscow" :decisions 18 :top-model "Game Theory"}
   {:id 9 :lat -23.5505 :lng -46.6333 :city "São Paulo" :decisions 22 :top-model "Opportunity Cost"}
   {:id 10 :lat 19.0760 :lng 72.8777 :city "Mumbai" :decisions 35 :top-model "Compound Interest"}])

(def model-origins
  "Origins of key mental models (thinkers' locations)"
  [{:model "Confirmation Bias" :thinker "Peter Wason" :lat 51.5074 :lng -0.1278 :year 1960}
   {:model "Availability Heuristic" :thinker "Kahneman & Tversky" :lat 32.0853 :lng 34.7818 :year 1973}
   {:model "First Principles" :thinker "Aristotle" :lat 37.9838 :lng 23.7275 :year -350}
   {:model "Margin of Safety" :thinker "Benjamin Graham" :lat 40.7128 :lng -74.0060 :year 1934}
   {:model "Circle of Competence" :thinker "Warren Buffett" :lat 41.2565 :lng -95.9345 :year 1996}
   {:model "Antifragility" :thinker "Nassim Taleb" :lat 33.8938 :lng 35.5018 :year 2012}
   {:model "Creative Destruction" :thinker "Joseph Schumpeter" :lat 48.2082 :lng 16.3738 :year 1942}
   {:model "Network Effects" :thinker "Robert Metcalfe" :lat 42.3601 :lng -71.0589 :year 1980}])

;; -- Electric UI Components --------------------------------------------------

(e/defn MapStyles []
  (dom/style
    (dom/text
     "#world-map {
        width: 100%;
        height: 500px;
        background: #f0f0f0;
        border-radius: 4px;
        border: 1px solid #e5e5e5;
      }
      .leaflet-container {
        font-family: 'Inter', sans-serif;
      }
      .custom-popup {
        font-size: 12px;
      }
      .custom-popup h4 {
        margin: 0 0 4px 0;
        font-size: 13px;
        font-weight: 600;
      }
      .custom-popup .stat {
        display: flex;
        justify-content: space-between;
        padding: 2px 0;
        border-bottom: 1px solid #e5e5e5;
      }
      .custom-popup .stat:last-child {
        border-bottom: none;
      }
      .map-legend {
        position: absolute;
        bottom: 20px;
        right: 20px;
        background: white;
        padding: 12px;
        border-radius: 4px;
        border: 1px solid #e5e5e5;
        font-size: 11px;
        z-index: 1000;
      }
      .legend-item {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 2px 0;
      }
      .legend-dot {
        width: 10px;
        height: 10px;
        border-radius: 50%;
      }")))

(e/defn MapLegend []
  (dom/div
    (dom/props {:class "map-legend"})
    (dom/div
      (dom/props {:style {:font-weight "600" :margin-bottom "8px"}})
      (dom/text "Legend"))
    (dom/div
      (dom/props {:class "legend-item"})
      (dom/div
        (dom/props {:class "legend-dot"
                    :style {:background "#dc2626"}}))
      (dom/text "High activity (40+)"))
    (dom/div
      (dom/props {:class "legend-item"})
      (dom/div
        (dom/props {:class "legend-dot"
                    :style {:background "#525252"}}))
      (dom/text "Medium activity (20-39)"))
    (dom/div
      (dom/props {:class "legend-item"})
      (dom/div
        (dom/props {:class "legend-dot"
                    :style {:background "#a3a3a3"}}))
      (dom/text "Low activity (<20)"))))

(e/defn LocationStats [locations]
  (dom/div
    (dom/props {:class "card"
                :style {:margin-top "16px"}})
    (dom/div
      (dom/props {:class "card-header"})
      (dom/text "Activity by Location"))
    (dom/table
      (dom/props {:class "data-table"})
      (dom/thead
        (dom/tr
          (dom/th (dom/text "City"))
          (dom/th (dom/text "Decisions"))
          (dom/th (dom/text "Top Model"))
          (dom/th (dom/text "Trend"))))
      (dom/tbody
        (e/for [{:keys [city decisions top-model]} (sort-by :decisions > locations)]
          (dom/tr
            (dom/td
              (dom/props {:style {:font-weight "500"}})
              (dom/text city))
            (dom/td
              (dom/props {:style {:font-variant-numeric "tabular-nums"}})
              (dom/text (str decisions)))
            (dom/td
              (dom/span
                (dom/props {:class "tag"})
                (dom/text top-model)))
            (dom/td
              (dom/span
                (dom/props {:style {:color "#16a34a" :font-size "11px"}})
                (dom/text "↑ 12%")))))))))

(e/defn ModelOrigins [origins]
  (dom/div
    (dom/props {:class "card"
                :style {:margin-top "16px"}})
    (dom/div
      (dom/props {:class "card-header"})
      (dom/text "Mental Model Origins"))
    (dom/table
      (dom/props {:class "data-table"})
      (dom/thead
        (dom/tr
          (dom/th (dom/text "Model"))
          (dom/th (dom/text "Thinker"))
          (dom/th (dom/text "Year"))))
      (dom/tbody
        (e/for [{:keys [model thinker year]} (sort-by :year origins)]
          (dom/tr
            (dom/td
              (dom/props {:style {:font-weight "500"}})
              (dom/text model))
            (dom/td
              (dom/props {:style {:color "#525252"}})
              (dom/text thinker))
            (dom/td
              (dom/props {:style {:font-variant-numeric "tabular-nums"}})
              (dom/text (if (neg? year)
                          (str (Math/abs year) " BCE")
                          (str year))))))))))

(e/defn WorldMapPage []
  (dom/div
    (dom/props {:style {:padding "24px"}})
    
    ;; Include Leaflet CSS
    (dom/link
      (dom/props {:rel "stylesheet"
                  :href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"}))
    
    ;; Include Leaflet JS
    (dom/script
      (dom/props {:src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"}))
    
    (MapStyles.)
    
    ;; Header
    (dom/div
      (dom/props {:style {:display "flex"
                          :justify-content "space-between"
                          :align-items "center"
                          :margin-bottom "16px"}})
      (dom/div
        (dom/h3
          (dom/props {:style {:font-size "16px"
                              :font-weight "600"
                              :margin "0"}})
          (dom/text "Global Mental Model Usage"))
        (dom/p
          (dom/props {:style {:font-size "12px"
                              :color "#525252"
                              :margin "4px 0 0"}})
          (dom/text "Spatial-temporal analysis of decision patterns")))
      
      ;; View controls
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px"}})
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Activity"))
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Origins"))
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Connections"))))
    
    ;; Map container
    (dom/div
      (dom/props {:style {:position "relative"}})
      (dom/div
        (dom/props {:id "world-map"}))
      (MapLegend.))
    
    ;; Two column layout for stats
    (dom/div
      (dom/props {:style {:display "grid"
                          :grid-template-columns "1fr 1fr"
                          :gap "16px"}})
      (LocationStats. sample-locations)
      (ModelOrigins. model-origins))
    
    ;; Initialize map after render
    #?(:cljs
       (e/on-unmount
        (fn []
          ;; Cleanup map instance if needed
          nil)))
    
    ;; Map initialization script
    (dom/script
      (dom/text
       "setTimeout(function() {
          if (window.L && !window.worldMap) {
            var map = L.map('world-map').setView([20, 0], 2);
            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
              attribution: '&copy; OpenStreetMap contributors',
              maxZoom: 18
            }).addTo(map);
            
            // Add markers for activity locations
            var locations = [
              {lat: 40.7128, lng: -74.0060, city: 'New York', decisions: 45},
              {lat: 51.5074, lng: -0.1278, city: 'London', decisions: 38},
              {lat: 35.6762, lng: 139.6503, city: 'Tokyo', decisions: 32},
              {lat: -33.8688, lng: 151.2093, city: 'Sydney', decisions: 28},
              {lat: 37.7749, lng: -122.4194, city: 'San Francisco', decisions: 52},
              {lat: 52.5200, lng: 13.4050, city: 'Berlin', decisions: 25},
              {lat: 1.3521, lng: 103.8198, city: 'Singapore', decisions: 41},
              {lat: 55.7558, lng: 37.6173, city: 'Moscow', decisions: 18},
              {lat: -23.5505, lng: -46.6333, city: 'São Paulo', decisions: 22},
              {lat: 19.0760, lng: 72.8777, city: 'Mumbai', decisions: 35}
            ];
            
            locations.forEach(function(loc) {
              var color = loc.decisions >= 40 ? '#dc2626' : 
                          loc.decisions >= 20 ? '#525252' : '#a3a3a3';
              var radius = Math.sqrt(loc.decisions) * 50000;
              
              L.circle([loc.lat, loc.lng], {
                radius: radius,
                color: color,
                fillColor: color,
                fillOpacity: 0.3,
                weight: 1
              }).addTo(map).bindPopup('<b>' + loc.city + '</b><br>' + loc.decisions + ' decisions');
              
              L.circleMarker([loc.lat, loc.lng], {
                radius: 6,
                color: '#ffffff',
                fillColor: color,
                fillOpacity: 1,
                weight: 2
              }).addTo(map);
            });
            
            window.worldMap = map;
          }
        }, 500);"))))
