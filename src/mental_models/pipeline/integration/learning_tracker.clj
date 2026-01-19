(ns mental-models.pipeline.integration.learning-tracker
  "Learning Tracker Module
   
   Mental model learning progress:
   - Learning path management
   - Progress tracking
   - Skill assessment
   - Spaced repetition
   - Achievement system"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; LEARNING TRACKER STATE
;; =============================================================================

(defonce tracker-state (atom {:learners (ConcurrentHashMap.)
                              :learning-paths (ConcurrentHashMap.)
                              :progress (ConcurrentHashMap.)
                              :assessments (ConcurrentHashMap.)
                              :achievements (ConcurrentHashMap.)
                              :review-queue (ConcurrentHashMap.)
                              :session-count (AtomicLong. 0)
                              :config {:review-intervals [1 3 7 14 30 60]
                                       :mastery-threshold 0.8
                                       :min-reviews 3}}))

;; =============================================================================
;; LEARNER MANAGEMENT
;; =============================================================================

(defn create-learner!
  "Create a learner profile."
  [learner-id {:keys [name level preferences goals]}]
  (log/info "Creating learner" {:id learner-id :name name})
  (.put ^ConcurrentHashMap (:learners @tracker-state) learner-id
        {:id learner-id
         :name name
         :level (or level :beginner)
         :preferences (or preferences {})
         :goals (or goals [])
         :total-sessions 0
         :total-time-minutes 0
         :streak-days 0
         :last-activity nil
         :created-at (System/currentTimeMillis)}))

(defn get-learner
  "Get a learner by ID."
  [learner-id]
  (.get ^ConcurrentHashMap (:learners @tracker-state) learner-id))

(defn update-learner!
  "Update a learner profile."
  [learner-id updates]
  (when-let [learner (get-learner learner-id)]
    (let [updated (merge learner updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:learners @tracker-state) learner-id updated)
      updated)))

(defn list-learners
  "List all learners."
  [& {:keys [level limit]}]
  (let [learners (vals (:learners @tracker-state))]
    (cond->> learners
      level (filter #(= (:level %) level))
      limit (take limit))))

;; =============================================================================
;; LEARNING PATHS
;; =============================================================================

(defn create-learning-path!
  "Create a learning path."
  [path-id {:keys [name description mental-models prerequisites difficulty estimated-hours]}]
  (log/info "Creating learning path" {:id path-id :name name})
  (.put ^ConcurrentHashMap (:learning-paths @tracker-state) path-id
        {:id path-id
         :name name
         :description description
         :mental-models (or mental-models [])
         :prerequisites (or prerequisites [])
         :difficulty (or difficulty :intermediate)
         :estimated-hours (or estimated-hours 10)
         :modules (count mental-models)
         :enabled true
         :created-at (System/currentTimeMillis)}))

(defn get-learning-path
  "Get a learning path by ID."
  [path-id]
  (.get ^ConcurrentHashMap (:learning-paths @tracker-state) path-id))

(defn list-learning-paths
  "List all learning paths."
  [& {:keys [difficulty enabled-only]}]
  (let [paths (vals (:learning-paths @tracker-state))]
    (cond->> paths
      enabled-only (filter :enabled)
      difficulty (filter #(= (:difficulty %) difficulty)))))

(defn enroll-in-path!
  "Enroll a learner in a learning path."
  [learner-id path-id]
  (when-let [path (get-learning-path path-id)]
    (let [progress-id (str learner-id "-" path-id)
          progress {:id progress-id
                    :learner-id learner-id
                    :path-id path-id
                    :status :enrolled
                    :current-module 0
                    :completed-modules []
                    :started-at (System/currentTimeMillis)
                    :last-activity (System/currentTimeMillis)}]
      (.put ^ConcurrentHashMap (:progress @tracker-state) progress-id progress)
      (log/info "Learner enrolled" {:learner learner-id :path path-id})
      progress)))

;; =============================================================================
;; PROGRESS TRACKING
;; =============================================================================

(defn get-progress
  "Get progress for a learner on a path."
  [learner-id path-id]
  (.get ^ConcurrentHashMap (:progress @tracker-state) (str learner-id "-" path-id)))

(defn update-progress!
  "Update learning progress."
  [learner-id path-id {:keys [module-completed time-spent score]}]
  (let [progress-id (str learner-id "-" path-id)]
    (when-let [progress (.get ^ConcurrentHashMap (:progress @tracker-state) progress-id)]
      (let [path (get-learning-path path-id)
            new-completed (if module-completed
                            (conj (:completed-modules progress) module-completed)
                            (:completed-modules progress))
            all-complete? (= (count new-completed) (count (:mental-models path)))
            updated (-> progress
                        (assoc :completed-modules new-completed)
                        (assoc :current-module (count new-completed))
                        (assoc :status (if all-complete? :completed :in-progress))
                        (assoc :last-activity (System/currentTimeMillis))
                        (update :total-time-minutes + (or time-spent 0)))]
        (.put ^ConcurrentHashMap (:progress @tracker-state) progress-id updated)
        (when all-complete?
          (events/publish! :learning/path-completed {:learner learner-id :path path-id}))
        updated))))

(defn get-learner-progress
  "Get all progress for a learner."
  [learner-id]
  (filter #(= (:learner-id %) learner-id)
          (vals (:progress @tracker-state))))

;; =============================================================================
;; SKILL ASSESSMENT
;; =============================================================================

(defn create-assessment!
  "Create a skill assessment."
  [assessment-id {:keys [learner-id mental-model questions]}]
  (log/debug "Creating assessment" {:id assessment-id :model mental-model})
  (.put ^ConcurrentHashMap (:assessments @tracker-state) assessment-id
        {:id assessment-id
         :learner-id learner-id
         :mental-model mental-model
         :questions (or questions [])
         :status :pending
         :score nil
         :created-at (System/currentTimeMillis)}))

(defn submit-assessment!
  "Submit assessment answers and calculate score."
  [assessment-id answers]
  (when-let [assessment (.get ^ConcurrentHashMap (:assessments @tracker-state) assessment-id)]
    (let [questions (:questions assessment)
          correct (count (filter (fn [[q a]]
                                   (= (:correct-answer q) a))
                                 (map vector questions answers)))
          score (if (pos? (count questions))
                  (/ correct (count questions))
                  0)
          mastery-threshold (get-in @tracker-state [:config :mastery-threshold])
          updated (assoc assessment
                         :answers answers
                         :score score
                         :passed (>= score mastery-threshold)
                         :status :completed
                         :completed-at (System/currentTimeMillis))]
      (.put ^ConcurrentHashMap (:assessments @tracker-state) assessment-id updated)
      (log/info "Assessment completed" {:id assessment-id :score score})
      updated)))

(defn get-assessment
  "Get an assessment by ID."
  [assessment-id]
  (.get ^ConcurrentHashMap (:assessments @tracker-state) assessment-id))

(defn get-learner-assessments
  "Get all assessments for a learner."
  [learner-id & {:keys [mental-model status]}]
  (let [assessments (vals (:assessments @tracker-state))]
    (cond->> assessments
      true (filter #(= (:learner-id %) learner-id))
      mental-model (filter #(= (:mental-model %) mental-model))
      status (filter #(= (:status %) status)))))

;; =============================================================================
;; SPACED REPETITION
;; =============================================================================

(defn calculate-next-review
  "Calculate next review time using spaced repetition."
  [review-count last-score]
  (let [intervals (get-in @tracker-state [:config :review-intervals])
        interval-idx (min review-count (dec (count intervals)))
        base-interval (nth intervals interval-idx)
        ;; Adjust based on score
        score-multiplier (if (>= last-score 0.8) 1.2 0.8)
        adjusted-interval (* base-interval score-multiplier 24 60 60 1000)]
    (+ (System/currentTimeMillis) (long adjusted-interval))))

(defn schedule-review!
  "Schedule a review for a mental model."
  [learner-id mental-model {:keys [score review-count]}]
  (let [review-id (str learner-id "-" mental-model)
        next-review (calculate-next-review (or review-count 0) (or score 0.5))
        review {:id review-id
                :learner-id learner-id
                :mental-model mental-model
                :review-count (inc (or review-count 0))
                :last-score score
                :next-review next-review
                :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:review-queue @tracker-state) review-id review)
    (log/debug "Review scheduled" {:learner learner-id :model mental-model})
    review))

(defn get-due-reviews
  "Get reviews that are due for a learner."
  [learner-id]
  (let [now (System/currentTimeMillis)
        reviews (vals (:review-queue @tracker-state))]
    (->> reviews
         (filter #(= (:learner-id %) learner-id))
         (filter #(<= (:next-review %) now))
         (sort-by :next-review))))

(defn complete-review!
  "Complete a review and reschedule."
  [learner-id mental-model score]
  (let [review-id (str learner-id "-" mental-model)
        existing (.get ^ConcurrentHashMap (:review-queue @tracker-state) review-id)
        review-count (if existing (:review-count existing) 0)]
    (schedule-review! learner-id mental-model {:score score :review-count review-count})))

;; =============================================================================
;; ACHIEVEMENTS
;; =============================================================================

(defn define-achievement!
  "Define an achievement."
  [achievement-id {:keys [name description criteria badge-icon points]}]
  (log/debug "Defining achievement" {:id achievement-id :name name})
  (.put ^ConcurrentHashMap (:achievements @tracker-state) achievement-id
        {:id achievement-id
         :name name
         :description description
         :criteria criteria
         :badge-icon badge-icon
         :points (or points 10)
         :earners []}))

(defn check-achievement
  "Check if a learner has earned an achievement."
  [learner-id achievement-id]
  (when-let [achievement (.get ^ConcurrentHashMap (:achievements @tracker-state) achievement-id)]
    (let [criteria (:criteria achievement)
          learner (get-learner learner-id)
          progress (get-learner-progress learner-id)]
      (when (fn? criteria)
        (criteria learner progress)))))

(defn award-achievement!
  "Award an achievement to a learner."
  [learner-id achievement-id]
  (when-let [achievement (.get ^ConcurrentHashMap (:achievements @tracker-state) achievement-id)]
    (let [already-earned? (some #{learner-id} (:earners achievement))]
      (when-not already-earned?
        (.put ^ConcurrentHashMap (:achievements @tracker-state) achievement-id
              (update achievement :earners conj learner-id))
        (log/info "Achievement awarded" {:learner learner-id :achievement achievement-id})
        (events/publish! :learning/achievement-earned {:learner learner-id
                                                       :achievement achievement-id
                                                       :points (:points achievement)})
        true))))

(defn get-learner-achievements
  "Get achievements earned by a learner."
  [learner-id]
  (filter #(some #{learner-id} (:earners %))
          (vals (:achievements @tracker-state))))

;; =============================================================================
;; LEARNING SESSIONS
;; =============================================================================

(defn start-session!
  "Start a learning session."
  [learner-id {:keys [mental-model path-id activity-type]}]
  (.incrementAndGet ^AtomicLong (:session-count @tracker-state))
  (metrics/inc-counter! :learningtracker/sessions)
  (let [session-id (str learner-id "-" (System/currentTimeMillis))
        session {:id session-id
                 :learner-id learner-id
                 :mental-model mental-model
                 :path-id path-id
                 :activity-type (or activity-type :study)
                 :started-at (System/currentTimeMillis)
                 :status :active}]
    (update-learner! learner-id {:last-activity (System/currentTimeMillis)})
    (log/info "Session started" {:session session-id :learner learner-id})
    session))

(defn end-session!
  "End a learning session."
  [session {:keys [completed score notes]}]
  (let [duration-ms (- (System/currentTimeMillis) (:started-at session))
        duration-minutes (/ duration-ms 60000)
        ended-session (assoc session
                             :ended-at (System/currentTimeMillis)
                             :duration-minutes duration-minutes
                             :completed completed
                             :score score
                             :notes notes
                             :status :completed)]
    ;; Update learner stats
    (update-learner! (:learner-id session)
                     {:total-sessions (inc (:total-sessions (get-learner (:learner-id session)) 0))
                      :total-time-minutes (+ (:total-time-minutes (get-learner (:learner-id session)) 0)
                                             duration-minutes)})
    (log/info "Session ended" {:session (:id session) :duration duration-minutes})
    ended-session))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-learner-stats
  "Get statistics for a learner."
  [learner-id]
  (let [learner (get-learner learner-id)
        progress (get-learner-progress learner-id)
        assessments (get-learner-assessments learner-id)
        achievements (get-learner-achievements learner-id)
        due-reviews (get-due-reviews learner-id)]
    {:learner learner
     :paths-enrolled (count progress)
     :paths-completed (count (filter #(= (:status %) :completed) progress))
     :assessments-taken (count assessments)
     :assessments-passed (count (filter :passed assessments))
     :achievements-earned (count achievements)
     :total-points (reduce + (map :points achievements))
     :reviews-due (count due-reviews)}))

(defn get-tracker-stats
  "Get overall tracker statistics."
  []
  {:learners (.size ^ConcurrentHashMap (:learners @tracker-state))
   :learning-paths (.size ^ConcurrentHashMap (:learning-paths @tracker-state))
   :progress-records (.size ^ConcurrentHashMap (:progress @tracker-state))
   :assessments (.size ^ConcurrentHashMap (:assessments @tracker-state))
   :achievements (.size ^ConcurrentHashMap (:achievements @tracker-state))
   :reviews-queued (.size ^ConcurrentHashMap (:review-queue @tracker-state))
   :total-sessions (.get ^AtomicLong (:session-count @tracker-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-default-paths!
  "Initialize default learning paths."
  []
  (log/info "Initializing default learning paths")
  ;; Cognitive Biases Path
  (create-learning-path! "cognitive-biases"
                         {:name "Cognitive Biases Mastery"
                          :description "Learn to recognize and counter cognitive biases"
                          :mental-models ["confirmation-bias" "availability-heuristic" "anchoring"
                                          "loss-aversion" "sunk-cost" "hindsight-bias"]
                          :difficulty :intermediate
                          :estimated-hours 15})
  ;; Decision Frameworks Path
  (create-learning-path! "decision-frameworks"
                         {:name "Decision Making Frameworks"
                          :description "Master frameworks for better decisions"
                          :mental-models ["second-order-thinking" "inversion" "circle-of-competence"
                                          "margin-of-safety" "opportunity-cost"]
                          :difficulty :advanced
                          :estimated-hours 20})
  ;; Munger Essentials Path
  (create-learning-path! "munger-essentials"
                         {:name "Charlie Munger Essentials"
                          :description "Core mental models from Charlie Munger"
                          :mental-models ["incentives" "inversion" "circle-of-competence"
                                          "lollapalooza-effect"]
                          :difficulty :beginner
                          :estimated-hours 10})
  (log/info "Default learning paths initialized" {:count 3}))

(defn init-default-achievements!
  "Initialize default achievements."
  []
  (log/info "Initializing default achievements")
  (define-achievement! "first-model"
                       {:name "First Steps"
                        :description "Complete your first mental model"
                        :points 10})
  (define-achievement! "bias-buster"
                       {:name "Bias Buster"
                        :description "Master 5 cognitive biases"
                        :points 50})
  (define-achievement! "decision-master"
                       {:name "Decision Master"
                        :description "Complete the Decision Frameworks path"
                        :points 100})
  (define-achievement! "streak-7"
                       {:name "Week Warrior"
                        :description "Maintain a 7-day learning streak"
                        :points 25})
  (log/info "Default achievements initialized" {:count 4}))

(defn init-learning-tracker!
  "Initialize learning tracker."
  []
  (log/info "Initializing learning tracker")
  ;; Register feature flag
  (flags/register-flag! "learning-tracker" "Enable learning tracker" true)
  ;; Create metrics
  (metrics/create-counter! :learningtracker/sessions "Learning sessions started")
  (metrics/create-gauge! :learningtracker/learners "Total learners"
                         #(.size ^ConcurrentHashMap (:learners @tracker-state)))
  ;; Initialize defaults
  (init-default-paths!)
  (init-default-achievements!)
  (log/info "Learning tracker initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-learning-tracker-status []
  {:enabled (flags/is-enabled? "learning-tracker")
   :learners (.size ^ConcurrentHashMap (:learners @tracker-state))
   :learning-paths (.size ^ConcurrentHashMap (:learning-paths @tracker-state))
   :progress-records (.size ^ConcurrentHashMap (:progress @tracker-state))
   :stats (get-tracker-stats)
   :config (:config @tracker-state)})
