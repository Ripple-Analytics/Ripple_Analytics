(ns mental-models.models.complexity-part7
  "Mental Models - Complexity Category (Part 7)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 7)
;; ============================================

(register-model
 {:name "ethos-pathos-logos"
  :category "rhetoric"
  :originator "Aristotle"
  :description "Three modes of persuasion: credibility, emotion, and logic"
  :key-insight "Effective persuasion uses all three appropriately"
  :application "Build credibility; connect emotionally; argue logically"
  :failure-modes
  [(failure "logos-only" "medium"
            "Relying only on logic"
            :signals ["Unpersuasive despite being right" "Ignored"]
            :safeguards ["Add ethos and pathos" "Emotional connection"])
   (failure "pathos-manipulation" "high"
            "Manipulating through emotion alone"
            :signals ["Backlash" "Distrust when discovered"]
            :safeguards ["Ethical persuasion" "Substance backing"])
   (failure "ethos-neglect" "high"
            "Not establishing credibility"
            :signals ["Dismissed" "Not taken seriously"]
            :safeguards ["Build credibility first" "Demonstrate expertise"])
   (failure "audience-mismatch" "medium"
            "Wrong mix for audience"
            :signals ["Message doesn't land" "Resistance"]
            :safeguards ["Know audience" "Adapt approach"])
   (failure "authenticity-gap" "high"
            "Persuasion techniques without substance"
            :signals ["Seen as manipulative" "Trust loss"]
            :safeguards ["Genuine belief" "Authentic communication"])]})

(register-model
 {:name "steelmanning"
  :category "rhetoric"
  :originator "Philosophy tradition"
  :description "Arguing against the strongest version of opposing view"
  :key-insight "Defeating strong arguments is more convincing than defeating weak ones"
  :application "Strengthen opponent's argument before countering"
  :failure-modes
  [(failure "strawmanning" "high"
            "Attacking weak version of argument"
            :signals ["Easy wins" "Unconvinced opponents"]
            :safeguards ["Steelman first" "Strongest version"])
   (failure "over-steelmanning" "medium"
            "Making opponent's argument stronger than they can defend"
            :signals ["Arguing against phantom" "Wasted effort"]
            :safeguards ["Realistic steelman" "Actual positions"])
   (failure "steelman-paralysis" "medium"
            "Unable to counter after steelmanning"
            :signals ["Convinced by opponent" "No response"]
            :safeguards ["Prepare counter" "Know your position"])
   (failure "performative-steelmanning" "medium"
            "Steelmanning for show, not understanding"
            :signals ["Superficial" "Missed nuance"]
            :safeguards ["Genuine engagement" "Deep understanding"])
   (failure "steelman-avoidance" "high"
            "Avoiding steelmanning difficult positions"
            :signals ["Weak arguments" "Echo chamber"]
            :safeguards ["Engage strongest" "Intellectual honesty"])]})

;; ============================================
;; Export Functions
;; ============================================

(defn export-all-models
  "Export all models and metadata."
  []
  {:models @!models
   :categories @!categories
   :failure-modes @!failure-modes
   :total-models (count @!models)
   :total-failure-modes (count @!failure-modes)})

(defn model-summary
  "Get a summary of a model."
  [name]
  (when-let [model (get-model name)]
    {:name (:name model)
     :category (:category model)
     :originator (:originator model)
     :description (:description model)
     :failure-mode-count (count (:failure-modes model))}))

(defn all-model-summaries
  "Get summaries of all models."
  []
  (map #(model-summary (:name %)) (vals @!models)))
;; ============================================