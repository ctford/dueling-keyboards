(ns dueling-keyboards.talk
  (:require [overtone.live :refer :all :exclude [stop exp sharp flat]]
            [clojure.repl :as repl]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.chord :as chord]
            [leipzig.scale :refer [A B C low sharp flat major]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [dueling-keyboards.tuning :as tuning]
            [dueling-keyboards.instrument :as inst]))

;;;;;;;;;;;;;;;;;;;
;;; Periodicity ;;;
;;;;;;;;;;;;;;;;;;;

(defn sine-wave [& freqs]
  (doseq [f freqs] (inst/sine-wave f)))

(comment

  (sine-wave 400)

  (sine-wave 400 399)

)

;;;;;;;;;;;;;;;;;;
;;; Consonance ;;;
;;;;;;;;;;;;;;;;;;

(defn organs [& freqs]
  (doseq [f freqs] (inst/organ f)))

(comment

    (organs 400 400)

    (organs 400 800)

    (organs 400 600)

    (organs 400 (* 400 (Math/sqrt 2)))

)

;;;;;;;;;;;;;;
;;; Chords ;;;
;;;;;;;;;;;;;;

(def speed-of-sound 340)

(defn m->hz [wavelength]
  (/ speed-of-sound wavelength))

(comment

    ; V
    (organs 300 400 500)

    ; VIb
    (organs (* 4.1666666 100) (* 5 100) (* 6.25 100))

    ; VIb
    (organs (* 25/6 100) (* 25/5 100) (* 25/4 100))

    ; VIb
    (organs (-> 6 (* 17/125) m->hz) (-> 5 (* 17/125) m->hz) (-> 4 (* 17/125) m->hz))

    ; I
    (organs 400 (* 400 4/3) (* 400 5/3))

)

;;;;;;;;;;;;;;;;;;
;;; Pythagoras ;;;
;;;;;;;;;;;;;;;;;;

(def pythagorean-comma
  "The discrepency between an octave built from 3/2s and 2/1."
  531441/524288)












;;;;;;;;;;;;;;;;;;;;;;
;;; Tuning systems ;;;
;;;;;;;;;;;;;;;;;;;;;;

(def pythagorean-tuning
  "Converts midi to hertz, putting the entire Pythagorean comma
   into one ghastly interval."
  (let [pure 3/2
        wolf (/ pure pythagorean-comma)
        fifths [pure pure pure pure pure pure
                pure wolf pure pure pure]]
    (tuning/tune fifths)))







(def meantone-temperament
  "Converts midi to hertz, using a variant of Pythagorean tuning
   designed to get a pure 5/4 major third."
  (let [narrow (Math/pow 5 1/4)
        wolf (* narrow 128/125)
        fifths [narrow narrow narrow narrow narrow narrow
                narrow wolf narrow narrow narrow]]
    (tuning/tune fifths)))











(def equal-temperament
  "Converts midi to hertz, spreading out the Pythagorean comma
   evenly across all the intervals."
  (let [narrow (Math/pow 2 7/12)
        fifths [narrow narrow narrow narrow narrow narrow
                narrow narrow narrow narrow narrow]]
    (tuning/tune fifths)))












(comment

  (let [consonant-fifth (phrase [5 1] [[69 76] nil])
        dissonant-fifth (phrase [5 1] [[70 77] nil])
        interval dissonant-fifth]
    (->>
      interval (where :pitch pythagorean-tuning)
      (then (->> interval (where :pitch meantone-temperament)))
      (then (->> interval (where :pitch equal-temperament)))
      live/play))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional composition ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def row-row
  "A simple melody built from durations and pitches."
  ; Row, row, row  your boat,
  (->> (phrase [3/3  3/3  2/3  1/3  3/3]
               [  0    0    0    1    2])
       (then
         ; Gent-ly   down the  stream,
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  2    1    2    3    4]))
       (then    ; Merrily, merrily, merrily, merrily,
             (phrase (repeat 12 1/3)
                     (mapcat (partial repeat 3) [7 4 2 0])))
       (then
         ; Life is   but  a    dream!
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  4    3    2    1    0]))
       (canon/canon #(->> ((canon/simple 4) %)
                          (canon/canon (canon/simple 4))))
       (where :pitch (comp low A major))))

(comment

  (->> row-row
       ;(where :pitch pythagorean-tuning)
       (where :pitch meantone-temperament)
       ;(where :pitch equal-temperament)
       live/play)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Kolmogorov complexity ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def air-on-a-g-string
  (repeat 1000 \G))

(defmacro description-length [sym]
  (let [definition (-> sym repl/source-fn read-string last)]
    (-> definition print-str count)))

(defn result-length [sym]
  (-> sym print-str count))

(comment
  (description-length air-on-a-g-string)
  (result-length air-on-a-g-string)

  (description-length row-row)
  (result-length row-row)
  )






(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (organ hertz :dur seconds :vol 0.1)))
