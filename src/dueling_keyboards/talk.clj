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

(defn sine-wave [& freqs]
  (doseq [f freqs] (inst/sine-wave f)))

(defn organ [& freqs]
  (doseq [f freqs] (inst/organ f)))

; This makes some ratios work out nicely, and doesn't materially
; affect anything even though the real figure is closer to 340.
(def speed-of-sound 500)

(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (some-> hertz (inst/organ :dur seconds :vol 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dueling Keyboards    ;;;
;;;                      ;;;
;;; Chris Ford (@ctford) ;;;
;;; ThoughtWorks         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;




















;;;;;;;;;;;;;;;;;;;
;;; Periodicity ;;;
;;;;;;;;;;;;;;;;;;;


(comment
  (sine-wave 400)

  (sine-wave 400 399)

  (organ 400 399)

)

; sin(x) + sin(y) = 2 * sin(1/2 * (x+y)) * cos(1/2 * (x-y))











;;;;;;;;;;;;;;;;;;
;;; Consonance ;;;
;;;;;;;;;;;;;;;;;;


(comment

    (organ 400 400)

    (organ 400 500)

    (organ 400 600)

    (organ 400 700)

    (organ 400 800)

    (organ 400 (* 400 (Math/sqrt 2)))

)






;;;;;;;;;;;;;;
;;; Chords ;;;
;;;;;;;;;;;;;;


(defn m->hz [wavelength]
  (/ speed-of-sound wavelength))

(comment

    (organ 400 500 600)

    (organ 416.66666 500 625)

    (organ (m->hz 6/5) (m->hz 5/5) (m->hz 4/5))

    (organ 400 (* 400 4/3) (* 400 5/3))

)







;;;;;;;;;;;;;;;;;;
;;; Pythagoras ;;;
;;;;;;;;;;;;;;;;;;


(def pythagorean-comma
  "The discrepency between an octave built from 3/2s and 2/1."
  (/ (Math/pow 3 12) (Math/pow 2 19)))


















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














;;;;;;;;;;;;;;;;;;
;;; Comparison ;;;
;;;;;;;;;;;;;;;;;;


(comment

  (let [consonant-fifth (phrase [5 1] [[69 76] nil])
        dissonant-fifth (phrase [5 1] [[70 77] nil])
        interval consonant-fifth]
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
  (let [read-def (fn [s] (-> s repl/source-fn read-string last))]
    (-> sym read-def print-str count)))

(defn result-length [expr]
  (-> expr print-str count))

(comment
  (description-length air-on-a-g-string)
  (result-length air-on-a-g-string)

  (description-length row-row)
  (result-length row-row)
)




