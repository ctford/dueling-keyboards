(ns dueling-keyboards.talk
  (:require [overtone.live :refer :all :exclude [stop exp]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [dueling-keyboards.instrument :as instrument]
            [dueling-keyboards.tuning :as tuning]))

;;;;;;;;;;;;;;;;;;
;;; Pythagoras ;;;
;;;;;;;;;;;;;;;;;;

(def pythagorean-comma
  "The difference between an octave constructed out of pure fifths and 2/1."
  531441/524288)

;;;;;;;;;;;;;;;;;;;;;;
;;; Tuning systems ;;;
;;;;;;;;;;;;;;;;;;;;;;

(def pythagorean-tuning
  "Converts midi to hertz, putting the entire Pythagorean comma into one ghastly interval."
  (let [pure 3/2
        wolf (/ pure pythagorean-comma)
        fifths [pure pure pure pure pure pure pure wolf pure pure pure]]
    (tuning/tune fifths)))

(def meantone-temperament
  "Converts midi to hertz, using a variant of Pythagorean tuning designed to get a consonant major third."
  (let [narrow (java.lang.Math/pow 5 1/4)
        wolf (* narrow 128/125)
        fifths [narrow narrow narrow narrow narrow narrow narrow wolf narrow narrow narrow]]
    (tuning/tune fifths)))

(def equal-temperament
  "Converts midi to hertz, spreading out the Pythagorean comma evenly across all the intervals."
  (let [narrow (java.lang.Math/pow 2 7/12)
        fifths [narrow narrow narrow narrow narrow narrow narrow narrow narrow narrow narrow]]
    (tuning/tune fifths)))

; TODO: Demo of tuning.

;;;;;;;;;;;;;;
;;; Norman ;;;
;;;;;;;;;;;;;;

; See picture.

;;;;;;;;;;;;;;;
;;; Baganda ;;;
;;;;;;;;;;;;;;;

(defn baganda-temperament
  "Converts midi to hertz, using a five tone version of equal temperament."
  [pitch]
  (/ (* tuning/concert-a 4) (reduce * (repeat pitch (java.lang.Math/pow 2 1/5)))))

(def akadinda
  (->> (phrase (repeat 1/4) (concat (range 18) [17 17 17 17 18 18] [17 17 17 17 18 18] [17 17 17 12 12 12]))
       (canon/canon (comp (canon/simple 1/6) (canon/interval -4)))
       (canon/canon (comp (canon/simple 1/6) (canon/interval -4)))
       (where :pitch baganda-temperament)))

(comment
  (live/play akadinda)
  (live/jam (var akadinda))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional composition ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (instrument/matooke hertz seconds)))

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
       (canon/canon (canon/simple 4))
       (where :pitch (comp equal-temperament scale/A scale/major))))

; TODO: Add Kolmogorov complexity macros.

(comment
  (live/play row-row)
  (live/jam (var row-row))
)
