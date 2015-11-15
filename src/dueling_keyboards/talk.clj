(ns dueling-keyboards.talk
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [dueling-keyboards.instrument :as instrument]))

;;;;;;;;;;;;;;;;;;
;;; Pythagoras ;;;
;;;;;;;;;;;;;;;;;;

(def pythagorean-comma
  "The difference between an octave constructed out of pure fifths and 2/1."
  531441/524288)

(def concert-a 440)
(defn align-concert-a
  [tuning]
  (fn [midi] (-> midi (- 69) tuning (* concert-a))))

(defn temper
  [midi ratios]
  (cond
    (< midi 0) (* 1/2 (temper (+ midi 12) ratios))
    (> midi 11) (* 2 (temper (- midi 12) ratios))
    :otherwise (nth ratios midi)))

(defn tune
  [incremental-ratios]
  (let [root 69
        geometric-progression (partial reductions * 1)
        ratios (->> (geometric-progression incremental-ratios)
                    (map (fn normalise [ratio] (if (< ratio 2) ratio (normalise (/ ratio 2)))))
                    sort)]
    (align-concert-a #(temper % ratios))))

(def pythagorean-tuning
  "Converts midi to hertz, putting the entire Pythagorean comma into one ghastly interval."
  (let [pure 3/2
        wolf (/ pure pythagorean-comma)
        fifths [pure pure pure pure pure pure pure wolf pure pure pure]]
    (tune fifths)))

(def meantone-temperament
  "Converts midi to hertz, using a variant of Pythagorean tuning designed to get a consonant major third."
  (let [narrow (java.lang.Math/pow 5 1/4)
        wolf (* narrow 128/125)
        fifths [narrow narrow narrow narrow narrow narrow narrow wolf narrow narrow narrow]]
    (tune fifths)))

(def equal-temperament
  "Converts midi to hertz, spreading out the Pythagorean comma evenly across all the intervals."
  (let [narrow (java.lang.Math/pow 2 7/12)
        fifths [narrow narrow narrow narrow narrow narrow narrow narrow narrow narrow narrow]]
    (tune fifths)))

(defn baganda-temperament
  "Converts midi to hertz, using a five tone version of equal temperament."
  [pitch]
  (/ (* concert-a 4) (reduce * (repeat pitch (java.lang.Math/pow 2 1/5)))))

(def baganda-scale
  (->> (phrase (repeat 1/4) (concat (range 18) [17 17 17 17 18 18] [17 17 17 17 18 18] [17 17 17 12 12 12]))
       (canon/canon (comp (canon/simple 1/6) (canon/interval -4)))
       (canon/canon (comp (canon/simple 1/6) (canon/interval -4)))
       (where :pitch baganda-temperament)))

(comment
  (live/play baganda-scale)
  (live/jam (var baganda-scale))
)

;;;;;;;;;;;;;;
;;; Norman ;;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis by compression ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(comment
  (live/play row-row)
  (live/jam (var row-row))
)
