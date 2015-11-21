(ns dueling-keyboards.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [dueling-keyboards.talk :refer [equal-temperament]]))


(def dueling
  (->>
    (times 2 (phrase (mapcat repeat [2 6 1] [1/4 1/2 5]) [2 3 4 2 3 1 2 0 1]))
    (then (times 2 (phrase (mapcat repeat [6 1] [1/2 5]) [0 0 1 2 0 2 1])))
    (then (times 2 (phrase [1/4 1/4 1/2 1/2 1/2 4/2] [chord/triad chord/triad chord/triad (chord/root chord/triad 3) chord/triad -7])))
    (where :time (bpm 90))
    (where :duration (bpm 90))
    (where :pitch (comp equal-temperament scale/G scale/major))))

(def sec (-> chord/triad (chord/inversion 2) (dissoc :i)))

(def im-not-worried
  (let [chorus (->>
                 (phrase (cycle [5/2 1/2 1/2 1/2]) [[-2 3.5] -2 1 0 [-2 3] -2 1 0 [-3 2] -3 1 0 [-3 1] -3 1 0])
                 (with (->> (phrase (repeat 1/2) (mapcat repeat (cycle [6 1 1]) [1 1 2 3 2 1 0 -1 -2 -3 1 -3])) (where :pitch scale/lower)))
                 (times 2))
        plain (->> (phrase (repeat 32 1/2) (repeat -3))
                   (with (phrase (repeat 4) [1 0.5 0 -0.5])))
        variation (->> plain
                       (canon/canon (canon/interval -7))
                       (where :pitch scale/lower)
                       (with (phrase (repeat 4)
                                     [(-> sec (chord/root -3))
                                      (-> sec (chord/root -2) (update-in [:iii] (scale/from 0.5)))
                                      (-> sec (chord/root -2))
                                      (-> sec (chord/root -3))]))
                       (where :pitch (comp scale/raise scale/raise)))
        tail (->> (phrase (repeat 8 1/2) (cycle [-3 -3 -3 -3 -3 -3 1 -3]))
                        (where :pitch scale/lower)
                        (with (phrase [5/2 1/2 1/2 1/2] [[-3 1] -2 1 0]) )
                        )]
    (->> (times 2 chorus )
         (then (times 3 tail))
         (then (times 4 variation))
         (then (times 2 (with chorus (phrase [8 6 1 1 8 4 2 1 1] [12 11 12 13 12 14 13 12 11]))))
         (then (times 3 tail))
         (then (times 2 plain))
         (where :time (bpm 150))
         (where :duration (bpm 150))
         (where :pitch (comp equal-temperament scale/C scale/major)))))

(defmacro defs  [names values]
  `(do
     ~@(map
         (fn [name value] `(def ~name ~value))
         names (eval values))))

(def char->ascii int)

(defs [A B C D E F G]
  (map
    (comp scale/A scale/low scale/minor)
    (range)))

(defn ascii->midi  [n]
  (->> n
       char
       str
       (symbol "dueling-keyboards.geb")
       find-var
       deref))

(def geb
  (let [theme (->> "GEB"
                   (map char->ascii)
                   (phrase [4 4 8])
                   (canon/canon #(where :pitch ascii->midi %)))
        theme2 (->> theme (with (->> (phrase [4 4 8] "GEB") (where :part (is :sample)))))
        bass (phrase [4 4 8] [-2 -1 0])
        bass2 (phrase (repeat 4 4) (cycle [3 0]))
        decoration (phrase (repeat 64 1/4) (cycle [7 8 9 11 7 6]))
        grind (->> [-2 -1 0 0]
                   (mapthen #(->> (phrase (repeat 7 1/2) (interleave [[0 2] [0 2] [0 3] [0 2]] (repeat -3)))
                                  (where :pitch (scale/from %))))
                   (then (phrase (repeat 4 1/2) (interleave [[0 3] [0 2]] (repeat -3)))))
        twiddle (with (phrase (repeat 32 1/2) (cycle [4 2 2 0 -1])) (phrase (repeat 64 1/4) (cycle [4 2 5 4 5 4 7 7])))]
    (->> bass
         (where :pitch (comp scale/lower scale/lower))
         ;(with twiddle)
         ;(with decoration)
         ;(with grind)
         (where :pitch (comp scale/B scale/minor))
         (with theme)
         (where :pitch equal-temperament)
         (where :time (bpm 90))
         (where :duration (bpm 90)))))

(comment
  (map fx-chorus [0 1])
  (map fx-distortion [0 1] [0.8 0.6] [0.3 0.5])
  (volume 0.8)
  (live/jam (var geb))
  (def geb nil)
  )

; Instrumentation
(definst overchauffeur [freq 110 dur 1.0 vol 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc 4/3) (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc 8/3) (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc 1/3) (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc 1) (sin-osc (* 0.5 freq))))
      (* 3)
      (clip2 0.8)
      (rlpf (line:kr 2000 1000))
      (* (env-gen (adsr 0.5 0.2 0.3 0.1) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi (overchauffeur seconds)))

(defn book [initial]
  (({\G (sample "samples/godel.wav" :start 4000)
     \E (sample "samples/escher.wav")
     \B (sample "samples/bach.wav")}
    initial)))

(defmethod live/play-note :sample
  [{initial :pitch}]
  (book initial))
