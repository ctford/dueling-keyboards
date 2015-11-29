(ns dueling-keyboards.jam
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [dueling-keyboards.talk :refer
             [equal-temperament pythagorean-tuning]]))

(comment
  (live/jam (var dueling))
  (live/jam (var im-not-worried))

  (map fx-chorus [0 1])
  (volume 0.8)
)

(def dueling
  (->>
    (times 2 (phrase (mapcat repeat [2 6 1 1] [1/4 1/2 1 4])
                     [2 3 4 2 3 1 2 0 1 nil]))
    (then (times 2 (phrase (mapcat repeat [6 1 1] [1/2 1 4])
                           [0 0 1 2 0 2 1 nil])))
    (then (times 2 (phrase [1/4 1/4 1/2 1/2 1/2 2]
                           [chord/triad chord/triad chord/triad
                            (chord/root chord/triad 3)
                            chord/triad nil])))
    (tempo (bpm 75))
    (where :pitch (comp equal-temperament scale/G scale/major))))

(def sec (-> chord/triad (chord/inversion 2) (dissoc :i)))

(def im-not-worried
  (let [riff (phrase (cycle [5/2 1/2 1/2 1/2])
                     [[-2 3.5] -2 1 0
                      [-2 3] -2 1 0
                      [-3 2] -3 1 0
                      [-3 1] -3 1 0])
        bass (->> (phrase (repeat 1/2)
                          (mapcat
                            repeat
                            (cycle [6 1 1])
                            [1 1 2 3 2 1 0 -1 -2 -3 1 -3]))
                  (where :pitch scale/lower))
        chorus (->> (with bass riff)
                    (times 2))
        plain (->> (phrase [16] [[-3 -10]])
                   (with (phrase (repeat 64 1/4)
                                 (mapcat #(take 16
                                                (cycle [%2 %1 11 %1]))
                                         [6 7.5 7 6]
                                         [8 9 9 8])))
                   (with (phrase (repeat 4) [8 7.5 7 6.5])))
        chords (phrase (repeat 4)
                          [(-> sec (chord/root 4))
                           (-> sec (chord/root 5)
                               (update-in [:iii] + 0.5))
                           (-> sec (chord/root 5))
                           (-> sec (chord/root 4))])
        scene (phrase (repeat 32 1/2) (repeat [-3 -10]))
        vary (with scene chords)
        tail (->> (phrase (repeat 8 1/2)
                          (cycle [-3 -3 -3 -3 -3 -3 1 -3]))
                  (where :pitch scale/lower)
                  (with (phrase [5/2 1/2 1/2 1/2]
                                [[-3 1] -2 1 0])))
        harmony (phrase [8 6 1 1 8 4 2 1 1]
                        [12 11 12 13 12 14 13 12 11])]
    (->> (times 2 bass)
         (then (times 2 chorus))
         (then (times 3 tail))
         (then (times 4 vary))
         (then (times 2 (with chorus harmony)))
         (then (times 3 tail))
         (then (times 4 plain))
         (tempo (bpm 150))
         (where :pitch (comp scale/C scale/major))
         (where :pitch equal-temperament))))

(definst over-it [freq 110 dur 1.0 vol 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc 4/3) (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc 8/3) (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc 1/3) (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc 5/8) (sin-osc (* 0.5 freq))))
      (* 3)
      (clip2 0.8)
      (rlpf (line:kr 2000 1000))
      (* (env-gen (adsr 0.5 0.2 0.5 0.1)
                  (line:kr 1 0 dur) :action FREE))
      (* vol)))

(defmethod live/play-note :default
  [{hz :pitch seconds :duration}]
  (when hz (over-it hz seconds)))
