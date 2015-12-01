(ns dueling-keyboards.jam
  (:require [overtone.live :refer :all :exclude [stop sixth]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [dueling-keyboards.talk :refer
             [equal-temperament pythagorean-tuning]]))

(comment
  (->> dueling
       live/play)
)

(def dueling
  (->>
    (phrase [1/4 1/4 1/2 1/2 1 3/2]
            (map #(-> chord/triad (chord/root %)) [0 0 0 3 0]))
    (then (phrase [2] [nil]))
    (times 2)
    (then (mapthen #(->>
                      (phrase (repeat 1/2) [0 1 2 3 4 3 2])
                      (then (phrase [9/2] [nil]))
                      (where :pitch (scale/from %)))
                   [0 3 0 4]))
    (tempo (bpm 75))
    (all :attack 0.01)
    (where :pitch (comp scale/G scale/major))))

(comment
  (over-it (* 55 16/9) 24)
  (live/jam (var im-not-worried))

  (map fx-chorus [0 1])
  (volume 0.8)
)

(def im-not-worried
  (let [riff (->> (phrase (cycle [5/2 1/2 1/2 1/2])
                          [[-2 3.5] -2 1 0
                           [-2 3] -2 1 0
                           [-3 2] -3 1 0
                           [-3 1] -3 1 0])
                  (times 4))
        bass (->> (phrase (repeat 1/2)
                          (mapcat
                            repeat
                            (cycle [6 1 1])
                            [-6 -6 -5 -4 -5 -6 -7 -8 -9 -10 -6 -10]))
                  (times 4))
        arpeggios (->> (phrase
                         (repeat 64 1/4)
                         (mapcat #(take 16 (cycle [%2 %1 11 %1]))
                                 [6 7.5 7 6]
                                 [8 9 9 8]))
                       (times 4))
        harmonies (->> (phrase
                         (cycle [10/4 1 1/4 1/4 4])
                         [[1 6] 4 1 1.5 [2 7.5] [2 7] 5 3 3.5 [4 6]])
                       (times 4))
        pulse (phrase (repeat 64 1/2) (repeat -10))
        drone (phrase [32] [-10])
        postscript (->> (phrase (repeat 8 1/2)
                                (cycle [-10 -10 -10 -10 -10 -10 -6 -10]))
                        (with (phrase [5/2 1/2 1/2 1/2]
                                      [[-3 1] -2 1 0]))
                        (times 3))
        extra (->> (phrase [8 6 1 1 8 4 2 1 1]
                           [12 11 12 13 12 14 13 12 11])
                   (times 2))]
    (->>
      drone
   ;   (with
   ;   bass
   ;   riff
   ;   extra
   ;   )
   ;   (then postscript)
   ;   (then
   ;     (with
   ;           pulse
   ;           harmonies
   ;           arpeggios
   ;           )
   ;   )
   ;(then postscript)
      (tempo (bpm 150))
      (where :pitch (comp scale/C scale/major)))))

(definst over-it [freq 440 dur 1.0 attack 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc 4/3) (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc 8/3) (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc 1/3) (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc 5/8) (sin-osc (* 0.5 freq))))
      (* 3)
      (clip2 0.8)
      (rlpf (line:kr 2000 800 dur) 0.8)
      (* (env-gen (adsr attack 0.2 0.5 0.1)
                  (line:kr 1 0 dur) :action FREE))
      (* 0.5)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration attack :attack}]
  (some-> midi equal-temperament (over-it seconds (or attack 0.5))))
