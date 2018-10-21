(ns dueling-keyboards.jam
  (:require [overtone.live :refer :all :exclude [stop sixth]]
            [overtone.inst.synth :as synth]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [dueling-keyboards.talk :refer [equal-temperament pythagorean-tuning]]
            [dueling-keyboards.instrument :as inst]))


;;;;;;;;;;;;;;;;;;;;;;
;;; Dueling banjos ;;;
;;;;;;;;;;;;;;;;;;;;;;

(comment

  (let [strum #(->> % (phrase (concat [1/4 1/4] (repeat 1/2))) (then (phrase [4] [nil])))
        chord #(-> chord/triad (chord/root %))]
    (->>
      (strum (concat (map chord [0 0 0 3 0])))
      ;(times 2)
      #_(then (mapthen #(->> (strum (concat [0 0 0 1 2 3 4 3 2]))
                           (where :pitch (partial + %)))
                     [0 0 3 0 4 0]))
      (tempo (bpm 75))
      (all :attack 0.001)
      (where :pitch (comp equal-temperament scale/G scale/major))
      live/play))

)






;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dueling keyboards ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (live/jam (var dueling-keyboards))

  (def dueling-keyboards nil)

  (volume 0.8)
)

(defn with-beat [notes]
  (let [hits [:tock nil :tick nil :tock :tick :tick2 :tick]]
    (->> notes
       (with (take (* 2 (duration notes))
                   (->> (phrase (repeat 1/2) (repeat -14))
                        (having :part (cycle hits))))))))

(def dueling-keyboards
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
                  (times 4)
                  ;            (where :pitch scale/lower)
                  )
        arpeggios (->> (phrase
                         (repeat 64 1/4)
                         (mapcat #(take 16 (cycle [%2 %1 11 %1]))
                                 [6 7.5 7 6]
                                 [8 9 9 8]))
                       (times 4)
                       (all :part :melody))
        harmonies (->> (phrase
                         (cycle [10/4 1 1/4 1/4 4])
                         [[1 6] 4 1 1.5 [2 7.5] [2 7] 5 3 3.5 [4 6]])
                       (times 4)
                       (all :part :melody)
                       )
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
     #_    (with
             bass
     ;        riff
     ;        extra
             )
      ;    (then postscript)
     #_      (then
               (with
                 pulse
                 harmonies
     ;            arpeggios
                 ))
      ;(then postscript)
      with-beat
      (tempo (bpm 150))
      (where :pitch (comp equal-temperament scale/C scale/major)))))

(def room 0.8)

(defmethod live/play-note :default
  [{midi :pitch seconds :duration attack :attack}]
  (some-> midi (inst/over-it seconds (or attack 0.05) :volume 0.5 :room room))
;  (some-> midi (inst/kick seconds (or attack 0.05) :volume 0.3))
  )

(defmethod live/play-note :melody
  [{midi :pitch seconds :duration attack :attack}]
  (some-> midi (inst/organ seconds (or attack 0.05) :vol 0.3 :room room)))

(defmethod live/play-note :tock
  [{midi :pitch seconds :duration}]
  (some-> midi (inst/kick :vol 1.3 :env-ratio 15 :room room)))

(defmethod live/play-note :tick
  [{midi :pitch seconds :duration}]
   (some-> midi (inst/quick-kick :room room)))

(defmethod live/play-note :tick2
  [{midi :pitch seconds :duration}]
   (some-> midi (* 2/3) (inst/quick-kick :room room)))
