(ns dueling-keyboards.instrument
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.live :refer [stop]]))

(definst matooke [freq 110 dur 1.0 vol 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.001 freq))))
      (+ (* 1/8 (sin-osc (* 5.001 freq))))
      (clip2 0.8)
      (lpf (* 5 440))
      (* (env-gen (adsr 0.01 0.05 0.15 0.2) (line:kr 1 0 dur) :action FREE))
      (+ (* (env-gen (perc 0.02 0.03)) (* 1/3 (sin-osc (* 0.5 freq)))))
      (* vol)))
