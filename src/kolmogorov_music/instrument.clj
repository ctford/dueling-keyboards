(ns kolmogorov-music.instrument
  (:require [overtone.live :refer :all]))

(definst overchauffeur [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (sin-osc (* 3.01 freq)))
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 1/2 (sin-osc (* 1/2 freq))))
      (rlpf (line:kr 3000 0 dur) 1/5)
      (clip2 0.7)
      (* (env-gen (adsr 0.001 0.03 0.9 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))