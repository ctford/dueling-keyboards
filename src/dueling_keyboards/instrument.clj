(ns dueling-keyboards.instrument
  (:require [overtone.live :refer :all]))

(definst sine-wave [freq 440 dur 5.0 vol 0.3]
  (-> (sin-osc freq)
      (* vol)
      (* (env-gen (adsr 0.1 0.15 0.5 0.3)
                  (line:kr 1 0 dur) :action FREE))))

(definst organ [freq 440 dur 5.0 vol 1.0 pan 0.0 wet 0.5 room 0.5 limit 20000 attack 0.1]
  (->
    (map #(sin-osc (* freq %)) (range 1 8))
    mix
    (* (env-gen (asr attack 1.0 0.5) (line:kr 1.0 0.0 dur) :action FREE))
    (lpf 2500)
    (free-verb :mix 0.5 :damp 0.5 :room 0.8)
    (* vol)))

(definst over-it [freq 440 dur 1.0 attack 0.5 volume 0.5]
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
      (* volume 0.5)))

