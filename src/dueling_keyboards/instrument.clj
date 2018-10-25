(ns dueling-keyboards.instrument
  (:require [overtone.live :refer :all]))

(definst sine-wave [freq 440 dur 5.0 vol 0.3]
  (-> (sin-osc freq)
      (* vol)
      (* (env-gen (adsr 0.1 0.15 0.5 0.3)
                  (line:kr 1 0 dur) :action FREE))))

(defsynth walker [out-bus 0 freq 0.5]
  (out:kr out-bus (lf-noise1:kr freq)))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk))
(def resonance (mul-add (in:kr random-walk) 200 800))

(definst organ [freq 440 dur 5.0 vol 0.3 wet 0.5 room 0.5 limit 5000 attack 0.1]
  (->
    (map #(sin-osc (* freq %)) (range 1 5))
    mix
    (lpf limit)
    (free-verb :mix 0.5 :damp wet :room room)
    (* (env-gen (asr attack 1.0 0.5) (line:kr 1.0 0.0 dur) :action FREE))
    (* vol)))

(definst over-it [freq 440 dur 1.0 attack 0.5 volume 0.5 room 0.5 wet 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc 4/3) (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc 8/3) (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc 1/3) (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc 5/8) (sin-osc (* 0.5 freq))))
      (* 3)
      (clip2 0.8)
      (* 2)
      (rlpf (line:kr 2000 400 dur) 0.8)
      (free-verb :mix 0.5 :damp wet :room room)
      (* (env-gen (adsr attack 0.2 0.5 0.1)
                  (line:kr 1 0 dur) :action FREE))
      ;(rlpf resonance 0.1)
      (* volume 0.5)))

; Adapted from Overtone core
(definst kick
  [freq       {:default 50 :min 40 :max 140 :step 1}
   vol        {:default 0.5 :min 0.0 :max 5.0 :step 0.001}
   env-ratio  {:default 3 :min 1.2 :max 8.0 :step 0.1}
   freq-decay {:default 0.02 :min 0.001 :max 1.0 :step 0.001}
   amp-decay  {:default 0.5 :min 0.001 :max 1.0 :step 0.001}]
  (let [fenv (* (env-gen (envelope [env-ratio 1] [freq-decay] :exp)) freq)
        aenv (env-gen (perc 0.005 amp-decay) :action FREE)]
    (-> fenv
        (sin-osc (* 0.5 Math/PI))
        (* aenv)
        (free-verb :mix 0.5 :damp 0.5 :room 0.5)
        (rlpf resonance 0.3)
        (* vol))))

; Adapted from Overtone core
(definst quick-kick
  [freq {:default 20.0 :min 20 :max 400 :step 1}
   vol  {:default 0.5 :min 0.0 :max 1.0 :step 0.001}
   attack {:default 0.0001 :min 0.00001 :max 2 :step 0.0001}
   decay  {:default 0.374 :min 0.00001 :max 2 :step 0.0001}
   fattack {:default 0.001 :min 0.00001 :max 2 :step 0.0001}
   fdecay {:default 0.282 :min 0.00001 :max 2 :step 0.0001}
   amp {:default 0.8 :min 0.01 :max 1 :step 0.01}]
  (let [freq-env (env-gen:kr (perc fattack fdecay))
        wave (sin-osc (+ (* 0.5 freq) (* 14 freq freq-env)))
        env  (x-line:kr 1 0 decay :action FREE)
        src (* env wave)
        dist (clip2 (* 2 (tanh (* 3 (distort (* 1.5 src))))) 0.8)
        eq (b-peak-eq dist 57.41 1 44)]
    (-> (* amp eq)
        (rlpf resonance 0.3)
        (free-verb :mix 0.5 :damp 0.5 :room 0.5)
        (* vol))))
