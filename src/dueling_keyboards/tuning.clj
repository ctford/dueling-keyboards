(ns dueling-keyboards.tuning)

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
                    (map (fn normalise [ratio]
                           (if (< ratio 2)
                             ratio
                             (normalise (/ ratio 2)))))
                    sort)]
    (align-concert-a #(temper % ratios))))
