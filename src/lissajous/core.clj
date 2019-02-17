(ns lissajous.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; x = Asin(at + phi)
; y = Bsin(bt)

(def size 600)
(def t-scale 40)

(def ch1 {:amp (/ size 4) :freq 3})
(def ch2 {:amp (/ size 4) :freq 1})
(def phi (q/radians 0))

(q/set-state! {:x1 0 :x2 0 :y1 0 :y2 0})

(defn setup []
  (q/frame-rate 60)
  (q/stroke 255)
  (q/background 0))

(defn f-ch1
  "The coordinates of a sampled point at time t.
   Plot sine-curve 1 along the x-axis"
  [s t]
  [(* s t-scale)
   (* (:amp ch1)
      (q/sin (+ (* (/ (:freq ch1) Math/PI) (- s t))
                phi)))])

(defn f-ch2
  "The coordinates of a sampled point at time t.
   Plot sine-curve 2 along the y-axis"
  [s t]
  [(* (:amp ch2)
      (q/sin (* (/ (:freq ch2) Math/PI) (- s t))))
   (* s t-scale)])

(defn draw-channel [f-ch t]
  (let [ds 0.05]
    (doseq [s (range 0 (/ size t-scale) ds)]
      (q/line (f-ch s t)
              (f-ch (- s ds) t)))))

(defn draw-screen [x]
  (q/no-stroke)
  (q/fill 255 240)
  (q/rect (- 0 (/ x 2)) (- 0 (/ x 2)) x x))

(defn draw-lissajous [t]
  (draw-screen (/ size 2))
  (q/stroke 255 10 10)
  (q/stroke-weight 3)
  (let [ds 0.2
        t0 (if (> (- t 20) 0)
             (- t 20)
             0)]
    (doseq [s (range t0 t ds)]
      (q/stroke 255 50 50 (* 255 (/ (- s t0) 20)))
      (q/line (f-lissajous s)
              (f-lissajous (+ s ds))))

    (q/stroke 0 0 255)
    (q/stroke-weight 10)
    (q/point (first (f-lissajous (+ t ds)))
             (last (f-lissajous (+ t ds))))

    (q/stroke-weight 3)
    (q/line [-300
             (second (f-lissajous (+ t ds)))]
            [300
             (second (f-lissajous (+ t ds)))])

    (q/line [(first (f-lissajous (+ t ds)))
             -300]
            [(first (f-lissajous (+ t ds)))
             300])))

(defn f-lissajous [s]
  [(* (:amp ch2)
      (q/sin (* (/ (:freq ch2) Math/PI) (+ s 100))
                ))
   (* (:amp ch1)
      (q/sin (+ (* (/ (:freq ch1) Math/PI)  (+ s 80))
                phi)))])

(defn draw []
  (let [t (/ (q/frame-count) 10)]
    (q/background 0)
    (q/stroke 255)
    (q/stroke-weight 3)

    (q/with-translation [0 (/ (q/height) 2)]
      (draw-channel f-ch1 t))

    (q/with-translation [(/ (q/width) 2) 0]
      (draw-channel f-ch2 t))

    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (draw-lissajous t))))

(q/defsketch lissajous
  :size [size size]
  :setup setup
  :draw draw)
