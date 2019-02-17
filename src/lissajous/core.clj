(ns lissajous.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; x = Asin(at + phi)
; y = Bsin(bt)

(def size 300)
(def t-scale 100)
(def lissajous-box-size (/ size 2))
(def ds 0.1)

(def amp (/ size 4))
(def ch1 {:amp amp :freq 2.5})
(def ch2 {:amp amp :freq 3})
(def phi (q/radians 0))

(defn update-image []
  (q/set-state! :image (create-image)))

(defn f-ch1 [t s]
  (let [freq (:freq ch1)]
    [(* s (/ size t-scale) 2)
     (* (:amp ch1)
        (q/sin (+ phi
                  (* (/ freq (* 2 Math/PI))
                     (+ s t)))))]))

(defn f-ch2 [t s]
    (let [freq (:freq ch2)]
      [(* (:amp ch2)
          (q/sin (* (/ freq (* 2 Math/PI))
                    (+ (+ s t)))))
       (* s (/ size t-scale) 2)]))

(defn f-lissajous [t]
  [(* (:amp ch2)
      (q/sin (* (/ (:freq ch2) (* 2 Math/PI)) t)))
   (* (:amp ch1)
      (q/sin (+ (* (/ (:freq ch1) (* 2 Math/PI)) t)
                phi)))])

(defn draw-channel [f-ch t]
  (doseq [s (range 0 t-scale ds)]
    (q/line (f-ch t s)
            (f-ch t (+ s ds)))))

(defn draw-lissajous-box [x]
  (q/image (q/state :image) (- 0 (/ size 4)) (- 0 (/ size 4))))

(defn draw-lissajous [t]
  (let [t0 (if (> (- t 10) 0) (- t 10) 0)
        ly (second (f-lissajous (- t ds)))
        lx (first (f-lissajous (- t ds)))
        offset (- 0 (/ size 2))]

    (draw-lissajous-box lissajous-box-size)

    (q/stroke 255 220)
    (q/stroke-weight 1)
    (q/line [lx ly] [offset ly])
    (q/line [lx ly] [lx offset])

    (q/stroke 68 242 91)
    (q/stroke-weight 12)
    (q/point lx ly)

    (doseq [s (range t0 t ds)]
      (q/stroke-weight 2)
      (q/stroke 68 242 91 (* 255 (/ (- s t0) 10)))
      (q/line (f-lissajous s)
              (f-lissajous (- s ds))))))


(defn draw []
  (let [t (/ (q/frame-count) 5)]
    (q/background 26 80 118)
    (q/stroke 175 195 193)
    (q/stroke-weight 2)

    (q/with-translation [0 (/ (q/height) 2)]
      (draw-channel f-ch1 t))

    (q/with-translation [(/ (q/width) 2) 0]
      (draw-channel f-ch2 t))

    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (draw-lissajous t))))

(defn create-image []
  (let [width (* 2 amp)
        im (q/create-image width width :rgb)]
    (dotimes [x width]
      (dotimes [y width]
        (q/set-pixel im x y (q/color 26 80 118 230))))
    (doseq [s (range 0 2000 0.1)]
      (let [xy (f-lissajous s)]
        (q/set-pixel im
                     (+ (first xy) (/ width 2))
                     (+ (second xy) (/ width 2))
                     (q/color 242 90 67))))
    im))

(defn setup []
  (q/set-state! :image (create-image))
  (q/frame-rate 60))

(q/defsketch lissajous
  :size [size size]
  :setup setup
  :draw draw)
