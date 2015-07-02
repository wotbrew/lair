(ns lair.rect)

(def rect vector)

(defn intersects?
  ([[x1 y1 w1 h1] [x2 y2 w2 h2]]
   (intersects? x1 y1 (or w1 1) (or h1 1) x2 y2 (or w2 1) (or h2 1)))
  ([x1 y1 w1 h1 x2 y2 w2 h2]
   (or (and (<= x1 x2 (+ x1 w1))
            (<= y1 y2 (+ y1 h1)))
       (and (<= x1 (+ x2 w2) (+ x1 w1))
            (<= y1 (+ y2 h2) (+ y1 h1))))))

(defn scale
  ([[x y w h] n]
   (scale x y w h n))
  ([x y w h n]
   (rect (int (* x n)) (int (* y n)) (int (* w n)) (int (* h n)))))

(defn points
  ([[x y w h]]
   (points x y w h))
  ([x y w h]
   (for [x (range x (+ x w))
         y (range y (+ y h))]
     (vector x y))))
