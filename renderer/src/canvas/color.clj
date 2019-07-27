(ns canvas.color
  (:use [uncomplicate.fluokitten core jvm]
        [la-math.vector]))

(defn make-color
  "Make a color"
  ^doubles
  [r g b]
  (make-tuple r g b 0.0))

(defn r
  "Get the red component"
  ^double
  [^doubles c]
  (x c))

(defn g
  "Get the green component"
  ^double
  [^doubles c]
  (y c))

(defn b
  "Get the blue component"
  ^double
  [^doubles c]
  (z c))

(defn c=
  "Define if two colors are the same"
  [^doubles a ^doubles b]
  (v= a b))

(defn c+
  "Color addition"
  ^doubles
  [^doubles a ^doubles b]
  (v+ a b))

(defn c-
  "Color substraction"
  ^doubles
  [^doubles a ^doubles b]
  (v- a b))

(defn c*
  "Color-scalar multiplication"
  ^doubles
  [^doubles c ^double s]
  (v* c s))

(defn c*c
  "Hadamart product of colors"
  ^doubles
  [^doubles a ^doubles b]
  (fmap e* a b))

(defn pixel->255
  "Convert pixel on 0 - 1 notation to 0 - 255 notation"
  ^doubles
  [^doubles pixel]
  (fmap #(cond
           (>= % 1) 255
           (<= % 0) 0
           :else (Math/ceil (* % 255))) pixel))

;; color constants

(def black (make-color 0 0 0))
(def white (make-color 1 1 1))
