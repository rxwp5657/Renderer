(ns parser.parser
  (:use [la-math.vector]
        [render.primitives.group]
        [render.primitives.triangle]))

(defrecord Parser [vertex groups ignored current-group])

(defn make-parser
  "Make a parser"
  []
  (Parser. [] {:default (make-group)} 0 (keyword "default")))

;; Vertex processing

(defn- process-data-v
  "Process data"
  [data]
  (let [v-data (filter #(not (clojure.string/blank? %)) (rest data))]
    (apply make-point (map #(Double. (clojure.string/trim %)) v-data))))

(defn- add-vertex
  "add a vertex to the vertex array"
  [parser data]
  (let [vertex (process-data-v data)]
    (assoc parser :vertex (conj (:vertex parser) vertex))))

;; Face processing

(defn- process-data-f
  "Process data"
  [data]
  (let [indices-s (filter #(not (clojure.string/blank? %)) (rest data))
        indices   (map #(Integer. (clojure.string/trim %)) indices-s)]
    indices))

(defn- get-vertices
  "Get the vertices that conform a polygon"
  [parser indices]
  (map #(get (:vertex parser) (dec %)) indices))

(defn add-face
  "Process face"
  [parser data]
  (let [indices-d (process-data-f data)
        vertices  (get-vertices parser indices-d)]
    (loop [index 1]
      (if (= index (- (count vertices) 1))
        parser
        (let [triangle (make-triangle (nth vertices 0) (nth vertices index) (nth vertices (inc index)))
              g (get (:groups parser) (:current-group parser))
              temp (add-child g triangle)]
          (recur (inc index)))))))

;; Group processing

(defn add-group
  "Track the current group"
  [parser data]
  (let [key (keyword (first (rest data)))
        new-group (assoc parser :groups (assoc (:groups parser) key (make-group)))]
    (assoc new-group :current-group key)))

;; Ignore lines

(defn add-ignored
  "Increment the number of ignored lines"
  [parser]
  (update parser :ignored inc))

;; Line processing

(defn- decompose
  "Tokenize line by spaces"
  [line]
  (clojure.string/split line #" "))

(defn- process-line
  "process a line"
  [parser line]
  (let [data (decompose line)]
    (cond
      (= "v" (first data)) (add-vertex parser data)
      (= "f" (first data)) (add-face   parser data)
      (= "g" (first data)) (add-group  parser data)
      :else (add-ignored parser))))

;; File processing

(defn parse-obj-file
  "Parse an obj file"
  [file-name]
  (with-open [rdr (clojure.java.io/reader file-name)]
    (let [lines (reduce conj [] (line-seq rdr))]
      (loop [actual (first lines)
             rst    (rest lines)
             parser (make-parser)]
        (if (nil? actual)
          parser
          (recur (first rst) (rest rst) (process-line parser actual)))))))

;; Group processing

(defn obj-to-group
  "Transform a parser-obj to a group"
  [parser]
  (let [g (make-group)]
    (do
      (doseq [group (seq (:groups parser))] (when (not (empty? (:shapes (last group)))) (add-child g (last group))))
      g)))
