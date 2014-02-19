(ns source-counter.core
  (:use clojure.java.io)
  (:use clojure.pprint))

(defmacro dbg [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn read-file
  "Reads a file line by line and returns it as a vector of lines."
  [path]
  (with-open [rd (reader path)]
    (reduce conj [] (line-seq rd))))

(defn check-file-exts
  "For a vector of file extensions, checks if name matches either of them."
  [name extensions]
  (reduce #(or %1 %2) (map #(.endsWith name %) extensions)))

(defn get-file-ext
  "Returns the file extension of filename."
  [filename]
  (str "." (last (clojure.string/split filename #"\."))))

(defn find-files
  "Returns a tree-seq of all files that have one of the file extensions
   that are passed in the extensions vector."
  [path extensions]
  (filter
   #(check-file-exts (.getName %) extensions)
   (file-seq (file path))))

(defn count-loc
  "Returns a map containing entries for :comment, :blank and :code, with values
   that are the numbers of code for each of those in the given list of strings."
  [file-lines check-line]
  (reduce (fn [map kw]
            (let [counter (get map kw 0)]
            (assoc map kw (inc counter))))
          {}
          (map check-line file-lines)))

(defn check-clojure-line [l]
  (let [line (.trim l)]
    (cond (.startsWith line ";") :comment
          (.isEmpty line) :blank
          :else :code)))

(defn check-c-ish-line [l]
  (let [line (.trim l)]
    (cond (or (.startsWith line "//") (.startsWith line "/*") (.startsWith line "*")) :comment
          (.isEmpty line) :blank
          :else :code)))

(defn check-py-line [l]
  (let [line (.trim l)]
    (cond  (.startsWith line "#") :comment
           (.isEmpty line) :blank
           :else :code)))

(defn check-html-line [l]
  (let [line (.trim l)]
    (cond (.startsWith line "<!--") :comment
          (.isEmpty line) :blank
          :else :code)))

(def ^:dynamic *lang-map*
  [[[".clj"] check-clojure-line]
   [[".html" ".htm"] check-html-line]
   [[".scala" ".cs" ".c" ".cpp" ".java" ".h" ".hpp" ".js"] check-c-ish-line]
   [[".rb" ".py"] check-py-line]])

(def ^:dynamic *supported-languages*
  (reduce (fn [vec [exts _]] (into vec exts)) [] *lang-map*))

(def ^:dynamic *lang-names*
  {".clj" "Clojure"
   ".scala" "Scala"
   ".c" "C"
   ".cpp" "C++"
   ".hpp" "C++ Header"
   ".h" "C Header"
   ".rb" "Ruby"
   ".py" "Python"
   ".cs" "C#"
   ".java" "Java"
   ".js" "Javascript"
   ".html" "HTML"
   ".htm" "HTML"})

(defn seq-contains?
  "Returns whether coll contains a target value."
  [coll target]
  (some #(= target %) coll))

(defn first-non-nil
  "Returns the first non-nil element in coll."
  [coll]
  (first (drop-while nil? coll)))

(defn- get-from-lang-map [e]
  (first-non-nil
   (for [[exts method] *lang-map*]
     (when (seq-contains? exts e) method))))

(defn count-loc-in-dir
  "Returns a map from file extensions to LoC counts. Counts all of the files
   in the directory and recursively counts all other directories within that
   one too."
  [path]
  (let [files (find-files path *supported-languages*)
        data (map (fn [file]
                    [(get-file-ext (.getName file)) (read-file file)])
                  files)]
    (reduce (fn [map [ext code]]
       (let [previous (get map ext {:blank 0, :code 0, :comment 0})
             next (count-loc code (get-from-lang-map ext))]
         (assoc map ext (merge-with + previous next))))
     {}
     data)))

(defn print-loc-data
  "Prints a map produced by count-loc-in-dir in a nice table to stdout."
  [data]
  (let [headings ["Language" "Blank" "Comment" "Code"]
        sorted-data (sort (fn [a b] (> (:code (second a)) (:code (second b)))) data)
        values (vec (for [[lang stats] sorted-data]
                 {"Language" (get *lang-names* lang)
                  "Blank" (get stats :blank)
                  "Code" (get stats :code)
                  "Comment" (get stats :comment)}))
        values-with-sum (conj values
                              {"Language" "SUM"
                               "Blank" (reduce + (map #(get % "Blank") values))
                               "Code" (reduce + (map #(get % "Code") values))
                               "Comment" (reduce + (map #(get % "Comment") values))})]
    (print-table headings values-with-sum)))

(defn usage []
  (println "Usage: source-counter <path>"))

(defn -main [& args]
  (when (not= 1 (count args))
    (do (usage)
        (System/exit 0)))
  (print-loc-data (count-loc-in-dir (first args))))
