(ns measurementpurejvm
  (:require [clojure.data.csv :as csv]
            [babashka.fs :as fs]
            [clojure.java.io :as io] 
            [clojure.string :as string]
            [criterium.core :refer [bench]])
  (:import java.io.IOException)
  (:gen-class))

(set! *warn-on-reflection* true)

(defn valido? [s]
  (let [path (first s)
        f (fs/file path)
        args (count s)]
    (cond
      (not (fs/exists? f)) (do (prn "El input no es una ruta valida")
                               false)
      (> args 1) (do (prn "Solo se admite una ruta como argumento")
                     false)
      :else (.getAbsolutePath f))))

(defn leer [s]  
  (try
    (csv/read-csv (io/reader s :encoding "UTF-8"))
    (catch IOException e (prn (.getMessage e)))))

(defn extraer-datos [s]
  (nthrest s 2))

(defn parsear [s] 
  (pmap
   (comp
    (fn [arr]
      [(first arr) (-> (second arr) Double/parseDouble)])
    #(string/split (first %) #";"))
   s))

(defn ->map [s]
  (->> (pmap #(apply hash-map %) s)
       (apply merge-with vector)))

(defn aplicar-calculos [v]
  #_(println v)
  (let [len (count v)
        [-min mean -max] ((juxt #(apply min %) #(-> (reduce + %) (/ len)) #(apply max %)) v)]
    (format "%1.1f/%2.1f/%3.1f" -min mean -max)))

 
(defn formatear [coll]
  (doseq [[k v] coll :let [temps (-> (if (vector? v) 
                                 (flatten v)
                                 (vector v)) 
                               aplicar-calculos)]] 
    (prn (str k "=" temps))))

(defn run [arch]
  (-> (leer arch)
      extraer-datos
      parsear
      ->map
      formatear))

(defn -main [& args]
  (bench 
   (let [arg args]
     (try
       (when-let [p (valido? arg)] 
         (time (run p)))
       (catch IOException e (prn (.getMessage e)))
       (catch Exception e (prn (.getMessage e))))))) 
 
(comment 
  (leer "Doc24 - Planilla Copagos COMPLETA.csv")
  (let [x (csv/read-csv (io/reader "C:\\Users\\jrivero\\Downloads\\weather_stations.csv"))]
    (nthrest (nthrest x 2) 20))  
   
  (let [m (->> (leer "C:\\Users\\jrivero\\Downloads\\weather_stations.csv") 
              extraer-datos
              parsear 
              ->map
               formatear
              #_(pmap #(apply hash-map %))
              #_frequencies 
              #_(sort-by val >)
               #_vals
               #_(filter (or char? string?)))]
    #_(flatten (get m "Dumri"))
    m
    #_(formatear m)) 
 
  (format "%1.1f/%2.1f/%3.1f" 10.3434 20.35454 20.5443)
 
  (seq (lazy-seq [2 4]))
  (reduce conj 31.3 [33.63])
  
  ((fn [[a b]] [a (Double/parseDouble b)]) ["Tojio" "20.20"])

  (apply hash-map ["a" 23])
  (merge-with (fn [elem1 elem2]
                (let [col (if (coll? elem2) elem2 (list elem2))]
                  (cons elem1 (lazy-seq col)))) 
              {"a" 10.4} {"b" 333} {"a" 19.4} {"c" 43} {"b" 4})

  (time (aplicar-calculos [20.3232 32.92332 12.9012 32.983223 78.34232323 89.3443234 56.9993443 14.423433214])) 
  (time ((juxt #(-> (reduce + %) (/ (count %))) #(apply min %) #(apply max %)) [20.3232 32.92332 12.9012 32.983223 78.34232323 89.3443234 56.9993443 14.423433214]))
  (filter (or (complement number?) coll?) '(3 53 3 'a "fd" true {:a 3} [4 53]))
  (filter (or (complement number?) coll?) '(3 53 3 34 54 544 64 '(((32) 443) 4343)))
  
(- 240 164)

  ;; Quité el doall en el pmap y obtuvo una mejora leve en el tiempo
  )

 