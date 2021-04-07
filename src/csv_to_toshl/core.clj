(ns csv-to-toshl.core
  (:require [clojure.string :as s]
            [clojure.pprint :as pp])
  (:gen-class))

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn identify-source [ap]
  (cond (= (:col-count ap) 8) :SBANKEN
        (= (:col-count ap) 5) :BIEN
        :else :UNKNOWN))

(defn read-file [filename]
  (let [lines (-> (slurp filename) (s/split #"(\r\n|\n)"))
        data (map #(s/split % #";") lines)
        col-count (->> (map count data) (apply max))
        account-print {:file-name filename
                       :col-count col-count
                       :data      (map #(pad col-count % "") data)}]
    (assoc account-print :type (identify-source account-print))))

(defn format-sbanken [ap]
  (assoc ap :data (->> (:data ap)
                       (drop 3)
                       (drop-last 2)
                       ((fn [data] (into [["Date" "Date" "Skip" "Skip" "Skip" "Description" "Expense" "Income"]] data))))))


(defn format-data [{:keys [type] :as ap}]
  (cond (= type :SBANKEN) (format-sbanken ap)
        :else ap))

(defn ap->csv [{:keys [data] :as ap}]
  (assoc ap :csv (->> data (map #(s/join ";" %)) (s/join "\n"))))

(defn store-file! [{:keys [csv file-name]}]
  (spit (-> file-name) csv))

(defn -main
  [& args]
  (let [processed (map #(->> (read-file %) format-data ap->csv (partial store-file!)))]
    (do processed)))


(comment
  (-> (read-file "C:\\97291021585_2021_03_15-2021_04_04.csv")
      format-data
      ap->csv
      store-file!))