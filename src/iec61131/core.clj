(ns iec61131.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [iec61131.ld-compiler-claude :as ld]
            ;; Uncomment when implemented
            ;; [iec61131.st-compiler :as st]
            ;; [iec61131.il-compiler :as il]
            )
  (:gen-class))

(def cli-options
  [["-i" "--input FILE" "Input file"
    :validate [#(and (string? %) (.exists (java.io.File. %))) "Input file must exist"]]
   ["-o" "--output FILE" "Output file"
    :default "-"] ;; stdout
   ["-t" "--target LANG" "Target language for compilation"
    :default "il"
    :validate [#(contains? #{"il" "c" "js"} %) "Target must be one of: il, c, js"]]
   ["-h" "--help" "Show help"]])

(defn usage [options-summary]
  (->> ["IEC 61131-3 Language Compilers"
        ""
        "Usage: clj -M:run-ld-compiler [options]"
        "       clj -M:run-st-compiler [options]"
        "       clj -M:run-il-compiler [options]"
        ""
        "Options:"
        options-summary
        ""
        "Examples:"
        "  clj -M:run-ld-compiler -i resources/examples/example.ld -o output.il"
        "  clj -M:run-ld-compiler -i resources/examples/example.ld -t c -o output.c"]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn compile-file [compiler-fn input-file target-language output-file]
  (let [input (slurp input-file)
        output (compiler-fn input :target-language target-language)]
    (if (= output-file "-")
      (println output)
      (spit output-file output))))

(defn dispatch-compiler [args]
  (let [ns-name (-> *ns* ns-name str)
        compiler-type (if (str/ends-with? ns-name "ld-compiler")
                        :ld
                        (if (str/ends-with? ns-name "st-compiler")
                          :st
                          (if (str/ends-with? ns-name "il-compiler")
                            :il
                            :unknown)))]
    (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
      (cond
        (:help options) (println (usage summary))
        errors (println (error-msg errors))
        :else (case compiler-type
                :ld (compile-file ld/compile-ld
                                  (:input options)
                                  (:target options)
                                  (:output options))
                :st (println "ST compiler not yet implemented")
                :il (println "IL compiler not yet implemented")
                :unknown (println "Unknown compiler type"))))))

(defn -main [& args]
  (dispatch-compiler args)
  (System/exit 0))