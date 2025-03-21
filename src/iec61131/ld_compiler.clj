(ns iec61131.ld-compiler
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(def ld-grammar "
<ld-program> = program-declaration? variable-declaration* network-list program-termination? <EOL>?
program-declaration = ('PROGRAM' | 'FUNCTION' | 'FUNCTION_BLOCK') <whitespace> identifier <EOL>
program-termination = ('END_PROGRAM' | 'END_FUNCTION' | 'END_FUNCTION_BLOCK') <EOL>
variable-declaration = ('VAR' | 'VAR_INPUT' | 'VAR_OUTPUT' | 'VAR_IN_OUT' | 'VAR_TEMP' | 'VAR_GLOBAL') 
                       <whitespace>? variable-qualifier? <EOL>
                       (<whitespace>? variable-spec <semicolon> <EOL>)* 
                       'END_VAR' <EOL>
variable-qualifier = 'CONSTANT' | 'RETAIN' | 'NON_RETAIN'
variable-spec = <whitespace>? identifier (<whitespace>? <comma> <whitespace>? identifier)* <whitespace>? <colon> <whitespace>? data-type (<whitespace>? <colon> <whitespace>? variable-initialization)?
<whitespace> = #'[ \t]+'
<data-type> = elementary-type | derived-type | array-type | struct-type
elementary-type = 'BOOL' | 'BYTE' | 'WORD' | 'DWORD' | 'LWORD' | 
                 'SINT' | 'INT' | 'DINT' | 'LINT' | 
                 'USINT' | 'UINT' | 'UDINT' | 'ULINT' | 
                 'REAL' | 'LREAL' | 'TIME' | 'DATE' | 'TIME_OF_DAY' | 'DATE_AND_TIME' | 
                 'STRING' | 'WSTRING'
derived-type = identifier
array-type = 'ARRAY' <open-bracket> subrange (<comma> subrange)* <close-bracket> 'OF' <whitespace> data-type
subrange = signed-integer? <range-separator> signed-integer?
struct-type = 'STRUCT' <EOL> (struct-element <semicolon> <EOL>)* 'END_STRUCT'
struct-element = identifier <colon> data-type
variable-initialization = constant-expression

<network-list> = network (<EOL> network)*
network = network-label? network-comment? rung <EOL>?
network-label = 'NETWORK' <whitespace> (integer | identifier) <colon> <whitespace>? (string-constant | unquoted-text)? <EOL>
unquoted-text = #'[^\r\n]+'
network-comment = comment
rung = left-power-rail ladder-elements right-power-rail (<EOL> parallel-branch-continuation)*
parallel-branch-continuation = <'|'> <whitespace>* (('+--' ladder-elements) | right-power-rail)?
left-power-rail = <'|'>
right-power-rail = <'|'>
<ladder-elements> = ladder-element*
ladder-element = ladder-contact | ladder-coil | ladder-function | ladder-branch
<ladder-contact> = normal-contact | negated-contact | positive-transition-contact | negative-transition-contact
normal-contact = <'--| |--'> identifier <'--'>?
negated-contact = <'--|/|--'> identifier <'--'>?
positive-transition-contact = <'--|P|--'> identifier <'--'>?
negative-transition-contact = <'--|N|--'> identifier <'--'>?
<ladder-coil> = normal-coil | negated-coil | set-coil | reset-coil | positive-transition-coil | negative-transition-coil
normal-coil = <'--|( )--'> identifier <'--'>
negated-coil = <'--|/( )--'> identifier <'--'>
set-coil = <'--|S( )--'> identifier <'--'>
reset-coil = <'--|R( )--'> identifier <'--'>
positive-transition-coil = <'--|P( )--'> identifier <'--'>
negative-transition-coil = <'--|N( )--'> identifier <'--'>
ladder-function = <'--['> function-name function-parameters <']--'>
function-name = identifier
function-parameters = <open-paren> parameter-list? <close-paren>
parameter-list = parameter (<comma> parameter)*
parameter = (identifier <assign>)? expression
<expression> = simple-expression | structured-expression
simple-expression = term (addition-operator term)*
term = factor (multiplication-operator factor)*
factor = variable | constant | function-call | <open-paren> expression <close-paren>
addition-operator = <'+'> | <'-'> | <'OR'> | <'XOR'>
multiplication-operator = <'*'> | <'/'> | <'MOD'> | <'AND'> | <'&'>
structured-expression = <open-brace> member-list <close-brace>
member-list = member (<comma> member)*
member = (member-name <assign>)? value
member-name = identifier
value = expression
function-call = identifier <open-paren> parameter-list? <close-paren>
<ladder-branch> = parallel-branch | divergent-branch
parallel-branch = <'--+'> ladder-element* <'+--'>?
divergent-branch = <'--<'> ladder-element* <'>--'>
<identifier> = #'[a-zA-Z][a-zA-Z0-9_]*'
<integer> = #'[0-9]+'
signed-integer = sign? integer
<sign> = '+' | '-'
<constant-expression> = constant | expression
<constant> = boolean-constant | numeric-constant | time-constant | date-constant | string-constant
boolean-constant = 'TRUE' | 'FALSE' | '1' | '0'
<numeric-constant> = integer-constant | real-constant
integer-constant = sign? decimal-integer | sign? <'2#'> binary-integer | sign? <'8#'> octal-integer | sign? <'16#'> hex-integer
<decimal-integer> = integer
binary-integer = binary-digit+
binary-digit = '0' | '1'
octal-integer = octal-digit+
octal-digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
hex-integer = hex-digit+
hex-digit = #'[0-9a-fA-F]'
real-constant = decimal-integer <'.'> decimal-integer exponent?
exponent = ('E' | 'e') sign? decimal-integer
time-constant = ('T' | 'TIME')? <'#'> days? hours? minutes? seconds? milliseconds?
days = decimal-integer <'d'>
hours = decimal-integer <'h'>
minutes = decimal-integer <'m'>
seconds = decimal-integer <'s'>
milliseconds = decimal-integer <'ms'>
date-constant = ('D' | 'DATE') <'#'> year <'-'> month <'-'> day
year = decimal-integer
month = decimal-integer
day = decimal-integer
string-constant = #\"'[^']*'\" | #\"\\\"[^\\\"]*\\\"\"
<variable> = identifier
<comment> = <'(*'> #'(?s).*?(?=\\*\\))' <'*)'> | <'//'> #'[^\\r\\n]*' <EOL>
<EOL> = #'[\r\n]+' | '\r\n'
<semicolon> = ';'
<colon> = ':'
<comma> = ','
<open-bracket> = '['
<close-bracket> = ']'
<open-paren> = '('
<close-paren> = ')'
<open-brace> = '{'
<close-brace> = '}'
<assign> = ':='
<range-separator> = '..'
")

(def ld-parser
  (insta/parser ld-grammar))

(defn transform-program-declaration [& args]
  {:type (first args)
   :name (second args)})

(defn transform-program-termination [termination]
  {:type termination})

(defn transform-variable-declaration [& args]
  (let [var-type (first args)
        var-qualifier (when (contains? #{"CONSTANT" "RETAIN" "NON_RETAIN"} (second args))
                        (second args))
        specs (if var-qualifier
                (drop 2 args)
                (rest args))]
    {:type var-type
     :qualifier var-qualifier
     :variables specs}))

(defn transform-variable-spec [& args]
  (let [identifiers (butlast (butlast args))
        data-type (nth (reverse args) 1)
        initialization (when (> (count args) (+ (count identifiers) 1))
                         (last args))]
    {:identifiers identifiers
     :data-type data-type
     :initialization initialization}))

(defn transform-network [& args]
  (let [components (vec args)
        label (when (and (> (count components) 0)
                         (= :network-label (get-in components [0 :type])))
                (first components))
        comment (when (and (> (count components) (if label 1 0))
                           (= :network-comment (get-in components [(if label 1 0)] :type)))
                  (nth components (if label 1 0)))
        rung (nth components (if comment (if label 2 1) (if label 1 0)))]
    {:type :network
     :label label
     :comment comment
     :rung rung}))

(defn transform-network-label [& args]
  {:type :network-label
   :number (first args)
   :title (when (> (count args) 1) (second args))})

(defn transform-rung [& args]
  (let [elements (second args)  ;; Initial ladder-elements
        continuations (drop 3 args)  ;; Parallel branch continuations
        filtered-continuations (filter #(not (nil? (get % 2))) continuations)]  ;; Keep only non-empty continuations
    {:type :rung
     :elements (concat elements (mapcat #(get % 2) filtered-continuations))}))

(defn transform-ladder-contact [contact-type identifier]
  {:type contact-type
   :variable identifier})

(defn transform-ladder-coil [coil-type identifier]
  {:type coil-type
   :variable identifier})

(defn transform-ladder-function [& args]
  {:type :function
   :name (first args)
   :parameters (rest args)})

(defn transform-ladder-branch [branch-type & elements]
  {:type branch-type
   :elements elements})

(defn transform-parameter [& args]
  (if (= 2 (count args))
    {:name (first args)
     :value (second args)}
    {:value (first args)}))

(def ld-transformer
  {:program-declaration transform-program-declaration
   :program-termination transform-program-termination
   :variable-declaration transform-variable-declaration
   :variable-spec transform-variable-spec
   :network transform-network
   :network-label transform-network-label
   :rung transform-rung
   :normal-contact (partial transform-ladder-contact :normal-contact)
   :negated-contact (partial transform-ladder-contact :negated-contact)
   :positive-transition-contact (partial transform-ladder-contact :positive-transition-contact)
   :negative-transition-contact (partial transform-ladder-contact :negative-transition-contact)
   :normal-coil (partial transform-ladder-coil :normal-coil)
   :negated-coil (partial transform-ladder-coil :negated-coil)
   :set-coil (partial transform-ladder-coil :set-coil)
   :reset-coil (partial transform-ladder-coil :reset-coil)
   :positive-transition-coil (partial transform-ladder-coil :positive-transition-coil)
   :negative-transition-coil (partial transform-ladder-coil :negative-transition-coil)
   :ladder-function transform-ladder-function
   :parallel-branch (partial transform-ladder-branch :parallel-branch)
   :divergent-branch (partial transform-ladder-branch :divergent-branch)
   :parameter transform-parameter
   :identifier identity
   :integer #(Integer/parseInt %)
   :boolean-constant #(Boolean/parseBoolean %)
   :string-constant identity})

(defn parse-ld [input]
  (insta/transform ld-transformer (ld-parser input)))

(defn compile-to-il [ast]
  (let [buffer (StringBuilder.)]
    (doseq [node ast]
      (case (if (map? node) (:type node) node)
        "PROGRAM" (.append buffer (str "PROGRAM " (:name node) "\n"))
        "VAR" (do
                (.append buffer (str "VAR\n"))
                (doseq [var-spec (:variables node)]
                  (.append buffer (str (str/join ", " (:identifiers var-spec))
                                       " : " (:data-type var-spec) ";\n")))
                (.append buffer "END_VAR\n"))
        :network (do
                   (when-let [label (:label node)]
                     (.append buffer (str "NETWORK " (:number label)
                                          (when (:title label) (str " : " (:title label)))
                                          "\n")))
                   (let [elements (get-in node [:rung :elements])]
                     (when (seq elements)
                       (letfn [(compile-branch [branch-elems]
                                 (let [branch-buffer (StringBuilder.)]
                                   (doseq [elem branch-elems]
                                     (case (:type elem)
                                       :normal-contact (.append branch-buffer (str "LD " (:variable elem) "\n"))
                                       :negated-contact (.append branch-buffer (str "LDN " (:variable elem) "\n"))
                                       :normal-coil (.append buffer (str "ST " (:variable elem) "\n"))
                                       :parallel-branch (let [sub-branch (compile-branch (:elements elem))]
                                                          (.append branch-buffer sub-branch))))
                                   (.toString branch-buffer)))
                               (compile-elements [elems]
                                 (when (seq elems)
                                   (let [elem (first elems)]
                                     (case (:type elem)
                                       :normal-contact (do
                                                         (.append buffer (str "LD " (:variable elem) "\n"))
                                                         (compile-rest (rest elems)))
                                       :negated-contact (do
                                                          (.append buffer (str "LDN " (:variable elem) "\n"))
                                                          (compile-rest (rest elems)))
                                       :normal-coil (do
                                                      (.append buffer (str "ST " (:variable elem) "\n")))
                                       :parallel-branch (let [branch-output (compile-branch (:elements elem))]
                                                          (.append buffer branch-output)
                                                          (when (seq (rest elems))
                                                            (.append buffer "OR\n"))
                                                          (compile-rest (rest elems)))
                                       nil))))
                               (compile-rest [elems]
                                 (doseq [elem elems]
                                   (case (:type elem)
                                     :normal-contact (.append buffer (str "AND " (:variable elem) "\n"))
                                     :negated-contact (.append buffer (str "ANDN " (:variable elem) "\n"))
                                     :normal-coil (.append buffer (str "ST " (:variable elem) "\n"))
                                     :parallel-branch (let [branch-output (compile-branch (:elements elem))]
                                                        (.append buffer branch-output)
                                                        (.append buffer "OR\n")))))]
                         (compile-elements elements))))
                   (.append buffer "\n"))
        "END_PROGRAM" (.append buffer "END_PROGRAM\n")))
    (.toString buffer)))

(defn compile-to-c [ast]
  "#include <stdio.h>\n#include <bool.h>\n\n// Generated C code for IEC 61131-3 LD program\n")

(defn compile-to-js [ast]
  "// Generated JavaScript code for IEC 61131-3 LD program\n")

(defn compile-to-executable [ast target-language]
  (case target-language
    "c" (compile-to-c ast)
    "js" (compile-to-js ast)
    (throw (Exception. (str "Unsupported target language: " target-language)))))

(defn compile-ld [input & {:keys [target-language] :or {target-language "il"}}]
  (try
    (let [input (str input "\n")  ;; Ensure input ends with newline
          ast (parse-ld input)]
      (if (insta/failure? ast)
        (throw (Exception. (str "Parsing failed: " (pr-str ast))))
        (case target-language
          "il" (compile-to-il ast)
          ("c" "js") (compile-to-executable ast target-language)
          (throw (Exception. (str "Unsupported target language: " target-language))))))
    (catch Exception e
      (str "Error: " (.getMessage e)))))

(defn -main [& args]
  (if (and (>= (count args) 2) (= (first args) "-i"))
    (let [input-file (second args)
          target-language (if (and (>= (count args) 4) (= (nth args 2) "-t"))
                            (nth args 3)
                            "il")
          input (slurp input-file)
          output (compile-ld input :target-language target-language)]
      (println output))
    (println "Usage: ld-compiler -i <input-file> [-t <target-language>]")))

(comment
  (def example-ld "
PROGRAM Example
VAR
  Input1, Input2 : BOOL;
  Output1 : BOOL;
END_VAR

NETWORK 1: Main Logic
|--| |--Input1--|/|--Input2--|(  )--Output1--|
|
")
  (def parsed (parse-ld example-ld))
  (println (compile-ld example-ld))
  (println (compile-ld example-ld :target-language "c"))

  (require '[iec61131.ld-compiler :as ld] :reload)
  (def file-content (slurp "resources/examples/motorcontrol.ld"))
  (println "Compilation result:")
  (println (compile-ld file-content)))