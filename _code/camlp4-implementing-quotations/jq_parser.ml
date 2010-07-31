open Camlp4.PreCast
open Jq_ast

module Gram = MakeGram(Lexer)
let json = Gram.Entry.mk "json"

;;

EXTEND Gram
  json: [[
      "null" -> Jq_null
    | "true" -> Jq_bool true
    | "false" -> Jq_bool false
    | i = INT -> Jq_number (float_of_string i)
    | f = FLOAT -> Jq_number (float_of_string f)
    | s = STRING -> Jq_string s
    | "["; es = LIST0 json SEP ","; "]" -> Jq_array es
    | "{";
        kvs =
          LIST0
            [ s = STRING; ":"; j = json -> (s, j) ]
            SEP ",";
      "}" -> Jq_object kvs
  ]];
END
