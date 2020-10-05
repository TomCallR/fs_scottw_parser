namespace ParserLibrary

open System
open Parser

module Json =

    type JValue =
        | JString of string
        | JNumber of float
        | JBool of bool
        | JNull
        | JObject of Map<string, JValue>
        | JArray of JValue list

    // applies the parser p, ignores the result, and returns x.
    let (>>%) p x =
        p |>> (fun _ -> x)

    let jNull = pstring "null" >>% JNull

    let jBool =
        let jTrue = pstring "true" >>% JBool true
        let jFalse = pstring "false" >>% JBool false
        jTrue <|> jFalse
        <?> "bool"
    
    let jUnescapedChar =
        satisfy (fun ch -> ch<>'\\' && ch<>'\"') "unescaped char"

        