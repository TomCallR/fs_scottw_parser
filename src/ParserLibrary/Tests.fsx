//
// TESTS
//
#load "ParserLib.fs"
#load "JsonLib.fs"

open System
open ParserLibrary
open ParserLibrary.Parser
open ParserLibrary.Json



//run pint "-123Z"
//|> printResult
//// -123

//printfn "-----"

//run pint "-Z123"
//|> printResult
//// Line:0 Col:1 Error parsing integer
//// -Z123
//// ^Unexpected 'Z'

//printfn "-----"

//run pfloat "-123.45Z"
//|> printResult
//// -123.45

//printfn "-----"

//run pfloat "-123Z45"
//|> printResult
//// Line:0 Col:4 Error parsing float
//// -123Z45
//// ^Unexpected 'Z'

//printfn "-----"

//run jNull "null" |> printResult   
//// Success: JNull

//printfn "-----"

//run jNull "nulp" |> printResult  
//// Line:0 Col:3 Error parsing null
//// nulp
//// ^Unexpected 'p'

//printfn "-----"

//run jBool "true" |> printResult
//// Success: JBool true

//printfn "-----"

//run jBool "false" |> printResult
//// Success: JBool false

//printfn "-----"

//run jBool "truX" |> printResult  
//// Line:0 Col:0 Error parsing bool
//// truX
//// ^Unexpected 't'

printfn "-----"

run jUnescapedChar "a" |> printResult 
// Success 'a'

printfn "-----"

run jUnescapedChar "\\" |> printResult
// Line:0 Col:0 Error parsing char
// \
// ^Unexpected '\'

printfn "-----"


