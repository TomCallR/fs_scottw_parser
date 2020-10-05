//
// SECOND POST
// PARTIE REECRITURE AVEC BIND
//
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
//

open System

type Parser<'T> = Parser of (string -> Result<'T * string, string>)

let pchar charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty(str) then
            Error "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Ok(charToMatch, remaining)
            else
                let msg =
                    sprintf "Expecting '%c'. Got '%c'" charToMatch first

                Error msg

    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input

let returnP x =
    let innerFn input = Ok(x, input)
    Parser innerFn

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let innerFn input =
        let res = run p input
        match res with
        | Ok (value, remains) ->
            let p2 = f value
            run p2 remains
        | Error err -> Error err
    Parser innerFn

let (>>=) p f = bindP f p

let mapP f =
    bindP (f >> returnP)

let andThen p1 p2 =
    p1 >>= (fun p1res ->
    p2 >>= (fun p2res ->
    returnP (p1res, p2res)))

let (.>>.) = andThen

let applyP fP xP =
    fP >>= (fun f ->
    xP >>= (fun x ->
    returnP (f x)))

let (<*>) = applyP

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Error _ ->
            let result2 = run parser2 input
            result2
        | _ -> result1

    Parser innerFn

let (<|>) = orElse

let choice listOfParsers = List.reduce (<|>) listOfParsers
let anyOf listOfChars = listOfChars |> List.map pchar |> choice

let (<!>) = mapP
let (|>>) x f = mapP f x

let parseDigit = anyOf [ '0' .. '9' ]

let parseThreeDigitsAsStr =
    let tupleParser =
        parseDigit .>>. parseDigit .>>. parseDigit

    let transformTuple ((c1, c2), c3) = String [| c1; c2; c3 |]
    mapP transformTuple tupleParser

let parseThreeDigitsAsInt = mapP int parseThreeDigitsAsStr

// run parseThreeDigitsAsStr "123A" |> (printfn "%A")
// run parseThreeDigitsAsInt "123A" |> (printfn "%A")

let lift2 f xP yP =
    returnP f <*> xP <*> yP

let addP =
    lift2 (+)

let startsWith (str: string) (prefix: string) =
    str.StartsWith(prefix)

let startsWithP =
    lift2 startsWith

let rec sequence parserList =
    let cons head tail = head::tail
    let consP = lift2 cons
    match parserList with
    | [] -> returnP []
    | headP :: tailP -> consP headP (sequence tailP) 

let charListToString charList =
    String (List.toArray charList)

let pstring str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToString

let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Error _ -> ([], input)
    | Ok(firstValue, inputAfterFirstPass) ->
        let (followingValues, remainingInput) = parseZeroOrMore parser inputAfterFirstPass
        let values = firstValue :: followingValues
        (values, remainingInput)

let many parser =
    let innerFn input =
        Ok (parseZeroOrMore parser input)
    Parser innerFn

let many1 p =
    p >>= (fun head ->
    many p >>= (fun tail ->
        returnP (head::tail)))

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let pint =
    let resultToInt (sign, digitList) =
        let value = String(List.toArray digitList) |> int
        match sign with
        | Some _ -> -value
        | None -> value
    let digit = anyOf ['0'..'9']
    let digitsWithSign = opt (pchar '-') .>>. (many1 digit)
    digitsWithSign
    |>> resultToInt

let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (v1, v2) ->v1)

let (>>.) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (v1, v2) ->v2)

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenp = sep >>. p
    let plist = many (sep >>. p)
    p .>>. (many sepThenp)
    |>> (fun (head, tail) -> head::tail)

let sepBy p sep =
    sepBy1 p sep <|> returnP []


// Learn more about F# at http://fsharp.org
module Tests =

    // let parseA = pchar 'A'
    // run parseA "ABC" |> (printfn "%A")

    // let parseA = pchar 'A'
    // let parseB = pchar 'B'
    // let parseC = pchar 'C'
    // let parserAandB = parseA .>>. parseB
    // let parserAorB = parseA <|> parseB
    // let bOrElseC = parseB <|> parseC
    // let aAndThenBorC = parseA .>>. bOrElseC
    // run parserAandB "ABC" |> (printfn "%A")
    // run parserAandB "ZBC" |> (printfn "%A")
    // run parserAandB "AZC" |> (printfn "%A")
    // run parserAorB "AZZ" |> (printfn "%A")
    // run parserAorB "BZZ" |> (printfn "%A")
    // run parserAorB "CZZ" |> (printfn "%A")
    // run aAndThenBorC "ABZ" |> (printfn "%A")
    // run aAndThenBorC "ACZ" |> (printfn "%A")
    // run aAndThenBorC "QBZ" |> (printfn "%A")
    // run aAndThenBorC "AQZ" |> (printfn "%A")

    // let parseLowercase = anyOf [ 'a' .. 'z' ]


    // run parseLowercase "aBC" |> (printfn "%A") // Ok ('a', "BC")
    // run parseLowercase "ABC" |> (printfn "%A") // Error "Expecting 'z'. Got 'A'"
    // run parseDigit "1ABC" |> (printfn "%A") // Ok ("1", "ABC")
    // run parseDigit "9ABC" |> (printfn "%A") // Ok ("9", "ABC")
    // run parseDigit "|ABC" |> (printfn "%A") // Error "Expecting '9'. Got '|'"

    // run parseA "ABC" |> (printfn "%A")

    // run parserAandB "ABC" |> (printfn "%A")
    // run parserAandB "ZBC" |> (printfn "%A")
    // run parserAandB "AZC" |> (printfn "%A")
    // run parserAorB "AZZ" |> (printfn "%A")
    // run parserAorB "BZZ" |> (printfn "%A")
    // run parserAorB "CZZ" |> (printfn "%A")
    // run aAndThenBorC "ABZ" |> (printfn "%A")
    // run aAndThenBorC "ACZ" |> (printfn "%A")
    // run aAndThenBorC "QBZ" |> (printfn "%A")
    // run aAndThenBorC "AQZ" |> (printfn "%A")

    // run parseLowercase "aBC" |> (printfn "%A") // Ok ('a', "BC")
    // run parseLowercase "ABC" |> (printfn "%A") // Error "Expecting 'z'. Got 'A'"
    // run parseDigit "1ABC" |> (printfn "%A") // Ok ("1", "ABC")
    // run parseDigit "9ABC" |> (printfn "%A") // Ok ("9", "ABC")
    // run parseDigit "|ABC" |> (printfn "%A") // Error "Expecting '9'. Got '|'"

    // run parseThreeDigitsAsStr "123A" |> (printfn "%A")
    // run parseThreeDigitsAsInt "123A" |> (printfn "%A")

    // let parsers = [pchar 'A'; pchar 'B'; pchar 'C']
    // let combined = sequence parsers
    // run combined "ABCD" |> (printfn "%A")

    // let parseABC = pstring "ABC"
    // run parseABC "ABCDEF" |> (printfn "%A")
    // run parseABC "A|CDEF" |> (printfn "%A")
    // run parseABC "AB|DEF" |> (printfn "%A")

    // let manyA = many (pchar 'A')
    // run manyA "ABCD" |> (printfn "%A")
    // run manyA "AACD" |> (printfn "%A")
    // run manyA "AAAD" |> (printfn "%A")
    // run manyA "BCD" |> (printfn "%A")

    // let manyAB = many (pstring "AB")
    // run manyAB "ABCD" |> (printfn "%A")  // Success (["AB"], "CD")
    // run manyAB "ABABCD" |> (printfn "%A")  // Success (["AB"; "AB"], "CD")
    // run manyAB "ZCD" |> (printfn "%A")  // Success ([], "ZCD")
    // run manyAB "AZCD" |> (printfn "%A")  // Success ([], "AZCD")

    // let whitespaceChar = anyOf [' '; '\t'; '\n']
    // let whitespace = many whitespaceChar

    // run whitespace "ABC" |> (printfn "%A")  // Success ([], "ABC")
    // run whitespace " ABC" |> (printfn "%A")  // Success ([' '], "ABC")
    // run whitespace "\tABC" |> (printfn "%A")  // Success (['\t'], "ABC")

    // let digit = anyOf ['0'..'9']

    // define parser for one or more digits
    // let digits = many1 digit
    // run digits "1ABC" |> (printfn "%A")  // Success (['1'], "ABC")
    // run digits "12BC" |> (printfn "%A")  // Success (['1'; '2'], "BC")
    // run digits "123C" |> (printfn "%A")  // Success (['1'; '2'; '3'], "C")
    // run digits "1234" |> (printfn "%A")  // Success (['1'; '2'; '3'; '4'], "")

    // run digits "ABC" |> (printfn "%A")   // Failure "Expecting '9'. Got 'A'"

    // run pint "1ABC" |> (printfn "%A")  // Success (1, "ABC")
    // run pint "12BC" |> (printfn "%A")  // Success (12, "BC")
    // run pint "123C" |> (printfn "%A")  // Success (123, "C")
    // run pint "1234" |> (printfn "%A")  // Success (1234, "")

    // run pint "ABC" |> (printfn "%A")   // Failure "Expecting '9'. Got 'A'"

    // let digit = anyOf ['0'..'9']
    // let digitThenSemicolon = digit .>>. opt (pchar ';')

    // run (opt (pchar ';')) ";1" |> printfn "%A"
    // run (opt (pchar ';')) "ze;1" |> printfn "%A"
    // printfn "%A" (run digitThenSemicolon "1;")  // Success (('1', Some ';'), "")
    // printfn "%A" (run digitThenSemicolon "1")   // Success (('1', None), "")

    // printfn "%A" (run pint "123C")   // Success (123, "C")
    // printfn "%A" (run pint "-123C")  // Success (-123, "C")

    // let pdoublequote = pchar '"'
    // let quotedInteger = between pdoublequote pint pdoublequote
    // run quotedInteger "\"1234\"" |> printfn "%A"   // Success (1234, "")
    // run quotedInteger "1234" |> printfn "%A"       // Failure "Expecting '"'. Got '1'"

    let comma = pchar ','
    let digit = anyOf ['0'..'9']

    let zeroOrMoreDigitList = sepBy digit comma
    let oneOrMoreDigitList = sepBy1 digit comma

    run oneOrMoreDigitList "1;" |> printfn "%A"       // Success (['1'], ";")
    run oneOrMoreDigitList "1,2;" |> printfn "%A"     // Success (['1'; '2'], ";")
    run oneOrMoreDigitList "1,2,3;" |> printfn "%A"   // Success (['1'; '2'; '3'], ";")
    run oneOrMoreDigitList "Z;" |> printfn "%A"       // Failure "Expecting '9'. Got 'Z'"

    run zeroOrMoreDigitList "1;" |> printfn "%A"      // Success (['1'], ";")
    run zeroOrMoreDigitList "1,2;" |> printfn "%A"    // Success (['1'; '2'], ";")
    run zeroOrMoreDigitList "1,2,3;" |> printfn "%A"  // Success (['1'; '2'; '3'], ";")
    run zeroOrMoreDigitList "Z;" |> printfn "%A"      // Success ([], "Z;")
