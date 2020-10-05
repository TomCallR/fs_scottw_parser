namespace ParserLibrary

//
// THIRD POST
// REECRITURE AVEC MESSAGES D'ERREUR AMELIORES
//
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/
//

// From https://gist.github.com/swlaschin/485f418fede6b6a36d89#file-understanding_parser_combinators-3-fsx
module TextInput =

    open System

    type Position = { Line: int; Column: int }

    // Define the current input state
    type InputState =
        { Lines: array<string>
          Position: Position }

    // define an initial position
    let initialPos = { Line = 0; Column = 0 }

    // increment the column number
    let incrCol (pos: Position) = { pos with Column = pos.Column + 1 }

    // increment the line number (also set column to 0)
    let incrLine pos = { Line = pos.Line + 1; Column = 0 }

    // init InputState from string
    let fromStr str =
        if String.IsNullOrEmpty(str) then
            { Lines = [||]; Position = initialPos }
        else
            let separators = [| "\r\n"; "\n" |]

            let lines =
                str.Split(separators, StringSplitOptions.None)

            { Lines = lines; Position = initialPos }

    // return the current line
    let currentLine (inputState: InputState) =
        let linePos = inputState.Position.Line
        if linePos < inputState.Lines.Length then inputState.Lines.[linePos] else "EOF"

    /// Get the next character from the input, if any
    /// else return None. Also return the updated InputState
    let nextChar (input: InputState) =
        let linePos = input.Position.Line
        let colPos = input.Position.Column
        if linePos >= input.Lines.Length then
            (input, None)
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let ch = currentLine.[colPos]
                let newPos = incrCol input.Position
                let newState = { input with Position = newPos }
                (newState, Some ch)
            else
                let ch = '\n'
                let newPos = incrLine input.Position
                let newState = { input with Position = newPos }
                (newState, Some ch)

/// Test
// module Input_Test =
//     let rec readAllChars input =
//         [ let remainingInput, charOpt = nextChar input
//           match charOpt with
//           | None ->
//               // end of input
//               ()
//           | Some ch ->
//               // return first character
//               yield ch
//               // return the remaining characters
//               yield! readAllChars remainingInput ]

//     fromStr "" |> readAllChars |> printfn "%A" // []
//     fromStr "a" |> readAllChars |> printfn "%A" // ['a'; '\n']
//     fromStr "ab" |> readAllChars |> printfn "%A" // ['a'; 'b'; '\n']
//     fromStr "a\nb" |> readAllChars |> printfn "%A" // ['a'; '\n'; 'b'; '\n']


// ===========================================
// Parser code
// ===========================================

module Parser =

    open System

    type Input = TextInput.InputState
    type ParserLabel = string
    type ParserError = string

    // Position for an error
    type ParserPosition =
        { CurrentLine: string
          Line: int
          Column: int }

    type Parser<'T> =
        { ParseFn: (Input -> Result<'T * Input, ParserLabel * ParserError * ParserPosition>)
          Label: ParserLabel }

    /// Run the parser on a InputState
    let runOnInput parser input =
        // call inner function with input
        parser.ParseFn input

    /// Run the parser on a string
    let run parser inputStr =
        // call inner function with input
        runOnInput parser (TextInput.fromStr inputStr)

    // =============================================
    // Error messages
    // =============================================

    // Print parsing result
    let printResult result =
        match result with
        | Ok (value, _) -> printfn "%A" value
        | Error (label, err, ppos) ->
            let errorLine = ppos.CurrentLine
            let colPos = ppos.Column
            let linePos = ppos.Line
            let failureCaret = sprintf "%*s^%s" colPos "" err
            printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

    let parserPositionFromInputState (inputState: Input) =
        { CurrentLine = TextInput.currentLine inputState
          Line = inputState.Position.Line
          Column = inputState.Position.Column }

    // Get and set label of a parser
    let getLabel parser = parser.Label

    let setLabel parser newLabel =
        let newFn input =
            let res = runOnInput parser input
            match res with
            | Ok (values) -> Ok(values)
            | Error (label, err, ppos) -> Error(newLabel, err, ppos)

        { ParseFn = newFn; Label = newLabel }

    let (<?>) = setLabel

    // Parser from value
    let returnP x =
        let innerFn input = Ok(x, input)
        { ParseFn = innerFn; Label = "" }

    /// "bindP" takes a parser-producing function f, and a parser p
    /// and passes the output of p into f, to create a new parser
    let bindP f p =
        let label = "unknown"

        let innerFn input =
            let res1 = runOnInput p input
            match res1 with
            | Ok (value1, remainingInput) ->
                let p2 = f value1
                runOnInput p2 remainingInput
            | Error (label, msg, ppos) -> Error(label, msg, ppos)

        { ParseFn = innerFn; Label = label }

    let (>>=) p f = bindP f p

    // Map
    let mapP f = bindP (f >> returnP)

    let (<!>) = mapP
    let (|>>) x f = mapP f x

    // Apply
    let applyP fP xP = fP >>= (fun f -> xP >>= (f >> returnP))
    let (<*>) = applyP

    // Lift2 : apply for two parameter functions
    let lift2 f xP yP = returnP f <*> xP <*> yP

    // Sequence
    let rec sequence parserList =
        let cons head tail = head :: tail
        let consP = lift2 cons
        match parserList with
        | [] -> returnP []
        | headP :: tailP -> consP headP (sequence tailP)

    // AndThen
    let andThen p1 p2 =
        let label =
            sprintf "%s andThen %s" (getLabel p1) (getLabel p2)

        p1
        >>= (fun p1res -> p2 >>= (fun p2res -> returnP (p1res, p2res)))
        <?> label

    let (.>>.) = andThen

    // OrElse
    let orElse parser1 parser2 =
        let label =
            sprintf "%s orElse %s" (getLabel parser1) (getLabel parser2)

        let innerFn input =
            let result1 = parser1.ParseFn input
            match result1 with
            | Ok (_) -> result1
            | Error (_) ->
                let result2 = runOnInput parser2 input
                match result2 with
                | Ok (values) -> Ok(values)
                | Error (label, err, ppos) -> Error(label, err, ppos)

        { ParseFn = innerFn; Label = label }

    let (<|>) = orElse

    //
    let rec parseZeroOrMore parser input =
        let firstResult = runOnInput parser input
        match firstResult with
        | Error _ -> ([], input)
        | Ok (firstValue, inputAfterFirstPass) ->
            let (followingValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstPass

            let values = firstValue :: followingValues
            (values, remainingInput)

    // parser repeated 0 to n times
    let many parser =
        let label = sprintf "many %s" (getLabel parser)
        let innerFn input = Ok(parseZeroOrMore parser input)
        { ParseFn = innerFn; Label = label }

    /// matches one or more occurences of the specified parser
    let many1 p =
        let label = sprintf "many1 %s" (getLabel p)
        p
        >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail)))
        <?> label

    // opt
    let opt p =
        let label = sprintf "opt %s" (getLabel p)
        let some = p |>> Some
        let none = returnP None
        some <|> none <?> label

    // orElse generalized
    let choice listOfParsers = List.reduce (<|>) listOfParsers

    // tools to build parsers that discard some caracters
    let (.>>) p1 p2 = p1 .>>. p2 |> mapP (fun (v1, v2) -> v1)

    let (>>.) p1 p2 = p1 .>>. p2 |> mapP (fun (v1, v2) -> v2)

    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        let sepThenp = sep >>. p
        let plist = many (sep >>. p)
        p
        .>>. (many sepThenp)
        |>> (fun (head, tail) -> head :: tail)

    let sepBy p sep = sepBy1 p sep <|> returnP []

    /// Match an input token if the predicate is satisfied
    let satisfy predicate label =
        let innerFn input =
            let remainingInput, charOpt = TextInput.nextChar input
            match charOpt with
            | None ->
                let err = "No more input"
                let ppos = parserPositionFromInputState input
                Error(label, err, ppos)
            | Some first ->
                if predicate first then
                    Ok(first, remainingInput)
                else
                    let err = sprintf "Unexpected '%c'" first
                    let ppos = parserPositionFromInputState input
                    Error(label, err, ppos)

        { ParseFn = innerFn; Label = label }

    // parser from char
    let pchar charToMatch =
        let label = sprintf "%c" charToMatch
        let predicate ch = (ch = charToMatch)
        satisfy predicate label

    // parser from string
    let charListToString charList = String(List.toArray charList)

    // one parser in the list
    let anyOf listOfChars =
        let label = sprintf "any of %A" listOfChars
        listOfChars |> List.map pchar |> choice <?> label

    /// Parses a sequence of zero or more chars with the char parser cp.
    /// It returns the parsed chars as a string.
    let manyChars cp = many cp |>> charListToString

    /// Parses a sequence of one or more chars with the char parser cp.
    /// It returns the parsed chars as a string.
    let manyChars1 cp = many1 cp |>> charListToString

    /// parse a specific string
    let pstring str =
        let label = str
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP charListToString
        <?> label

    // let digitChar =
//     let predicate = Char.IsDigit
//     let label = "digit"
//     satisfy predicate label

    /// parse a whitespace char
    let whitespaceChar =
        let predicate = Char.IsWhiteSpace
        let label = "whitespace"
        satisfy predicate label

    /// parse zero or more whitespace char
    let spaces = many whitespaceChar

    /// parse one or more whitespace char
    let spaces1 = many1 whitespaceChar

    /// parse a digit
    let digitChar =
        let predicate = Char.IsDigit
        let label = "digit"
        satisfy predicate label

    /// parse an integer
    let pint =
        let resultToInt (sign, digits) =
            let i = int digits
            match sign with
            | Some ch -> -i
            | None -> i
        // define parser for one or more digits
        let digits = manyChars1 digitChar
        let label = "integer"
        opt (pchar '-')
        .>>. digits
        |> mapP resultToInt
        <?> label


    // parse a float
    let pfloat =
        let label = "float"
        //helper
        let resultToFloat (((sign, digits1), point), digits2) =
            let fl = sprintf "%s.%s" digits1 digits2 |> float
            match sign with
            | Some ch -> -fl
            | None -> fl
        // define parser for one or more digits
        let digits = manyChars1 digitChar
        opt (pchar '-')
        .>>. digits
        .>>. (pchar '.')
        .>>. digits
        |> mapP resultToFloat
        <?> label
