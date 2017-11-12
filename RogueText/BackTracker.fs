module RogueText.BackTracker

open RogueText
open RogueText.Tokenizer

type private InternalState = 
    | ReadingAttributes
    | ReadingContents
    | StartTag
    | EndTag
    
let private getNext position (items: _ array) =
    if position >= 0 && position < items.Length then Some items.[position]
    else None
    
let private consumeAttribute (position: int) (tokens: AttributeToken array) =
    match tokens.[position] with
    | AttributeToken.Identifier(identifier) -> 
        match tokens |> getNext (position + 1) with
        | Some AttributeToken.Equals ->
            match tokens |> getNext(position + 2) with
            | None -> 
                (Result.Error <| sprintf "Expected value for \"%s\"." identifier), 0
            | Some AttributeToken.Equals -> 
                (Result.Error "Unexpected equals."),0
            | Some( AttributeToken.Identifier(value)) -> 
                (Result.Ok(identifier, Some value)), 3
        | Some(AttributeToken.Identifier(_)) 
        | None -> 
            (Result.Ok(identifier, None)), 1
    | AttributeToken.Equals -> 
        (Result.Error "Unexpected equals."),0

let ReadAttributes (text: string) =
    let tokens = TokenizeAttributes text

    let rec eatTokens nextPosition attributes =
        let next = tokens |> consumeAttribute nextPosition
        match next with
        | Result.Ok(identifier, value), consumed ->
            let attributes =
                if System.String.IsNullOrWhiteSpace identifier then
                    attributes
                else
                    Map.add identifier value attributes
            if consumed + nextPosition >= tokens.Length then
                Result.Ok attributes
            else
                eatTokens (nextPosition + consumed) attributes
        | Result.Error(error), _ -> 
            Result.Error error

    if tokens.Length <> 0 then
        eatTokens 0 Map.empty
    else
        Result.Ok Map.empty

let private nextList mapper (previous: Result<_,_>, items: _ list) =
    match previous with
    | Error err -> (Error err), items
    | Ok success -> 
        match items with
        | [] -> (Error "No element."), items
        | head :: tail -> 
            match mapper(head, tail, success) with
            | (Error err), remaining -> (Error err), remaining
            | (Ok success), remaining -> (Ok success), remaining

let private next mapper (previous: Result<_,_>, items: _ list) =
    match previous with
    | Error err -> (Error err), items
    | Ok success -> 
        match items with
        | [] -> (Error "No element."), items
        | head :: tail -> 
            match mapper(head, success) with
            | Error err -> (Error err), tail
            | Ok success -> (Ok success), tail

let private choose mapper (previous: Result<_,_>, items: _ list) =
    match previous with
    | Error err -> (Error err), items
    | Ok success -> 
        match items with
        | [] -> (Error "No element."), items
        | head :: tail -> 
            match mapper(head, success) with
            | Error err -> (Error err), tail
            | Ok opt ->
                match opt with
                | Some x -> (Ok x), tail
                | None -> previous, items
                
let private start (items: _ list) =
    Ok(), items

let private expect tagTokenType previous =
    previous
    |> next (fun (head, prev) -> 
        if TagTokenType.Of head = tagTokenType then
            Ok prev
        else
            Error <| sprintf "Expected type %A." tagTokenType
    )
    
[<RequireQualifiedAccess>]
type private TagStart =
    | Ended
    | Closed
    
let private consumeTagStart (tokens: TagToken list) =
    tokens 
    |> start
    |> expect OpenTagStartType
    |> nextList (fun (head, tail, _) -> 
        match head with
        | TagToken.OpenTag ->
            Ok (Map.empty, TagStart.Closed), tail
        | TagToken.OpenTagClose -> 
            Ok (Map.empty, TagStart.Closed), tail
        | TagToken.OpenTagEnd ->
            Ok (Map.empty, TagStart.Ended), tail
        | TagToken.Text text ->
            match ReadAttributes text with
            | Ok attributes ->
                tail
                |> start
                |> next (function
                    | (TagToken.OpenTagClose,_) -> 
                        Ok(attributes, TagStart.Closed)
                    | (TagToken.OpenTagEnd,_) -> 
                        Ok(attributes, TagStart.Ended)
                    | _ ->
                        Error "Expected close of tag start."
                )
            | Error err ->
                Error "Unable to parse attributes.", tail
        | _ ->
            Error "Expected OpenTagClose, OpenTagEnd, or Text.", tail
    )

let private consumeVariableTag (tokens: TagToken list) =
    tokens 
    |> start
    |> expect OpenVariableTagType
    |> next (fun (head, _) -> 
        match head with
        | TagToken.Text text ->
            Ok <| AST.VariableTag("@" + text)
        | _ ->
            Error "Expected text."
    )
    |> expect OpenTagCloseType

let private consumeText (tokens: TagToken list) =
    tokens
    |> start
    |> next(fun (head, _) ->
        match head with
        | TagToken.Text text -> 
            Ok <| AST.Contents text
        | TagToken.Whitespace ->
            Ok <| AST.Contents " "
        | _ ->
            Error "Expected text."
    )

let private consumeTagEnd (tokens: TagToken list) =
    tokens
    |> start
    |> expect TagTokenType.CloseTagType

type private StateStack = 
    | OpenTag of ASTAttributes
    | Element of AST
        
let rec private readAST (tokens: TagToken list) (state: StateStack list) =
    let tryTagStart() =
        match consumeTagStart tokens with
        | Ok(attributes, TagStart.Ended), remaining ->
            let tag = AST.Tag(attributes, [])
            readAST remaining ((Element tag) :: state)
        | Ok(attributes, TagStart.Closed), remaining ->
            let tag = OpenTag attributes
            readAST remaining (tag :: state)
        | (Error err), _ -> Error err

    let tryTagEnd() = 
        match consumeTagEnd tokens with
        | Ok(), remaining ->
            let rec buildContents tail items =
                match tail with
                | [] -> Error "Expected Something"
                | head :: tail -> 
                    match head with
                    | OpenTag attributes -> 
                        Ok(attributes, items, tail)
                    | Element item ->
                        buildContents tail (item :: items)
            match  buildContents state [] with
            | Ok (attributes, contents, state) -> 
                readAST remaining ((Element <| AST.Tag(attributes, contents)) :: state)
            | Error err ->
                Error err
        | Error err, _ -> Error err

    let tryConsumeText() =
        match consumeText tokens with
        | Ok text, remaining ->
            readAST remaining ((Element text) :: state)
        | Error err, _ -> Error err
    
    let tryConsumeVariable() =
        match consumeVariableTag tokens with
        | Ok ast, remaining ->
            readAST remaining ((Element ast) :: state)
        | Error _, _ ->
            Error "Expected something"

    if tokens.IsEmpty then Ok state
    else
        let tryFirst check2 check1 =
            match check1 with
            | Ok ok -> Ok ok
            | Error _ -> check2()

        tryTagStart()
        |> tryFirst tryTagEnd
        |> tryFirst tryConsumeText
        |> tryFirst tryConsumeVariable
   
let ReadTemplate text =
    if System.String.IsNullOrWhiteSpace text then Error "expected input"
    else

    let tokens = TokenizeTags text |> List.ofArray
    
    let result = readAST tokens []

    result 
    |> Result.map (
        List.choose (function | OpenTag _ -> None | Element ast -> Some ast) 
        >> function 
           | [] -> failwith "Never expected empty result"
           | x :: [] -> x
           | items -> AST.Tag(Map.empty, List.rev items)
    )