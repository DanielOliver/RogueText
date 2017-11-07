module RogueText.BackTracker

open RogueText.Tokenizer

type Attributes = Map<string, string option>

type AST =
    | Contents of string
    | Tag of Attributes * AST list

type private InternalState = 
    | ReadingAttributes
    | ReadingContents
    | StartTag
    | EndTag
    
let private getNext2 position (items: _ array) =
    if position >= 0 && position < items.Length then Some items.[position]
    else None
    
let private consumeAttribute (position: int) (tokens: AttributeToken array) =
    match tokens.[position] with
    | AttributeToken.Identifier(identifier) -> 
        match tokens |> getNext2 (position + 1) with
        | Some AttributeToken.Equals ->
            match tokens |> getNext2(position + 2) with
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
        




let nextList mapper (previous: Result<_,_>, items: _ list) =
    match previous with
    | Error err -> (Error err), items
    | Ok success -> 
        match items with
        | [] -> (Error "No element."), items
        | head :: tail -> 
            match mapper(head, tail, success) with
            | (Error err), remaining -> (Error err), remaining
            | (Ok success), remaining -> (Ok success), remaining

let next mapper (previous: Result<_,_>, items: _ list) =
    match previous with
    | Error err -> (Error err), items
    | Ok success -> 
        match items with
        | [] -> (Error "No element."), items
        | head :: tail -> 
            match mapper(head, success) with
            | Error err -> (Error err), tail
            | Ok success -> (Ok success), tail

let choose mapper (previous: Result<_,_>, items: _ list) =
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
                
let start (items: _ list) =
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
type TagStart =
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


type StateStack = 
    | OpenTag of Attributes
    | Element of AST

let rec private readAST (tokens: TagToken list) (state: StateStack list) =
    if tokens.IsEmpty then Ok state
    else
        match consumeTagStart tokens with
        | Ok(attributes, TagStart.Ended), remaining ->
            let tag = AST.Tag(attributes, [])
            readAST remaining ((Element tag) :: state)

        | Ok(attributes, TagStart.Closed), remaining ->
            let tag = OpenTag attributes
            readAST remaining (tag :: state)

        | (Error _), _ ->
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

            | Error _, _ ->
                match consumeText tokens with
                | Ok text, remaining ->
                    readAST remaining ((Element text) :: state)

                | Error _, _ ->
                    Error "Expected something"

let ReadTemplate text =
    let tokens = TokenizeTags text |> List.ofArray
    
    let result = readAST tokens []

    result |> Result.map List.rev


