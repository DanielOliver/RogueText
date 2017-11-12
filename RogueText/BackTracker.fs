module RogueText.BackTracker

open RogueText
open RogueText.Tokenizer
    
let private getNext position (items: _ array) =
    if position >= 0 && position < items.Length then Some items.[position]
    else None
    
let private normalizeVariableName (name: string) =
    (if name.StartsWith("@") then name.Substring(1)
     else name).Trim()

let private getTagText (text: string) =
    if text.StartsWith("@") then
        text |> normalizeVariableName |> TagText.Variable
    else
        text.Trim() |> TagText.Text
    
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
    
let private map mapping (result, tail) =
    (result |> Result.map mapping, tail)

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
                (Result.Ok(identifier, Some <| getTagText value)), 3
        | Some(AttributeToken.Identifier(_)) 
        | None -> 
            (Result.Ok(identifier, None)), 1
    | AttributeToken.Equals -> 
        (Result.Error "Unexpected equals."),0

let ReadAttributes (text: string) =
    let tokens = TokenizeAttributes text

    let rec eatTokens nextPosition (attributes: TaggedAttributes) =
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
        
let private consumeTagStart (tokens: TagToken list) =
    tokens 
    |> start
    |> expect OpenTagStartType
    |> nextList (fun (head, tail, _) -> 
        match head with
        | TagToken.Text text ->
            match ReadAttributes text with
            | Ok attributes ->
                let startTag = TaggedText.StartTag attributes
                tail
                |> start
                |> next (function
                    | (TagToken.OpenTagClose,_) -> 
                        Ok(List.singleton startTag)
                    | (TagToken.OpenTagEnd,_) -> 
                        let endTag = TaggedText.EndTag
                        Ok([ endTag; startTag ])
                    | _ ->
                        Error "Expected close of tag start."
                )
            | Error err ->
                Error "Unable to parse attributes.", tail
        | _ ->
            Error "Expected attributes: invalid empty start tag.", tail
    )
    
let private consumeVariableTag (tokens: TagToken list) =
    tokens 
    |> start
    |> expect OpenVariableTagType
    |> next (fun (head, _) -> 
        match head with
        | TagToken.Text text ->
            text
            |> normalizeVariableName
            |> TaggedText.VariableTag
            |> Ok
        | _ ->
            Error "Expected variable identifier."
    )
    |> expect OpenTagCloseType
    |> map List.singleton
    
let private consumeText (tokens: TagToken list) =
    tokens
    |> start
    |> next(fun (head, _) ->
        match head with
        | TagToken.Text text -> 
            text
            |> TaggedText.Text
            |> Ok
        | TagToken.Whitespace ->
            " "
            |> TaggedText.Text
            |> Ok
        | _ ->
            Error "Expected text."
    )
    |> map List.singleton

let private consumeTagEnd (tokens: TagToken list) =
    tokens
    |> start
    |> expect TagTokenType.CloseTagType
    |> map (fun () -> List.singleton TaggedText.EndTag)
    
let rec private readAST (tokens: TagToken list) (state: TaggedText list) =
    if tokens.IsEmpty then Ok state, tokens
    else
        let tryFirst check check1 =
            match check1 with
            | (Ok ok), remaining -> (Ok ok), remaining
            | Error _, _ -> check tokens

        let (consumption, remaining) = 
            consumeTagStart tokens
            |> tryFirst consumeTagEnd
            |> tryFirst consumeText
            |> tryFirst consumeVariableTag
            
        match consumption with
        | Ok addedState ->
            readAST remaining (List.append addedState state)
        | Error _ ->
            consumption, remaining
               
let ReadTemplate text =
    if System.String.IsNullOrWhiteSpace text then Error "expected input"
    else

    let tokens = TokenizeTags text |> List.ofArray
    
    let (result, _) = readAST tokens []

    result 
    |> Result.map List.rev
    