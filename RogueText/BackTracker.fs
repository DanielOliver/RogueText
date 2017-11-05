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


let ReadAttributes (text: string) =
    let tokens = TokenizeAttributes text

    let getNext position =
        if position >= 0 && position < tokens.Length then Some tokens.[position]
        else None

    let readAttribute (position: int) =
        match tokens.[position] with
        | AttributeToken.Identifier(identifier) -> 
            match getNext (position + 1) with
            | Some AttributeToken.Equals ->
                match getNext(position + 2) with
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
    
    let rec eatTokens nextPosition attributes =
        let next = readAttribute nextPosition
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


//let ReadTemplate (tokens: TagToken list)


