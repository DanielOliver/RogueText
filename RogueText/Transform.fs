module RogueText.Transform

open RogueText

let private normalizeVariableName (name: string) =
    if name.StartsWith("@") then name.Substring(1)
    else name

let GetVariables (ast: AST) =
    let getVariablesFromAttributes (attributes: ASTAttributes) =
        attributes
        |> Seq.filter(fun value -> match value.Value with | Some(x) -> x.StartsWith("@") | None -> false)
        |> Seq.map (fun value -> value.Value.Value |> normalizeVariableName) //Assume value exists because of above line.

    let rec getTags branch =
        match branch with
        | Contents _ -> Seq.empty
        | Tag(attributes, tags) ->
            let variables = getVariablesFromAttributes attributes
            variables 
            |> Seq.append (tags |> Seq.map getTags |> Seq.concat) 
            |> Seq.distinct
        | AST.VariableTag text ->
            text 
            |> normalizeVariableName 
            |> Seq.singleton
    
    ast |> getTags |> Seq.toList    

let private ASTtoTaggedAttributes (attributes: ASTAttributes) =
    attributes
    |> Map.map(
        fun _ value -> 
        match value with 
        | Some(x) -> 
            if x.StartsWith("@") then
                x |> normalizeVariableName |> TagText.Variable |> Some
            else
                x |> TagText.Text |> Some
        | None -> 
            None)

let FlattenAST (ast: AST) =
    let rec createTaggedText current branch =
        match branch with
        | Contents text -> (TaggedText.Text text) :: current
        | Tag(attributes, tags) ->
            let attributes = ASTtoTaggedAttributes attributes
            let startTag = StartTag attributes
            let next = tags |> List.fold createTaggedText (startTag :: current)
            EndTag :: next
        | AST.VariableTag text ->
            let next = text |> normalizeVariableName |> TaggedText.VariableTag
            next :: current
    List.rev( createTaggedText [] ast)


