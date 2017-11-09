module RogueText.Transform

open RogueText

let private variableTags =
    [|
        "pluralIf", "number"
        "number", "number"
        "text", "string"
    |] |> Map.ofArray


let private normalizeVariableName (name: string) =
    if name.StartsWith("@") then name.Substring(1)
    else name

let GetVariables (ast: AST) =
    let getVariablesFromAttributes (attributes: Attributes) =
        attributes
        |> Map.filter(fun key value -> match value with | Some(x) -> x.StartsWith("@") | None -> false)
        |> Map.toSeq
        |> Seq.map (fun (_, value) -> value.Value |> normalizeVariableName) //Assume value exists because of above line.

    let rec getTags branch =
        match branch with
        | Contents _ -> Seq.empty
        | Tag(attributes, tags) ->
            let variables = getVariablesFromAttributes attributes
            variables 
            |> Seq.append (tags |> Seq.map getTags |> Seq.concat) 
            |> Seq.distinct
        | VariableTag text ->
            text 
            |> normalizeVariableName 
            |> Seq.singleton
    
    ast |> getTags |> Seq.toList


