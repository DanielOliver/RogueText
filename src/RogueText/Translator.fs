module RogueText.Translator

open RogueText.Core

let rec private createTypeText (argType: Types) =
    match argType with
    | Types.Array subType -> sprintf "%s array" (createTypeText subType)
    | Types.Option subType -> sprintf "%s option" (createTypeText subType)
    | Types.Boolean -> "bool"
    | Types.String -> "string"
    | Types.Number -> "decimal"
    | Types.Function _ 
    | Types.None _ -> failwithf "createTypeText is not defined for %A." argType

let private createFunctionArgument (argument: FunctionArgument) =
    sprintf "%s: %s" argument.Name (argument.Type |> createTypeText)

let private createFunctionDefinition isFirstDefinition (sentence: SentenceFunction) =
    let arguments = System.String.Join(",", sentence.Arguments |> Seq.sortBy(fun t -> t.Name) |> Seq.map createFunctionArgument)
    let modifierText = sentence.AccessModifier.ToString().ToLower()
    let assignment = if isFirstDefinition then "let rec" else "and"
    printfn "%s %s ``%s`` (%s) =" assignment modifierText sentence.Name arguments

let Translate (tree: SentenceFunction list) =
    tree 
    |> List.tryHead 
    |> Option.iter (fun first ->
        first |> createFunctionDefinition true
        tree |> List.tail |> List.iter (createFunctionDefinition false)
    )
    


