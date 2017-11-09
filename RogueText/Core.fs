namespace RogueText

type Attributes = Map<string, string option>

type AST =
    | Contents of string
    | Tag of Attributes * AST list
    | VariableTag of string
