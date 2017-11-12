namespace RogueText

type ASTAttributes = Map<string, string option>

type AST =
    | Contents of string
    | Tag of ASTAttributes * AST list
    | VariableTag of string
    
type TagText =
    | Variable of string
    | Text of string

type TaggedAttributes = Map<string, TagText option>

type TaggedText =
    | StartTag of TaggedAttributes
    | EndTag
    | Text of string
    | VariableTag of string