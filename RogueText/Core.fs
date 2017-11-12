namespace RogueText
    
type TagText =
    | Variable of string
    | Text of string

type TaggedAttributes = Map<string, TagText option>

type TaggedText =
    | StartTag of TaggedAttributes
    | EndTag
    | Text of string
    | VariableTag of string