namespace RogueText.Core


type Types =
    | Number of decimal
    | Boolean of bool
    | String of string
    | Array of Types array
    | None

type Attributes = Map<string, Types>

and WordFragment =
    {   Word: string
        Attributes: Attributes
    }

and Element = 
    {   Fragments: Tree list
        Attributes: Attributes
        Name: string
    }

and Tree =
    | Text of string
    | Word of WordFragment
    | Element of Element
