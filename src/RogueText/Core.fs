namespace RogueText.Core


type Types =
    | Number of decimal
    | Boolean of bool
    | String of string
    | Array of Types array

type Attributes = Types Set

and WordFragment =
    {   Word: string
        Attributes: Attributes
    }

and Component = 
    {   Fragments: Tree list
        Attributes: Attributes
    }

and Tree =
    | Text of string
    | Word of WordFragment
    | Branch of Component
