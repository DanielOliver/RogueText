namespace RogueText.Core

[<RequireQualifiedAccess>]
type Types =
    | Number of decimal
    | Boolean of bool
    | String of string
    | Array of Types array
    | None
    | Function of Arguments: Types array * Result: Types
    
[<RequireQualifiedAccess>]
type AccessModifier =
    | Public
    | Private

type Attributes = Map<string, Types>

type Variable =
    {   Name: string
    }

type FunctionCall =
    {   Name: string
        Arguments: Map<string, Argument>
    }
    
and [<RequireQualifiedAccess>] Argument =
    | FunctionCall of FunctionCall
    | Variable of Variable

and WordFragment =
    {   Word: string
        Attributes: Attributes
    }

and Element = 
    {   Fragments: SentenceTree list
        Attributes: Attributes
        Name: string
    }

and SentenceTree =
    | Text of string
    | Word of WordFragment
    | Element of Element

type SentenceFunction =
    {   Name: string
        Arguments: Types array
        Sentence: SentenceTree
        AccessModifier: AccessModifier
    }
