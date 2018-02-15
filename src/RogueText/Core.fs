namespace RogueText.Core


type FunctionArgument =
    {   Name: string
        Type: Types
    }

and [<RequireQualifiedAccess>] Types =
    | Number
    | Boolean
    | String 
    | Array of Types
    | None
    | Function of Arguments: FunctionArgument array * Result: Types
    

and [<RequireQualifiedAccess>] Values =
    | Number of decimal
    | Boolean of bool
    | String of string
    | Array of Values
    | None    

[<RequireQualifiedAccess>]
type AccessModifier =
    | Public
    | Private

type Attributes = Map<string, Values>

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
        Arguments: FunctionArgument array
        Sentence: SentenceTree
        AccessModifier: AccessModifier
    }
