namespace RogueText.Core


type [<RequireQualifiedAccess>] Types =
    | Number
    | Boolean
    | String 
    | List of Types
    | Function of Arguments: Types List * Result: Types
    | Element

and [<RequireQualifiedAccess>] Values =
    | Variable of Name: string
    | Number of decimal
    | Boolean of bool
    | String of string
    | List of Values list
    | Element of Element
    | FunctionCall of FunctionCall

and FunctionCall =
    {   Name: string
        Module: string option
        Parameters: Values list
    }
    
and [<RequireQualifiedAccess>] WordFragment =
    | RawText of string
    | FunctionCall of FunctionCall

and Element = 
    {   Fragments: SentenceTree list
        Attributes: Attributes
        Parameters: Values list
        Name: string
    }

and [<RequireQualifiedAccess>] SentenceTree =
    | Text of string
    | Word of WordFragment
    | Element of Element

and Attributes = Map<string, Values option>

[<RequireQualifiedAccess>]
type AccessModifier =
    | Public
    | Private
    
type FunctionDeclaration =
    {   SentenceTree: SentenceTree
        Name: string
        AccessModifier: AccessModifier
    }

type VariableDeclaration =
    {   Name: string
        Value: Values
    }
