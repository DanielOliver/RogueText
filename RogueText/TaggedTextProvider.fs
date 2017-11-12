namespace RogueText 


open System
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection


module private ProviderTests =
    open Microsoft.FSharp.Quotations
    
    let toProperty text =
        let invokeCode =
            Expr.Value text
        let invokeType = invokeCode.Type
        ProvidedProperty("show", invokeType, isStatic = true, getterCode = fun _ -> invokeCode)

// --------------------------------------------------------------------------------------

[<TypeProvider>]
type public TaggedTextProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)
    
    let ns = "RogueText"
    let asm = Assembly.GetExecutingAssembly()

    let tPrintProvider = ProvidedTypeDefinition(asm, ns, "Print", Some(typeof<obj>))

    let parameters = [ProvidedStaticParameter("FormatString", typeof<string>)]

    do tPrintProvider.DefineStaticParameters(parameters, fun typeName args ->
        let formatString = args.[0] :?> string

        let provider = ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        provider.HideObjectMethods <- true
        formatString |> ProviderTests.toProperty |> provider.AddMember

        provider
        )
    
    do
        this.AddNamespace(ns, [tPrintProvider])

[<assembly:TypeProviderAssembly>]
do ()