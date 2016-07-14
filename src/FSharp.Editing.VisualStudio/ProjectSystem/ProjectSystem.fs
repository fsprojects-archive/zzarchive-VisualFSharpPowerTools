namespace FSharp.Editing.VisualStudio.ProjectSystem

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open FSharp.Editing
open FSharp.Editing.VisualStudio

open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop

type Expr = System.Linq.Expressions.Expression
exception AssemblyMissingException of string

[<Export>]
type FSharpLanguageService [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) = 
    let dte = serviceProvider.GetDte()

    let assemblyInfo = 
        let version = VisualStudioVersion.fromDTEVersion dte.Version
        String.Format("FSharp.LanguageService, Version={0}.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a", 
                        VisualStudioVersion.toString version)
                              
    let asm = lazy try Assembly.Load(assemblyInfo)
                   with _ ->
                       let ex = AssemblyMissingException "FSharp.LanguageService"
                       Logging.logException ex
                       raise ex

    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

    let getColorStateAtStartOfLine = lazy (
        let ty = asm.Value.GetType("Microsoft.VisualStudio.FSharp.LanguageService.VsTextColorState")
        let m = ty.GetMethod ("GetColorStateAtStartOfLine", staticFlags)
        let stateParam = Expr.Parameter typeof<IVsTextColorState>
        let lineParam = Expr.Parameter typeof<int>
        let lambda = Expr.Lambda<Func<IVsTextColorState, int, int>>(Expr.Call(m, stateParam, lineParam), stateParam, lineParam)
        lambda.Compile().Invoke
    )
    
    let lexStateOfColorState = lazy (
        let ty = asm.Value.GetType("Microsoft.VisualStudio.FSharp.LanguageService.ColorStateLookup")
        let m = ty.GetMethod ("LexStateOfColorState", staticFlags)
        let lineParam = Expr.Parameter typeof<int>
        let lambda = Expr.Lambda<Func<int, int64>>(Expr.Call(m, lineParam), lineParam)
        lambda.Compile().Invoke
    )

    member __.GetColorStateAtStartOfLine(vsColorState: IVsTextColorState, line: int): int =
        getColorStateAtStartOfLine.Value(vsColorState, line)

    member __.LexStateOfColorState(colorState: int): int64 = lexStateOfColorState.Value colorState