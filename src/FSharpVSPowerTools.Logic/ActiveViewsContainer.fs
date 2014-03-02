namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Collections.Generic
open System.ComponentModel.Composition

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor

[<Export(typeof<ActiveViewsContainer>)>]
type ActiveViewsContainer() =

    let gate = obj()
    let activeViews = HashSet(HashIdentity.Reference)

    member x.RegisterActiveView(view: IWpfTextView) = 
        let rec disposable: IDisposable = view.Closed.Subscribe(removeView)
        and removeView _ = 
            lock gate (fun () -> activeViews.Remove(view) |> ignore)
            disposable.Dispose()

        lock gate (fun() -> activeViews.Add(view) |> ignore)
    
    member x.MapOpenViews f = 
        lock gate <| fun() ->
            [
                for view in activeViews do 
                    if not view.IsClosed then 
                        match f view with
                        | Some v -> yield v
                        | None -> ()
            ]
