namespace FSharp.Editing.Server

open FSharp.Editing.Messages

module MessageRouter =
    let start() =
        let agent = MailboxProcessor.Start <| fun mb ->
            let rec loop ctx =
                async {
                    let! message = mb.Receive()
                    match ctx with
                    | Some ctx ->
                        match message with
                        | ClientMessage.Notification _x ->
                            //match x with
                            //| ClientNotification.ProjectLoaded project ->
                              //  return! loop (Some { ctx with Solution = ctx.Solution |> Solution.addOrUpdateProject project })
                            //dispatchClientNotification x
                            return! loop (Some ctx)
                        | ClientMessage.Request _x -> 
                            //dispatchClientRequest x
                            return! loop (Some ctx)
                    | None -> failwith "No solution opened."

                }
            loop None

        agent.Error.Add <| fun e -> Logger.error "%O" e
        agent.Post