namespace FSharp.Editing.Server

open FSharp.Editing.Messages

module MessageRouter =
    let start() =
        let agent = MailboxProcessor.Start <| fun mb ->
            let rec loop() =
                async {
                    let! message = mb.Receive()
                    match message with
                    | ClientMessage.Notification _x ->
                        //dispatchClientNotification x
                        return! loop()
                    | ClientMessage.Request _x -> 
                        //dispatchClientRequest x
                        return! loop()

                }
            loop()

        agent.Error.Add <| fun e -> Logger.error "%O" e
        agent.Post