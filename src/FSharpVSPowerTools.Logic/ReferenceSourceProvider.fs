namespace FSharpVSPowerTools.Navigation

open System
open System.IO
open System.Diagnostics
open System.Net.Http
open System.Net.NetworkInformation
open System.Windows.Threading
open System.Security.Cryptography
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.ComponentModel.Composition

// Reference https://github.com/SLaks/Ref12/blob/master/Ref12/Services/ReferenceSourceProvider.cs

type ReferenceSourceProvider(baseUrl: string) =
    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                Interval = TimeSpan.FromMinutes(60.0))

    let mutable availableAssemblies = Set.empty

    let lookUpAvailableAssemblies() =
        async {
            use handler = new HttpClientHandler(UseDefaultCredentials = true)
            use http = new HttpClient(handler)
            let! assemblyList = http.GetStringAsync(baseUrl + "/assemblies.txt") |> Async.AwaitTask
            let assemblies = 
                assemblyList.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> s.Remove(s.IndexOf(';')))
                |> Set.ofArray
            return availableAssemblies <- assemblies
        }
        |> Async.StartImmediateSafe

    let networkAvailabilityChanged (arg: NetworkAvailabilityEventArgs) = 
        if arg.IsAvailable then
            lookUpAvailableAssemblies()

    let lookUpSubscription = timer.Tick.Subscribe(fun _ -> lookUpAvailableAssemblies())
   
    let networkAvailabilitySubscription = NetworkChange.NetworkAvailabilityChanged.Subscribe(networkAvailabilityChanged)
    let networkAddressSubscription = NetworkChange.NetworkAddressChanged.Subscribe(fun _ -> lookUpAvailableAssemblies())

    let byteArrayToHexString (bytes: byte []) digits =
        let chars = Array.zeroCreate digits
        for i in 0..digits/2-1 do
            let b1 = byte (bytes.[i] >>> 4)
            chars.[i * 2] <- if b1 > 9uy then char (b1 + 87uy) else char (b1 + 0x30uy)
            let b2 = byte (bytes.[i] &&& 0xFuy)
            chars.[i * 2 + 1] <- if b2 > 9uy then char (b2 + 87uy) else char (b2 + 0x30uy)
        String(chars)

    let getMD5Hash (input: string) digits =
        use md5 = MD5.Create()
        let bytes = Encoding.UTF8.GetBytes(input)
        let hashBytes = md5.ComputeHash(bytes)
        byteArrayToHexString hashBytes digits

    member __.IsActivated =
        timer.IsEnabled

    member __.Activate() =
        timer.Start()
        lookUpAvailableAssemblies()

    member __.AvailableAssemblies = availableAssemblies

    member __.NavigateTo(symbol: FSharpSymbol) = 
        let xmlDocSig =
            match symbol with
            | MemberFunctionOrValue mem ->
                Some mem.XmlDocSig
            | TypedAstPatterns.Entity(entity, _, _) ->
                Some entity.XmlDocSig
            | _ -> None
        xmlDocSig
        |> Option.iter (fun xmlDocSig ->
            symbol.Assembly.FileName
            |> Option.map Path.GetFileNameWithoutExtension
            |> Option.iter (fun assemblyName ->
                let url = baseUrl + "/" + assemblyName + "/a.html#" + getMD5Hash xmlDocSig 16
                Process.Start url |> ignore))
    
    interface IDisposable with
        member __.Dispose() =
            lookUpSubscription.Dispose()
            networkAvailabilitySubscription.Dispose()
            networkAddressSubscription.Dispose()
            timer.Stop()
