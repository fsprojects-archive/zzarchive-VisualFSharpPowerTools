namespace FSharpVSPowerTools.Navigation

open System
open System.IO
open System.Net.Http
open System.Net.NetworkInformation
open System.Windows.Threading
open System.Security.Cryptography
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

// Reference https://github.com/SLaks/Ref12/blob/master/Ref12/Services/ReferenceSourceProvider.cs

type ReferenceSourceProvider(baseUrl: string) =
    let timer = DispatcherTimer (DispatcherPriority.ApplicationIdle, Interval = TimeSpan.FromMinutes 60.)
    let mutable availableAssemblies = Set.empty

    let lookUpAvailableAssemblies() =
        async {
            use handler = new HttpClientHandler(UseDefaultCredentials = true)
            use http = new HttpClient(handler)
            let! assemblyList = http.GetStringAsync(baseUrl + "/assemblies.txt") |> Async.AwaitTask
            let assemblies = 
                assemblyList 
                |> String.getNonEmptyLines
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

    let getMD5Hash (input: string) =
        use md5 = MD5.Create()
        let bytes = Encoding.UTF8.GetBytes(input)
        let hashBytes = md5.ComputeHash(bytes)
        Array.toHexString hashBytes

    member __.IsActivated =
        timer.IsEnabled

    member __.Activate() =
        timer.Start()
        lookUpAvailableAssemblies()

    member __.AvailableAssemblies = availableAssemblies

    member __.TryGetNavigatedUrl(symbol: FSharpSymbol) = 
        let xmlDocSig =
            match symbol with
            | MemberFunctionOrValue mem ->
                Some mem.XmlDocSig
            | TypedAstPatterns.Entity(entity, _, _) ->
                Some entity.XmlDocSig
            | _ -> None
        xmlDocSig
        |> Option.bind (fun xmlDocSig ->
            symbol.Assembly.FileName
            |> Option.map Path.GetFileNameWithoutExtension
            |> Option.map (fun assemblyName ->
                let url = baseUrl + "/" + assemblyName + "/a.html#" + getMD5Hash xmlDocSig
                Logging.logInfo (fun _ -> sprintf "Go to definition at '%s'." url)
                url))
        
    interface IDisposable with
        member __.Dispose() =
            lookUpSubscription.Dispose()
            networkAvailabilitySubscription.Dispose()
            networkAddressSubscription.Dispose()
            timer.Stop()
