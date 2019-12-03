module Client =

    open System.Net
    open System.Net.Sockets
    open System.IO
    open System.Threading.Tasks

    let awaitTaskVoid = Async.AwaitIAsyncResult >> Async.Ignore

    type Client() =

        let tcpClient = new TcpClient() // tcpClient is private since this is an internal let binding

        member private this.listenAsync (callback) =
            async {
                let stream = tcpClient.GetStream()
                use streamReader = new StreamReader(stream)

                while true do
                    let! line = streamReader.ReadLineAsync() |> Async.AwaitTask
                    streamReader.DiscardBufferedData()
                    callback line
            }

        member this.ConnectTo(host, port, callback) =
            async {
                do! awaitTaskVoid (tcpClient.ConnectAsync(host = host, port = port))
                this.listenAsync (callback)
                |> Async.StartAsTask
                |> ignore
            }
            |> Async.StartAsTask

        member this.Write msg =
            match tcpClient.Connected with
            | true ->
                async {
                    let streamWriter = new StreamWriter(tcpClient.GetStream()) //use would close the baseStream --> dispose the stream elsewhere
                    do! awaitTaskVoid (streamWriter.WriteLineAsync(msg.ToString()))
                    do! awaitTaskVoid (streamWriter.FlushAsync())
                }
                |> Async.StartAsTask
            | _ -> failwith "client not connected"

        interface System.IDisposable with
            member this.Dispose() = tcpClient.Close()

let main =

    let data_parser msg = printfn "-> %A" msg
    let callback = fun msg -> printfn "-> %A" msg

    let c = new Client.Client()
    let t = c.ConnectTo("127.0.0.1", 8081, data_parser)
    while true do
        printf "Msg: "
        let msg = System.Console.ReadLine()
        c.Write(msg) |> ignore

main
