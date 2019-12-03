open System


let client = new System.Net.Sockets.TcpClient()

let rec asyncPrintResponse (stream: System.Net.Sockets.NetworkStream) =
    async {

        let bytes: array<byte> =
            [| for x in 0 .. (client.ReceiveBufferSize) do
                yield (byte 0) |]

        // Read can return anything from 0 to numBytesToRead.
        // This method blocks until at least one byte is read.
        stream.Read(bytes, 0, (int) client.ReceiveBufferSize) |> ignore

        // Returns the data received from the host to the console.
        let returndata: string = System.Text.Encoding.UTF8.GetString(bytes)

        Console.WriteLine("This is what the host returned to you: " + returndata)

        return! asyncPrintResponse stream
    }

let TcpClientTest() =
    client.Connect("localhost", 5000)

    let stream = client.GetStream()
    let bytes =
        System.Text.Encoding.UTF8.GetBytes("{ \"id\": 0, \"method\": \"user/login\", \"params\": \"floffly \" }\n")

    asyncPrintResponse stream |> Async.Start
    stream.Write(bytes, 0, bytes.Length)

    stream.Flush()
    System.Threading.Thread.Sleep(TimeSpan.FromSeconds(60.0))


TcpClientTest()
