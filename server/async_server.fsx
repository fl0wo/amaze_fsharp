#load "./serber.fsx"

type User(nome, x, y, colore) =
    member this.nome = nome
    member this.x = x
    member this.y = y
    member this.colore = colore

let mutable users: Map<string, User> = Map.empty

module CmdParser =
    let parser endpoint msg = printfn "<- %s %A" endpoint msg

let main =
    let t = [ 1 .. 5 ] |> AsyncSerber.removeFirst (fun item -> item = 6)

    let listenTask = (AsyncSerber.listen 8081 CmdParser.parser)

    System.Console.ReadLine() |> ignore

main
