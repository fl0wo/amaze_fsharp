open System

#load "./../models/models.fsx"
#load "./../view/view.fsx"
#load "./../controls/controller.fsx"

#load "./../server/client.fsx"

open Client
open Models
open View
open Controller


type SingletonConnection private () =
    static let instance = SingletonConnection()
    static member Instance = instance

    member val client = new Client.Client() with get, set

    member this.init = this.client.ConnectTo("127.0.0.1", 8081, (fun msg -> printfn "-> %A" msg))

    member this.sendCmd cmd = this.client.Write(cmd) |> ignore

let singletonConnection = SingletonConnection.Instance

singletonConnection.init

let askInput title =
    printfn "Inserire %s : \n" title
    Console.ReadLine()

let nome: string = askInput "nome"
let r = askInput "righe (numero intero) "
let c = askInput "colonne (numero intero) "
let mappa: Mappa = Mappa((r |> int), (c |> int))

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn

//let mutable comands: string list = []

let lambdaMove =
    fun (x: int) (y: int) ->
        //comands <- comands @ [ "MO " + nome + " " + x.ToString() + " " + y.ToString() ]
        singletonConnection.sendCmd ("MO " + nome + " " + x.ToString() + " " + y.ToString())

let user: Player = Player(startX, startY, lambdaMove)
let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)


singletonConnection.sendCmd ("LO " + nome + " " + startY.ToString() + " " + startX.ToString() + " " + "0")
singletonConnection.sendCmd ("CR " + nome + " " + r.ToString() + " " + c.ToString() + " " + mappa.ToString())
singletonConnection.sendCmd ("MO " + nome + " 12 12")

// comands <- comands @ [ ("LO " + nome + " " + startY.ToString() + " " + startX.ToString() + " " + "0") ]
// comands <- comands @ [ "CR " + nome + " " + r.ToString() + " " + c.ToString() + " " + mappa.ToString() ]
// comands <- comands @ [ "MO " + nome + " 12 12" ]

let contr: Controller = Controller(mappa, user, (endX, endY))

contr.reactiveKey()
// UtilsView.printMap (mappa.getIstanceWith user (endY, endX) true) user (endY, endX)
// let execComands =
//     while true do
//         while not (List.isEmpty comands) do
//             match comands with
//             | [] -> ()
//             | t :: c ->
//                 (singletonConnection.sendCmd t)
//                 (comands <- c)
// execComands

Threading.Thread.Sleep(-1)
