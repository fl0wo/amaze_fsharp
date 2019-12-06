open System

#load "./../models/models.fsx"
#load "./../view/view.fsx"
#load "./../controls/controller.fsx"
#load "./../utils/utils.fsx"

#load "./../server/client.fsx"
#load "./../parsers/fast_json_parser.fsx"

open Client
open Models
open View
open Controller
open Fast_json_parser
open Utils


type SingletonConnection private () =
    static let instance = SingletonConnection()

    static member Instance = instance

    member val drawCallback =  (fun (x:Maze) -> ()) with get,set

    member val client = new Client.Client() with get, set

    member val currentLobby: string = "flo"

    member this.my_substr (cI: string) (cF: string) (str: string) =
        let firstI: int = str.IndexOf(cI)
        let neededL: int = str.LastIndexOf(cF) - firstI

        str.Substring(firstI + 1, neededL)


    member this.my_substr2 (cI: string) (cF: string) (str: string) =
        let firstI: int = str.IndexOf(cI)
        let neededL: int = str.IndexOf(cF) - firstI

        str.Substring(firstI + 1, neededL)

    member this.my_substr3 (cI: string) (str: string) =
        let firstI: int = str.IndexOf(cI)
        let neededL: int =  (str.Length-1)-firstI

        str.Substring(firstI + 1, neededL)

    member this.applyLobbyUpdates (s: string) = 

        let v:Maze = decode s // val v : geo = {t = "Point"; coordinates = [|-7.002648; 110.449961|];}

        (this.drawCallback v)

    member this.callback =
        fun (msg: string) ->
            let mutable parsedMsg = msg

            parsedMsg <- (this.my_substr "[" "]" parsedMsg)

            let mutable lobbyN = (this.my_substr2 "(" "}]})" parsedMsg)

            while not (String.IsNullOrEmpty lobbyN) do

                if (lobbyN.Contains("lobbyofflo")) then
                    // im interested in this one
                    let lobbyIJoined = (this.my_substr3 "," lobbyN).Replace(" ","");

                    this.applyLobbyUpdates (lobbyIJoined + "]}")

                    lobbyN <- "";
                else
                    parsedMsg <- (this.my_substr3 "}]})" parsedMsg)
                    lobbyN <- (this.my_substr2 "(lobbyof" "}]})" parsedMsg)


    member this.init = this.client.ConnectTo("127.0.0.1", 8081, this.callback)

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
    fun ((x: int),(y: int)) ->
        //comands <- comands @ [ "MO " + nome + " " + x.ToString() + " " + y.ToString() ]
        singletonConnection.sendCmd ("MO " + nome + " " + x.ToString() + " " + y.ToString())

let user: Player = Player(startX, startY)
user.lambdaMove <- lambdaMove
let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)

singletonConnection.drawCallback <- 
    fun (maze:Maze) ->
        let mappa_bin:array<array<int>> = (Utils.fromStringToMatrix maze.m maze.r maze.c);
        
        UtilsView.printMap (mappa.getIstanceWith([|
            for part in maze.partecipants do
                yield Player(part.x,part.y)
        |], mappa_bin ,(endY, endX),true)) user (endY, endX)


singletonConnection.sendCmd ("LO " + nome + " " + startY.ToString() + " " + startX.ToString() + " " + "0")
singletonConnection.sendCmd ("CR " + nome + " " + r.ToString() + " " + c.ToString() + " " + mappa.ToString())
//singletonConnection.sendCmd ("MO " + nome + " 12 12")

// comands <- comands @ [ ("LO " + nome + " " + startY.ToString() + " " + startX.ToString() + " " + "0") ]
// comands <- comands @ [ "CR " + nome + " " + r.ToString() + " " + c.ToString() + " " + mappa.ToString() ]
// comands <- comands @ [ "MO " + nome + " 12 12" ]

let contr: Controller = Controller(mappa, user, (endX, endY))

contr.reactiveKey()

UtilsView.printMap (mappa.getIstanceWith([|user|],(endY, endX),true)) user (endY, endX)
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
