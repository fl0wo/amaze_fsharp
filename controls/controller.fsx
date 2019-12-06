open System
open System.IO

let rand = System.Random()

#load "./../models/models.fsx"
#load "./../view/view.fsx"

open Models
open View

type Controller(mappa: Mappa, utente: Player, finish: int * int) =

    member this.labirinto: Mappa = mappa
    member this.utente: Player = utente

    member this.finish: int * int = finish

    member this.onKey (k: string): bool =
        match k with
        | "LeftArrow"
        | "A" -> (this.labirinto.canGo (this.utente.x - 1) (this.utente.y)) && this.utente.goLeft
        | "RightArrow"
        | "D" -> this.labirinto.canGo (this.utente.x + 1) (this.utente.y) && this.utente.goRight
        | "DownArrow"
        | "S" -> this.labirinto.canGo this.utente.x (this.utente.y + 1) && this.utente.goDown
        | "UpArrow"
        | "W" -> this.labirinto.canGo this.utente.x (this.utente.y - 1) && this.utente.goUp
        | "Spacebar" -> (false)
        | _ -> (false)

    member this.reactiveKey() =
        async {
            let! key = Async.FromContinuations(fun (cont, _, _) ->
                           cont (Console.ReadKey())
                           this.reactiveKey())
            let keyName: string = key.Key.ToString()

            let needToRefresh = this.onKey keyName

            let (endX, endY) = this.finish

            if needToRefresh && UtilsView.canPrint then ()
        }
        |> Async.Start
