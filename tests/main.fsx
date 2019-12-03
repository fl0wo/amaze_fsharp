open System
open System.IO

let rand = System.Random()

#load "./../utils/map.fsx"
#load "./../utils/player.fsx"
#load "./../utils/utils.fsx"

open Utils
open Player
open Mappa

let mappa: Mappa = new Mappa(21, 21)

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn
let user: Player = new Player(startX, startY)

let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)

module Control =

    let onKey (k: string): bool =
        match k with
        | "LeftArrow"
        | "A" -> (mappa.canGo (user.x - 1) (user.y)) && user.goLeft
        | "RightArrow"
        | "D" -> mappa.canGo (user.x + 1) (user.y) && user.goRight
        | "DownArrow"
        | "S" -> mappa.canGo user.x (user.y + 1) && user.goDown
        | "UpArrow"
        | "W" -> mappa.canGo user.x (user.y - 1) && user.goUp
        | "Spacebar" -> (false)
        | _ -> (false)

let rec reactiveKey() =
    async {
        let! key = Async.FromContinuations(fun (cont, _, _) ->
                       cont (Console.ReadKey())
                       reactiveKey())
        let keyName: string = key.Key.ToString()

        let needToRefresh = Control.onKey keyName

        if needToRefresh && UtilsView.canPrint then
            UtilsView.printMap (mappa.getIstanceWith user (endY, endX) true) user (endY, endX)
    }
    |> Async.Start


let main_form =

    reactiveKey()
    UtilsView.printMap (mappa.getIstanceWith user (endY, endX) true) user (endY, endX)

    // NO END RN
    Threading.Thread.Sleep(-1)

main_form
