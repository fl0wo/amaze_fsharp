open System

#load "./../models/models.fsx"
#load "./../view/view.fsx"
#load "./../controls/controller.fsx"
#load "./../animations/splashscreen.fsx"


open Models
open View
open Controller

open Splashscreen

// startAnimation 1500

let askInput title =
    printfn "Inserire %s : \n" title
    Console.ReadLine()

let righe = askInput "righe" |> int
let colonne = askInput "colonne" |> int

let mappa: Mappa = Mappa(righe, colonne)

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn
let user: Player = Player(startX, startY, (fun x y -> ()))
let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)

let c: Controller = Controller(mappa, user, (endX, endY))

c.reactiveKey()
UtilsView.printMap (mappa.getIstanceWith ([| user |], (endY, endX), true)) user (endY, endX)
Threading.Thread.Sleep(-1)
