open System

#load "./../models/models.fsx"
#load "./../view/view.fsx"
#load "./../controls/controller.fsx"
#load "./../animations/splashscreen.fsx"


open Models
open View
open Controller

open Splashscreen

startAnimation 3000

let mutable oldTop = Console.CursorTop
let mutable oldLeft = Console.CursorLeft

let mS = MenuSettingsController(oldLeft, oldTop)

mS.initDefaultValues

let askInput title suggerimento =
    printfn "Inserire %s (es %s ): \n" title suggerimento
    let input = Console.ReadLine()
    mS.addProperty (title, input) |> ignore
    input


let righe = askInput "righe" "21" |> int
let colonne = askInput "colonne" "21" |> int

mS.printColorList UtilsView.coloriDisponibili

askInput "colore giocatore" "2" |> int
askInput "colore uscita" "3" |> int
askInput "colore percorso" "3" |> int
askInput "colore muretti" "3" |> int

UtilsView.mySettings <- mS.hashSettings

// Prendo righe e colonne dai settings
let mappa: Mappa = Mappa(
    (UtilsView.getSetting "righe") |> int
,  (UtilsView.getSetting "colonne") |> int )

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn
let user: Player = Player(startX, startY, (fun x y -> ()))
let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)

let c: Controller = Controller(mappa, user, (endX, endY))

c.reactiveKey()

UtilsView.lastCursorPosition <- (Console.CursorLeft, Console.CursorTop)

UtilsView.printMap (mappa.getIstanceWith ([| user |], (endY, endX),false)) user (endY, endX)
Threading.Thread.Sleep(-1)
