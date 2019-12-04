open System

#load "./../models/models.fsx"
#load "./../view/view.fsx"
#load "./../controls/controller.fsx"

open Models
open View
open Controller

let mappa: Mappa = Mappa(21, 21)

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn
let user: Player = Player(startX, startY)

let (endX, endY) = mappa.randSpawn

mappa.setEnd (endX, endY)

let c: Controller = Controller(mappa, user, (endX, endY))

c.reactiveKey()
UtilsView.printMap (mappa.getIstanceWith user (endY, endX) true) user (endY, endX)

Threading.Thread.Sleep(-1)
