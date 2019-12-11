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

            if needToRefresh && UtilsView.canPrint then
                UtilsView.printMap (mappa.getIstanceWith ([| utente |], (endY, endX), true)) utente (endY, endX)
        }
        |> Async.Start


type Property(title, value, cursorPosition) =

    member val title = title with get, set
    member val value = value with get, set
    member val cursorPosition = cursorPosition


type MenuSettingsController(startingCursor: int * int) =

    member val startingCursor = startingCursor with get, set
    member val properties = Map.empty with get, set

    member this.addProperty (title: string, value: string) =
        let p = Property(title, value, this.startingCursor)

        this.properties <- this.properties.Add(title, p)

        let xC, yC = this.startingCursor
        this.startingCursor <- (xC, yC + 1)

        title


    member this.hashSettings =
        let mutable returningMap: Map<string, string> = Map.empty

        this.properties |> Map.iter (fun title property -> returningMap <- returningMap.Add(title, property.value))

        returningMap


    member this.setProperty (title: string, value: string) =
        match this.properties.TryGetValue(title) with
        | true, (u: Property) ->
            u.value <- value
            this.properties.Add(title, u) |> ignore
        | _ -> ()

    member this.propertiesList = Map.toList this.properties

    member this.printColorList colorList =
        printfn ""
        for c in UtilsView.coloriDisponibili do
            Console.ForegroundColor <- enum c
            printf "%s%s " UtilsView.rettangolo UtilsView.rettangolo
        Console.ResetColor()
        printfn "\n"
        for i in 0 .. (UtilsView.coloriDisponibili.Length - 1) do
            printf "%d " i
            if (i < 10) then printf " "
        printfn "\n"

    member this.printSettings =
        let oldPosition = (Console.CursorLeft, Console.CursorTop)
        this.properties
        |> Map.iter (fun title property ->
            Console.SetCursorPosition(property.cursorPosition)
            Console.Write(property.title + " : " + property.value))
        Console.SetCursorPosition oldPosition
