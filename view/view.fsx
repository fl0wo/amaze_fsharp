open System
open System.IO

let rand = System.Random()

#load "./../utils/utils.fsx"
#load "./../models/models.fsx"

open Utils
open Models

module UtilsView =

    let mutable canPrint = true

    let N_UPPER_BORDER = 2
    let rettangolo = "█"

    let reset = "\u001b[0m"
    let normal = "\u001b[37;1m"

    let colori: array<string> =
        [| for x in 1 .. 10 do
            yield ("\u001b[3" + x.ToString() + ";1m") |]

    let monitor = new Object() // lock

    // let mutable oldMapClone:array<array<int>> = null;

    let setWindowSize (w: int) (h: int) = Console.SetWindowSize(w, h)

    let cls = Console.Clear()

    let __genBufferWithAnsii (m: array<array<int>>): string =
        let mutable mbuffer = "\n\n" // resize with N_UPPER_BORDER
        for r in m do
            for c in r do
                if (c > 1) then mbuffer <- mbuffer + colori.[c] + rettangolo + rettangolo
                else if (c = 1) then mbuffer <- mbuffer + normal + rettangolo + rettangolo
                else mbuffer <- mbuffer + "  " + reset
            mbuffer <- mbuffer + "\n"
        mbuffer

    let __genBuffer (m: array<array<int>>) (kindOf): string =
        let mutable mbuffer = "\n\n" // resize with N_UPPER_BORDER
        for r in m do
            for c in r do
                if (c = kindOf) then mbuffer <- mbuffer + rettangolo + rettangolo
                else mbuffer <- mbuffer + "  "
            mbuffer <- mbuffer + "\n"
        mbuffer

    let colorCell (x, y) c =
        Console.SetCursorPosition(x * 2, y + (N_UPPER_BORDER))
        Console.ForegroundColor <- c
        Console.WriteLine(rettangolo + rettangolo)
        Console.SetCursorPosition(0, 0)
        Console.ResetColor()

    let colorBuffer (buffer: string) c =
        Console.SetCursorPosition(0, 0)
        Console.ForegroundColor <- c
        Console.WriteLine(buffer)
        Console.SetCursorPosition(0, 0)
        Console.ResetColor()

    let colorPaths (m: array<array<int>>) (kindOfBlock) (c) =
        for i in 0 .. (m.Length - 1) do
            for j in 0 .. (m.[i].Length - 1) do
                // ricoloro solo se ha un colore diverso, altrimenti non ha senso
                if (m.[i].[j] = kindOfBlock) then colorCell (j, i) c

    let printMappaLinux (m: array<array<int>>) =
        if (canPrint) then
            canPrint <- false
            let mbuffer = __genBufferWithAnsii m
            Console.SetCursorPosition(0, N_UPPER_BORDER)

            printfn "%s" mbuffer
            canPrint <- true
        else
            ()

    let printMappaWindows (m: array<array<int>>) (u: Player) (e: int * int) =
        if (canPrint) then

            Console.ForegroundColor <- ConsoleColor.White

            let wall_buffer = (__genBuffer m (int ColorEnum.Bloccato))

            (colorBuffer wall_buffer ConsoleColor.White)

            (colorPaths m (int ColorEnum.Percorso) ConsoleColor.Green)
            // Coloro l'utente
            (colorCell (u.x, u.y) ConsoleColor.Blue)

            let (endY, endX) = e

            // Coloro la fine
            (colorCell (endX, endY) ConsoleColor.Red)
        else
            ()

    let printMap (m: array<array<int>>) (u: Player) (e) =
        // Lock per eseguire questa sezione critica solo uno alla volta
        // Anche se "spamma" stampa con i suoi tempi.
        lock monitor (fun () ->
            match Utils.getOS with
            | Windows -> printMappaWindows m u e // ESCAPE ANSII non disponibile
            | Linux -> printMappaLinux m // ESCAPE ANSII disponibile
            | OSX -> printMappaLinux m // ESCAPE ANSII disponibile

            // oldMapClone <- m;
            )
