open System
open System.IO

let rand = System.Random()


(*
                                                          _    _ _   _ _
                                                         | |  | | | (_) |
                                                         | |  | | |_ _| |___
                                                         | |  | | __| | / __|
                                                         | |__| | |_| | \__ \
                                                          \____/ \__|_|_|___/
*)

module Mate =
    let randDir = (rand.Next(0, 2))

    let minimo (x: int) (y: int) =
        if (x < y) then x
        else y

    let massimo (x: int) (y: int) =
        if (x > y) then x
        else y

    let noOverflow (x: int) (M: int) = massimo 0 (minimo x M)


type OS =
        | OSX
        | Windows
        | Linux

module Utils =

    let initMatrix R C colore =
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield colore |] |]

    let initPaths R C = 
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield (-1,-1) |] |]
    let getOS = 
        match int Environment.OSVersion.Platform with
        | 4 | 128 -> Linux
        | 6       -> OSX
        | _       -> Windows

type ColorEnum =
    | Bloccato = 1
    | Aperto = 0
    | User = 3
    | End = 4
    | Percorso = 5


(*
                                                          __  __           _      _
                                                         |  \/  |         | |    | |
                                                         | \  / | ___   __| | ___| |
                                                         | |\/| |/ _ \ / _` |/ _ \ |
                                                         | |  | | (_) | (_| |  __/ |
                                                         |_|  |_|\___/ \__,_|\___|_|
*)

// GESTIONE COSE

type Player(x: int, y: int) =
    let mutable _x: int = x
    let mutable _y: int = y

    member this.x = _x
    member this.y = _y

    member this.isLegal (x, y) = (x > 0 && y > 0)

    member this.goUp = _y <- (_y - 1);this.isLegal(x,y);
    member this.goDown = _y <- (_y + 1);this.isLegal(x,y);
    member this.goLeft = _x <- (_x - 1);this.isLegal(x,y);
    member this.goRight = _x <- (_x + 1);this.isLegal(x,y);

type Mappa(r: int, c: int) =
    let _mappa: array<array<int >> = Utils.initMatrix r c (int ColorEnum.Bloccato)

    let mutable _end: int * int = (0,0)
    let mutable _paths:array<array<int * int>> = Utils.initPaths r c
    let mutable arePathsGenerated = false;

    member this.paths = _paths;
    member this.finish = _end
    member this.mappa = _mappa

    member this.r = r
    member this.c = c

    member this.setEnd (x,y) = 
        _end <- (x,y);
        _mappa.[y].[x] <- (int ColorEnum.End)

    member this.isLegal (x, y) = (x > 0 && x < (c - 1) && y > 0 && y < (r - 1))

    member this.canGo y x =     
        this.isLegal(x,y) && _mappa.[x].[y] <> (int ColorEnum.Bloccato);

    // Generates the labirinth
    member this.initLabirinto =

        let frontier (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> this.isLegal (x, y) && _mappa.[x].[y] = (int ColorEnum.Bloccato))
        let neighbor (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> this.isLegal (x, y) && _mappa.[x].[y] = (int ColorEnum.Aperto))

        let randomCell() = (1 + rand.Next(c - 1)), (1 + rand.Next(r - 1))

        let removeAt index (lst: (int * int) list): (int * int) list =
            let x, y = lst.[index]
            lst |> List.filter (fun (a, b) -> not (a = x && b = y))

        let between p1 p2 =
            let x =
                match (fst p2 - fst p1) with
                | 0 -> fst p1
                | 2 -> 1 + fst p1
                | -2 -> -1 + fst p1
                | _ -> -1

            let y =
                match (snd p2 - snd p1) with
                | 0 -> snd p1
                | 2 -> 1 + snd p1
                | -2 -> -1 + snd p1
                | _ -> -1

            (x, y)

        let connect_adj (x, y) =
            let neighbors = neighbor (x, y)
            if (neighbors <> []) then
                let pickedIndex = rand.Next(neighbors.Length)
                let xn, yn = neighbors.[pickedIndex]
                let xb, yb = between (x, y) (xn, yn)
                _mappa.[xb].[yb] <- (int ColorEnum.Aperto)
            ()

        let rec extend front =
            match front with
            | [] -> ()
            | _ ->
                let pickedIndex = rand.Next(front.Length)
                let xf, yf = front.[pickedIndex]
                _mappa.[xf].[yf] <- (int ColorEnum.Aperto)
                connect_adj (xf, yf)
                extend ((front |> removeAt pickedIndex) @ frontier (xf, yf))

        let x, y = randomCell()
        _mappa.[x].[y] <- (int ColorEnum.Aperto)
        extend (frontier (x, y))

        _mappa

    member this.dfs (matrix:array<array<int>>) start = 
        let dirs = [(0,1);(0,-1);(-1,0);(1,0)];
        let (currentR,currentC) = start;

        for (dirX,dirY) in dirs do
            let mutable x = currentR+dirX;
            let mutable y = currentC+dirY;
            let mutable count = 0;

            if(this.isLegal(x,y) && (matrix.[x].[y] <> (int ColorEnum.Bloccato)) ) then
                if (_paths.[x].[y] = (-1,-1)) then // se non l'ho ancora visitato
                    _paths.[x].[y] <- (currentR,currentC);
                    //printfn "eppure entra qui %A" (currentR,currentC)
                    (this.dfs matrix (x,y))

    member this.applyPathOn (map:array<array<int>>) finish curPosition  : array<array<int>> = 
        if(curPosition <> finish) then 
            let (curR,curC) = curPosition;
            map.[curR].[curC] <- (int ColorEnum.Percorso)
            let (nextR,nextC) = _paths.[curR].[curC];
            this.applyPathOn map finish (nextR,nextC)
        else map


    member this.getIstanceWith (tUser: Player) (finish:int*int) (showSolution:bool): array<array<int>> =
        let mutable clone: array<array<int>> = Utils.initMatrix (this.r) (this.c) (int ColorEnum.Bloccato)

        // Applico tutti i blocchi del labirinto
        for x in 0 .. (this.r - 1) do
            for y in 0 .. (this.c - 1) do
                clone.[x].[y] <- this.mappa.[x].[y]

        if( not arePathsGenerated) then
            let (eY,eX) = finish;
            _paths.[eY].[eX] <- (eY,eX);
            this.dfs clone finish   // Parto dalla fine e vedo come arrivare in tutte le celle
            arePathsGenerated <- true;

        if(showSolution) then
            // Applico tutti i blocchi del percorso risolutivo
            clone <- (this.applyPathOn clone finish (tUser.y,tUser.x))


        // Apply current user
        clone.[tUser.y].[tUser.x] <- (int ColorEnum.User)
        clone

    // Sempre un quadratino vuoto circondato da almeno 3 muretti
    member this.randSpawn =
        let mutable coolSpawns = []
        let mutable nCoolSpawns = 0
        for y in 1 .. (r - 1) do
            for x in 1 .. (c - 1) do
                if (_mappa.[y].[x] = (int ColorEnum.Aperto)) then
                    let mutable nAdiacentWalls = 0
                    if (_mappa.[y - 1].[x] = (int ColorEnum.Bloccato)) then nAdiacentWalls <- nAdiacentWalls + 1
                    if (_mappa.[y + 1].[x] = (int ColorEnum.Bloccato)) then nAdiacentWalls <- nAdiacentWalls + 1
                    if (_mappa.[y].[x - 1] = (int ColorEnum.Bloccato)) then nAdiacentWalls <- nAdiacentWalls + 1
                    if (_mappa.[y].[x + 1] = (int ColorEnum.Bloccato)) then nAdiacentWalls <- nAdiacentWalls + 1

                    if nAdiacentWalls>2 then
                        coolSpawns <- coolSpawns @ [ (y, x) ]
                        nCoolSpawns <- nCoolSpawns + 1

        if nCoolSpawns = 0 then (0, 0)
        else coolSpawns.[rand.Next(nCoolSpawns)]




(*

                                                         \ \    / (_)
                                                          \ \  / / _  _____      __
                                                           \ \/ / | |/ _ \ \ /\ / /
                                                            \  /  | |  __/\ V  V /
                                                             \/   |_|\___| \_/\_/
*)

module UtilsView =

    let mutable canPrint = true

    let N_UPPER_BORDER = 2;
    let rettangolo = "█"

    let reset = "\u001b[0m"
    let normal = "\u001b[37;1m"

    let colori: array<string> =
        [| for x in 1 .. 8 do
            yield ("\u001b[3" + x.ToString() + ";1m") |]

    let monitor = new Object() // lock


    let setWindowSize (w: int) (h: int) = Console.SetWindowSize(w, h)

    let cls =
        Console.Clear()

    let __genBufferWithAnsii (m: array<array<int>>) :string = 
        let mutable mbuffer = "\n\n"    // resize with N_UPPER_BORDER
        for r in m do
            for c in r do
                if (c > 1) then mbuffer <- mbuffer +  colori.[c] + rettangolo + rettangolo 
                else if (c = 1) then mbuffer <- mbuffer + normal+ rettangolo + rettangolo
                else mbuffer <- mbuffer + "  " + reset
            mbuffer <- mbuffer + "\n"
        mbuffer

    let __genBuffer (m: array<array<int>>) (kindOf) :string = 
        let mutable mbuffer = "\n\n"    // resize with N_UPPER_BORDER
        for r in m do
            for c in r do
                if (c = kindOf) then mbuffer <- mbuffer + rettangolo + rettangolo
                else mbuffer <- mbuffer + "  "
            mbuffer <- mbuffer + "\n"
        mbuffer

    let colorCell (x,y) c = 
        Console.SetCursorPosition(x * 2, y + (N_UPPER_BORDER));
        Console.BackgroundColor <- c
        Console.WriteLine("  ")
        Console.SetCursorPosition(0, 0)
        Console.ResetColor()

    let colorBuffer (buffer:string) c = 
        Console.SetCursorPosition(0, 0)
        Console.ForegroundColor <- c
        Console.WriteLine(buffer)
        Console.SetCursorPosition(0, 0)
        Console.ResetColor()

    let colorPaths (m: array<array<int>>) (kindOfBlock) (c) = 
        for i in 0..(m.Length-1) do
            for j in 0..(m.[i].Length-1) do
                if(m.[i].[j] = kindOfBlock) then
                    colorCell (j,i) c

    let printMappaLinux (m: array<array<int>>) =
        if (canPrint) then
            canPrint <- false
        
            let mbuffer = __genBufferWithAnsii m
            Console.SetCursorPosition(0, N_UPPER_BORDER)

            printfn "%s" mbuffer
            canPrint <- true
        else
            ()

    let printMappaWindows (m:array<array<int>>) (u:Player) (e:int*int) =
        if (canPrint) then

            Console.ForegroundColor <- ConsoleColor.White

            let wall_buffer = (__genBuffer m (int ColorEnum.Bloccato))

            (colorBuffer wall_buffer ConsoleColor.White)

            (colorPaths m (int ColorEnum.Percorso) ConsoleColor.Green)
            // Coloro l'utente
            (colorCell (u.x, u.y) ConsoleColor.Blue)

            let (endY,endX) = e;

            // Coloro la fine
            (colorCell (endX,endY) ConsoleColor.Red)
        else ()

    let printMap (m:array<array<int>>) (u:Player) (e) =
        match Utils.getOS with
        | Windows -> lock monitor (fun () -> (printMappaWindows m u e))
        | Linux -> printMappaLinux m;
        | OSX -> printMappaLinux m;


(*
                                                           _____            _             _
                                                          / ____|          | |           | |
                                                         | |     ___  _ __ | |_ _ __ ___ | |___
                                                         | |    / _ \| '_ \| __| '__/ _ \| / __|
                                                         | |___| (_) | | | | |_| | | (_) | \__ \
                                                          \_____\___/|_| |_|\__|_|  \___/|_|___/
*)



let mappa: Mappa = new Mappa(21, 21)

mappa.initLabirinto

let (startY, startX) = mappa.randSpawn
let user: Player = new Player(startX, startY)

let (endX, endY) = mappa.randSpawn
mappa.setEnd (endX,endY);

module Control =

    let onKey (k: string): bool =
        match k with
        | "LeftArrow" | "A" -> (mappa.canGo (user.x-1) (user.y)) && user.goLeft;
        | "RightArrow" | "D" -> mappa.canGo (user.x+1) (user.y) && user.goRight;
        | "DownArrow" | "S" -> mappa.canGo user.x (user.y+1) && user.goDown;
        | "UpArrow" | "W" -> mappa.canGo user.x (user.y-1) && user.goUp;
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
            UtilsView.printMap (mappa.getIstanceWith user (endY,endX) true) user (endY, endX)
    }
    |> Async.Start



(*
          _____                    _____                    _____                    _____
         /\    \                  /\    \                  /\    \                  /\    \
        /::\____\                /::\    \                /::\    \                /::\____\
       /::::|   |               /::::\    \               \:::\    \              /::::|   |
      /:::::|   |              /::::::\    \               \:::\    \            /:::::|   |
     /::::::|   |             /:::/\:::\    \               \:::\    \          /::::::|   |
    /:::/|::|   |            /:::/__\:::\    \               \:::\    \        /:::/|::|   |
   /:::/ |::|   |           /::::\   \:::\    \              /::::\    \      /:::/ |::|   |
  /:::/  |::|___|______    /::::::\   \:::\    \    ____    /::::::\    \    /:::/  |::|   | _____
 /:::/   |::::::::\    \  /:::/\:::\   \:::\    \  /\   \  /:::/\:::\    \  /:::/   |::|   |/\    \
/:::/    |:::::::::\____\/:::/  \:::\   \:::\____\/::\   \/:::/  \:::\____\/:: /    |::|   /::\____\
\::/    / _____/:::/    /\::/    \:::\  /:::/    /\:::\  /:::/    \::/    /\::/    /|::|  /:::/    /
 \/____/      /:::/    /  \/____/ \:::\/:::/    /  \:::\/:::/    / \/____/  \/____/ |::| /:::/    /
             /:::/    /            \::::::/    /    \::::::/    /                   |::|/:::/    /
            /:::/    /              \::::/    /      \::::/____/                    |::::::/    /
           /:::/    /               /:::/    /        \:::\    \                    |:::::/    /
          /:::/    /               /:::/    /          \:::\    \                   |::::/    /
         /:::/    /               /:::/    /            \:::\    \                  /:::/    /
        /:::/    /               /:::/    /              \:::\____\                /:::/    /
        \::/    /                \::/    /                \::/    /                \::/    /
         \/____/                  \/____/                  \/____/                  \/____/

*)



let main_form = 

    reactiveKey()
    UtilsView.printMap (mappa.getIstanceWith user (endY,endX) true) user (endY, endX)

    // NO END RN
    Threading.Thread.Sleep(-1)

main_form