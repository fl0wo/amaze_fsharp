


#load "./../utils/utils.fsx"

open System
open Utils

let genMatrixMaze c r =

    let _mappa:array<array<int>> = Utils.initMatrix (c*2) (r*2) (int ColorEnum.Bloccato)

    let isLegal (x, y) =
        (x > 0 && x < (c - 1) && y > 0 && y < (r - 1))

    let canGo y x =
            isLegal(x,y) && _mappa.[x].[y] <> (int ColorEnum.Bloccato);

    // Generates the labirinth
    let initLabirinto =

        let frontier (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> isLegal (x, y) && _mappa.[x].[y] = (int ColorEnum.Bloccato))
        let neighbor (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> isLegal (x, y) && _mappa.[x].[y] = (int ColorEnum.Aperto))

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

    initLabirinto

(*


#load "./../utils/utils.fsx"

open System
open Utils

let genMatrixMaze r c =

    let _mappa: array<array<int>> = Utils.initMatrix r c (int ColorEnum.Bloccato)

    let isLegal (riga, colonna) = (riga> 0 && riga < (r - 1) && colonna > 0 && colonna < (c - 1))

    let canGo riga colonna = isLegal (riga, colonna) && _mappa.[riga].[colonna] = (int ColorEnum.Bloccato)

    // Generates the labirinth
    let initLabirinto =

        let randomCell() = (1 + rand.Next(c - 1)), (1 + rand.Next(r - 1))

        let rec random_dfs myR myC =
            printfn "ok %A" _mappa.[myR].[myC]
            _mappa.[myR].[myC] <- (int ColorEnum.Aperto)
            printfn "nok %A" _mappa.[myR].[myC]

            let dirs =
                [| (0, 1)
                   (0, -1)
                   (-1, 0)
                   (1, 0) |]
                |> Utils.shuffle

            for dirR, dirC in dirs do
                let nextR = myR + dirR
                let nextC = myC + dirC

                if ((canGo nextR nextC)) then (random_dfs nextR nextC)

        let x, y = randomCell()
        (random_dfs x y)

        printfn "%A" _mappa
        _mappa

    initLabirinto
    
    *)