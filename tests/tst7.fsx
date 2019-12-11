#load "./../utils/utils.fsx"
#load "./../view/view.fsx"

open System
open Utils
open View

let genMatrixMaze c r =

    let _mappa:array<array<int>> = Utils.initMatrix (c) (r) (int ColorEnum.Bloccato)

    let isntOutside (x, y) =
        (x > 0 && x < (c - 1) && y > 0 && y < (r - 1))

    let canGo y x =
            isntOutside(x,y) && _mappa.[x].[y] <> (int ColorEnum.Bloccato);

    // Genera un labirinto
    let initLabirinto =

        let muretti (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> isntOutside (x, y) && _mappa.[x].[y] = (int ColorEnum.Bloccato))

        let viette (x, y) =
            [ x - 2, y
              x + 2, y
              x, y - 2
              x, y + 2 ]
            |> List.filter (fun (x, y) -> isntOutside (x, y) && _mappa.[x].[y] = (int ColorEnum.Aperto))

        let cellaRandom() = (1 + rand.Next(c - 1)), (1 + rand.Next(r - 1))

        let remove_at i (l: (int * int) list): (int * int) list =
            let riga,colonna = l.[i]
            l |> List.filter (fun (y, x) -> not (y = riga && x = colonna))

        // fst prende il primo elemento di una tupla
        // snd il secondo
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
            let vie_adj = viette (x, y)
            if (vie_adj <> []) then
                let pickedIndex = rand.Next(vie_adj.Length)
                let xn, yn = vie_adj.[pickedIndex]

                let xb, yb = between (x, y) (xn, yn)
                _mappa.[xb].[yb] <- (int ColorEnum.Aperto)
            ()

        let rec apri_vie muri =
            match muri with
            | [] -> ()
            | _ ->
                let indice = rand.Next(muri.Length)
                let xf, yf = muri.[indice]
                _mappa.[xf].[yf] <- (int ColorEnum.Aperto)
                connect_adj (xf, yf)

                let wall_buffer = (UtilsView.__genBuffer _mappa (int ColorEnum.Bloccato))
                (UtilsView.colorBuffer wall_buffer ConsoleColor.White)

                Threading.Thread.Sleep(65);

                apri_vie ((muri |> remove_at indice) @ muretti (xf, yf))

        let riga, colonna = cellaRandom()
        _mappa.[riga].[colonna] <- (int ColorEnum.Aperto)
        apri_vie (muretti (riga, colonna))

        _mappa

    initLabirinto


genMatrixMaze 31 31