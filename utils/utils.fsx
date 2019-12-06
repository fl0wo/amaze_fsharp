let rand = System.Random()

open System
open System.IO


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

    let inline charToInt c = int c - int '0'

    let initMatrix R C colore =
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield colore |] |]

    let initPaths R C =
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield (-1, -1) |] |]

    let getOS =
        match int Environment.OSVersion.Platform with
        | 4
        | 128 -> Linux
        | 6 -> OSX
        | _ -> Windows

    let fromStringToMatrix (str: string) righe colonne: array<array<int>> =
        let mutable ctr = 0 //evitabile
        [| for r in 1 .. righe do
            yield [| for c in 1 .. colonne do
                         ctr <- ctr + 1
                         yield (str.[ctr - 1]) |> charToInt |] |]


// array2d[i][j] = array1d[(j*10) + i]; is wrong. It should be array2d[i][j] = array1d[j%3+i*3];

// let ss = "101010101010"

// printfn "%A" (Utils.fromStringToMatrix ss 3 3)

type ColorEnum =
    | Bloccato = 1
    | Aperto = 0
    | User = 3
    | End = 4
    | Percorso = 5
