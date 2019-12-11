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

    let fromStringToMatrixChar (str: string) righe colonne: array<array<char>> =
        let mutable ctr = 0 //evitabile
        [| for r in 1 .. righe do
            yield [| for c in 1 .. colonne do
                         ctr <- ctr + 1
                         yield (str.[ctr - 1]) |] |]

    let fromMatrixToString (m: array<array<char>>) =
        let mutable str = ""
        for r in m do
            for c in r do
                str <- str + c.ToString()
            str <- "\n"

    let swap (a: _ []) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle (a: 'T array): 'T array =
        (Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a)
        a

(*
            111111111
            000001000
            111110111
        
        
        *)
// let ttoString m =
//     let mutable buf: string = ""
//     for r in (m) do
//         for c in r do
//             buf <- buf + c.ToString()
//     buf

// let ss =
//     ttoString
//         [| [| 1; 1; 1; 1; 1; 1; 1 |]
//            [| 0; 0; 0; 1; 0; 0; 0 |]
//            [| 1; 1; 1; 0; 1; 1; 1 |] |]

// printfn "%A" ss
// let ss =
//     "1111111111111111111111110000000001010000011111111401110101010111111100000100010101010111110111011101010111011111010001000100000001111111111111010101111111110101000100010000011111010111010101110111111100010000010100000111111101110111010111111111000001010001000001111101110111110101011111110100000000010100011111110111010101010111111100010001010101000111111101111101010111011111010100000101000101111101111111011101111111110000000000010000011111111111111111111111111111111111111111111111"

// printfn "%A" (Utils.fromStringToMatrix ss 21 21)

// array2d[i][j] = array1d[(j*10) + i]; is wrong. It should be array2d[i][j] = array1d[j%3+i*3];

// let ss = "101010101010"

// printfn "%A" (Utils.fromStringToMatrix ss 3 3)

type ColorEnum =
    | Bloccato = 1
    | Aperto = 0
    | User = 3
    | End = 4
    | Percorso = 5
