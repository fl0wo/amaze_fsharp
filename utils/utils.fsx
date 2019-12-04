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

type ColorEnum =
    | Bloccato = 1
    | Aperto = 0
    | User = 3
    | End = 4
    | Percorso = 5
5