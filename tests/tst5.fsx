#load "../parsers/json_parser.fsx"

#load "./../models/models.fsx"


open Parser

let ss =
    parse
        "{\" nome \" : \"lobby_nameflo \"  , \" creator \" : { \"nome\" : \"flo\", \"x\" :\r\n2, \"y\" : 14, \"colore\" : 0}, \"r\":21, \" c \" : 21, \" m \" : \"11111111111111111111111 \" , \r\n\" partecipants \"  : [{ \"nome\" : \"flo\", \"x\" : 12, \"y\" : 12, \"colore\" : 0}]}"

type User =
    { nome: string
      x: int
      y: int
      colore: int }

type Maze =
    { nome: string
      creator: User
      m: string
      partecipants: User list }

(*
    type Json =
    | Null
    | Bool of bool
    | Number of float
    | String of string
    | Array of Json list
    | Object of (string * Json) list
*)

let fromJson2Maze s: Maze =


    let o = ss

    printfn "%A" o
