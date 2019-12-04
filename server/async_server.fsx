#load "./serber.fsx"

type User(nome, x: int, y: int, colore: int) =
    member this.nome = nome
    member val x = x with get, set
    member val y = y with get, set
    member this.colore = colore

    override this.ToString() =
        "{" + " \"nome\" : " + "\"" + this.nome + "\"" + "," + " \"x\" : " + this.x.ToString() + "," + " \"y\" : "
        + this.y.ToString() + "," + " \"colore\" : " + this.colore.ToString() + "}"

type Maze(_nome: string, creator: User, r: int, c: int, _m: string, users: User list) =

    member this.creator = creator
    member this.r = r
    member this.c = c

    member val partecipants = users with get, set
    member val nome = _nome with get, set
    member val m = _m with get, set

    override this.ToString() =
        "{" + "\" nome \" : \"" + this.nome + " \"  , " + "\" creator \" : " + this.creator.ToString() + ", "
        + "\"r\":" + this.r.ToString() + ", " + "\" c \" : " + this.c.ToString() + ", " + "\" m \" : \""
        + this.m.ToString() + " \" , \" partecipants \"  : " + this.partecipants.ToString() + "}"

(*
JO -> JOIN (JO flo lobby1)
MO -> MOVE (MO flo x y)
CR -> CREATE (CR flo mazeofflo r c m)
LO -> LOGIN (LO flo x y c)
*)

module CmdParser =


    let mutable lobbyes: Map<string, Maze> = Map.empty
    let mutable users: Map<string, User> = Map.empty

    let get_user hash =
        match users.TryGetValue(hash) with
            | true, (u: User) -> u

    let login username x y c =
        users <- users.Add(username, User(username, x, y, c))
        ()

    // let join user lobby =
    //     match lobbyes.TryGetValue(lobby) with
    //     | true, (l: Maze) ->
    //         match users.TryGetValue(user) with
    //         | true, (u: User) -> l.partecipants <- l.partecipants @ [ u ]

    // let move user x y =
    //     match users.TryGetValue(user) with
    //     | true, (u: User) ->
    //         u.x <- x
    //         u.y <- y



    // let create user_name lobby_name r c m =

    //     match users.TryGetValue(user_name) with
    //     | true, (u: User) ->
    //         lobbyes.Add(lobby_name, Maze(lobby_name, u, r, c, m, [ u ]))
    //         ()

    let split (s: string) = s.Split [| ' ' |]

    let parser endpoint (msg: string) =
        let cmd = msg.[0..1]
        let rest = msg.[3..(msg.Length - 1)]

        let par = split rest

        printfn "%A %A %A" cmd rest par

        match cmd with
        // | "JO" -> (join par.[0] par.[1])
        // | "MO" -> (move par.[0] (par.[1] |> int) (par.[2] |> int) )
        // | "CR" -> (create par.[0] par.[1] (par.[2] |> int)  (par.[3] |> int) (par.[4]) )
        | "LO" -> (login par.[0] (par.[1] |> int) (par.[2] |> int) (par.[3] |> int))

        lobbyes <-
            lobbyes.Add("lobby1", Maze("lobby1", (get_user "flo"), 21, 21, "010101010101001101010100101", [(get_user "flo");(get_user "flo")]))


        printfn "<- %s %A %A" endpoint msg lobbyes
        lobbyes


let main =
    let t = [ 1 .. 5 ] |> AsyncSerber.removeFirst (fun item -> item = 6)

    let listenTask = (AsyncSerber.listen 8081 CmdParser.parser)

    System.Console.ReadLine() |> ignore

main
