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

    member this.r = r
    member this.c = c

    member val creator = creator with get, set
    member val partecipants = users with get, set
    member val nome = _nome with get, set
    member val m = _m with get, set

    override this.ToString() =
        "{" + "\" nome \" : \"" + this.nome + " \"  , " + "\" creator \" : " + this.creator.ToString() + ", " + "\"r\":"
        + this.r.ToString() + ", " + "\" c \" : " + this.c.ToString() + ", " + "\" m \" : \"" + this.m.ToString()
        + " \" , \" partecipants \"  : " + this.partecipants.ToString() + "}"

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

    let get_lobby hash = 
        match lobbyes.TryGetValue(hash) with
        | true, (m: Maze) -> m

    let login username x y c =
        users <- users.Add(username, User(username, x, y, c))
        ()

    let join user lobby =
        let l:Maze = (get_lobby lobby)

        let u:User = (get_user user)

        l.partecipants <- l.partecipants @ [ u ];

        lobbyes.Add(lobby,l) |> ignore;
        ();

    let move user_name x y =
        let u:User = (get_user user_name)
        u.x <- x;
        u.y <- y;

        users <- users.Add(user_name, u);

    let create user_name r c m =
        lobbyes <-
            lobbyes.Add
                ("lobbyof" + user_name, Maze("lobby_name"+user_name, (get_user user_name), r, c, m, [ (get_user user_name) ]))

    let split (s: string) = s.Split [| ' ' |]

    let parser endpoint (msg: string) =
        printfn "parser called %A" msg ;
        let cmd = msg.[0..1]
        let rest = msg.[3..(msg.Length - 1)]

        let par = split rest

        printfn "%A %A %A" cmd rest par

        match cmd with
        | "JO" -> (join par.[0] par.[1])
        | "MO" -> (move par.[0] (par.[1] |> int) (par.[2] |> int) )
        | "CR" ->
            printfn "ma %A" (par.[3])
            (create par.[0] (par.[1] |> int) (par.[2] |> int) (par.[3]))
        | "LO" -> (login par.[0] (par.[1] |> int) (par.[2] |> int) (par.[3] |> int))
        | _ -> ()

        printfn "<- %s %A %A" endpoint msg lobbyes

        //Update user positions

        lobbyes |> Map.iter (fun key value ->
            printfn "key: %s value: %A" key value
            let m:Maze = get_lobby key

            let updatedCreator = (get_user m.creator.nome)
            m.creator <- updatedCreator

            let mutable updatedPartecipants = [];

            m.partecipants |> List.iter (fun x -> 
                let pU:User = (get_user x.nome)
                updatedPartecipants <- updatedPartecipants @ [pU];
            )

            m.partecipants <- updatedPartecipants;

            lobbyes <- lobbyes.Add(key, m)
        );

        lobbyes


let main =
    let t = [ 1 .. 5 ] |> AsyncSerber.removeFirst (fun item -> item = 6)

    let listenTask = (AsyncSerber.listen 8081 CmdParser.parser)

    System.Console.ReadLine() |> ignore

main
