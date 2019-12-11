
let rand = System.Random()

open System
open System.IO

#load "./../utils/utils.fsx"
#load "./../maze-generators/dfs_stack.fsx"
#load "./../maze-generators/dfs_tree.fsx"

open Dfs_stack
open Dfs_tree

open Utils

type Player(x: int, y: int,lambdaMove) =

    // da usare val with get,set ... ma vabbhe
    let mutable _x: int = x
    let mutable _y: int = y

    // member val lambdaMove = (fun x y -> ()) with get,set

    member this.lambdaMove = lambdaMove;

    member this.x = _x
    member this.y = _y

    member this.isLegal (x, y) = (x > 0 && y > 0)

    member this.updateAll x y = 
        (this.lambdaMove x y);
        this.isLegal(x,y);

    member this.goUp = _y <- (_y - 1);(this.updateAll _x _y)
    member this.goDown = _y <- (_y + 1);(this.updateAll _x _y)
    member this.goLeft = _x <- (_x - 1);(this.updateAll _x _y)
    member this.goRight = _x <- (_x + 1);(this.updateAll _x _y)

type Mappa(r: int, c: int) =
    let mutable _mappa: array<array<int >> = Utils.initMatrix r c (int ColorEnum.Bloccato)

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

    member this.isLegal (x, y) = 
        (x > 0 && x < (c - 1) && y > 0 && y < (r - 1))

    member this.canGo y x =     
            this.isLegal(x,y) && _mappa.[x].[y] <> (int ColorEnum.Bloccato);

    member this.initLabirinto = 
        _mappa <- genMatrixMaze r c

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


    member this.getIstanceWith ((tUser: array<Player>),(mappa_ : array<array<int>>),(finish:int*int),(showSolution:bool)) : array<array<int>> =
        let mutable clone: array<array<int>> = Utils.initMatrix (this.r) (this.c) (int ColorEnum.Bloccato)

        // Applico tutti i blocchi del labirinto
        for x in 0 .. (this.r - 1) do
            for y in 0 .. (this.c - 1) do
                clone.[x].[y] <- mappa_.[x].[y]

        if( not arePathsGenerated) then
            let (eY,eX) = finish;
            _paths.[eY].[eX] <- (eY,eX);
            this.dfs clone finish   // Parto dalla fine e vedo come arrivare in tutte le celle
            arePathsGenerated <- true;

        let firstUser = tUser.[0];

        if(showSolution) then
            // Applico tutti i blocchi del percorso risolutivo
            clone <- (this.applyPathOn clone finish (firstUser.y,firstUser.x))


        // Apply current user
        clone.[firstUser.y].[firstUser.x] <- (int ColorEnum.User)
        clone

        member this.getIstanceWith ((tUser: array<Player>),(finish:int*int),(showSolution:bool)): array<array<int>> =
            (this.getIstanceWith(tUser,(this.mappa),finish,showSolution))

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

    override this.ToString() =
        let mutable buf:string = "";
        for r in (this.mappa) do
            for c in r do
                buf <- buf + c.ToString();
        buf
