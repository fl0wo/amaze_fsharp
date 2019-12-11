open System
open System.Windows

#load "./../view/view.fsx"
#load "./..//utils/utils.fsx"

open Utils
open View

let width, height = 15, 10
let random = Random()
 
let tiles = Array.init height (fun _ -> Array.zeroCreate<bool> width)
 
type Tree =
    | Leaf of int * int
    | Node of int * int * Tree list
 
let rec buildMaze (x, y) =
    let shuffleArray array =
        let n = Array.length array
        for x = 1 to n do
            let i = n - x
            let j = random.Next(i)
            let tmp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- tmp
        array
 
    let neighbors = 
        [| x - 1, y; 
           x + 1, y; 
           x, y - 1; 
           x, y + 1 |]
        |> shuffleArray
        |> Array.filter(fun (x, y) -> x >= 0 && x < width && 
                                      y >= 0 && y < height)
    let visited = [ for x, y in neighbors do
                        if not tiles.[y].[x] then
                            tiles.[y].[x] <- true
                            yield buildMaze (x, y) ]
    if List.length visited > 0 then
        Node(x, y, visited) 
    else
        Leaf(x, y)
 
let maze = buildMaze (0, 0)
 
let mutable matrxi: array<array<int>> =
    [| for x in 0 .. (width*2) do
        yield [| for x in 0 .. (height*2) do
                     yield 0 |] |]

let rec drawMaze prevx prevy node =
    let addTile (x, y) =
        matrxi.[x].[y] <- 1;

    let colorGrid x y =
        let finalX, finalY = x * 2, y * 2
        let interX = finalX + prevx - x
        let interY = finalY + prevy - y
        addTile (finalX, finalY)
        if (interX <> finalX || interY <> finalY) then
            addTile (interX, interY)

    match node with
    | Node(x, y, children) ->
        colorGrid x y
        List.iter (drawMaze x y) children
    | Leaf(x, y) ->
        colorGrid x y

drawMaze 0 0 maze

let wall_buffer = (UtilsView.__genBuffer matrxi (int ColorEnum.Bloccato))

(UtilsView.colorBuffer wall_buffer ConsoleColor.White)

