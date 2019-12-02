let esc = string (char 0x1B)


let csi = esc + "["
let printsequencef f = Printf.kprintf (fun s -> System.Console.Write(esc + s)) f
let printcsif f = Printf.kprintf (fun s -> System.Console.Write(csi + s)) f
let selectGraphicRendition (gr: int list) = printcsif "%sm" (System.String.Join(";", gr |> Seq.map string))
let resetColor() = selectGraphicRendition [ 0 ]
let setForeground i = selectGraphicRendition ([ 30 + i ])
let setBackground i = selectGraphicRendition ([ 40 + i ])

let setExtendedForeground i = selectGraphicRendition [ 38; 5; i ]
let setExtendedBackground i = selectGraphicRendition [ 48; 5; i ]
let setForegroundRgb r g b = selectGraphicRendition [ 38; 2; r; g; b ]
let setBackgroundRgb r g b = selectGraphicRendition [ 48; 2; r; g; b ]

// let extendedBlock i =
//     for j in i .. i + 5 do
//         setExtendedBackground j
//         printf "  "

// for row in 0 .. 5 do
//     for b in 0 .. 5 do
//         extendedBlock (16 + 36 * b + row * 6)
//         resetColor()
//         printf " "
//     printfn ""
open System
open System.Threading

Console.ForegroundColor <- ConsoleColor.Red
Console.BackgroundColor <- ConsoleColor.Yellow
Console.WriteLine("Red on Yellow")

Console.ForegroundColor <- ConsoleColor.White
Console.BackgroundColor <- ConsoleColor.Black
Console.WriteLine("White on Black")

Console.ForegroundColor <- ConsoleColor.Green
Console.BackgroundColor <- ConsoleColor.Blue
Console.WriteLine("Green on Blue")

Console.ResetColor()
Console.WriteLine("Back to normal")

Console.WriteLine("\u001b[31mHello World!\u001b[0m")



Console.WriteLine("\u001b[31mHello World!\u001b[0m")
