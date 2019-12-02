open System.IO
open System

let captureOutput f =
    use writer = new StringWriter()

    use restoreOut =
        let origOut = Console.Out
        { new IDisposable with
            member __.Dispose() = Console.SetOut origOut }
    Console.SetOut writer
    f()
    writer.ToString()

let f x () =
    Console.ForegroundColor <- ConsoleColor.Red
    printf "%s" x

let mutable op = captureOutput (f "Hello World")

Console.ResetColor

op <- op + captureOutput (f "Dio")

Console.ResetColor
printfn " OP : %s" op
