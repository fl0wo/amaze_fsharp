open System

#load "./../utils/utils.fsx"

open Utils

let FLO =
    "          _____                    _____           _______         \r\n         /\\    \\                  /\\    \\         /::\\    \\        \r\n        /::\\    \\                /::\\____\\       /::::\\    \\       \r\n       /::::\\    \\              /:::/    /      /::::::\\    \\      \r\n      /::::::\\    \\            /:::/    /      /::::::::\\    \\     \r\n     /:::/\\:::\\    \\          /:::/    /      /:::/~~\\:::\\    \\    \r\n    /:::/__\\:::\\    \\        /:::/    /      /:::/    \\:::\\    \\   \r\n   /::::\\   \\:::\\    \\      /:::/    /      /:::/    / \\:::\\    \\  \r\n  /::::::\\   \\:::\\    \\    /:::/    /      /:::/____/   \\:::\\____\\ \r\n /:::/\\:::\\   \\:::\\    \\  /:::/    /      |:::|    |     |:::|    |\r\n/:::/  \\:::\\   \\:::\\____\\/:::/____/       |:::|____|     |:::|    |\r\n\\::/    \\:::\\   \\::/    /\\:::\\    \\        \\:::\\    \\   /:::/    / \r\n \\/____/ \\:::\\   \\/____/  \\:::\\    \\        \\:::\\    \\ /:::/    /  \r\n          \\:::\\    \\       \\:::\\    \\        \\:::\\    /:::/    /   \r\n           \\:::\\____\\       \\:::\\    \\        \\:::\\__/:::/    /    \r\n            \\::/    /        \\:::\\    \\        \\::::::::/    /     \r\n             \\/____/          \\:::\\    \\        \\::::::/    /      \r\n                               \\:::\\    \\        \\::::/    /       \r\n                                \\:::\\____\\        \\::/____/        \r\n                                 \\::/    /         ~~              \r\n                                  \\/____/                          \r\n                                                                   "
let LOF =
    "          _____           _______                   _____          \r\n         /\\    \\         /::\\    \\                 /\\    \\         \r\n        /::\\____\\       /::::\\    \\               /::\\    \\        \r\n       /:::/    /      /::::::\\    \\             /::::\\    \\       \r\n      /:::/    /      /::::::::\\    \\           /::::::\\    \\      \r\n     /:::/    /      /:::/~~\\:::\\    \\         /:::/\\:::\\    \\     \r\n    /:::/    /      /:::/    \\:::\\    \\       /:::/__\\:::\\    \\    \r\n   /:::/    /      /:::/    / \\:::\\    \\     /::::\\   \\:::\\    \\   \r\n  /:::/    /      /:::/____/   \\:::\\____\\   /::::::\\   \\:::\\    \\  \r\n /:::/    /      |:::|    |     |:::|    | /:::/\\:::\\   \\:::\\    \\ \r\n/:::/____/       |:::|____|     |:::|    |/:::/  \\:::\\   \\:::\\____\\\r\n\\:::\\    \\        \\:::\\    \\   /:::/    / \\::/    \\:::\\   \\::/    /\r\n \\:::\\    \\        \\:::\\    \\ /:::/    /   \\/____/ \\:::\\   \\/____/ \r\n  \\:::\\    \\        \\:::\\    /:::/    /             \\:::\\    \\     \r\n   \\:::\\    \\        \\:::\\__/:::/    /               \\:::\\____\\    \r\n    \\:::\\    \\        \\::::::::/    /                 \\::/    /    \r\n     \\:::\\    \\        \\::::::/    /                   \\/____/     \r\n      \\:::\\    \\        \\::::/    /                                \r\n       \\:::\\____\\        \\::/____/                                 \r\n        \\::/    /         ~~                                       \r\n         \\/____/                                                   \r\n                                                                   "
let OFL =
    "         _______                   _____                    _____  \r\n        /::\\    \\                 /\\    \\                  /\\    \\ \r\n       /::::\\    \\               /::\\    \\                /::\\____\\\r\n      /::::::\\    \\             /::::\\    \\              /:::/    /\r\n     /::::::::\\    \\           /::::::\\    \\            /:::/    / \r\n    /:::/~~\\:::\\    \\         /:::/\\:::\\    \\          /:::/    /  \r\n   /:::/    \\:::\\    \\       /:::/__\\:::\\    \\        /:::/    /   \r\n  /:::/    / \\:::\\    \\     /::::\\   \\:::\\    \\      /:::/    /    \r\n /:::/____/   \\:::\\____\\   /::::::\\   \\:::\\    \\    /:::/    /     \r\n|:::|    |     |:::|    | /:::/\\:::\\   \\:::\\    \\  /:::/    /      \r\n|:::|____|     |:::|    |/:::/  \\:::\\   \\:::\\____\\/:::/____/       \r\n \\:::\\    \\   /:::/    / \\::/    \\:::\\   \\::/    /\\:::\\    \\       \r\n  \\:::\\    \\ /:::/    /   \\/____/ \\:::\\   \\/____/  \\:::\\    \\      \r\n   \\:::\\    /:::/    /             \\:::\\    \\       \\:::\\    \\     \r\n    \\:::\\__/:::/    /               \\:::\\____\\       \\:::\\    \\    \r\n     \\::::::::/    /                 \\::/    /        \\:::\\    \\   \r\n      \\::::::/    /                   \\/____/          \\:::\\    \\  \r\n       \\::::/    /                                      \\:::\\    \\ \r\n        \\::/____/                                        \\:::\\____\\\r\n         ~~                                               \\::/    /\r\n                                                           \\/____/ \r\n                                                                   "

let floAnimation = [| FLO; LOF; OFL |]

let printTxt exceptIndex =
    Console.Clear()

    Console.SetCursorPosition(0, 0)

    printfn "%s" (floAnimation.[exceptIndex])


let startAnimation msTime =
    async {
        let nSteps = 10
        let deltaT = msTime / nSteps
        let mutable currentTime = msTime

        let mutable currentX = 0

        let mutable rotation = 0

        (printTxt rotation) |> ignore


        while (currentTime > 0) do
            (printTxt rotation) |> ignore


            rotation <- (rotation + 1) % floAnimation.Length
            currentX <- currentX
            currentTime <- currentTime - deltaT

            Threading.Thread.Sleep(deltaT)
    }
    |> Async.RunSynchronously
