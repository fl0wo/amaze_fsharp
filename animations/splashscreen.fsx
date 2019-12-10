open System

#load "./../utils/utils.fsx"

open Utils

let FLO = "
          _____                    _____           _______         
         /\    \                  /\    \         /::\    \        
        /::\    \                /::\____\       /::::\    \       
       /::::\    \              /:::/    /      /::::::\    \      
      /::::::\    \            /:::/    /      /::::::::\    \     
     /:::/\:::\    \          /:::/    /      /:::/~~\:::\    \    
    /:::/__\:::\    \        /:::/    /      /:::/    \:::\    \   
   /::::\   \:::\    \      /:::/    /      /:::/    / \:::\    \  
  /::::::\   \:::\    \    /:::/    /      /:::/____/   \:::\____\ 
 /:::/\:::\   \:::\    \  /:::/    /      |:::|    |     |:::|    |
/:::/  \:::\   \:::\____\/:::/____/       |:::|____|     |:::|    |
\::/    \:::\   \::/    /\:::\    \        \:::\    \   /:::/    / 
 \/____/ \:::\   \/____/  \:::\    \        \:::\    \ /:::/    /  
          \:::\    \       \:::\    \        \:::\    /:::/    /   
           \:::\____\       \:::\    \        \:::\__/:::/    /    
            \::/    /        \:::\    \        \::::::::/    /     
             \/____/          \:::\    \        \::::::/    /      
                               \:::\    \        \::::/    /       
                                \:::\____\        \::/____/        
                                 \::/    /         ~~              
                                  \/____/                          
                                                                   "


let lenghtHoriziontal = "          _____                    _____           _______         ".Length
let lenghtVertical = 29 - 8

let printTxt exceptIndex =
    Console.Clear()

    Console.SetCursorPosition(0, 0)

    let mutable m: array<array<char>> = Utils.fromStringToMatrixChar (FLO) lenghtVertical lenghtHoriziontal

    let str = Utils.fromMatrixToString (m)

    Console.Write(str)




let startAnimation msTime =
    let nSteps = 10
    let deltaT = msTime / nSteps
    let mutable currentTime = msTime

    let mutable currentX = 0

    let mutable rotation = 0

    (printTxt rotation)


    while (currentTime > 0) do
        (printTxt rotation)

        currentX <- currentX
        currentTime <- currentTime - deltaT

        Threading.Thread.Sleep(deltaT)
