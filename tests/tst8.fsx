
(*
    0,0
    5,5
    x : 5-0 = -1
    y : 5-0 = -1

    2,2
*)
let between p1 p2 =
    let x =
        match (fst p2 - fst p1) with
        | 0 -> fst p1
        | 2 -> 1 + fst p1
        | -2 -> -1 + fst p1
        | _ -> -1

    let y =
        match (snd p2 - snd p1) with
        | 0 -> snd p1
        | 2 -> 1 + snd p1
        | -2 -> -1 + snd p1
        | _ -> -1

    (x, y)

let stress = 5

for i in 0..stress do 
    for j in 0..stress do 
        for k in 0..stress do 
            for l in 0..stress do 
                if (fst (between (i,j) (k,l)) <> -1 && (snd (between (i,j) (k,l)) <> -1)) then
                    printfn "%A b %A = %A" (i,j) (k,l) (between (i,j) (k,l))

(*
(0, 0) b (0, 0) = (0, 0)
(0, 0) b (0, 2) = (0, 1)
(0, 0) b (2, 0) = (1, 0)
(0, 0) b (2, 2) = (1, 1)
(0, 1) b (0, 1) = (0, 1)
(0, 1) b (0, 3) = (0, 2)
(0, 1) b (2, 1) = (1, 1)
(0, 1) b (2, 3) = (1, 2)
(0, 2) b (0, 0) = (0, 1)
(0, 2) b (0, 2) = (0, 2)
(0, 2) b (0, 4) = (0, 3)
(0, 2) b (2, 0) = (1, 1)
(0, 2) b (2, 2) = (1, 2)
(0, 2) b (2, 4) = (1, 3)
(0, 3) b (0, 1) = (0, 2)
(0, 3) b (0, 3) = (0, 3)
(0, 3) b (0, 5) = (0, 4)
(0, 3) b (2, 1) = (1, 2)
(0, 3) b (2, 3) = (1, 3)
(0, 3) b (2, 5) = (1, 4)
(0, 4) b (0, 2) = (0, 3)
(0, 4) b (0, 4) = (0, 4)
(0, 4) b (2, 2) = (1, 3)
(0, 4) b (2, 4) = (1, 4)
(0, 5) b (0, 3) = (0, 4)
(0, 5) b (0, 5) = (0, 5)
(0, 5) b (2, 3) = (1, 4)
(0, 5) b (2, 5) = (1, 5)
(1, 0) b (1, 0) = (1, 0)
(1, 0) b (1, 2) = (1, 1)
(1, 0) b (3, 0) = (2, 0)
(1, 0) b (3, 2) = (2, 1)
(1, 1) b (1, 1) = (1, 1)
(1, 1) b (1, 3) = (1, 2)
(1, 1) b (3, 1) = (2, 1)
(1, 1) b (3, 3) = (2, 2)
(1, 2) b (1, 0) = (1, 1)
(1, 2) b (1, 2) = (1, 2)
(1, 2) b (1, 4) = (1, 3)
(1, 2) b (3, 0) = (2, 1)
(1, 2) b (3, 2) = (2, 2)
(1, 2) b (3, 4) = (2, 3)
(1, 3) b (1, 1) = (1, 2)
(1, 3) b (1, 3) = (1, 3)
(1, 3) b (1, 5) = (1, 4)
(1, 3) b (3, 1) = (2, 2)
(1, 3) b (3, 3) = (2, 3)
(1, 3) b (3, 5) = (2, 4)
(1, 4) b (1, 2) = (1, 3)
(1, 4) b (1, 4) = (1, 4)
(1, 4) b (3, 2) = (2, 3)
(1, 4) b (3, 4) = (2, 4)
(1, 5) b (1, 3) = (1, 4)
(1, 5) b (1, 5) = (1, 5)
(1, 5) b (3, 3) = (2, 4)
(1, 5) b (3, 5) = (2, 5)
(2, 0) b (0, 0) = (1, 0)
(2, 0) b (0, 2) = (1, 1)
(2, 0) b (2, 0) = (2, 0)
(2, 0) b (2, 2) = (2, 1)
(2, 0) b (4, 0) = (3, 0)
(2, 0) b (4, 2) = (3, 1)
(2, 1) b (0, 1) = (1, 1)
(2, 1) b (0, 3) = (1, 2)
(2, 1) b (2, 1) = (2, 1)
(2, 1) b (2, 3) = (2, 2)
(2, 1) b (4, 1) = (3, 1)
(2, 1) b (4, 3) = (3, 2)
(2, 2) b (0, 0) = (1, 1)
(2, 2) b (0, 2) = (1, 2)
(2, 2) b (0, 4) = (1, 3)
(2, 2) b (2, 0) = (2, 1)
(2, 2) b (2, 2) = (2, 2)
(2, 2) b (2, 4) = (2, 3)
(2, 2) b (4, 0) = (3, 1)
(2, 2) b (4, 2) = (3, 2)
(2, 2) b (4, 4) = (3, 3)
(2, 3) b (0, 1) = (1, 2)
(2, 3) b (0, 3) = (1, 3)
(2, 3) b (0, 5) = (1, 4)
(2, 3) b (2, 1) = (2, 2)
(2, 3) b (2, 3) = (2, 3)
(2, 3) b (2, 5) = (2, 4)
(2, 3) b (4, 1) = (3, 2)
(2, 3) b (4, 3) = (3, 3)
(2, 3) b (4, 5) = (3, 4)
(2, 4) b (0, 2) = (1, 3)
(2, 4) b (0, 4) = (1, 4)
(2, 4) b (2, 2) = (2, 3)
(2, 4) b (2, 4) = (2, 4)
(2, 4) b (4, 2) = (3, 3)
(2, 4) b (4, 4) = (3, 4)
(2, 5) b (0, 3) = (1, 4)
(2, 5) b (0, 5) = (1, 5)
(2, 5) b (2, 3) = (2, 4)
(2, 5) b (2, 5) = (2, 5)
(2, 5) b (4, 3) = (3, 4)
(2, 5) b (4, 5) = (3, 5)
(3, 0) b (1, 0) = (2, 0)
(3, 0) b (1, 2) = (2, 1)
(3, 0) b (3, 0) = (3, 0)
(3, 0) b (3, 2) = (3, 1)
(3, 0) b (5, 0) = (4, 0)
(3, 0) b (5, 2) = (4, 1)
(3, 1) b (1, 1) = (2, 1)
(3, 1) b (1, 3) = (2, 2)
(3, 1) b (3, 1) = (3, 1)
(3, 1) b (3, 3) = (3, 2)
(3, 1) b (5, 1) = (4, 1)
(3, 1) b (5, 3) = (4, 2)
(3, 2) b (1, 0) = (2, 1)
(3, 2) b (1, 2) = (2, 2)
(3, 2) b (1, 4) = (2, 3)
(3, 2) b (3, 0) = (3, 1)
(3, 2) b (3, 2) = (3, 2)
(3, 2) b (3, 4) = (3, 3)
(3, 2) b (5, 0) = (4, 1)
(3, 2) b (5, 2) = (4, 2)
(3, 2) b (5, 4) = (4, 3)
(3, 3) b (1, 1) = (2, 2)
(3, 3) b (1, 3) = (2, 3)
(3, 3) b (1, 5) = (2, 4)
(3, 3) b (3, 1) = (3, 2)
(3, 3) b (3, 3) = (3, 3)
(3, 3) b (3, 5) = (3, 4)
(3, 3) b (5, 1) = (4, 2)
(3, 3) b (5, 3) = (4, 3)
(3, 3) b (5, 5) = (4, 4)
(3, 4) b (1, 2) = (2, 3)
(3, 4) b (1, 4) = (2, 4)
(3, 4) b (3, 2) = (3, 3)
(3, 4) b (3, 4) = (3, 4)
(3, 4) b (5, 2) = (4, 3)
(3, 4) b (5, 4) = (4, 4)
(3, 5) b (1, 3) = (2, 4)
(3, 5) b (1, 5) = (2, 5)
(3, 5) b (3, 3) = (3, 4)
(3, 5) b (3, 5) = (3, 5)
(3, 5) b (5, 3) = (4, 4)
(3, 5) b (5, 5) = (4, 5)
(4, 0) b (2, 0) = (3, 0)
(4, 0) b (2, 2) = (3, 1)
(4, 0) b (4, 0) = (4, 0)
(4, 0) b (4, 2) = (4, 1)
(4, 1) b (2, 1) = (3, 1)
(4, 1) b (2, 3) = (3, 2)
(4, 1) b (4, 1) = (4, 1)
(4, 1) b (4, 3) = (4, 2)
(4, 2) b (2, 0) = (3, 1)
(4, 2) b (2, 2) = (3, 2)
(4, 2) b (2, 4) = (3, 3)
(4, 2) b (4, 0) = (4, 1)
(4, 2) b (4, 2) = (4, 2)
(4, 2) b (4, 4) = (4, 3)
(4, 3) b (2, 1) = (3, 2)
(4, 3) b (2, 3) = (3, 3)
(4, 3) b (2, 5) = (3, 4)
(4, 3) b (4, 1) = (4, 2)
(4, 3) b (4, 3) = (4, 3)
(4, 3) b (4, 5) = (4, 4)
(4, 4) b (2, 2) = (3, 3)
(4, 4) b (2, 4) = (3, 4)
(4, 4) b (4, 2) = (4, 3)
(4, 4) b (4, 4) = (4, 4)
(4, 5) b (2, 3) = (3, 4)
(4, 5) b (2, 5) = (3, 5)
(4, 5) b (4, 3) = (4, 4)
(4, 5) b (4, 5) = (4, 5)
(5, 0) b (3, 0) = (4, 0)
(5, 0) b (3, 2) = (4, 1)
(5, 0) b (5, 0) = (5, 0)
(5, 0) b (5, 2) = (5, 1)
(5, 1) b (3, 1) = (4, 1)
(5, 1) b (3, 3) = (4, 2)
(5, 1) b (5, 1) = (5, 1)
(5, 1) b (5, 3) = (5, 2)
(5, 2) b (3, 0) = (4, 1)
(5, 2) b (3, 2) = (4, 2)
(5, 2) b (3, 4) = (4, 3)
(5, 2) b (5, 0) = (5, 1)
(5, 2) b (5, 2) = (5, 2)
(5, 2) b (5, 4) = (5, 3)
(5, 3) b (3, 1) = (4, 2)
(5, 3) b (3, 3) = (4, 3)
(5, 3) b (3, 5) = (4, 4)
(5, 3) b (5, 1) = (5, 2)
(5, 3) b (5, 3) = (5, 3)
(5, 3) b (5, 5) = (5, 4)
(5, 4) b (3, 2) = (4, 3)
(5, 4) b (3, 4) = (4, 4)
(5, 4) b (5, 2) = (5, 3)
(5, 4) b (5, 4) = (5, 4)
(5, 5) b (3, 3) = (4, 4)
(5, 5) b (3, 5) = (4, 5)
(5, 5) b (5, 3) = (5, 4)
(5, 5) b (5, 5) = (5, 5)
*)