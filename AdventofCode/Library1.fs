module day1

open System
open System.Text.RegularExpressions

type Turn = 
    | Right
    | Left

type Instruction = 
    | Turn of Turn
    | Move of int

type State = 
    { pos : int * int
      facing : int }

let dir = "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, 
        L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, 
        R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, 
        R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, 
        R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, 
        L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, 
        L4, L4, L3, L4, L3, L5, R2, R4, L2"

let rmatches = 
    Regex.Matches(dir, "([LR])|(\\d+)") // match L, R, or any digit (not commas or spaces)
    |> Seq.cast<Match> // originally of type MatchCollection but can be cast to a Seq<'a>
    |> Seq.map (fun m -> m.Value) // MatchCollection objects have Captures, Groups, Index, Length, Success, and Value members
    |> Seq.collect (function 
           | "L" -> Turn Left
           | "R" -> [ Turn Right ]
           | n -> Move(Convert.ToInt32(n))

let directions = 
    dir.Split(',')
    |> Array.toList
    |> List.map (fun s -> s.Trim())

let turnToNumber t = 
    match t with
    | "R" -> 1
    | "L" -> (-1)
    | _ -> failwith "Invalid direction to turn to."

let newCoords heading n (x, y) = 
    match heading with
    | 1 -> x, y + n
    | 2 -> x + n, y
    | 3 -> x, y - n
    | 4 | 0 -> x - n, y
    | _ -> failwith "Invalid direction to walk in."

let rec findBaseWithCoords d facing coords = 
    let x, y = coords
    match d with
    | (h : string) :: t -> 
        let turn', walk = h.Substring(0, 1), Convert.ToDouble(h.Substring(1))
        let turn = turnToNumber turn'
        let newFacing = (facing + turn |> abs) % 4 // feel like the problem is here in the modulos. 
        findBaseWithCoords t newFacing (newCoords newFacing walk (x, y))
    | [] -> abs (x) + abs (y)

let findBase d = 
    let origin = 0.0, 0.0 // the starting point to which all distance is relative
    findBaseWithCoords d 1 origin

let exThree = [ "R5"; "L5"; "R5"; "R3" ]
let exTwo = [ "R2"; "L3" ]
let exOne = [ "R2"; "R2"; "R2" ]

[ exOne; exTwo; exThree; directions ]
|> List.map findBase
|> printfn "%A"
// gives the wrong answer for directions, 425 instead of 252.
