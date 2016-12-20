module Day1
open System.Text.RegularExpressions

type Turn = 
    | Right
    | Left

type Instruction = 
    | Turn of Turn
    | Move of int

type State = 
    { pos : int * int
      heading : int * int }

let initState = 
    { pos = 0, 0
      heading = 0, 1 }

// initial heading represents facing 0. Simply add the heading for every movement in a direction.
// West is -1, 0; east is 1, 0; south is 0, -1
let north, east, south, west = (0, 1), (1, 0), (0, -1), (-1, 0)

let (|North|East|South|West|) = 
    function 
    | (0, 1) -> North
    | (1, 0) -> East
    | (0, -1) -> South
    | (-1, 0) -> West

let rotate facing dir = 
    match dir, facing with
    | Left, North -> west
    | Left, East -> north
    | Left, South -> east
    | Left, West -> south
    | Right, North -> east
    | Right, East -> south
    | Right, South -> west
    | Right, West -> west

let dir = 
    "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, L4, L4, L3, L4, L3, L5, R2, R4, L2"

let directions = 
    Regex.Matches(dir, @"([LR])|(\d+)") // match L, R, or any digit (not commas or spaces)
    |> Seq.cast<Match> // originally of type MatchCollection but IEnumerable can be cast to a Seq<'a>
    |> Seq.map ((fun m -> m.Value) >> (function 
                | "L" -> Turn Left
                | "R" -> Turn Right
                | n -> Move(int n)))

let newPos state distance = 
    match state with
    | { pos = x, y; heading = n, m } -> { state with pos = x + (n * distance), y + (m * distance) }

let findBaseWithCoords state d = 
    Seq.fold (fun acc x -> 
        match x with
        | Turn Left -> { acc with heading = (rotate acc.heading Left) }
        | Turn Right -> { acc with heading = (rotate acc.heading Right) }
        | Move n -> (newPos acc n)) state d

let findBase d = 
    let finalState = findBaseWithCoords initState d
    let x, y = finalState.pos
    abs (x) + abs (y)

let exOne = 
    seq [ Turn Right
          Move 2
          Turn Left
          Move 3 ]

let exTwo = 
    seq [ Turn Right
          Move 2
          Turn Right
          Move 2
          Turn Right
          Move 2 ]

let exThree = 
    seq [ Turn Right
          Move 5
          Turn Left
          Move 5
          Turn Right
          Move 5
          Turn Right
          Move 3 ]
// gives the wrong answer for directions, 303 instead of 252.
