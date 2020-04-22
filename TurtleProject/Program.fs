open System.IO

type Direction =
    |North
    |East
    |South
    |West

type Vector2 = 
    val X :int
    val Y :int
    new(x, y) = { X = x; Y = y; }

type Action =
    |Move
    |Rotate

type Game = { 
    height:int
    width:int
    startDirection:Direction
    startTile : Vector2
    exitTile : Vector2
    bombs: Vector2 array
    }       

type TurtleState =
    | Normal
    | Exploded
    | Exited

type Turtle =
    val Position : Vector2
    val Direction : Direction
    val State : TurtleState
    new(pos, dir, state) = { Position = pos; Direction = dir; State = state }

let RotateTurtle90Degrees (turtle:Turtle) =
    match turtle.Direction with
    | North -> Turtle(turtle.Position, East, turtle.State)
    | East -> Turtle(turtle.Position, South, turtle.State)
    | South -> Turtle(turtle.Position, West, turtle.State)
    | West -> Turtle(turtle.Position, North, turtle.State)

type Game with 
    member this.Move(state:Turtle) =
        let isExit (position:Vector2) = 
            if this.exitTile.X = position.X && this.exitTile.Y = position.Y then
                true
            else
                false

        let isBomb (position:Vector2) = 
            match this.bombs |> Array.tryFind (fun pos -> pos.X = position.X && pos.Y = position.Y) with
            | Some value -> true
            | None -> false

        let newPosition = 
            match state.Direction with
            | North -> new Vector2(state.Position.X, state.Position.Y - 1)
            | East -> new Vector2(state.Position.X + 1, state.Position.Y)
            | South -> new Vector2(state.Position.X, state.Position.Y + 1)
            | West -> new Vector2(state.Position.X - 1, state.Position.Y)

        //check if move is within bounds, ignore if not
        if(newPosition.X < 0 || newPosition.X >= this.width || newPosition.Y < 0 || newPosition.Y >= this.height) then
            state
        else
            if isBomb newPosition then
                new Turtle(newPosition, state.Direction, Exploded)
            elif isExit newPosition then
                new Turtle(newPosition, state.Direction, Exited)
            else
                new Turtle(newPosition, state.Direction, Normal)

let InitializeTurtle game =
    new Turtle(game.startTile, game.startDirection, Normal)     

[<EntryPoint>]
let main argv =
    let mapFileContent = File.ReadAllText(argv.[0])
    let moveFileContent = File.ReadAllText(argv.[1])

    let gameInstance = Newtonsoft.Json.JsonConvert.DeserializeObject<Game>(mapFileContent)
    let moveData = Newtonsoft.Json.JsonConvert.DeserializeObject<Action array array>(moveFileContent)
      
    let startTurtleState = InitializeTurtle gameInstance

    let rec PerformActionSequence (turtle:Turtle) (actions:Action array) (index:int) =       
        if index = actions.Length then 
            turtle
        else
            let currentMove = actions.[index]
            match currentMove with
            | Move -> 
                let movedTurtle = gameInstance.Move(turtle)
                match movedTurtle.State with
                | Normal -> PerformActionSequence movedTurtle actions (index+1)
                | Exploded -> movedTurtle
                | Exited -> movedTurtle
            | Rotate -> 
                let rotatedTurtle = RotateTurtle90Degrees(turtle)
                PerformActionSequence rotatedTurtle actions (index+1)

    for sequence in moveData do 
        let turtle = PerformActionSequence startTurtleState sequence 0
        match turtle.State with
            | Normal -> printfn("Still in danger!")
            | Exploded -> printfn("Mine hit!")
            | Exited -> printfn("Success!")
    0