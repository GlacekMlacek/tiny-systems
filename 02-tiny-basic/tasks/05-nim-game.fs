// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC
open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue(s) -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

let getLine state line =
  match List.tryFind (fun (n, _) -> n = line) state.Program with
  | Some(newLine) -> newLine
  | None -> failwith "line does not exist"

let addLine state (line, cmd) = 
    { state with Program = List.sortBy (fun (n, _) -> n) ((line, cmd) :: (List.filter (fun (n, _) -> n <> line) state.Program)) }

let getNumberValue value =
  match value with
  | NumberValue n -> n
  | _ -> failwith "value is not number"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args = 
  match args with 
  | [BoolValue a; BoolValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryNumOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression expr state = 
  // TODO: We need an extra function 'MIN' that returns the smaller of
  // the two given numbers (in F#, the function 'min' does exactly this.)
  match expr with
  | Const(v) -> v
  | Function(op, args) ->
      let eargs = List.map (fun e -> evalExpression e state) args
      match op with
      | "RND" -> NumberValue (state.Random.Next(0, getNumberValue (List.head eargs)))
      | "MIN" -> NumberValue (List.min (List.map (fun e -> getNumberValue e) eargs))
      | "=" -> binaryRelOp (=) eargs
      | ">" -> binaryRelOp (>) eargs
      | "<" -> binaryRelOp (<) eargs
      | "-" -> binaryNumOp (-) eargs
      | "||" -> binaryBoolOp (||) eargs
      | _ -> failwith "unsupported func"
  | Variable v ->
      match Map.tryFind v state.Variables with
      | Some v -> v
      | None -> failwith "variable not found"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      runCommand state (getLine state line)
  | Print(exprs) ->
      List.map printValue (List.map (fun expr -> evalExpression expr state) exprs) |> ignore
      runNextLine state line
  | Assign(s, e) -> runNextLine { state with Variables = Map.add s (evalExpression e state) state.Variables } line
  | If(e, c) ->
      match evalExpression e state with
      | BoolValue b -> if b then runCommand state (line, c) else runNextLine state line
      | _ -> failwith "if was expecting a bool"
  | Clear ->
      Console.Clear()
      runNextLine state line
  | Poke(ex, ey, ee) ->
      let x = evalExpression ex state
      let y = evalExpression ey state
      let c = evalExpression ee state
      match x, y, c with
      | (NumberValue x, NumberValue y, StringValue e) ->
          Console.CursorLeft <- x
          Console.CursorTop <- y
          Console.Write(e)
      | _ -> failwith "Poke was expecting int, int, string"
      runNextLine state line

  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input name ->
      match Int32.TryParse (Console.ReadLine()) with
      | true, x -> runCommand state (line, (Assign(name, Const(NumberValue x))))
      | _ -> runCommand state (line, (Input name))
  | Stop -> state

and runNextLine state line =
  match List.tryFind (fun (n, _) -> n > line) state.Program with
  | Some(newLine) -> runCommand state newLine
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some(ln) -> addLine state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)

let runInputs state cmds =
  List.fold (fun acc cmd -> runInput acc cmd) state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
