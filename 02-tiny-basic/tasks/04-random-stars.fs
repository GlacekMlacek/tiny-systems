// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
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
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    // TODO: You will need to include random number generator in the state!
    Random : System.Random
    }

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

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
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
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
  match expr with
  | Const(v) -> v
  | Function(op, args) ->
      let eargs = List.map (fun e -> evalExpression e state) args
      match op with
      | "RND" -> NumberValue (state.Random.Next(0, getNumberValue (List.head eargs)))
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
  | Print(expr) ->
      printValue (evalExpression expr state)
      runNextLine state line
  | Assign(s, e) -> runNextLine { state with Variables = Map.add s (evalExpression e state) state.Variables } line
  | If(e, c) ->
      match evalExpression e state with
      | BoolValue b -> if b then runCommand state (line, c) else runNextLine state line
      | _ -> failwith "if was expecting a bool"
  
  // TODO: Implement two commands for screen manipulation
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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
