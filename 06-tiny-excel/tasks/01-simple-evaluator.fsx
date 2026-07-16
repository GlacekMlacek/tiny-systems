// ----------------------------------------------------------------------------
// 01 - Simple expression evaluator
// ----------------------------------------------------------------------------

// Address represents column and row of a spreadsheet
// This is indexed from 1, so B10 would be (2, 10).
type Address = int * int

// Result of evaluating an expression. A value can be
// primitive (number or string) or an Error if things go wrong.
type Value = 
  | Number of int
  | String of string
  | Error of string
  
// Minimal formula language with just constants, references and
// functions (we will start with just functions named '+' and '*')
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

// A sheet is a mapping from addresses (that contain formulas)
// to expressions. Note that in real Excel, there is a difference
// between cells with data (written as just '123') and cells 
// containing formulas (written as '=123'). We ignore this.
type Sheet = Map<Address, Expr>


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------


let evalBinHelp x y func = 
  match x, y with
  | Number(x), Number(y) -> Number(func x y)
  | _, _ -> Error("unknown value")


let rec eval (sheet:Sheet) expr = 
  // TODO: Implement simple recursive evauator!  
  // * This should support functions "+" and "*" with two arguments. 
  // * All other function calls should evaluate to Error.
  // * Reference to a cell that is not in 'sheet' should evaluate to Error.
  // * To evaluat Reference, get the expression from the cell and evaluate that.
  //   (we will replace this with incremental event-based code later)
  match expr with
    | Const c -> c
    | Reference adr -> 
        let nexpr = Map.tryFind adr sheet
        match nexpr with
        | Some(nexpr) -> eval sheet nexpr
        | None -> Error("reference not found")
    | Function(s, es)-> 
        let mappedList = List.map(fun x -> eval sheet x) es
        match s with
        | "+" -> mappedList |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a + b)) (Number 0)
        | "*" -> mappedList |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a * b)) (Number 1)
        | _ -> Error("function not defined")
    // | _ -> failwith "erm what"
  // failwith "not implemented!"



// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  // let c = int s.[0] - 65
  // let ns = int s.[1..]
  Address(int s.[0] - 65, int s.[1..])
  // TODO: Parse a cell reference such as 'A10' or 'C3'. You can assume that
  // this will always be one letter followed by a number. You can access 
  // characters using "Hello".[2], convert them to integer using 'int' 
  // (int 'A' returns 65, but int "123" parses the string and returns 123).
  // failwith "not implemented!"


let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")])
    addr "A4", Function("+", [Reference(addr "A2"); Reference(addr "A3")])
    addr "A5", Function("+", [Reference(addr "A3"); Reference(addr "A4")])
    addr "A6", Function("+", [Reference(addr "A4"); Reference(addr "A5")])
    addr "A7", Function("+", [Reference(addr "A5"); Reference(addr "A6")])
    addr "A8", Function("+", [Reference(addr "A6"); Reference(addr "A7")])
    addr "A9", Function("+", [Reference(addr "A7"); Reference(addr "A8")])
    addr "A10", Function("+", [Reference(addr "A8"); Reference(addr "A9")]) ]
  |> Map.ofList

printfn "fib: %A" fib

// Should return: Number 13
eval fib (Reference(addr "A8")) |> printfn "%A"

// Should return: Number 21
eval fib (Reference(addr "A9")) |> printfn "%A"

// Should return: Number 34
eval fib (Reference(addr "A10")) |> printfn "%A"

// Should return: Error "Missing value"
eval fib (Reference(addr "A11")) |> printfn "%A"
