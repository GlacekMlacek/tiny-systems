// ----------------------------------------------------------------------------
// 02 - "Drag down" formula expanding
// ----------------------------------------------------------------------------

type Address = int * int

type Value = 
  | Number of int
  | String of string
  | Error of string
  
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type Sheet = Map<Address, Expr>

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  // TODO: Replace references in expression 'srcExpr' in a way that 
  // corresponds to moving the expression from address (srcRow, srcCol)
  // to address (tgtRow, tgtCol). So for example, if a formula 'A1+A2' is
  // moved from 'A3' to 'B10' then it should change to 'B8+B9' (address
  // is incremented by column difference 1 and row difference 7)
  let diffCol = tgtCol - srcCol
  let diffRow = tgtRow - srcRow
  match srcExpr with
  | Const(v) -> Const(v)
  | Reference(col, row) -> Reference(Address(col + diffCol,row + diffRow))
  | Function(s, es) -> Function(s, List.map(fun x -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) x) es)
  // failwith "not implemented!"


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:Sheet) : Sheet = 
  // TODO: Expand formula at address (srcCol, srcRow) to all the cells 
  // between itself and target cell at address (tgtCol, tgtRow) and
  // add the new formulas to the given sheet, returning the new sheet.
  // 
  // HINT: You can use list comprehension with 'for .. in .. do' and 
  // 'yield' or you can use 'List.init'. The comprehension is nicer, 
  // but you need to figure out the right syntax! Once you generate
  // new cells, you can add them to the Map using List.fold (with the 
  // sheet as the current state, updated in each step using Map.add).
  // let ah = printfn "%A" sheet
  let expr = Map.tryFind (Address (srcCol, srcRow)) sheet
  match expr with
  | Some(formula) ->
        let newSheet = [ yield! Map.toSeq sheet
                         for col in seq {srcCol .. tgtCol} do
                            for row in seq {srcRow .. tgtRow} do
                                yield Address(col, row), relocateReferences (srcCol, srcRow) (col, row) formula ]
        newSheet |> Map.ofList
  | None -> Map.empty
  // failwith "not implemented!"


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------

let evalBinHelp x y func = 
  match x, y with
  | Number(x), Number(y) -> Number(func x y)
  | _, _ -> Error("unknown value")


let rec eval (sheet:Sheet) expr = 
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


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  Address(int s.[0] - 65, int s.[1..])


let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> Map.ofList
  |> expand (addr "A3") (addr "A10")

printfn "fib: %A" fib

// Should return: Number 13
eval fib (Reference(addr "A8")) |> printfn "A8(13): %A"

// Should return: Number 21
eval fib (Reference(addr "A9")) |> printfn "A9(21): %A"

// Should return: Number 34
eval fib (Reference(addr "A10")) |> printfn "A10(34): %A"

// Should return: Error "Missing value"
eval fib (Reference(addr "A11")) |> printfn "A11(err): %A"


// Column 'A' is a sequence of numbers increasing by 1
// Column 'B' is the factorial of the corresponding number
// i.e.: Bn = An * B(n-1) = An * A(n-1)!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> Map.ofList
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// A6 should be 5, B6 should be 120
eval fac (Reference(addr "A6")) |> printfn "%A"
eval fac (Reference(addr "B6")) |> printfn "%A"

// A11 should be 10, B11 should be 3628800
eval fac (Reference(addr "A11")) |> printfn "%A"  
eval fac (Reference(addr "B11")) |> printfn "%A"  
