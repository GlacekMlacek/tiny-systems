// ----------------------------------------------------------------------------
// 03 - Reactive event-based structure
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

// Node in a dependency graph that represents a spreadsheet cell
// For each cell, we store the original expression, evalauted value
// and an event to be triggered when the value changes.
type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr } 

// A live spreadsheet is a mapping from addresses to graph nodes
type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let evalBinHelp x y func = 
  match x, y with
  | Number(x), Number(y) -> Number(func x y)
  | _, _ -> Error("unknown value")


let rec eval (sheet:LiveSheet) expr = 
  // TODO: Modify the 'Reference' case. Instead of recursively calling 
  // 'eval', this should now locate the graph node and return the 'Value'
  // that is stored in the graph node!
  match expr with
    | Const c -> c
    | Reference adr -> 
        let nexpr = Map.tryFind adr sheet
        match nexpr with
        | Some(nd) -> nd.Value
        | None -> Error("reference not found")
    | Function(s, es)-> 
        let mappedList = List.map(fun x -> eval sheet x) es
        match s with
        | "+" -> mappedList |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a + b)) (Number 0)
        | "*" -> mappedList |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a * b)) (Number 1)
        | _ -> Error("function not defined")


let makeNode (sheet:LiveSheet) (expr:Expr) : CellNode = 
  // TODO: Create a dependency graph node. In this step, we just want
  // to get the same functionality as before (i.e., no event handling)
  // so evaluate the expression, store it and return the node.
  {Value = eval sheet expr; Expr = expr}
  // failwith "not implemented"


let makeSheet (list:(Address * Expr) list) : LiveSheet = 
  // TODO: Previously, we could turn a list of mappings into a sheet just
  // by using Map.ofList. This no longer works, because we need to add
  // cells one by one (we should make sure that all cells on which the new one
  // depends are already in the sheet, but we assume examples are given
  // in a correct order). To do this, use 'List.fold' and 'makeNode'. 
  List.fold(fun acc (adr, expr) -> acc.Add (adr, (makeNode acc expr))) Map.empty list
  // failwith "not implemented"


// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  let diffCol = tgtCol - srcCol
  let diffRow = tgtRow - srcRow
  match srcExpr with
  | Const(v) -> Const(v)
  | Reference(col, row) -> Reference(Address(col + diffCol,row + diffRow))
  | Function(s, es) -> Function(s, List.map(fun x -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) x) es)


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet = 
  // TODO: This needs to call 'makeNode' and add the resulting node, 
  // instead of just adding the expression to the map as is.
  let expr = Map.tryFind (Address (srcCol, srcRow)) sheet
  match expr with
  | Some(nd) ->
        let newSheet = [ 
                         for col in seq {srcCol .. tgtCol} do
                            for row in seq {srcRow .. tgtRow} do
                                yield Address(col, row), relocateReferences (srcCol, srcRow) (col, row) nd.Expr ]
        // newSheet |> Map.ofList
        List.fold(fun acc (adr, expr) -> acc.Add(adr, (makeNode acc expr))) sheet newSheet
  | None -> Map.empty


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  Address(int s.[0] - 65, int s.[1..])

let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8")) |> printfn "A8(13): %A"
// Should return: Number 21
eval fib (Reference(addr "A9")) |> printfn "A9(21): %A"
// Should return: Number 34
eval fib (Reference(addr "A10")) |> printfn "A10(34): %A"
// Should return: Error "Missing value"
eval fib (Reference(addr "A11")) |> printfn "A11(err): %A"


let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// Should return: Number 5
eval fac (Reference(addr "A6")) |> printfn "A6(5): %A"
// Should return: Number 120
eval fac (Reference(addr "B6")) |> printfn "B6(120): %A"

// Should return: Number 10
eval fac (Reference(addr "A11")) |> printfn "A11(10): %A"
// Should return: Number 3628800
eval fac (Reference(addr "B11")) |> printfn "B11(3628800): %A"

