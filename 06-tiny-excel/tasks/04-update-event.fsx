// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
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

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    // NOTE: Added event that will be triggered when the 
    // expression and value of the node is changed.
    Updated : Event<unit> } 

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let evalBinHelp x y func = 
  match x, y with
  | Number(x), Number(y) -> Number(func x y)
  | _, _ -> Error("unknown value")


let rec eval (sheet:LiveSheet) expr = 
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
        | "-" -> mappedList.[1..] |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a - b)) mappedList.[0]
        | "*" -> mappedList |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a * b)) (Number 1)
        | "/" -> mappedList.[1..] |> List.fold(fun acc x -> evalBinHelp acc x (fun a b -> a / b)) mappedList.[0]
        | _ -> Error("function not defined")
  

let rec collectReferences (expr:Expr) : Address list = 
  // TODO: Collect the addresses of all references that appear in the 
  // expression 'expr'. This needs to call itself recursively for all
  // arguments of 'Function' and concatenate the returned lists.
  // HINT: This looks nice if you use 'List.collect'.
  match expr with
  | Const _ -> []
  | Reference adr -> [adr]
  | Function(_, es) -> List.collect(fun e -> collectReferences e) es
  // failwith "not implemented"


let makeNode (sheet:LiveSheet) expr = 
  // TODO: Add handling of 'Update' events!
  //
  // * When creating a node, we need to create a new event and 
  //   set it as the 'Updated' event of the returned node.
  // * We then need to define 'update' function that will be triggered
  //   when any of the cells on which this one depends change. In the 
  //   function, re-evaluate the formula, set the new value and trigger
  //   our Updated event to notify other cells.
  // * Before returning, use 'collectReferences' to find all cells on which
  //   this one depends and add 'update' as the handler of their 
  //   'Updated' event
  //
  // let nevent = new Event<unit>()
  let adrs = collectReferences expr
  let nd = {Value = eval sheet expr; Expr = expr; Updated = new Event<unit>()}
  for adr in adrs do
    sheet.[adr].Updated.Publish.Add(fun () ->
        nd.Value <- eval sheet expr
        nd.Updated.Trigger())
  nd


let updateNode addr (sheet:LiveSheet) expr = 
  // TODO: For now, we ignore the fact that the new expression may have
  // different set of references than the one we are replacing. 
  // So, we can just get the node, set the new expression and value
  // and trigger the Updated event!
  // failwith "not implemented"
  sheet.[addr].Value <- eval sheet expr
  sheet.[addr].Expr <- expr
  sheet.[addr].Updated.Trigger()


// let makeSheet list = 
let makeSheet (list:(Address * Expr) list) : LiveSheet = 
  List.fold(fun acc (adr, expr) -> acc.Add (adr, (makeNode acc expr))) Map.empty list

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

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
let tempConv = 
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0) 
    addr "C1", 
      Function("/", [ 
        Function("*", [ 
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ]) 
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0) 
    // TODO: Add formula for Celsius to Fahrenheit conversion to 'C2'
    // addr "C2", Const(Error "not implemented") ]
    addr "C2", Function("+", [
        Function("/",  [
          Function("*", [ Reference(addr "B2"); Const(Number(9)) ])
          Const(Number(5)) ])
        Const(Number(32))] ) ]
  |> makeSheet


// tempConv |> printfn "tempConv: %A"

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1")) |> printfn "-17: %A"
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1")) |> printfn "0: %A"
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1")) |> printfn "37: %A"

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2")) |> printfn "32: %A"
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2")) |> printfn "212: %A"
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2")) |> printfn "100: %A"

