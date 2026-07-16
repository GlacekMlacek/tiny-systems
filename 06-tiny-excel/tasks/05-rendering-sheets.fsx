// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
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

let rec collectReferences expr = 
  match expr with
  | Const _ -> []
  | Reference adr -> [adr]
  | Function(_, es) -> List.collect(fun e -> collectReferences e) es

let makeNode (sheet:LiveSheet) expr = 
  let adrs = collectReferences expr
  let nd = {Value = eval sheet expr; Expr = expr; Updated = new Event<unit>()}
  for adr in adrs do
    sheet.[adr].Updated.Publish.Add(fun () ->
        nd.Value <- eval sheet expr
        nd.Updated.Trigger())
  nd

let updateNode addr (sheet:LiveSheet) expr = 
  sheet.[addr].Value <- eval sheet expr
  sheet.[addr].Expr <- expr
  sheet.[addr].Updated.Trigger()

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


let valueGet (value : Value) : string =
  match value with
  | Number(n) -> string n
  | String(s) -> s
  | Error(e)  -> sprintf "#%s" e

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  "<span class='e'>not implemented</span>"
  
let display (sheet:LiveSheet) = 
  // TODO: Find the greates row and column index
  let mutable maxCol = -1
  let mutable maxRow = -1

  for idk in sheet do
    let (row, col) = idk.Key
    maxCol <- if col > maxCol then col else maxCol
    maxRow <- if row > maxRow then row else maxRow

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>""")

  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do 
    wr.Write(sprintf "<th> %d </th>" col)
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do 
    // wr.Write($"<tr><th> ?? </th>")
    wr.Write(sprintf "<tr><th> %d </th>" row)
    for col in 1 .. maxCol do 
      // let nexpr = Map.tryFind adr sheet
      match (Map.tryFind (row, col) sheet) with
      | Some(v) -> wr.Write(sprintf "<td> %s </td>" (valueGet v.Value))
      | None -> wr.Write("<td>    </td>")
      // wr.Write("<td> !! </td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  Address(int s.[0] - 64, int s.[1..])

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4! 
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
    addr "C2", Function("+", [
        Function("/",  [
          Function("*", [ Reference(addr "B2"); Const(Number(9)) ])
          Const(Number(5)) ])
        Const(Number(32))] ) ]
  |> makeSheet
display tempConv
