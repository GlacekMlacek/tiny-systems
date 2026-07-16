// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
      | ValNum n ->
          match op with
          | "-" -> ValNum(-n)
          | _ -> failwith "unsupported unary operator"
      | _ -> failwith "Uunsopported val type in unary"
  | If(p, et, ef) ->
      let pe = evaluate ctx p
      match pe with
      | ValNum b ->
          match b with
            | 1 -> evaluate ctx et
            | _ -> evaluate ctx ef
      | _ -> failwith "unsupported val type in if"
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)
  | Application(e1, e2) ->
      match evaluate ctx e1 with
      | ValClosure(v, e, nctx) -> evaluate (Map.add v (evaluate ctx e2) nctx) e
      | _ -> failwith "was expecting a closure"
  | Let(v, e1, e2) ->
    let arg = evaluate ctx e1
    let nctx = Map.add v arg ctx
    evaluate nctx e2
  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)
  | TupleGet(b, e) ->
      let ee = evaluate ctx e
      match b, ee with
          | true, ValTuple(t, _) -> t
          | false, ValTuple(_, f) -> f
          | _, _ -> failwith "expecting tuple"

  | Match(e, v, e1, e2) ->
      let ee = evaluate ctx e
      match ee with
      | ValCase(true, ev) -> evaluate (Map.add v ev ctx) e1
      | ValCase(false, ev) -> evaluate (Map.add v ev ctx) e2
      | _ -> failwith "expecting case"
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!

  | Case(b, e) ->
      ValCase(b, evaluate ctx e)
      // TODO: Create a union value.

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1 |> printfn "%A"

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2 |> printfn "%A"

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3 |> printfn "%A"
