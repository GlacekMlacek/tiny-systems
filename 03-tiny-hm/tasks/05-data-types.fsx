// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with
  | TyVariable s -> s = vcheck
  | TyList t -> occursCheck vcheck t
  | TyFunction(t1, t2) -> (occursCheck vcheck t1 ) || (occursCheck vcheck t2)
  | TyTuple(t1, t2) ->  (occursCheck vcheck t1 ) || (occursCheck vcheck t2)
  | _ -> false

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match t1 with
  | TyVariable v -> if Map.containsKey v subst then substType subst subst.[v] else t1
  | TyList t -> TyList(substType subst t)
  | TyFunction(t1, t2) -> TyFunction(substType subst t1, substType subst t2)
  | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)
  | _ -> t1

let substConstrs subst cs = 
  List.map (fun (t1, t2) -> (substType subst t1), (substType subst t2)) cs
 
let rec solve cs =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs -> solve ((t1, t2)::cs)
  | (TyVariable v, n)::cs
  | (n, TyVariable v)::cs ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let cs = substConstrs (Map.ofList [(v, n)]) cs
      let subst = solve cs
      let n = substType (Map.ofList subst) n
      (v, n)::subst
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | _ -> printfn "%A" cs; failwith "cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> TyNumber, []
  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op
  | Variable v ->  if ctx.ContainsKey v then ctx[v], [] else failwith "var not found"
  | If(econd, etrue, efalse) ->
      let tcond, scond = generate ctx econd
      let ttrue, strue = generate ctx etrue
      let tfalse, sfalse = generate ctx efalse
      ttrue, scond @ strue @ sfalse @ [tcond, TyBool; ttrue, tfalse]
  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (Map.add v t1 ctx) e2
      t2, s1 @ s2
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let t, s = generate (Map.add v targ ctx) e
      TyFunction(targ, t), s
  | Application(e1, e2) -> 
      let t = newTyVariable()
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      t, [TyFunction(t2, t), t1] @ s1 @ s2 

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      let ttrue = newTyVariable()
      let tfalse = newTyVariable()
      let t, s = generate ctx e
      (if b then ttrue else tfalse), s @ [t, TyTuple(ttrue, tfalse)]

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer |> printfn "%A => int"
TupleGet(false, etup) |> infer |> printfn "%A => int"

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer |> printfn "%A => ('a * ('b * 'c) -> 'a * 'b)"

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
// TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer |> printfn "%A -> fails"


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer |> printfn "%A -> ('b -> (('b -> 'a) -> ('b * 'a)))"
