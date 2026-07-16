// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  // TODO: Check if variable 'v' appears anywhere inside 'n'
  match n with
  | Zero -> false 
  | Succ x -> occursCheck v x
  | Variable s -> s = v

let rec substite (v:string) (subst:Number) (n:Number) =
  // TODO: Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Zero -> Zero
  | Succ x -> Succ (substite v subst x)
  | Variable s -> if s = v then subst else Variable s

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  // TODO: Substitute 'v' for 'subst' (use 'substitute') in 
  // all numbers in all the constraints in 'constraints'
  // HINT: You can use 'List.map' to implement this.
  List.map (fun (n1, n2) -> (substite v subst n1), (substite v subst n2)) constraints

let substituteAll (subst:list<string * Number>) (n:Number) =
  // TODO: Perform all substitutions specified  in 'subst' on the number 'n'
  // HINT: You can use 'List.fold' to implement this. Fold has a type:
  //   ('State -> 'T -> 'State) -> 'State -> List<'T> -> 'State
  // In this case, 'State will be the Number on which we want to apply 
  // the substitutions and List<'T> will be a list of substitutions.
  List.fold (fun acc (v, subst) -> substite v subst acc) n subst

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints
  | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
[ Succ(Variable "x"), Succ(Zero) ] |> printfn "%A <-> x = zero"

// Should fail: S(Z) <> Z
// solve 
//   [ Succ(Succ(Zero)), Succ(Zero) ] |> printfn "%A -> fails"

// Should fail: No 'x' such that S(S(x)) = S(Z)
// solve 
//   [ Succ(Succ(Variable "x")), Succ(Zero) ] |> printfn "%A -> fails"

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ] |> printfn "%A <-> s(Z)"

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ] |> printfn "%A <-> s(s(z))"

// Not done: Need occurs check
// solve
//   [ Variable "x", Succ(Variable "x") ] |> printfn "%A -> fail" // fails
