// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with
  | TyVariable s -> s = vcheck
  | TyList t -> occursCheck vcheck t
  | _ -> false
 
let rec substType (subst:Map<string, Type>) ty = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyVariable v -> if Map.containsKey v subst then substType subst subst.[v] else ty
  | TyList t -> TyList(substType subst t)
  | _ -> ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  List.map (fun (t1, t2) -> (substType subst t1), (substType subst t2)) cs
 

let rec solve cs =
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
  | _ -> printfn "%A" cs; failwith "cannot be solved"
  // TODO: Fill in the remaining cases! You can closely follow the
  // example from task 1 - the logic here is exactly the same.


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ] |> printfn "%A -> a=num b=list<num>"

// Cannot be solved (list<'a> <> bool)
// solve  
//   [ TyList(TyVariable("a")), TyVariable("b")
//     TyVariable("b"), TyBool ] |> printfn "%A -> fail" // fails

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ] |> printfn "%A -> a=num b=list<num>"

// Cannot be solved ('a <> list<'a>)
// solve  
//   [ TyList(TyVariable("a")), TyVariable("a") ] |> printfn "%A -> fail" // fails
