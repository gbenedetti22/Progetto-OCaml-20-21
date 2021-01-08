(*progetto ocaml - Set impl.*)

type ide = string;;
type typeSet= Int | Bool | String;;

type exp = CstInt of int
         | CstTrue 
         | CstFalse
         | Den of ide
         | CstString of string
         | Empty of typeSet
         | Singleton of exp
         | Of of typeSet * collection
         | Union of exp * exp
         | Intersection of exp * exp
         | Difference of exp * exp
         | Push of exp * exp
         | RemoveFrom of exp * exp
         | IsEmpty of exp
         | Contains of exp * exp
         | IsSubset of exp * exp
         | MaxOf of exp
         | MinOf of exp
         | For_all of exp * exp
         | Exists of exp * exp
         | Filter of exp * exp
         | Map of exp * exp
         | Sum of exp * exp
         | Sub of exp * exp
         | Times of exp * exp
         | Mod of exp * exp
         | Ifthenelse of exp * exp * exp
         | Eq of exp * exp
         | And of exp * exp
         | Or of exp * exp
         | Not of exp
         | Let of ide * exp * exp
         | Fun of ide * exp
         | Letrec of ide * ide * exp * exp
         | Apply of exp * exp
and collection = Empty | Value of exp * collection
type 'v env = (string * 'v) list;;
type evT = Int of int | Bool of bool | String of string | Set of (evT list) * typeSet | Closure of ide * exp * evT env | RecClosure of ide * ide * exp * evT env | Unbound;;

let emptyEnv  = [ ("", Unbound) ] ;;

let bind (s: evT env) (i:string) (x:evT) = ( i, x ) :: s;;

let rec lookup (s:evT env) (i:string) = match s with
  | [] ->  Unbound
  | (j,v)::sl when j = i -> v
  | _::sl -> lookup sl i;;

let typecheck (x, (y : evT)) = match x with	
  | "int" -> 
      (match y with 
       | Int(u) -> true
       | _ -> false)

  | "bool" -> 
      (match y with 
       | Bool(u) -> true
       | _ -> false)
      
  | "string" -> 
      (match y with 
       | String(u) -> true
       | _ -> false)
      
  | "set" ->
      (match y with
       | Set(l1,_type_) -> true
       | _ -> false)
  | _ -> failwith ("not a valid type");;

let rec getFunType (x) : typeSet =
  match x with
  | CstTrue | CstFalse | Eq(_, _) | And(_, _) | Or(_, _) | Not(_) -> Bool
  | CstString(_) -> String
  | CstInt(_) | Sum(_) | Sub(_) | Times(_) -> Int
  | Ifthenelse(cond, _then_, _else_) -> let ret = getFunType(_then_) in if ret = getFunType(_else_) then ret else failwith("if type error")
  | Let(_, _, fbody) -> getFunType(fbody)
  | Fun(_, fbody) -> getFunType(fbody)
  | _ -> failwith("run-time error");;

let typeof (v : evT)=
  match v with 
  |Int(x) -> (Int : typeSet)
  |String(x) -> (String : typeSet)
  |Bool(x) -> Bool
  |_ -> failwith("runtime-error");;

let int_eq(x,y) =   
  match (typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v = w)
  | (_,_,_,_) -> failwith("run-time error ");;
       
let int_plus(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v + w)
  | (_,_,_,_) -> failwith("run-time error ");;

let int_sub(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v - w)
  | (_,_,_,_) -> failwith("run-time error ");;

let int_times(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v * w)
  | (_,_,_,_) -> failwith("run-time error");;

let int_mod(x, y) = 
  match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v mod w)
  | (_,_,_,_) -> failwith("run-time error ");;


let bool_and(x, y) =
  match (x, y) with
  | (Bool(i), Bool(j)) -> Bool(i && j);
  | (_, _) -> failwith("run-time error");;

let bool_or(x, y) =
  match (x, y) with
  | (Bool(i), Bool(j)) -> Bool(i || j);
  | (_, _) -> failwith("run-time error");;

let bool_not(x) =
  match x with
  | Bool(i) -> Bool(not i);
  | _ -> failwith("nonboolean expression");;

let is_inside ((s : evT),(v : evT))=
  match s with
  |Set([],_type_)->Bool(false)
  |Set(x::xs,_type_)->if typeof v != _type_ then failwith("tipi non corretti") 
      else Bool(List.mem v (x::xs))
  |_ -> failwith("not a set");;

let set_lenght(s : evT)=
  match s with
  |Set(l1,_type_) -> List.length l1
  |_ -> failwith("not a set");;

(*Creazione di insiemi*)
let empty_set (t : typeSet)=Set([],t);;
let singleton (v : evT)=Set([v],typeof v);;

(*Operazioni di base tra insiemi*)
let push ((set : evT),(v : evT)) =
  if not (typecheck("set",set)) then failwith("not a set") else
  if is_inside(set,v)=Bool(true) then set else
    match set with
    |Set ([],_type_) -> if typeof v != _type_ then failwith("tipi non corretti") 
        else Set ([v],_type_)
    |Set (l1,_type_) -> if typeof v != _type_ then failwith("tipi non corretti") 
        else
          Set (v::l1,_type_)
    |_ -> failwith("runtime-error");;

let remove_from ((set : evT),(v : evT))=
  let rec aux set v (set1_temp : evT)=
    match set with
    |Set([],_type_)->set1_temp
    |Set(x::xs,_type_)->let check=_type_=(typeof v)
        in if check && x=v then aux (Set(xs,_type_)) v set1_temp
        else aux (Set(xs,_type_)) v (push (set1_temp,x))
    |_ -> failwith("not a set")
  in aux set v (Set([],typeof v));;

(*Operazioni primitive tra insiemi*)
let is_subset ((set1 : evT),(set2 : evT))=
  if set_lenght(set1)=0 then Bool(true) else
    match (set1,set2) with
    |(Set(l1,_type1_),Set([],_type2_))->Bool(false)
    |(Set(l1,_type1_),Set(l2,_type2_))->let rec aux l1 l2 ret=
                                          match l1 with
                                          |[]->ret
                                          |x::xs->if is_inside ((Set(l2,_type2_)),x)=Bool(true) then aux xs l2 (Bool(true))
                                              else Bool(false)
        in aux l1 l2 (Bool(true))
    |_ -> failwith("not a set");;

let rec union ((set1 : evT),(set2 : evT))=
  if set_lenght(set1) > set_lenght(set2) then union(set2,set1) else 
    match (set1,set2) with
    |(Set(l1,_type1_),Set(l2,_type2_)) -> if _type1_!=_type2_ then failwith("tipi non corretti")
        else (let rec aux l1 s2=
                match l1 with
                |[]->Set([],_type2_)
                |[x]->(push(s2,x))
                |x::xs -> aux xs (push(s2,x))
              in aux l1 (Set(l2,_type2_)))
    |(_,_)->failwith("not a set");;

let rec intersection ((set1 : evT),(set2 : evT))=
  if set_lenght(set1) > set_lenght(set2) then intersection(set2,set1) else
    match (set1,set2) with
    |(Set(l1,_type1_),Set(l2,_type2_))-> if _type1_!=_type2_ then failwith("tipi non corretti") else 
          let rec aux l1 l2 ret=
            match l1 with
            |[]->Set([],_type1_)
            |[x]->if is_inside ((Set(l2,_type1_)),x)=Bool(true) then Set(x::ret,_type1_)
                else Set(ret,_type1_)
            |x::xs -> if is_inside ((Set(l2,_type1_)),x)=Bool(true) then aux xs l2 (x::ret)
                else aux xs l2 ret
          in aux l1 l2 []
    |(_,_) -> failwith("not a set");;

let rec difference ((set1 : evT),(set2 : evT))=
  match (set1,set2) with
  |(Set([],t1),set)->empty_set t1
  |(set,Set([],t2))->set
  |(Set(l1,_type1_),Set(l2,_type2_))-> if _type1_!=_type2_ then failwith("tipi non corretti") else 
        let rec aux l1 l2 ret=
          match l1 with
          |[]->empty_set _type1_
          |[x]->if is_inside ((Set(l2,_type1_)),x)=Bool(false) then Set(x::ret,_type1_)
              else Set(ret,_type1_)
          |x::xs -> if is_inside ((Set(l2,_type1_)),x)=Bool(false) then aux xs l2 (x::ret)
              else aux xs l2 ret
        in aux l1 l2 []
  |(_,_) -> failwith("not a set");;

(*Operazioni aggiuntive*)

let is_empty ((s : evT))=
  match s with
  |Set([],_type_)->Bool(true)
  |Set(l1,_type_)->Bool(false)
  |_ -> failwith("not a set");;

let maxOf ((s : evT))=
  if set_lenght(s)==1 then 
    match s with 
    |Set([x],_type_)->x
    |_ -> failwith("not a set")
  else
    let rec aux s max skip=
      match s with
      |Set([],_type_)->Unbound
      |Set([x],_type_)->if x>max then x else max
      |Set(x::xs,_type_)->if skip then aux (Set(xs,_type_)) x false
          else if x>max then aux (Set(xs,_type_)) x skip
          else aux (Set(xs,_type_)) max skip
      |_ -> failwith("not a set") 
    in aux s (Unbound) true;;

let minOf ((s : evT))=
  if set_lenght(s)==1 then 
    match s with 
    |Set([x],_type_)->x
    |_ -> failwith("not a set")
  else
    let rec aux s min skip=
      match s with
      |Set([],_type_)->Unbound
      |Set([x],_type_)->if x<min then x else min
      |Set(x::xs,_type_)->if skip then aux (Set(xs,_type_)) x false
          else if x<min then aux (Set(xs,_type_)) x skip
          else aux (Set(xs,_type_)) min skip
      |_ -> failwith("not a set") 
    in aux s (Unbound) true;;

let rec eval  (e:exp) (s:evT env) = match e with
  | CstInt(n) -> Int(n)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | CstString(s) -> String(s)
  | Empty(i) -> empty_set i
  | Singleton(v) -> singleton (eval v s)
  | Of(t, values) -> let rec getSetFrom v s1=
                       match v with
                       |Value(x,Empty) -> if typeof (eval x s) != t then failwith("tipi non corretti") 
                           else (push(s1,(eval x s)))
                       |Value(x,Value(y,z))->if typeof (eval x s) != t then failwith("tipi non corretti")
                           else getSetFrom (Value(y,z)) (push(s1,(eval x s)))
                       |Empty -> s1
      in getSetFrom values (empty_set(t))
  | Union(e1, e2) -> union((eval e1 s),(eval e2 s))
  | Intersection(e1, e2) -> intersection((eval e1 s),(eval e2 s))
  | Difference(e1, e2) -> difference((eval e1 s),(eval e2 s))
  | Push(e1, e2) -> push((eval e1 s),(eval e2 s))
  | RemoveFrom(e1, e2) -> remove_from((eval e1 s), (eval e2 s))
  | IsEmpty(e) -> is_empty((eval e s))
  | Contains(e1, e2) -> is_inside((eval e1 s), (eval e2 s))
  | IsSubset(e1, e2) -> is_subset((eval e1 s), (eval e2 s))
  | MaxOf(e) -> maxOf((eval e s))
  | MinOf(e) -> minOf((eval e s))
  | For_all(f, e) -> for_all((eval f s), (eval e s))
  | Exists(f, e) -> exist((eval f s), (eval e s))
  | Filter(f, e) -> filter((eval f s), (eval e s))
  | Map(f, e) -> map((eval f s), (eval e s))
  | Eq(e1, e2) -> int_eq((eval e1 s), (eval e2 s))
  | And(e1, e2) -> bool_and((eval e1 s), (eval e2 s))
  | Or(e1, e2) -> bool_or((eval e1 s), (eval e2 s))
  | Not(e) -> bool_not(eval e s)
  | Times(e1,e2) -> int_times((eval e1 s), (eval e2 s))
  | Sum(e1, e2) -> int_plus((eval e1 s), (eval e2 s))
  | Sub(e1, e2) -> int_sub((eval e1 s), (eval e2 s))
  | Mod(e1, e2) -> int_mod((eval e1 s), (eval e2 s))
  | Ifthenelse(e1,e2,e3) -> let g = eval e1 s in
      (match (typecheck("bool", g), g, getFunType(e2)=getFunType(e3)) with
       | (true, Bool(true), true) -> eval e2 s
       | (true, Bool(false), true) -> eval e3 s
       | (_, _, _) -> failwith ("nonboolean guard"))
  | Den(i) -> lookup s i
  | Let(i, e, ebody) -> eval ebody (bind s i (eval e s))
  | Fun(arg, ebody) -> Closure(arg,ebody,s)
  | Letrec(f, arg, fBody, letBody) -> 
      let benv = bind (s) (f) (RecClosure(f, arg, fBody,s)) in eval letBody benv
  | Apply(eF, eArg) ->
      let fclosure = eval eF s in 
      (match fclosure with 
       | Closure(arg, fbody, fDecEnv) ->
           let aVal = eval eArg s in
           let aenv = bind fDecEnv arg aVal in 
           eval fbody aenv
       | RecClosure(f, arg, fbody, fDecEnv) ->
           let aVal = eval eArg s in
           let rEnv = bind fDecEnv f fclosure in
           let aenv = bind rEnv arg aVal in 
           eval fbody aenv
       | _ -> failwith("non functional value"))
and for_all (pred,set)=
  match (pred,set) with
  |(Closure(func_var,fbody,env),Set(l1,_type_)) ->let rec aux(var,body, env, l1, ret)=
                                                    match l1 with
                                                    |[]->ret
                                                    | x::xs -> let varEnv= bind env var x in let ret=eval fbody varEnv in if ret=Bool(true) then aux (var,body,env,xs, ret) 
                                                        else if ret=Bool(false) then ret else failwith("not a predicate")
      in aux (func_var,fbody,emptyEnv,l1,(Bool(false)))
  |(_,_) -> failwith("runtime-error")

and exist (pred,set)=
  match (pred,set) with
  |(Closure(func_var,fbody,env),Set(l1,_type_)) ->let rec aux(var,body, env, l1, ret)=
                                                    match l1 with
                                                    |[]->ret
                                                    | x::xs -> let varEnv= bind env var x in let ret=eval fbody varEnv in if ret=Bool(false) then aux (var,body,env,xs, ret) 
                                                        else if ret=Bool(true) then ret else failwith("not a predicate")
      in aux (func_var,fbody,emptyEnv,l1,(Bool(false)))
  |(_,_) -> failwith("runtime-error")

and filter (pred,set)=
  match (pred,set) with
  |(Closure(func_var,fbody,env),Set(l1,_type_)) ->let rec aux(var,body, env, l1, ret_set, _type_)=
                                                    match l1 with
                                                    |[]-> Set(ret_set,_type_)
                                                    | x::xs -> let varEnv= bind env var x 
                                                        in let ret=eval fbody varEnv 
                                                        in if ret=Bool(true) then aux (var,body,env,xs, (x::ret_set),_type_) 
                                                        else if ret=Bool(false) then aux (var,body,env,xs, ret_set,_type_)
                                                        else failwith("not a predicate")
      in aux (func_var,fbody,emptyEnv,l1,[],_type_)
  |(_,_) -> failwith("runtime-error")

and map (func,set)=
  match (func,set) with
  |(Closure(func_var,fbody,env),Set(l1,_type_)) ->let rec aux(var,body, env, l1, ret_set,_type_)=
                                                    match l1 with
                                                    |[]->ret_set
                                                    | x::xs -> let varEnv= bind env var x 
                                                        in aux (var,body,env,xs, (push(ret_set,(eval fbody varEnv))),_type_)
      in aux (func_var,fbody,emptyEnv,l1,empty_set(getFunType(fbody)),_type_)
  |(_,_) -> failwith("runtime-error");;