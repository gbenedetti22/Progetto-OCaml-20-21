(*progetto ocaml - Set impl.*)

type ide = string;;

type exp = CstInt of int
         | CstTrue 
         | CstFalse
         | Den of ide
         | CstString of string
         | Empty of tval
         | Singleton of exp
         | Of of tval * collection
         | Union of exp * exp
         | Intersection of exp * exp
         | Difference of exp * exp
         | Push of exp * exp
         | RemoveFrom of exp * exp
         | IsEmpty of exp
         | Contains of exp * exp
         | Subset of exp * exp
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
         | Fun of ide * tval * exp
         | Letrec of ide * ide * tval * tval * exp * exp
         | Apply of exp * exp
and collection = Empty | Value of exp * collection
and tval= TInt | TBool | TString | FunT of tval * tval | Null;;
type 'v env = (string * 'v) list;;;;

let tEmptyEnv  = [ ("", Null) ] ;;

let bind_s (s: tval env) (i:string) (x:tval) = ( i, x ) :: s;;

let rec lookup_s (s:tval env) (i:string) = match s with
  | [] ->  Null
  | (j,v)::sl when j = i -> v
  | _::sl -> lookup_s sl i;;

let rec teval (e:exp) (s:tval env) : tval=
  let check_set (s1)=
    match s1 with
    | Singleton(_) | Of(_) | Empty(_) -> TBool
    | _ -> Null in
  match e with
  | CstInt(n) -> TInt
  | CstTrue -> TBool
  | CstFalse -> TBool
  | CstString(s) -> TString
  | Empty(i) -> i
  | Singleton(v) -> teval v s
  | Of(t, values) ->
      let rec check v=
        match v with
        |Value(x,Empty) -> if t != teval x s then failwith("tipi non corretti")
            else t
        |Value(x,y)->if t != teval x s then failwith("type-error") else check y
        |Empty -> Null
      in check values
  | Union(e1, e2) | Intersection(e1, e2) | Difference(e1, e2) | Subset(e1, e2)-> 
      if check_set(e1) != TBool && check_set(e2) != TBool then failwith("not a set") else
        let a1= teval e1 s in
        let a2=teval e2 s in
        if a1=a2 then a1 else failwith("type error") 
  | Push(s1,v) | RemoveFrom(s1, v) -> 
      if check_set(s1)!=TBool then failwith("not a set") else
        let a1= teval s1 s in
        let a2=teval v s in
        if a1=a2 then a1 else failwith("type error") 
  | MaxOf(e) | MinOf(e)-> 
      if check_set(e)=TBool then teval e s else failwith("not a set")
  | IsEmpty(s1) -> if check_set(s1) != TBool then failwith("not a set")
      else TBool
  | Contains(s1,v) -> if check_set(s1) != TBool then failwith("not a set") else
        let t1=teval s1 s in
        let t2=teval v s in if t1=t2 then TBool else failwith("type error")
  | Eq(c1, c2) | And(c1, c2) | Or(c1, c2) -> 
      let g1=teval c1 s in
      let g2= teval c2 s in
      if g1=g2 then TBool else failwith("eq boolean error")
  | Not(e) ->if check_set(e) = TBool then failwith("set is not a boolean value") else
        let t1=teval e s 
        in if t1 != TBool then failwith("not a boolean value") else t1
  | Sum(a,b) | Sub(a,b) | Times(a,b) | Mod(a,b)->( 
      if check_set(a) = TBool && check_set(b) = TBool then failwith("runtime-error") else 
        let t1=teval a s in
        let t2=teval b s in
        match (t1,t2) with
        |(TInt,TInt)-> TInt
        |_ -> failwith("not valid type"))
  | Ifthenelse(cond,_then_,_else_) ->
      if check_set(cond) = TBool then failwith("if statement error ") else (*per il set booleano*)
        let c1=teval cond s in
        let g1=teval _then_ s in
        let g2=teval _else_ s in 
        if g1=g2 && c1=TBool then g1 else failwith("type mismatch")
  | Den(i) -> lookup_s s i
  | Let(i, e, ebody) -> teval ebody (bind_s s i (teval e s)) 
  | Fun(i, t1, e) -> 
      let tenv1 = bind_s s i t1 in
      let t2 = teval e tenv1 in FunT(t1,t2)
  | Letrec(f, arg, t1, t2, fbody, letbody) ->
      let tenv1=bind_s s f (FunT(t1,t2)) in
      let tenv2=bind_s tenv1 arg t1 in
      let t1=teval fbody tenv2 in
      let t2=teval letbody tenv2 in
      if t1=t2 then t1 else failwith("type-error")
  | Apply (e1, e2) ->
      let f = teval e1 s in
      ( match f with
        |FunT(t1,t2) -> if ((teval e2 s) = t1 )
            then t2 else failwith("type-error")
        |_ -> failwith("wrong type"))
  | For_all(pred,set) | Exists(pred,set) | Filter(pred,set) ->
      if check_set(set) != TBool then failwith("not a set") else
        let e1=teval pred s in
        let e2= teval set s in
        (match (e1,e2) with
         |(FunT(t1,t2),_typeSet_)-> if t1 != _typeSet_ || t2 != TBool then failwith("pred-set type mismatch")
             else t1
         |_ -> failwith("not a predicate")
        )
  | Map(funct,set) -> 
      if check_set(set) != TBool then failwith("not a set") else
        let e1=teval funct s in
        let e2= teval set s in
        (match (e1,e2) with
         |(FunT(t1,t2),_typeSet_)-> if t1 != _typeSet_ then failwith("pred-set type mismatch")
             else t1
         |_ -> failwith("not a function")
        );;

(*========================================= TEST-CASE =============================================================*)

let set=Of(TInt,Value(CstInt(1),Value(CstInt(1),Value(CstInt(5),Value(CstInt(7),Value(CstInt(2),Empty))))));;(*{1, 5, 7, 2}*)
teval set tEmptyEnv;;

let p=Push(set,CstInt(4));;
(*let p=Push(set,CstString("a"));; -> type-error*)
teval p tEmptyEnv;;

let pred_pari=Fun("x",TInt,Ifthenelse(Eq(Mod(Den("x"),CstInt(2)),CstInt(0)),CstTrue,CstFalse));;
teval pred_pari tEmptyEnv;;

let s=Letrec("fact", "n",TInt,TInt,   
             Ifthenelse(Eq(Den("n"),CstInt(0)),
                        CstInt(1),
                        Times(Den("n"),Apply(Den("fact"),Sub(Den("n"),CstInt(1))))), Apply(Den("fact"),CstInt(3)));;
teval s tEmptyEnv;;

let s=Letrec("str", "n",TInt,TString,   
             Ifthenelse(Eq(Den("n"),CstInt(0)),
                        CstString("a"),
                        Apply(Den("str"),Sub(Den("n"),CstInt(1)))), Apply(Den("str"),CstInt(3)));;
teval s tEmptyEnv;;

let fa1=For_all(pred_pari,set);;
let fa2=For_all(pred_pari,Singleton(CstString("a")));;

teval fa1 tEmptyEnv;;
(*teval (Not(set)) tEmptyEnv;; -> errore*)