(*TEST-CASE - Main*)

let set1=Singleton(CstInt(3));;(*{ 3 }*)
let set2=Of(Int,Value(CstInt(1),Value(CstInt(1),Value(CstInt(5),Value(CstInt(7),Value(CstInt(2),Empty))))));;(*{1, 5, 7, 2}*)
let emptyset=Empty(String);;(*{empty}*)
                            
print_string "Set 1: ";;
eval set1 emptyEnv;;

print_string "Set 2: ";;
eval set2 emptyEnv;;                            
                            
let union=Union(set1,set2);;
let inters=Intersection(set1,set2);;
let sub=Difference(set1,set2);;

print_string "Unione: ";;
eval union emptyEnv;;

print_string "Intersezione: ";;
eval inters emptyEnv;;

print_string "Differenza: ";;
eval sub emptyEnv;;

print_string "Inserisco i valori 4,7,5 dentro Set 1: ";;
print_string "Set 1 (prima): ";;
eval set1 emptyEnv;;

let set1=Push(set1,CstInt(4));;(*3, 4*)
let set1=Push(set1,CstInt(7));;(*3, 4, 7*)
let set1=Push(set1,CstInt(5));;(*3, 4, 7, 5*)
                               
print_string "Set 1 (dopo): ";;
eval set1 emptyEnv;;                               

print_string "Rimuovo il 7";;
let set1=RemoveFrom(set1,CstInt(7));;(*3, 4, 5*)
eval set1 emptyEnv;;                               

print_string "Controllo se emptySet è vuoto";;
let ismaybeEmpty=IsEmpty(emptyset);;
eval ismaybeEmpty emptyEnv;;                               

print_string "Controllo se Set 1 ha il 7 rimosso prima";;
let cont=Contains(set1,CstInt(7));;
eval cont emptyEnv;;                               

print_string "Max e Min di Set 1 con il 7";;
let max=MaxOf(Push(set1,CstInt(7)));;(*3, 4, 7, 5* -> max = 7)*)
let min=MinOf(Push(set1,CstInt(7)));;

eval max emptyEnv;;
eval min emptyEnv;;

(*Operatori funzionali*)
print_string "Operatori funzionali su Set 2";;
(*let forall=For_all(CstInt(1),set2);; -> da runtime-error*)
eval set2 emptyEnv;;

(*Semplice predicato che contralla 
se tutti gli elementi del Set sono pari/dispari*)

(*pred_pari x = if x % 2=0 then true else false *)
let pred_pari=Fun("x",Ifthenelse(Eq(Mod(Den("x"),CstInt(2)),CstInt(0)),CstTrue,CstFalse));;
let pred_dispari=Fun("x",Ifthenelse(Not(Eq(Mod(Den("x"),CstInt(2)),CstInt(0))),CstTrue,CstFalse));;

let funct_make_pari=Fun("x",Times(Den("x"),CstInt(2)));;

let forall_pari=For_all(pred_pari,set2);;
let forall_dispari=For_all(pred_dispari,set2);;

print_string "Controllo se tutti i valori di Set 2 sono pari/dispari";;
eval forall_pari emptyEnv;;
eval forall_dispari emptyEnv;;

print_string "Controllo se esiste un valore di Set 2 pari/dispari";; 
let exists_pari=Exist(pred_pari,set2);;
let exists_dispari=Exist(pred_dispari,set2);;

eval exists_pari emptyEnv;;
eval exists_dispari emptyEnv;;

print_string "Filtro i valori di Set 2 pari/dispari";;

let filter_pari=Filter(pred_pari,set2);;
let filter_dispari=Filter(pred_dispari,set2);;

eval filter_pari emptyEnv;;
eval filter_dispari emptyEnv;;

print_string "Rendo tutti i valori di Set 2 pari";;
let map_make_pari=Map(funct_make_pari,set2);;

eval map_make_pari emptyEnv;;

(*Funzione che ritorna due tipi diversi*) 
(*error_fun x= if x=2 then x+1 else false*)

(*let error_fun=Fun("x",Ifthenelse(Eq(Den("x"),CstInt(2)),Sum(Den("x"),CstInt(1)),CstFalse));;
 eval (Map(error_fun,set2)) emptyEnv;;*)