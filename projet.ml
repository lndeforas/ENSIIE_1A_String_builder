(* Echauffement *)

(* Question 1 *)

type string_builder =
  |Leaf of string * int
  |Node of string_builder * string_builder * int ;;

let word s = Leaf(s, String.length s);;

let concat sb1 sb2 = 
  match sb1, sb2 with
  |Leaf(a,x),Leaf(b,y) -> Node(Leaf(a,x),Leaf(b,y),x+y)
  |Leaf(a,x),Node(b,c,y) ->Node(Leaf(a,x),Node(b,c,y),x+y)
  |Node(a,b,x),Leaf(c,y)->Node(Node(a,b,x),Leaf(c,y),x+y)
  |Node(a,b,x),Node(c,d,y)->Node(Node(a,b,x),Node(c,d,y),x+y);;

(* Question 2 *)

let rec char_at i sb = match sb with
  |Leaf(a,x) -> String.get a i
  |Node(Leaf(a,x),b,y) -> if x > i then String.get a i 
      else char_at (i-x) b
  |Node(Node(a,b,x),c,y) -> if x>i then char_at i (Node(a,b,x))
      else char_at (i-x) c;;

(* Question 3 *) 

let rec sub_string i m sb = match sb with 
  |Leaf(a,x) -> Leaf(String.sub a i m,m)
  |Node(Leaf(a,x),b,y)->
      if x > i then 
        if x > i+m-1 then Leaf(String.sub a i m,m)
        else Node(Leaf(String.sub a i (x-i), x-i), sub_string 0 (m-x+i) b, m)
      else sub_string (i-x) m b
  |Node(Node(a,b,x),c,y) -> 
      if x > i then
        if x > i+m-1 then sub_string i m (Node(a,b,x))
        else Node(sub_string i (x-i) (Node(a,b,x)),sub_string 0 (m-x+i) c,m)
      else sub_string (i-x) m c;;

(* Equilibrage *)

(* Question 4 *)

let cost sb = 
  let rec aux sb depth = match sb with
    |Leaf(a,x)->x*depth
    |Node(a,b,x)-> (aux a (depth+1)) + (aux b (depth+1))
  in aux sb 0;;

(* Question 5 *) 

let rec depth sb = match sb with
|Leaf(a,x) -> 0
|Node(a,b,x) -> 1 + max (depth a) (depth b);;

let rec random_string depth = 
  let rec length sb = match sb with 
    |Leaf(a,x) -> x
    |Node(a,b,x) -> (length a) + (length b) in 
  let random_word nb_lettres= 
    let j=(Random.int nb_lettres)+1 in 
    let rec aux i = match i with
      |1 -> (String.make 1 (Char.chr ((Random.int 58)+65)))
      |i -> (aux (i-1))^(String.make 1 (Char.chr ((Random.int 58)+65)))
    in aux j in
  if depth=0 then word (random_word 10)
  else let a=random_string (depth-1) and b=random_string (depth-1) in 
    Node(a, b, (length a)+ (length b));;

(* Question 6 *)

let rec list_of_string = function
  |Leaf(a,x)->[a]
  |Node(a,b,x)->(list_of_string a)@(list_of_string b);;

(* Question 7 *) 

let balance sb=
  let list_feuilles sb = List.map word (list_of_string sb)
  in let weak_cost lsb=
       let rec valmin lsb vmin=match lsb with
         |[]->vmin
         |[a]->vmin
         |h::t->if (cost (concat h (List.hd t)))<vmin then valmin t (cost (concat h (List.hd t)))
             else valmin t vmin
       in valmin lsb max_int
  in let retire_concat lsb = 
       let vmin=weak_cost lsb in 
       let rec aux lsb vmin= match lsb with
         |[]->[]
         |[a]->[a]
         |h::t-> if (cost (concat h (List.hd t)))<=vmin then (concat h (List.hd t))::(List.tl t)
             else h::(aux t vmin)
       in aux lsb vmin
  in let rec aux_b =function
      |[]->failwith "Vide"
      |[a]->a
      |h::t->aux_b (retire_concat (h::t)) 
  in aux_b (list_feuilles sb);;
  
(* Question 8 *) 

let rec tri_insertion = function
  |[] -> []
  |h::t -> 
      let rec insere e l = match l with
        |[] -> [e]
        |h::t -> if e<h then e::l else h::(insere e t) 
      in insere h (tri_insertion t);;

let values l = let n=List.length l in 
  let rec mean = function
    |[]-> 0. 
    |h::t -> (mean t) +. ( (float_of_int h) /. (float_of_int n) ) in
  let rec min = function 
    |[] -> max_int
    |h::t -> if h<min t then h else min t in 
  let rec max = function 
    |[] -> min_int
    |h::t -> if h>max t then h else max t in 
  let l_tri = tri_insertion l in 
  let rec median l_tri n = if n=0 then  List.hd l_tri
    else begin match l_tri with 
      |[] -> -1
      |h::t -> median t (n-1) end
  in min l, mean l, median l_tri (n/2), max l;;

let gain nb_arbres taille_arbres =
  let rec arbres i = if i=0 then [] else
      let sb= random_string taille_arbres in ((cost sb)-(cost (balance sb)))::(arbres (i-1))
  in values (arbres nb_arbres);; 

(* Cas de tests *)

let () = assert (word "Hello" = Leaf("Hello",5));;

let sb = Node(Node(Leaf("Co",2),Leaf("u",1),3),Leaf("cou",3),6);;
let sb2 = Node(Leaf(" le ",4), Node(Node(Leaf("cha",3),Leaf("m",1),4),Node(Leaf("e",1),Leaf("au",2),3),7),11);;

let () = assert (concat sb sb2=Node(sb,sb2,17));;

let () = assert (char_at 4 sb2 ='c');;

let () = assert (sub_string 1 4 sb = Node(Node(Leaf("o",1),Leaf("u",1),2),Leaf("co",2),4));;

let () = assert (cost sb = 9);;

let () = assert (depth sb = 2);;

let () = assert (depth (random_string 2) = 2);;

let () = assert (list_of_string sb =["Co";"u";"cou"]);;

let () = assert (balance sb2 = Node(Node(Leaf(" le ",4),Leaf("cha",3),7),
Node(Node(Leaf("m",1),Leaf("e",1),2),Leaf("au",2),4),11));;

let (a,b,c,d) = (gain 15 3);;

let () = assert (a<=c);;

let () = assert (b<= float_of_int d);;

print_string ("ok ! \n");;

