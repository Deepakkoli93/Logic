type comparison = Less | Equal | Greater;;
module type ORDERED_TYPES =
    sig
      type t
      
      val compare: t -> t -> comparison
      
    end;;

module type ORDERED_TYPER =
    sig
      type t
      type r
      val compare: t -> t -> comparison
      val comparer: r -> r -> comparison
      val maket: t -> t
      val maker: t -> r
      val maker_inorder: (int*int) -> r 
      val makeintint: r -> (int*int)
      val makeint: t -> int 
      val makeinv: r -> r
      
    end;;

module Set =
    functor (Elt: ORDERED_TYPES) ->
      struct
        type element = Elt.t
        type set = element list
        let empty = []
        let rec add x s =
          match s with
            [] -> [x]
          | hd::tl ->
             match Elt.compare x hd with
               Equal   -> s         (* x is already in s *)
             | Less    -> x :: s    (* x is smaller than all elements of s *)
             | Greater -> hd :: add x tl
        let rec member x s =
          match s with
            [] -> false
          | hd::tl ->
              match Elt.compare x hd with
                Equal   -> true     (* x belongs to s *)
              | Less    -> false    (* x is smaller than all elements of s *)
              | Greater -> member x tl

        let rec subseteq x y = 
          match x with
          [] -> true
         |hd::tl -> (match member hd y with
                      true -> subseteq tl y
                     |false -> false )     
        let rec setequal x y =
          match x with
          [] -> (match y with
                 [] -> true
                |hd::tl -> false)
         |hd::tl -> (match y with
                   [] -> false
                    |a::b -> match Elt.compare hd a with
                              Equal -> setequal tl b
                             | _ -> false )

        let rec union x y = 
          match x with 
          [] -> y
         |hd::tl -> union tl (add hd y)

        let rec intersect x y = 
          match x with
           [] -> []
          |hd::tl -> (match y with
                      [] -> []
                     |a::b -> match Elt.compare hd a with
                               Equal -> hd::(intersect tl b)
                              |Less -> (intersect tl y)
                              |Greater -> (intersect x b))


         let rec difference x y = 
           match x with
            [] -> []
           |hd::tl -> (match y with
                        [] -> x
                       |a::b -> match Elt.compare hd a with
                                 Equal -> (difference tl b)
                                |Less -> hd::(difference tl y)
                                |Greater -> (difference x b))

         let rec cartesian x y =
           match x with
            [] -> []
           |hd::tl -> (match y with
                        [] -> []
                       |a::b -> List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) y) x))
     
let power xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]
     

     end;;


module OrderedString =
    struct
      type t = string
      let compare x y = if x = y then Equal else if x < y then Less else Greater
    end;;

module OrderedInteger =
    struct
      type t = int
      type r = (int*int)
      let maket x = x
      let maker x = (x,x)
      let maker_inorder (x,y) = (x,y)
      let makeinv (x,y) = (y,x)
      let makeintint (x,y) = (x,y)
      let makeint x = x
      let compare x y = if x = y then Equal else if x < y then Less else Greater
      let comparer x y = if x = y then Equal else if x < y then Less else Greater
    end;;

module S1 = Set(OrderedString);;


let a=S1.add "foo" S1.empty;;
let a=S1.add "bar" a;;
let b=S1.add "bar" S1.empty;;  
(*let b=S1.add "foo" b;;*)
let b=S1.add "foobar" b;;
let d=S1.empty  
(*let c = S1.power d;;     *)

module Relation = 
  functor (Elt: ORDERED_TYPER) ->
  struct
  type element = (Elt.t * Elt.t)
  type relation = element list
  let rec identity x = match x with
                        [] -> []
                       |hd::tl -> (Elt.maker(hd))::(identity tl)

  let rec helpcomp x y = match x with
                         (a,b) -> (match y with
                                    [] -> []
                                   |(y1,y2)::tl -> match Elt.compare b y1 with
                                               Equal -> (a,y2)::(helpcomp x tl)
                                              |_-> (helpcomp x tl)
                                              ) 

  let rec composition x y = match x with 
                             [] -> []
                             |hd::tl -> (helpcomp hd y)@(composition tl y)

 (* inverse is not ordered *)
  let rec inverse x = match x with
                       [] -> []
                      |hd::tl -> (Elt.makeinv(hd))::(inverse tl)

   let rec add x s =
          match s with
            [] -> [x]
          | hd::tl ->
             match Elt.comparer x hd with
               Equal   -> s         (* x is already in s *)
             | Less    -> x :: s    (* x is smaller than all elements of s *)
             | Greater -> hd :: add x tl



  let rec member x s =
          match s with
            [] -> false
          | hd::tl ->
              match Elt.compare x hd with
                Equal   -> true     (* x belongs to s *)
              | Less    -> false    (* x is smaller than all elements of s *)
              | Greater -> member x tl


  let rec union x y = 
          match x with 
          [] -> y
         |hd::tl -> union tl (add hd y)

  let rec intersect x y = 
          match x with
           [] -> []
          |hd::tl -> (match y with
                      [] -> []
                     |a::b -> match Elt.comparer hd a with
                               Equal -> hd::(intersect tl b)
                              |Less -> (intersect tl y)
                              |Greater -> (intersect x b))


  let rec refclosure v e = union (identity v) e 
  let rec symclosure e = union (inverse e) e

 

 (*let rec composition x y = match x with
                            [] -> []
                           |hd::tl -> (match hd with
                                       (a,b) -> )*)
 (*type element = Set
 type relation = element list list
 let rec identity x = match x with
                    [] -> []
                   |hd::tl -> [hd;hd]::(identity tl)

 let rec helpcomp2 a b = match b with
                            [] -> []
                           |hd::tl -> if (List.hd(hd)=a) then List.tl(hd)
                                      else helpcomp2 a tl

 let rec helpcomp a b= match a with
                              [] -> []
                             |hd::tl -> (helpcomp2 hd b)@(helpcomp tl b)

 let rec composition f1 f2 = match f1 with
                              [] -> []
                             |hd::tl -> (List.hd(hd)::(helpcomp (List.tl(hd)) f2))::(composition tl f2)*)

 
let rec convert c_transclos = match c_transclos with 
                                     [] -> []
                                     |hd::tl -> Elt.maker_inorder(hd)::convert(tl)

let rec printtc a v c= 
  for i=0 to v do
   for j=0 to v do 
    if (a.(i).(j) == true ) then c := (i,j)::!c
  done;
done;
 convert !c 

open Array;;
let transitiveclosure graph reach v c_transclos=
 for i=0 to v do
  for j=0 to v do
    reach.(i).(j) <- graph.(i).(j)
  done;
 done;

 for k=0 to v do
  for i=0 to v do
   for j=0 to v do
    reach.(i).(j) <- reach.(i).(j) or (reach.(i).(k) & reach.(k).(j))
   done;
  done;
 done;
 
 printtc reach v c_transclos 


 (*let transcloc graph reach v c_transclos = 
  transitiveclosure graph reach v c_transclos and  *)
(*let transclos graph reach c c_transclos = transitiveclosure graph reach v c_transclos 
                                            convert !c_transclos*)


let rec makearr l graph reach v c_transclos= (match l with
                      [] -> transitiveclosure graph reach v c_transclos
                     |(a,b)::tl -> graph.(a).(b) <- true;makearr tl graph reach v c_transclos)

let reftransclos v e graph reach c_transclos= makearr (List.map Elt.makeintint (refclosure v e)) graph reach c_transclos

let eqclos v e graph reach c_transclos = let l = (List.map Elt.makeintint (symclosure (refclosure v e)) )
                                          in makearr l graph reach (List.length v) c_transclos

end;;

let graph = Array.make_matrix 4 4 10;;
(*graph=[|[|1,1,0,1|];[|0,1,1,0|];[|0,0,1,1|];[|0,0,0,1|80]|];;*)
(*for i=0 to 3 do
  for j=0 to 3 do
   graph.(i).(j) <- 0
 done;
done;;*)
let graph=[|[|true;true;false;true|];[|false;true;true;false|];[|false;false;true;true|];[|false;false;false;true|]|];;
let reach = Array.make_matrix 4 4 false;;
(*transitiveclosure graph reach 3;;*)


let c_transclos=ref [];;

module R = Relation(OrderedInteger);;
R.transitiveclosure graph reach 3 c_transclos;;
(*let r1 = [(1,2);(2,3);(3,1)];;
let r2 = [(1,2);(2,3);(3,1);(2,5)];;
R.identity [1;2;3];;
R.composition r1 r2;;
R.union r1 r2;;
R.intersect r1 r2;;
R.refclosure [1;2;3] r1;;
R.symclosure r1;;*)

