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
     
         (*let power x = (let subsets xs = (List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]))*)
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
                      |(a,b)::tl -> (b,a)::(inverse tl)

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

 


end;;

module R = Relation(OrderedInteger);;
let r1 = [(1,2);(2,3);(3,1)];;
let r2 = [(1,2);(2,3);(3,1);(2,5)];;
R.identity [1;2;3];;
R.composition r1 r2;;
R.union r1 r2;;
R.intersect r1 r2;;
R.refclosure [1;2;3] r1;;

