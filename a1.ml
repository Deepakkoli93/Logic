type comparison = Less | Equal | Greater;;
module type ORDERED_TYPE =
    sig
      type t
      val compare: t -> t -> comparison
    end;;

module Set =
    functor (Elt: ORDERED_TYPE) ->
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
     
         let power x = let subsets xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]];;
     end;;


module OrderedString =
    struct
      type t = string
      let compare x y = if x = y then Equal else if x < y then Less else Greater
    end;;

module S1 = Set(OrderedString);;


let a=S1.add "foo" S1.empty;;
let a=S1.add "bar" a;;
let b=S1.add "bar" S1.empty;;  
(*let b=S1.add "foo" b;;*)
let b=S1.add "foobar" b;;
let d=S1.empty  
let c = S1.power d;;     
