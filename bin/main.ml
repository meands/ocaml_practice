(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last xs

  
(* Find the last two (last and penultimate) elements of a list. *)
let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x, y)
  | _::xs -> last_two xs


(* Find the N'th element of a list. *)
let rec find_nth n = function
  | [] -> None
  | x::xs -> if n = 0 then Some x else find_nth (n-1) xs  

(* Find the number of elements of a list. *)
let rec length list = 
  let rec inc c = function
    | [] -> c
    | _::xs -> inc (c+1) xs
  in inc 0 list

(* Reverse a list. *)
let rev list = 
  let rec aux r = function
    | [] -> r
    | x::xs -> aux (x::r) xs
  in aux [] list

(* Find out whether a list is a palindrome. *)
let is_palindrome list = rev list = list

(* Run-length encoding *)
let encode list = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count, x)::acc
    | a::(b::c) -> if a = b then aux (count+1) acc c else aux 0 ((count+1, a)::acc) c
in aux 0 [] list

(* Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. 
  Only elements with duplicates are transferred as (N E) lists. *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
let encode_modified list = 
  let create_element count el = 
    if count = 1 then One el
    else Many (count, el) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_element count x)::acc
    | a::(b::c) -> if a = b then aux (count+1) acc c else aux 0 ((create_element (count+1) a)::acc) c
in aux 0 [] list

(* Duplicate the elements of a list. *)
let rec duplicate = function 
  | [] -> [] 
  | x::xs -> x::x::duplicate xs

(* Split a list into two parts; the length of the first part is given. 
  If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
let split list cnt = 
  let rec aux acc n = function 
    | [] -> (List.rev acc, [])
    | x::xs as l -> if n = 0 then (List.rev acc, l) else aux (x::acc) (n-1) xs
in aux [] cnt list

(* Remove the K'th element from a list. *)
let rec remove_at k = function
  | [] -> []
  | x::xs -> if k = 0 then xs else x::remove_at (k-1) xs

(* Insert an Element at a Given Position Into a List *)
let rec insert_at el n = function
  | [] -> [el]
  | x::xs as l -> if n = 0 then el::l else x::insert_at el (n-1) xs

(* Create a List Containing All Integers Within a Given Range *)
let range a b =
  let rec aux a b = 
    if a > b then [] else a::aux (a+1) b
in if a > b then List.rev (aux b a) else aux a b

(* Lotto: Draw N Different Random Numbers From the Set 1..M *)

(* Generate a Random Permutation of the Elements of a List *)

(* Determine Whether Two Positive Integer Numbers Are Coprime *)

(* Compare the Two Methods of Calculating Euler's Totient Function *)

(* Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. *)
