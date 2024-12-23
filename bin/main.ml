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