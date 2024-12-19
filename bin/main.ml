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

