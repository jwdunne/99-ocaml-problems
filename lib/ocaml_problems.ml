let rec last l =
  match l with [] -> None | [a] -> Some a | _ :: rest -> last rest

let%test _ = last [1; 2; 3] = Some 3

let%test _ = last [] = None

let rec last_two = function
  | [] | [_] ->
      None
  | [a; b] ->
      Some (a, b)
  | _ :: rest ->
      last_two rest

let%test _ = last_two [1; 2; 3; 4] = Some (3, 4)

let%test _ = last_two [1] = None

let%test _ = last_two [] = None

let rec at n l =
  match l with
  | [] ->
      None
  | a :: _ when n = 0 ->
      Some a
  | _ :: rest ->
      at (n - 1) rest

let%test _ = at 1 [1; 2; 3] = Some 2

let%test _ = at 3 [1; 2; 3] = None

let length l =
  let rec aux n = function [] -> n | _ :: rest -> aux (n + 1) rest in
  aux 0 l

let%test _ = length [1; 2; 3] = 3

let%test _ = length [] = 0

let rev l =
  let rec aux r = function [] -> r | h :: t -> aux (h :: r) t in
  aux [] l

let%test _ = rev [1; 2; 3] = [3; 2; 1]

let%test _ = rev [] = []

let is_palindrome l = List.rev l = l

let%test _ = is_palindrome [1; 2; 1]

let%test _ = not (is_palindrome [1; 2; 3])

type 'a node = One of 'a | Many of 'a node list

let flatten l =
  let rec aux r = function
    | [] ->
        r
    | One x :: t ->
        aux (x :: r) t
    | Many l :: t ->
        aux (aux r l) t
  in
  List.rev (aux [] l)

let%test _ = flatten [One 1; One 2; One 3] = [1; 2; 3]

let%test _ = flatten [One 1; Many [One 2; Many [One 3]]] = [1; 2; 3]

let hd_opt = function [] -> None | a :: _ -> Some a

let compress l =
  let rec aux r = function
    | [] ->
        r
    | h :: t ->
        if Some h = hd_opt r then aux r t else aux (h :: r) t
  in
  List.rev (aux [] l)

let%test _ = (compress [1; 1; 1; 2; 2; 2; 3; 3; 3]) = [1; 2; 3]


