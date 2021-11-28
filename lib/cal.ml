let split s = List.init (String.length s) (String.get s)
let%test _ = split "hello" = [ 'h'; 'e'; 'l'; 'l'; 'o'  ]

(* calicurate *)
let plus a b = a + b
let%test _ = plus 1 1 = 2

let minus a b = a - b
let%test _ = minus 2 1 = 1

let times a b = a * b
let%test _ = times 2 3 = 6

let devide a b =  a / b
let%test _ = devide 3  2 =  1

let cal f stack =
    let rec cal_all f a stack =
        match Stack.pop_opt stack with
            | Some b -> cal_all f (f a b) stack
            | None -> Stack.push a stack
    in
        cal_all f (Stack.pop stack) stack


let chars_to_number chars =
    let chars_to_string chars = String.concat "" (List.map (String.make 1) chars) in
    let string_to_number str = int_of_string str in
        string_to_number (chars_to_string chars)

let parse_number tokens =
    let rec f nums tokens =
        match tokens with
            | h :: tail ->
                    (match h with
                        | '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9'| '0' -> f (List.append nums [h]) tail
                        | _ -> (tokens , chars_to_number nums))
                    | [] -> (tokens, chars_to_number nums)
    in
        f [] tokens

let parse src =
    let tokens = split src in
    let stack = Stack.create () in
    let rec f tokens =
        match tokens with
            |  h::tail ->
                    (match h with
                        | ' ' ->  f tail
                        | '+' -> cal plus stack; f tail
                        | '-' -> cal minus stack; f tail
                        | '*' -> cal times stack; f tail
                        | '/' -> cal devide stack; f tail
                        | '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9'| '0' ->
                                let (tail, n) = parse_number tokens in
                                Stack.push n stack; f tail
                        | _ -> f tail)
            | [] -> Stack.pop stack
    in
        f tokens

let%test _ = parse "1 2 3 + 4 5 +" = 15
let%test _ = parse "1 2 3 + 4 *" = 24
let%test _ = parse "10 2 3 + 4 *" = 60
let%test _ = parse "10 2 3 + 4 *" = 60





















