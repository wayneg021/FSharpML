module F

let rec luc x =
    match x with
    | x when x <= 0 -> failwith "values must be greater than zero"
    | 1 -> 1
    | 2 -> 3
    | x -> luc (x - 1) + luc (x - 2)


