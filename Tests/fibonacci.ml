let fibo n =
  let rec fib_tail n a b =
    if n = 0 then a
    else if n = 1 then b
    else fib_tail (n - 1) b (a + b)
  in
  fib_tail n 0 1
;;

let rec fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2)
;;

fibo 10 = fibonacci 10;;

fibo 3 = fibonacci 10;;

fibo 10 = fibonacci 9;;
