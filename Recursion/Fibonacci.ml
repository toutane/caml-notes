let rec fibo = function
    0 | 1 -> 1
    | n -> fibo (n - 1) + fibo (n - 2) ;;

fibo 4 ;;
fibo 16 ;;
