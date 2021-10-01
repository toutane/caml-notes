let move src dest =
  print_int src ;
  print_string " -> " ;
  print_int dest ;
  print_newline()
;;

let hanoi n =
  let rec play n src dest =
    if n = 1 then move src dest
    else
      let inter = 6 - (src + dest)
      in
      begin
        play (n - 1) src inter ;
        move src dest ;
        play (n - 1) inter dest
      end
  in
  if n < 1 then invalid_arg "Hanoi: number of disk invalid"
  else
    play n 1 3
;;

hanoi 1 ;;
hanoi 2 ;;
hanoi 3 ;;
  
    
