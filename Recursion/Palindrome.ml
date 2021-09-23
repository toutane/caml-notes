let is_pal w =
  let length = String.length w
  in
  if length <= 1 then true
  else
    let rec compare i =
      if length - (2 * i) < 1 then true
      else
        if w.[i] = w.[(length - 1) - i] then compare (i + 1)
        else false
    in
    compare 0
;;

is_pal "a" ;;
is_pal "aa" ;;
is_pal "an" ;;
is_pal "hello" ;;
is_pal "anbbna" ;;
is_pal "asdrirdsa" ;;
             
  
