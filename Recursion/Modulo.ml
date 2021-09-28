let rec modulo a b =
  if a < b then a
  else modulo (a - b) b;;

modulo 14 3 ;;
