(* TP1: Premiers pas avec Caml - Exercice 7: Lendemain *)

(* is_date_valid function check if the given date is in the valid format: "jj/mm/aaaa" *)
let is_date_valid date day month year =
	  if year < 0 then false
	  else (
	    if month <= 0 || month > 12
	    then false
	    else
	      match month with
		  2 | 4 | 6 | 9 | 11 -> (
		    if day <= 0 || day > 30
		    then false
		    else true
		  )
		| _ -> (
		  if day <= 0 || day > 31
		  then false
		  else true
		)
	) ;;
						
(* val is_date_valid : string -> bool = <fun> *)
		  
(* tomorrow function return the next's day date in two possible formats: "jj/mm/aaaa" or "literal_month dd, yyyy" *) 
let tomorrow date =
  let formatted_date = (
    if String.length date > 10
    then  
      let month1 =
	match String.sub date 0 (String.index_from date 0 ' ') with
	    "January" -> "01"
	  | "February" -> "02"
	  | "March" -> "03"
	  | "April" -> "04"
	  | "May" -> "05"
	  | "June" -> "06"
	  | "July" -> "07"
	  | "August" -> "08"
	  | "September" -> "09"
	  | "October" -> "10"
	  | "November" -> "11"
	  | "December" -> "12"
	  | _ -> invalid_arg "tomorrow: not a valid month"  
      and day1 = String.sub date ((String.index_from date 0 ' ') + 1) (((String.index_from date 0 ',') - 1) - (String.index_from date 0 ' '))
      and year1 = String.sub date ((String.index_from date 0 ',') + 2) 4
      in (
	day1 ^ "/" ^  month1 ^ "/" ^ year1
      )
    else date 
  )
  in
  
  let day = int_of_string (String.sub formatted_date 0 2)
  and month = int_of_string (String.sub formatted_date 3 2)
  and year = int_of_string (String.sub formatted_date 6 4)
  in if is_date_valid formatted_date day month year 
    then (
      let new_day =
	match month with
	    2 | 4 | 6 | 9 | 11 -> (
	      if day = 30
	      then "01"
		else (
		  if day < 9
		  then "0" ^ string_of_int (day + 1)
		  else string_of_int (day + 1)
		)
	    )
	  | _ -> (
	    if day = 31
	      then "01"
	      else (
		if day < 9
		then "0" ^ string_of_int (day + 1)
		else string_of_int (day + 1)
	      )
	  )
      in let new_month =
	   if new_day = "01"
	   then (
	     if month < 9
	     then "0" ^ string_of_int (month + 1)
	     else (
			 if month < 12
			 then string_of_int (month + 1)
			 else "01"
	     )
	   )
	   else (
	     if month < 10
	     then "0" ^ string_of_int month
	     else string_of_int month
	   )
	 in let new_year =
	      if month <> int_of_string new_month && month = 12
	      then string_of_int (year + 1)
	      else string_of_int year
	    in if date = formatted_date
	      then ( new_day ^ "/" ^ new_month ^ "/" ^ new_year )
	      else (
		let literal_month =
		  match new_month with
		      "01" -> "January"
		    | "02" -> "February"
		    | "03" -> "March"
		    | "04" -> "April"
		    | "05" -> "May"
		    | "06" -> "June"
		    | "07" -> "July"
		    | "08" -> "August"
		    | "09" -> "September"
		    | "10" -> "October"
		    | "11" -> "November"
		    | "12" -> "December"
		    | _ -> invalid_arg "tomorrow: month don't match"
		in (
		  literal_month ^ " " ^ new_day ^ ", " ^ new_year
		)
	      )
    )
    else invalid_arg "tomorrow: not a valid date" ;;

(* val tomorrow : string -> bool = <fun> *)

tomorrow "31/02/2005" ;;
(* Execption: Invalid_argument "not a valid date"  *)
tomorrow "30/08/2005" ;;
(* - : string = "31/08/2005" *)
tomorrow "31/08/2005" ;;
(* - : string = "01/09/2005" *)
tomorrow "31/12/2005" ;;
(* - : string = "01/01/2006" *)
tomorrow "December 25, 2005" ;;
(* - : string = "December 26, 2005" *)
