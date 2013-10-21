(* (1) Returns boolean indicating if first date truple is older than the latter (year, month, day) *)
fun is_older(x : int*int*int, y : int*int*int) =
    if (#1 x) < (#1 y)
    then true
    else if (#1 x) = (#1 y)
         then (* compare month & day *)
            if (#2 x) < (#2 y)
	    then true
            else if (#2 x) = (#2 y)
	         then (#3 x) < (#3 y) (* compare day *)
	    else false
         else false;

(* (2) Returns the number of dates which match the target month argument *)
fun number_in_month(dateList : (int*int*int) list, month : int) =
    if null dateList
    then 0
    else if #2 (hd dateList) = month
         then 1 + number_in_month((tl dateList), month)
         else 0 + number_in_month((tl dateList), month);

(* (3) Return the number of dates which contain months in the month list *)
fun number_in_months(dateList : (int*int*int) list, monthList : int list) = 
    if null monthList
    then 0
    else number_in_month(dateList, (hd monthList)) + number_in_months(dateList, (tl monthList));

(* (4) Returns a list of dates corresponding to the 'month' argument *)
fun dates_in_month(dateList : (int*int*int) list, month : int) =
    if null dateList
    then []
    else if #2 (hd dateList) = month
         then (hd dateList) :: dates_in_month((tl dateList), month)
         else dates_in_month((tl dateList), month);

(* (5) returns a  list of dates with months matching those listed in 'monthList' (no repetition) *)
fun dates_in_months(dateList : (int*int*int) list, monthList : int list) = 
    if null monthList
    then []
    else dates_in_month(dateList, hd monthList) @ dates_in_months(dateList, tl monthList);

(* (6) returns the nth element from a list of strings. According to TA, bounds errors do not need to be handled  *)
fun get_nth(stringList : string list, n : int) =
    if n = 1
    then hd stringList
    else get_nth(tl stringList, n - 1);
    
(* (7) converts truple to string representation *)
fun date_to_string(date : (int*int*int)) = 
    get_nth(
       ["January", "February", "March", "April", 
        "May", "June", "July", "August", 
        "September", "October", "November", "December"], #2 date) ^ " " ^ 
    Int.toString(#3 date) ^ ", " ^ 
    Int.toString(#1 date);

(* (8) returns n | the first n elements of intList summed is less than sum, and n+1 is GTE the sum *)
exception ListSumError;
fun number_before_reaching_sum(sum : int, intList : int list) =
    if sum <= 0
    then ~1
    else if null intList
         then raise ListSumError
         else 1 + number_before_reaching_sum(sum - (hd intList), tl intList);

(* (9) returns an integer representing what month a day is in *)
fun what_month(dayOfYear : int) =
    number_before_reaching_sum(dayOfYear, [31,28,31,30,31,30,31,31,30,31,30,31]) + 1;

(* (10) returns an int list [m1, m2, ..., mn] where m1 is the month of day1, m2 is the month of day1+1and mn is the month of day2 *)
fun month_range(day1 : int, day2 : int) = 
    if day1 = day2
    then [ what_month(day1) ]
    else if day1 > day2
         then []
         else what_month(day1) :: month_range(day1 + 1, day2);

(* (11) returns a truple representing the oldest date in the argument list *)
fun oldest(dateList : (int*int*int) list) =
    if null dateList
    then NONE
    else 
        let fun min_date (dateList : (int*int*int) list) =
                if null (tl dateList)
                then hd dateList
                else let val result = min_date(tl dateList)
                     in  
                         if is_older(hd dateList, result)
                         then hd dateList
                         else result
                     end
         in 
             SOME (min_date(dateList))
         end;
    
(* Helper function for determining if an element 'n' exists in a list *)
fun in_list(nList : int list, n : int) =
    if null nList
    then false
    else if (hd nList) = n
         then true
         else in_list(tl nList, n);

(* Helper function, given an int list returns a list with duplicates removed  *)
fun de_dup(nList : int list) = 
    if null nList
    then nList
    else if null (tl nList)
         then nList
         else if in_list(tl nList, hd nList)
              then de_dup(tl nList)
              else (hd nList) :: de_dup(tl nList);

(* (12) similar to solutions in problems 3 & 5, though duplicates have no impact *)
fun number_in_months_challenge(dateList : (int*int*int) list, monthList : int list) = 
    number_in_months(dateList, de_dup(monthList));

fun dates_in_months_challenge(dateList : (int*int*int) list, monthList : int list) =
    dates_in_months(dateList, de_dup(monthList));

fun get_nth_int(intList: int list, n: int) =
    if n = 1
    then hd intList
    else get_nth_int(tl intList, n - 1);

(* Helper function, given a month, day, and dayList; returns boolean if date is valid *)
fun reasonable_date_in_list(dayList: int list, month : int, day : int) =
    if month < 1 orelse month > 12
    then false
    else if day <= 0 orelse day > get_nth_int(dayList, month)
         then false
         else true;

(* Helper function, given a month + day, determines if pair is valid for non-leap years *)
fun reasonable_date_non_leap_year(month : int, day : int) =
    reasonable_date_in_list([31,28,31,30,31,30,31,31,30,31,30,31], month, day);

(* Helper function, given a month + day, determines if pair is valid for leap years *)
fun reasonable_date_leap_year(month : int, day : int) = 
    reasonable_date_in_list([31,29,31,30,31,30,31,31,30,31,30,31], month, day);

fun reasonable_date(date : (int*int*int)) =
    if #1 date <= 0 (* year valid range, must be greater than zero *)
    then false
    else if #2 date < 1 orelse #2 date > 12 (* month valid range, 1 <= m <= 12 *)
         then false
         else (* day valid range, depends on year and month *) 
              if (#1 date mod 400 = 0) orelse ((#1 date mod 4 = 0) andalso not (#1 date mod 100 = 0))
              then reasonable_date_leap_year(#2 date, #3 date)
              else reasonable_date_non_leap_year(#2 date, #3 date);
