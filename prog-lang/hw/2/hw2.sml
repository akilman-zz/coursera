(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* (1a) Removes a element 'x' from the argument list. It is assumed that 'x' appears in the list at most once  *)
fun all_except_option (x, []) = NONE
  | all_except_option (x, x'::xs) =
    case same_string(x, x') of
       true  => SOME xs
     | false => case all_except_option(x, xs) of
                  NONE     => NONE
	        | SOME xs' => SOME(x' :: xs'); 


(* (1b) Returns a string list of substitutions given a single string argument *)
fun get_substitutions1 ([], s) = []
  | get_substitutions1 (x :: xs, s) =
    case all_except_option(s, x) of
	NONE    => get_substitutions1(xs, s)
     |  SOME x' => x' @ get_substitutions1(xs, s);


(* (1c) Same as (1b) though now it should use a tail-recursive helper function *)
fun get_substitutions2 (xs, s) =
    let fun f (xs, s, acc) = 
	    case xs of
		[]       => acc
	     | x' :: xs' => case all_except_option(s, x') of
				NONE    => f (xs', s, acc)
			     | SOME x'' => f (xs', s, acc @ x'')
    in
	f (xs, s, [])
    end;


(* (1d) Given a substitution list and a name redcord, return a list of records with all possible
substitutions of the first name *)
fun similar_names (substitutions, 
		   name_record : {first:string, middle:string, last:string}) =
    let fun f (first_name_list, middle_name, last_name) =
	    case first_name_list of
		[] => []
	     | x :: xs => 
	       {first=x, middle=middle_name, last=last_name} :: f (xs, middle_name, last_name) 
     in
	 case name_record of
	     {first, middle, last} => {first=first, middle=middle, last=last} 
				      :: f (get_substitutions2(substitutions, first), middle, last)
     end;					 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Normally I'd comment each method, but for the first few methods they're really self-explanatory *)

fun card_color c = 
    case c of
	(Clubs, _) => Black
     | (Spades, _) => Black
     | (Hearts, _) => Red
     | (Diamonds, _) => Red;

fun card_value c = 
    case c of 
	(_, Ace) => 11
     | (_, King) => 10
     | (_, Queen) => 10
     | (_, Jack) => 10
     | (_, Num(x)) => x;

(* Removes a card from the given list, 'cs' *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
     | c' :: cs' => 
       if c = c' 
       then cs' 
       else c' :: remove_card(cs', c, e);

(* Returns a boolean indicating if all cards in 'cs' are the same color *)
fun all_same_color (cs) =
    case cs of
	[] => true
     | _ :: [] => true
     | c :: c' :: cs' => card_color(c) = card_color(c') 
			 andalso all_same_color(c' :: cs');
(* Returns the sum of card values in 'cs' *)
fun sum_cards (cs) =
    let fun f (cs, acc) =
	case cs of 
	    [] => acc
	 | c :: cs' => f(cs', acc + card_value(c))
    in
	f (cs, 0)
    end;

(* Scores a set of cards, 'cs' according to the criteria set  forth by the assignment *)
fun score (cs, goal) =
    let val sum = sum_cards(cs)
	val preliminary_score = 
	    if sum > goal 
            then 3 * (sum - goal) 
	    else goal - sum 
    in
	if all_same_color(cs)
	then preliminary_score div 2
        else preliminary_score
    end;

(* 'Officiate[s]' a game given a card set 'cs', a set of  moves 'ms', and a given integer 'goal'  *)
fun officiate (cs, ms, goal) =
    let fun f (cs, ms, held) =
	    case (cs, ms, held) of 
		(_, [], held) => score(held, goal) (* no moves left  *)
	     | ([], _, held) => score(held, goal) (* no cards left *) 
	     | (c :: cs', Draw :: ms', held) => if sum_cards(c :: held) > goal (* draw *)
						then score(c :: held, goal)
						else f(cs', ms', c :: held)
	     | (cs', Discard(c) :: ms', held) => (* discard *)
	        f(cs', ms', remove_card(held, c, IllegalMove))
    in
        f(cs, ms, [])
    end;
