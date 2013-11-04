(* Coursera Programming Languages, Homework 3, Provided Code *)



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 
   Given a list of strings, a list of strings is returned such that 
   only those which start with a capital letter are returned 
*)
fun only_capitals (xs) = 
    List.filter(fn s => 
		   Char.isUpper(String.sub(s, 0))) xs;


(*  
   Given a list of strings, returns the longest where the first is
   returned in instances of a tie.
*)
fun longest_string1 (xs) = 
    List.foldl(fn (x,biggest) => 
		  if String.size(x) > String.size(biggest) 
		  then x 
		  else biggest) "" xs; 


(*
   Given a list of strings, returns the longest where the last is
   return in instances of a tie.    
*)
fun longest_string2 (xs) = 
    List.foldl(fn (x,biggest) => 
		  if String.size(x) >= String.size(biggest) 
		  then x 
		  else biggest) "" xs; 


(* 
   Applies a function 'f' via foldl to help determine the biggest
   string
*)
fun longest_string_helper f xs =
    List.foldl(fn (x,biggest) =>
		  if f(String.size(x), String.size(biggest))
		  then x
	          else biggest) "" xs;


(*
   Use the helper function such that behavior is the same as 
   longest_string1
*)
fun longest_string3 xs = longest_string_helper(fn (x,y) => x > y) xs;


(*
   Use the helper function such that behavior is the same as 
   longest_string2
*)
fun longest_string4 xs = longest_string_helper(fn (x,y) => x >= y) xs;

(*
   Given xs, returns the longest string that starts with a capital
   letter
*)
val longest_capitalized = fn xs => (longest_string1 o only_capitals) xs;

(*
   Reverses a string using a composition of ML library fucntions
*)
fun rev_string s = (String.implode o rev  o String.explode) s;

exception NoAnswer;

(*
   Given xs, returns the first entry which 'predicate' returns true
*)
fun first_answer predicate xs =
    case xs of
	[] => raise NoAnswer
     | x :: xs' => case predicate(x) of
		       SOME x' => x'
		    | NONE => first_answer predicate xs';
    
(*
   Given xs, returns all answers for which 'predicate' returns true
*)
fun all_answers predicate xs =
    let fun loop predicate (xs, acc) =
	    case xs of
		[] => SOME acc
	      | x :: xs' => case predicate(x) of
 				SOME x' => loop predicate (xs', acc @ x')
			     | NONE => NONE
    in
	loop predicate (xs, [])
    end;	     

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern;

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu;

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;


(*
   Given a pattern 'p', return the number of wildcards, using the 
   provided helper function 'g'
*)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p;


(*
   Given a pattern 'p', return the sum of the number of wildcards, and
   the length of included variables, using the provided helper 
   function 'g'
*)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn s => String.size(s)) p;



(*
   Given a pattern 'p' and string 's', return the number of variables
   which match string 's'
*)
fun count_some_var (s, p) =
    g (fn () => 0) (fn s' => if s = s' then 1 else 0) p;


(*
   Given a pattern 'p' returns a true iff all variables in the pattern
   are distinct
*)
fun check_pat p = 
    let fun get_var_strings (p, acc) =
	    case p of 
		Variable x => x :: acc
	     | TupleP ps => (List.foldl (fn (p', i) => get_var_strings(p', i)) acc ps) @ acc
	     | ConstructorP(_,p') => get_var_strings(p', acc)
	     | _ => []
			
        fun unique_check xs = 
	    case xs of
		[] => true
	     | x :: xs' => if List.exists(fn x' => x = x') xs'
			   then false
			   else unique_check(xs')
    in 
	(unique_check o get_var_strings) (p, [])
    end;


(* 
   Given a (value, pattern) pair, return an option list of bindings
*)
fun match (value, pattern) =
    case (pattern, value) of
	(Wildcard, _) => SOME []
     | (Variable(s), v) => SOME[(s,v)]
     | (UnitP, Unit) => SOME[]
     | (ConstP n, Const n') => 
       if n = n' then SOME[] else NONE
     | (ConstructorP (s1, p), Constructor (s2, v)) => 
       if s1 = s2 then match(v, p) else NONE 
     | (TupleP (ps), Tuple (vs)) => 
       if List.length ps = List.length vs
       then all_answers match (ListPair.zip(vs, ps))
       else NONE
     | _ => NONE;


(*
   Given a value and a list of patterns, return an option which 
   contains the bindings of value to the first matching pattern in 
   the list
*)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE;
