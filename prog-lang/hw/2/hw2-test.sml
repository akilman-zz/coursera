exception AssertionErrorException of string

fun assertTrue(name: string, expr: bool) =
  if expr
    then true
    else raise (AssertionErrorException name);

fun assertFalse(name: string, expr: bool) =
  if expr
    then raise (AssertionErrorException name)
    else true;

use "hw2.sml";

print("all_except_option test cases");
assertTrue("empty list", all_except_option("a", []) = NONE);
assertTrue("start removal", all_except_option("a", ["a", "b", "c"]) = SOME(["b", "c"]));
assertTrue("middle removal", all_except_option("b", ["a", "b", "c"]) = SOME(["a", "c"]));
assertTrue("end removal", all_except_option("c", ["a", "b", "c"]) = SOME(["a", "b"]));
assertTrue("no match", all_except_option("z", ["a", "b", "c"]) = NONE);
assertTrue("sample 1", all_except_option("string", ["string"]) = SOME []);

print("get_substitutions1 test cases");
assertTrue("empty list", get_substitutions1([], "Fred") = []);
assertTrue("sample 1", 
	   get_substitutions1(
	       [
		 ["Fred", "Fredrick"], 
		 ["Elizabeth", "Betty"], 
		 ["Freddie", "Fred", "F"]
	       ], "Fred") = ["Fredrick", "Freddie", "F"]);

assertTrue("sample 2", 
	   get_substitutions1(
	       [
		 ["Fred", "Fredrick"], 
		 ["Jeff", "Jeffrey"], 
		 ["Geoff", "Jeff", "Jeffrey"]
	       ], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]);
assertTrue("sample 3", get_substitutions1([["foo"],["there"]], "foo") = []);



print("get_substitutions2 test cases");
assertTrue("empty list", get_substitutions2([], "Fred") = []);

assertTrue("sample 1", 
	   get_substitutions2(
	       [
		 ["Fred", "Fredrick"], 
		 ["Elizabeth", "Betty"], 
		 ["Freddie", "Fred", "F"]
	       ], "Fred") = ["Fredrick", "Freddie", "F"]);

assertTrue("sample 2", 
	   get_substitutions2(
	       [
		 ["Fred", "Fredrick"], 
		 ["Jeff", "Jeffrey"], 
		 ["Geoff", "Jeff", "Jeffrey"]
	       ], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]);
assertTrue("sample 3", get_substitutions2([["foo"],["there"]], "foo") = []);


print("similar_names test cases");
assertTrue("sample 1", 
	   similar_names(
	       [
		 ["Fred", "Fredrick"], 
		 ["Elizabeth", "Betty"], 
		 ["Freddie", "Fred", "F"]
	       ], 
	       {first="Fred", middle="W", last="Smith"}) = 
	   [
	     {first="Fred",last="Smith",middle="W"}, 
	     {first="Fredrick",last="Smith",middle="W"}, 
	     {first="Freddie",last="Smith",middle="W"}, 
	     {first="F",last="Smith",middle="W"}
	  ]);

assertTrue("empty substitution list",
	   similar_names(
	       [],
	       {first="Anthony", middle="S", last="Kilman"}) =
	   [{first="Anthony", middle="S", last="Kilman"}]);

print("card_color test cases");
assertTrue("red 1", card_color((Hearts, Num(2))) = Red);
assertTrue("red 2", card_color((Diamonds, Num(4))) = Red);
assertTrue("black 1", card_color((Spades, King)) = Black);
assertTrue("black 2", card_color((Clubs, Queen)) = Black);
assertTrue("sample 1", card_color((Clubs, Num 2)) = Black);

print("card_value test cases");
assertTrue("ace", card_value((Hearts, Ace)) = 11);
assertTrue("king", card_value((Clubs, King)) = 10);
assertTrue("queen", card_value((Spades, Queen)) = 10);
assertTrue("number", card_value((Diamonds, Num(9))) = 9);
assertTrue("sample 1", card_value((Clubs, Num 2)) = 2);

print("remove_card test cases");
(* Manually tested exception case as I'm not sure how to handle locally yet  *)
assertTrue("sample 1", remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []);
assertTrue("card 1", 
	   remove_card(
	       [
		 (Hearts, Num(2)), 
		 (Hearts, Num(3)),
		 (Hearts, Num(4))
	       ], 
	       (Hearts, Num(2)), IllegalMove) = [(Hearts, Num(3)), (Hearts, Num(4))]);

assertTrue("card 1", 
	   remove_card(
	        [
		 (Hearts, Num(2)), 
		 (Hearts, Num(3)),
		 (Hearts, Num(4))
	       ], 
	       (Hearts, Num(3)), IllegalMove) = [(Hearts, Num(2)), (Hearts, Num(4))]);


print("all_same_color test cases");
assertTrue("sample 1", all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true);
assertTrue("all red", 
	   all_same_color(
	       [
		 (Hearts, Num(2)), 
		 (Hearts, Num(3)),
		 (Diamonds, Num(4))
	       ]) = true);

assertTrue("all black", 
	   all_same_color(
	       [
		 (Clubs, Num(2)), 
		 (Spades, Num(3)),
		 (Spades, Num(4))
	       ]) = true);

assertTrue("mixed", 
	   all_same_color(
	       [
		 (Clubs, Num(2)), 
		 (Hearts, Num(3)),
		 (Hearts, Num(4))
	       ]) = false);


print("sum_cards test cases");

assertTrue("sample 1", sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4);
assertTrue("empty list", sum_cards([]) = 0);
assertTrue("single element list", sum_cards([(Spades, Ace)]) = 11);
assertTrue("multi element list",
	   sum_cards(
	       [
		 (Clubs, Num(2)), 
		 (Hearts, Num(3)),
		 (Hearts, Num(4))
	       ]) = 9);

print("score test cases");

assertTrue("no cards", score([], 42) = 21);
assertTrue("single card below goal", score([(Spades, Ace)], 42) = 15);
assertTrue("single card above goal", score([(Spades, Ace)], 10) = 1);
assertTrue("Multi card below goal same color", score([(Spades, Num(2)), (Clubs, Num(2))], 5) = 0);
assertTrue("Multi card below goal diff color", score([(Spades, Num(2)), (Hearts, Num(2))], 5) = 1);
assertTrue("Multi card above goal same color", score([(Spades, Num(2)), (Clubs, Num(2))], 3) = 1);
assertTrue("Multi card above goal diff color", score([(Spades, Num(2)), (Hearts, Num(2))], 3) = 3);
assertTrue("sample 1", score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4);

print("officiate test cases");
assertTrue("sample 1", officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6);
assertTrue("sample 2", officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3);
assertTrue("sample 3", ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true));
