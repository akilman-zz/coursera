(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

exception AssertionErrorException of string

fun assertTrue(name: string, expr: bool) =
  if expr
    then true
    else raise (AssertionErrorException name);

fun assertFalse(name: string, expr: bool) =
  if expr
    then raise (AssertionErrorException name)
    else true;

use "hw3.sml";

print("only_capitals");
assertTrue("sample test 1", only_capitals ["A","B","C"] = ["A","B","C"]);
assertTrue("some caps", only_capitals ["a", "b", "C"] = ["C"]);
assertTrue("empty list", only_capitals [] = []);

print("longest_string1");
assertTrue("test 1", longest_string1 ["A","bc","C"] = "bc");
assertTrue("all same length", longest_string1 ["a", "b", "c"] = "a");

print("longest_string2");
assertTrue("test 2", longest_string2 ["A","bc","C"] = "bc");
assertTrue("all same length", longest_string2 ["a", "b", "c"] = "c");

print("longest_string3");
assertTrue("test 1", longest_string3 ["A","bc","C"] = "bc");
assertTrue("all same length", longest_string3 ["a", "b", "c"] = "a");

print("longest_string4");
assertTrue("test 2", longest_string4 ["A","bc","C"] = "bc");
assertTrue("all same length", longest_string4 ["a", "b", "c"] = "c");

print("longest capitalized");
assertTrue("test 1", longest_capitalized ["A","bc","C"] = "A");
assertTrue("trim long non caps", longest_capitalized ["A", "B", "C", "aaaaaaaaaaaaa"] = "A");
assertTrue("proper caps longest", longest_capitalized ["A", "Bbbb", "Ccc"] = "Bbbb");

print("rev_string");
assertTrue("test 1", rev_string "abc" = "cba");
assertTrue("empty string", rev_string "" = "");
assertTrue("1 char string", rev_string "a" = "a");
assertTrue("larger string", rev_string "abcdef" = "fedcba");

print("first answer");
assertTrue("test 1", first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4);

print("all_answers");
assertTrue("test 1", all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE);
assertTrue("test 2", all_answers (fn x => if x > 0 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]);
 
print("count_wildcards");
assertTrue("test 1", count_wildcards Wildcard = 1);
assertTrue("multiple wildcards", count_wildcards (TupleP [Wildcard, Wildcard]) = 2);
assertTrue("no wildcards", count_wildcards (UnitP) = 0);

print("count_wild_and_variable_lengths");
assertTrue("test 1", count_wild_and_variable_lengths (Variable("a")) = 1);
assertTrue("mixed", count_wild_and_variable_lengths (TupleP [Wildcard, Variable("abc")]) = 4);

print("count_some_var");
assertTrue("test 1", count_some_var ("x", Variable("x")) = 1);
assertTrue("multiple", count_some_var ("x", TupleP [Wildcard, Variable("x")]) = 1);
assertTrue("multiple matches", count_some_var ("y", TupleP [Variable("y"), Variable("y"), Wildcard]) = 2); 

print("check_pat");
assertTrue("test 1", check_pat (Variable("x")) = true);
assertTrue("multi distinct", check_pat (TupleP [Variable("a"), Variable("b")]));
assertFalse("multi same", check_pat (TupleP [Variable("a"), Variable("a")]));

print("match");
assertTrue("test 1", match (Const(1), UnitP) = NONE);
assertTrue("wildcard", match (Unit, Wildcard) = SOME []);
assertTrue("variable", match (Unit, Variable("a")) = SOME [("a",Unit)]);
assertTrue("unitp", match (Unit, UnitP) = SOME []);
assertTrue("const positive", match (Const(4), ConstP(4)) = SOME[]);
assertTrue("const negative", match (Const(4), ConstP(2)) = NONE);
assertTrue("constructor positive", match (Constructor("ctor", Unit), ConstructorP("ctor", UnitP)) = SOME[]);
assertTrue("constructor negative", match (Constructor("ctor", Unit), ConstructorP("meh", UnitP)) = NONE);
assertTrue("tuple positive", match (Tuple [Const(2), Const(4)], TupleP[ConstP(2), ConstP(4)]) = SOME[]);
assertTrue("tuple negative", match (Tuple [Const(2), Const(4)], TupleP[ConstP(2)]) = NONE);

print("first_match");
assertTrue("test 1", first_match Unit [UnitP] = SOME []);

