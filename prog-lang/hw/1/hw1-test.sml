exception AssertionErrorException of string

fun assertTrue(name: string, expr: bool) =
  if expr
    then true
    else raise (AssertionErrorException name);

fun assertFalse(name: string, expr: bool) =
  if expr
    then raise (AssertionErrorException name)
    else true;

use "hw1.sml";

print("is_older test cases");
assertTrue("older - by year", is_older((1,1,1), (2,1,1)));
assertTrue("older - by month", is_older((1,1,1), (1,2,1)));
assertTrue("older - by day", is_older((1,1,1), (1,1,2)));
assertFalse("not older - by year", is_older((2,1,1), (1,1,1)));
assertFalse("not older - by month", is_older((1,2,1), (1,1,1)));
assertFalse("not older - by day", is_older((1,1,2), (1,1,1)));

print("number_in_month test cases");
assertTrue("Empty list case", number_in_month([], 1) = 0);
assertTrue("Single list element match", number_in_month([(1,2,3)], 2) = 1);
assertTrue("Multi element list, single match", number_in_month([(1,2,3),(4,5,6)], 2) = 1);
assertTrue("Multi element list, multi-match", number_in_month([(1,2,3),(4,2,6)], 2) = 2);
assertTrue("Multi element list, no matches", number_in_month([(1,2,3),(4,5,6)], 4) = 0);

print("number_in_months test cases");
assertTrue("Empty month list", number_in_months([(1,2,3)],[]) = 0);
assertTrue("Single date, match", number_in_months([(1,2,3)], [2]) = 1);
assertTrue("Single date, no match", number_in_months([(1,2,3)], [1]) = 0);
assertTrue("Multiple dates, multiple matches", number_in_months([(1,2,3), (4,2,5), (3,1,4)], [1,2]) = 3);
assertTrue("Multiple dates, no matches", number_in_months([(1,2,3), (4,2,5), (3,1,4)], [7,9]) = 0);

print("dates_in_month test cases");
assertTrue("No matches", dates_in_month([(1,2,3)], 4) = []);
assertTrue("Single match", dates_in_month([(1,2,3)], 2) = [(1,2,3)]);
assertTrue("Multi match", dates_in_month([(1,2,3),(4,3,5), (5,2,7)], 2) = [(1,2,3),(5,2,7)]);
assertTrue("Empty date set", dates_in_month([], 1) = []);

print("dates_in_months test cases");
assertTrue("No matches", dates_in_months([(1,2,3)], [1]) = []);
assertTrue("Single match", dates_in_months([(1,2,3)], [2]) = [(1,2,3)]);
assertTrue("Multi match", dates_in_months([(1,2,3), (4,5,6), (7,2,9)], [2]) = [(1,2,3), (7,2,9)]);
assertTrue("Multi match, multi-month", dates_in_months([(1,2,3), (4,5,6), (7,8,9)], [2,5,8]) = [(1,2,3), (4,5,6), (7,8,9)]);
assertTrue("No match, multi month", dates_in_months([(1,2,3), (4,5,6), (7,8,9)], [9, 12, 42]) = []);

print("get_nth test cases");
assertTrue("first", get_nth(["a", "b", "c"], 1) = "a");
assertTrue("second", get_nth(["a", "b", "c"], 2) = "b");
assertTrue("third", get_nth(["a", "b", "c"], 3) = "c");

print("date_to_string test cases");
assertTrue("test 1", date_to_string(2013, 1, 1) = "January 1, 2013");
assertTrue("test 2", date_to_string(2013, 12, 1) = "December 1, 2013");
assertTrue("test 3", date_to_string(2013, 6, 25) = "June 25, 2013");

print("number_before_reaching_sum test cases");
assertTrue("basic list", number_before_reaching_sum(3, [1,2,3]) = 1);
assertTrue("larger list", number_before_reaching_sum(15, [1,2,3,4,5,6]) = 4);
assertTrue("no count required", number_before_reaching_sum(1, [1,2,3,4,5]) = 0);

print("what_month test cases");
assertTrue("Today", what_month(282) = 10);
assertTrue("Jan 2", what_month(2) = 1);
assertTrue("Dec 31", what_month(365) = 12);

print("month_range test cases");
assertTrue("jan-feb", month_range(31,32) = [1,2]);
assertTrue("nov, dec, dec", month_range(334,336) = [11, 12, 12]);
assertTrue("same day case", month_range(1,1) = [1]);
assertTrue("reversed order case", month_range(2,1) = []);

print("oldest test cases");
assertTrue("empty date list", oldest([]) = NONE);
assertTrue("single entry", oldest([(1,1,1)]) = SOME((1,1,1)));
assertTrue("multiple entries", oldest([(12,1,1), (11,2,3), (1,1,1), (42,42,42)]) = SOME((1,1,1)));

print("in_list test cases");
assertFalse("emtpy list", in_list([], 1));
assertTrue("single element match", in_list([1], 1));
assertFalse("single element, no match", in_list([1], 2));
assertTrue("multi element match", in_list([1,2,3,4,5], 5));
assertFalse("multi element match", in_list([1,2,3,4,5], 6));

print("de_dup test cases");
assertTrue("empty list", de_dup([]) = []);
assertTrue("single element list", de_dup([1]) = [1]);
assertTrue("all dups", de_dup([1,1,1,1,1]) = [1]);
assertTrue("mixed case", de_dup([1,1,1,2,3,3,3,4,5,5,5]) = [1,2,3,4,5]);

print("number in months challenge question");
assertTrue("empty month list", number_in_months_challenge([(1,2,3)], []) = 0);
assertTrue("single element month list", number_in_months_challenge([(1,2,3)], [2]) = 1);
assertTrue("single element month list with dups", number_in_months_challenge([(1,2,3)], [2,2,2,2,2]) = 1);

print("dates in months challenge question");
assertTrue("empty month list", dates_in_months_challenge([(1,2,3)], []) = []);
assertTrue("single element month list", dates_in_months_challenge([(1,2,3)], [2]) = [(1,2,3)]);
assertTrue("single element month list with dups", dates_in_months_challenge([(1,2,3)], [2,2,2]) = [(1,2,3)]);

print("reasonable date test cases");
assertFalse("negative year", reasonable_date((~1,1,1)));
assertFalse("zero year", reasonable_date((0,1,1)));
assertTrue("positive year, month, and day", reasonable_date((1,1,1)));
assertFalse("negative month", reasonable_date((1,~1,1)));
assertFalse("zero month", reasonable_date((1,0,1)));
assertFalse("negative day", reasonable_date((1,1,~1)));
assertFalse("zero day", reasonable_date((1,1,0)));
assertFalse("day too high on leap year", reasonable_date((1,2,29)));
assertFalse("day too high in non leap year", reasonable_date((1,1,32)));
assertFalse("day too high in non leap year 2", reasonable_date((1,9,31)));
assertFalse("month too high", reasonable_date((1,13,1)));
assertTrue("feb 29, 400", reasonable_date((400,2,29)));
assertFalse("feb 29, 100", reasonable_date((100,2,29)));
assertTrue("feb 29, 4", reasonable_date((4, 2, 29)));
