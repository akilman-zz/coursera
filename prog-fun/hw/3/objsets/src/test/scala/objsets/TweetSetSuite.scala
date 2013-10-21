package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite { 
  
  trait TestSets {
    val emptySet = new Empty
    
    val aTweet = new Tweet("a", "a body", 20)
    val a = emptySet.incl(aTweet)
    
    val bTweet = new Tweet("b", "b body", 20)
    val ab = a.incl(bTweet)
    
    val cTweet = new Tweet("c", "c body", 7)
    val dTweet = new Tweet("d", "d body", 9)
    
    val abc = ab.incl(cTweet)
    val abd = ab.incl(dTweet)
    val abcd = abc.incl(dTweet)
    
    val eTweet = new Tweet("e", "e body", 42)
    val abcde = abcd.incl(eTweet) 
    
    val fTweet = new Tweet("f", "f body", 43)
    val gTweet = new Tweet("g", "g body", 44)
    val hTweet = new Tweet("h", "h body", 45)
    val fgh = emptySet.incl(fTweet).incl(gTweet).incl(hTweet)
    
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  /*************************************************************************
   * Filter tests
   *************************************************************************/
  
  test("filter: on empty set") {
    new TestSets {
      assert(size(emptySet.filter(tw => tw.user == "a")) === 0)
    }
  }
  
  test("filter: matches none") {
    new TestSets {
      assert(size(abc.filter(tw => tw.user == "z")) === 0)
    }
  }

  test("filter: a on abc") {
    new TestSets {
      assert(size(abcd.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 retweets on abc") {
    new TestSets {
      assert(size(abcd.filter(tw => tw.retweets == 20)) === 2)
    }
  }
  
  test("filter: matches all") {
    new TestSets {
      assert(
        size(
          abc.filter(
            tw => tw.user == "a" || tw.user == "b" || tw.user == "c")) === 3)
    }
  }

  /*************************************************************************
   * Union tests
   *************************************************************************/
  
  test("union: abc and abd") {
    new TestSets {
      
      val myAbcd = abc.union(abd)
      assert(size(myAbcd) === 4)
      
      assert(myAbcd.contains(aTweet))
      assert(myAbcd.contains(bTweet))
      assert(myAbcd.contains(cTweet))
      assert(myAbcd.contains(dTweet))
      
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      
      val myAbcd = abcd.union(emptySet)
      assert(size(myAbcd) === 4)
      
      assert(myAbcd.contains(aTweet))
      assert(myAbcd.contains(bTweet))
      assert(myAbcd.contains(cTweet))
      assert(myAbcd.contains(dTweet))
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      
      val myAbcd = emptySet.union(abcd)
      assert(size(myAbcd) === 4)
      
      assert(myAbcd.contains(aTweet))
      assert(myAbcd.contains(bTweet))
      assert(myAbcd.contains(cTweet))
      assert(myAbcd.contains(dTweet))
    }
  }
  
  test("union: all matching") {
    new TestSets {
      
      val myAbc = abc.union(abc)
      assert(size(myAbc) === 3)
      
      assert(myAbc.contains(aTweet))
      assert(myAbc.contains(bTweet))
      assert(myAbc.contains(cTweet))
    }
  }
  
  test("union: empty union empty") {
    new TestSets {
      val myEmptySet = emptySet.union(emptySet)
      assert(size(emptySet) === 0)
      assert(!myEmptySet.contains(aTweet))
    }
  }
  
  test("union: disjoint set union") {
    new TestSets {
      val mySet = abcde.union(fgh);
      assert(size(mySet) === 8)
      
      assert(mySet.contains(aTweet))
      assert(mySet.contains(bTweet))
      assert(mySet.contains(cTweet))
      assert(mySet.contains(dTweet))
      assert(mySet.contains(eTweet))
      assert(mySet.contains(fTweet))
      assert(mySet.contains(gTweet))
      assert(mySet.contains(hTweet))
    }
  }
  
  /*************************************************************************
   * Most retweeted tests
   *************************************************************************/
  test("mostRetweeted: 1") {
    new TestSets {
      assert(abcde.mostRetweeted.user == "e")
    }
  }
  
  test("mostRetweeted: emptySet") {
    new TestSets {
      try
      {
        val nothing = emptySet.mostRetweeted
        fail()
      }
      catch 
      {
        case e: java.util.NoSuchElementException => assert(true)
      }
    }
  }
  
  test("mostRetweeted: single element set") {
    new TestSets {
      assert(a.mostRetweeted.user == "a")
    }
  }
  
  test("mostRetweeted: bigger set") {
    new TestSets {
      assert(abcde.union(fgh).mostRetweeted.user == "h")
    }
  }

  /*************************************************************************
   * Descending tweets
   *************************************************************************/
  test("descending: abcd") { // a | b, d, c
    new TestSets {
      val trends = abcd.descendingByRetweet
      
      // a | b
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.head.retweets === 20)
      
      // to skip next a | b
      val otherList = trends.tail
      assert(!otherList.isEmpty)
      assert(otherList.head.user == "a" || otherList.head.user == "b")
      assert(otherList.head.retweets === 20)
      
      // d > c
      val dList = otherList.tail
      assert(!dList.isEmpty)
      assert(dList.head.user == "d")
      assert(dList.head.retweets === 9)

      // c
      val cList = dList.tail
      assert(!dList.isEmpty)
      assert(cList.head.user == "c")
      assert(cList.head.retweets === 7)
    }
  }
  
  test("descending: emptySet") {
    new TestSets {
      val trends = emptySet.descendingByRetweet
      assert(trends.isEmpty)
    }
  }
  
  
}
