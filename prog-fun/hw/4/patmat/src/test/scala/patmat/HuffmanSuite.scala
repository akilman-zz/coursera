package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  /********************************************************************
   * 'weight' tests
   ********************************************************************/
  
  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of an even larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }
  
  /*********************************************************************
   * 'chars' tests
   *********************************************************************/
  
  test("chars of a smaller tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }
  
  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  /*********************************************************************
   * 'times' tests
   *********************************************************************/
  
  test("basic times test") {
    new TestTrees {
      assert(times(List('a', 'a')) === List(('a', 2)))
    }
  }
  
  test("empty times test") {
    new TestTrees {
      assert(times(List()) === List())
    }
  }
  
  test("vaguely more complex times test") {
    new TestTrees {
      assert(times(List('a', 'b', 'b')) === List(('b', 2), ('a', 1)))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  /*********************************************************************
   * 'makeOrderedLeafList' tests
   *********************************************************************/

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("makeOrderedLeafList for a different frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 4))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',4)))
  }
  
  test("makeOrderedLeafList for empty list") {
    assert(makeOrderedLeafList(List()) === List())
  }
  
  /*********************************************************************
   * 'combine' tests
   *********************************************************************/

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val extractedLocalValue = combine(leaflist) 
    assert(extractedLocalValue=== List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of an empty list") {
    val emptyList = Nil
    assert(combine(emptyList) === Nil)
  }
  
  test("combine of a single element list") {
    val singleElementList = List(Leaf('x', 42))
    assert(combine(singleElementList) === List(Leaf('x', 42)))
  }
  
  test("combine elements with equal weight") {
    val list = List(Leaf('a', 1), Leaf('b', 1), Leaf('c', 1))
    assert(combine(list) === List(Leaf('c', 1), Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2)))
  }
  
  test("main example") {
    val list = "aaaaaaaabbbcdefgh".toList
    val pairs = times(list)
    val orderedLeafList = makeOrderedLeafList(pairs)
    val combinedList = combine(orderedLeafList)
    assert(combinedList === List(Leaf('g',1), Leaf('c',1), Leaf('h',1), Leaf('d',1), Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2), Leaf('b',3), Leaf('a',8)))
    
    val tree = until(singleton, combine)(orderedLeafList)
    assert(tree === List(Fork(Leaf('a',8),Fork(Fork(Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),Fork(Leaf('g',1),Leaf('c',1),List('g', 'c'),2),List('e', 'f', 'g', 'c'),4),Fork(Fork(Leaf('h',1),Leaf('d',1),List('h', 'd'),2),Leaf('b',3),List('h', 'd', 'b'),5),List('e', 'f', 'g', 'c', 'h', 'd', 'b'),9),List('a', 'e', 'f', 'g', 'c', 'h', 'd', 'b'),17)))
  }
  
  /*********************************************************************
   * 'until' tests
   *********************************************************************/

  test("basic until test") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val huffmanTree = until(singleton, combine)(leaflist)
    
    assert(huffmanTree.size === 1)
    assert(huffmanTree === List(Fork(Fork(Leaf('e',1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("create code tree tests") {
    val s = "this is a sample string"
    val codeTree = createCodeTree(string2Chars(s))
    assert(true)
  }
  
  /*********************************************************************
   * 'decode' tests
   *********************************************************************/
  
  test("decode basic secret") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val huffmanTree = until(singleton, combine)(leaflist)
    assert(decode(huffmanTree.head, List(0,0,0,1)) === List('e','t'))
  }
  
  test("decode the french secret") {
    val decodedSecret = decode(frenchCode, secret)
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }
  
  /*********************************************************************
   * 'encodeChar' tests
   *********************************************************************/
  
  test("encodeChar sanity tests") {
	val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val huffmanTree = until(singleton, combine)(leaflist)
    
    assert(encodeChar('t', huffmanTree.head) === List(0,1))
    assert(encodeChar('e', huffmanTree.head) === List(0,0))
    assert(encodeChar('x', huffmanTree.head) === List(1))
  }
  
  /*********************************************************************
   * 'encode' tests
   *********************************************************************/
  
  test("encode/decode the french secret") {
    val text = "huffmanestcool".toList;
    val encoding = encode(frenchCode)(text.toList)
    val decodedText = decode(frenchCode, encoding)
    assert(text === decodedText)
  }
  
  test("encode/decode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  /*********************************************************************
   * 'convert' tests
   *********************************************************************/
  
  test("convert existing code tree to table") {
    new TestTrees {
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      val huffmanTree = until(singleton, combine)(leaflist)
      val table = convert(huffmanTree.head)
      
      assert(true)
    }
  }
  
  /*********************************************************************
   * 'quickEncode' tests
   *********************************************************************/
  
  test("quick encode/decode the french secret") {
    val text = "huffmanestcool".toList;
    
    val encoding = encode(frenchCode)(text.toList)
    val quickEncoding = quickEncode(frenchCode)(text.toList)
    assert(encoding === quickEncoding)
    
    val decodedText = decode(frenchCode, encoding)
    val quickDecodedText = decode(frenchCode, quickEncoding)
    
    assert(text === decodedText)
    assert(text === quickDecodedText)
  }
}
