import java.text.MessageFormat

import com.edawg878.common.MessageFormatter._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class MessageFormatSpec extends UnitSpec {

  "fmt" should "return a MessageFormat" in {
    val format = fmt"Hello"
    format.getClass should be(classOf[MessageFormat])
  }

  "fmt" should "cache MessageFormat" in {
    val result1 = fmt"you have [{0}] {0,choice,0#words|1#word|1<words}"
    val result2 = fmt"you have [{0}] {0,choice,0#words|1#word|1<words}"
    val result3 = fmt"you have [{0}] {0,choice,0#words|1#word|1<words}"
    val result4 = fmt"other"
    assert(result1 eq result2)
    assert(result1 eq result3)
    assert(result1 ne result4)
  }

  "fmt.info" should "colorize output" in {
    val result = fmt"Test [String]".info()
    val expected = PRIMARY + "Test " + SECONDARY + "String" + PRIMARY
    result should equal(expected)
  }

  "fmt.err" should "colorize output" in {
    val result = fmt"Test String".err()
    val expected = ERROR + "Test String"
    result should equal(expected)
  }

  "fmt.info" should "format args" in {
    val arg1 = 1
    val arg2 = 2
    val result = fmt"Testing {0} {1}".info(arg1, arg2)
    val expected = PRIMARY + "Testing " + arg1 + " " + arg2
    result should equal(expected)
  }

  "fmt.err" should "format args" in {
    val arg1 = 1
    val arg2 = 2
    val result = fmt"Testing {0} {1}".err(arg1, arg2)
    val expected = ERROR + "Testing " + arg1 + " " + arg2
    result should equal(expected)
  }

  "fmt.info" should "support choice format" in {
    val format = fmt"{0,choice,0#words|1#word|1<words}"
    format.info(0) should equal(PRIMARY + "words")
    format.info(1) should equal(PRIMARY + "word")
    for (i <- 2 to 10) {
      format.info(i) should equal(PRIMARY + "words")
    }
  }

  "fmt.err" should "support choice format" in {
    val format = fmt"{0,choice,0#words|1#word|1<words}"
    format.err(0) should equal(ERROR + "words")
    format.err(1) should equal(ERROR + "word")
    for (i <- 2 to 10) {
      format.err(i) should equal(ERROR + "words")
    }
  }

}
