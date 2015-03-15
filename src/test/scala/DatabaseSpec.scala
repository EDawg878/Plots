import java.util.UUID

import com.edawg878.common.Database.BSONHandlers
import reactivemongo.bson.BSON

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
class DatabaseSpec extends UnitSpec with BSONHandlers {

  "BSON UUID handler" should "be transitive" in {
    for (i <- 1 to 100) {
      val before = UUID.randomUUID()
      val binary = BSON.write(before)
      val after = BSON.read(binary)
      before should equal(after)
    }
  }

}
