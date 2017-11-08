package org.goldenport.xml

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._

/*
 * @since   Nov.  8, 2017
 * @version Nov.  8, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class XmlUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import XmlUtils._

  "adjustEmptyDiv" should {
    "typical" which {
      "no div" in {
        adjustEmptyDiv(<span>a</span>) should be(<span>a</span>)
      }
      "double empty" in {
        adjustEmptyDiv(<div><div>a</div></div>) should be(<div>a</div>)
      }
      "triple empty" in {
        adjustEmptyDiv(<div><div><div>a</div></div></div>) should be(<div>a</div>)
      }
      "deep div" in {
        adjustEmptyDiv(<div><div><div x="X">a</div></div></div>) should be(<div x="X">a</div>)
      }
      "empty class and deep div" in {
        adjustEmptyDiv(<div class=""><div class=""><div x="X">a</div></div></div>) should be(<div x="X">a</div>)
      }
      "empty class and intermidiate empty div" in {
        adjustEmptyDiv(<div a="A"><div class=""><div x="X">a</div></div></div>) should be(<div a="A"><div x="X">a</div></div>)
      }
      "empty class and intermidiate empty div2" in {
        adjustEmptyDiv(<div a="A"><div class=""><div x="X">a</div><div y="Y">a</div></div></div>).toString should be(<div a="A"><div x="X">a</div><div y="Y">a</div></div>.toString)
      }
    }
  }
}
