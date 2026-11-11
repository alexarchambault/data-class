package dataclass.plugintests

import utest._

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters._
import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}

object TastyTests extends TestSuite {

  /** The TASTy files of the classes compiled with the plugin */
  private def tastyFiles(): List[String] = {
    val dir =
      Paths.get(
        classOf[Typed[?]].getProtectionDomain.getCodeSource.getLocation.toURI
      )
    val stream = Files.walk(dir)
    try
      stream.iterator.asScala
        .map(_.toString)
        .filter(_.endsWith(".tasty"))
        .toList
    finally stream.close()
  }

  val tests = Tests {
    // like Scaladoc does
    test("trees can be read back") {
      val files = tastyFiles()
      assert(files.nonEmpty)
      var read = 0
      val inspector = new Inspector {
        def inspect(using
            quotes: Quotes
        )(tastys: List[Tasty[quotes.type]]): Unit = {
          import quotes.reflect.*
          val traverser = new TreeTraverser {}
          for (tasty <- tastys) {
            traverser.traverseTree(tasty.ast)(Symbol.spliceOwner)
            read += 1
          }
        }
      }
      assert(TastyInspector.inspectTastyFiles(files)(inspector))
      assert(read == files.length)
    }
  }
}
