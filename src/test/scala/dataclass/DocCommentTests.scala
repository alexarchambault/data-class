package dataclass

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.tools.nsc.doc.{DocFactory, Settings}
import scala.tools.nsc.doc.model.DocTemplateEntity
import scala.tools.nsc.reporters.ConsoleReporter

import utest._

object DocCommentTests extends TestSuite {

  private val source =
    """package doctest
      |
      |import dataclass.data
      |
      |/** Top doc */
      |@data class Top(a: Int)
      |
      |object Wrapper {
      |
      |  /** Nested doc */
      |  @data class Nested(a: Int)
      |
      |  /** Nested case class doc */
      |  @data case class NestedCase(a: Int)
      |
      |  /** With companion doc */
      |  @data class WithCompanion(a: Int)
      |  object WithCompanion
      |
      |  object Inner {
      |    /** Deeper doc */
      |    @data class Deeper[T](t: T)
      |  }
      |}
      |""".stripMargin

  /** The doc comments of the classes of `source`, as Scaladoc sees them */
  private lazy val comments: Map[String, String] = {
    // passed by the build: classpath with the data-class macros, and the
    // options enabling macro annotations
    val classpath = sys.props("dataclass.test.docClasspath")
    val options = sys.props("dataclass.test.docScalacOptions").split(' ').toList

    val dir = Files.createTempDirectory("data-class-doc-tests")
    val file = dir.resolve("Source.scala")
    val universe =
      try {
        Files.write(file, source.getBytes(StandardCharsets.UTF_8))
        val settings = new Settings(msg => throw new Exception(msg))
        settings.classpath.value = classpath
        // nothing gets written there, only the Scaladoc model is computed
        settings.outdir.value = dir.toString
        val (ok, _) = settings.processArguments(options, processAll = true)
        assert(ok)
        val reporter = new ConsoleReporter(settings)
        val universe = new DocFactory(reporter, settings)
          .makeUniverse(Left(List(file.toString)))
          .getOrElse(throw new Exception("Scaladoc failed"))
        assert(!reporter.hasErrors)
        universe
      } finally {
        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
      }

    def templates(entity: DocTemplateEntity): Seq[DocTemplateEntity] =
      entity +: entity.templates
        .collect { case t: DocTemplateEntity => t }
        .flatMap(templates)

    templates(universe.rootPackage)
      .filter(_.isClass)
      .map { cls =>
        cls.qualifiedName -> cls.comment.fold("")(_.short.toString)
      }
      .toMap
  }

  private def checkComment(className: String, expected: String): Unit = {
    val comment = comments.getOrElse(className, "")
    assert(comment.contains(expected))
  }

  val tests = Tests {
    test("top-level class") {
      checkComment("doctest.Top", "Top doc")
    }
    test("nested class") {
      checkComment("doctest.Wrapper.Nested", "Nested doc")
    }
    test("nested case class") {
      checkComment("doctest.Wrapper.NestedCase", "Nested case class doc")
    }
    test("nested class with companion") {
      checkComment("doctest.Wrapper.WithCompanion", "With companion doc")
    }
    test("class nested deeper") {
      checkComment("doctest.Wrapper.Inner.Deeper", "Deeper doc")
    }
  }
}
