package definiti.tests.parser

import definiti.core.ast.Location
import definiti.core.{Error, SimpleError}
import org.antlr.v4.runtime.{BaseErrorListener, RecognitionException, Recognizer}

import scala.collection.mutable.ListBuffer

private[tests] case class ErrorItem(file: String, line: Int, column: Int, msg: String) {
  def prettyPrint: String = {
    s"""Error on file $file position $line-$column: $msg"""
  }

  def toError: Error = SimpleError(prettyPrint)
}

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
private[tests] class ErrorListener(origin: Location) extends BaseErrorListener {
  private val errorsBuffer = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    errorsBuffer.append(ErrorItem(origin.file, origin.range.start.line + line, charPositionInLine, msg))
  }

  def log(): Unit = errorsBuffer.foreach(System.err.println)

  def hasError: Boolean = errorsBuffer.nonEmpty

  def errors: Seq[ErrorItem] = errorsBuffer
}