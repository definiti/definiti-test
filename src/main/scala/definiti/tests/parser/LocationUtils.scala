package definiti.tests.parser

import definiti.common.ast._
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}

trait LocationUtils {
  def location: Location

  def getLocationFromContext(context: ParserRuleContext): Location = {
    val internalRange = getRangeFromContext(context)
    val range = internalRange.copy(
      start = internalRange.start.copy(line = location.range.start.line + internalRange.start.line - 1),
      end = internalRange.end.copy(line = location.range.start.line + internalRange.end.line - 1)
    )
    Location(location.file, range)
  }

  def getRangeFromContext(context: ParserRuleContext): Range = {
    val start = Option(context.getStart)
      .map(token => Position(token.getLine, token.getCharPositionInLine + 1))
      .getOrElse(Position.default)

    val end = Option(context.getStop)
      .map(token => Position(token.getLine, token.getCharPositionInLine + token.getText.length + 1))
      .getOrElse(Position.default)

    Range(start, end)
  }

  def getRangeFromTerminalNode(terminalNode: TerminalNode): Range = {
    val symbol = terminalNode.getSymbol
    Range(
      Position(symbol.getLine, symbol.getCharPositionInLine + 1),
      Position(symbol.getLine, symbol.getCharPositionInLine + symbol.getText.length + 1)
    )
  }

  def getRangeFromToken(token: Token): Range = {
    Range(
      Position(token.getLine, token.getCharPositionInLine + 1),
      Position(token.getLine, token.getCharPositionInLine + token.getText.length + 1)
    )
  }
}
