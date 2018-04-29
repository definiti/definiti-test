package definiti.tests.validation.helpers

import definiti.core.ast.{ExtendedContext, Library, NamespaceElement}
import definiti.tests.AST.TestsContext

/**
 * This object exists because core system does not provide package and imports in contexts.
 * After updating core, this object can be removed.
 */
object PackageFinder {
  def packageOfContext(context: TestsContext, library: Library): String = {
    library.namespaces
      .find(namespace => namespace.elements.exists(isContext(_, context)))
      .map(_.fullName)
      .getOrElse("")
  }

  private def isContext(namespaceElement: NamespaceElement, context: TestsContext): Boolean = {
    namespaceElement match {
      case ExtendedContext(_, content, _) if content == context => true
      case _ => false
    }
  }
}
