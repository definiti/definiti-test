package definiti.tests.validation

import definiti.common.ast.Library
import definiti.common.control.ControlResult
import definiti.tests.AST.TestsContext
import definiti.tests.Configuration

class TestsValidation(library: Library, configuration: Configuration) {
  def validate(context: TestsContext): ControlResult = {
    Controls.all
      .filter(configuration.programConfiguration.isControlAccepted)
      .map(_.control(context, library))
  }
}
