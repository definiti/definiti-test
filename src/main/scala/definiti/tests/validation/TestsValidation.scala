package definiti.tests.validation

import definiti.common.ast.Library
import definiti.common.control.ControlResult
import definiti.tests.Configuration

class TestsValidation(library: Library, configuration: Configuration) {
  def validate(context: ValidationContext): ControlResult = {
    Controls.all
      .filter(configuration.programConfiguration.isControlAccepted)
      .map(_.control(context, library))
  }
}
