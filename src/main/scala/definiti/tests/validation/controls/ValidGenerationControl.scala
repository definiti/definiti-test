package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.utils.CollectionUtils
import definiti.tests.validation.helpers.{ExpressionTypes, Types}
import definiti.tests.validation.{GeneratorMeta, ValidationContext}

object ValidGenerationControl extends Control[ValidationContext] {
  override def description: String = "Check if generation expressions are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case generationExpression: GenerationExpression => generationExpression
    }.map(controlGenerationExpression(_, context))
  }

  private def controlGenerationExpression(expression: GenerationExpression, context: ValidationContext): ControlResult = {
    context.getGenerator(expression.name)
      .map(controlGenerationWithGenerator(expression, _, context))
      .getOrElse(unknownGenerator(expression.name, expression.location))
  }

  private def controlGenerationWithGenerator(generation: GenerationExpression, generator: GeneratorMeta, context: ValidationContext): ControlResult = {
    if (hasValidNumberOfArguments(generation, generator)) {
      controlArgumentAndParameterTypes(generation, generator, context)
    } else {
      invalidNumberOfArgument(generator.parameters.length, generation.arguments.length, generation.location)
    }
  }

  private def hasValidNumberOfArguments(generation: GenerationExpression, generator: GeneratorMeta): Boolean = {
    if (generator.parameters.lastOption.exists(_.isRest)) {
      generation.arguments.length >= generator.parameters.length - 1
    } else {
      generation.arguments.length == generator.parameters.length
    }
  }

  private def controlArgumentAndParameterTypes(generation: GenerationExpression, generator: GeneratorMeta, context: ValidationContext): ControlResult = {
    val lastType = generator.parameters.lastOption.map(_.typ).getOrElse(Type("Any"))
    generation.arguments
      .zip(CollectionUtils.fill(generator.parameters.map(_.typ), lastType, generation.arguments.size))
      .map { case (expression, parameterType) =>
        val expressionType = ExpressionTypes.getTypeOfExpression(expression, context)
        val genericReplacements = generator.generics.zip(generation.generics).toMap
        val parameterTypeWithReplacedGenerics = Types.replaceGenerics(parameterType, genericReplacements)
        if (Types.finalType(expressionType, context) == Types.finalType(parameterTypeWithReplacedGenerics, context)) {
          ControlResult.OK
        } else {
          ControlResult(unexpectedType(parameterTypeWithReplacedGenerics, expressionType, expression.location))
        }
      }
  }

  def invalidNumberOfArgument(expected: Int, got: Int, location: Location): Alert = {
    alert(s"Unexpected number of arguments (expected: ${expected}, got: ${got})", location)
  }

  def unexpectedType(expected: Type, got: Type, location: Location): Alert = {
    alert(s"Unexpected type (expected: ${expected.readableString}, got: ${got.readableString})", location)
  }

  def unknownGenerator(name: String, location: Location): Alert = {
    alert(s"Unknown generator ${name}", location)
  }
}
