package scala.generator

import io.apibuilder.spec.v0.models.{Field, Model}

object PrimitiveWrapper {

  val FieldName: String = "value"

  def className(union: ScalaUnion, primitive: ScalaPrimitive, container: String = ""): String = {
    primitive match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Joda | ScalaPrimitive.DateTimeIso8601Java | ScalaPrimitive.Decimal | ScalaPrimitive.ObjectAsPlay | ScalaPrimitive.ObjectAsCirce | ScalaPrimitive.JsonValueAsPlay | ScalaPrimitive.JsonValueAsCirce | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {
        ScalaUtil.toClassName(union.name) + container + ScalaUtil.toClassName(primitive.shortName)
      }
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        ScalaUtil.toClassName(union.name)
      }
    }
  }

  def isBasicType(primitive: ScalaPrimitive): Boolean = {
    primitive match {
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        false
      }
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Joda | ScalaPrimitive.DateTimeIso8601Java | ScalaPrimitive.Decimal | ScalaPrimitive.ObjectAsPlay | ScalaPrimitive.ObjectAsCirce | ScalaPrimitive.JsonValueAsPlay | ScalaPrimitive.JsonValueAsCirce | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {

        true
      }
    }
  }

}

case class PrimitiveWrapper(ssd: ScalaService) {
  import PrimitiveWrapper.isBasicType

  case class Wrapper(model: ScalaModel, union: ScalaUnion)

  val wrappers: Seq[Wrapper] = ssd.unions.flatMap { union =>
    (union.types.map(_.datatype).collect {
      case p: ScalaPrimitive if isBasicType(p) => p
    }.sortWith(_.shortName < _.shortName).map { p =>
      val name = PrimitiveWrapper.className(union, p)
      val model = Model(
        name = name,
        plural = s"${name}s",
        description = Some(s"Wrapper class to support the union types containing the datatype[${p.apidocType}]"),
        fields = Seq(
          Field(
            name = PrimitiveWrapper.FieldName,
            `type` = p.apidocType,
            required = true
          )
        )
      )
      Wrapper(
        new ScalaModel(ssd, model),
        union
      )
    }) ++
    (union.types.map(_.datatype).collect {
      case ScalaDatatype.List(p: ScalaPrimitive) => p
    }.sortWith(_.shortName < _.shortName).map { p =>
      val name = PrimitiveWrapper.className(union, p, "Seq")
      val model = Model(
        name = name,
        plural = s"${name}s",
        description = Some(s"Wrapper class to support the union types containing the datatype[[${p.apidocType}]]"),
        fields = Seq(
          Field(
            name = PrimitiveWrapper.FieldName,
            `type` = s"[${p.apidocType}]",
            required = true
          )
        )
      )
      Wrapper(
        new ScalaModel(ssd, model),
        union
      )
    })
  }

}
