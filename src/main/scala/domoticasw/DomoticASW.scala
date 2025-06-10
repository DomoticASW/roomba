package domoticasw

object DomoticASW:
  type ActionId = String
  type PropertyId = String
  type ActualTypes = String | Int | Double | Boolean | Color | Unit
  case class Color(r: Int, g: Int, b: Int)

  enum Type:
    case String
    case Int
    case Double
    case Boolean
    case Color
    case Void

  object Type:
    def fromString(s: String): Option[Type] = s match
      case "String"  => Some(Type.String)
      case "Boolean" => Some(Type.Boolean)
      case "Int"     => Some(Type.Int)
      case "Double"  => Some(Type.Double)
      case "Color"   => Some(Type.Color)
      case "Void"    => Some(Type.Void)
      case _         => ???

  case class DeviceRegistration(
      id: String,
      name: String,
      properties: Seq[DeviceProperty],
      actions: Seq[DeviceAction],
      events: Seq[String]
  )

  enum DeviceProperty(id: PropertyId, name: String, value: ActualTypes):
    case WithSetter(
        id: PropertyId,
        name: String,
        value: ActualTypes,
        setterActionId: ActionId
    ) extends DeviceProperty(id, name, value)
    case WithTypeConstraint(
        id: PropertyId,
        name: String,
        value: ActualTypes,
        typeConstraints: TypeConstraints
    ) extends DeviceProperty(id, name, value)

  case class DeviceAction(
      id: ActionId,
      name: String,
      description: Option[String],
      inputTypeConstraints: TypeConstraints
  )

  enum TypeConstraints:
    case Enum(values: Set[String])
    case IntRange(min: Int, max: Int)
    case DoubleRange(min: Double, max: Double)
    case None(`type`: Type)
