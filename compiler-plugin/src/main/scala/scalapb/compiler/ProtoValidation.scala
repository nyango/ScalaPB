package scalapb.compiler

import com.google.protobuf.Descriptors._
import scala.collection.JavaConverters._

class ProtoValidation(implicits: DescriptorImplicits) {
  import implicits._

  def validateFile(fd: FileDescriptor): Unit = {
    fd.getEnumTypes.asScala.foreach(validateEnum)
    fd.getMessageTypes.asScala.foreach(validateMessage)
    if (fd.scalaOptions.hasPrimitiveWrappers && fd.scalaOptions.getNoPrimitiveWrappers) {
      throw new GeneratorException(
        s"primitive_wrappers and no_primitive_wrappers must not be used at the same time."
      )
    }
  }

  def validateEnum(e: EnumDescriptor): Unit = {
    if (e.getValues.asScala.exists(_.getName.toUpperCase == "UNRECOGNIZED")) {
      throw new GeneratorException(
        s"The enum value 'UNRECOGNIZED' in ${e.getName} is not allowed due to conflict with the catch-all " +
          "Unrecognized(v: Int) value."
      )
    }
  }

  def validateMessage(m: Descriptor): Unit = {
    m.getEnumTypes.asScala.foreach(validateEnum)
    m.getNestedTypes.asScala.foreach(validateMessage)
    m.getFields.asScala.foreach(validateField)
    if (m.isSealedOneofType) {
      val oneof = m.getOneofs.get(0)
      if (m.getFields.size() != oneof.getFields.size()) {
        throw new GeneratorException(s"${m.getFullName}: sealed oneofs must have all their fields inside a single oneof")
      }
      val fields = oneof.getFields.asScala
      fields.find(!_.isMessage).foreach {
        field =>
          throw new GeneratorException(
            s"${m.getFullName}.${field.getName}: sealed oneofs must have all their fields be message types")
      }
      fields.find(_.getMessageType.getContainingType != m).foreach {
        field =>
          throw new GeneratorException(
            s"${m.getFullName}.${field.getName}: all sealed oneof cases must be defined inside the container message")
      }
      val distinctTypes = fields.map(_.getMessageType).toSet
      if (distinctTypes.size != fields.size) {
        throw new GeneratorException(
          s"${m.getFullName}: all sealed oneof cases must be of a distinct message type")
      }
    }
  }

  def validateField(fd: FieldDescriptor): Unit = {
    if (ProtoValidation.ForbiddenFieldNames.contains(fd.scalaName))
      throw new GeneratorException(
        s"Field named '${fd.getName}' in message '${fd.getFullName}' is not allowed. See https://scalapb.github.io/customizations.html#custom-names"
      )
    if (!fd.isRepeated && fd.fieldOptions.hasCollectionType)
      throw new GeneratorException(
        s"${fd.getFullName}: Field ${fd.getName} has collection_type set but is not a repeated field."
      )
    if (!fd.isMapField && (fd.fieldOptions.hasKeyType || fd.fieldOptions.hasValueType)) {
      throw new GeneratorException(
        s"${fd.getFullName}: Field ${fd.getName} is not a map but specifies key_type or value_type."
      )
    }
    if (fd.isMapField && fd.fieldOptions.hasCollectionType)
      throw new GeneratorException(
        s"${fd.getFullName}: Field ${fd.getName} is a map but has collection_type specified."
      )
    if (fd.isMapField && fd.fieldOptions.hasType)
      throw new GeneratorException(
        s"${fd.getFullName}: Field ${fd.getName} is a map and has type specified. Use key_type or value_type instead."
      )
    if (!fd.isOptional && fd.fieldOptions.hasNoBox)
      throw new GeneratorException(
        s"${fd.getFullName}: Field ${fd.getName} has no_box set but is not an optional field."
      )
  }
}

object ProtoValidation {

  val ForbiddenFieldNames = Set(
    "hashCode",
    "equals",
    "class",
    "clone",
    "copy",
    "finalize",
    "getClass",
    "notify",
    "notifyAll",
    "toString",
    "wait",
    "productArity",
    "productIterator",
    "productPrefix"
  )
}
