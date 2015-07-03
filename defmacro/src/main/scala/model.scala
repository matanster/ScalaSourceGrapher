package model

case class Method(val name: String)

case class ModelType(val name: String,
                     val typeType: String,
                     var methods: Seq[Method] = Seq(),
                     var applies: Seq[OwnedMethod] = Seq())

case class OwnedMethod(val methodTypeOwner: ModelType, val method: Method)

