package model

import play.api.libs.json

case class Method(val name: String)

case class OwnedMethod(val methodTypeOwner: Type, val method: Method)

case class Type(val name: String,
                val singleton: Boolean = false,
  var methods: Seq[Method] = Seq(),
  var applies: Seq[OwnedMethod] = Seq())
  

