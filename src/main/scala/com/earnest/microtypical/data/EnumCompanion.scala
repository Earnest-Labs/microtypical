package com.earnest.microtypical.data

trait EnumCompanion [V, E] extends ValidatedCompanion [V, String, E] {
  protected def encoding: Map [V, String]
  protected def readError (s: String): E

  private val encode = encoding
  private val decode = encode map (_.swap)

  implicit val enumerated: Enumerated [V] = Enumerated instance encode.keySet

  override implicit val model: Model [V, String, E] =
    Model instance (
      r => decode get r toRight readError (r),
      encode.apply)
}
