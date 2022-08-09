package io.scalaland.chimney.dsl.internal

sealed trait RuntimeStorage extends Product with Serializable

object RuntimeStorage {
  final case class ::[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) +H, +T <: RuntimeStorage](
                                                                                                                          head: H,
                                                                                                                          tail: T
                                                                                                                        ) extends RuntimeStorage {
    def ::[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) G](h: G): G :: H :: T = new ::(h, this)
  }
  sealed trait Empty extends RuntimeStorage
  case object Empty extends Empty {
    def ::[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) H](h: H): H :: Empty = new ::(h, this)
  }
}
