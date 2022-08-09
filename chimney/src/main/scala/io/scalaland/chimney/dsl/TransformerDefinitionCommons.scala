package io.scalaland.chimney.dsl

import io.scalaland.chimney.dsl.internal._
//import io.scalaland.chimney.dsl.internal.RuntimeStorage._
import io.scalaland.chimney.internal.TransformerCfg

object TransformerDefinitionCommons {
  def emptyRuntimeDataStore: RuntimeStorage.Empty = RuntimeStorage.Empty
}

trait TransformerDefinitionCommons[
    +UpdateCfg[_ <: TransformerCfg],
    +UpdateRuntimeData[_, +_ <: RuntimeStorage],
    +RuntimeData <: RuntimeStorage
] {

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeData

  /** updates runtime data in the upper transformer definition  */
  protected def __updateRuntimeData[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) T](
      newRuntimeData: T
  ): UpdateRuntimeData[T, RuntimeData]

  // used by generated code to help debugging

  /** Used internally by macro. Please don't use in your code. */
  def __refineConfig[C1 <: TransformerCfg]: UpdateCfg[C1] =
    this.asInstanceOf[UpdateCfg[C1]]

  /** Used internally by macro. Please don't use in your code. */
  def __addOverride[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) T](
      overrideData: T
  ): UpdateRuntimeData[T, RuntimeData] =
    __updateRuntimeData[T](overrideData)

  /** Used internally by macro. Please don't use in your code. */
  def __addInstance[@specialized(Int, Short, Long, Double, Float, Char, Byte, Boolean, Unit) T](
      instanceData: T
  ): UpdateRuntimeData[T, RuntimeData] =
    __updateRuntimeData[T](instanceData)
}
