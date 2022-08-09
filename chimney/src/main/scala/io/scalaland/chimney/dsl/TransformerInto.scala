package io.scalaland.chimney.dsl

import io.scalaland.chimney.dsl.internal.RuntimeStorage
import io.scalaland.chimney.dsl.internal.RuntimeStorage._
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @param  source object to transform
  * @param  td     transformer definition
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam C      type-level encoded config
  */
final class TransformerInto[From, To, C <: TransformerCfg, Flags <: TransformerFlags, +RuntimeData <: RuntimeStorage](
    val source: From,
    val td: TransformerDefinition[From, To, C, Flags, RuntimeData]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerInto[From, To, C, F1, RuntimeData]], Flags] {

  /** Lifts current transformation with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def lift[F[+_]]: TransformerFInto[F, From, To, WrapperType[F, C], Flags, RuntimeData] =
    new TransformerFInto[F, From, To, WrapperType[F, C], Flags, RuntimeData](source, td.lift[F])

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): TransformerInto[From, To, _ <: TransformerCfg, Flags, U :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldConstF[F[+_], T, U](
      selector: To => T,
      value: F[U]
  )(implicit ev: U <:< T): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags, F[U] :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withFieldConstFImpl[F]

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    * */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerInto[From, To, _ <: TransformerCfg, Flags, (From => U) :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl

  /** Use `map` provided here to compute wrapped value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldComputedF[F[+_], T, U](
      selector: To => T,
      f: From => F[U]
  )(implicit ev: U <:< T): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags, (From => F[U]) :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedFImpl[F]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    * */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerInto[From, To, _ <: TransformerCfg, Flags, RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withFieldRenamedImpl

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _ <: TransformerCfg, Flags, (Inst => To) :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl

  /** Use `f` to calculate the (missing) wrapped coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withCoproductInstanceF[F[+_], Inst](f: Inst => F[To]): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags, (Inst => F[To]) :: RuntimeData] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceFImpl[F]

  /** Apply configured transformation in-place.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return transformed value of type `To`
    */
  def transform[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): To =
    macro TransformerBlackboxMacros.transformImpl[From, To, C, Flags, ScopeFlags]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineTransformerDefinition[C1 <: TransformerCfg, NewRuntimeData <: RuntimeStorage](
      f: TransformerDefinition[From, To, C, Flags, RuntimeData] => TransformerDefinition[From, To, C1, Flags, NewRuntimeData]
  ): TransformerInto[From, To, C1, Flags, NewRuntimeData] =
    new TransformerInto[From, To, C1, Flags, NewRuntimeData](source, f(td))

}
