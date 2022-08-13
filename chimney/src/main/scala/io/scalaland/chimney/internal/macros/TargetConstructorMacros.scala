package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.AssertUtils

import scala.reflect.macros.blackbox

trait TargetConstructorMacros extends Model with AssertUtils {

  val c: blackbox.Context

  import c.universe._

  def mkNewClass(classTpe: Type, args: Iterable[Tree]): Tree = {
    q"new $classTpe(..$args)"
  }

  def mkNewJavaBean(classTpe: Type, argsMapping: Iterable[(Target, Tree)]): Tree = {

    val fn = freshTermName(classTpe)

    val objCreation = q"val $fn = new $classTpe"
    val setterInvocations = argsMapping.map {
      case (target, argTree) =>
        val setterName = TermName("set" + target.name.capitalize)
        q"$fn.$setterName($argTree)"
    }.toSeq

    q"{..${objCreation +: setterInvocations}; $fn}"
  }

  def mkCoproductInstance(
      transformerDefinitionPrefix: Tree,
      srcPrefixTree: Tree,
      instSymbol: Symbol,
      To: Type,
      derivationTarget: DerivationTarget
  ): Tree = {
    val instFullName = instSymbol.fullName
    val fullTargetName = To.typeSymbol.fullName
    val finalTpe = derivationTarget.targetType(To)
    q"""
      $transformerDefinitionPrefix
        .instances(($instFullName, $fullTargetName))
        .asInstanceOf[Any => $finalTpe]
        .apply($srcPrefixTree)
        .asInstanceOf[$finalTpe]
    """
  }

  /**
    * Generate result tree for given transformer derivation target from a value tree
    *
    * @param derivationTarget decides about if/how the value tree is wrapped
    * @param valueTree value tree for which we build target result tree
    * @return potentially wrapped value tree
    */
  def mkTransformerBodyTree0(derivationTarget: DerivationTarget)(valueTree: Tree): Tree = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        valueTree
      case DerivationTarget.PartialTransformer(_) =>
        q"_root_.io.scalaland.chimney.PartialTransformer.Result.Value($valueTree)"
      case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        q"${wrapperSupportInstance}.pure($valueTree)"
    }
  }

  def mkTransformerBodyTree1(
      To: Type,
      target: Target,
      transformerBodyTree: TransformerBodyTree,
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Tree => Tree
  ): Tree = {
    mkTransformerBodyTree(To, Seq(target), Seq(transformerBodyTree), derivationTarget) {
      case Seq(innerTree) => mkTargetValueTree(innerTree)
    }
  }

  // TODO: describe
  /**
    *
    *
    * @param To
    * @param targets
    * @param bodyTreeArgs
    * @param derivationTarget
    * @param mkTargetValueTree
    * @return
    */
  def mkTransformerBodyTree(
      To: Type,
      targets: Seq[Target],
      bodyTreeArgs: Seq[TransformerBodyTree],
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Seq[Tree] => Tree
  ): Tree = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(_.isTotalTarget),
          "All derived body trees arguments must be total in Total target derivation!"
        )
        mkTargetValueTree(bodyTreeArgs.map(_.tree))

      case pt: DerivationTarget.PartialTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isPartialTarget),
          "Only Total and Partial body tree arguments are supported in Partial target derivation!"
        )

        val (totalArgs, partialArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (partialArgs.isEmpty) {
          mkTransformerBodyTree0(pt)(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
        } else {
          val (partialTargets, partialBodyTrees) = partialArgs.unzip
          val partialTrees = partialBodyTrees.map(_.tree)
          val partialTreesArray = q"Array(..${partialTrees})"
          val arrayFn = freshTermName("array")
          val arrayFnTree = q"$arrayFn"

          val argIndices = partialTargets.indices

          val patRefArgsMap = (partialTargets zip argIndices).map {
            case (target, argIndex) => target -> q"$arrayFn($argIndex)"
          }.toMap
          val pureArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = pureArgsMap ++ patRefArgsMap

          val updatedArgs = targets.map(argsMap)

          q"""
             _root_.io.scalaland.chimney.PartialTransformer.Result.sequence(${partialTreesArray}.iterator, ${pt.failFastTree})
               .map { $arrayFnTree =>
                  ${mkTargetValueTree(updatedArgs)}
               }
           """
        }

      case lt @ DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isLiftedTarget),
          "Only Total and Lifted body tree arguments are supported in Lifted target derivation!"
        )

        val (totalArgs, liftedArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (liftedArgs.isEmpty) {
          mkTransformerBodyTree0(lt)(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
        } else {

          val (liftedTargets, liftedBodyTrees) = liftedArgs.unzip
          val liftedTrees = liftedBodyTrees.map(_.tree)
          val productF = liftedTrees.reduceRight { (tree, rest) =>
            q"$wrapperSupportInstance.product($tree, $rest)"
          }

          val argNames = liftedTargets.map(target => freshTermName(target.name))
          val argTypes = liftedTargets.map(_.tpe)
          val bindTreesF = argNames.map { termName =>
            Bind(termName, Ident(termNames.WILDCARD))
          }
          val productType = argTypes.map(tpe => tq"$tpe").reduceRight[Tree]((param, tree) => tq"($param, $tree)")
          val patternF = bindTreesF.reduceRight[Tree]((param, tree) => pq"(..${List(param, tree)})")

          val patRefArgsMap = (liftedTargets zip argNames).map { case (target, argName) => target -> q"$argName" }.toMap
          val pureArgsMap = totalArgs.map { case (target, bt)                           => target -> bt.tree }.toMap
          val argsMap = pureArgsMap ++ patRefArgsMap

          val updatedArgs = targets.map(argsMap)

          q"""
            $wrapperSupportInstance.map[$productType, $To](
              $productF,
              { case $patternF => ${mkTargetValueTree(updatedArgs)} }
            )
          """
        }
    }
  }
}
