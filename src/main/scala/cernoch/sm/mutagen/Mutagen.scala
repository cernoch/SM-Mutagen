package cernoch.sm.mutagen

import cernoch.scalogic._
import cernoch.scalogic.storage.Dumpable

/**
 * Mutagenesis dataset
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Mutagen(easy:Boolean) extends Dumpable[BLC[Atom[Val[_]]]] {

  private val clDom = CatDom("cls", false, Set("pos", "neg"))
  private val exDom = NumDom("exNo", true)
  private val exVar = Var(exDom)
  private val clVar = Var(clDom)
  private val exRel = new Btom("ex", List(clVar, exVar), Set(), Set())

  val domain = origDom + exDom

  def origDom = Tools.lines("domains.txt")
    .filter(_.trim.length > 0)
    .map{ Domain(_) }
    .toSet

  val schema = origSch
    .map{ _.head }
    .map{ prependExNo }
    .map{ BLC(_) } + BLC(exRel)

  def origSch = Tools.lines("model.txt")
    .filter(_.trim.length > 0) // Remove empty lines
    .map{ Btom(_,origDom) }
    .map{ BLC(_) }
    .toSet

  private def prependExNo
    (a: Btom[Var])
  = new Btom(
    a.pred,
    exVar :: a.args,
    a.hooks,
    a.modeIn + exVar
  )

  def data = Tools.lines (if (easy) "muta-easy.txt" else "muta-hard.txt")

  def dump = dumpRaw.flatten.map{BLC(_)}.toIterable
  
  def dumpRaw = {
    val g = new Grammar(origSch.map{i => i.head.pred -> i.head }.toMap)
    for ( (row, exNo) <- data zip Stream.from(1).iterator
        ; (cls, atoms) = g.parseLine(row))
      yield {

      val exampleAtom = exRel mapAllArgs Dict(
        clVar -> Val( if (cls) "pos" else "neg", clDom ),
        exVar -> Val( exNo, exDom )
      )

      val otherAtoms =
        for ((arch,atom) <- atoms)
          yield new Atom(atom.pred, Val(exNo,exDom) :: atom.args)

      exampleAtom :: otherAtoms
    }
  }
}


/**
 * Prints all atoms in the dataset
 */
object Mutagen {
  
  def main(args: Array[String]) : Unit
  = new Mutagen(args == Array("easy")).dump.map(println)
}
