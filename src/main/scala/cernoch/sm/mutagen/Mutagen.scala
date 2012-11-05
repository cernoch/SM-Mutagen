package cernoch.sm.mutagen

import cernoch.scalogic._
import cernoch.scalogic.storage.Dumpable
import java.lang.String

import java.io.InputStreamReader
import util.parsing.input.StreamReader
import util.parsing.combinator.JavaTokenParsers
import cernoch.scalogic.Dict._

/**
 *
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
    (a: Btom[FFT])
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
    for ((row, exNo) <- data zip Stream.from(1).iterator;
         (cls, atoms) = g.parseLine(row))
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


object Tools {

  private[mutagen] def lines(name:String) = source(name) match {
    case None => throw new Exception("File not found.")
    case Some(v) => v.getLines
  }

  private[mutagen] def source(name:String) = stream(name) match {
    case Some(v) => Some(io.Source.fromInputStream(v))
    case None => None
  }

  private[mutagen] def reader(name:String) = stream(name) match {
    case Some(v) => Some(StreamReader(new InputStreamReader(v)))
    case None => None
  }

  private[mutagen] def stream(name: String) = {
    val fname = "cernoch/sm/mutagen/data/" + name
    val stream = getClass.getClassLoader.getResourceAsStream(fname)
    if (stream == null) None else Some(stream)
  }
}


object Mutagen {
  
  def main(args: Array[String]) : Unit
  = new Mutagen(args == Array("easy")).dump.map(println)
}


class Grammar(doms:Map[String,Btom[Var]]) extends JavaTokenParsers {

  def parseLine(l:String)
    = parse(line,l) match {
        case Success(r,_) => r
        case x => throw new SyntaxError("Unknown syntax error: " + x)
      }

  def line = "^".r ~> tridaDelim ~ repsep(atom, ",\\s*".r) <~ endOfLine ^^ {
    case trida ~ atoms => { (trida, atoms) }
    case _ => throw new SyntaxError("Something went wrong.")
  }

  def endOfLine = comment | "$".r

  def atom = mystr ~ arguments ^^ {
    case predicate ~ arguments => {

      def str2val(s:List[String], a:List[Term]) : List[Val[_]] = (s,a) match {
        case (arg :: aTail , war :: wTail) => war.dom match {
          case dom@CatDom(_,_,_) =>         Val(arg, dom) :: str2val(aTail,wTail)
          case dom@NumDom(_,_) =>   Val(BigInt(arg), dom) :: str2val(aTail,wTail)
          case dom@DecDom(_) => Val(BigDecimal(arg), dom) :: str2val(aTail,wTail)
        }
        case (Nil, Nil) => List()
        //TODO: Better exception
        case _ => throw new SyntaxError("Lists of non-equal length");
      }

      // The archetype
      val arch = doms.get(predicate).get
      // Substitute arguments
      val dict = (arch.args, str2val(arguments, arch.args)).zipped.toMap[Term,Val[_]]
      (arch, arch.mapAllArgs(dict))
    }
  }

  def arguments = "\\(".r ~> repsep(argument, ",\\s*".r) <~ "\\)".r
  def argument = mystr | floatingPointNumber

  def tridaDelim = trida <~ "\\s*".r
  def trida = tridaPos | tridaNeg
  def tridaPos = "+" ^^ { case _ => true }
  def tridaNeg = "-" ^^ { case _ => false }

  def comment = """//.*$""".r

  def mystr = ident | stringLiteral ^^ { s =>
    if (s.length() > 0 && s.charAt(0) == '"')
      s.substring(1, s.length()-1) else s
  }
}

class SyntaxError(msg: String) extends RuntimeException(msg) {}