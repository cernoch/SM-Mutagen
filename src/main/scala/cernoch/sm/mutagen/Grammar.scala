package cernoch.sm.mutagen

import util.parsing.combinator.JavaTokenParsers
import cernoch.scalogic._
import annotation.tailrec

/**
 * Parsing the "Ondra" data format
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
private class Grammar(doms:Map[String,Btom[Var]]) extends JavaTokenParsers {

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

      // Converts list of ``String``s to a list of ``Val``s
      def str2val
      ( s:List[String], a:List[Term] )
      : List[Val[_]]
      = (s,a) match {
        case (Nil, Nil) => List()

        case (arg :: aTail , war :: wTail) => (war.dom match {
          case dom@CatDom(_,_,_) =>         Val(arg, dom)
          case dom@NumDom(_,_) =>   Val(BigInt(arg), dom)
          case dom@DecDom(_) => Val(BigDecimal(arg), dom)
        }) :: str2val(aTail,wTail)

        case _ => throw new SyntaxError("Lists of non-equal length");
      }

      // The archetype
      val arch = doms.get(predicate).get
      // Substitute arguments
      val dict =
        (arch.args, str2val(arguments, arch.args))
        .zipped
        .toMap[Term,Val[_]]

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
