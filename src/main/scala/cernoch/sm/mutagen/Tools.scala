package cernoch.sm.mutagen

import util.parsing.input.StreamReader._
import java.io.InputStreamReader
import util.parsing.input.StreamReader

/**
 * JAR-resource reading routines
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
private object Tools {

  def lines(name:String) = source(name) match {
    case None => throw new Exception("File not found.")
    case Some(v) => v.getLines
  }

  def source(name:String) = stream(name) match {
    case Some(v) => Some(io.Source.fromInputStream(v))
    case None => None
  }

  def reader(name:String) = stream(name) match {
    case Some(v) => Some(StreamReader(new InputStreamReader(v)))
    case None => None
  }

  def stream(name: String) = {
    val fname = "cernoch/sm/mutagen/data/" + name
    val stream = getClass.getClassLoader.getResourceAsStream(fname)
    if (stream == null) None else Some(stream)
  }
}
