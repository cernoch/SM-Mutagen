package cernoch.sm.mutagen

/**
 * Syntax error occuring while parsing the dataset
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class SyntaxError(msg: String) extends RuntimeException(msg) {}
