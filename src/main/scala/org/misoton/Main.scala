package org.misoton

object Main {

  import Lexer._

  def main(args: Array[String]): Unit = {
    val SP = hidden(" " / "\n")
    val P = "Pen" ~ lookAhead((SP ~ "Pineapple") / $) / "Pineapple" ~ lookAhead(SP ~ "Apple")
    val A = "Apple"
    val PPAP = P ~ SP ~ P ~ SP ~ A ~ SP ~ P <|

    println(lex("Pen Pineapple Apple Pen", PPAP))
    println(lex("Pineapple Pen Apple Pineapple", PPAP))
    println(lex("Pen Pen Pen Pen", PPAP))
    println(lex("Pen Pen Apple Pen", PPAP))
    println(lex("Pen Pen Apple Pen Pen", PPAP))
  }
}
