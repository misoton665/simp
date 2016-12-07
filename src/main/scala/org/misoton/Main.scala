package org.misoton

object Main {

  import Lexer._

  val SP = hidden(" " / "\n")
  val cP = "Pen" ~ lookAhead((SP ~ "Pineapple") / $) / "Pineapple" ~ lookAhead(SP ~ "Apple")
  val cA = "Apple"
  val obstinatePPAPLexer = cP ~ SP ~ cP ~ SP ~ cA ~ SP ~ cP <|

  val fP = """(P[a-z]*)""".r
  val fA = """(A[a-z]*)""".r
  val freedomPPAPLexer = fP ~ SP ~ fP ~ SP ~ fA ~ SP ~ fP <|

  def main(args: Array[String]): Unit = {

    println("obstinate:")
    println(lex("Pen Pineapple Apple Pen", obstinatePPAPLexer))
    println(lex("Pineapple Pen Apple Pineapple", obstinatePPAPLexer))
    println(lex("Pen Pen Pen Pen", obstinatePPAPLexer))
    println(lex("Pen Pen Apple Pen", obstinatePPAPLexer))
    println(lex("Pen Pen Apple Pen Pen", obstinatePPAPLexer))


    println("\nfreedom:")
    println(lex("Pen Pineapple Apple Pen", freedomPPAPLexer))
    println(lex("Pineapple Pen Apple Pineapple", freedomPPAPLexer))
    println(lex("Pen Pen Pen Pen", freedomPPAPLexer))
    println(lex("Pen Pen Apple Pen", freedomPPAPLexer))
    println(lex("Pen Pen Apple Pen Pen", freedomPPAPLexer))
  }
}
