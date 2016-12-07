package org.misoton

object Lexer {
  // トークン
  trait Token

  // ID
  case class IDToken(value: String) extends Token {
    override def toString: String = "ID: \"" + value + "\""
  }

  // Nil(親ノードのトークン)
  case object NilToken extends Token {
    override def toString: String = "NilToken"
  }

  // トークンツリー
  sealed trait TokenTree

  // トークンノード
  sealed case class TokenTreeNode[T <: Token](token: Token, var children: Seq[TokenTree]) extends TokenTree {
    def addChild[R <: Token](tokenTreeNode: TokenTreeNode[R]) = {
      children = children :+ tokenTreeNode
    }

    override def toString: String = token.toString + "(" + children.mkString(",") + ")"
  }

  type State = (String, Seq[TokenTree])
  type Input = State
  type Output = Either[String, State]

  sealed abstract class Lexer extends (Input => Output) {
    // 親ノードをつくる
    def <| : Lexer = lexerGen { (input) =>
      this (input) match {
        case Right((inputStr, children)) => Right((inputStr, Seq(TokenTreeNode(NilToken, children))))
        case err@Left(_) => err
      }
    }

    // Lexerの連結
    def ~(that: Lexer): Lexer = lexerGen { (input) =>
      this (input) match {
        case Right(output) => that(output)
        case err@Left(_) => err
      }
    }

    // this でなければ that
    def /(that: Lexer): Lexer = lexerGen { (input) =>
      this(input) match {
        case ok@Right(_) => ok
        case Left(_) => that(input)
      }
    }
  }

  // 解析結果を隠す（読み飛ばす）
  def hidden(hidingLexer: Lexer): Lexer = lexerGen { (input) =>
    val (_, children) = input
    hidingLexer(input) match {
      case Right((inputStr, _)) => Right((inputStr, children))
      case err@Left(_) => err
    }
  }

  // 先読み
  def lookAhead(lookAheadLexer: Lexer): Lexer = lexerGen { (input) =>
    lookAheadLexer(input) match {
      case Right(_) => Right(input)
      case err@Left(_) => err
    }
  }

  // 入力の終わり
  lazy val $ = lexerGen {
    case output@("", tokenTree) => Right(output)
    case (inputStr, _) => Left("This position is not end of the input on \"" + inputStr + "\"")
  }

  // 関数からLexerの生成
  def lexerGen(f: Input => Output): Lexer = new Lexer {
    override def apply(input: Input): Output = f(input)
  }

  // 文字列からLexerの生成
  implicit def string2lexer(str: String): Lexer = {
    lexerGen((input) => {
      val (inputStr, nodes) = input
      if (inputStr startsWith str) Right((inputStr substring str.length, nodes :+ TokenTreeNode[IDToken](IDToken(str), Nil)))
      else Left("\"" + inputStr + "\" cannot match with \"" + str + "\"")
    })
  }

  // 字句解析の実行
  def lex(inputStr: String, lexer: Lexer): Either[String, Seq[TokenTree]] = {
    val initialState = (inputStr, Seq())
    lexer(initialState) match {
      case Right(("", tokenTreeSeq)) => Right(tokenTreeSeq)
      case Right((leftString, _)) => Left("input string was left: \"" + leftString + "\"")
      case Left(errMsg) => Left(errMsg)
    }
  }
}
