/*
Write a lisp-like interpreter that evaluates expressions in the following grammar:

 Expr := (add Expr Expr)
 Expr := (mult Expr Expr)
 Expr := (let Name Expr Expr)
 Expr := Name
 Expr := Integer
 Name := String

Examples:

  (add 5 1) = 6
  (mult 3 5) = 15
  (add (mult 5 3) (mult 2 3)) = 21
  (let x (add 1 2) (add x 3)) = 6
  (let x (add 1 2) (let x (add 1 3) (mult x 2))) = 8
*/


object LispLikeInterpreter extends App {

  import scala.util.Try

  // splits input into top-level arguments
  def tokenize(input: String): List[String] = {

    val (_, _, result) =
      (input + " ").zipWithIndex.foldLeft(0, 0, List.empty[String]) { (acc, ci) =>
        val (brackets, pos, result) = acc
        val (char, i) = ci

        char match {
          case '(' => (brackets + 1, pos, result)
          case ')' => (brackets - 1, pos, result)
          case ' ' if brackets == 0 =>
            val arg = input.substring(pos, i)
            (brackets, i + 1, arg :: result)
          case _ => acc
        }
      }

    result.reverse
  }

  def eval(input: String, ctx: Map[String, Int] = Map.empty): Int =
    ctx.getOrElse(input, Try(input.toInt).getOrElse {
      val withoutParens = input.substring(1, input.size - 1)

      tokenize(withoutParens) match {
        case "add" :: arg1 :: arg2 :: Nil => eval(arg1, ctx) + eval(arg2, ctx)

        case "mult" :: arg1 :: arg2 :: Nil => eval(arg1, ctx) * eval(arg2, ctx)

        case "let" :: name :: bindExpr :: expr :: Nil => eval(expr, ctx.updated(name, eval(bindExpr, ctx)))

        case _ => throw new IllegalArgumentException(input)
      }
    })

  // test
  println(eval("(add 5 1)"))
  println(eval("(mult 3 5)"))
  println(eval("(add (mult 5 3) (mult 2 3))"))
  println(eval("(let x (add 1 2) (add x 3))"))
  println(eval("(let x (add 1 2) (let x (add 1 3) (mult x 2)))"))
}
