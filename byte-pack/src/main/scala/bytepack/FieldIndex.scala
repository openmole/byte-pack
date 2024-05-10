package bytepack

/*
 * Copyright (C) 2024 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object FieldIndex:
  import scala.annotation.experimental
  import scala.quoted.*
  import scala.deriving.*

  inline def fields[T]: Map[String, Int] = ${ fieldsImpl[T] }

   // because .typeArgs is @experimental
  def fieldsImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[Map[String, Int]] =
    import quotes.reflect.*
    val typ = TypeRepr.of[T]
    val sym = typ.typeSymbol
    val typeArgs = typ.typeArgs

    val comp = sym.companionClass
    val mod = Ref(sym.companionModule)
    val names = sym.caseFields.zipWithIndex.map((f, i) => f.name -> i) //if p.flags.is(Flags.HasDefault)

    val namesExpr =
      Expr.ofSeq(names.map(Expr(_)))

    '{ $namesExpr.toMap }



  class MkFieldIndex[From]:
    transparent inline def apply[To](inline lambda: From => To): Int = ${ fieldIndexImpl[From, To]('{lambda}) }

//  inline def fieldIndex[F](f: F => Any): Int = ${ fieldIndexImpl[F]('{f}) }

  def fieldIndexImpl[F, T](f: Expr[F => T])(using quotes: Quotes, tpef: Type[F]): Expr[Int] =
    // val term = f.show
    import quotes.*
    import quotes.reflect.*


    def recur(tree: Tree, selects: List[String]): List[String] = tree match
      case Ident(_) if selects.nonEmpty => selects
      case s @ Select(qual, name) => recur(qual, name :: selects)
      case Inlined(_, _, t) => recur(t, selects)
      case DefDef(_, _, _, Some(t)) => recur(t, selects)
      case Block(l, _) => l.flatMap(t => recur(t, selects))
      case _ => List()

//    println(f.asTerm.show(using Printer.TreeStructure))
//    println(recur(f.asTerm, List()))

    val sym = TypeRepr.of[F].typeSymbol

//    object CaseClass:
//      def unapply(term: Term): Option[Term] =
//        term.tpe.classSymbol.flatMap: sym =>
//          Option.when(sym.flags.is(Flags.Case))(term)
//
//
    //   f.asTerm match
    //     case Select(_,_) => "test"

    //val source = f.asTerm.pos.sourceCode
    val selects = recur(f.asTerm, List())
    if selects.size != 1 then report.errorAndAbort("Only one level of case class is supported for now", f.asTerm.pos)

    val field = selects.head //source.head.dropWhile(_ != '.').drop(1)

    val index =
      sym.caseFields.zipWithIndex.find((f, _) => f.name == field) match
        case Some(f) => f._2 //if p.flags.is(Flags.HasDefault)
        case None => report.errorAndAbort(s"No field named ${field} found in case class ${sym}", f.asTerm.pos)

    //   val field = source.head.dropWhile(_ != '.').drop(1)

    val v = Expr(index)
    val pack =
      Expr.summon[PackProduct[F]] match
        case Some(p) => p
        case None => report.errorAndAbort(s"Not found PackProduct for type ${sym}", f.asTerm.pos)

    '{
      Pack.indexOf[F]($v)(using $pack)
    }


  class MkUnpackField[From]:
    transparent inline def apply[To](b: IArray[Byte], inline lambda: From => To): To = ${ unpackFieldImpl[From, To]('{lambda}, '{b}) }


  def unpackFieldImpl[F, T](f: Expr[F => T], b: Expr[IArray[Byte]])(using quotes: Quotes, tpef: Type[F], tpeT: Type[T]): Expr[T] =
    import quotes.*
    import quotes.reflect.*

    val packT =
      Expr.summon[Pack[T]] match
        case Some(p) => p
        case None => report.errorAndAbort(s"Not found Pack for type ${tpeT}", f.asTerm.pos)

    '{
      val index = Pack.indexOf[F]($f)
      ${packT}.unpack(index, $b)
    }


//
//    val body = comp.tree.asInstanceOf[ClassDef].body
//    val idents: List[Term] =
//      for case deff@DefDef(name, _, _, _) <- body
//          if name.startsWith("$lessinit$greater$default")
//      yield mod.select(deff.symbol).appliedToTypes(typeArgs)
//
//    val identsExpr: Expr[List[Any]] =
//      Expr.ofList(idents.map(_.asExpr))
//
//    '{ $namesExpr.zip($identsExpr).toMap }
