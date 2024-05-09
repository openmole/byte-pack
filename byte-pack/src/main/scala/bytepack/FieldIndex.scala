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


//  inline def fieldIndex[F](f: F => Any): Int = ${ fieldIndexImpl[F]('{f}) }

  def fieldIndexImpl[F](f: Expr[F => Any])(using quotes: Quotes, tpef: Type[F]): Expr[Int] =
    // val term = f.show
    import quotes.*
    import quotes.reflect.*

    val sym = TypeRepr.of[F].typeSymbol

//    object CaseClass:
//      def unapply(term: Term): Option[Term] =
//        term.tpe.classSymbol.flatMap: sym =>
//          Option.when(sym.flags.is(Flags.Case))(term)
//
//
    //   f.asTerm match
    //     case Select(_,_) => "test"

    val source = f.asTerm.pos.sourceCode

    val field = source.head.dropWhile(_ != '.').drop(1)
    val index =
      sym.caseFields.zipWithIndex.find((f, _) => f.name == field) match
        case Some(f) => f._2 //if p.flags.is(Flags.HasDefault)
        case None => report.errorAndAbort(s"No field named ${source} found in case class ${sym}", f.asTerm.pos)

    //   val field = source.head.dropWhile(_ != '.').drop(1)

    val v = Expr(index)
    val pack =
      Expr.summon[PackProduct[F]] match
        case Some(p) => p
        case None => report.errorAndAbort(s"Not found PackProduct for type ${sym}", f.asTerm.pos)

    '{
      Pack.indexOf[F]($v)(using $pack)
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
