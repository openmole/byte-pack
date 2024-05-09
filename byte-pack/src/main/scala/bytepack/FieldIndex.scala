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
