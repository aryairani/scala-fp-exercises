package moviereview
import scalaz.Apply

object FunctionOps {
  implicit class Function1Ops[T1,R](f: (T1) => R) {
    def ap[F[_]](t1: =>F[T1])(implicit a: Apply[F]) =
      a.apply(t1)(f)
  }

  implicit class Function2Ops[T1,T2,R](f: (T1,T2) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2])(implicit a: Apply[F]) =
      a.apply2(t1,t2)(f)
  }

  implicit class Function3Ops[T1,T2,T3,R](f: (T1,T2,T3) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3])(implicit a: Apply[F]) =
      a.apply3(t1,t2,t3)(f)
  }

  implicit class Function4Ops[T1,T2,T3,T4,R](f: (T1,T2,T3,T4) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4])(implicit a: Apply[F]) =
      a.apply4(t1,t2,t3,t4)(f)
  }

  implicit class Function5Ops[T1,T2,T3,T4,T5,R](f: (T1,T2,T3,T4,T5) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5])(implicit a: Apply[F]) =
      a.apply5(t1,t2,t3,t4,t5)(f)
  }

  implicit class Function6Ops[T1,T2,T3,T4,T5,T6,R](f: (T1,T2,T3,T4,T5,T6) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6])(implicit a: Apply[F]) =
      a.apply6(t1,t2,t3,t4,t5,t6)(f)
  }

  implicit class Function7Ops[T1,T2,T3,T4,T5,T6,T7,R](f: (T1,T2,T3,T4,T5,T6,T7) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7])(implicit a: Apply[F]) =
      a.apply7(t1,t2,t3,t4,t5,t6,t7)(f)
  }

  implicit class Function8Ops[T1,T2,T3,T4,T5,T6,T7,T8,R](f: (T1,T2,T3,T4,T5,T6,T7,T8) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7], t8: =>F[T8])(implicit a: Apply[F]) =
      a.apply8(t1,t2,t3,t4,t5,t6,t7,t8)(f)
  }

  implicit class Function9Ops[T1,T2,T3,T4,T5,T6,T7,T8,T9,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7], t8: =>F[T8], t9: =>F[T9])(implicit a: Apply[F]) =
      a.apply9(t1,t2,t3,t4,t5,t6,t7,t8,t9)(f)
  }

  implicit class Function10Ops[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7], t8: =>F[T8], t9: =>F[T9], t10: =>F[T10])(implicit a: Apply[F]) =
      a.apply10(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)(f)
  }

  implicit class Function11Ops[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7], t8: =>F[T8], t9: =>F[T9], t10: =>F[T10], t11: =>F[T11])(implicit a: Apply[F]) =
      a.apply11(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)(f)
  }

  implicit class Function12Ops[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) => R) {
    def ap[F[_]](t1: =>F[T1], t2: =>F[T2], t3: =>F[T3], t4: =>F[T4], t5: =>F[T5], t6: =>F[T6], t7: =>F[T7], t8: =>F[T8], t9: =>F[T9], t10: =>F[T10], t11: =>F[T11], t12: =>F[T12])(implicit a: Apply[F]) =
      a.apply12(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)(f)
  }



  def makeFunctionOps(n: Int) = {
    val range = 1 to n
    def typeName = (i:Int) => "T"+i
    def varName = (i:Int) => "t"+i
    def argDecl = (n:Int) => s"${varName(n)}: => F[${typeName(n)}]"
    val commaSeparatedTypes = range.map(typeName).mkString(",")
    val commaSeparatedArgDecls = range.map(i => s"t$i: =>F[T$i]").mkString(", ")
    val commaSeparatedArgs = range.map(varName).mkString(",")
    s"""implicit class Function${n}Ops[$commaSeparatedTypes,R](f: ($commaSeparatedTypes) => R) {
       |  def ap[F[_]]($commaSeparatedArgDecls)(implicit a: Apply[F]) =
       |    a.apply$n($commaSeparatedArgs)(f)
       |}
       |""".stripMargin
  }
}