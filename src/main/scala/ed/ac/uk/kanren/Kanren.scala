package ed.ac.uk.kanren

import scala.language.implicitConversions

object Kanren {

  val ImproperEndOfList = ed.ac.uk.kanren.ImproperEndOfList
  val Var = ed.ac.uk.kanren.Var
  val Val = ed.ac.uk.kanren.Val
  type KList = ed.ac.uk.kanren.KList
  type Var = ed.ac.uk.kanren.Var
  type Val = ed.ac.uk.kanren.Val
  val KList = ed.ac.uk.kanren.KList
  type Goal = ed.ac.uk.kanren.Goal
  type Term = ed.ac.uk.kanren.Term
  val list = KList
  val cons = KCons

//  def consO       : (Term, Term, Term)       => Goal = KanrenList.consO
//  def carO        : (Term, Term)             => Goal = KanrenList.carO
//  def cdrO        : (Term, Term)             => Goal = KanrenList.cdrO
//  def nullO       :  Term                    => Goal = KanrenList.nullO
//  def appendO     : (Term, Term, Term)       => Goal = KanrenList.appendO
//  def memberO     : (Term, Term)             => Goal = KanrenList.memberO
//  def listO       :  Term                    => Goal = KanrenList.listO
//  def selectO     : (Term, Term, Term)       => Goal = KanrenList.selectO
//  def sameLengthO : (Term, Term)             => Goal = KanrenList.sameLengthO
//  def reversO     : (Term, Term)             => Goal = KanrenList.reversO
//  def replaceO    : (Term, Term, Term, Term) => Goal = KanrenList.replaceO
//  def assocO      : (Term, Term, Term)       => Goal = KanrenList.assocO

  val SearchStrategy = ed.ac.uk.kanren.SearchStrategy

  var searchStrategy : SearchStrategy = BFS
  var TRACE : Boolean = false // if you set this to true probably you should also set searchStrategy to DFS

  implicit def Any2Val(x:Any): Val = Val(x)

  // this redundant syntax is needed in Scala because Scala doesn't allow lazy varargs
  implicit def goal2clause (x: Goal                                                       ) : Clause = clause(x                                                          )
  implicit def tuple2clause(x:(Goal, Goal)                                                ) : Clause = clause(x._1, x._2                                                 )
  implicit def tuple2clause(x:(Goal, Goal, Goal)                                          ) : Clause = clause(x._1, x._2, x._3                                           )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal)                                    ) : Clause = clause(x._1, x._2, x._3, x._4                                     )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal)                              ) : Clause = clause(x._1, x._2, x._3, x._4, x._5                               )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal, Goal)                        ) : Clause = clause(x._1, x._2, x._3, x._4, x._5, x._6                         )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal, Goal, Goal)                  ) : Clause = clause(x._1, x._2, x._3, x._4, x._5, x._6, x._7                   )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal)            ) : Clause = clause(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8             )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal)      ) : Clause = clause(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9       )
  implicit def tuple2clause(x:(Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal)) : Clause = clause(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)

  def run(f : Term => Goal) : Stream[Term] = run(-1, f)
  def run(f : (Term, Term) => Goal) : Stream[(Term, Term)] = run(-1, f)
  def run(f : (Term, Term, Term) => Goal) : Stream[(Term, Term, Term)] = run(-1, f)
  def run(f : (Term, Term, Term, Term) => Goal) : Stream[(Term, Term, Term, Term)] = run(-1, f)

  def run(k:Int, f : Term => Goal) : Stream[Term] = {
    val q1 = Var()
    val res = f(q1)(Substitution.empty)
    val resCut = if(k > 0)
      res.take(k)
    else
      res.toScalaStream
    resCut.map(_.reify(q1))
  }

  def run(k:Int, f : (Term, Term) => Goal) : Stream[(Term, Term)] = {
    val q1 = Var()
    val q2 = Var()
    val res = f(q1, q2)(Substitution.empty)
    val resCut = if(k > 0)
      res.take(k)
    else
      res.toScalaStream
    resCut.map{ subs =>
      val Stream(a1, a2) = subs.reify(list(q1, q2)).asKList.toScalaStream
      (a1, a2)
    }
  }

  def run(k:Int, f : (Term, Term, Term) => Goal) : Stream[(Term, Term, Term)] = {
    val q1 = Var()
    val q2 = Var()
    val q3 = Var()
    val res = f(q1, q2, q3)(Substitution.empty)
    val resCut = if(k > 0)
      res.take(k)
    else
      res.toScalaStream
    resCut.map{ subs =>
      val Stream(a1, a2, a3) = subs.reify(list(q1, q2, q3)).asKList.toScalaStream
      (a1, a2, a3)
    }
  }
  def run(k:Int, f : (Term, Term, Term, Term) => Goal) : Stream[(Term, Term, Term, Term)] = {
    val q1 = Var()
    val q2 = Var()
    val q3 = Var()
    val q4 = Var()
    val res = f(q1, q2, q3, q4)(Substitution.empty)
    val resCut = if(k > 0)
      res.take(k)
    else
      res.toScalaStream
    resCut.map{ subs =>
      val Stream(a1, a2, a3, a4) = subs.reify(list(q1, q2, q3, q4)).asKList.toScalaStream
      (a1, a2, a3, a4)
    }
  }

  def conde(
             c1 : => Clause,
             c2 : => Clause=clause(FAIL),
             c3 : => Clause=clause(FAIL),
             c4 : => Clause=clause(FAIL),
             c5 : => Clause=clause(FAIL),
             c6 : => Clause=clause(FAIL),
             c7 : => Clause=clause(FAIL),
             c8 : => Clause=clause(FAIL),
             c9 : => Clause=clause(FAIL),
             c10: => Clause=clause(FAIL),
             c11: => Clause=clause(FAIL),
             c12: => Clause=clause(FAIL),
             c13: => Clause=clause(FAIL),
             c14: => Clause=clause(FAIL),
             c15: => Clause=clause(FAIL),
             c16: => Clause=clause(FAIL),
             c17: => Clause=clause(FAIL),
             c18: => Clause=clause(FAIL),
             c19: => Clause=clause(FAIL),
             c20: => Clause=clause(FAIL),
             c21: => Clause=clause(FAIL),
             c22: => Clause=clause(FAIL),
             c23: => Clause=clause(FAIL),
             c24: => Clause=clause(FAIL),
             c25: => Clause=clause(FAIL),
           ) : Goal = c1.whole  || c2.whole  || c3.whole  || c4.whole  || c5.whole  || c6.whole  || c7.whole  || c8.whole  || c9.whole  || c10.whole || c11.whole ||
                      c12.whole || c13.whole || c14.whole || c15.whole || c16.whole || c17.whole || c18.whole || c19.whole || c20.whole || c21.whole || c22.whole ||
                      c23.whole || c24.whole || c25.whole

  class Clause private[kanren](
                       cond  : =>Goal,
                       body1 : =>Goal,
                       body2 : =>Goal,
                       body3 : =>Goal,
                       body4 : =>Goal,
                       body5 : =>Goal,
                       body6 : =>Goal,
                       body7 : =>Goal,
                       body8 : =>Goal,
                       body9 : =>Goal,
                       body10: =>Goal,
                     ) extends Goal {
    lazy val first = cond
    lazy val rest  = body1 && body2 && body3 && body4 && body5 && body6 && body7 && body8 && body9 && body10
    lazy val whole = first && rest

    override def apply(s: Substitution): SearchStream[Substitution] = whole(s)
  }

  def clause(
    cond  : =>Goal,
    body1 : =>Goal = SUCCESS,
    body2 : =>Goal = SUCCESS,
    body3 : =>Goal = SUCCESS,
    body4 : =>Goal = SUCCESS,
    body5 : =>Goal = SUCCESS,
    body6 : =>Goal = SUCCESS,
    body7 : =>Goal = SUCCESS,
    body8 : =>Goal = SUCCESS,
    body9 : =>Goal = SUCCESS,
    body10: =>Goal = SUCCESS,
  ) : Clause = new Clause(cond, body1, body2, body3, body4, body5, body6, body7, body8, body9, body10)


  // special ignore var
  def __ : Var = Var()

  def fresh(f:  Var                                                                                       => Goal) : Goal = f(__                                                            )
  def fresh(f: (Var, Var)                                                                                 => Goal) : Goal = f(__, __                                                        )
  def fresh(f: (Var, Var, Var)                                                                            => Goal) : Goal = f(__, __, __                                                    )
  def fresh(f: (Var, Var, Var, Var)                                                                       => Goal) : Goal = f(__, __, __, __                                                )
  def fresh(f: (Var, Var, Var, Var, Var)                                                                  => Goal) : Goal = f(__, __, __, __, __                                            )
  def fresh(f: (Var, Var, Var, Var, Var, Var)                                                             => Goal) : Goal = f(__, __, __, __, __, __                                        )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var)                                                        => Goal) : Goal = f(__, __, __, __, __, __, __                                    )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var)                                                   => Goal) : Goal = f(__, __, __, __, __, __, __, __                                )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var)                                              => Goal) : Goal = f(__, __, __, __, __, __, __, __, __                            )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                                         => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __                        )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                                    => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __                    )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                               => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __                )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                          => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __            )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                     => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __, __        )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)                => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __, __, __    )
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)           => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __)
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var)      => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __)
  def fresh(f: (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var, Var) => Goal) : Goal = f(__, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __)

  val FAIL    : Goal = _ => SearchStream.empty
  val SUCCESS : Goal = SearchStream(_)
  val ALWAYS  : Goal = SUCCESS || ALWAYS
  val NEVER   : Goal = s => SearchStreamSuspension( NEVER(s) )

}

