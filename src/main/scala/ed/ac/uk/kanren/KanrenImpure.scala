package ed.ac.uk.kanren

import Kanren._

object KanrenImpure {

  def ifte(g1: => Goal, g2: => Goal, g3: => Goal) : Goal =
    s => g1(s).partiallyForceEval match {
      case _:SearchStreamEmpty[Substitution] => g3(s)
      case ss => ss.flatMap(g2(_))(Kanren.searchStrategy)
    }

  def not(g: => Goal) : Goal = ifte(g, FAIL, SUCCESS)

  def once(g: => Goal) : Goal = (s: Substitution) => g(s).take(1).headOption.map(SearchStream(_)).getOrElse(SearchStream.empty)

  def condA(c1: => Clause                                                                                                                                                        ) : Goal = c1.whole
  def condA(c1: => Clause, c2: => Clause                                                                                                                                         ) : Goal = ifte(c1.first, c1.rest, condA(c2                                      ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause                                                                                                                          ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3                                  ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause                                                                                                           ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4                              ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause                                                                                            ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5                          ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause                                                                             ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6                      ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause, c7: => Clause                                                              ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6, c7                  ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause, c7: => Clause, c8: => Clause                                               ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6, c7, c8              ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause, c7: => Clause, c8: => Clause, c9: => Clause                                ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6, c7, c8, c9          ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause, c7: => Clause, c8: => Clause, c9: => Clause, c10: => Clause                ) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6, c7, c8, c9, c10     ))
  def condA(c1: => Clause, c2: => Clause, c3: => Clause, c4: => Clause, c5: => Clause, c6: => Clause, c7: => Clause, c8: => Clause, c9: => Clause, c10: => Clause, c11: => Clause) : Goal = ifte(c1.first, c1.rest, condA(c2, c3, c4, c5, c6, c7, c8, c9, c10, c11))

  def condU(c1 : => Clause,
            c2 : => Clause = clause(FAIL),
            c3 : => Clause = clause(FAIL),
            c4 : => Clause = clause(FAIL),
            c5 : => Clause = clause(FAIL),
            c6 : => Clause = clause(FAIL),
            c7 : => Clause = clause(FAIL),
            c8 : => Clause = clause(FAIL),
            c9 : => Clause = clause(FAIL),
            c10: => Clause = clause(FAIL),
            c11: => Clause = clause(FAIL),
           ) : Goal = once(condA(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11))

  def project(x1:Term                                    )(f:  Term                          => Goal ) : Goal = s => f( s.walkStar(x1)                                                                 )(s)
  def project(x1:Term, x2:Term                           )(f: (Term, Term)                   => Goal ) : Goal = s => f( s.walkStar(x1), s.walkStar(x2)                                                 )(s)
  def project(x1:Term, x2:Term, x3:Term                  )(f: (Term, Term, Term)             => Goal ) : Goal = s => f( s.walkStar(x1), s.walkStar(x2), s.walkStar(x3)                                 )(s)
  def project(x1:Term, x2:Term, x3:Term, x4:Term         )(f: (Term, Term, Term, Term)       => Goal ) : Goal = s => f( s.walkStar(x1), s.walkStar(x2), s.walkStar(x3), s.walkStar(x4)                 )(s)
  def project(x1:Term, x2:Term, x3:Term, x4:Term, x5:Term)(f: (Term, Term, Term, Term, Term) => Goal ) : Goal = s => f( s.walkStar(x1), s.walkStar(x2), s.walkStar(x3), s.walkStar(x4), s.walkStar(x5) )(s)

  def plusI(x:Term, y:Term, z:Term) : Goal = project(x, y, z){
    case ( Val(x:Int), Val(y:Int), _          ) => z === Val(x+y)
    case ( Val(x:Int), _         , Val(z:Int) ) => y === Val(z-x)
    case ( _         , Val(y:Int), Val(z:Int) ) => x === Val(z-y)
    case _                                      => FAIL
  }

  def copyTermI(u:Term, v:Term) : Goal = project(u)( alphaTransform(_) === v )

  def alphaTransform(u:Term) : Term = {
    val mapping : Map[Var, Var]  = allVars(u).toList.map(x => (x, Var())).toMap
    replaceVars(u, mapping)
  }

  private def replaceVars(u:Term, mapping:Map[Var, Var]) : Term = u match {
    case x:Var => mapping.getOrElse(x, x)
    case KCons(h, t) => KCons(replaceVars(h, mapping), replaceVars(t, mapping))
    case x => x
  }

  private def allVars(u:Term) : Set[Var] = u match {
    case u:Var => Set(u)
    case KCons(h, t) => allVars(h) union allVars(t)
    case _ => Set()
  }

}
