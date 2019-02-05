package ed.ac.uk.kanren

import Kanren._

object KanrenList {

  implicit class ScalaList2KList(xs:List[Term]){
    def asKList : KList = KList.fromList(xs)
  }

  def listImproper(xs:Term*) : KList = {
    assert(xs.size > 1)
    listImproper(xs.toList)
  }

  private def listImproper(xs:List[Term]) : KList =
    if(xs.size == 2)
      cons(xs(0), xs(1))
    else
      cons(xs.head, listImproper(xs.tail))

  def consO(h:Term, t:Term, l:Term) : Goal =
    KCons(h, t) === l

  def carO(h:Term, l:Term) : Goal =
    consO(h, __, l)

  def cdrO(t:Term, l:Term) : Goal =
    consO(__, t, l)

  def nullO(x:Term) : Goal =
    x === KNil

  def appendO(l:Term, t:Term, out:Term) : Goal =
    conde(
      (
        nullO(l),
        t === out
      ),
      (
        fresh((a, d, res) => (
        consO(a, d, l),
        consO(a, res, out),
        appendO(d, t, res)
      )))
    )

  def memberO(x:Term, l:Term) : Goal =
    conde(
      consO(x, __, l),
      fresh(y => (consO(__, y, l), memberO(x, y)))
    )

  def listO(l:Term) : Goal = conde(
    nullO(l),
    fresh(t => cdrO(t, l) && listO(t) )
  )

  def selectO(e:Term, l:Term, rest:Term) : Goal =
    conde(
      consO(e, rest, l),
      fresh((h, t, taie) => (
        consO(h, t, l),
        consO(h, taie, rest),
        selectO(e, t, taie)
      ))
    )

  def sameLengthO(l:Term, r:Term) : Goal =
    conde(
      (nullO(l), nullO(r)),
      (fresh((lt, rt) => (
        cdrO(lt, l),
        cdrO(rt, r),
        sameLengthO(lt, rt)
      )))
    )

  def reversO(l:Term, r:Term) : Goal =
    conde(
      (
        nullO(l),
        nullO(r)
      ),
      (fresh((lh, lt, ltr) => (
        consO(lh, lt, l),
        appendO(ltr, KList(lh), r),
        reversO(lt, ltr)
      )))
    )

  def replaceO(toRemove:Term, toInsert:Term, l:Term, r:Term) : Goal = fresh( (head, tailL, tailR) =>
    conde(
      (nullO(l), FAIL),
      (consO(toRemove, tailL, l), consO(toInsert, tailL, r)),
      (consO(head, tailL, l), consO(head, tailR, r), replaceO(toRemove, toInsert, tailL, tailR))
    ))

  def assocO(keys:Term, values:Term, set:Term) : Goal = fresh( (k, v, tSet, tValues, tKeys) =>
    conde(
      (nullO(keys), nullO(values), nullO(set)),
      (
        consO(k, tKeys, keys),
        consO(v, tValues, values),
        consO(list(k, v), tSet, set),
        assocO(tKeys, tValues, tSet)
      )
    ))

}
