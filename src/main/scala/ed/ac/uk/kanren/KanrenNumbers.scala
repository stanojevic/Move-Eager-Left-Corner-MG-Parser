package ed.ac.uk.kanren

import Kanren._
import KanrenList._

object KanrenNumbers {

  def buildNum(n:Int) : KList = {
    assert(n>=0)
    if(n % 2 == 1)
      cons(1, buildNum((n-1)/2))
    else if(n != 0 && n%2 == 0)
      cons(0, buildNum(n/2))
    else // if(n == 0)
      KList()
  }

  def unbuildNum(t:Term, degree:Int=0) : Int = t match {
    case KNil => 0
    case KCons(Val(1), KNil) => math.pow(2, degree).toInt
    case KCons(Val(x:Int), t) => x*math.pow(2, degree).toInt + unbuildNum(t, degree+1)
    case _ => throw new Exception("fail to unbuild the number")
  }

  def positivO(n : Term) : Goal =
    listImproper(__, __) === n

  def gtOneO(n : Term) : Goal =
    listImproper(__, __, __) === n

  def addO(x:Term, y:Term, r:Term) : Goal = adderO(0, x, y, r)

  def minusO(x:Term, y:Term, r:Term) : Goal = addO(y, r, x)

  def multO(n:Term, m:Term, p:Term) : Goal = conde(
    (nullO(n), nullO(p)),
    (positivO(n), nullO(m), nullO(p)),
    (n===list(1), positivO(m), m===p),
    (gtOneO(n), m===list(1), n===p),
    fresh( (x, z) => (
      consO(0, x, n), positivO(x), consO(0, z, p), positivO(z), gtOneO(m), multO(x, m, z)
    )),
    fresh( (x, y) => (
      consO(1, x, n), positivO(x), consO(0, y, m), positivO(y), multO(m, n, p)
    )),
    fresh( (x, y) => (
      consO(1, x, n), positivO(x), consO(1, y, m), positivO(y), oddMultO(x, n, m, p)
    ))
  )

  def ltO(n:Term, m:Term) : Goal = conde(
    ltLenO(n, m),
    (
      eqLenO(n, m),
      fresh( x =>
        positivO(x) && addO(n, x, m)
      )
    )
  )

  def ltOrEqO(n:Term, m:Term) : Goal = conde(
    n === m,
    ltO(n, m)
  )

  private def oddMultO(x:Term, n:Term, m:Term, p:Term) : Goal = fresh( q => (
    boundStarO(q, p, n, m),
    multO(x, m, q),
    addO(listImproper(0, q), m, p)
  ))

  private def boundStarO(q:Term, p:Term, n:Term, m:Term) : Goal = conde(
    (nullO(q), positivO(p)),
    fresh( (a0, a1, a2, a3, x, y, z) => (
      consO(a0, x, q),
      consO(a1, y, p),
      conde(
        (nullO(n), consO(a2, z, m), boundStarO(x, y, z, list())),
        (consO(a3, z, n), boundStarO(x, y, z, m))
      )
    )))

  def lengthO(l:Term, n:Term) : Goal = conde(
    (nullO(l), nullO(n)),
    fresh((d, res) => (
      cdrO(l, d),
      addO(list(1), res, n),
      lengthO(d, res)
    ))
  )

//  def divO(n:Term, m:Term, quotent:Term, remein:Term) : Goal = conde(
//    (nullO(quotent), n===remein, ltO(n, m)),
//    (quotent===list(1), nullO(remein), n===m, ltO(remein, m)),
//    (ltO(m, n), ltO(remein, m), fresh(mq => (
//      ltOrEqO(mq, n),
//      multO(m, quotent, mq),
//      addO(mq, remein, n)
//    )))
//  )

//  def divO(n:Term, m:Term, quotent:Term, remein:Term) : Goal = fresh( mq => (
//    ltO(remein, m),
//    ltOrEqO(mq, n),
//    multO(m, quotent, mq),
//    addO(mq, remein, n)
//  ))

  /**
    * THIS DOESN'T WORK
    */
//  @deprecated
//  def divO(n:Term, m:Term, quotent:Term, remein:Term) : Goal = conde(
//    (nullO(quotent), remein===n, ltO(n, m)),
//    (
//      quotent === list(1),
//      eqLenO(m, n),
//      addO(remein, m, n),
//      ltO(remein, m)
//    ),
//    (
//      positivO(quotent),
//      ltO(m, n),
//      ltO(remein, m),
//      nWiderThanMO(n, m, quotent, remein)
//    )
//  )

//  private def nWiderThanMO(n:Term, m:Term, q:Term, r:Term) : Goal = fresh( (nHigh, nLow, qHigh, qLow, mqLow, mrqLow, rr, rHigh) => (
//    splitO(n, r, nLow, nHigh),
//    splitO(q, r, qLow, qHigh),
//    conde(
//      (
//        nullO(nHigh),
//        nullO(qHigh),
//        minusO(nLow, r, mqLow),
//        multO(m, qLow, mqLow)
//      ),
//      (
//        positivO(nHigh),
//        multO(m, qLow, mqLow),
//        addO(r, mqLow, mrqLow),
//        minusO(mrqLow, nLow, rr),
//        splitO(rr, r, list(), rHigh),
//        divO(nHigh, m, qHigh, rHigh)
//      )
//    )
//  ))

//  private def splitO(n:Term, r:Term, l:Term, h:Term) : Goal = conde(
//    (nullO(n), nullO(h), nullO(l)),
//    fresh( (b, np) => (
//      listImproper(0, b, np) === n,
//      nullO(r),
//      listImproper(b, np) === h,
//      nullO(l)
//    )),
//    fresh( np => (
//      listImproper(1, np) === n,
//      nullO(r),
//      np === h,
//      l === list(1)
//    )),
//    fresh( (b, np, a, rp) => (
//      listImproper(0, b, np) === n,
//      listImproper(a, rp) === r,
//      nullO(l),
//      splitO(listImproper(b, np), rp, list(), h)
//    )),
//    fresh( (np, a, rp) => (
//      listImproper(1, np) === n,
//      listImproper(a, rp) === r,
//      l === list(1),
//      splitO(np, rp, list(), h)
//    )),
//    fresh( (b, np, a, rp, lp) => (
//      listImproper(b, np) === n,
//      listImproper(a, rp) === r,
//      listImproper(b, lp) === l,
//      positivO(lp),
//      splitO(np, rp, lp, h)
//    ))
//  )

  def numO(n:Term) : Goal = conde(
    nullO(n),
    n===list(1),
    (
      gtOneO(n),
      fresh( t => conde(
        (consO(0, t, n), numO(t)),
        (consO(1, t, n), numO(t)),
      ))
    )
  )

  private def fullAdderO(b:Term, x:Term, y:Term, r:Term, c:Term) : Goal = conde(
    (b===0, x===0, y===0, r===0, c===0),
    (b===1, x===0, y===0, r===1, c===0),
    (b===0, x===1, y===0, r===1, c===0),
    (b===1, x===1, y===0, r===0, c===1),
    (b===0, x===0, y===1, r===1, c===0),
    (b===1, x===0, y===1, r===0, c===1),
    (b===0, x===1, y===1, r===0, c===1),
    (b===1, x===1, y===1, r===1, c===1)
  )

  private def genAdderO(b:Term, n:Term, m:Term, r:Term) : Goal = fresh( (a, c, d, e, x, y, z) => (
    cons(a, x) === n,
    cons(d, y) === m,
    positivO(y),
    cons(c, z) === r,
    positivO(z),
    fullAdderO(b, a, d, c, e),
    adderO(e, x, y, z)
  ))

  private def adderO(b:Term, n:Term, m:Term, r:Term) : Goal = conde(
    (b === 0, nullO(m), n===r),
    (b === 0, nullO(n), m===r, positivO(m)),
    (b === 1, nullO(m), adderO(0, n, list(1), r)),
    (b === 1, nullO(n), positivO(m), adderO(0, list(1), m, r)),
    (n === list(1), m === list(1), fresh( (a, c) => list(a,c) === r && fullAdderO(b, 1, 1, a, c))),
    (n === list(1), genAdderO(b, n, m, r)),
    (m === list(1), gtOneO(n), gtOneO(r), adderO(b, list(1), n, r)),
    (gtOneO(n), genAdderO(b, n, m, r))
  )

  private def eqLenO(n:Term, m:Term) : Goal = conde(
    (nullO(n), nullO(m)),
    (n===list(1), m===list(1)),
    fresh( (x, y) => (
      cdrO(x, n), positivO(x),
      cdrO(y, m), positivO(y),
      eqLenO(x, y)
    ))
  )

  private def ltLenO(n:Term, m:Term) : Goal = conde(
    (nullO(n), positivO(m)),
    (n===list(1), gtOneO(m)),
    fresh( (x, y) => (
      cdrO(x, n), positivO(x),
      cdrO(y, m), positivO(y),
      ltLenO(x, y)
    ))
  )

}

