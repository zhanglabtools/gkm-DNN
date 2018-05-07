package Count

import Lmer.LmerName
import Lmer.Lmerfv
import Misc.combn
import Misc.Lmer2Gkm
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

import scala.collection.mutable

/**
  * Created by Jack on 2017/11/11.
  */

object Gkm {

  private def help1(locat: Array[Int],
            nucMat: Array[String],
            l: Int = 7):Array[String] = {
    val gapMat = Array.fill(nucMat.length, l)('N')
    for {i <- nucMat.indices
         j <- locat.indices}
      gapMat(i)(locat(j)) =nucMat(i)(j)
    gapMat.map(_.mkString(""))
  }

  def GkmName(l: Int = 7,
              k: Int = 5,
              alphabet: String = "ACGT",
              gapEnd: Boolean = false,
              degenerate: Boolean = true): Array[String] = {

    // special cases
    if (k > l) throw new UnsupportedOperationException("k is bigger than l")
    if (k == l) return Lmer.LmerName(l, alphabet)
    if (k == 1 && !gapEnd) return Lmer.LmerName(1, alphabet)

    // common cases
    val gap = if (gapEnd) combn(l, l - k) else combn(l - 2, l - k).map(x => x.map(_+1))
    val loc = gap.map(x => (0 until l).toArray.diff(x))
    val gkmName = loc.flatMap(help1(_, LmerName(k, alphabet), l))

    if (!degenerate | gapEnd) return gkmName

    val gkmName1 = GkmName(l = l - 1, k = k, alphabet = alphabet,
      gapEnd = gapEnd, degenerate = degenerate)
    gkmName1 ++ gkmName
  }

  private def Gkmfv(sequences: Array[String],
            l: Int,
            k:Int,
            gapEnd: Boolean = false,
            rc: Boolean = true): INDArray = {
    // only two steps to get the result
    // 1) generate the Lmer-fv (single strand)
    // 2) transform to gkm-fv
    val lmerfv = Lmerfv(sequences, l, rc = false)
    val trans = Lmer2Gkm(l, k, gapEnd = gapEnd, rc = rc) // quick enough currently!
    lmerfv.mmul(trans)
  }

  def Gkmcfv(sequences: Array[String],
             l: Int,
             k:Int,
             rc: Boolean): INDArray = {
    if (k > l) throw new UnsupportedOperationException("k is bigger than l")
    val lmerfv = Lmerfv(sequences, k, rc)
    if (k == l) return lmerfv

    val result = new Array[INDArray](l - k + 1)
    result(0) = lmerfv
    for (i <- 1 to l-k) result(i) = Gkmfv(sequences, i+k, k, rc = rc)
    Nd4j.hstack(result:_*)
  }

  private def Gkmfv(sequences: Array[String],
                    l: Int,
                    k:Int,
                    trans: mutable.Map[String, INDArray]): INDArray = {
    // use the predefined transformation
    // 1) generate the Lmer-fv (single strand)
    // 2) transform to gkm-fv
    val lmerfv = Lmerfv(sequences, l, rc = false)
    val transformation = trans(l+"-"+k) // extremely quick
    lmerfv.mmul(transformation)
  }

  /*
  Using cache to speed up the calculation.
   */
  def Gkmcfv(sequences: Array[String],
             l: Int,
             k:Int,
             rc: Boolean,
             trans: mutable.Map[String, INDArray]): INDArray = {
    if (k > l) throw new UnsupportedOperationException("k is bigger than l")
    val lmerfv = Lmerfv(sequences, k, rc)
    if (k == l) return lmerfv

    val result = new Array[INDArray](l - k + 1)
    result(0) = lmerfv
    for (i <- 1 to l-k) result(i) = Gkmfv(sequences, i+k, k, trans = trans)
    Nd4j.hstack(result:_*)
  }
}
