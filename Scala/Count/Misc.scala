package Count

import org.nd4j.linalg.api.ndarray.INDArray
import Lmer.LmerName
import Gkm.GkmName
import org.nd4j.linalg.factory.Nd4j

/**
  * Created by Jack on 2018/5/7.
  */
object Misc {

  def combn(x: Int, m: Int): Array[Array[Int]] = {
    // Tu produce combinational numbers
    if (m > x) throw new UnsupportedOperationException("m is bigger than x")
    (0 until x).toArray.combinations(m).toArray
  }

  /*
  Calculate the reverse complement sequences of the input sequences
   */
  def ReverseComp(s: Array[String]): Array[String] = {
    val s1 = s.map(_.toUpperCase)
    val s2 = s1.map(_.map {
      case 'A' => 'T'
      case 'C' => 'G'
      case 'G' => 'C'
      case 'T' => 'A'
      case _ => 'N'}.reverse)
    s2
  }

  /*
  Input should be output of LmerName or GkmName.
  Given an array of DNA strings, calculate the matching of the DNAs.
   */
  def DNAReverseMatch(s: Array[String]): Array[Array[Int]] = {
    val s1 = s.map(_.toUpperCase)
    val s2 = ReverseComp(s1)

    val t = s1.map(s2.indexOf(_)).zipWithIndex
    val tt= t.filter(x => x._1 < x._2)
    val t1: Array[Int] = tt.map(_._1)
    val t2: Array[Int] = tt.map(_._2)
    val t3: Array[Int] = t.filter(x => x._1 == x._2).map(_._1)
    Array(t1, t2, t3)
  }

  private def Lmer2Gkm(lmer: String, gap: Array[Array[Int]], k:Int, leng: Int):Array[Float] = {
    val result = Array.fill(leng)(0F)
    val nucMat = Array.fill(gap.length)(lmer.toCharArray.map(_.toString))
    for {i <- nucMat.indices
         j <- gap(i)}
      nucMat(i)(j) = ""
    for {i <- nucMat.indices
         t = i * math.pow(4, k).toInt +  Integer.parseInt(nucMat(i).mkString(""), 4)} result(t) = 1
    result
  }

  /*
  Calculating the transformation matric from lmerfv to gkmcfv.
  gapEnd: whether the desired gkms should contain gaps at the ends
  rc: whether the desired gkms are counted using the double strand DNA
   */
  def Lmer2Gkm(l: Int,
               k: Int,
               gapEnd: Boolean = false,
               rc: Boolean = true): INDArray = {
    // generate transformation matrix from Lmerfv to gkmfv
    val lmer = LmerName(l).map(_.map {
      case 'A' => '0'
      case 'C' => '1'
      case 'G' => '2'
      case 'T' => '3'})
    val gap = if (gapEnd) combn(l, l - k) else combn(l - 2, l - k).map(x => x.map(_+1))
    val leng = math.pow(4, k).toInt * gap.length
    val t = lmer.par.map(Lmer2Gkm(_, gap, k, leng)).toArray
    val result = Nd4j.create(t)
    // deal with double strand information
    if (!rc) return result
    val dsMatch = Misc.DNAReverseMatch(GkmName(l, k, gapEnd = gapEnd, degenerate = false))
    val t1 = result.getColumns(dsMatch(0):_*).add(result.getColumns(dsMatch(1):_*))
    if (dsMatch(2).isEmpty) t1 else Nd4j.hstack(t1, result.getColumns(dsMatch(2):_*))
  }

  def ReadFasta(file: String): Array[String] = {
    scala.io.Source.fromFile(file).getLines().zipWithIndex.filter(_._2 % 2 != 0).map(_._1).toArray
  }

  def main(args: Array[String]): Unit = {
    val a = Lmer2Gkm(7, 5)
    print("Finish")
  }

}
