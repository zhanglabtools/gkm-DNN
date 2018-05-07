package Count

import Misc.DNAReverseMatch
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

/**
  * Created by Jack on 2017/11/9.
  */

object Lmer {

  def LmerName(l: Int = 2, alphabet: String = "ACGT"): Array[String] = {
    if (l <= 0) throw new UnsupportedOperationException("word length is less than 1")
    if (l == 1) return alphabet.sliding(1).toArray
    for {i <- LmerName(l - 1, alphabet)
         j <- alphabet} yield i + j
  }

  /*
  Count a single DNA string to lmer frequency vector
   */
  def Lmerfv(sequence: String, l: Int): Array[Float] = {
    val leng = math.pow(4, l).toInt
    val lmers = sequence.toUpperCase().map {
      case 'A' => '0'
      case 'C' => '1'
      case 'G' => '2'
      case 'T' => '3'
      case _ => '0'
    }.sliding(l).map(Integer.parseInt(_, 4))
    val result: Array[Float] = Array.fill(leng)(0)
    lmers.foreach(result(_) += 1)
    result
  }

  /*
  Count DNA strings to lmer frequency vectors.
  Whether counting the reverse complement sequence is optional.
   */
  def Lmerfv(sequences: Array[String], l: Int, rc: Boolean = true): INDArray = {
    // count the frequency of lmers
    val count = sequences.par.map(Lmerfv(_, l)).toArray  // parallel version
    val rawCount = Nd4j.create(count)
    // deal with double strand information
    if (!rc) return rawCount
    val dsMatch = DNAReverseMatch(LmerName(l))
    val t1 = rawCount.getColumns(dsMatch(0):_*).add(rawCount.getColumns(dsMatch(1):_*))
    if (dsMatch(2).isEmpty) t1 else Nd4j.hstack(t1, rawCount.getColumns(dsMatch(2):_*))
  }
}
