case class MyRandom(seed: Long) {

  //T1
  def nextInt: (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = ((newSeed >>> 16).toInt)
    (n, nextRandom)
  }
}