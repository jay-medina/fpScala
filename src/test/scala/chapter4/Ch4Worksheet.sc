import chapter4._

val temp = Some(4)

val arr = (1 to 4).toArray.map(_.toDouble)

def mean(xs: Seq[Double]): Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)

var meanValue = mean(arr) match { case Some(v) => v}

val arr2 = arr.map(x => Math.pow(x - meanValue, 2))

mean(arr2)


