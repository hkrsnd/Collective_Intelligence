import scala.io.Source
import scala.xml.{XML, NodeSeq}
import scala.math._

package org.pii.collective.cluster {
  object Cluster {
    def readfile(filename: String): (List[String], List[String], List[List[Int]]) = {
      val source = Source.fromFile("blogdata.txt")
      val lines = source.getLines().toList
      source.close

      val colnames = lines(0).trim.split("\t").tail.toList
      
      def counting(lines: List[String]): (List[String], List[List[Int]]) =
        lines match {
          case Nil => (Nil, List.empty[List[Int]])
          case a :: as => {
            val (rownames, data) = counting(as)
            val line = a.trim.split("\t")
            (line(0) :: rownames, line.tail.map(_.toInt).toList :: data)
          }
        }

      val (rownames, data) = counting(lines.tail)
      (rownames, colnames, data)
    }

    def pearson(v1: List[Int], v2: List[Int]): Double = {
      sum1 = v1.sum
      sum2 = v2.sum

      sum1Sq = v1.map(x => x.pow(2)).sum
      sum2Sq = v2.map(x => x.pow(2)).sum

      pSum = v1.map(x => v2.map(y => ))
    }
  }
}
