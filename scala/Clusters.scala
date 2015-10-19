import scala.io.Source
import scala.xml.{XML, NodeSeq}
import scala.math._

package org.pii.collective.cluster {
  class Cluster(
    val vec: List[Double],
    val left: Option[Cluster] = None,
    val right: Option[Cluster] = None,
    val distance: Double = 0.0,
    val id: Option[Int] = None
    )

  object Cluster {
    def apply(vec: List[Double], left: Option[Cluster] = None, right: Option[Cluster] = None, distance: Double = 0.0, id: Option[Int] = None) = {
      new Cluster(vec, left, right, distance, id)
    }

    def readfile(filename: String): (List[String], List[String], List[List[Double]]) = {
      val source = Source.fromFile("blogdata.txt")
      val lines = source.getLines().toList
      source.close
      val colnames = lines(0).trim.split("\t").tail.toList
      def counting(lines: List[String]): (List[String], List[List[Double]]) =
        lines match {
          case Nil => (Nil, List.empty[List[Double]])
          case a :: as => {
            val (rownames, data) = counting(as)
            val line = a.trim.split("\t")
            (line(0) :: rownames, line.tail.map(_.toDouble).toList :: data)
          }
        }
      val (rownames, data) = counting(lines.tail)
      (rownames, colnames, data)
    }

    def pearson(v1: List[Double], v2: List[Double]): Double = {
      val sum1 = v1.sum
      val sum2 = v2.sum
      val sum1Sq = v1.map(x => pow(x, 2)).sum
      val sum2Sq = v2.map(x => pow(x, 2)).sum
      val pSum = (for (i <- Range(0, v1.size)) yield {v1(i) * v2(i)}).sum
      val num = pSum - (sum1 * sum1 / v1.size)
      val den = sqrt((sum1Sq - pow(sum1, 2) / v1.size) * (sum2Sq - pow(sum2, 2) / v1.size))
      if(den == 0) return 0
      1.0 - num / den
    }

    def hcluster(rows: List[List[Double]], distance: (List[Double], List[Double]) => Double = pearson): Cluster = {
      // The first clusters is each of the rows
      val clust = (for (i <- Range(0, rows.size)) yield {
        Cluster(rows(i), id = Some(i))
      }).toList

      def clustering(clust: List[Cluster], cid: Int = -1): List[Cluster] = {
        val length = clust.size
        if(length <= 1){
          clust
        } else {
          // Compare distances, find the closest cluster pair
          val closest_distance = clust.combinations(2).map(x => 
              distance(x.head.vec, x.tail.head.vec)).min
          val closest_pair = clust.combinations(2).filter(x => 
              distance(x.head.vec, x.tail.head.vec) == closest_distance).toList.head
          val merge_vec = (closest_pair.head.vec zip closest_pair.tail.head.vec).map(x => 
              (x._1 + x._2) / 2).toList
          val new_cluster = apply(merge_vec, left = Some(closest_pair.head), right = Some(closest_pair.tail.head), id = Some(cid))

          clustering(new_cluster :: clust.filter(x => 
              x.vec != closest_pair(0).vec && x.vec != closest_pair(1).vec), cid - 1)
        }
      }
      clustering(clust)(0)
    }

    def printclust(clust: Cluster, labels: Option[List[String]], n: Int = 0): Unit = {
      for (i <- Range(0, n)) print(" ")
      if(clust.id.get < 0) {
        println("-")
      } else {
        if(labels == None) println(clust.id.get)
        else println(labels.get(clust.id.get))
      }
      if(clust.left != None) printclust(clust.left.get, labels = labels, n = n + 1)
      if(clust.right != None) printclust(clust.right.get, labels = labels, n = n + 1)
    }
  }
}
