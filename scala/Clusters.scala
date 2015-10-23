import scala.io.Source
import scala.math._
import scala.util.Random
import scala.xml.{XML, NodeSeq}

import java.awt.Image
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import javax.imageio.ImageIO
import java.awt.Color
import java.awt.geom._
import java.io.File

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
    
    def getheight(clust: Cluster): Double = {
      clust match {
        case clust if clust.left == None && clust.right == None =>
          1
        case clust =>
          getheight(clust.left.get) + getheight(clust.right.get)
      }
    }

    def drawdendrogram(clust:Cluster, labels:Option[List[String]],                
    jpeg:String="cluster.jpg"):Unit = {
    // 背景作成のための高さと幅を定義
    val h = getheight(clust) * 20
    val w = 1200
    // クラスタの深さを取得して縮尺を定義
    val depth = getdepth(clust)
    val scaling = (w - 150) / depth
                
    // 描画用のオブジェクトを作成
    val im = new BufferedImage(w.toInt, h.toInt, BufferedImage.TYPE_INT_RGB)
    var g = im.createGraphics()
    // 背景を描画
    g.setPaint(Color.white)
    g.fill(new Rectangle2D.Double(0, 0, w, h))
    g.drawImage(im, null, 0, 0)
                
    // ノードの描画を開始
    g = drawnode(g, clust, 10, (h / 2).toInt, scaling, labels)
                
    // ファイルに書き出し
    try {       
      ImageIO.write(im, "jpeg", new File(jpeg))
    }catch {    
      case e:Exception => println("image write error")
    }           
  }

    def drawnode(draw: Graphics2D, clust: Cluster, x: Int, y: Int, scaling: Double, labels: Option[List[String]]): Graphics2D = {
      if(clust.id.get < 0){
        val h1 = getheight(clust.left.get) * 20
        val h2 = getheight(clust.right.get) * 20
        val top = y - (h1 + h2) / 2
        val bottom = y + (h1 + h2) / 2

        val ll = clust.distance * scaling

        draw.setPaint(Color.red)
        draw.draw(new Line2D.Double(x, top + h1 / 2, x, bottom - h2 / 2))
        draw.draw(new Line2D.Double(x, bottom - h2 / 2, x + ll, bottom - h2 / 2))
        draw.draw(new Line2D.Double(x, top + h1 / 2, x + ll, top + h1 / 2))

        drawnode(draw, clust.left.get, (x + ll).toInt, (top + h1 / 2).toInt, scaling, labels)
        drawnode(draw, clust.right.get, (x + ll).toInt, (top - h2 / 2).toInt, scaling, labels)
      } else {
        val label = if (labels != None) labels.get(clust.id.get) else clust.id.get.toString
        draw.setPaint(Color.black)
        draw.drawString(label, x, y)
      }
      return draw
    }

    def scaledown(data: List[List[Double]], distance: (List[Double], List[Double]) => Double = pearson, rate: Double = 1.01): (List[Double], List[Double]) = {
      val n = data.size
      val realdist = data.combinations(2).map(x =>
          distance(x(0), x(1))).toList
      // create random location
      val loc = List.fill(n)(Random.nextDouble()) zip List.fill(n)(Random.nextDouble())
      // phisical distance between each random points
      val fakedist = loc.combinations(2).map(x =>
          sqrt((pow((x(0)._1 - x(1)._1), 2) + pow((x(0)._2 - x(1)._2), 2)))).toList
      var totalerror = 0.0
      val grad = for (com <- loc.combinations(2); fd <- fakedist; rd <- realdist) yield {
        val errorterm = (fd - rd) / rd
        totalerror = totalerror + abs(errorterm)
        (((com(1)._1 - com(2)._1) / fd) * errorterm, ((com(1)._2 - com(2)._2) / fd) * errorterm, totalerror)
      }
      val fixed_loc = for (gr <- grad, lc <- loc) yield {
        // TODO if lasterror < totalerror
        (loc._1 - rate * gr._1, loc._2 - rate * gr._2)
      }
    }
  }
}
