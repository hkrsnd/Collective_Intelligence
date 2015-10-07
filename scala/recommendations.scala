import Math.{sqrt, pow}

package org.pii.collective.recommend{
  object Recommendation {
    val critics: Map[String, Map[String, Double]] = Map(
      "Lisa Rose" -> Map("Lady in water" -> 2.5, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0, "Superman Returns" -> 3.5, "You, Me and Dupree" -> 2.5, "The Night Listener" -> 3.0),
      "Gene Seymour" -> Map("Lady in water" -> 3.0, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 1.5, "Superman Returns" -> 5.0, "You, Me and Dupree" -> 3.0, "The Night Listener" -> 3.0),
      "Michael Phillips" -> Map("Lady in water" -> 2.5, "Snakes on a Plane" -> 3.0, "Superman Returns" -> 3.5, "The Night Listener" -> 4.0),
      "Claudia Puig" -> Map("Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0, "Superman Returns" -> 4.0, "You, Me and Dupree" -> 2.5, "The Night Listener" -> 4.5),
      "Mick LaSalle" -> Map("Lady in the water" -> 3.0, "Snakes on a Plane" -> 4.0, "Just My Luck" -> 2.0, "Superman Returns" -> 3.0, "You, Me and Dupree" -> 2.0, "The Night Listener" -> 3.0),
      "Jack Matthews" -> Map("Lady in water" -> 3.0, "Snakes on a Plane" -> 4.0, "Superman Returns" -> 5.0, "You, Me and Dupree" -> 3.5, "The Night Listener" -> 3.0),
      "Toby" -> Map("Snakes on a Plane" -> 4.5, "Superman Returns" -> 4.0, "You, Me and Dupree" -> 1.0)
    )

    def sim_distance(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
      // if nothing is commented by both, return 0
      if(!prefs(person1).exists(p1 => prefs(person2).exists(p2 => p1._1 == p2._1)))
        return 0

      val sum_of_squares = prefs(person1).filter(p1 => 
          prefs(person2).exists(p2 => p1._1 == p2._1)).map(p1 =>
            pow(prefs(person1)(p1._1) - prefs(person2)(p1._1), 2)).sum

      1 / (1 + sum_of_squares)
    }

    def sim_distance(person1: String, person2: String): Double = {
      sim_distance(critics, person1: String, person2: String)
    }

    def sim_pearson(prefs: Map[String, Map[String, Double]], p1: String, p2: String): Double = {
      val si = prefs(p1).filter(x => prefs(p2).contains(x._1)).map(_._1)

      val n = si.size
      if(n == 0) return 0

      val sum1 = si.map(prefs(p1)(_)).sum
      val sum2 = si.map(prefs(p2)(_)).sum

      val sum1Sq = si.map(x => pow(prefs(p1)(x), 2)).sum
      val sum2Sq = si.map(x => pow(prefs(p2)(x), 2)).sum

      val pSum = si.map(x => prefs(p1)(x) * prefs(p2)(x)).sum

      //pearson score
      val num = pSum - (sum1 * sum2 / n)
      val den = sqrt(sum1Sq - pow(sum1, 2) / n) * sqrt(sum2Sq - pow(sum2, 2) / n)

      if(den == 0) return 0

      //return
      num / den
    }

    def topMatches(prefs:Map[String, Map[String,Double]], person: String, n: Int = 5,
      similarity:(Map[String, Map[String,Double]], String, String) => Double = Recommendation.sim_pearson
      ): List[(Double, String)] = {
        prefs.filter(other => other._1 != person).map(other =>
            (similarity(prefs, person, other._1), other._1)).toList.sort(_._1 > _._1).take(n)
      }

    def getRecommendations(prefs: Map[String, Map[String, Double]], person: String, similarity: (Map[String, Map[String, Double]], String, String) => Double = Recommendation.sim_pearson): List[(Double, String)] = {
      val other_sims = prefs.filter(other => other._1 != person).map(other =>
          (other, similarity(prefs, person, other._1)))

      import scala.collection.mutable
      //initialize
      val totals = mutable.Map.empty[String, Double]
      val simSums = mutable.Map.empty[String, Double]

      for((other, sim) <- other_sims;if(0 < sim)){
        for(item <- other._2; if(!prefs(person).contains(item._1) || prefs(person)(item._1) == 0))
             yield {
               //weighted score
               //initialize
               if(!totals.isDefinedAt(item._1)) totals(item._1) = 0
               // socore * similarity
               totals(item._1) += item._2 * sim

               if(!simSums.isDefinedAt(item._1)) simSums(item._1) = 0

               simSums(item._1) += sim
             }
      }
      totals.map(total => (total._2 / simSums(total._1), total._1)).toList.sort(_._1 > _._1)
    }
  }
}
