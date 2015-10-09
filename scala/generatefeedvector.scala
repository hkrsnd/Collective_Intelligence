
import scala.io.Source
import scala.xml.{XML, NodeSeq}
//import scala.xml.parsing.XhtmlParser
import scala.collection.mutable

package org.pii.collective.cluster{
  object GenerateFeedVector {
    def getwordcounts(url: String): (String, Map[String, Int]) = {
      // read RSS feed
      val source = Source.fromURL(url)
      val feeds = XML.loadString(source.getLines.mkString)
      source.close
  
      // \\ is deep search method of XML
      // text method removes XML tags
      // ex. <a>Sounds good</a>.text ==> String = Sounds good
      val title = (feeds \\ "title")(0).text
  
      // RSS contents
      val summary = feeds \\ "summary"
      val descriptions = if(0 < summary.size){
        summary
      } else {
        feeds \\ "description"
      }
      //split words
      def getwords(html: String): List[String] = {
        "<[^>]+>".r.replaceAllIn(html, "").split("[^A-Z^a-z]").map(_.toLowerCase).toList
      }
      val wc = mutable.Map.empty[String, Int]
      for(description <- descriptions){
        for(word <- getwords(title + ' ' + description.text)){
          if(!wc.isDefinedAt(word)) wc(word) = 0
          wc(word) += 1
        }
      }
      // return tuple
      (title, wc.toMap)
    }
  
    def parseFeed(datafile: String = "feedlist.txt"): (Int, Map[String, Int], Map[String, Map[String, Int]]) = {
      // Get URL list file
      val source = Source.fromFile(datafile)
      val feedlist = source.getLines().toList
      source.close
  
      // Initialize mutable variables for adding them up
      val apcount = mutable.Map.empty[String, Int]
      val wordcounts = mutable.Map.empty[String, Map[String, Int]]
  
      // Get contents from each URLs
      for(feedurl <- feedlist){
        try{
          // How often the word appears
          var (title, wc) = getwordcounts(feedurl)
          // Adding up
          wordcounts(title) = wc
          for(w <- wc){
            if(!apcount.isDefinedAt(w._1)) apcount(w._1) = 0
            if(1 < w._2) apcount(w._1) += 1
          }
          println("Sccess to parse feed" + feedurl)
        } catch {
          case e: Exception => println("Failed to parse feed" + feedurl)
        }
      }
      // Return tuple
      return (feedlist.size, apcount.toMap, wordcounts.toMap)
    }
  
    def filterData(apcount: Map[String, Int], feednum: Int): List[String] = {
      apcount.filter(x => 0.1 < x._2.toDouble / feednum && x._2.toDouble / feednum < 0.5).map(x =>
          x._1).toList
    }
  
    // Write the data to the file
    def writeData(wordlist: List[String], wordcounts: Map[String, Map[String, Int]], datafile: String = "blogdata.txt"){
      import java.io.PrintWriter
      // Write header
      val out = new PrintWriter(datafile)
      out.print("Blog")
      wordlist.map(x => out.print("\t" + x))
      out.print("\n")
      // Write data contents
      wordcounts.map{
        case(title, wc) => {
          out.print(title)
          for(word <- wordlist){
            if(wc.contains(word)) out.print("\t" + wc(word))
            else out.print("\t0")
          }
          out.print("\n")
        }
      }
      out.close()
    }
    def main(args: Array[String]){
      val (feedsize, apcount, wordcounts) = parseFeed("feedlist.txt")
  
      val wordlist = filterData(apcount, feedsize)
      writeData(wordlist, wordcounts)
    }
  }
}
