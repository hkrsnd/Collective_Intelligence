package org.pii5656.collective.searchengine
import java.io.StringReader
import nu.validator.htmlparser.{sax,common}
import sax.HtmlParser
import common.XmlViolationPolicy
import org.xml.sax.InputSource
import scala.io.{Source, BufferedSource}
import scala.slick.driver.SQLiteDriver.api._
import slick.lifted.TableQuery
import slick.jdbc.JdbcBackend.Database;
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.sys
import scala.xml.{Node,XML}
import scala.xml.parsing.NoBindingFactoryAdapter
import org.sqlite.JDBC
import org.pii5656.collective.searchengine.DB._

object Crawler {
  val ignorewords = Set("the", "of", "to", "and", "a", "in", "is", "it")
  def toNode(str: String): Node = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)
    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(str)))
    saxer.rootElem
  }
  // return long string list contains all text of the page
  // ALL TEXT OF THE PAGE
  def getTextOnly(feeds: Node): String = {
    val resulttext = for (feed <- feeds) yield {
      feed.text + "\n"
    }
    (resulttext.toList :\ "")(_ + _)
  }

  def separateWords(text: String): List[String] = {
    text.split("[^a-zA-Z]").toList.filter(x => x != "")
  }

  def getEntryId(table: String, field: String, value: String, createNew: Boolean = true): Int = {
    def insertNewWord = {
      val f2 = for {
        x <- db.run(sqlu" insert into #$table (#$field) values ('#$value')")
        last_rowid <- db.run(sql"select last_insert_rowid()".as[Int].head)
      } yield last_rowid
      f2.onSuccess { case last_rowid =>
        last_rowid
      }
      f2
    }
    val f1 = db.run(sql"select rowid from #$table where #$field = '#$value'".as[Int].headOption)
    f1.onSuccess { 
      case Some(rowid) if rowid == null => None
      case Some(rowid) => rowid 
    }
    Await.result(f1, Duration.Inf).getOrElse(Await.result(insertNewWord, Duration.Inf))
  }

  def isIndexed(url: String): Boolean = {
    var bool = false
    val f = for {
     u <- db.run(DB.urllist.filter(_.url === url).result.head)
     v <- db.run(DB.wordlocation.filter(_.urlid === u._1).result.headOption)
    } yield v
    f.onSuccess { case ff =>
      if (ff != None)
        bool = true
    }
    bool
  }

  def addToIndex(url: String, feeds: Node): Int = {
    if (this.isIndexed(url)) return 0
  
    println("Indexing " + url)
    // get words
    val text = this.getTextOnly(feeds)
    val words = this.separateWords(text)

    // get URL id
    val urlid = this.getEntryId("urllist", "url", url)
    println("urlid: " + urlid.toString)

    for (word <- words.zipWithIndex.filter(x => !ignorewords.contains(x._1))) {
      // wordid 存在すれば既存のID　存在しなければインサートした後のID
      val wordid = this.getEntryId("wordlist", "word", word._1)
      println("wordid: " + wordid.toString)
      db.run(wordlocation.map(w => (w.urlid, w.wordid, w.location)) += (urlid, wordid, word._2))
    }
    return 0
  }

  def addLinkRef(urlFrom: String, urlTo: String, linkText: String): Unit = {
    val words = this.separateWords(linkText).filter(x => !ignorewords.contains(x))
    val fromid = this.getEntryId("urllist", "url", urlFrom)
    val toid = this.getEntryId("urllist", "url", urlTo)
    if (fromid == toid) return {}

    val f1 = for {
      cur <- db.run(sqlu"insert into link(fromid, toid) values (#$fromid, #$toid)")
      linkid <- db.run(sql"select last_insert_rowid()".as[Int].head)
    } yield linkid
    f1.onSuccess {
      case s => {}
    }
    val linkid = Await.result(f1, Duration.Inf)
    for (word <- words) {
      val wordid = this.getEntryId("wordlist", "word", word)
      db.run(sqlu"insert into linkwords(linkid, wordid) values (#$linkid, #$wordid)")
    }
  }

  def crawl(pages: List[String], depth: Int): Unit = {
    if (depth == 0) sys.exit(0)
    // initialize variables
    var source: BufferedSource = null
    var feeds: scala.xml.Node = null
    var newpages = Set("")
    // try to get contents from each URL
    for (page <- pages) {
      try{
        // get URL contents
        source = Source.fromURL(page)
        feeds = toNode(source.mkString)
        } catch {
          case e: Exception => println("Could not open " + page)
          case e: org.xml.sax.SAXParseException => println("Could not open " + page)
        }
    
      if(source != null && feeds != null){
        //source.close
        addToIndex(page, feeds)
        // search <a href="http://..."> in URL
        // get "http://..."
        val links = (feeds \\ "@href").map(x =>
            x.mkString).filter(x => x.startsWith("http"))
        for (link <- links) {
          val url = link.toString.split("#")(0) // remove anchor
          if (!isIndexed(url))
            newpages += url
          val linkText = getTextOnly(feeds)
          addLinkRef(page, url, linkText)
        }
      }
    }
    crawl(newpages.toList, depth-1)
  }
}

object Searcher {
  def getMatchRows(query: String): (List[Int], List[Int]) = {
    /*
    def buildQuery(
      fieldlist: String, tablelist: String, clauselist: String, wordids: List[Int], tablenumber: Int
    ): String = {
        val words = query.split(" ")
        for (word <- words) {
          val wordrow = db.run(sql"select rowid from wordlist where word = #$word".as[Int].headOption)
          wordrow.onSuccess {
            case Some(id) if (id != null) => 
              val wordid = wordrow.get
              wordids += wordid
          }
          tablenumber match {
            case num > 0 =>

          }
        }
    }*/
   var fieldlist = "w0.urlid"
   var tablelist = ""
   var clauselist = ""
   var wordids: List[Int] = List()

   val words = query.split(" ")
   var tablenumber = 0

   for (word <- words) {
     val wordrow = db.run(sql"select rowid from wordlist where word = #$word".as[Int].headOption)
     wordrow.onSuccess {
       case Some(wordrow) if wordrow != null =>
         val wordid = wordrow
         wordids += wordid
         if (tablenumber > 0) {
           tablelist += ","
           clauselist += " and"
           clauselist += s"w${tablenumber-1}.urlid = w${tablenumber}.urlid and "
         }
         fieldlist += s",w${tablenumber}.location"
         tablelist += s"wordlocation w${tablenumber}"
         clauselist += s"w${tablenumber}.wordid = ${wordid.toString}"
         tablenumber += 1
     }
   }
   val cur = db.run(sqlu"select #$fieldlist from #$tablelist where #$clauselist")
   cur.onSuccess {
     case c =>
       c
   }
   (Await.result(cur, Duration.Inf), wordids)
  }
}

object DB {
  //val db = Database.forURL("jdbc:sqlite:searchindex.db", driver = "org.sqlite.JDBC")
  val db = Database.forConfig("db")
  class Link(tag: Tag) extends Table[(Int, Int, Int)](tag, "link") {
    def id = column[Int]("rowid", O.PrimaryKey)
    def fromid = column[Int]("fromid")
    def toid = column[Int]("toid")
    def * = (id, fromid, toid)
  }
  val links = TableQuery[Link]

  class LinkWords(tag: Tag) extends Table[(Int, Int)](tag, "linkwords") {
    def wordid = column[Int]("wordid")
    def linkid = column[Int]("linkid")
    def * = (wordid, linkid)
  }
  val linkwords = TableQuery[LinkWords]

  class WordList(tag: Tag) extends Table[(Int, String)](tag, "wordlist") {
    def rowid = column[Int]("rowid", O.PrimaryKey)
    def word = column[String]("word")
    def * = (rowid, word)
  }
  val wordlist = TableQuery[WordList]

  class UrlList(tag: Tag) extends Table[(Int, String)](tag, "urllist") {
    def rowid = column[Int]("rowid", O.PrimaryKey)
    def url = column[String]("url")
    def * = (rowid, url)
  }
  val urllist = TableQuery[UrlList]

  class WordLocation(tag: Tag) extends Table[(Int, Int, Int)](tag, "wordlocation") {
    def urlid = column[Int]("urlid", O.PrimaryKey)
    def wordid = column[Int]("wordid")
    def location = column[Int]("location")
    def * = (urlid, wordid, location)
  }
  val wordlocation = TableQuery[WordLocation]

  def createIndexTables(db: Database): Future[Int] = {
    db.run(sqlu"""create table wordlist(word)""")
    db.run(sqlu"""create table linkwords(wordid, linkid)""")
    db.run(sqlu"""create table urllist(url)""")
    db.run(sqlu"""create table wordlocation(urlid, wordid, location)""")
    db.run(sqlu"""create table link(fromid integer, toid integer)""")
    db.run(sqlu"""create index wordidx on wordlist(word)""")
    db.run(sqlu"""create index urlidx on urllist(url)""")
    db.run(sqlu"""create index wordurlidx on wordlocation(wordid)""")
    db.run(sqlu"""create index urltoidx on link(toid)""")
    db.run(sqlu"""create index urlfromid on link(fromid)""")
  }
}
