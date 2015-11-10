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
import ExecutionContext.Implicits.global
import scala.sys
import scala.xml.{Node,XML}
import scala.xml.parsing.NoBindingFactoryAdapter

object Crawler {
  def toNode(str: String): Node = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)
    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(str)))
    saxer.rootElem
  }
  def getEntryId(table: String, field: String, value: String, createNew: Boolean = true): Int = {
    0
  }
  def getTextOnly(url: String): String = url
  def addToIndex(page: String): Unit = println("Indexing " + page)
  def addLinkRef(page: String, url: String, linkText: String): Unit = {}
  def isIndexed(page: String): Boolean = false

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
        addToIndex(page)
        // search <a href="http://..."> in URL
        // get "http://..."
        val links = (feeds \\ "@href").map(x =>
            x.mkString).filter(x => x.startsWith("http"))
        for (link <- links) {
          val url = link.split("#")(0) // remove anchor
          if (!isIndexed(url))
            newpages += url
          val linkText = getTextOnly(url)
          addLinkRef(page, url, linkText)
        }
        // source.close
        }
      }
      crawl(newpages.toList, depth-1)
  }
  object DB {
    val db = Database.forURL("jdbc:sqlite:searchindex", driver = "org.sqlite.JDBC")
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
      def rowid = column[Int]("rowid")
      def word = column[String]("word")
      def * = (rowid, word)
    }
    val wordlist = TableQuery[WordList]
    class UrlList(tag: Tag) extends Table[(Int, String)](tag, "urllist") {
      def rowid = column[Int]("rowid")
      def url = column[String]("url")
      def * = (rowid, url)
    }
    val urllist = TableQuery[UrlList]
    class WordLocation(tag: Tag) extends Table[(Int, Int, Int)](tag, "wordlocation") {
      def urlid = column[Int]("urlid")
      def wordid = column[Int]("wordid")
      def location = column[Int]("location")
      def * = (urlid, wordid, location)
    }
    val wordlocation = TableQuery[WordLocation]

    def createIndexTables(): Unit = {

    }
  }
