package org.pii5656.collective.searchengine
import java.io.StringReader
import nu.validator.htmlparser.{sax,common}
import sax.HtmlParser
import common.XmlViolationPolicy
import org.xml.sax.InputSource
import scala.io.{Source, BufferedSource}
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
  def addToIndex(page: String): Unit = println("Indexing " + page)
  def addLinkRef(page: String, url: String, linkText: String): Unit = {}
  def getTextOnly(url: String): String = url
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
}
