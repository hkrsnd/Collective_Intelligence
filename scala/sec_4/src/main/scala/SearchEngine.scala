package org.pii5656.collective.searchengine
import scala.io.Source
import scala.xml.XML

import java.io.StringReader
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter
import nu.validator.htmlparser.{sax,common}
import sax.HtmlParser
import common.XmlViolationPolicy
import org.xml.sax.InputSource

object Crawler {
  def toNode(str: String): Node = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)
    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(str)))
    saxer.rootElem
  }
  def addToIndex(page: String): Unit = println(page)
  def addLinkRef(page: String, link: String, linkText: String): Unit = {println(link)}
  def getTextOnly(link: String): String = link
  def isIndexed(page: String): Boolean = false
  def crawl(pages: List[String], depth: Int = 1): Unit = {
    for (i <- 1 to depth) {
      // initialize newpages
      var newpages = Set("")
      // for each URL
      for (page <- pages) {
        // get URL contents
        val source = Source.fromURL(page)
        //val feeds = XML.loadString(source.getLines.mkString)
        val feeds = toNode(source.mkString)
        //source.close
        addToIndex(page)
        // search <a> in URL
        val links = (feeds \\ "a").map(x => x.text)
        // val links = (feeds \\ "@href").text
        for (link <- links) {
          if (!isIndexed(link))
            newpages += link
          val linkText = getTextOnly(link)
          addLinkRef(page, link, linkText)
        }
        source.close
      }
    }
  }
}
