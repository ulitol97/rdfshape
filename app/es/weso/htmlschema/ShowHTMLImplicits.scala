package es.weso.htmlschema

import es.weso.rdf.PrefixMap
import es.weso.utils.ShowHTML
import es.weso.utils.ShowHTML._
import es.weso.rdf.nodes.RDFNode
import es.weso.utils.PrefixMapUtils._
import es.weso.schema.{Result,
              Explanation,
              InfoNode,
              Solution,
              ErrorInfo,
 ShapeLabel}

import scala.xml.Utility._

case class ShowHTMLImplicits(schema: HTMLSchema) {

  implicit val shapeLabelHTML = new ShowHTML[ShapeLabel] {
    override def showHTML(lbl: ShapeLabel): String = {
      "<span class=\"shape\">" + lbl.str + "</span>"
    }
  }

  implicit val explanationHTML = new ShowHTML[Explanation] {
    override def showHTML(e: Explanation): String =
      "<span class=\"explanation\">" + code(e.str) + "</span>"
  }
  implicit val infoNodeHTML = new ShowHTML[InfoNode] {
    override def showHTML(node: InfoNode): String =  {
      val sb = new StringBuilder
      sb ++= "<ul class=\"positiveShapes\">"
      for ((s, e) <- node.hasShapes) {
        sb ++= ("<li><span class=\"shape\"" + ShowHTML[ShapeLabel].showHTML(s) + "</span>" +
          "<span class=\"explanation\">" + ShowHTML[Explanation].showHTML(e) + "</span></li>")
      }
      sb.append("</ul>")
      sb.append("<ul class=\"negativeShapes\">")
      for ((s, e) <- node.hasNoShapes) {
        sb ++= ("<li><span class=\"shape\"" + ShowHTML[ShapeLabel].showHTML(s) + "</span>" +
          "<span class=\"explanation\">" + ShowHTML[Explanation].showHTML(e) + "</span></li>")
      }
      sb.append("</ul>")
      sb.toString
    }
  }

  implicit val errorHTML = new ShowHTML[ErrorInfo] {
    override def showHTML(e: ErrorInfo): String = {
      "<span class='error'>" + code(e.str) + "</span>"
   }
  }

  implicit val solutionHTML = new ShowHTML[Solution] {
    override def showHTML(s: Solution): String = {
      val sb = new StringBuilder
      sb ++= "<h2>Solution</h2>"
      sb ++= """<table class=\"result\"><tr><th>Node</th><th>Shapes</th></tr>"""
      for (pair <- s.map.toSeq) {
        val (node, info) = pair
        sb ++= ("<tr><td class=\"node\">" + node2Html(node, s.pm) + "</td>" +
          "<td class=\"shapes\">" + ShowHTML[InfoNode].showHTML(info) + "</td></tr>")
      }
      sb ++= "</table>"
      sb.toString
    }
  }

  implicit val resultHTML = new ShowHTML[Result] {
    override def showHTML(r: Result): String = {
      val sb = new StringBuilder
      if (r.isValid) {
        if (r.noSolutions(r.solutions)) {
          sb ++= "<h2>No solutions found</h2"
        } else {
          // TODO: Refactor to remove concept of cut
          for ((solution, n) <- r.solutions zip (1 to r.cut)) {
            sb ++= "<h2 class='result'>Result " + r.printNumber(n, r.cut) + "</h2>"
            // sb ++= schema.htmlBeforeSolutions
            sb ++= ShowHTML[Solution].showHTML(solution)
            // sb ++= schema.htmlAfterSolutions
          }
        }
      } else {
        val numErrors = r.errors.size
        val errorStr = if (numErrors == 1) "Error" else "Errors"
        sb ++= "<div class=\"errors\">"
        sb ++= s"<p class='errorMsg'>${numErrors} $errorStr found</p>"
        sb ++= "<table class='display' id='results' >"
        sb ++= schema.htmlBeforeErrors
        for (error <- r.errors) {
          sb ++= ShowHTML[ErrorInfo].showHTML(error)
        }
        sb ++= schema.htmlAfterErrors
        sb ++= "</table>"
      }
      sb.toString
    }

  }


  def node2Html(node: RDFNode, pm:PrefixMap): String = {
    if (node.isIRI) code(showIRI(node.toIRI)(pm))
    else code(node.toString)
  }

  def code(str: String): String = {
    s"<code>${escape(str)}</code>"
  }

}