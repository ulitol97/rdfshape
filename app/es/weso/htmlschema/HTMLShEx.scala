package es.weso.htmlschema

import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.schema.{Result, ShEx}
import es.weso.shex.{Schema => ShExSchema, ShapeLabel => ShExLabel}

import scala.util._

case class HTMLShEx(schema: ShEx) extends HTMLSchema {
  override def name = schema.name

  override def formats = schema.formats
  
  override def toHTML(format:String): String = {
    "<pre>" + schema.serialize(format) + "</pre>"
  }
  
  override def validate(rdf: RDFReader) : Result = {
    schema.validate(rdf)
  }
  
  override def validateNodeShape(node: IRI, shape: String, rdf: RDFReader) : Result = {
    ???
   /* val matcher = ShExMatcher(schema,rdf)
    val pm = schema.pm
    val maybeLabel = pm.qname(shape).map(lbl => ShExLabel.mkLabel(lbl)).flatten
    maybeLabel match {
      case Some(lbl) => {
        val r = matcher.match_node_label(node)(lbl)
        validationResult2Result(r)
      }
      case None => Result.errStr(s"Cannot make label from shape $shape")
    } */
  }
  
  override def validateNodeAllShapes(node: IRI, rdf: RDFReader) : Result = {
    ???
  }
  
  override def validateAllNodesAllShapes(rdf: RDFReader) : Result = {
    ???
  }
  
  def hasSolutions(rs: Seq[Map[RDFNode,(Seq[ShExLabel],Seq[ShExLabel])]]): Boolean = {
    if (rs.size == 0) false
    else if (rs.size == 1 && rs.head.isEmpty) false
    else true
  }
  
/*  def validationResult2Result(result: ValidationResult[RDFNode,ShExLabel,Throwable]): Result = {
    val isValid = result.isValid
    val (msg,solutions,errors): (String,Seq[HTMLSolution],Seq[HTMLErrorInfo]) = {
      result.extract match {
        case Success(rs) => {
          if (hasSolutions(rs))
            ("Solutions found", rs.map(cnvSol(_)), Seq())
          else
            ("No Results", Seq(), Seq(HTMLErrorInfo("No results")))
        }
        case Failure(e) => (s"Error $e.getMessage", Seq(), Seq(HTMLErrorInfo(e.getMessage)))
      }
    }
    val r = Result(isValid, msg, solutions, errors)
    println(s"validationresult2Result result: $result, r: $r")
    r
  }
  
  def cnvSol(rs: Map[RDFNode, (Seq[ShExLabel], Seq[ShExLabel])]): HTMLSolution = {
    HTMLSolution(rs.mapValues(cnvShapes(_)))
  }
  
  def cnvShapes(pair: (Seq[ShExLabel], Seq[ShExLabel])): HTMLInfoNode = {
    val (shapes,noShapes) = pair
    HTMLInfoNode(shapes.map(mkLabelExplanation(_)),noShapes.map(mkLabelExplanation(_)))
  }
  
  def mkLabelExplanation(lbl: ShExLabel): (ShapeLabel,Explanation) = {
    (ShapeLabel(lbl.toString),Explanation(""))
  } */
  
  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[HTMLSchema] = {
    println(s"HTMLShEx fromString: $cs $format")
    val result = ShEx.fromString(cs,format,base).map(HTMLShEx(_)) match {
      case Success(schema) => {
        println(s"Schema obtained: $schema")
        Success(schema)
      }
      case Failure(e) => {
        println(s"fromString error : $e")
        Failure(e)
      }
    }
    println(s"HTMLShEx fromString end...")
    result
  }
  
  override def fromRDF(rdf: RDFReader): Try[HTMLSchema] = {
    throw new Exception("HTMLShEx: not implemented fromRDF")
    // schema.fromRDF(rdf, name)
  }
  
  override def serialize(format: String): Try[String] = {
    schema.serialize(format)
  }
  
  override def empty: HTMLSchema = HTMLShEx(ShEx.empty)

  override def shapes: List[String] = {
    schema.shapes
  }
  
  override def pm: PrefixMap = schema.pm

}

object HTMLShEx {
  def empty: HTMLShEx = HTMLShEx(schema = ShEx.empty)
  
  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[HTMLShEx] = {
    ShEx.fromString(cs,format,base).map(HTMLShEx(_))
  }
  
}
