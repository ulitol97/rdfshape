package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import util._

abstract class HTMLSchema extends Schema {

 override def empty: HTMLSchema

 override def validate(rdf: RDFReader): Result

 override def validateNodeShape(node: IRI, label: String, rdf: RDFReader): Result

 override def validateNodeAllShapes(node: IRI, rdf: RDFReader): Result

 override def validateAllNodesAllShapes(rdf: RDFReader): Result

 override def defaultFormat: String = formats.head

 def fromString(cs: CharSequence, format: String, base: Option[String]): Try[HTMLSchema]

 def fromRDF(rdf: RDFReader): Try[HTMLSchema]

 def toHTML(format: String): String
 /**
  * String to add to HTML conversion of validating solution
  */
 def htmlBeforeErrors: String = ""

 def htmlAfterErrors: String = ""

 /**
  * String to add to HTML conversion of validation errors
  */
 def htmlBeforeSolutions: String = ""

 def htmlAfterSolutions: String = ""
}
