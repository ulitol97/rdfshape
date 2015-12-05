package es.weso.utils

import java.net.URL
import play.Logger
import java.io.File
import scala.util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.shacl.DataFormats

object RDFUtils {

  lazy val defaultDataFormat: String = "TURTLE"

  def parseRDF(str: String, format: String): Try[RDFReader] = {
    RDFAsJenaModel.fromChars(str,format) 
  }

  def getFormat(maybeFormat: Option[String]): Try[String] = {
    val format = maybeFormat.getOrElse(defaultDataFormat)
    if (isAvailableFormat(format)) 
       Try(format)
    else  
       Failure(throw new Exception("Unsupported RDF format " + format))
  }
  
  def isAvailableFormat(format: String): Boolean = {
    DataFormats.available(format)
  }
  
  def availableFormats: Seq[String] = {
    DataFormats.formats
  }
}