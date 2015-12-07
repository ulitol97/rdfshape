package es.weso.utils

import java.net.URL
import play.Logger
import java.io.File
import scala.util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.shacl.DataFormat

object RDFUtils {

  lazy val defaultDataFormat: String = "TURTLE"

  def parseRDF(str: String, format: String): Try[RDFReader] = {
    RDFAsJenaModel.fromChars(str,format) 
  }
  
  def getFormat(format: String): Try[String] = {
    DataFormat.lookup(format).map(_.name)
  }
  
  def isAvailableFormat(format: String): Boolean = {
    DataFormat.available(format)
  }
  
  def availableFormats: Seq[String] = {
    DataFormat.formats.map(_.name)
  }
}