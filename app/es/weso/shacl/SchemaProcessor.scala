package es.weso.shacl

import scala.util._

case class SchemaProcessor(name: String) {
 override def toString = name
}

object SchemaProcessor {
  
  val publicAvailableProcessors : List[SchemaProcessor] = {
    List(ShExcala, SHACL_FPWD)
  }

  val availableProcessors : List[SchemaProcessor] = 
    List(SHACL, ShExcala, SHACL_FPWD)
    
  val processorNames : List[String]= 
    availableProcessors.map(_.name)

  lazy val default = ShExcala
  
  lazy val defaultSchemaProcessor = default.name
  
  def lookup(key:String): Try[SchemaProcessor] = {
    val keyUpper = key.toUpperCase
    availableProcessors.find(_.name == keyUpper) match {
      case None => Failure(new Exception (s"Key $key not found in available schema versions: $processorNames"))
      case Some(x) => Success(x)
    }
  }
  
  
// This version is kept for compatibility reasons
// It behaves as ShEx although it is called SHACL_0.1
object SHACL extends SchemaProcessor("SHACL_0.1")

// First version of ShExcala
object ShExcala extends SchemaProcessor("SHEXCALA_0.1") 

// SHACL as published in FPWD
// This is based on TopQuadrant implementation
object SHACL_FPWD extends SchemaProcessor("SHACL_FPWD")

}