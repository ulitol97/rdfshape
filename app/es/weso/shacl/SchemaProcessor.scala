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
    
  val availableNames : List[String]= 
    availableProcessors.map(_.name)

  lazy val default = ShExcala
  
  lazy val defaultSchemaProcessor = default.name
  
  def lookup(key:String): Try[SchemaProcessor] = {
    availableProcessors.find(_.name == key) match {
      case None => Failure(new Exception (s"Key $key not found in available schema versions: $availableNames"))
      case Some(x) => Success(x)
    }
  }
  
/*  def get(key:String): SchemaProcessor = {
    availableProcessors.find(_.name == key).getOrElse(default)
  } */
  
  def lookupOption(key: Option[String]): Try[SchemaProcessor] = {
    if (key.isDefined) {
      lookup(key.get)
    } else {
      Success(default)
    }
  }
  
// This version is kept for compatibility reasons
// It behaves as ShEx although it is called SHACL_0.1
object SHACL extends SchemaProcessor("SHACL_0.1")

// This is ShEx based on ShExcala
object ShExcala extends SchemaProcessor("ShExcala") 

// SHACL as published in FPWD
// This is based on TopQuadrant implementation
object SHACL_FPWD extends SchemaProcessor("SHACL_FPWD")

}