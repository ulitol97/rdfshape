package es.weso.shacl

import scala.util._

case class SchemaVocabulary(name: String) {
 override def toString = s"$name"
}

object SchemaVocabulary {

object SHACL extends SchemaVocabulary("SHACL")
object ShEx extends SchemaVocabulary("ShEx")
  
val availableVocabularies : List[SchemaVocabulary] = 
    List(SHACL, ShEx)
    
val availableNames : List[String] = 
   availableVocabularies.map(_.name)

lazy val default = ShEx
  
lazy val defaultSchemaVocabulary = default.name
  
def lookup(key:String): Try[SchemaVocabulary] = {
    availableVocabularies.find(_.name == key) match {
      case None => Failure(new Exception (s"lookup vocabulary: $key not found in: $availableNames"))
      case Some(x) => Success(x)
    }
  }
  
  def get(key:String): SchemaVocabulary = {
    availableVocabularies.find(_.name == key).getOrElse(default)
  }
}