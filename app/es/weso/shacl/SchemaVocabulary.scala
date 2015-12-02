package es.weso.shacl

import scala.util._

sealed trait SchemaVocabulary {
 
 def name: String
 
 override def toString = s"$name"
}
object SchemaVocabulary {

case object SHACL extends SchemaVocabulary {
 override def name = "SHACL"
}

case object ShEx extends SchemaVocabulary {
 override def versionName = "ShEx"
}
  
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