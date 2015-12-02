package es.weso.shacl

import scala.util._

case class SchemaLanguage(format: String, vocabulary: SchemaVocabulary) {
 override def toString = s"$format/$vocabulary"
}

object SchemaLanguages {
  
  lazy val availableLanguages : Seq[SchemaLanguage] = {
    val rdfFormats = SchemaFormats.rdfFormats.map(_.toString)
    val schemaVocabularies = SchemaVocabulary.availableVocabularies
    val rdfCombinations = rdfFormats.map(f => schemaVocabularies.map(v => SchemaLanguage(f,v))).flatten
    
    // TODO: Add ShexC
    rdfCombinations
  }
    
    
  val availables : List[(String,String)]= 
    availableLanguages.map(l => (l.format,l.vocabulary.toString))

  lazy val default = ShExcala
  
  lazy val defaultSchemaProcessor = default.name
  
  def lookup(key:String): Try[SchemaProcessor] = {
    availableLanguages.find(_.name == key) match {
      case None => Failure(new Exception (s"Key $key not found in available schema versions: $availableNames"))
      case Some(x) => Success(x)
    }
  }
  
  def get(key:String): SchemaProcessor = {
    availableProcessors.find(_.name == key).getOrElse(default)
  }
}