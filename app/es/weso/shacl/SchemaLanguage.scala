package es.weso.shacl

import scala.util._

case class SchemaLanguage(
    format: String, 
    vocabulary: SchemaVocabulary) {
 override def toString = s"$format/$vocabulary"
}

object SchemaLanguages {

  lazy val shExc : SchemaLanguage = 
    SchemaLanguage(SchemaFormats.shexc.name, SchemaVocabulary.ShEx)
  
  lazy val availableLanguages : Seq[SchemaLanguage] = {
    val rdfFormats = SchemaFormats.rdfFormats.map(_.toString)
    val schemaVocabularies = SchemaVocabulary.availableVocabularies
    val rdfCombinations = rdfFormats.map(f => schemaVocabularies.map(v => SchemaLanguage(f,v))).flatten
    
    shExc +: rdfCombinations 
  }
    
    
  val availables : Seq[(String,String)]= 
    availableLanguages.map(l => (l.format,l.vocabulary.toString))

  lazy val default : SchemaLanguage = shExc
  
  def lookup(format:String, vocab: String): Try[SchemaLanguage] = {
    availableLanguages.find(l => l.format == format && l.vocabulary == vocab) match {
      case None => Failure(new Exception (s"$format/$vocab not found in available schema languages: $availables"))
      case Some(x) => Success(x)
    }
  }
  
  def get(format:String, vocab: String): SchemaLanguage = {
    availableLanguages.find(l => l.format == format && l.vocabulary == vocab).getOrElse(default)
  }
}