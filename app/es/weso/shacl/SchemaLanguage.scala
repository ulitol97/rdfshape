package es.weso.shacl

import scala.util._
import es.weso.utils.RDFUtils

case class SchemaLanguage(
    format: String, 
    vocabulary: SchemaVocabulary) {
 override def toString = s"$format/$vocabulary"
}

object SchemaLanguage {

  object shExc extends SchemaLanguage(SchemaFormats.shexc.name, SchemaVocabulary.ShEx)
  
  lazy val availableLanguages : Seq[SchemaLanguage] = {
    val rdfFormats = SchemaFormats.rdfFormats.map(_.toString)
    val schemaVocabularies = SchemaVocabulary.availableVocabularies
    val rdfCombinations = rdfFormats.map(f => schemaVocabularies.map(v => SchemaLanguage(f,v))).flatten
    shExc +: rdfCombinations 
  }
    
  val availableFormats: Seq[String] = {
    availableLanguages.map(_.format).distinct
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
  
 def lookupOption(maybeFormat: Option[String], maybeVocabulary: Option[String]): Try[SchemaLanguage] = {
    maybeFormat match {
      case Some(format) => 
        maybeVocabulary match {
          case Some(vocab) => lookup(format,vocab)
          case None => lookupOnlyFormat(format)
        }
      case None => Success(default)
    }
  }
 
 def lookupOnlyFormat(format: String): Try[SchemaLanguage] = {
   format match {
     case "SHEXC" => Success(shExc)
     case _ => 
       if (RDFUtils.isAvailableFormat(format)) 
         Success(SchemaLanguage(format, SchemaVocabulary.default))
       else
         Failure(throw new Exception(s"Cannot find a schema language for format $format"))
   }
 }

  
/*  def get(format:String, vocab: String): SchemaLanguage = {
    availableLanguages.find(l => l.format == format && l.vocabulary == vocab).getOrElse(default)
  } */
}