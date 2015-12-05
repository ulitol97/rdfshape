package controllers

import play.api._
import play.api.mvc._
import scala.concurrent._
import akka.actor._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import java.io.ByteArrayInputStream
import play.api._
import play.api.mvc._
import play.api.libs.Files._
import es.weso.shacl.Schema
import scala.util._
import es.weso.rdf._
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdf.jena._
import es.weso.monads.{Result => SchemaResult, Failure => SchemaFailure}
import es.weso.shacl.{Schema => ShExSchema, SchemaFormats}
import es.weso.utils.RDFUtils
import es.weso.utils.RDFUtils._
import es.weso.utils.TryUtils._
import es.weso.shacl.SchemaLanguage 
import java.net.URL
import java.io.File
import es.weso.utils.IOUtils._
import Multipart._
import play.api.libs.json._

trait SchemaConverter { this: Controller => 
  
 lazy val targetFormatKey = "targetFormat"
 lazy val targetVocabularyKey = "targetVocabulary"

 def converterSchemaFuture(
          schema: String
        , inputFormat: String
        , schemaVocabulary: String
        , schemaProcessor: String
        , targetFormat: String
        , targetVocabulary: String
    ) : Future[Try[(String,SchemaInput)]]= {
       Future(for {
         schemaInput <- SchemaInput.build(schema,inputFormat,schemaVocabulary,schemaProcessor)
         targetLanguage <- SchemaLanguage.lookup(targetFormat,targetVocabulary)
         converted <- schemaInput.convertSchema(targetLanguage) 
       } yield (converted,schemaInput))
  }
  
  
  def convert_schema_get(
          schema: String
        , inputFormat: String
        , schemaVocabulary: String
        , schemaProcessor: String
        , targetFormat: String
        , targetVocabulary: String
        ) = Action.async {  
        converterSchemaFuture(schema,inputFormat, schemaVocabulary, schemaProcessor,targetFormat, targetVocabulary).map(output => {
              output match {
                case Success((result,schemaInput)) => {
                  val vf = ValidationForm.fromSchemaConversion(schemaInput)
                  Ok(views.html.convert_schema(vf,targetFormat,targetVocabulary, result))
                }
                case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
              }
          })
  }

  def convert_schema_post = Action { request => {
     val r = for ( mf <- getMultipartForm(request)
                 ; schemaInput <- parseSchemaInput(mf)
                 ; str_schema <- schemaInput.getSchemaStr
                 ; targetFormat <- parseKey(mf, targetFormatKey)
                 ; targetVocabulary <- parseKey(mf, targetVocabularyKey)
                 ; targetLanguage <- SchemaLanguage.lookup(targetFormat,targetVocabulary)
                 ; outputStr <- schemaInput.convertSchema(targetLanguage)
                 ) yield (schemaInput, targetLanguage, outputStr)
     
      r match {
       case Success((schemaInput, targetLanguage,result)) => {
         Logger.info("Convert_schema_post: " + schemaInput)
         val vf = ValidationForm.fromSchemaConversion(schemaInput)
         Ok(views.html.convert_schema(vf,targetLanguage.format, targetLanguage.vocabulary.name,result))
       }
       case Failure(e) => {
        Logger.info("Exception raised: " + e.getMessage)
        BadRequest(views.html.errorPage(e.getMessage)) 
       } 
      }
    } 
  }

  def schemaFormats = Action {
    Ok(Json.toJson(SchemaFormats.toList))
  }
    
}

object SchemaConverter extends Controller with SchemaConverter
