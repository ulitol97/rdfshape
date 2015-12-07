package controllers

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import java.io.ByteArrayInputStream
import org.apache.commons.io.FileUtils
import play.api._
import play.api.mvc._
import play.api.libs.Files._
import scala.util._
import es.weso.rdf._
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdf.jena._
import es.weso.monads.{Result => SchemaResult, Failure => SchemaFailure, Passed}
import es.weso.utils.TryUtils._
import es.weso.utils.RDFUtils._
import es.weso.utils.IOUtils._
import java.net.URL
import java.io.File
import scala.util._
import DataOptions._
import SchemaOptions._
import es.weso.shacl.SchemaFormat
import es.weso.shacl.SchemaProcessor
import es.weso.shacl.SchemaVocabulary
import play.api.Logger


trait Checker { this: Controller =>

  import Multipart._

  def data(data: String, dataFormat: String) = { 
    Validator.validate_get(data,
        dataFormat,
        DEFAULT_SHOW_DATA,
        "", 
        SchemaFormat.default.name, 
        SchemaVocabulary.default.name, 
        SchemaProcessor.default.name, 
        None,
        DEFAULT_CUT,
        DEFAULT_ShowSchema)
  }
  
  def schema(schema: String, 
      schemaFormat: String, 
      schemaVocabulary: String, 
      schemaProcessor: String) = {
    Action.async {  
     Logger.info(s"schemaFormat = $schemaFormat, vocab = $schemaVocabulary, processor=$schemaProcessor")
     schema_Future(schema,schemaFormat, schemaVocabulary, schemaProcessor).map(result => 
               result match {
                case Success((str,schemaInput)) => {
                  val vf = ValidationForm.fromSchemaConversion(schemaInput)
                  Ok(views.html.check_schema(vf,str))
                }
                case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
               }
          )
    }
  }
    

  
  def schema_Future(
          schema: String
        , schemaFormat: String
        , schemaVocabulary: String
        , schemaProcessor: String
        ) : Future[Try[(String,SchemaInput)]]= {
    val result = {
      for {
        schemaInput <- SchemaInput.build(schema,schemaFormat,schemaVocabulary,schemaProcessor)
        str <- schemaInput.convertSchema(schemaInput.schemaLanguage) 
      } yield (str,schemaInput)
    }
    Future(result)
  }   
  
  
  def schema_post = Action { request => {
     val r = for ( mf <- getMultipartForm(request)
                 ; schemaInput <- parseSchemaInput(mf)
                 ; str_schema <- schemaInput.getSchemaStr
                 ; outputStr <- schemaInput.convertSchema(schemaInput.schemaLanguage)
                 ) yield (schemaInput, outputStr)
     
      r match {
       case Success((schemaInput, result)) => {
         val vf = ValidationForm.fromSchemaConversion(schemaInput)
         Ok(views.html.check_schema(vf,result))
       }
       case Failure(e) => BadRequest(views.html.errorPage(e.getMessage)) 
      }
    } 
  }
 

}

object Checker extends Controller with Checker