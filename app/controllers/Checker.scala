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
import es.weso.rdf.nodes.IRI
import es.weso.rdf.jena._
import es.weso.monads.{Passed, Failure => SchemaFailure, Result => SchemaResult}
import es.weso.utils._
import es.weso.utils.TryUtils._
import es.weso.utils.RDFUtils._
import es.weso.utils.IOUtils._
import java.net.URL
import java.io.File
import javax.inject.{Inject, Singleton}

import scala.util.{Success => TrySuccess}
import DataOptions._
import SchemaOptions._
import es.weso.htmlschema.HTMLSchemas
import play.api.i18n.{I18nSupport, MessagesApi}


@Singleton
class Checker @Inject()(implicit val webJarAssets: WebJarAssets,
                        val messagesApi: MessagesApi) extends Controller with I18nSupport {

  import Multipart._


  def data(data: String, dataFormat: String) = {
    Action {
      Ok(views.html.about("data")(ValidationForm()))
    }
    // throw new Exception("data: call to ValidatorSchemaData")
/*    ValidatorSchemaData.validate_get(data,
        Some(dataFormat),
        DEFAULT_SHOW_DATA,
        false,
        None,
        None,
        "",
        None,
        None,
        "All",
        DEFAULT_CUT,
        DEFAULT_ShowSchema) */
  }
  
/*  def schema(schema: String, schemaFormat: String, schemaName: String) = {
    println("Schema name: " + schemaName)
    Action.async {  
     schema_Future(schema, schemaFormat, schemaName).map(result => {
               result match {
                case TrySuccess(str) => {
                  val schemaInput = SchemaInput(schema,schemaFormat,schemaName)
                  val vf = ValidationForm.fromSchemaConversion(schemaInput)
                  Ok(views.html.check_schema(vf,str))
                }
                case Failure(e) => 
                  BadRequest(views.html.errorPage(e.getMessage))
              }
          })
    }
  } */

  def schema(schema: String, schemaFormat: String, schemaName: String) = Action {
    println(s"Schema2 name: $schemaName. String: $schema")
    HTMLSchemas.fromString(schema,schemaFormat,schemaName,None) match {
      case Success(schemaValue) => {
        println(s"SchemaValue: $schemaValue")
        schemaValue.serialize("SHEXC") match {
          case Success(result) => {
            val schemaInput = SchemaInput(schema, schemaFormat, schemaName)
            val vf = ValidationForm.fromSchemaConversion(schemaInput)
            Ok(views.html.check_schema(vf,result))
          }
          case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
        }
      }
      case Failure(e) => {
        BadRequest(views.html.errorPage(e.getMessage))
      }
    }
  }

  def schema_Future(
          schema: String
        , schemaFormat: String
        , schemaName: String
        ) : Future[Try[String]]= {
    println(s"Checking schema: $schemaName, format: $schemaFormat, schema: $schema")
    val schemaInput = SchemaInput(schema,schemaFormat,schemaName)
    val output = schemaInput.convertSchema(schemaFormat)
    println(s"Output: $output")
    Future(output)
  }   
  
  
  def schema_post = Action { request => {
     val r = for ( mf <- getMultipartForm(request)
                 ; schemaInput <- parseSchemaInput(mf)
//                 ; str_schema <- schemaInput.getSchemaStr
                 ; outputStr <- schemaInput.convertSchema(schemaInput.inputFormat)
                 ) yield (schemaInput, outputStr)
     
      r match {
       case TrySuccess((schemaInput, result)) => {
         val vf = ValidationForm.fromSchemaConversion(schemaInput)
         Ok(views.html.check_schema(vf,result))
       }
       case Failure(e) => BadRequest(views.html.errorPage(e.getMessage)) 
      }
    } 
  }
 
  def data_post = Action { request => {
     val r = for ( mf <- getMultipartForm(request)
                 ; dataInput <- parseDataInput(mf)
                 ; schemaName <- parseSchemaName(mf)
                 ; rdfs <- parseBoolean(mf,"rdfs")
                 // ; str_data <- dataInput.getDataStr
                 ; outputStr <- dataInput.convertData(dataInput.dataFormat)
                 ) yield (dataInput, outputStr, schemaName, rdfs)
     
      r match {
       case TrySuccess((dataInput, result, schemaName, rdfs)) => {
         val vf = ValidationForm.fromDataConversion(result, dataInput.dataFormat, schemaName, rdfs)
         Ok(views.html.check_data(vf,result))
       }
       case Failure(e) => BadRequest(views.html.errorPage(e.getMessage)) 
      }
    } 
  }

}

