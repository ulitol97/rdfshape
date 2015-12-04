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
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import es.weso.rdf._
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdf.jena._
import es.weso.monads.{Result => SchemaResult, Failure => SchemaFailure, Passed}
import es.weso.utils._
import es.weso.utils.TryUtils._
import es.weso.utils.RDFUtils._
import es.weso.utils.IOUtils._
import es.weso.shacl._
import java.net.URL
import java.io.File
import play.api.Logger

object Multipart {

  lazy val withIRIKey = "withIRI"
  lazy val iriKey = "iri"
  lazy val iriStem = "#iri"
  lazy val noIriStem = "#noIri"
  lazy val dataKey = "data"
  lazy val dataFormatKey = "data_format"
  lazy val dataUriKey = "data_uri"
  lazy val dataFileKey = "data_file"
  lazy val showDataKey = "showData"
  lazy val dataTextareaKey = "data_textarea"
  lazy val schemaKey = "schema"
  lazy val schemaFormatKey = "schema_format"
  lazy val schemaLanguageKey = "schema_language"
  lazy val schemaProcessorKey = "schema_processor"
  lazy val schemaVocabularyKey = "schema_vocabulary"
  lazy val schemaStem = "#schema"
  lazy val noSchemaStem = "#no_schema" 
  lazy val inputSchemaKey = "input-schema"
  lazy val schemaUriKey = "schema_uri"
  lazy val schemaFileKey = "schema_file"
  lazy val schemaTextareaKey = "schema_textarea"
  lazy val showSchemaKey = "showSchema"
  lazy val MAX_CUT = 100
  lazy val MIN_CUT = 1

   def getValidationForm(request: Request[AnyContent]): Try[ValidationForm] = {
    for ( mf <- getMultipartForm(request)
        ; inputData <- parseInputData(mf)
        ; opt_data <- parseOptData(mf)
        ; opt_schema <- parseOptSchema(mf)
        )
    yield {
     val has_schema = opt_schema.isDefined
     val input_schema = if (has_schema) 
              opt_schema.get._1 
             else 
                SchemaInput()

     val opts_schema = if (has_schema) 
              opt_schema.get._2 
              else 
                SchemaOptions.default
      ValidationForm(inputData, opt_data, has_schema, input_schema, opts_schema, "")
    }
  }

  def getValidationFormSchema(request: Request[AnyContent]): Try[ValidationForm] = {
    for ( mf <- getMultipartForm(request)
        ; inputSchema <- parseSchemaInput(mf)
        ; optsSchema <- parseSchemaOptions(mf)
        )
    yield {
      ValidationForm(DataInput(), DataOptions.default,true, inputSchema, optsSchema, "")
    }
  }
  
  def getMultipartForm(request: Request[AnyContent]): Try[MultipartFormData[TemporaryFile]] = {
    val body: AnyContent = request.body
   body.asMultipartFormData match {
      case Some(mf) => TrySuccess(mf)
      case None => TryFailure(new Exception("Expecting MultiformData request body"))
    }
  }

  
  def parseFile(mf: MultipartFormData[TemporaryFile], key:String) : Try[Option[File]] = {
    mf.file(key) match {
         case Some(f) => TrySuccess(Some(f.ref.file))
         case None => {
          // Failure(throw new Exception("File " + key + " not found in multipart form data")) 
          TrySuccess(None) 
         }
    }
  }
  
  def parseOptIRI(mf: MultipartFormData[TemporaryFile]): Try[Option[IRI]] = {
    val withIRI = parseWithIRI(mf)
    withIRI match {
      case TrySuccess(true) => {
        parseIRI(mf) match {
            case TrySuccess(iri) => TrySuccess(Some(iri))
            case TryFailure(e) => TryFailure(e)
          }
      }
      case TrySuccess(false) => TrySuccess(None)
      case TryFailure(e) => TryFailure(e)  
    }
  }

  def parseWithIRI(mf: MultipartFormData[TemporaryFile]): Try[Boolean] = {
    for (value <- parseKey(mf,withIRIKey)) yield {
      if (value.startsWith(noIriStem)) false
      else if (value.startsWith(iriStem)) true
      else throw new Exception("parseWithIRI: unknown value " + value)
    }
  }

  def parseIRI(mf: MultipartFormData[TemporaryFile]): Try[IRI] = {
    for (value <- parseKey(mf,iriKey)) yield IRI(value)
  }

  def parseInputData(mf: MultipartFormData[TemporaryFile]): Try[DataInput] = {
   for ( input_type_data <- parseInputType(mf,dataKey)
       ; data_uri <- parseKeyOrElse(mf,dataUriKey,"")
       ; data_textarea <- parseKeyOrElse(mf,dataTextareaKey,"")
       ; data_file <- parseFile(mf,dataFileKey)
//       ; data_endpoint <- parseKey(mf,"data_endpoint")
       ) yield {
     DataInput(input_type_data, data_uri, data_file, data_textarea)
   }
  }
  
  def parseOptData(mf: MultipartFormData[TemporaryFile]): Try[DataOptions] = {
    for ( showData <- parseBoolean(mf,showDataKey)
        ; data_format <- parseKey(mf,dataFormatKey)
        ) yield 
        DataOptions(format = data_format, 
                    showData = showData)
   }

  def parseOptSchema(mf: MultipartFormData[TemporaryFile]): Try[Option[(SchemaInput,SchemaOptions)]] = {
    for (value <- parseKey(mf,schemaKey)) yield {
      value match {
        case `schemaStem` => {
          val opts = 
            for ( input <- parseSchemaInput(mf)
                ; options <- parseSchemaOptions(mf)
                ) yield (input,options)
            opts.map(pair => Some(pair)).get
        }
        case `noSchemaStem` => 
          None
        case _ => 
          throw new Exception("Unknown value for key schema: " + value)
      }
    }
  }
  
  def parseSchemaVocabulary(mf: MultipartFormData[TemporaryFile]): Try[SchemaVocabulary] = {
    for {
      schema_vocabulary <- parseKey(mf,schemaVocabularyKey)
    ; schemaVocabulary <- SchemaVocabulary.lookup(schema_vocabulary)
    } yield schemaVocabulary
  }

  def parseSchemaProcessor(mf: MultipartFormData[TemporaryFile]): Try[SchemaProcessor] = {
    for {
      schema_processor <- parseKey(mf,schemaProcessorKey)
    ; schemaProcessor <- SchemaProcessors.lookup(schema_processor)
    } yield schemaProcessor
  }
  
  def parseSchemaLanguage(mf: MultipartFormData[TemporaryFile]): Try[SchemaLanguage] = {
    for {
      schema_format <- parseKey(mf,schemaFormatKey)
    ; schema_vocab <- parseSchemaVocabulary(mf)
    } yield SchemaLanguage(schema_format,schema_vocab)
  }

  
  def parseSchemaInput(mf: MultipartFormData[TemporaryFile]): Try[SchemaInput] = {
    for ( input_type_schema <- parseInputType(mf,inputSchemaKey)
        ; schema_uri <- parseKey(mf,schemaUriKey)
        ; schema_file <- parseFile(mf,schemaFileKey)
        ; schema_textarea <- parseKey(mf,schemaTextareaKey)
        ; schema_format <- parseKey(mf,schemaFormatKey)
        ; schema_language <- parseSchemaLanguage(mf)
        ; schema_processor <- parseSchemaProcessor(mf)
        )
   yield
     SchemaInput(input_type_schema,
         schema_uri, 
         schema_file, 
         schema_textarea, 
         schema_format, 
         schema_language,
         schema_processor)
  }

  def parseSchemaOptions(mf: MultipartFormData[TemporaryFile]): Try[SchemaOptions] = {
    for ( cut <- parseInt(mf,"cut",MIN_CUT,MAX_CUT)
        ; opt_iri <- parseOptIRI(mf)
        ; showSchema <- parseBoolean(mf,showSchemaKey)
        ; schemaLanguage <- parseSchemaLanguage(mf)
        ; schemaProcessor <- parseSchemaProcessor(mf)
        )
   yield
     SchemaOptions(cut,opt_iri,showSchema,schemaLanguage,schemaProcessor)
  }

  def parseBoolean(mf: MultipartFormData[TemporaryFile], key: String): Try[Boolean] = {
    for (value <- parseKey(mf,key)) yield {
      value match {
        case "true" => true
        case "false" => false
        case _ => throw new Exception("parseBoolean: unknown value " + value + " for key " + key)
      }
    }
  }

  def parseInt(mf: MultipartFormData[TemporaryFile], key: String, min: Int, max:Int): Try[Int] = {
    for (value <- parseKey(mf,key)) yield {
      val n = value.toInt
      if (n < min || n > max) throw new Exception("parseInt, n " + n + " must be between " + min + " and " + max)
      else n
    }
  }

  def parseInputType(mf: MultipartFormData[TemporaryFile], key: String): Try[InputType] = {
    for (value <- parseKey(mf,key))
      yield {
      // The following code is to transform #XXX_* to XXX 
      val pattern = ("#(.*)_.*" ).r
      val extracted = pattern.findFirstMatchIn(value).map(_ group 1)
      extracted match {
        case Some("byUri") => ByUri
        case Some("byFile") => ByFile
        case Some("byInput") => ByInput
        case Some("byEndpoint")  => ByEndpoint
        case Some("byDereference") => ByDereference
        case x => throw new Exception("Unknown value for " + key + ": " + value + ". match = " + x)
    }
   }
 } 
  
 def parseKey(mf: MultipartFormData[TemporaryFile],key:String): Try[String] = {
   if (mf.asFormUrlEncoded(key).size == 1) {
      TrySuccess(mf.asFormUrlEncoded(key).head)
   } else TryFailure(throw new 
        Exception("parseKey: key " + key + 
        " must have one value but it has = " + mf.asFormUrlEncoded(key)))
 }

 def parseKeyOrElse(mf: MultipartFormData[TemporaryFile],key:String,alternative: String): Try[String] = {
   if (mf.asFormUrlEncoded(key).size == 1) {
     TrySuccess(mf.asFormUrlEncoded(key).head)
   } else {
     Logger.info("parseKeyOrElse: key " + key + " not found")
     TrySuccess(alternative)
   } 
     
 }

}