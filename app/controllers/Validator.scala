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
import scala.concurrent.Future
import es.weso.rdf._
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdf.jena._
import es.weso.utils.CommonUtils._
import es.weso.utils.RDFUtils
import es.weso.utils.IOUtils._
import java.net.URL
import java.io.File
import DataOptions._
import SchemaOptions._
import es.weso.shacl.{ Action => _, _}
import es.weso.shacl.converter.RDF2Schema
import es.weso.shacl.converter.Schema2RDF

trait Validator { this: Controller =>

  import Multipart._

  /*  def onlyData(data: String, dataFormat: String, schemaVersion: String) = {
    validate_get(
        data, 
        Some(dataFormat), 
        DEFAULT_SHOW_DATA, None, None, schemaVersion, None, DEFAULT_CUT, false)
  } */

  def data(
    data: String,
    dataFormat: String,
    maybeProcessor: Option[String]): Action[AnyContent] = {
    // Create a shapes graph and join it to the RDF data
    val rdf: RDFBuilder = RDFAsJenaModel.empty
    val trySchema = for {
      format <-RDFUtils.getFormat(Some(dataFormat))
      processor <- SchemaProcessor.lookup(maybeProcessor.getOrElse(SchemaProcessor.default.name))
      opts_data = DataOptions(format = format, showData = true)
      opts_schema = SchemaOptions.default
      (schema, pm) <- Schema.fromString(data, dataFormat)
    } yield (schema, processor)
    trySchema match {
      case Success((schema,processor)) => {
        validate_get(data,
          Some(dataFormat),
          DEFAULT_SHOW_DATA,
          Some(schema.serialize("SHEXC")),
          Some("SHEXC"),
          Some("SHEX"),
          Some(processor.name),
          None,
          DEFAULT_CUT,
          DEFAULT_ShowSchema)
      }
      case Failure(e) =>
        throw new Exception(s"data: unsupported failure: $e. Data: $data, $dataFormat")
      }
  }

  def dataSchema(
    data: String,
    dataFormat: String,
    schema: String,
    schemaFormat: String,
    schemaVocabulary: String,
    schemaProcessor: String): Action[AnyContent] = {
    validate_get(data,
      Some(dataFormat),
      DEFAULT_SHOW_DATA,
      Some(schema),
      Some(schemaFormat),
      Some(schemaVocabulary),
      Some(schemaProcessor),
      None,
      DEFAULT_CUT,
      DEFAULT_ShowSchema)
  }

  def dataSchemaNode(
    data: String,
    dataFormat: String,
    schema: String,
    schemaFormat: String,
    schemaVocabulary: String,
    schemaProcessor: String,
    node: String) = {
    validate_get(data,
      Some(dataFormat),
      DEFAULT_SHOW_DATA,
      Some(schema),
      Some(schemaFormat), Some(schemaVocabulary), Some(schemaProcessor), Some(node),
      DEFAULT_CUT,
      DEFAULT_ShowSchema)
  }

  def validate_rdf_get_Future(
    str_data: String,
    formatData: Option[String],
    showData: Boolean,
    schemaVocabulary: Option[String],
    schemaProcessor: Option[String],
    opt_iri: Option[String],
    cut: Int,
    showSchema: Boolean): Future[Try[ValidationResult]] = {
    val tryResult = 
      for {
        format <- RDFUtils.getFormat(formatData) 
        opts_data = DataOptions(format = format, showData = showData)
        iri = opt_iri.map(str => IRI(str))
        opts_schema = SchemaOptions(cut = cut, opt_iri = iri, showSchema)
        rdf <- RDFUtils.parseRDF(str_data, format) 
        processor <- SchemaProcessor.lookupOption(schemaProcessor)
        language <- SchemaLanguage.lookupOption(formatData,schemaVocabulary)
        // TODO...do something different for SHACL
        (schema, pm) <- RDF2Schema.rdf2Schema(rdf)
      } yield 
         ValidationResult.validate(
            rdf,
            str_data,
            opts_data,
            true,
            str_data,
            language, 
            processor, 
            opts_schema)
     Future(tryResult)
  }
  

  def validate_get_Future(
    str_data: String,
    formatData: Option[String],
    showData: Boolean,
    opt_schema: Option[String],
    schemaFormat: Option[String],
    schemaVocabulary: Option[String],
    schemaProcessor: Option[String],
    opt_iri: Option[String],
    cut: Int,
    showSchema: Boolean): Future[Try[ValidationResult]] = {
    val tryResult = for {
      format <- RDFUtils.getFormat(formatData) 
      opts_data = DataOptions(format = format, showData = showData)
      withSchema = opt_schema.isDefined
      iri = opt_iri.map(str => IRI(str))
      opts_schema = SchemaOptions(cut = cut, opt_iri = iri, showSchema)
      str_schema = opt_schema.getOrElse("")
      rdf <- RDFUtils.parseRDF(str_data, format) 
      processor <- SchemaProcessor.lookupOption(schemaProcessor)
      language <- SchemaLanguage.lookupOption(formatData,schemaVocabulary)
        // TODO...do something different for SHACL
       (schema, pm) <- RDF2Schema.rdf2Schema(rdf)
    } yield 
     ValidationResult.validate(
              rdf,
              str_data,
              opts_data,
              withSchema,
              str_schema,
              language, 
              processor, 
              opts_schema)
 
      Future(tryResult)
  }

  // TODO: Simplify this ugly code...long list of arguments
  def validate_get_old(
    str_data: String, 
    dataFormat: Option[String], 
    showData: Boolean, 
    opt_schema: Option[String], 
    maybeSchemaFormat: Option[String], 
    schemaVersion: String,
    opt_iri: Option[String], 
    cut: Int, 
    showSchema: Boolean
    ) = Action.async {
    validate_get_Future(str_data,
      dataFormat,
      showData,
      opt_schema,
      maybeSchemaFormat,
      Some(SchemaVocabulary.ShEx.name),
      Some(SchemaProcessor.ShExcala.name),
      opt_iri,
      cut,
      showSchema).map(vrf => {
        vrf match {
          case Success(vr) => {
            val vf = ValidationForm.fromResult(vr)
            Ok(views.html.index(vr, vf))
          }
          case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
        }
      })
  }

  // TODO: Simplify this ugly code...long list of arguments
  def validate_get(
    str_data: String, 
    dataFormat: Option[String], 
    showData: Boolean, 
    opt_schema: Option[String], 
    schemaFormat: Option[String], 
    schemaVocabulary: Option[String],
    schemaProcessor: Option[String],
    opt_iri: Option[String], 
    cut: Int, 
    showSchema: Boolean
    ) = Action.async {
    validate_get_Future(str_data,
      dataFormat,
      showData,
      opt_schema,
      schemaFormat,
      schemaVocabulary,
      schemaProcessor,
      opt_iri,
      cut,
      showSchema).map(vrf => {
        vrf match {
          case Success(vr) => {
            val vf = ValidationForm.fromResult(vr)
            Ok(views.html.index(vr, vf))
          }
          case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
        }
      })
  }

  // TODO: Simplify this ugly code...long list of arguments
  def validate_rdf_get(
    str_data: String,
    dataFormat: Option[String],
    showData: Boolean,
    schemaVocabulary: Option[String],
    schemaProcessor: Option[String],
    opt_iri: Option[String], 
    cut: Int, 
    showSchema: Boolean): Action[AnyContent] = Action.async {
    validate_rdf_get_Future(str_data,
      dataFormat,
      showData,
      schemaVocabulary,
      schemaProcessor,
      opt_iri,
      cut,
      showSchema).map(vrf => {
        vrf match {
          case Success(vr) => {
            val vf = ValidationForm.fromResult(vr)
            Ok(views.html.index(vr, vf))
          }
          case Failure(e) => 
            BadRequest(views.html.errorPage(e.getMessage))
        }
      })
  }

  def validate_post = Action.async { request =>
    {
      val pair = for (
        vf <- getValidationForm(request); str_data <- vf.dataInput.getDataStr
      ) yield (vf, str_data)

      scala.concurrent.Future {
        pair match {
          case Success((vf, str_data)) => {
            val tryValidate =
              for (
                data <- vf.dataInput.getData(vf.dataOptions.format); 
                str_schema <- vf.schemaInput.getSchemaStr
              ) yield {
                ValidationResult.validate(
                  data,
                  str_data,
                  vf.dataOptions,
                  vf.withSchema,
                  str_schema,
                  vf.schemaInput.schemaLanguage,
                  vf.schemaInput.schemaProcessor,
                  vf.schemaOptions)
              }
            val vr = getWithRecoverFunction(tryValidate, recoverValidationResult(str_data, vf))
            Ok(views.html.index(vr, vf))
          }
          case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
        }
      }
    }
  }

  def recoverValidationResult(str_data: String, vf: ValidationForm)(e: Throwable): ValidationResult = {
    val schema_str: String = Try(vf.schemaInput.getSchemaStr.get).getOrElse("")
    ValidationResult(
      Some(false),
      e.getMessage(),
      Stream(),
      List(),
      str_data,
      vf.dataOptions,
      vf.withSchema,
      schema_str,
      vf.schemaInput.schemaLanguage,
      vf.schemaInput.schemaProcessor,
      vf.schemaOptions,
      PrefixMap.empty)
  }
  
  def byEndpoint(
    schema: String,
    schemaFormat: String,
    schemaVocabulary: String, 
    schemaProcessor: String, 
    endpoint: String): Action[AnyContent] = Action { requesr => 
    BadRequest(views.html.errorPage("This option is currently under maintenance"))
  }

  def byDereference(
    schema: String,
    schemaFormat: String,
    schemaVocabulary: String, 
    schemaProcessor: String, 
    focusNode: String): Action[AnyContent] = Action { request =>
      BadRequest(views.html.errorPage("This option is currently under maintenance"))
  }

}

object Validator extends Controller with Validator 