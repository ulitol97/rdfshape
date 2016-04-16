package controllers

import scala.concurrent.Future
import util._

import Multipart._
import es.weso.shacl.DataFormat
import es.weso.utils.RDFUtils.parseRDF
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Controller

class DataConverter extends Controller { 

 def converterDataFuture(
          data: String
        , dataFormat: String
        , outputFormat: String
    ) : Future[Try[String]]= {
       Future(parseRDF(data,dataFormat).map(_.serialize(outputFormat)))
  }
  
  
  def convert_data_get(
          data: String
        , dataFormat: String
        , targetFormat: String
        ) = Action.async {  
        converterDataFuture(data,dataFormat, targetFormat).map(output => {
              output match {
                case Success(result) => {
                  val vf = ValidationForm.fromDataConversion(data,dataFormat)
                  Ok(views.html.convert_data(vf,targetFormat,result))
                }
                case Failure(e) => BadRequest(views.html.errorPage(e.getMessage))
              }
          })
  }


    def convert_data_post = Action { request => {
     val r = for ( 
       mf <- getMultipartForm(request)
     ; vf <- getValidationForm(request)
     ; str_data <- vf.dataInput.getDataStr
     ; outputFormat <- parseKey(mf, "outputFormat")
     ; data <- vf.dataInput.getData(vf.dataOptions.format)
     ) yield (vf,outputFormat,data.serialize(outputFormat))
     
      r match {
       case Success((vf,outputFormat,result)) =>
             Ok(views.html.convert_data(vf,outputFormat,result))
       case Failure(e) => BadRequest(views.html.errorPage(e.getMessage)) 
      }
    } 
  }
  
  def dataFormats = Action {
    Ok(Json.toJson(DataFormat.toList))
  }
   
}

