package controllers

// import es.weso.shex.{Schema => ShexSchema}
import es.weso.shacl._
import es.weso.shacl.SchemaProcessor._
import es.weso.rdf._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdfgraph.nodes.IRI
import java.io.File
import es.weso.utils.IOUtils._
import util._
import es.weso.utils.SchemaUtils
import es.weso.shacl.SchemaVocabulary
import es.weso.shacl.SchemaProcessor
import es.weso.shacl.SchemaLanguage

case class SchemaInput(
      input_type_Schema: InputType
    , schema_uri : String
    , schema_file: Option[File]
    , schema_textarea: String
    , schemaLanguage: SchemaLanguage
    , schemaProcessor: SchemaProcessor
    ) {
  
  def convertSchema(targetLanguage: SchemaLanguage): Try[String] = {
    schemaProcessor match {
      case SchemaProcessor.SHACL => // TODO: SHACL = Shexcala by now...remove in the future
        for { inputStr <- getSchemaStr
            ; outStr <- ShaclConverter(inputStr, schemaLanguage.format.name, targetLanguage)
            } 
        yield outStr
      case SchemaProcessor.ShExcala =>
        for { inputStr <- getSchemaStr
            ; outStr <- ShaclConverter(inputStr, schemaLanguage.format.name, targetLanguage)
            } 
        yield outStr
      case SHACL_FPWD => {
        throw new Error(s"convertSchema: Unsupported schemaProcessor: $schemaProcessor yet")
      }
      case _ => {
        throw new Error(s"convertSchema: Unsupported schemaProcessor: $schemaProcessor yet")
      }
    }
  }
  
  def ShaclConverter(str: String, 
      inputFormat: String, 
      targetLanguage: SchemaLanguage): Try[String] = {
    if (str == "") Success("")
    else {
      for {
        (schema,pm) <- Schema.fromString(str,inputFormat)
      } yield schema.serialize(targetLanguage.format.name)
    }
  }
  
  def getSchemaStr: Try[String] = {
   input_type_Schema match {
     case ByUri => 
       if (schema_uri == "") 
    	 			Failure(throw new Exception("Empty URI"))
    	 		   else getURI(schema_uri)
     case ByFile => 
       getFileContents(schema_file)
     case ByInput => 
       Success(schema_textarea)
     case _ => 
       Failure(throw new Exception("get_SchemaString: Unsupported input type"))
   }
  }
  
  def extract_str() : String = {
    this.getSchemaStr.getOrElse("")
  }
  
  def schemaVocabulary: SchemaVocabulary = {
    schemaLanguage.vocabulary
  }
  
  def inputFormat: String = {
    schemaLanguage.format.name
  }

}
    
object SchemaInput {
  def apply() : SchemaInput = 
    SchemaInput( 
               input_type_Schema = ByInput
             , schema_uri = ""
             , schema_file = None
             , schema_textarea = ""
             , schemaLanguage = SchemaLanguage.default
             , schemaProcessor = SchemaProcessor.default
             )

  def build(str: String, format: String, vocab: String, processor: String): Try[SchemaInput] =
     for {
       schemaLanguage <- SchemaLanguage.lookup(format,vocab)
       schemaProcessor <- SchemaProcessor.lookup(processor)
     } yield 
    SchemaInput( 
               input_type_Schema = ByInput
        	   , schema_uri = ""
        	   , schema_file = None
        	   , schema_textarea = str
             , schemaLanguage = schemaLanguage
             , schemaProcessor = schemaProcessor
        	   )
     
  def apply(str: String, language: SchemaLanguage, processor: SchemaProcessor): SchemaInput = 
    SchemaInput( 
               input_type_Schema = ByInput
        	   , schema_uri = ""
        	   , schema_file = None
        	   , schema_textarea = str
             , schemaLanguage = language
             , schemaProcessor = processor
        	   )
        	   
}
