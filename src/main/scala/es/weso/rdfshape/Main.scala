package es.weso.rdfshape

import cats.effect._
import com.typesafe.scalalogging._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema._
import es.weso.server._
import es.weso.utils.FileUtils
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import java.nio.file._
import scala.concurrent.duration._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI

object Main extends App with LazyLogging {
    try {
      run(args)
    } catch {
      case e: Exception =>
        println(s"Error: ${e.getMessage}")
    }

  def run(args: Array[String]): Unit = {
    val opts = new MainOpts(args, errorDriver)
    opts.verify()

    if (opts.server()) {
      RDFShapeServer.main(args)
    }

    val baseFolder: Path = if (opts.baseFolder.isDefined) {
      Paths.get(opts.baseFolder())
    } else {
      Paths.get(".")
    }

    val startTime = System.nanoTime()

    val base = Some(FileUtils.currentFolderURL)

    /*
    val validateOptions: IO[(String, Schema, ValidationTrigger,Result)] = getRDFReader(opts, baseFolder).use(rdf => for {
      schema <- getSchema(opts, baseFolder, rdf)
      triggerName = opts.trigger.toOption.getOrElse(ValidationTrigger.default.name)
      shapeMapStr <- getShapeMapStr(opts)
      trigger <- either2es(ValidationTrigger.findTrigger(triggerName, shapeMapStr, base,
        opts.node.toOption, opts.shapeLabel.toOption,
        rdf.getPrefixMap(), schema.pm))
      outDataFormat = opts.outDataFormat.getOrElse(opts.dataFormat())        
      str <- rdf.serialize(outDataFormat)  
    } yield (str, schema, trigger))

    validateOptions.attempt.unsafeRunSync match {
      case Left(e) => {
        println(s"Error: $e")
      }
      case Right((str, schema, trigger)) => {
        if (opts.showData()) {
          println(str)
        }
        if (opts.showSchema()) {
          // If not specified uses the input schema format
          val outSchemaFormat = opts.outSchemaFormat.getOrElse(opts.schemaFormat())
          schema.serialize(outSchemaFormat).attempt.unsafeRunSync match {
            case Right(str) => println(str)
            case Left(e) => println(s"Error showing schema $schema with format $outSchemaFormat: ${e.getMessage}")
          }
        }

        if (opts.showShapeMap()) {
          println(s"Trigger shapemap: ${trigger.shapeMap}")
          println(s"ShapeMap: ${trigger.shapeMap.serialize(opts.outShapeMapFormat())}")
          println(s"Trigger json: ${trigger.toJson.spaces2}")
        }

        val result = schema.validate(rdf, trigger)

        if (opts.showLog()) {
          logger.info("Show log info = true")
          logger.info(s"JSON result: ${result.unsafeRunSync().toJsonString2spaces}")
        }

        if (opts.showResult() || opts.outputFile.isDefined) {
          val resultSerialized = result.unsafeRunSync().serialize(opts.resultFormat())
          if (opts.showResult()) println(resultSerialized)
          if (opts.outputFile.isDefined)
            FileUtils.writeFile(opts.outputFile(), resultSerialized)
        }

        if (opts.showValidationReport()) {
          val vr = result.unsafeRunSync().validationReport
          for {
            rdf <- vr
            str = rdf.serialize(opts.validationReportFormat()).unsafeRunSync()
          } yield str

            /*.fold(
            e => println(s"Error: ${e.getMessage}"),
            println(_)
          )*/
        }

        if (opts.cnvEngine.isDefined) {
          logger.error("Conversion between engines don't implemented yet")
        }

        if (opts.time()) {
          val endTime = System.nanoTime()
          val time: Long = endTime - startTime
          printTime("Time", opts, time)
        }

      } 
    } */

  }

  def printTime(msg: String, opts: MainOpts, nanos: Long): Unit = {
    if (opts.time()) {
      val time = Duration(nanos, NANOSECONDS).toMillis
      println(f"$msg%s, $time%10d")
    }
  }

  private def errorDriver(e: Throwable, scallop: Scallop): Nothing = e match {
    case Help(s) =>
      println("Help: " + s)
      scallop.printHelp
      sys.exit(0)
    case _ =>
      println("Error: %s".format(e.getMessage))
      scallop.printHelp
      sys.exit(1)
  }

/*  def getShapeMapStr(opts: MainOpts): IO[String] = {
    if (opts.shapeMap.isDefined) {
      // val shapeMapFormat = opts.shapeMapFormat.toOption.getOrElse("COMPACT")
      for {
        // TODO: Allow different shapeMap formats
        content <- FileUtils.getContents(opts.shapeMap())
      } yield content.toString
    } else EitherT.pure[IO,String]("")
  } */

  def getRDFReader(opts: MainOpts, baseFolder: Path): IO[Resource[IO,RDFReader]] = {
    val base = Some(IRI(FileUtils.currentFolderURL))
    if (opts.data.isDefined) {
      val path = baseFolder.resolve(opts.data())
      for {
        res <- RDFAsJenaModel.fromFile(path.toFile, opts.dataFormat(), base)
/*        newRdf <- if (opts.inference.isDefined) {
          io2es(rdf.applyInference(opts.inference()))
        } else
          ok_es(rdf) */
      } yield res
    } else {
      logger.info("RDF Data option not specified")
      RDFAsJenaModel.empty
    }
  }

  def getSchema(opts: MainOpts, baseFolder: Path, rdf: RDFReader): IO[Schema] = {
    val base = Some(FileUtils.currentFolderURL)
    if (opts.schema.isDefined) {
      val path = baseFolder.resolve(opts.schema())
      val schema = Schemas.fromFile(path.toFile, opts.schemaFormat(), opts.engine(), base)
      schema
    } else {
      logger.info("Schema not specified. Extracting schema from data")
      Schemas.fromRDF(rdf, opts.engine())
    }
  }
}

