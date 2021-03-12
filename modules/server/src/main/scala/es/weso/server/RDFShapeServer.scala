package es.weso.server
import cats.effect._

import scala.concurrent.ExecutionContext.global
import scala.util.Properties.envOrNone

/**
 * RDFShape server
 **/

object RDFShapeServer extends IOApp {

  private val ip = "0.0.0.0"
  private val port = envOrNone("PORT") map (_.toInt) getOrElse 8080
  println(s"PORT: $port")

  def main(args: List[String]): Unit = {
    run(args).unsafeRunSync()
  }

  override def run(args: List[String]): IO[ExitCode]  = {
    val blocker = Blocker.liftExecutionContext(global)
    Server.stream[IO](blocker,port,ip).compile.drain.as(ExitCode.Success)
    /* for {
     sslCtx <- SSLHelper.getContextFromClassPath("password", "alno631")
    } yield ExitCode.Success */
  }


}
