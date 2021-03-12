package es.weso.server

import cats.data.EitherT
import cats.effect._
import cats.implicits._
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.server.APIDefinitions._
import es.weso.server.QueryParams._
import es.weso.utils.IOUtils._
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.server.staticcontent.resourceServiceBuilder
import org.log4s.getLogger

import scala.util.Try

class APIService[F[_]:ConcurrentEffect: Timer](blocker: Blocker,
                                               client: Client[F])(implicit cs: ContextShift[F])
  extends Http4sDsl[F] {

  private val relativeBase = Defaults.relativeBase
  private val logger = getLogger

  private val swagger =
    resourceServiceBuilder[F]("/swagger", blocker) // ResourceService.Config())

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case req @ GET -> Root / `api` / "health" => for { 
      _ <- LiftIO[F].liftIO (IO { pprint.log(req) })
      resp <- Ok("OK")
    } yield resp

    case req @ GET -> Root / `api` / "endpoint" / "outgoing" :?
      OptEndpointParam(optEndpoint) +&
      OptNodeParam(optNode) +&
      LimitParam(optLimit)
      => for {
      eitherOutgoing <- getOutgoing(optEndpoint, optNode, optLimit).value
      resp <- eitherOutgoing.fold((s: String) => errJson(s"Error: $s"), (outgoing: Outgoing) => Ok(outgoing.toJson))
    } yield resp

    // Contents on /swagger are directly mapped to /swagger
    // case r @ GET -> _ if r.pathInfo.startsWith(UriPath.fromString("/swagger/")) => swagger.toRoutes. // getOrElseF(NotFound())

  }

  private def parseInt(s: String): Either[String, Int] =
    Try(s.toInt).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  private def errJson(msg: String): F[Response[F]] =
    Ok(Json.fromFields(List(("error",Json.fromString(msg)))))

  private def getOutgoing(optEndpoint: Option[String], optNode: Option[String], optLimit: Option[String]
                 ): EitherT[F,String,Outgoing] = {
    for {
      endpointIRI <- EitherT.fromEither[F](Either.fromOption(optEndpoint,"No endpoint provided").flatMap(IRI.fromString(_)))
      node <- EitherT.fromEither[F](Either.fromOption(optNode,"No node provided").flatMap(IRI.fromString(_)))
      limit <- EitherT.fromEither[F](parseInt(optLimit.getOrElse("1")))
      o <- outgoing(endpointIRI,node,limit)
    } yield o
  }

  private def outgoing(endpoint: IRI, node: IRI, limit: Int): ESF[Outgoing,F] = for {
    triples <- esio2esf(stream2es(Endpoint(endpoint).triplesWithSubject(node)))
  } yield Outgoing.fromTriples(node,endpoint,triples.toSet)

    //    Monad[F].pure(Left(s"Not implemented"))

}

object APIService {
  def apply[F[_]: ConcurrentEffect: ContextShift: Timer](blocker: Blocker, client: Client[F]): APIService[F] =
    new APIService[F](blocker, client)
}
