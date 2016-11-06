package controllers

import play.api._
import play.api.mvc._
import buildinfo._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.PrefixMap
import play.api.i18n.{I18nSupport, MessagesApi}
import javax.inject._

// We use traits instead of objects to be able to test them
// More info: https://www.playframework.com/documentation/2.3.x/ScalaTestingWithScalaTest
@Singleton
class Application @Inject()(implicit val webJarAssets: WebJarAssets,
                            val messagesApi: MessagesApi) extends Controller with I18nSupport {

  lazy val name = "RDFShape" 
  lazy val shexcalaName = BuildInfo.name + "(" + BuildInfo.version + ")" 


  def index = Action {
    val pm = PrefixMap.empty
    val iri = rdf_type
    val pm2 = pm.addPrefix("",iri)
    println("Hoooola. Prefix map = " + pm2 + "!")
    Ok(views.html.index())
  }

  def about = Action {
    Ok(views.html.about(name + "|" + shexcalaName)(ValidationForm()))
  }

  def help = Action {
    Ok(views.html.help()(ValidationForm()))
  }
    
}

