package es.weso.utils
import scala.xml.Utility.escape

/**
  * Type class to show HTML values
  * @tparam A
  */
trait ShowHTML[A] {
  def showHTML(x: A): String
}

object ShowHTML {

  def apply[A](implicit instance: ShowHTML[A]): ShowHTML[A] = instance

  def code(x: String) = s"<code>${escape(x)}</code>"
  
  implicit val stringShowHTML = new ShowHTML[String] {
    override def showHTML(s: String) = escape(s)
  }

  implicit val intShowHTML = new ShowHTML[Int] {
    def showHTML(s: Int) = s.toString
  }

 implicit def seqShowHTML_UL[A](implicit sa: ShowHTML[A]) = new ShowHTML[Seq[A]] {
  def showHTML(xs: Seq[A]): String = {
    val sb = new StringBuilder
    sb.append("<ul>")
    for (x <- xs) { sb.append(s"<li>${sa.showHTML(x)}</li>") }
    sb.append("</ul>")
    sb.toString
  }
}

/* implicit val pairShowHTML[A: ShowHTML, B:ShowHTML] = new ShowHTML[(A,B)] {
  def showHTML(x: (A,B)):String = 
    "<span class=\"pair_1\">" + 
     x._1.showHTML +
    "</span><span class=\"pair_2\">" +
    x._2.showHTML +
    "</span>"
 }
*/
}


