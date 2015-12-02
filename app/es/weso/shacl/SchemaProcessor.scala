package es.weso.shacl

import scala.util._

sealed trait SchemaProcessor {
 
 def name: String 
 
 override def toString = name
}

// This version is kept for compatibility reasons
// It behaves as ShEx although it is called SHACL_0.1
case object SHACL extends SchemaProcessor {
 override def versionName = "SHACL_0.1"
}

// This is ShEx based on ShExcala
case object ShExcala extends SchemaProcessor {
 override def versionName = "ShExcala"
}

// SHACL as published in FPWD
// This is based on TopQuadrant implementation
case object SHACL_FPWD extends SchemaProcessor {
 override def versionName = "SHACL_FPWD"
}


object SchemaProcessors {
  
  val publicAvailableProcessors : List[SchemaProcessor] = {
    List(ShExcala, SHACL_FPWD)
  }

  val availableProcessors : List[SchemaProcessor] = 
    List(SHACL, ShExcala, SHACL_FPWD)
    
  val availableNames : List[String]= 
    availableProcessors.map(_.name)

  lazy val default = ShExcala
  
  lazy val defaultSchemaProcessor = default.name
  
  def lookup(key:String): Try[SchemaProcessor] = {
    availableProcessors.find(_.name == key) match {
      case None => Failure(new Exception (s"Key $key not found in available schema versions: $availableNames"))
      case Some(x) => Success(x)
    }
  }
  
  def get(key:String): SchemaProcessor = {
    availableProcessors.find(_.name == key).getOrElse(default)
  }
}