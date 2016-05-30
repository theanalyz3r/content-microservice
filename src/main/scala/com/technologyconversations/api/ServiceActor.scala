package com.technologyconversations.api

import java.io.File

import akka.actor.Actor
import spray.http.HttpHeaders.RawHeader
import spray.json.DefaultJsonProtocol
import spray.routing.HttpService
import spray.httpx.SprayJsonSupport._
import com.mongodb.casbah.Imports._
import com.novus.salat._
import com.novus.salat.global._
import com.mongodb.casbah.MongoClient
import scala.util.Properties._

case class segmentReduced(_id: Int, title: String, author: String)
case class segment(_id: Int, title: String, author: String, description: String) {
  require(!title.isEmpty)
  require(!author.isEmpty)
}

class ServiceActor extends Actor with ServiceRoute with StaticRoute {

  val address = envOrElse("DB_PORT_27017_TCP", "localhost:27017")
  val client = MongoClient(MongoClientURI(s"mongodb://$address/"))
  val db = client(envOrElse("DB_DBNAME", "segments"))
  val collection = db(envOrElse("DB_COLLECTION", "segments"))

  def actorRefFactory = context
  def receive = runRoute {
    respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))
    { serviceRoute ~ staticRoute }
  }

}

trait StaticRoute extends HttpService {

  val staticRoute = pathPrefix("") {
    getFromDirectory("client/")
  }

}

trait ServiceRoute extends HttpService with DefaultJsonProtocol {

  implicit val segmentsReducedFormat = jsonFormat3(segmentReduced)
  implicit val segmentsFormat = jsonFormat4(segment)
  val collection: MongoCollection

  val serviceRoute = pathPrefix("api" / "v1" / "segments") {
    path("_id" / IntNumber) { id =>
      get {
        complete(
          grater[segment].asObject(
            collection.findOne(MongoDBObject("_id" -> id)).get
          )
        )
      } ~ delete {
        complete(
          grater[segment].asObject(
            collection.findAndRemove(MongoDBObject("_id" -> id)).get
          )
        )
      }
    } ~ pathEnd {
      get {
        complete(
          collection.find().toList.map(grater[segmentReduced].asObject(_))
        )
      } ~ put {
        entity(as[segment]) { segment =>
          collection.update(
            MongoDBObject("_id" -> segment._id),
            grater[segment].asDBObject(segment),
            upsert = true
          )
          complete(segment)
        }
      } ~ post {
        entity(as[segment]) { segment =>
          collection.update(
            MongoDBObject("_id" -> segment._id),
            grater[segment].asDBObject(segment),
            upsert = true
          )
          complete(segment)
        }
      }
    }
  }

}