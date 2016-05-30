package com.technologyconversations.api

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat._
import com.novus.salat.global._
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeExample
import spray.http.{ContentType, HttpEntity}
import spray.http.MediaTypes._
import spray.routing.HttpService
import spray.testkit.Specs2RouteTest
import spray.http.StatusCodes._
import spray.json._
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._

import spray.httpx.SprayJsonSupport._

class ServiceSpec extends Specification with Specs2RouteTest with HttpService with ServiceRoute with StaticRoute with BeforeExample {

  val client = MongoClient("localhost", 27017)
  val db = client("segments")
  val collection = db("segments")
  val apiUri = "/api/v1/segments"
  val staticUri = "/test.html"
  val segmentId = 1234

  def actorRefFactory = system
  def before = db.dropDatabase()

  sequential

  s"GET $staticUri" should {

    "return OK" in {
      Get(staticUri) ~> staticRoute ~> check {
        response.status must equalTo(OK)
      }
    }

    "return file content" in {
      Get(staticUri) ~> staticRoute ~> check {
        val content = responseAs[String]
        content must equalTo("This is just a test")
      }
    }

  }

  s"GET $apiUri" should {

    "return OK" in {
      Get(apiUri) ~> serviceRoute ~> check {
        response.status must equalTo(OK)
      }
    }

    "return all segments" in {
      val expected = insertsegments(3).map { segment =>
        segmentReduced(segment._id, segment.title, segment.author)
      }
      Get(apiUri) ~> serviceRoute ~> check {
        response.entity must not equalTo None
        val segments = responseAs[List[segmentReduced]]
        segments must haveSize(expected.size)
        segments must equalTo(expected)
      }
    }

  }

  s"GET $apiUri/_id/$segmentId" should {

    val expected = segment(segmentId, "Title", "Author", "Description")

    "return OK" in {
      insertsegment(expected)
      Get(s"$apiUri/_id/$segmentId") ~> serviceRoute ~> check {
        response.status must equalTo(OK)
      }
    }

    "return segment" in {
      insertsegment(expected)
      Get(s"$apiUri/_id/$segmentId") ~> serviceRoute ~> check {
        response.entity must not equalTo None
        val segment = responseAs[segment]
        segment must equalTo(expected)
      }
    }

  }

  s"PUT $apiUri" should {

    val expected = segment(segmentId, "PUT title", "Put author", "Put description")

    "return OK" in {
      Put(apiUri, expected) ~> serviceRoute ~> check {
        response.status must equalTo(OK)
      }
    }

    "return segment" in {
      Put(apiUri, expected) ~> serviceRoute ~> check {
        response.entity must not equalTo None
        val segment = responseAs[segment]
        segment must equalTo(expected)
      }
    }

    "insert segment to the DB" in {
      Put(apiUri, expected) ~> serviceRoute ~> check {
        response.status must equalTo(OK)
        val segment = getsegment(segmentId)
        segment must equalTo(expected)
      }
    }

    "update segment when it exists in the DB" in {
      collection.insert(grater[segment].asDBObject(expected))
      Put(apiUri, expected) ~> serviceRoute ~> check {
        response.status must equalTo(OK)
        val segment = getsegment(segmentId)
        segment must equalTo(expected)
      }
    }

  }

  s"DELETE $apiUri/_id/$segmentId" should {

    val expected = segment(segmentId, "Title", "Author", "Description")

    "return OK" in {
      insertsegment(expected)
      Delete(s"$apiUri/_id/$segmentId") ~> serviceRoute ~> check {
        response.status must equalTo(OK)
      }
    }

    "return segment" in {
      insertsegment(expected)
      Delete(s"$apiUri/_id/$segmentId") ~> serviceRoute ~> check {
        response.entity must not equalTo None
        val segment = responseAs[segment]
        segment must equalTo(expected)
      }
    }

    "remove segment from the DB" in {
      insertsegment(expected)
      Delete(s"$apiUri/_id/$segmentId") ~> serviceRoute ~> check {
        response.status must equalTo(OK)
        getsegments must haveSize(0)
      }
    }

  }

  def insertsegment(segment: segment) {
    collection.insert(grater[segment].asDBObject(segment))
  }

  def insertsegments(quantity: Int): List[segment] = {
    val segments = List.tabulate(quantity)(id => segment(id, s"Title $id", s"Author $id", s"Description $id"))
    for (segment <- segments) {
      collection.insert(grater[segment].asDBObject(segment))
    }
    segments
  }

  def getsegment(id: Int): segment = {
    val dbObject = collection.findOne(MongoDBObject("_id" -> id))
    grater[segment].asObject(dbObject.get)
  }

  def getsegments: List[segment] = {
    collection.find().toList.map(grater[segment].asObject(_))
  }

}
