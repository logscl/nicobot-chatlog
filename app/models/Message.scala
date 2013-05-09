package models

import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.joda.time.LocalTime
import play.api.libs.json.Reads._
import tools.CustomReads

case class Message(username: String, message: String, timestamp: DateTime)

object Message {
  
	implicit val messageReads: Reads[Message] = (
			(__ \ 'username).read[String] ~
			(__ \ 'message).read[String] ~
			(__ \ 'postedDate).read[DateTime](CustomReads.customJodaDateReads("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
			)(Message.apply _)

}