package tools

import scala.collection.Seq
import org.joda.time.DateTime
import play.api.data.validation.ValidationError
import play.api.libs.json._
import org.joda.time.DateTimeZone

object CustomReads {
    
  /**
	* Custom Reads for the `org.joda.time.DateTime` type.
	*
	* @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
	* @param corrector a simple string transformation function that can be used to transform input String before parsing. 
	* Useful when standards are not exactly respected and require a few tweaks
	* 
	* Keep the date as a UTC date, but will returns it in a brussels zone.
	*/
	  def customJodaDateReads(pattern: String, corrector: String => String = identity): Reads[org.joda.time.DateTime] = new Reads[org.joda.time.DateTime] {
	    import org.joda.time.DateTime
	
	    val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
	
	    def reads(json: JsValue): JsResult[DateTime] = json match {
	      case JsNumber(d) => JsSuccess(new DateTime(d.toLong))
	      case JsString(s) => parseDate(corrector(s)) match {
	        case Some(d) => JsSuccess(d)
	        case None => JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected.jodadate.format", pattern))))
	      }
	      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected.date"))))
	    }
	
	    private def parseDate(input: String): Option[DateTime] =
	      scala.util.control.Exception.allCatch[DateTime] opt (DateTime.parse(input, df).withZoneRetainFields(DateTimeZone.UTC).withZone(DateTimeZone.forID("Europe/Brussels")))
	
	  }

}