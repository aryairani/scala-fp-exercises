/**
 * Created with IntelliJ IDEA.
 * User: arya
 * Date: 12/3/12
 * Time: 3:00 PM
 * To change this template use File | Settings | File Templates.
 */

object util {
  import moviereview._

  /**
   * Parse a www-url-encoded query string into a Map[String,Option[String]]
   * @param s www-url-encoded query
   * @return Map is String -> Option[String] instead of String -> String,
   *         to distinguish between "name&occupation"     (no value)
   *                           and  "name=&occupation="   (empty value)
   */
  def parseQuery(s: String): Map[String,Option[String]] = {

    // matches if there's an equals sign
    val HasValue: scala.util.matching.Regex =
      "(.*)=(.*)".r

    def splitPair: String => (String,Option[String]) = {
      case HasValue(key, value) => key -> Some(value)
      case key                  => key -> None
    }

    // function to do utf8 url decoding
    val decode: String => String =
      java.net.URLDecoder.decode(_, "utf8")

    def decodePair: ((String, Option[String])) => (String,Option[String]) = {
      case (k,v) => decode(k) -> v.map(decode)
    }

    val params = s.split('&')
    val decodedPairs = params map splitPair filter { ! _._1.isEmpty } map decodePair

    Map(decodedPairs: _*)

  }

  /**
   * Construct a MovieReview from a www-url-encoded query string
   * @param urlEncodedQuery
   * @return
   */
  def reviewFromString(urlEncodedQuery: String): Option[MovieReview] =
    reviewFromMap(parseQuery(urlEncodedQuery))


}
