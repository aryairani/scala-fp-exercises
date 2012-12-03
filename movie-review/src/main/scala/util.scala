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

    /*
      1. Split the url-encoded string at '&' boundaries, yielding an Array[String]
      2. Eliminate any empty strings
      3. Use the splitPair function to split pieces at '=' boundary (if present),
           yielding a (String, Option[String]) tuple.
      4. Use the decodePair function to URL-decode each key and value
     */
    val decodedPairs = (
      s split '&'
        filter { ! _.isEmpty }
        map splitPair
        map decodePair
      )

    // Return a Map of the decoded pairs
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
