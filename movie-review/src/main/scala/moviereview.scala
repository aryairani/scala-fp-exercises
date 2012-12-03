/**
 * Created with IntelliJ IDEA.
 * User: arya
 * Date: 12/1/12
 * Time: 5:05 PM
 * To change this template use File | Settings | File Templates.
 */

object moviereview {

  /**
   * Creates a MovieReview instance out of a URL-decoded query parameter map, if possible.
   * If the "title", "user", or "review" values are missing or None, return None.
   *
   * It's a Map[ String, Option[String] ] to distinguish between parameters that have
   * an empty string value vs an absent string value.  ("path?a=&b=1" vs "path?a&b=1")
   *
   * Examples:
   *
   * reviewFromMap(
   *   Map(
   *     "title"->Some("T"),
   *     "user"->Some("U"),
   *     "review"->Some("R")
   *   )
   * ) == Some(MovieReview("T","U","R"))
   *
   * reviewFromMap(
   *   Map(
   *     "title"->"T",
   *     "user"->"U",
   *     "review"->None
   *   )
   * ) == None
   *
   * reviewFromMap(
   *   Map(
   *     "title"->"T",
   *     "user"->"U"
   *   )
   * ) == None
   */
  def reviewFromMap(query: Map[String,Option[String]]): Option[MovieReview] = {
    sys.error("todo")
  }

  // Self-explanatory
  case class MovieReview(title: String, user: String, review: String)
}

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