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
