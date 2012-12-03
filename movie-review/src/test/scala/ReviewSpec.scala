import util._
import moviereview.MovieReview

/**
 * Created with IntelliJ IDEA.
 * User: arya
 * Date: 12/1/12
 * Time: 5:28 PM
 * To change this template use File | Settings | File Templates.
 */
class ReviewSpec extends org.scalatest.FeatureSpec {

  feature("Construct Movie Review from parsed query parameter map") {

    scenario("all query parameters are present") {
      assert(reviewFromString("title=The%20Collection&user=arya&review=It%20was%20terrible.") ===
        Some(MovieReview("The Collection","arya","It was terrible.")))
    }

    scenario("extra query parameters are present") {
      assert(reviewFromString("title=The%20Collection&rating=5.5&user=arya&review=It%20was%20terrible.") ===
        Some(MovieReview("The Collection","arya","It was terrible.")))
    }

    scenario("some query parameters are empty") {
      assert(reviewFromString("title&user=arya&review=It%20was%20terrible.") === None)
      assert(reviewFromString("title=The%20Collection&user&review=It%20was%20terrible.") === None)
      assert(reviewFromString("title=The%20Collection&user=arya&review") === None)
    }

    scenario("some query parameters are missing") {
      assert(reviewFromString("user=arya&review=It%20was%20terrible.") === None)
      assert(reviewFromString("title=The%20Collection&review=It%20was%20terrible.") === None)
      assert(reviewFromString("title=The%20Collection&user=arya") === None)
    }
  }
}

class ParserSpec extends org.scalatest.FeatureSpec {
  feature("Parses URL-encoded query parameters") {
    scenario("no parameters present") {
      assert(parseQuery("") === Map())
    }
    scenario("all parameters have values") {
      assert(parseQuery("title=T&user=U&review=Great") ===
        Map(
          "title" -> Some("T"),
          "user" -> Some("U"),
          "review" -> Some("Great")
        )
      )
    }
    scenario("some parameters have values")  {
      assert(parseQuery("title&user&review=Great") ===
        Map(
          "title" -> None,
          "user" -> None,
          "review" -> Some("Great")
        )
      )
    }
    scenario("no parameters have values")  {
      assert(parseQuery("a&b&c") ===
        Map(
          "a" -> None,
          "b" -> None,
          "c" -> None
        )
      )
    }
  }
}
