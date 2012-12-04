package moviereview

object aryasolutions {
  import moviereview._

  /*
    Basic basic.
    Short-circuits on failure:      No
    Parameters named / referenced:  5 x N
    Parameters appear in test:      2 x N
    Failure value is specified:     1
  */
  def simpleReview1(query: Map[String,Option[String]]) : Option[MovieReview] = {
    val titleMaybe = query.get("title") // 1, 2
    val userMaybe = query.get("user")
    val reviewMaybe = query.get("review")

    if (titleMaybe.isDefined  &&  titleMaybe.get.isDefined  &&  // 3, 4
      userMaybe.isDefined   &&  userMaybe.get.isDefined   &&
      reviewMaybe.isDefined &&  reviewMaybe.get.isDefined)
    {
      Some(MovieReview(titleMaybe.get.get, userMaybe.get.get, reviewMaybe.get.get)) // 5
    }
    else
      None
  }

  /*
    Nested ifs
    Short-circuits on failure:      Yes
    Parameters named / referenced:  5 x N
    Parameters appear in test:      2 x N
    Failure value is specified:     1 x N
   */
  def simpleReview2(query: Map[String,Option[String]]) : Option[MovieReview] = {
    val titleMaybe = query.get("title")
    if (titleMaybe.isDefined && titleMaybe.get.isDefined) {
      val userMaybe = query.get("user")
      if (userMaybe.isDefined && userMaybe.get.isDefined) {
        val reviewMaybe = query.get("review")
        if (reviewMaybe.isDefined && reviewMaybe.get.isDefined)
          Some(MovieReview(titleMaybe.get.get, userMaybe.get.get, reviewMaybe.get.get))
        else None
      }
      else None
    }
    else None
  }

  /*
    Nested case statements.
    Short-circuits on failure:      Yes
    Parameters named / referenced:  3 x N
    Parameters appear in test:      1 x N
    Failure value is specified:     1 x N
   */
  def simpleReview3(query: Map[String,Option[String]]) : Option[MovieReview] = {
    query.get("title") match {
      case Some(Some(title)) =>
        query.get("user") match {
          case Some(Some(user)) =>
            query.get("review") match {
              case Some(Some(review)) =>
                Some(MovieReview(title, user, review))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /*
    Single case statement.
    Short-circuits on failure:      No
    Parameters named / referenced:  5 x N
    Parameters appear in test:      1 x N
    Failure value is specified:     1 (total)
   */
  def maybeReview(query: Map[String,Option[String]]) : Option[MovieReview] = {
    val titleMaybe = query.get("title")
    val userMaybe = query.get("user")
    val reviewMaybe = query.get("review")
    (titleMaybe, userMaybe, reviewMaybe) match {
      case (
        Some(Some(title)),
        Some(Some(user)),
        Some(Some(review))
        ) => Some(MovieReview(title, user, review))
      case _ => None
    }
  }

  /*
    Single case statement, without temp variables.
    Short-circuits on failure:      No
    Parameters named / referenced:  3 x N
    Parameters appear in test:      1 x N
    Failure value is specified:     1 (total)
   */
  def maybeReview2(query: Map[String,Option[String]]) : Option[MovieReview] = {
    def get = (s:String) => query.get(s)

    (get("title"), get("user"), get("review")) match {
      case (
        Some(Some(title)),
        Some(Some(user)),
        Some(Some(review))
        ) => Some(MovieReview(title, user, review))
      case _ => None
    }
  }

  /**
   * Helper to flatten the Option[ Option[String] ] that comes out of Map.get
   */
  def lookup(key: String, query: Map[String,Option[String]]): Option[String] =
    query.get(key) match {
      case Some(Some(x)) => Some(x)
      case _ => None
    }

  /**
   * Alternative implementation of lookup()
   */
  def lookup2(key: String, query: Map[String,Option[String]]): Option[String] =
    query.get(key).map(_.get)

  /* Again using .flatten method in Scala 2.10
  def lookup3(key: String, query: Map[String,Option[String]]): Option[String] =
    query.get(key).flatten
  */

  /*
    For comprehension.

    Short-circuits on failure:      Yes
    Parameters named / referenced:  3 x N
    Parameters appear in test:      0  (defined by Monad)
    Failure value is specified:     0  (defined by Monad)
   */
  def maybeReview3(query: Map[String,Option[String]]): Option[MovieReview] = {
    for {
      title <- lookup("title", query)
      user <- lookup("user", query)
      review <- lookup("review", query)
    } yield MovieReview(title, user, review)
  }

  /*
    Minor variation of previous.
   */
  def maybeReview4(query: Map[String,Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)
    for {
      title <- get("title")
      user <- get("user")
      review <- get("review")
    } yield MovieReview(title, user, review)
  }

  /*
    Reference to MovieReview creation function; used below
   */
  val makeMovieReview: (String,String,String) => MovieReview =
    MovieReview.apply _

  /*
    Lots of advanced FP functionality.
   */
  import scalaz._
  import Scalaz._

  /*
    Lift `makeMovieReview` function into the Option monad, and call the lifted function instead.

    Short-circuits on failure:      No
    Parameters named / referenced:  1 x N
    Parameters appear in test:      0  (defined by Monad)
    Failure value is specified:     0  (defined by Monad)
   */
  def liftReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    // Apply[Option].lift3 converts (String,String,String) => MovieReview
    // to one that takes three Option[Strings] and returns an Option[MovieReview]
    val lifted = Apply[Option].lift3(makeMovieReview)

    lifted(get("title"), get("user"), get("review"))
  }


  /*
    Apply3 does a lift3 and then applies it.

    Short-circuits on failure:      Yes
    Parameters named / referenced:  1 x N
    Parameters appear in test:      0  (defined by Monad)
    Failure value is specified:     0  (defined by Monad)
   */
  def applyReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    Apply[Option].apply3(get("title"), get("user"), get("review"))(makeMovieReview)
  }

  /*
    Apply syntax.  Params come before function, to tell the type inferencer which Monad to lift into.

    Short-circuits on failure:      Yes
    Parameters named / referenced:  1 x N
    Parameters appear in test:      0  (defined by Monad)
    Failure value is specified:     0  (defined by Monad)
   */
  def applyReview2(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    ^^(get("title"), get("user"), get("review"))(makeMovieReview)
  }

  /*
    ApplicativeBuilder syntax.  No need to counts args or ^^^s

    Short-circuits on failure:      No
    Parameters named / referenced:  1 x N
    Parameters appear in test:      0  (defined by Monad)
    Failure value is specified:     0  (defined by Monad)
   */
  def applicativeBuilderReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    get("title") |@| get("user") |@| get("review") apply makeMovieReview
  }

}
