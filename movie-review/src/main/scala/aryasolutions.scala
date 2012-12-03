object aryasolutions {
  import moviereview._

  /*
    Basic basic.
    Looks up all params, even if an early one fails.
    Parameters are named / referenced 5 times.
    Failure value is specified once.
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
    Nested ifs avoid unnecessary calls to query.get
    Parameters are named / referenced 5 times.
    Failure value is specified 3 times.
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
    Avoids unnecessary calls to query.get
    Parameters are named / referenced 3 times.
    Failure value is specified 3 times.
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
    Looks up all params, even if an early one fails.
    Parameters are named / referenced 5 times.
    Failure value is specified 1 time.
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
    Looks up all params, even if an early one fails.
    Parameters are named / referenced 3 times.
    Failure value is specified 1 time.
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
    Avoids unnecessary calls to query.get
    Parameters are named / referenced 3 times.
    Failure value is defined by Option monad.
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
    Looks up all params, even if an early one fails.
    Parameters are named / referenced once.
    Failure value is implied.
   */
  def liftReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    // Apply[Option].lift3 converts (String,String,String) => MovieReview
    // to one that takes three Option[Strings] and returns an Option[MovieReview]
    val lifted = Apply[Option].lift3(makeMovieReview)

    lifted(get("title"), get("user"), get("review"))
  }

  def liftReview2(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    // There's an implicit version that works if the target type is known,
    //   but this was even more typing than the previous one.
    val lifted: (Option[String],Option[String],Option[String]) => Option[MovieReview] =
      makeMovieReview

    lifted(get("title"), get("user"), get("review"))
  }


  /*
    Apply3 does a lift3 and then applies it.
    Lazily avoids unnecessary calls to query.get
   */
  def applyReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    Apply[Option].apply3(get("title"), get("user"), get("review"))(makeMovieReview)
  }

  /*
    Apply syntax.  Params come before function, to tell the type inferencer which Monad to lift into.
    Parameters are named / referenced once.
    Failure value is implied.
    Lazily avoids unnecessary calls to query.get
   */
  def applyReview2(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    ^^(get("title"), get("user"), get("review"))(makeMovieReview)
  }

  /*
    ApplicativeBuilder syntax.  No need to counts args or ^^^s
    Parameters are named / referenced once.
    Failure value is implied.
    Looks up all params, even if an early one fails.
   */
  def applicativeBuilderReview(query: Map[String, Option[String]]): Option[MovieReview] = {
    def get(s:String) = lookup(s, query)

    get("title") |@| get("user") |@| get("review") apply makeMovieReview
  }

}
