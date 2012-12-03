object aryasolutions {
  import phonenumbers._

  val v0 = new Lookup {
    
  }

  def lookup[K,V](k: K, l: List[(K,V)]): Option[V] = l find (_._1 == k) map (_._2)
  def contextIs(c: Context): PhoneEntry => Boolean = _._1 == c
  def phoneFromEntry: PhoneEntry => Phone = _._2

  /*

   */
  val v1 = new Lookup {
    def onePersonalPhone(db: PhoneDB) =
      lookup(Home, db) orElse
        lookup(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookup(Business, db) orElse lookup(Mobile, db)

    def allPersonalPhones(db: PhoneDB) =
      (db filter contextIs(Home)) ++ (db filter contextIs(Mobile)) map phoneFromEntry

    def allBusinessPhones(db: PhoneDB) =
      (db filter contextIs(Business)) ++ (db filter contextIs(Mobile)) map phoneFromEntry
  }

  import scalaz._
  import scalaz.syntax.all._
  import scalaz.std.list._
  import scalaz.std.option._

  val v2 = new Lookup {

    def onePersonalPhone(db: PhoneDB) = lookup(Home, db) <+> lookup(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookup(Business, db) <+> lookup(Mobile, db)

    def allBusinessPhones(db: PhoneDB) =
      (db filter contextIs(Business)) <+> (db filter contextIs(Mobile)) map phoneFromEntry

    def allPersonalPhones(db: PhoneDB) =
      (db filter contextIs(Home)) <+> (db filter contextIs(Mobile)) map phoneFromEntry
  }

  def lookupM[M[_]:MonadPlus,K,V](k: K, l: List[(K,V)]): M[V] = (k,l) match {
    case (_, Nil) => MonadPlus[M].empty
    case (k, (kk,vv) :: rest) =>
      if (k == kk)
        vv.point[M] <+> lookupM[M,K,V](k, rest)
      else
        lookupM[M,K,V](k,rest)
  }

  def lookupMv2[M[_]:MonadPlus,K,V](k: K, l: List[(K,V)]): M[V] =
    l.foldLeft(PlusEmpty[M].empty[V]) {
      case (acc,(kk,vv)) => if (k == kk) acc <+> vv.point[M] else acc
    }

  val v3 = new Lookup {
    def lookupOption[K,V] = lookupM[Option,K,V] _
    def lookupList[K,V] = lookupM[List,K,V] _

    def onePersonalPhone(db: PhoneDB) = lookupOption(Home, db) <+> lookupOption(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookupOption(Business, db) <+> lookupOption(Mobile, db)

    def allPersonalPhones(db: PhoneDB) = lookupList(Home, db) <+> lookupList(Mobile,db)

    def allBusinessPhones(db: PhoneDB) = lookupList(Business, db) <+> lookupList(Mobile,db)
  }

  val v4 = new Lookup {

    // won't compile if not private?
    private def lookup2[M[_]:MonadPlus](c: Context, db: PhoneDB): M[Phone] =
      lookupM[M,Context,Phone](c, db)

    private def blahAndMobile[M[_]:MonadPlus](x: Context, db: PhoneDB): M[Phone] =
      lookup2[M](x, db) <+> lookup2[M](Mobile, db)

    /*
    private def lookup3[N[_]:MonadPlus] = lookupM[N,Context,Phone] _

    private def blahAndMobile[M[_]:MonadPlus](x: Context, db: PhoneDB): M[Phone] =
      lookup3[M].apply(x, db) <+> lookup3[M].apply(Mobile,db)
    */

    def onePersonalPhone(db: PhoneDB) = blahAndMobile[Option](Home, db)

    def oneBusinessPhone(db: PhoneDB) = blahAndMobile[Option](Business, db)

    def allPersonalPhones(db: PhoneDB) = blahAndMobile[List](Home, db)

    def allBusinessPhones(db: PhoneDB) = blahAndMobile[List](Business, db)
  }

}