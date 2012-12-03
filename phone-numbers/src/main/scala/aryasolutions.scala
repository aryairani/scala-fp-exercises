object aryasolutions {
  import phonenumbers._

  /*
    Look up a value in an association list (alist)
   */
  def lookup[K,V](k: K, l: List[(K,V)]): Option[V] = (k,l) match {
    case (_, Nil) =>
      None
    case (key, (k,v)::rest) =>
      if (k == key)
        Some(v)
      else lookup(k, rest)
  }

  object v_1 extends Lookup {
    /*
      Look up first Home number, and if defined, use it; otherwise look up first mobile number, and use it.
     */
    def onePersonalPhone(db: phonenumbers.PhoneDB): Option[Phone] = {
      val homeOption = lookup(Home, db)
      if (homeOption.isDefined) homeOption
      else lookup(Mobile, db)
    }

    /*
      orElse lets you chain Options together
     */
    def oneBusinessPhone(db: phonenumbers.PhoneDB): Option[Phone] =
      lookup(Home, db) orElse lookup(Mobile, db)


    /*
      filter (higher-order function) and then map (hof) and then append
     */
    def allPersonalPhones(db: phonenumbers.PhoneDB): List[Phone] = {
      val homeNumberList = db.filter(contextIsHome).map(getNumber)
      val mobileNumberList = db.filter(contextIsMobile).map(getNumber)
      homeNumberList ++ mobileNumberList
    }
    def contextIsHome(e: PhoneEntry) = { e._1 == Home }
    def contextIsMobile(e: PhoneEntry) = { e._1 == Mobile }
    def contextIsBusiness(e: PhoneEntry) = { e._1 == Business }
    def getNumber(e: PhoneEntry) = e._2

    /*
      append and then map
     */
    def allBusinessPhones(db: phonenumbers.PhoneDB): List[Phone] =
      db.filter(contextIsBusiness) ++ db.filter(contextIsMobile) map (getNumber)
  }

  object v0 extends Lookup {
    def contextIsHome(e: PhoneEntry) = { e._1 == Home }
    def contextIsMobile(e: PhoneEntry) = { e._1 == Mobile }
    def contextIsBusiness(e: PhoneEntry) = { e._1 == Business }

    /**
     * Return one Home phone if known, otherwise one Mobile phone if known
     */
    def onePersonalPhone(db: phonenumbers.PhoneDB): Option[Phone] = {
      val homeOption = db.find(contextIsHome)
      if (homeOption.isDefined)
        Some(homeOption.get._2)
      else {
        val mobileOption = db.find(contextIsMobile)
        if (mobileOption.isDefined)
          Some(mobileOption.get._2)
        else None
      }
    }

    /**
     * Return one Business phone if known, otherwise one Mobile phone if known
     */
    def oneBusinessPhone(db: phonenumbers.PhoneDB): Option[Phone] = {
      val businessNumberOption = db.find(contextIsBusiness).map(getNumber)
      val mobileNumberOption = db.find(contextIsMobile).map(getNumber)
      businessNumberOption orElse mobileNumberOption
    }
    def getNumber(e: PhoneEntry) = e._2

    /**
     * Return all Home phones followed by all Mobile phones
     */
    def allPersonalPhones(db: phonenumbers.PhoneDB): List[Phone] = {
      val homeNumberList = db.filter(contextIsHome).map(getNumber)
      val mobileNumberList = db.filter(contextIsMobile).map(getNumber)
      homeNumberList ++ mobileNumberList
    }

    /**
     * Return all Business phones followed by all Mobile phones
     */
    def allBusinessPhones(db: phonenumbers.PhoneDB): List[Phone] = {
      val businessNumberList = db.filter(contextIsBusiness).map(getNumber)
      val mobileNumberList = db.filter(contextIsMobile).map(getNumber)
      businessNumberList ++ mobileNumberList
    }
  }

  /*
    a single contextIs(c) instead of individual contextIs_____ functions
    contextIs returns a function
   */
  def contextIs(c: Context) = { (e: PhoneEntry) => e._1 == c }
  import v0.getNumber

  object v12 extends Lookup {
    def onePersonalPhone(db: PhoneDB) =
      lookup(Home, db) orElse lookup(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) =
      lookup(Business, db) orElse lookup(Mobile, db)

    def allPersonalPhones(db: PhoneDB) =
      (db filter contextIs(Home)) ++ (db filter contextIs(Mobile)) map getNumber

    def allBusinessPhones(db: PhoneDB) =
      (db filter contextIs(Business)) ++ (db filter contextIs(Mobile)) map getNumber

  }

  /*
    Same as previous, but using `db.find` and `map`, instead of `lookup`
   */
  object v11 extends Lookup {
    def onePersonalPhone(db: PhoneDB) =
      db.find(contextIs(Home)) orElse db.find(contextIs(Mobile)) map getNumber

    def oneBusinessPhone(db: PhoneDB) =
      db.find(contextIs(Business)) orElse db.find(contextIs(Mobile)) map getNumber

    def allPersonalPhones(db: PhoneDB) =
      (db filter contextIs(Home)) ++ (db filter contextIs(Mobile)) map getNumber

    def allBusinessPhones(db: PhoneDB) =
      (db filter contextIs(Business)) ++ (db filter contextIs(Mobile)) map getNumber
  }

  import scalaz._
  import Scalaz._

  /*
    <+> defined in Plus, MonadPlus
   */
  object v2 extends Lookup {

    def onePersonalPhone(db: PhoneDB) = lookup(Home, db) <+> lookup(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookup(Business, db) <+> lookup(Mobile, db)

    def allBusinessPhones(db: PhoneDB) =
      (db filter contextIs(Business)) <+> (db filter contextIs(Mobile)) map getNumber

    def allPersonalPhones(db: PhoneDB) =
      (db filter contextIs(Home)) <+> (db filter contextIs(Mobile)) map getNumber
  }

  /*
    Can define a generic version of lookup which returns any desired Monad (supporting MonadPlus)
   */
  def lookupM[M[_]:MonadPlus,K,V](k: K, l: List[(K,V)]): M[V] = (k,l) match {
    case (_, Nil) => MonadPlus[M].empty
    case (k, (kk,vv) :: rest) =>
      if (k == kk)
        vv.point[M] <+> lookupM[M,K,V](k, rest)
      else
        lookupM[M,K,V](k,rest)
  }

  /*
    A different implementation of the same
   */
  def lookupM_2[M[_]:MonadPlus,K,V](k: K, l: List[(K,V)]): M[V] =
    l.foldLeft(MonadPlus[M].empty[V]) {
      case (acc,(kk,vv)) => if (k == kk) acc <+> vv.point[M] else acc
    }

  /*
    Can now use lookupM to generate specific lookups
   */
  object v3 extends Lookup {
    def lookupOption[K,V] = lookupM[Option,K,V] _
    def lookupList[K,V] = lookupM[List,K,V] _

    def onePersonalPhone(db: PhoneDB) = lookupOption(Home, db) <+> lookupOption(Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookupOption(Business, db) <+> lookupOption(Mobile, db)

    def allPersonalPhones(db: PhoneDB) = lookupList(Home, db) <+> lookupList(Mobile,db)

    def allBusinessPhones(db: PhoneDB) = lookupList(Business, db) <+> lookupList(Mobile,db)
  }


  /*
    Or you can generate the lookup right at the call site.
   */
  object v35 extends Lookup {

    def lookup[M[_]:MonadPlus](c: Context, db: PhoneDB) = lookupM[M,Context,Phone](c,db)

    def onePersonalPhone(db: PhoneDB) = lookup[Option](Home, db) <+> lookup[Option](Mobile, db)

    def oneBusinessPhone(db: PhoneDB) = lookup[Option](Business, db) <+> lookup[Option](Mobile, db)

    def allPersonalPhones(db: PhoneDB) = lookup[List](Home, db) <+> lookup[List](Mobile,db)

    def allBusinessPhones(db: PhoneDB) = lookup[List](Business, db) <+> lookup[List](Mobile,db)

  }

  /*
    Noting that each of these looks up numbers for a certain context, and then adds the Mobile numbers,
    can factor that out, if desired.
   */
  object v4 extends Lookup {

    def lookup[M[_]:MonadPlus](c: Context, db: PhoneDB) = lookupM[M,Context,Phone](c,db)

    def whateverPlusMobile[M[_]:MonadPlus](x: Context, db: PhoneDB) = lookup[M](x,db) <+> lookup[M](Mobile,db)

    def onePersonalPhone(db: PhoneDB) = whateverPlusMobile[Option](Home, db)

    def oneBusinessPhone(db: PhoneDB) = whateverPlusMobile[Option](Business, db)

    def allPersonalPhones(db: PhoneDB) = whateverPlusMobile[List](Home, db)

    def allBusinessPhones(db: PhoneDB) = whateverPlusMobile[List](Business, db)
  }

  /*
    Or cache the lookup function
   */
  object v41 extends Lookup {

    def whateverPlusMobile[M[_]:MonadPlus](x: Context, db: PhoneDB): M[Phone] = {
      val lookup = lookupM[M,Context,Phone] _
      lookup(x,db) <+> lookup(Mobile,db)
    }

    def onePersonalPhone(db: PhoneDB) = whateverPlusMobile[Option](Home, db)

    def oneBusinessPhone(db: PhoneDB) = whateverPlusMobile[Option](Business, db)

    def allPersonalPhones(db: PhoneDB) = whateverPlusMobile[List](Home, db)

    def allBusinessPhones(db: PhoneDB) = whateverPlusMobile[List](Business, db)
  }

}

