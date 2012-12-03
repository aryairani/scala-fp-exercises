object phonenumbers {
  sealed trait Context
  case object Home extends Context
  case object Mobile extends Context
  case object Business extends Context
  type Phone = String
  type PhoneEntry = (Context,Phone)
  type PhoneDB = List[PhoneEntry]

  trait Lookup {
    /**
     * Return one Home phone if known, otherwise one Mobile phone if known
     * @param db
     * @return
     */
    def onePersonalPhone(db: PhoneDB): Option[Phone]

    /**
     * Return all Home phones followed by all Mobile phones
     * @param db
     * @return
     */
    def allPersonalPhones(db: PhoneDB): List[Phone]

    /**
     * Return one Business phone if known, otherwise one Mobile phone if known
     * @param db
     * @return
     */
    def oneBusinessPhone(db: PhoneDB): Option[Phone]

    /**
     * Return all Business phones followed by all Mobile phones
     * @param db
     * @return
     */
    def allBusinessPhones(db: PhoneDB): List[Phone]

  }

  val Solution: Lookup = sys.error("todo") // new Lookup { ... }

}
