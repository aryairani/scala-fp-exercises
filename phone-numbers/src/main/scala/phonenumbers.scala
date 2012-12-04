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
     * Return the first Home phone if known, otherwise the first Mobile phone if known
     * @param db
     * @return
     */
    def onePersonalPhone(db: PhoneDB): Option[Phone]

    /**
     * Return all Home phones in their existing order, followed by all Mobile phones in their existing order
     * @param db
     * @return
     */
    def allPersonalPhones(db: PhoneDB): List[Phone]

    /**
     * Return the first Business phone if known, otherwise the first Mobile phone if known
     * @param db
     * @return
     */
    def oneBusinessPhone(db: PhoneDB): Option[Phone]

    /**
     * Return all Business phones in their existing order, followed by all Mobile phones in their existing order
     * @param db
     * @return
     */
    def allBusinessPhones(db: PhoneDB): List[Phone]

  }

  val Solution: Lookup = sys.error("todo") // new Lookup { ... }

}
