/**
 * Created with IntelliJ IDEA.
 * User: arya
 * Date: 12/2/12
 * Time: 3:30 AM
 * To change this template use File | Settings | File Templates.
 */
class PhoneSpec extends org.scalatest.FeatureSpec {
  import phonenumbers._

  val albulena = List((Home, "+355-652-55512"))

  val nils = List(
    (Mobile, "+47-922-55-512"),
    (Business, "+47-922-12-121"),
    (Home, "+47-925-55-121"),
    (Business, "+47-922-25-551")
  )

  val twalumba = List((Business, "+260-02-55-5121"))


  scenario("Look up a single personal phone") {
    assert(Solution.onePersonalPhone(albulena) === Some("+355-652-55512"))
    assert(Solution.onePersonalPhone(nils) === Some("+47-925-55-121"))
    assert(Solution.onePersonalPhone(twalumba) === None)
  }

  scenario("Look up a single work phone") {
    assert(Solution.oneBusinessPhone(albulena) === None)
    assert(Solution.oneBusinessPhone(nils) === Some("+47-922-12-121"))
    assert(Solution.oneBusinessPhone(twalumba) === Some("+260-02-55-5121"))
  }

  scenario("Look up all personal phones") {
    assert(Solution.allPersonalPhones(albulena) === List("+355-652-55512"))
    assert(Solution.allPersonalPhones(nils) === List("+47-925-55-121", "+47-922-55-512"))
    assert(Solution.allPersonalPhones(twalumba) === Nil)
  }

  scenario("Look up all work phones") {
    assert(Solution.allBusinessPhones(albulena) === Nil)
    assert(Solution.allBusinessPhones(nils) === List("+47-922-12-121", "+47-922-25-551", "+47-922-55-512"))
    assert(Solution.allBusinessPhones(twalumba) === List("+260-02-55-5121"))
  }

}
