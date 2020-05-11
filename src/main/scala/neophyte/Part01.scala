package neophyte.part01

/**
 * Case classes are special because Scala automatically creates a companion object for them: a
 * singleton object that contains not only an apply method for creating new instances of the case class,
 * but also an unapply method – the method that needs to be implemented by an object in order for it
 * to be an extractor.
 */
object Part01 {

  case class EMailAddress(firstName: String, lastName: String, score: Int)

  trait User {
    val name: String
    val score: Int
  }

  class FreeUser(val name: String, val score: Int, val upgradeProbability: Double) extends User

  class PremiumUser(val name: String, val score: Int) extends User

  object FreeUser {
    val upgradeProbablityThreshold = 0.75
    // The method expects some object of type S and returns an Option of type TupleN , where N is the
    // number of parameters to extract
    def unapply(u: FreeUser) = Some((u.name, u.score, u.upgradeProbability))
  }
  // it is not necessary for an extractor to reside in the companion object of the class for which it is applicable
  object PremiumCandidate {
    def unapply(u: FreeUser): Boolean = u.upgradeProbability > FreeUser.upgradeProbablityThreshold
  }

  object PremiumUser {
    def unapply(u: PremiumUser) = Some((u.name, u.score))
  }



  def advance(emails: List[EMailAddress]): Int = {
    require(emails.size >= 2)

    emails match {
      case EMailAddress(_, _, score0) :: EMailAddress(_, _, score1) :: _ => score0 + score1
      case _ => 0
    }
  }

  def greeting(u: User): String = u match {
    case FreeUser(name, _, _) => s"Hello ${name}"
    case PremiumUser(name, _) => s"Welcome back ${name}"
  }

  def greeting2(u: User): String = u match {
    case FreeUser(name, _, p) if (p > FreeUser.upgradeProbablityThreshold) => s"Hello ${name}, what can I do for you today?"
    case FreeUser(name, _, _) => s"Hello ${name}"
    case PremiumUser(name, _) => s"Welcome back ${name}"
  }

  def greeting3(u: User) = u match {
    case freeUser @ PremiumCandidate() => s"Hello ${freeUser.name}, what can I do for you today? How about an upgrade?"
    case _ => s"Hello you"
  }

  /**
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part01.getClass.getName.split('$').head}")

    val emails = List(EMailAddress("Joe", "Doe", 40), EMailAddress("Roland", "Tritsch", 2))
    println(s"Score: ${advance(emails)}") // Score: 42

    val puser = new PremiumUser("Daniel", 42)
    println(s"Greeting: ${greeting(puser)}") // Greeting: Welcome back Daniel

    val fuser = new FreeUser("Sam", 2000, 0.8)
    println(s"Greeting: ${greeting2(fuser)}") // Greeting: Hello Sam, what can I do for you today?
    println(s"Greeting: ${greeting3(fuser)}") // Greeting: Hello Sam, what can I do for you today? How about an upgrade
  }

}
