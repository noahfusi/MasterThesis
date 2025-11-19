import java.io.ByteArrayInputStream
import org.scalatest.funsuite.AnyFunSuite

class NospressoSpec extends AnyFunSuite {

  /** Simule l'entrée utilisateur */
  def withInput[T](input: String)(body: => T): T = {
    val in = new ByteArrayInputStream(input.getBytes())
    val oldIn = System.in
    System.setIn(in)
    try body
    finally System.setIn(oldIn)
  }

  // -------------------------------------------------------------------
  // validatePin (tests basés sur le nombre de tentatives et le résultat)
  // -------------------------------------------------------------------

  test("validatePin – code correct retourne true") {
    val pins = Array("434343")

    val result = withInput("434343\n") {
      Nospresso.validatePin(0, pins)
    }

    assert(result)
  }

  test("validatePin – code incorrect retourne false dès la première tentative") {
    val pins = Array("434343")

    val result = withInput("111111\n") {
      Nospresso.validatePin(0, pins)
    }

    assert(!result)
  }

  test("validatePin – retourne false après 3 tentatives échouées") {
    val pins = Array("434343")

    val input =
      """111111
        |222222
        |333333
        |""".stripMargin

    val result = withInput(input) {
      Nospresso.validatePin(0, pins)
    }

    assert(!result)
  }

  // -------------------------------------------------------------------
  // updatePin (tests basés sur la mise à jour réelle du tableau)
  // -------------------------------------------------------------------

  test("updatePin – met à jour le PIN correctement") {
    val pins = Array("434343")

    withInput("123456\n") {
      Nospresso.updatePin(0, pins)
    }

    assert(pins(0) == "123456")
  }

  test("updatePin – n'accepte que les PIN de 6 chiffres") {
    val pins = Array("434343")

    val input =
      """1234
        |12
        |abc123
        |123456
        |""".stripMargin

    withInput(input) {
      Nospresso.updatePin(0, pins)
    }

    assert(pins(0) == "123456")  // seul format valide
  }

  // -------------------------------------------------------------------
  // restockMachine (tests basés sur l’effet de bord sur les tableaux)
  // -------------------------------------------------------------------

  test("restockMachine – augmente les stocks") {
    val coffee = Array(30)
    val sugar  = Array(15)
    val milk   = Array(200)

    val input =
      """100
        |200
        |500
        |""".stripMargin

    withInput(input) {
      Nospresso.restockMachine(0, coffee, sugar, milk)
    }

    assert(coffee(0) == 130)
    assert(sugar(0) == 215)
    assert(milk(0) == 700)
  }

  // -------------------------------------------------------------------
  // serveClient (tests basés sur le retour booléen et la mutation stock)
  // -------------------------------------------------------------------

  test("serveClient – transaction réussie si stock suffisant") {
    val coffee = Array(100)
    val sugar  = Array(100)
    val milk   = Array(100)

    // Hypothèse : méthode demande choix + quantités + paiement
    val input =
      """1
        |1
        |1
        |ok
        |""".stripMargin

    val result = withInput(input) {
      Nospresso.serveClient(0, coffee, sugar, milk)
    }

    assert(result)
    assert(coffee(0) < 100)
    assert(sugar(0) < 100)
    assert(milk(0) < 100)
  }

  test("serveClient – échoue si stock insuffisant") {
    val coffee = Array(0)   // rupture
    val sugar  = Array(100)
    val milk   = Array(100)

    val input =
      """1
        |1
        |1
        |ok
        |""".stripMargin

    val result = withInput(input) {
      Nospresso.serveClient(0, coffee, sugar, milk)
    }

    assert(!result)
    assert(coffee(0) == 0) // les stocks ne changent pas
  }

}
