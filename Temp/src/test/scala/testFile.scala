import utest._
import java.io.ByteArrayInputStream

object NospressoTests extends TestSuite {

  /** Utilitaire d’injection d’entrée */
  def withInput[T](input: String)(body: => T): T = {
    val in = new ByteArrayInputStream(input.getBytes("UTF-8"))
    val old = System.in
    System.setIn(in)
    try body finally System.setIn(old)
  }

  val tests = Tests {

    // --------------------------------------------------------------------------
    // validatePin
    // --------------------------------------------------------------------------
    test("validatePin - correct PIN returns true") {
      val pins = Array("434343", "434343", "434343")
      val result = withInput("434343\n")(Nospresso.validatePin(0, pins))
      assert(result == true)
    }

    test("validatePin - incorrect PIN returns false immediately") {
      val pins = Array("434343")
      val result = withInput("123456\n")(Nospresso.validatePin(0, pins))
      assert(result == false)
    }

    test("validatePin - 3 failed attempts returns false") {
      val pins = Array("434343")
      val input = "111111\n222222\n333333\n"
      val result = withInput(input)(Nospresso.validatePin(0, pins))
      assert(result == false)
    }

    // --------------------------------------------------------------------------
    // updatePin
    // --------------------------------------------------------------------------
    test("updatePin - updates only if 6 digits") {
      val pins = Array("434343")
      val input =
        """123
          |12
          |abcd
          |123456
          |""".stripMargin

      withInput(input) {
        Nospresso.updatePin(0, pins)
      }

      assert(pins(0) == "123456")
    }

    // --------------------------------------------------------------------------
    // restockMachine
    // --------------------------------------------------------------------------
    test("restockMachine - increases stocks correctly") {
      val coffee = Array(30)
      val sugar  = Array(20)
      val milk   = Array(100)

      val input = "100\n200\n300\n"
      withInput(input) {
        Nospresso.restockMachine(0, coffee, sugar, milk)
      }

      assert(coffee(0) == 130)
      assert(sugar(0) == 220)
      assert(milk(0)  == 400)
    }

    // --------------------------------------------------------------------------
    // serveClient
    // --------------------------------------------------------------------------
    test("serveClient - returns true and deducts stocks when valid") {
      val coffee = Array(50)
      val sugar  = Array(50)
      val milk   = Array(50)

      val input =
        """1
          |1
          |ok
          |""".stripMargin

      val result = withInput(input) {
        Nospresso.serveClient(0, coffee, sugar, milk)
      }

      assert(result == true)
      assert(coffee(0) < 50)
    }

    test("serveClient - returns false if stock insufficient") {
      val coffee = Array(0)
      val sugar  = Array(50)
      val milk   = Array(50)

      val input =
        """1
          |1
          |ok
          |""".stripMargin

      val result = withInput(input) {
        Nospresso.serveClient(0, coffee, sugar, milk)
      }

      assert(result == false)
      assert(coffee(0) == 0) // pas de modification
    }
  }
}