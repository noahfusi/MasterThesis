object Main {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn
    import scala.util.Random


    var coffeeStock = 50
    var sugarStock = 30
    var milkStock = 500

    val adminPIN = "434343"


    def displayMainMenu(): Unit = {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
    }


    def getValidInput(prompt: String, validOptions: List[String]): String = {
      var input = ""
      while (!validOptions.contains(input)) {
        println(prompt)
        input = StdIn.readLine()
      }
      input
    }

    // Mode Client
    def clientMode(): Unit = {
      val drink = getValidInput(
        "Veuillez sélectionner votre boisson :\n1) Expresso\n2) Cappuccino\n3) Latte",
        List("1", "2", "3")
      )
      val sugar = getValidInput(
        "Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)",
        List("1", "2", "3", "4")
      )
      val milk = if (drink != "1") {
        getValidInput(
          "Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non",
          List("1", "2")
        )
      } else "2"


      val (coffeeNeeded, milkNeeded, basePrice) = drink match {
        case "1" => (8, 0, 2.00) // Expresso
        case "2" => (6, 100, 2.50) // Cappuccino
        case "3" => (6, 120, 2.70) // Latte Petit
      }
      val sugarNeeded = sugar match {
        case "2" => 5
        case "3" => 10
        case "4" => 15
        case _   => 0
      }
      val milkExtra = if (milk == "1") 50 else 0
      val milkTotal = milkNeeded + milkExtra
      val sugarPrice = sugarNeeded * 0.02
      val totalPrice = basePrice + sugarPrice + (milkExtra * 0.05)


      if (coffeeStock < coffeeNeeded) {
        println("Erreur : Quantité de poudre de café insuffisante.")
      } else if (milkStock < milkTotal) {
        println("Erreur : Quantité de lait insuffisante.")
      } else if (sugarStock < sugarNeeded) {
        println("Erreur : Quantité de sucre insuffisante.")
      } else {

        coffeeStock -= coffeeNeeded
        milkStock -= milkTotal
        sugarStock -= sugarNeeded


        val paymentCode = Random.alphanumeric.take(5).mkString
        println(f"Veuillez payer en utilisant Twint. Votre code de paiement est : $paymentCode")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")


        println("Préparation de votre boisson...")
        Thread.sleep(2000)
        println("Votre boisson est prête ! Bonne dégustation !")
      }
    }

    // Mode Admin
    def adminMode(): Unit = {
      val pin = getValidInput("Entrez le code PIN : ", List(adminPIN))
      if (pin == adminPIN) {
        println(f"Stocks actuels:\nPoudre de café: $coffeeStock g\nSucre: $sugarStock g\nLait: $milkStock ml")
        println("Réapprovisionnement :")
        println("Quantité de poudre de café à ajouter :")
        coffeeStock += StdIn.readInt()
        println("Quantité de sucre à ajouter :")
        sugarStock += StdIn.readInt()
        println("Quantité de lait à ajouter (en ml) :")
        milkStock += StdIn.readInt()
        println("Niveaux de stock mis à jour.")
      } else {
        println("Code PIN incorrect.")
      }
    }


    var running = true
    while (running) {
      displayMainMenu()
      val choice = getValidInput(">", List("1", "2", "3"))
      choice match {
        case "1" => clientMode()
        case "2" => adminMode()
        case "3" =>
          println("Au revoir!")
          running = false
      }
    }
  }
}