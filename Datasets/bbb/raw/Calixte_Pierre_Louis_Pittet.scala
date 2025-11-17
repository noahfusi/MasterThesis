
import scala.io.StdIn
import scala.util.Random

object Machine {

  val NUMBER_OF_MACHINES = 5
  val MAX_PIN_ATTEMPTS = 3
  val DEFAULT_PIN = "434343"


  def initializeArrays(): (Array[String], Array[Double], Array[Double], Array[Double]) = {
    val machinePins = Array.fill(NUMBER_OF_MACHINES)(DEFAULT_PIN)
    val cafeStocks = Array.fill(NUMBER_OF_MACHINES)(50.0)
    val sucreStocks = Array.fill(NUMBER_OF_MACHINES)(30.0)
    val laitStocks = Array.fill(NUMBER_OF_MACHINES)(500.0)
    (machinePins, cafeStocks, sucreStocks, laitStocks)
  }

  // Valider PIN pour machine

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var remainingAttempts = MAX_PIN_ATTEMPTS
    while (remainingAttempts > 0) {
      println("Entrer le code PIN :")
      val enteredPin = StdIn.readLine()

      if (enteredPin == machinePins(machineId)) {
        println(s"Accès accordé à la machine " + (machineId+1) + ".")
        return true
      }

      remainingAttempts -= 1
      if (remainingAttempts > 0) {
        println(s"Code PIN incorrect. $remainingAttempts tentatives restantes.")
      }
    }

    println("Trop de tentatives échouées. Fin du programme")
    false
  }

  // Methode pour mettre à jour le PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    do {
      println("Entrer un nouveau code PIN a 6 chiffres ")
      newPin = StdIn.readLine()
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))

    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // Methode ingrédient recquis pour boisson
  def getIngredientRequirements(beverage: String, size: String): (Double, Double, Double) = {
    if (beverage == "Espresso") {
      (8.0, 0.0, 0.0)
    } else if (beverage == "Cappucino") {
      (6.0, 100.0, 10.0)
    } else if (beverage == "Latte") {
      if (size == "Petit") {
        (6.0, 120.0, 5.0)
      } else if (size == "Moyen") {
        (8.0, 150.0, 10.0)
      } else if (size == "Grand") {
        (12.0, 200.0, 15.0)
      } else {
        (0.0, 0.0, 0.0)
      }
    } else {
      (0.0, 0.0, 0.0)
    }
  }


  // Methode prix pour une boisson
  def getBasePrice(beverage: String, size: String): Double = {
    if (beverage == "Espresso") {
      2.00
    } else if (beverage == "Cappucino") {
      2.50
    } else if (beverage == "Latte") {
      if (size == "Petit") {
        2.70
      } else if (size == "Moyen") {
        3.20
      } else if (size == "Grand") {
        3.70
      } else {
        0.0
      }
    } else {
      0.0
    }
  }


  // Methode pour prix sucre
  def getSugarPrice(sugarAmount: Int): Double = {
    if (sugarAmount == 0) {
      0.00
    } else if (sugarAmount == 5) {
      0.10
    } else if (sugarAmount == 10) {
      0.20
    } else if (sugarAmount == 15) {
      0.30
    } else {
      0.00
    }
  }


  // Methode pour niveau du sucre
  def getSugarLevel(sugarAmount: Int): String = {
    if (sugarAmount == 0) {
      "Sans sucre"
    } else if (sugarAmount == 5) {
      "Peu (5g)"
    } else if (sugarAmount == 10) {
      "Moyen (10g)"
    } else if (sugarAmount == 15) {
      "Beaucoup (15g)"
    } else {
      "Inconnu"
    }
  }


  // Methode processus du paiement
  def processPayment(amount: Double): Unit = {
    val code = Random.alphanumeric.take(5).mkString
    println(s"\n Veuillez utiliser Twint.")
    println(s"Votre code de paiement est: $code")
    println("(En attente de paiement...)")
    Thread.sleep(2000)
    println("Paiement reçu!")
  }


  def serveClient(machineId: Int,
                  coffeeStocks: Array[Double],
                  sugarStocks: Array[Double],
                  milkStocks: Array[Double]): Boolean = {
    println("Veuillez sélectionner votre boisson: ")
    println("1. Espresso - CHF 2.00")
    println("2. Cappuccino - CHF 2.50")
    println("3. Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val beverageChoice = StdIn.readLine().toInt

    var beverage =""
    var size = ""
    var defaultMilk = 0.0
    if (beverageChoice == 1) {
      println("Espresso", "Standard", 0.0)
      beverage = "Espresso"
    }else if(beverageChoice == 2){
      println("Cappucino", "Standard", 100)
      beverage = "Cappucino"
    }else if (beverageChoice == 3) {
      beverage = "Latte"
      println("\nChoisissez votre taille:")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")

      var choixsize = StdIn.readInt().toInt
        if (choixsize==1) {
          defaultMilk = 120.0
          size = "Petit"
        } else if (choixsize == 2) {
          defaultMilk = 150.0
          size = "Moyen"
        } else if (choixsize == 3) {
          defaultMilk = 200.0
          size = "Grand"
        }else {
          return false
        }
    }



    val (requiredCoffee, requiredMilk, requiredSugar) = getIngredientRequirements(beverage, size)

    // Check stock disponible
    if (coffeeStocks(machineId) < requiredCoffee) {
      println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      return false
    }
    if (milkStocks(machineId) < requiredMilk) {
      println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      return false
    }

    // Selection sucre
    println("\nSouhaitez-vous ajouter du sucre?")
    println("1) Sans sucre")
    println("2) Petit (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    val sugarChoice = StdIn.readLine()
    val sugarAmount =
      if (sugarChoice == "1") {
        0
      } else if (sugarChoice == "2") {
        5
      } else if (sugarChoice == "3") {
        10
      } else if (sugarChoice == "4") {
        15
      } else {
        return false
      }


    // Check disponibiité de sucre
    if (sugarStocks(machineId) < sugarAmount) {
      println("Erreur: Sucre en quantité insuffisante.")
      return false
    }

    // Extra milk option pour Cappuccino et Latte
    var extraMilkDoses = 0
    if (beverage != "Espresso") {
      println("\nSouhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")

      if (StdIn.readLine() == "1") {
        println("Combien de doses? (1-3)")
        extraMilkDoses = StdIn.readLine().toInt
        if (extraMilkDoses < 1 || extraMilkDoses > 3) return false

        if (milkStocks(machineId) < requiredMilk + (extraMilkDoses * 50)) {
          println("Erreur: Quantité de lait insuffisante pour une dose de lait supplémentaire.")
          return false
        }
      }
    }

    // Calcul prix total
    val basePrice = getBasePrice(beverage, size)
    val sugarPrice = getSugarPrice(sugarAmount)
    val extraMilkPrice = extraMilkDoses * 0.05
    val totalPrice = basePrice + sugarPrice + extraMilkPrice

    // résumé commande
    println(s"\nboisson sélectionnée: $beverage ${if(size != "Standard") s"$size" else ""}")
    println(s"Niveau de sucre: ${getSugarLevel(sugarAmount)}")
    if (extraMilkDoses > 0) println(s"lait supplémentaire: $extraMilkDoses dose(s)")
    println(f"Prix Total : CHF $basePrice%.2f + CHF $sugarPrice%.2f + CHF $extraMilkPrice%.2f = $totalPrice%.2f")

    // Processus de paiement
    processPayment(totalPrice)

    // MAJ stocks
    coffeeStocks(machineId) -= requiredCoffee
    milkStocks(machineId) -= (requiredMilk + (extraMilkDoses * 50))
    sugarStocks(machineId) -= sugarAmount

    // Simule preparation de la boisson
    println(s"\nPreparation de votre boisson...")
    Thread.sleep(2000)
    println("Votre boisson est prête ! Bonne dégustation !\n")

    true
  }

  // Montrer stock
  def restockMachine(machineId: Int,
                     coffeeStocks: Array[Double],
                     sugarStocks: Array[Double],
                     milkStocks: Array[Double]): Unit = {
    println("Niveaux de stock actuels :")
    println(f"Poudre de café: ${coffeeStocks(machineId)}%.1fg")
    println(f"Sucre: ${sugarStocks(machineId)}%.1fg")
    println(f"Lait: ${milkStocks(machineId)/1000}%.1fL")

    println("Entrer les quantités à ajouter :")
    print("Poudre de café  > ")
    val coffeeAdd = StdIn.readDouble()
    print("Sucre  > ")
    val sugarAdd = StdIn.readDouble()
    print("Lait  > ")
    val milkAdd = StdIn.readDouble()

    // ajouter stocks
    coffeeStocks(machineId) += coffeeAdd
    sugarStocks(machineId) += sugarAdd
    milkStocks(machineId) += (milkAdd * 1000)  // Convert liters to ml

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // méthode management machine café
  def main(args: Array[String]): Unit = {

    val (machinePins, coffeeStocks, sugarStocks, milkStocks) = initializeArrays()

    while (true) {
      println("Selectionner une machine (1-5):")
      val machineId = StdIn.readInt() - 1

      if (machineId < 0 || machineId >= NUMBER_OF_MACHINES) {
        println("Machine invalide.")
        return
      }


        println("Veuillez sélectionner votre mode:")
        println("1. Mode client")
        println("2. Mode admin")
        println("3. Quitter")
        val mode = StdIn.readInt()

        if (mode == 1) {
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } else if (mode == 2) {
          if (validatePin(machineId, machinePins)) {
          println("1. Restock")
          println("2. Mise à jour du PIN")
          val adminOption = StdIn.readInt()

          if (adminOption == 1) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else if (adminOption == 2) {
            updatePin(machineId, machinePins)
          } else {
            println("Option invalide")
          }
          }
        } else if (mode == 3) {

        }else {
          println("Mode invalide")
        }

    }
  }
}