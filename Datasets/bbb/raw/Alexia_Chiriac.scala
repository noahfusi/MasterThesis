import scala.io.StdIn.readLine

object NospressoMultiMachine {
  val nbMachines = 5

  // Les stocks initiaux de chaque machine
  var coffeeStocks = Array(50, 50, 50, 50, 50) // Quantité café (g)
  var sugarStocks = Array(30, 30, 30, 30, 30)  // Quantité de sucre (g)
  var milkStocks = Array(500, 500, 500, 500, 500) // Quantité de lait (mL)
  var machinePins = Array("434343", "434343", "434343", "434343", "434343") // Code PIN

  // Vérifie le code PIN de la machine sélectionnée
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    for (attempt <- 1 to 3) {
      println("Entrez le code PIN :")
      if (readLine(" > ") == machinePins(machineId)) {
        println(s"Accès accordé à la Machine ${machineId + 1}.")
        return true
      }
      println(s"Code PIN incorrect. ${3 - attempt} tentatives restantes.")
    }
    println("Trop de tentatives échouées.")
    sys.exit(0)
  }

  // Met à jour le code PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    var newPin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres >")
      newPin = readLine(" > ")
    } while (!newPin.matches("\\d{6}"))
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // Renouvelle les stocks
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Entrez les quantités à ajouter :")

    println("Poudre de café >")
    coffeeStocks(machineId) += readLine(" > ").toInt

    println("Sucre >")
    sugarStocks(machineId) += readLine(" > ").toInt

    println("Lait >")
    milkStocks(machineId) += (readLine(" > ").toDouble * 1000).toInt

    println("Les stocks ont été mis à jour avec succès.")
  }

  // Traitement de la commande d'un client
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Sélectionnez votre boisson :")
    println("1) Expresso (8g de café)")
    println("2) Cappuccino (6g de café, 100ml de lait)")
    println("3) Latte (6g de café, 120ml de lait)")

    val choix = readLine(" > ").toInt
    choix match {
      case 1 =>
        if (coffeeStocks(machineId) >= 8) {
          coffeeStocks(machineId) -= 8
          println("Votre Expresso est prêt !")
        } else {
          println("Stock insuffisant de café.")
          return false
        }
      case 2 =>
        if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 100
          println("Votre Cappuccino est prêt !")
        } else {
          println("Stock insuffisant de café ou de lait.")
          return false
        }
      case 3 =>
        if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120) {
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 120
          println("Votre Latte est prêt !")
        } else {
          println("Stock insuffisant de café ou de lait.")
          return false
        }
      case _ => println("Choix invalide.")
    }
    true
  }

  // Sélection d'une machine
  def selectionnerMachine(): Int = {
    println("Machine sélectionnée (1-5)")
    readLine(" > ").toIntOption.filter(id => id >= 1 && id <= nbMachines).map(_ - 1).getOrElse(selectionnerMachine())
  }
}

object TP2 {
  def main(args: Array[String]): Unit = {
    while (true) {
      println("Nospresso Multi-Machines")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      // Lecture du choix de l'utilisateur
      val choix = readLine(" > ")
      choix match {
        case "1" =>
          val machineId = NospressoMultiMachine.selectionnerMachine()
          NospressoMultiMachine.serveClient(machineId, NospressoMultiMachine.coffeeStocks, NospressoMultiMachine.sugarStocks, NospressoMultiMachine.milkStocks)
        case "2" =>
          val machineId = NospressoMultiMachine.selectionnerMachine()
          if (NospressoMultiMachine.validatePin(machineId, NospressoMultiMachine.machinePins)) {
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            val choixAdmin = readLine(" > ")
            choixAdmin match {
              case "1" => NospressoMultiMachine.restockMachine(machineId, NospressoMultiMachine.coffeeStocks, NospressoMultiMachine.sugarStocks, NospressoMultiMachine.milkStocks)
              case "2" => NospressoMultiMachine.updatePin(machineId, NospressoMultiMachine.machinePins)
              case _ => println("Choix invalide.")
            }
          }
        case "3" => sys.exit(0)
        case _ => println("Choix invalide.")
      }
    }
  }
}
