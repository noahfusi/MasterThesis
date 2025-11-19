

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Nospresso {
  val nbMachines = 5 // Nombre total de machines
  val Pin = "434343" // Code PIN  pour toutes les machines
  val maxAttempts = 3 // Nombre maximum de tentatives pour le code PIN


  // Tableaux pour les stocks et les codes PIN
  var machinePins: Array[String] = Array.fill(nbMachines)(Pin)
  var coffeeStocks: Array[Int] = Array.fill(nbMachines)(500) // Stock initial en grammes
  var sugarStocks: Array[Int] = Array.fill(nbMachines)(300) // Stock initial en grammes
  var milkStocks: Array[Int] = Array.fill(nbMachines)(200)  // Stock initial en millilitres

  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("Sélectionnez un mode :")
      println("1) Mode Client")
      println("2) Mode Administrateur")
      println("3) Quitter")
      val choice = scala.io.StdIn.readInt()

      choice match {
        case 1 =>
          clientMode()
        case 2 =>
          if (adminMenu()) {
            println("Retour au menu principal...")
          }
        case 3 =>
          println("Au revoir !")
          continue = false
        case _ =>
          println("Choix invalide. Réessayer.")
      }
    }
  }

  def clientMode(): Unit = {
    val machineId = selectMachine()
    if (machineId != -1) {
      if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
        println("Transaction échouée. Vérifiez les stocks ou réessayez.")
      }
    }
  }

  def adminMenu(): Boolean = {
    val machineId = selectMachine()
    if (machineId == -1) return false

    if (validatePin(machineId, machinePins)) {
      println("Accès accordé.")
      println("Que voulez-vous faire ?")
      println("1. Réapprovisionner la machine")
      println("2. Mettre à jour le code PIN")
      val adminChoice = scala.io.StdIn.readInt()

      adminChoice match {
        case 1 =>
          restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        case 2 =>
          updatePin(machineId, machinePins)
        case _ =>
          println("Choix invalide.")
      }
      true
    } else {
      println("Échec d'accès. Retour au menu principal.")
      false
    }
  }

  def selectMachine(): Int = {
    println(s"Sélectionnez une machine (0-${nbMachines-1}):")
    val machineId = scala.io.StdIn.readInt()
    if (machineId >= 1 && machineId < nbMachines) {
      machineId
    } else {
      println("Machine invalide.")
      -1
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 0
    while (attempts < maxAttempts) {
      println("Entrez le code PIN :")
      val inputPin = scala.io.StdIn.readLine()
      if (inputPin == machinePins(machineId)) {
        return true
      } else {
        attempts += 1
        println(s"Code PIN incorrect. ${maxAttempts - attempts} tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    sys.exit(0)
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var valid = false
    while (!valid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = scala.io.StdIn.readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succès.")
        valid = true
      } else {
        println("Code PIN invalide. Essayez encore.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Sélectionnez une boisson :")
    println("1. Expresso - CHF 2.00")
    println("2. Cappuccino - CHF 2.50")
    println("3. Latte (Petite - CHF 2.70, Moyenne - CHF 3.20, Grande - CHF 3.70)")
    val choice = scala.io.StdIn.readInt()

    val coffeeType = choice match {
      case 1 => "Expresso"
      case 2 => "Cappuccino"
      case 3 =>
        println("Choisissez la taille du Latte :")
        println("1. Petite (CHF 2.70)")
        println("2. Moyenne (CHF 3.20)")
        println("3. Grande (CHF 3.70)")
        print("Choix : ")
        val choice = scala.io.StdIn.readInt()
        choice match {
          case 1 => "Latte_Small"
          case 2 => "Latte_Medium"
          case 3 => "Latte_Large"
          case _ =>
            println("Taille invalide. Retour au menu principal.")

        }
      case _ =>
        println("Choix invalide. Retour au menu principal.")
    }
    // Ajouter du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1. Sans sucre")
    println("2. Peu (5g) - CHF 0.10")
    println("3. Moyen (10g) - CHF 0.20")
    println("4. Beaucoup (15g) - CHF 0.30")
    print("Choix : ")
    val sugarChoice = scala.io.StdIn.readInt()
    val (sugarType, sugarAmout) = sugarChoice match {
      case 1 => ("Sans Sucre", 0.0)
      case 2 => ("Peu", 5.0)
      case 3 => ("Moyen", 10.0)
      case 4 => ("Beaucoup", 15.0)
      case _ =>
        println("Choix de sucre invalide. Retour au menu principal.")

    }

    choice match {
      case 1 =>
        if (coffeeStocks(machineId) >= 50) {
          coffeeStocks(machineId) -= 50
          println("Expresso servi.")
          true
        } else {
          println("Stock insuffisant de café.")
          false
        }
      case 2 =>
        if (coffeeStocks(machineId) >= 50 && milkStocks(machineId) >= 100) {
          coffeeStocks(machineId) -= 50
          milkStocks(machineId) -= 100
          println("Cappuccino.")
          true
        } else {
          println("Stock insuffisant.")
          false
        }
      case 3 =>
        if (coffeeStocks(machineId) >= 50 && sugarStocks(machineId) >= 20) {
          coffeeStocks(machineId) -= 50
          sugarStocks(machineId) -= 20
          println("Latte servi.")
          true
        } else {
          println("Stock insuffisant.")
          false
        }
      case _ =>
        println("Choix invalide.")
        false
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Quantité de café à ajouter (en grammes) :")
    val coffee = scala.io.StdIn.readInt()
    println("Quantité de sucre à ajouter (en grammes) :")
    val sugar = scala.io.StdIn.readInt()
    println("Quantité de lait à ajouter (en millilitres) :")
    val milk = scala.io.StdIn.readInt()

    if (coffee >= 0 && sugar >= 0 && milk >= 0) {
      coffeeStocks(machineId) += coffee
      sugarStocks(machineId) += sugar
      milkStocks(machineId) += milk
      println("Les stocks ont été mis à jour avec succès.")
    } else {
      println("Quantités invalides.")
    }
  }
}

