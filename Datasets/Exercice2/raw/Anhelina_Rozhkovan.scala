import scala.io.StdIn
import scala.util.Random

object MultiMachineNospresso {

  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50)
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(0.5)


  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN :")
      val enteredPin = StdIn.readLine()
      if (enteredPin == machinePins(machineId)) {
        println("Accès accordé.")
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. ${if (attempts > 0) s"$attempts tentative(s) restante(s)." else ""}")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }


  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine.")
    var newPin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      newPin = StdIn.readLine()
    } while (!newPin.matches("\\d{6}"))
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
  }


  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
    println(
      """Veuillez sélectionner votre boisson :
        |1) Expresso
        |2) Cappuccino
        |3) Latte""".stripMargin
    )
    val drink = StdIn.readLine()

    val latteSize = if (drink == "3") {
      var size = ""
      do {
        println(
          """Veuillez sélectionner la taille de votre Latte :
            |1) Petit
            |2) Moyen
            |3) Grand""".stripMargin
        )
        size = StdIn.readLine()
        if (!List("1", "2", "3").contains(size)) {
          println("Choix invalide, veuillez réessayer.")
        }
      } while (!List("1", "2", "3").contains(size))
      size
    } else "1"

    val sugarChoice = {
      var choice = ""
      do {
        println(
          """Souhaitez-vous ajouter du sucre ?
            |1) Sans sucre
            |2) Peu (5g)
            |3) Moyen (10g)
            |4) Beaucoup (15g)""".stripMargin
        )
        choice = StdIn.readLine()
        if (!List("1", "2", "3", "4").contains(choice)) {
          println("Choix invalide, veuillez réessayer.")
        }
      } while (!List("1", "2", "3", "4").contains(choice))
      choice
    }

    val milkExtra = if (drink != "1") {
      var choice = ""
      do {
        println(
          """Souhaitez-vous ajouter du lait en supplément ?
            |1) Oui
            |2) Non""".stripMargin
        )
        choice = StdIn.readLine()
        if (!List("1", "2").contains(choice)) {
          println("Choix invalide, veuillez réessayer.")
        }
      } while (!List("1", "2").contains(choice))
      choice == "1"
    } else false

    val (coffeeNeeded, milkNeededBase, basePrice) = drink match {
      case "1" => (8, 0.0, 2.0) // Expresso
      case "2" => (6, 0.1, 2.5) // Cappuccino
      case "3" =>
        latteSize match {
          case "1" => (6, 0.12, 2.7) // Latte Petit
          case "2" => (8, 0.15, 3.2) // Latte Moyen
          case "3" => (12, 0.2, 3.7) // Latte Grand
          case _ =>
            println("Sélection invalide.")
            return false
        }
      case _ =>
        println("Sélection invalide.")
        return false
    }

    val sugarNeeded = sugarChoice match {
      case "2" => 5
      case "3" => 10
      case "4" => 15
      case _   => 0
    }

    val milkNeeded = milkNeededBase + (if (milkExtra) 0.05 else 0.0)

    if (coffeeStocks(machineId) < coffeeNeeded || milkStocks(machineId) < milkNeeded || sugarStocks(machineId) < sugarNeeded) {
      if (coffeeStocks(machineId) < coffeeNeeded) println("Erreur : Quantité insuffisante de café.")
      if (milkStocks(machineId) < milkNeeded) println("Erreur : Quantité insuffisante de lait.")
      if (sugarStocks(machineId) < sugarNeeded) println("Erreur : Quantité insuffisante de sucre.")
      return false
    }


    coffeeStocks(machineId) -= coffeeNeeded
    milkStocks(machineId) -= milkNeeded
    sugarStocks(machineId) -= sugarNeeded

    val totalPrice = basePrice + sugarNeeded * 0.02 + (if (milkExtra) 0.5 else 0.0)

    println(f"Le prix total est de : CHF $totalPrice%.2f")
    val paymentCode = Random.alphanumeric.take(5).mkString
    println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $paymentCode")
    println("Merci ! Votre paiement a été accepté.")
    println("Votre boisson est prête ! Bonne dégustation !")
    true
  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("État actuel des stocks :")
    println(s"Café : ${coffeeStocks(machineId)} g")
    println(s"Sucre : ${sugarStocks(machineId)} g")
    println(s"Lait : ${milkStocks(machineId)} L")
    println("Réapprovisionnement des stocks.")

    println("Quantité de poudre de café à ajouter :")
    val coffeeToAdd = StdIn.readInt()
    println("Quantité de sucre à ajouter :")
    val sugarToAdd = StdIn.readInt()
    println("Quantité de lait à ajouter (en litres) :")
    val milkToAdd = StdIn.readDouble()

    coffeeStocks(machineId) += coffeeToAdd
    sugarStocks(machineId) += sugarToAdd
    milkStocks(machineId) += milkToAdd

    println("Les stocks ont été mis à jour avec succès.")
  }


  def displayMainMenu(): Unit = {
    println("Bienvenue dans le système Nospresso")
    println("1) Mode Client")
    println("2) Mode Administrateur")
    println("3) Quitter")
  }

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      displayMainMenu()
      val choice = StdIn.readLine()
      choice match {
        case "1" =>
          println("Sélectionnez une machine (1 à 5) :")
          val machineId = StdIn.readInt() - 1
          if (machineId >= 0 && machineId < nbMachines) {
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else {
            println("Machine invalide.")
          }
        case "2" =>
          println("Sélectionnez une machine (1 à 5) :")
          val machineId = StdIn.readInt() - 1
          if (machineId >= 0 && machineId < nbMachines) {
            if (validatePin(machineId, machinePins)) {
              println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN")
              val adminChoice = StdIn.readLine()
              adminChoice match {
                case "1" => restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
                case "2" => updatePin(machineId, machinePins)
                case _ => println("Option invalide.")
              }
            }
          } else {
            println("Machine invalide.")
          }
        case "3" =>
          println("Au revoir!")
          running = false
        case _ => println("Option invalide.")
      }
    }
  }
}
