import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  val nbMachines: Int = 5
  val machinePins: Array[String] = Array.fill(nbMachines)("434343")
  val coffeeStocks: Array[Double] = Array.fill(nbMachines)(50.0)
  val sugarStocks: Array[Double] = Array.fill(nbMachines)(30.0)
  val milkStocks: Array[Double] = Array.fill(nbMachines)(500.0)

  def main(args: Array[String]): Unit = {
    mainMenu()
  }

  // Menu principal
  def mainMenu(): Unit = {
    var continue = true
    while (continue) {
      println("\nNospresso Café")
      println("Veuillez sélectionner une machine (0 à 4) ou taper 5 pour quitter :")
      print("> ")

      val machineId = readLine().toIntOption.getOrElse(-1)
      if (machineId >= 0 && machineId < nbMachines) {
        println("Modes disponibles :\n1) Client\n2) Administrateur")
        print("> ")
        readLine().toIntOption match {
          case Some(1) => serveClient(machineId)
          case Some(2) => if (validatePin(machineId)) adminMenu(machineId)
          case _       => println("Option invalide.")
        }
      } else if (machineId == 5) {
        println("Merci d'avoir utilisé Nospresso. À bientôt !")
        continue = false
      } else {
        println("Sélection invalide, veuillez réessayer.")
      }
    }
  }

  // Validation du code PIN
  def validatePin(machineId: Int): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la Machine $machineId : ")
      val pin = readLine()
      if (pin == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. $attempts tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  // Menu Administrateur
  def adminMenu(machineId: Int): Unit = {
    println(s"Menu Administrateur - Machine $machineId")
    println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN\n3) Retour")
    print("> ")

    readLine().toIntOption match {
      case Some(1) => restockMachine(machineId)
      case Some(2) => updatePin(machineId)
      case Some(3) => println("Retour au menu principal.")
      case _       => println("Option invalide.")
    }
  }

  // Mise à jour du code PIN
  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres : ")
    var newPin = ""
    while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
      newPin = readLine()
      if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        println("Code PIN invalide. Entrez un code à 6 chiffres :")
      }
    }
    machinePins(machineId) = newPin
    println(s"Le code PIN pour la Machine $machineId a été mis à jour.")
  }

  // Service client
  def serveClient(machineId: Int): Unit = {
    println(s"Service Client - Machine $machineId")
    println("1) Expresso (CHF 2.00)\n2) Cappuccino (CHF 2.50)\n3) Latte (CHF 2.70 - CHF 3.70)")
    print("> ")

    val choix = readLine().toIntOption.getOrElse(-1)
    val prix = choix match {
      case 1 if verifierStock(machineId, 8, 0, 0)   => 2.0
      case 2 if verifierStock(machineId, 6, 0, 100) => 2.5
      case 3                                       => choisirTailleLatte(machineId)
      case _ =>
        println("Boisson invalide ou stock insuffisant.")
        return
    }
    val prixFinal = prix + choisirSucre(machineId) + (if (choix == 2 || choix == 3) choisirLaitSupplementaire(machineId) else 0.0)
    println(f"Prix total : CHF $prixFinal%.2f")
    println(s"Code de paiement : ${CodePaiement()}")
    println("Préparation de la boisson...\nVotre boisson est prête. Bonne dégustation !")
  }

  // Choisir la taille du Latte
  def choisirTailleLatte(machineId: Int): Double = {
    println("1) Petit (CHF 2.70)\n2) Moyen (CHF 3.20)\n3) Grand (CHF 3.70)")
    print("> ")
    readLine().toIntOption match {
      case Some(1) if verifierStock(machineId, 6, 0, 120) => 2.7
      case Some(2) if verifierStock(machineId, 8, 0, 150) => 3.2
      case Some(3) if verifierStock(machineId, 12, 0, 200) => 3.7
      case _ =>
        println("Taille invalide ou stock insuffisant.")
        0.0
    }
  }

  // Réapprovisionnement
  def restockMachine(machineId: Int): Unit = {
    println(s"Réapprovisionnement - Machine $machineId")
    println("Entrez la quantité de café à ajouter (g) : ")
    coffeeStocks(machineId) += readLine().toDoubleOption.getOrElse(0.0)
    println("Entrez la quantité de sucre à ajouter (g) : ")
    sugarStocks(machineId) += readLine().toDoubleOption.getOrElse(0.0)
    println("Entrez la quantité de lait à ajouter (ml) : ")
    milkStocks(machineId) += readLine().toDoubleOption.getOrElse(0.0)
    println("Réapprovisionnement effectué avec succès.")
  }

  // Vérification des stocks
  def verifierStock(machineId: Int, cafe: Double, sucre: Double, lait: Double): Boolean = {
    if (coffeeStocks(machineId) >= cafe && sugarStocks(machineId) >= sucre && milkStocks(machineId) >= lait) {
      coffeeStocks(machineId) -= cafe
      sugarStocks(machineId) -= sucre
      milkStocks(machineId) -= lait
      true
    } else {
      println("Stock insuffisant pour cette boisson.")
      false
    }
  }

  // Code de paiement
  def CodePaiement(): String = {
    val chars = ('A' to 'Z') ++ ('0' to '9')
    (1 to 5).map(_ => chars(Random.nextInt(chars.length))).mkString
  }

  // Options de sucre
  def choisirSucre(machineId: Int): Double = {
    println("Ajouter du sucre ? 1) Non 2) Peu (CHF 0.10) 3) Moyen (CHF 0.20) 4) Beaucoup (CHF 0.30)")
    print("> ")
    readLine().toIntOption match {
      case Some(2) if sugarStocks(machineId) >= 5  => sugarStocks(machineId) -= 5; 0.1
      case Some(3) if sugarStocks(machineId) >= 10 => sugarStocks(machineId) -= 10; 0.2
      case Some(4) if sugarStocks(machineId) >= 15 => sugarStocks(machineId) -= 15; 0.3
      case _                                       => 0.0
    }
  }

  // Lait supplémentaire
  def choisirLaitSupplementaire(machineId: Int): Double = {
    println("Ajouter du lait supplémentaire (CHF 0.05 par dose, max 3 doses) ? 1) Oui 2) Non")
    print("> ")
    if (readLine().toIntOption.contains(1)) {
      println("Combien de doses ? (max 3)")
      val doses = readLine().toIntOption.getOrElse(0).min(3)
      val quantiteLait = doses * 50
      if (milkStocks(machineId) >= quantiteLait) {
        milkStocks(machineId) -= quantiteLait
        doses * 0.05
      } else {
        println("Stock de lait insuffisant.")
        0.0
      }
    } else 0.0
  }
}