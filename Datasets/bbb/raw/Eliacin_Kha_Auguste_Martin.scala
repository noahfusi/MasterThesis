import scala.io.StdIn.readLine

object Nospresso {
  val nbMachines: Int = 5
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50)
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30)
  val milkStocks: Array[Int] = Array.fill(nbMachines)(500)
  val machinePins: Array[String] = Array.fill(nbMachines)("434343")
  val adminPassword: String = "admin2024"

  def main(args: Array[String]): Unit = {
    var continuer: Boolean = true
    while (continuer) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = readLine("> ")

      if (choix == "1") {
        println("Sélectionnez l'ID de la machine (0 à 4) :")
        val machineId = readLine("> ").toInt
        if (machineId >= 0 && machineId < nbMachines) {
          if (!serveClient(machineId)) {
            println("Stocks insuffisants pour cette machine.")
          }
        } else {
          println("ID de machine invalide.")
        }
      } else if (choix == "2") {
        println("Sélectionnez l'ID de la machine (0 à 4) :")
        val machineId = readLine("> ").toInt
        if (machineId >= 0 && machineId < nbMachines) {
          if (validatePin(machineId)) {
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            println("3) Réinitialiser le code PIN")
            val adminChoice = readLine("> ")
            if (adminChoice == "1") {
              restockMachine(machineId)
            } else if (adminChoice == "2") {
              updatePin(machineId)
            } else if (adminChoice == "3") {
              resetPin(machineId)
            } else {
              println("Choix invalide.")
            }
          } else {
            println("Code PIN incorrect après 3 tentatives. Fin du programme.")
            continuer = false
          }
        } else {
          println("ID de machine invalide.")
        }
      } else if (choix == "3") {
        continuer = false
      } else {
        println("Choix invalide. Veuillez réessayer.")
      }
    }
  }

  def validatePin(machineId: Int): Boolean = {
    var attempts = 0
    while (attempts < 3) {
      println(s"Entrez le code PIN pour la machine $machineId :")
      val pin = readLine("> ")
      if (pin == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      } else {
        attempts += 1
        println(s"Code PIN incorrect. ${3 - attempts} tentative(s) restante(s).")
      }
    }
    false
  }

  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var valid = false
    while (!valid) {
      val newPin = readLine("> ")
      if (newPin.matches("\\d{6}")) {
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.")
        valid = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Réessayez :")
      }
    }
  }

  def resetPin(machineId: Int): Unit = {
    println("Entrez le mot de passe administrateur pour confirmer :")
    val password = readLine("> ")
    if (password == adminPassword) {
      machinePins(machineId) = "434343"
      println(s"Le code PIN de la machine $machineId a été réinitialisé avec succès.")
    } else {
      println("Mot de passe incorrect. Réinitialisation annulée.")
    }
  }

  def serveClient(machineId: Int): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    val choixBoisson = readLine("> ")

    var cafeNeeded = 0
    var sucreNeeded = 0
    var laitNeeded = 0

    if (choixBoisson == "1") {
      cafeNeeded = 8
    } else if (choixBoisson == "2") {
      cafeNeeded = 6
      laitNeeded = 100
    } else if (choixBoisson == "3") {
      println("Quelle taille souhaitez-vous ?")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")
      val tailleLatte = readLine("> ")
      if (tailleLatte == "1") {
        cafeNeeded = 6
        laitNeeded = 120
      } else if (tailleLatte == "2") {
        cafeNeeded = 8
        laitNeeded = 150
      } else if (tailleLatte == "3") {
        cafeNeeded = 12
        laitNeeded = 200
      } else {
        println("Taille invalide. Commande annulée.")
        return false
      }
    } else {
      println("Choix invalide. Commande annulée.")
      return false
    }

    if (coffeeStocks(machineId) >= cafeNeeded && sugarStocks(machineId) >= sucreNeeded && milkStocks(machineId) >= laitNeeded) {
      coffeeStocks(machineId) -= cafeNeeded
      sugarStocks(machineId) -= sucreNeeded
      milkStocks(machineId) -= laitNeeded
      println("Préparation de votre boisson...")
      Thread.sleep(3000)
      println("Votre boisson est prête ! Bonne dégustation !")
      true
    } else {
      println("Stocks insuffisants pour préparer cette boisson.")
      false
    }
  }

  def restockMachine(machineId: Int): Unit = {
    println(s"Niveaux actuels - Café: ${coffeeStocks(machineId)}g, Sucre: ${sugarStocks(machineId)}g, Lait: ${milkStocks(machineId)}ml")
    println("Entrez les quantités à ajouter :")
    println("Poudre de café (g) :")
    val ajoutCafe = readLine("> ").toInt
    println("Sucre (g) :")
    val ajoutSucre = readLine("> ").toInt
    println("Lait (ml) :")
    val ajoutLait = readLine("> ").toInt

    coffeeStocks(machineId) += ajoutCafe
    sugarStocks(machineId) += ajoutSucre
    milkStocks(machineId) += ajoutLait

    println("Stocks mis à jour avec succès.")
  }
}
