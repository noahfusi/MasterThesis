import scala.io.StdIn._
import scala.util.Random

object NespressoMultiMachine {
  // Constante pour le nombre de machines
  val nbMachines = 5

  // Tableaux pour les codes PIN et les stocks
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50) // Stocks de café en grammes
  val sugarStocks = Array.fill(nbMachines)(30) // Stocks de sucre en grammes
  val milkStocks = Array.fill(nbMachines)(500) // Stocks de lait en millilitres

  def main(args: Array[String]): Unit = {
    println("Bienvenue chez Nospresso Multi-Machines !")

    var nospresso = true

    while (nospresso) {
      println("\nChoisissez une option :")
      println("1) Mode Client")
      println("2) Mode Administrateur")
      println("3) Quitter")

      val choix = readLine().toInt

      if (choix == 1) {
        modeClient()
      } else if (choix == 2) {
        modeAdministrateur()
      } else if (choix == 3) {
        nospresso = false
        println("Merci d'avoir utilisé Nospresso. À bientôt !")
      } else {
        println("Choix invalide. Essayez encore.")
      }
    }
  }

  // Fonctionnalité du mode client
  def modeClient(): Unit = {
    val machineId = selectMachine()
    if (machineId != -1) {
      var continuer = true
      while (continuer) {
        if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
          println("Commande échouée. Essayez une autre machine ou un autre produit.")
        }
        println("Voulez-vous commander une autre boisson ? (1: Oui, 2: Non)")
        val choix = readLine().toInt
        if (choix == 2) continuer = false
      }
    }
  }

  // Fonctionnalité du mode administrateur
  def modeAdministrateur(): Unit = {
    val machineId = selectMachine()
    if (machineId != -1 && validatePin(machineId, machinePins)) {
      println(s"\nAccès autorisé à la Machine ${machineId + 1}.")
      println("Niveaux de stock actuels :")
      println(f"Poudre de café : ${coffeeStocks(machineId)}g")
      println(f"Sucre          : ${sugarStocks(machineId)}g")
      println(f"Lait           : ${milkStocks(machineId) / 1000.0}%.1fL")
      println("1) Réapprovisionner")
      println("2) Mettre à jour le code PIN")

      val choice = readLine().toInt
      if (choice == 1) {
        restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
      } else if (choice == 2) {
        updatePin(machineId, machinePins)
      } else {
        println("Choix invalide.")
      }
    } else {
      println("Accès refusé ou tentatives épuisées.")
    }
  }

  // Fonctionnalité pour sélectionner une machine
  def selectMachine(): Int = {
    println("Sélectionnez une machine (1-à-5) :")
    val machineId = readLine().toInt - 1
    if (machineId >= 0 && machineId < nbMachines) machineId
    else {
      println("Machine invalide.")
      -1
    }
  }

  // Fonctionnalité de validation du code PIN pour l'administrateur
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN :")
      val pin = readLine()
      if (pin == machinePins(machineId)) {
        println("Code PIN valide.")
        return true
      } else {
        attempts -= 1
        println("Code PIN incorrect. Tentatives restantes : " + attempts)
      }
    }
    false
  }

  // Fonctionnalité pour mettre à jour le code PIN d'une machine
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
    var valid = false
    while (!valid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        valid = true
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Code PIN invalide. Veuillez réessayer.")
      }
    }
  }

  // Fonctionnalité pour servir un client avec une commande de boisson
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("\nChoisissez une boisson :")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")

    val choice = readLine().toInt
    var prixBoisson = 0.0
    val (coffeeNeeded, sugarNeededBase, milkNeededBase) = if (choice == 1) {
      prixBoisson = 2.00
      (2, 0, 0)
    } else if (choice == 2) {
      prixBoisson = 2.50
      (3, 0, 50)
    } else if (choice == 3) {
      println("Quelle taille souhaitez-vous ?")
      println("1) Petit - 2.70 CHF")
      println("2) Moyen - 3.20 CHF")
      println("3) Grand - 3.70 CHF")
      val tailleLatte = readLine().toInt
      if (tailleLatte == 1) {
        prixBoisson = 2.70
        (4, 0, 100)
      } else if (tailleLatte == 2) {
        prixBoisson = 3.20
        (5, 0, 150)
      } else if (tailleLatte == 3) {
        prixBoisson = 3.70
        (6, 0, 200)
      } else {
        println("Choix invalide.")
        return false
      }
    } else {
      println("Choix invalide.")
      return false
    }

    // Ajouter du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10 CHF")
    println("3) Moyen (10g) - 0.20 CHF")
    println("4) Beaucoup (15g) - 0.30 CHF")
    val choixSucre = readLine().toInt
    val sucreSupplementaire = if (choixSucre == 1) {
      0
    } else if (choixSucre == 2) {
      prixBoisson += 0.10
      5
    } else if (choixSucre == 3) {
      prixBoisson += 0.20
      10
    } else if (choixSucre == 4) {
      prixBoisson += 0.30
      15
    } else {
      println("Choix invalide.")
      return false
    }

    // Ajouter du lait supplémentaire
    println("Voulez-vous ajouter du lait supplémentaire ? (1: Oui, 2: Non)")
    val ajoutLait = readLine().toInt
    val laitSupplementaire = if (ajoutLait == 1) {
      println("Combien de doses (0 à 3) ?")
      val doses = readLine().toInt.max(0).min(3)
      prixBoisson += doses * 0.05
      doses * 50
    } else 0

    val sugarNeeded = sugarNeededBase + sucreSupplementaire
    val milkNeeded = milkNeededBase + laitSupplementaire

    if (coffeeStocks(machineId) >= coffeeNeeded && sugarStocks(machineId) >= sugarNeeded && milkStocks(machineId) >= milkNeeded) {
      coffeeStocks(machineId) -= coffeeNeeded
      sugarStocks(machineId) -= sugarNeeded
      milkStocks(machineId) -= milkNeeded

      // Génération du code de paiement
      val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var codePaiement = ""
      for (_ <- 1 to 5) {
        val randomIndex = (math.random() * caracteres.length).toInt
        codePaiement += caracteres(randomIndex)
      }

      println("Votre commande est prête !")
      println("Boisson : " + (if (choice == 1) "Expresso" else if (choice == 2) "Cappuccino" else "Latte"))
      println(f"Prix total : CHF $prixBoisson%.2f")
      println("Sucre : " + (if (choixSucre == 1) "Sans sucre" else if (choixSucre == 2) "Peu" else if (choixSucre == 3) "Moyen" else "Beaucoup"))
      println("Code de paiement : " + codePaiement)
      println("Paiement confirmé. Préparation de la boisson...")
      println("Votre boisson est prête ! Bonne dégustation !")
      true
    } else {
      println("Stock insuffisant pour cette commande.")
      false
    }
  }

  // Fonctionnalité pour réapprovisionner les stocks de la machine
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("\nRéapprovisionnement pour la Machine " + machineId)

    println("Ajout de café (en grammes) :")
    coffeeStocks(machineId) += readLine().toInt.max(0)

    println("Ajout de sucre (en grammes) :")
    sugarStocks(machineId) += readLine().toInt.max(0)

    println("Ajout de lait (en millilitres) :")
    milkStocks(machineId) += readLine().toInt.max(0)

    println("Stock mis à jour avec succès.")
  }
}