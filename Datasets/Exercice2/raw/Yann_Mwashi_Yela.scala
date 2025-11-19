import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    // Stocks initiaux
    var gdepoudredecafe = 50
    var gdesucre = 30
    var mldelait = 500
    val nbMachines = 5
    var machinePins = Array.fill(nbMachines)("434343")
    var coffeeStocks = Array.fill(nbMachines)(gdepoudredecafe)
    var sugarStocks = Array.fill(nbMachines)(gdesucre)
    var milkStocks = Array.fill(nbMachines)(mldelait)
    var programmeEnCours = true

    while (programmeEnCours) {
      println("\n          \nNospresso Café")
      println(" Veuillez sélectionner votre mode :")
      println(" 1) Client")
      println(" 2) Admin")
      println(" 3) Quitter")
      val modechoisi = readValidChoice(List(1, 2, 3))

      if (modechoisi == 1) {
        var clientSession = true
        while (clientSession) {
          println("Veuillez sélectionner une machine (1 à 5) :")
          val machineId = readValidChoice((1 to 5).toList) - 1
          val result = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          clientSession = !result // Si un problème se produit (false), retour au choix de la machine
        }
      } else if (modechoisi == 2) {
        println("Veuillez sélectionner une machine (1 à 5) :")
        val machineId = readValidChoice((1 to 5).toList) - 1
        if (validatePin(machineId, machinePins)) {
          adminOptions(machineId, coffeeStocks, sugarStocks, milkStocks, machinePins)
        } else {
          println("Trop de tentatives échouées. Fin du programme.")
          programmeEnCours = false
        }
      } else if (modechoisi == 3) {
        println("Votre session est terminée. Merci d'avoir utilisé Nospresso !")
        programmeEnCours = false
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val enteredPin = readLine(">").trim
      if (enteredPin == machinePins(machineId)) {
        println("Code PIN correct. Accès autorisé.")
        return true
      } else {
        attempts -= 1
        if (attempts > 0) {
          println(s"Code PIN incorrect. Tentatives restantes : $attempts.")
        }
      }
    }
    false
  }

  def adminOptions(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], machinePins: Array[String]): Unit = {
    println("Options disponibles :")
    println(" 1) Réapprovisionnement")
    println(" 2) Mise à jour du code PIN")
    val adminChoice = readValidChoice(List(1, 2))

    if (adminChoice == 1) {
      println(s"Quantités actuelles dans la machine ${machineId + 1} avant réapprovisionnement :")
      println(s"Poudre de café : ${coffeeStocks(machineId)}g")
      println(s"Sucre : ${sugarStocks(machineId)}g")
      printf("Lait : %.2f L\n", milkStocks(machineId) / 1000.0)
      restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
    } else if (adminChoice == 2) {
      updatePin(machineId, machinePins)
    }
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    do {
      println("Entrez un nouveau code à 6 chiffres :")
      newPin = readLine(">")
    } while (!newPin.matches("\\d{6}")) // Vérifie que le code contient exactement 6 chiffres

    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    val caract = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var twint = ""
    for (_ <- 1 to 5) {
      val chxcaract = (Math.random() * 36).toInt
      twint += caract(chxcaract)
    }

    println("Veuillez sélectionner votre boisson :")
    println(" 1) Expresso - CHF 2.00")
    println(" 2) Capuccino - CHF 2.50")
    println(" 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    val boisson = readValidChoice(List(1, 2, 3))

    var prixtotal = 0.0
    var coffeeNeeded = 0
    var milkNeeded = 0
    var boissonNom = ""
    var sucreNiveau = "Sans sucre"
    var laitSupplementaire = "Non"

    if (boisson == 1) {
      prixtotal = 2.00
      coffeeNeeded = 8
      milkNeeded = 0
      boissonNom = "Expresso"
    } else if (boisson == 2) {
      prixtotal = 2.50
      coffeeNeeded = 6
      milkNeeded = 100
      boissonNom = "Capuccino"
    } else if (boisson == 3) {
      println("Dimension du Latte :")
      println(" 1) Petit - CHF 2.70")
      println(" 2) Moyen - CHF 3.20")
      println(" 3) Grand - CHF 3.70")
      val dimensionlatte = readValidChoice(List(1, 2, 3))
      if (dimensionlatte == 1) {
        prixtotal = 2.70
        coffeeNeeded = 6
        milkNeeded = 120
        boissonNom = "Latte (Petit)"
      } else if (dimensionlatte == 2) {
        prixtotal = 3.20
        coffeeNeeded = 8
        milkNeeded = 150
        boissonNom = "Latte (Moyen)"
      } else if (dimensionlatte == 3) {
        prixtotal = 3.70
        coffeeNeeded = 12
        milkNeeded = 200
        boissonNom = "Latte (Grand)"
      }
    }

    if (coffeeStocks(machineId) < coffeeNeeded) {
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre machine ou vérifier les stocks en mode Admin.")
      return false
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println(" 1) Sans sucre")
    println(" 2) Peu (5g) - CHF 0.10")
    println(" 3) Moyen (10g) - CHF 0.20")
    println(" 4) Beaucoup (15g) - CHF 0.30")
    val sucre = readValidChoice(List(1, 2, 3, 4))
    val prixSucre = if (sucre == 1) 0.00 else if (sucre == 2) 0.10 else if (sucre == 3) 0.20 else 0.30

    if (sucre == 2) sucreNiveau = "Peu (5g)"
    else if (sucre == 3) sucreNiveau = "Moyen (10g)"
    else if (sucre == 4) sucreNiveau = "Beaucoup (15g)"

    if ((sucre == 2 && sugarStocks(machineId) < 5) ||
      (sucre == 3 && sugarStocks(machineId) < 10) ||
      (sucre == 4 && sugarStocks(machineId) < 15)) {
      println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre machine ou vérifier les stocks en mode Admin.")
      return false
    }

    // Ajout de lait uniquement pour Capuccino et Latte
    var extraMilkPrice = 0.0
    var extraMilkAmount = 0
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println(" 1) Oui")
      println(" 2) Non")
      val ajoutLait = readValidChoice(List(1, 2))
      if (ajoutLait == 1) {
        println("Combien de doses de lait supplémentaire (1 à 3 doses, 50ml par dose, CHF 0.05 par dose) ?")
        val doses = readValidChoice(List(1, 2, 3))
        extraMilkPrice = doses * 0.05
        extraMilkAmount = doses * 50
        laitSupplementaire = "Oui"
      }
    }

    if (milkStocks(machineId) < milkNeeded + extraMilkAmount) {
      println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre machine ou vérifier les stocks en mode Admin.")
      return false
    }

    prixtotal += prixSucre + extraMilkPrice

    // Afficher les détails de l'achat
    println(s"Boisson sélectionnée : $boissonNom")
    println(s"Niveau de sucre : $sucreNiveau")
    println(s"Lait supplémentaire : $laitSupplementaire")

    // Afficher le prix total
    println(f"Prix total : CHF ${prixtotal - prixSucre - extraMilkPrice}%.2f + CHF $prixSucre%.2f (sucre) + CHF $extraMilkPrice%.2f (lait supplémentaire) = CHF $prixtotal%.2f")
    println(s"Votre code de paiement Twint est : $twint")
    println("Validation du paiement...")
    Thread.sleep(3000)
    println("Paiement confirmé.")
    println("Préparation de votre boisson...")
    Thread.sleep(3000)

    coffeeStocks(machineId) -= coffeeNeeded
    sugarStocks(machineId) -= (if (sucre == 1) 0 else if (sucre == 2) 5 else if (sucre == 3) 10 else 15)
    milkStocks(machineId) -= milkNeeded + extraMilkAmount

    println("Votre boisson est prête ! Bonne dégustation !")
    true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Réapprovisionnement des stocks...")
    println("Entrez la quantité de poudre de café à ajouter (en grammes) :")
    val ajoutCafe = readLine(">").toInt
    println("Entrez la quantité de sucre à ajouter (en grammes) :")
    val ajoutSucre = readLine(">").toInt
    println("Entrez la quantité de lait à ajouter (en millilitres) :")
    val ajoutLait = readLine(">").toInt

    coffeeStocks(machineId) += ajoutCafe
    sugarStocks(machineId) += ajoutSucre
    milkStocks(machineId) += ajoutLait

    println("Stocks mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def readValidChoice(validChoices: List[Int]): Int = {
    var choice = -1
    while (!validChoices.contains(choice)) {
      choice = readLine(">").toIntOption.getOrElse(-1)
    }
    choice
  }
}







