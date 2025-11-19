import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val coffeeStocks = Array(50, 50, 50, 50, 50)
    val milkStocks = Array(500, 500, 500, 500, 500)
    val sugarStocks = Array(30, 30, 30, 30, 30)
    val machinePins = Array.fill(nbMachines)("434343")
    var programmeActif = true

    while (programmeActif) {
      println("         Nospresso Café")
      println("Veuillez sélectionner une machine (1 à "+nbMachines+") :")
      var choixMachine = readLine("> ").toInt
      while (choixMachine < 1 || choixMachine > nbMachines) {
        println("Choix invalide. Veuillez sélectionner une machine entre 1 et "+nbMachines+" :")
        choixMachine = readLine("> ").toInt
      }
      val machineIndex = choixMachine - 1

      println("Veuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var mode = readLine("> ").toInt
      while (mode < 1 || mode > 3) {
        println("Choix invalide. Veuillez sélectionner 1, 2 ou 3 :")
        mode = readLine("> ").toInt
      }

      if (mode == 1) {
        if (!serveClient(machineIndex, coffeeStocks, sugarStocks, milkStocks)) {
          println("Transaction échouée. Arrêt du programme.")
          programmeActif = false
        }
      } else if (mode == 2) {
        if (validatePin(machineIndex, machinePins)) {
          println("Accès administrateur accordé.")
          println("1) Réapprovisionner")
          println("2) Mettre à jour le code PIN")
          var actionAdmin = readLine("> ").toInt
          while (actionAdmin < 1 || actionAdmin > 2) {
            println("Choix invalide. Veuillez sélectionner 1 ou 2 :")
            actionAdmin = readLine("> ").toInt
          }
          if (actionAdmin == 1) {
            restockMachine(machineIndex, coffeeStocks, sugarStocks, milkStocks)
          } else if (actionAdmin == 2) {
            updatePin(machineIndex, machinePins)
          }

        } else {
          println("Accès refusé. Trop de tentatives échouées. Arrêt du programme.")
          programmeActif = false
        }
      } else if (mode == 3) {
        println("Merci de votre visite. À bientôt !")
        programmeActif = false
      }
      Thread.sleep(5000)
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN pour la machine "+(machineId + 1)+" :")
      val pin = readLine("> ")
      if (pin == machinePins(machineId)) return true
      attempts -= 1
      if (attempts > 0) {
        println("Code PIN incorrect. "+attempts+" tentative(s) restante(s).")
      }
    }
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mettre à jour le code PIN pour la machine "+(machineId + 1)+".")
    var pinValid = false
    while (!pinValid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = readLine("> ")
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        pinValid = true
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Code PIN invalide. Il doit contenir exactement 6 chiffres.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - 2.00")
    println("2) Cappuccino - 2.50")
    println("3) Latte - 3.20")
    var choix = readLine("> ").toInt
    while (choix < 1 || choix > 3) {
      println("Choix invalide. Veuillez sélectionner 1, 2 ou 3 :")
      choix = readLine("> ").toInt
    }

    var prixBoisson = 0.0
    var cafesuffisant = 0.0
    var laitsuffisant = 0.0
    var sucresuffisant = 0.0
    var boissonRecap = ""

    if (choix == 1) {
      prixBoisson = 2.00
      cafesuffisant = 8.0
      boissonRecap = "Expresso"
    } else if (choix == 2) {
      prixBoisson = 2.50
      cafesuffisant = 6.0
      laitsuffisant = 100
      boissonRecap = "Cappuccino"
    } else if (choix == 3) {
      println("Veuillez choisir la taille de votre Latte :")
      println("1) Petit 2.70")
      println("2) Moyen 3.20")
      println("3) Grand 3.70")
      var taille = readLine("> ").toInt
      while (taille < 1 || taille > 3) {
        println("Choix invalide. Veuillez sélectionner 1, 2 ou 3 :")
        taille = readLine("> ").toInt
      }

      if (taille == 1) {
        prixBoisson = 2.70
        cafesuffisant = 6
        laitsuffisant = 120
        boissonRecap = "Latte (Petit)"
      } else if (taille == 2) {
        prixBoisson = 3.20
        cafesuffisant = 8
        laitsuffisant = 150
        boissonRecap = "Latte (Moyen)"
      } else if (taille == 3) {
        prixBoisson = 3.70
        cafesuffisant = 12
        laitsuffisant = 200
        boissonRecap = "Latte (Grand)"
      }
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10")
    println("3) Moyen (10g) - 0.20")
    println("4) Beaucoup (15g) - 0.30")
    var sucre = readLine("> ").toInt
    while (sucre < 1 || sucre > 4) {
      println("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4 :")
      sucre = readLine("> ").toInt
    }

    var prixSucre = 0.0
    var sucreRecap = "Sans sucre"
    if (sucre == 2) {
      prixSucre = 0.10
      sucresuffisant = 5.0
      sucreRecap = "Peu (5g)"
    } else if (sucre == 3) {
      prixSucre = 0.20
      sucresuffisant = 10.0
      sucreRecap = "Moyen (10g)"
    } else if (sucre == 4) {
      prixSucre = 0.30
      sucresuffisant = 15.0
      sucreRecap = "Beaucoup (15g)"
    }

    var prixLaitSup = 0.0
    var laitSupplementaire = 0.0
    var laitsupRecap = "Non"
    if (choix == 2 || choix == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? (1) Oui (2) Non")
      var laitSupChoix = readLine("> ").toInt
      while (laitSupChoix < 1 || laitSupChoix > 2) {
        println("Choix invalide. Veuillez sélectionner 1 ou 2 :")
        laitSupChoix = readLine("> ").toInt
      }

      if (laitSupChoix == 1) {
        println("Combien de doses ? (max 3)")
        println("1) 50 mL (0.05)")
        println("2) 100 mL (0.10)")
        println("3) 150 mL (0.15)")
        var dose = readLine("> ").toInt
        while (dose < 1 || dose > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3 :")
          dose = readLine("> ").toInt
        }

        if (dose == 1 && milkStocks(machineId) >= 50) {
          prixLaitSup = 0.05
          laitSupplementaire = 50
          laitsupRecap = "Oui (1 dose)"
        } else if (dose == 2 && milkStocks(machineId) >= 100) {
          prixLaitSup = 0.10
          laitSupplementaire = 100
          laitsupRecap = "Oui (2 doses)"
        } else if (dose == 3 && milkStocks(machineId) >= 150) {
          prixLaitSup = 0.15
          laitSupplementaire = 150
          laitsupRecap = "Oui (3 doses)"
        } else {
          println("Stock de lait insuffisant pour ajouter cette dose.")
        }
      }
    }
    println("\nRécapitulatif de la commande :")
    println("Boisson : " + boissonRecap)
    println("Niveau de sucre : " + sucreRecap)
    if (choix == 2 || choix == 3) {
      println("Lait supplémentaire : " + laitsupRecap)
    }

    // Vérification des stocks
    if (coffeeStocks(machineId) < cafesuffisant) {
      println("\nErreur : Quantité de poudre à café insuffisante pour préparer votre boisson.\n")
      return true
    } else if (milkStocks(machineId) < (laitsuffisant + laitSupplementaire)) {
      println("\nErreur : Quantité de lait insuffisante pour préparer votre boisson.\n")
      return true
    } else if (sugarStocks(machineId) < sucresuffisant) {
      println("\nErreur : Quantité de sucre insuffisante pour préparer votre boisson.\n")
      return true
    }

    // Préparation et paiement
    if (choix == 1) {
      printf("Prix total : %.2f CHF + %.2f CHF = %.2f CHF\n",prixBoisson,prixSucre,prixSucre+ prixBoisson)
      println("")
    }else{
      printf("Prix total : %.2f CHF + %.2f CHF + %.2f CHF = %.2f CHF\n", prixBoisson, prixSucre, prixLaitSup, prixBoisson + prixSucre + prixLaitSup)
      println("")
    }
    println("\nVeuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + genererCodePaiement())
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("\nPaiement confirmé.")
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    println("Votre "+boissonRecap+"est prêt ! Bonne dégustation !")

    // Mise à jour des stocks
    coffeeStocks(machineId) -= cafesuffisant.toInt
    milkStocks(machineId) -= (laitsuffisant + laitSupplementaire).toInt
    sugarStocks(machineId) -= sucresuffisant.toInt

    true
  }

  def genererCodePaiement(): String = {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to 5).map(_ => alphabet.charAt((Math.random() * alphabet.length).toInt)).mkString
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Réapprovisionnement des stocks :")
    printf("Stock actuel de café : %d g. Entrez la quantité à ajouter :\n", coffeeStocks(machineId))
    coffeeStocks(machineId) += readLine("> ").toInt
    printf("Stock actuel de sucre : %d g. Entrez la quantité à ajouter :\n", sugarStocks(machineId))
    sugarStocks(machineId) += readLine("> ").toInt
    println("Stock actuel de lait : " + (milkStocks(machineId) * 0.001) + "L. Entrez la quantité à ajouter :")
    milkStocks(machineId) += (readLine("> ").toDouble * 1000).toInt
    println("Réapprovisionnement terminé.")
  }
}
