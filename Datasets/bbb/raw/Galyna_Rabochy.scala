import scala.io.StdIn._
import scala.util.Random

object exercice2 {

  // Initialisation des stocks pour plusieurs machines
  val nbr_machines = 5
  val machinePins: Array[String] = Array.fill(nbr_machines)("434343")
  val coffeeStocks: Array[Int] = Array.fill(nbr_machines)(50)
  val sugarStocks: Array[Int] = Array.fill(nbr_machines)(30)
  val milkStocks: Array[Int] = Array.fill(nbr_machines)(500) // lait en millilitres


  def main(args: Array[String]): Unit = {
    var running = true

    while (running) {
      var modeChoice = ""
      do {
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        modeChoice = readLine()
        if (modeChoice != "1" && modeChoice != "2" && modeChoice != "3") {
          println("Option invalide. Veuillez entrer 1, 2 ou 3.")
        }
      } while (modeChoice != "1" && modeChoice != "2" && modeChoice != "3")

      if (modeChoice == "1") {
        clientMode()
      } else if (modeChoice == "2") {
        adminMode()
      } else if (modeChoice == "3") {
        println("Merci d'avoir utilisé Nospresso ! Au revoir.")
        running = false
      }
    }
  }

  def clientMode(): Unit = {
    var machineId = -1
    do {
      println("Veuillez sélectionner une machine (0 à " + (nbr_machines - 1) + ")")
      print("> ")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        machineId = input.toInt
      } else {
        machineId = -1
      }

      if (machineId < 0 || machineId >= nbr_machines) {
        println("ID de machine invalide. Veuillez recommencer.")
      }
    } while (machineId < 0 || machineId >= nbr_machines)

    println("Bienvenue au mode Client pour la machine " + machineId)
    if (serveClient(machineId)) {
      println("Commande servie avec succès.")
    } else {
      println("Erreur : Commande non servie en raison de stocks insuffisants ou d'une erreur d'entrée.")
    }
  }

  def adminMode(): Unit = {
    var machineId = -1
    do {
      println("Veuillez sélectionner une machine (0 à " + (nbr_machines - 1) + ")")
      print("> ")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        machineId = input.toInt
      } else {
        machineId = -1
      }

      if (machineId < 0 || machineId >= nbr_machines) {
        println("ID de machine invalide. Veuillez recommencer.")
      }
    } while (machineId < 0 || machineId >= nbr_machines)

    println("Mode Admin sélectionné.")
    if (validatePin(machineId, machinePins)) {
      // Affichage des stocks après saisie correcte du PIN
      println("Niveaux de stock actuels : " + machineId + ":")
      println("Poudre de café : " + coffeeStocks(machineId) + "g")
      println("Lait : " + milkStocks(machineId) + "ml")
      println("Sucre : " + sugarStocks(machineId) + "g")

      var recharge = ""
      do {
        println("Souhaitez-vous réapprovisionner les stocks ?")
        println("1) Oui")
        println("2) Non")
        print("> ")
        recharge = readLine()
        if (recharge != "1" && recharge != "2") {
          println("Option invalide. Veuillez reesayer")
        }
      } while (recharge != "1" && recharge != "2")

      if (recharge == "1") {
        restockStocks(machineId)
      }

      // Réaffiche les stocks après réapprovisionnement
      println("Stocks mis à jour par la machine " + machineId + ":")
      println(" Café restant : " + coffeeStocks(machineId) + " g")
      println(" Lait restant : " + milkStocks(machineId) + " (attention des ml) ml")
      println(" Sucre restant : " + sugarStocks(machineId) + " g")

    } else {
      println("Code PIN incorrect. Accès refusé.")
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN :")
      val inputPin = readLine()
      if (inputPin == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      } else {
        tentatives -= 1
        println("Code PIN incorrect. Tentatives restantes :" + tentatives)
      }
    }
    false
  }

  def restockStocks(machineId: Int): Unit = {
    println("Réapprovisionnement des stocks.")
    println("Entrez la quantité de café à ajouter (g) :")
    coffeeStocks(machineId) += readInt()
    println("Entrez la quantité de sucre à ajouter (g) :")
    sugarStocks(machineId) += readInt()
    println("Entrez la quantité de lait à ajouter (attention ici ce sont des litres)(L) :")
    milkStocks(machineId) += (readDouble() * 1000).toInt
    println("Stocks mis à jour avec succès.")
    Thread.sleep(3000)
    println("Retour au menu principal...")
  }

  def serveClient(machineId: Int): Boolean = {
    var drinkChoice = ""
    do {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      drinkChoice = readLine()
      if (drinkChoice != "1" && drinkChoice != "2" && drinkChoice != "3") {
        println("Option invalide. Veuillez entrer 1, 2 ou 3.")
      }
    } while (drinkChoice != "1" && drinkChoice != "2" && drinkChoice != "3")

    var prixBase = 0.0
    var cafeQuantite = 0
    var laitQuantite = 0


    if (drinkChoice == "1") {
      prixBase = 2.00
      cafeQuantite = 8
    } else if (drinkChoice == "2") {
      prixBase = 2.50
      cafeQuantite = 6
      laitQuantite = 100
    } else if (drinkChoice == "3") {
      var sizeChoice = ""
      do {
        println("Choisissez une taille pour le Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")
        sizeChoice = readLine()
        if (sizeChoice != "1" && sizeChoice != "2" && sizeChoice != "3") {
          println("Option invalide. Veuillez entrer 1, 2 ou 3.")
        }
      } while (sizeChoice != "1" && sizeChoice != "2" && sizeChoice != "3")

      if (sizeChoice == "1") {
        prixBase = 2.70
        cafeQuantite = 6
        laitQuantite = 120
      } else if (sizeChoice == "2") {
        prixBase = 3.20
        cafeQuantite = 8
        laitQuantite = 150
      } else if (sizeChoice == "3") {
        prixBase = 3.70
        cafeQuantite = 12
        laitQuantite = 200
      }
    }

    var sucreQuantite = 0
    var sucrePrix = 0.00
    var sugarChoice = ""

    do {

      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")

      sugarChoice = readLine()

      if (sugarChoice != "1" && sugarChoice != "2" && sugarChoice != "3" && sugarChoice != "4") {
        println("Option invalide. Veuillez entrer 1, 2, 3 ou 4.")
      }
    } while (sugarChoice != "1" && sugarChoice != "2" && sugarChoice != "3" && sugarChoice != "4")

    if (sugarChoice == "1") {
      sucreQuantite = 0
      sucrePrix = 0.00
    } else if (sugarChoice == "2") {
      sucreQuantite = 5
      sucrePrix = 0.10
    } else if (sugarChoice == "3") {
      sucreQuantite = 10
      sucrePrix = 0.20
    } else if (sugarChoice == "4") {
      sucreQuantite = 15
      sucrePrix = 0.30
    } else {
      println("Option invalide pour le sucre.")
      return false
    }
    var laitprix = 0.0

    if (drinkChoice == "2" || drinkChoice =="3") {
      var validMilk = false
      do {
        println("Souhaitez-vous ajouter du lait supplémentaire ?\n1) Oui\n2) Non")
        print("> ")
        val addMilk = readLine()
        if (addMilk == "1") {
          println("Combien de dose ? entre (0-3)")
          var validDose = false
          do {
            print("> ")
            val doses = readLine().toIntOption.getOrElse(-1)
            if (doses >= 0 && doses <= 3) {
              laitQuantite += doses * 50
              laitprix = doses * 0.05
              validDose = true
              validMilk = true
            } else {
              println("Quantité de doses invalide.  Réessayer")
            }
          } while (!validDose)
        } else if (addMilk == "2") {
          validMilk = true
        } else {
          println("Option invalide pour le lait.  Réessayer")
        }
      } while (!validMilk)
    }






    // Vérification des stocks
    if (cafeQuantite > coffeeStocks(machineId)) {
      println("Quantité insuffisante de café.Choisissez une autre boisson")
      return false
    }
    if (laitQuantite > milkStocks(machineId)) {
      println("Quantité insuffisante de lait.Choisissez une autre boisson")
      return false
    }
    if (sucreQuantite > sugarStocks(machineId)) {
      println("Quantité insuffisante de sucre. Choisissez une autre boisson")
      return false
    }

    // Mise à jour des stocks
    coffeeStocks(machineId) -= cafeQuantite
    milkStocks(machineId) -= laitQuantite
    sugarStocks(machineId) -= sucreQuantite

    // Calcul du prix total
    val prixTotal = prixBase + sucrePrix + laitprix
    println(f"Prix total : CHF $prixTotal%.2f")
    val paymentCode = Random.alphanumeric.take(5).mkString.toUpperCase()
    println("Votre code de paiement est :" + paymentCode + " ")
    println("(En attente de validation du paiement...)")
    Thread.sleep(5000)
    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...\n [...]\n")
    Thread.sleep(5000)
    println("Votre boisson est prêt ! Bonne dégustation !\n")



    // Ne pas afficher les stocks ici, seulement en mode admin
    true
  }
}
