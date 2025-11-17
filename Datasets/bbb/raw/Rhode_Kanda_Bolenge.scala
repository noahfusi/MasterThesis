import scala.io.StdIn




object Main {
  def main(args: Array[String]): Unit = {
    var continuer = true

    object NospressoMultiMachines {
      val nbMachines: Int = 5
      val StockCafe: Array[Double] = Array(50.0, 50.0, 50.0, 50.0, 50.0) // Stock en grammes
      val StockSucre: Array[Double] = Array(30.0, 30.0, 30.0, 30.0, 30.0) // Stock en grammes
      val StockLait: Array[Double] = Array(500.0, 500.0, 500.0, 500.0, 500.0) // Stock en millilitres
      val codesPIN: Array[Int] = Array(434343, 434344, 434345, 434346, 434347)


      def afficherMenu(): Unit = {
        var continuer = true
        while (continuer) {
          println("Bienvenue ! Nospresso Multi-Machines")
          println("Veuillez sélectionner votre mode :")
          println("1. Mode client")
          println("2. Mode admin")
          println("3. Quitter")

          val choix = lireEntreeValide("Entrez votre choix (1, 2, 3) : ")
          if (choix == 1) {
            modeClient()
          } else if (choix == 2) {
            modeAdmin()
          } else if (choix == 3) {
            println("Au revoir!")
            continuer = false
          } else {
            println("Choix invalide, veuillez réessayer.")
          }
        }
      }

      def modeClient(): Unit = {
        val machineId = selectMachine()
        if (machineId >= 0) {
          val success = serveClient(machineId)
          if (!success) {
            println("Transaction échouée. Essayez une autre machine.")
          }
        }
      }

      def modeAdmin(): Unit = {
        val machineId = selectMachine()
        if (machineId >= 0 && validPin(machineId)) {
          println("1. Afficher les stocks")
          println("2. Réapprovisionner les stocks")
          println("3. Mettre à jour le code PIN")
          println("4. Retour")

          val choixAdmin = lireEntreeValide("Entrez votre choix : ")
          if (choixAdmin == 1) {
            affichageStocks(machineId)
          } else if (choixAdmin == 2) {
            reapprovisionnerStocks(machineId)
          } else if (choixAdmin == 3) {
            updatePin(machineId)
          } else {
            println("Choix invalide.")
          }
        }
      }

      def selectMachine(): Int = {
        val machineId = lireEntreeValide("Sélectionnez une machine (1-" + nbMachines + ") : ")
        if (machineId >= 1 && machineId <= nbMachines) {
          machineId - 1
        } else {
          println("ID de machine invalide.")
          -1
        }
      }

      def validPin(machineId: Int): Boolean = {
        var attempts = 0
        val maxAttempts = 3

        while (attempts < maxAttempts) {
          val enteredPin = lireEntreeValide("Entrez le code PIN : ")
          if (enteredPin == codesPIN(machineId)) {
            println("Accès accordé à la Machine " + (machineId + 1) + ".")
            return true
          } else {
            attempts += 1
            println("Code PIN incorrect. Tentatives restantes : " + (maxAttempts - attempts))
          }
        }

        println("Trop de tentatives échouées. Fin du programme.")
        false
      }

      def affichageStocks(machineId: Int): Unit = {
        println("Niveaux de stock actuels pour la machine " + (machineId + 1) + " :")
        println("Poudre de café : " + StockCafe(machineId) + " g")
        println("Sucre : " + StockSucre(machineId) + " g")
        println("Lait : " + StockLait(machineId) + " mL")
      }

      def reapprovisionnerStocks(machineId: Int): Unit = {
        println("Sélection de la Machine " + (machineId + 1) + ".")
        affichageStocks(machineId)

        val cafeToAdd = lireEntreeValide("Poudre de café à ajouter (en grammes) : ").toDouble
        val sucreToAdd = lireEntreeValide("Sucre à ajouter (en grammes) : ").toDouble
        val laitToAdd = lireEntreeValide("Lait à ajouter (en mililitres) : ").toDouble

        StockCafe(machineId) += cafeToAdd
        StockSucre(machineId) += sucreToAdd
        StockLait(machineId) += laitToAdd

        println("Les stocks ont été mis à jour avec succès.")
        println("Nouveaux stocks pour la machine " + (machineId + 1) + " :")
        affichageStocks(machineId)
      }

      def updatePin(machineId: Int): Unit = {
        println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
        val newPin = lireEntreeValide("Entrez un nouveau code PIN à 6 chiffres : ")

        if (newPin.toString.length == 6) {
          codesPIN(machineId) = newPin
          println("Le code PIN a été mis à jour avec succès.")
        } else {
          println("Le code PIN doit comporter exactement 6 chiffres.")
        }
      }

      def serveClient(machineId: Int): Boolean = {
        println("Choisissez votre boisson :")
        println("1) Expresso - 10g café")
        println("2) Cappuccino - 10g café, 100ml lait")
        println("3) Latte - 15g café, 150ml lait")

        val choixBoisson = lireEntreeValide("Entrez votre choix : ")
        var coffeeNeeded = 0
        var milkNeeded = 0

        if (choixBoisson == 1) {
          coffeeNeeded = 10
        } else if (choixBoisson == 2) {
          coffeeNeeded = 10
          milkNeeded = 100
        } else if (choixBoisson == 3) {
          coffeeNeeded = 15
          milkNeeded = 150
        } else {
          println("Choix invalide.")
          return false
        }

        println("Souhaitez-vous ajouter du sucre ? (5g par portion)")
        println("1) Non 2) Oui")
        val addSugar = lireEntreeValide("Entrez votre choix : ")
        val sugarNeeded = if (addSugar == 2) 5 else 0

        if (StockCafe(machineId) < coffeeNeeded || StockLait(machineId) < milkNeeded || StockSucre(machineId) < sugarNeeded) {
          println("Stocks insuffisants pour cette commande.")
          return false
        }

        StockCafe(machineId) -= coffeeNeeded
        StockLait(machineId) -= milkNeeded
        StockSucre(machineId) -= sugarNeeded

        println("Paiement via Twint...")
        println("Merci pour votre commande!")
        true
      }

      def lireEntreeValide(message: String): Int = {
        var entreeValide = false
        var resultat = 0
        while (!entreeValide) {
          try {
            print(message)
            resultat = scala.io.StdIn.readLine().toInt
            entreeValide = true
          } catch {
            case _: NumberFormatException =>
              println("Veuillez entrer un nombre valide.")
          }
        }
        resultat
      }
    }
    NospressoMultiMachines.afficherMenu()
      }
    }
