import scala.io.StdIn._

object Nospresso {

  // Définition des constantes
  val nbMachines = 5
  val maxTentativesPin = 3
  val codePinDefault = "434343"

  // Déclaration des tableaux pour gérer les stocks et les codes PIN
  var cafeStocks: Array[Int] = Array(50, 60, 70, 80, 90)
  var sucreStocks: Array[Int] = Array(30, 30, 30, 30, 30)
  var laitStocks: Array[Double] = Array(500.0, 500.0, 500.0, 500.0, 500.0)
  var machinePins: Array[String] = Array.fill(nbMachines)(codePinDefault)

  // Fonction de validation du code PIN
  def validatePin(machineId: Int): Boolean = {
    var attempts = 0
    var pinCorrect = false
    while (attempts < maxTentativesPin && !pinCorrect) {
      println("Entrez le code PIN pour la Machine " + (machineId + 1) + " :")
      val codeSaisi = readLine()
      if (codeSaisi == machinePins(machineId)) {
        pinCorrect = true
      } else {
        attempts += 1
        if (attempts < maxTentativesPin) {
          println("Code PIN incorrect. " + (maxTentativesPin - attempts) + " tentatives restantes.")
        }
      }
    }
    if (!pinCorrect) {
      println("Trop de tentatives échouées. Fin du programme.")
    }
    pinCorrect
  }

  // Fonction pour mettre à jour le code PIN
  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres pour la Machine " + (machineId + 1) + ":")
    var newPin = readLine()
    while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
      println("Le code PIN doit être composé de 6 chiffres. Veuillez réessayer.")
      newPin = readLine()
    }
    machinePins(machineId) = newPin
    println("Le code PIN pour la Machine " + (machineId + 1) + " a été mis à jour avec succès.")
  }

  // Fonction pour réapprovisionner les stocks
  def restockMachine(machineId: Int): Unit = {
    println("Réapprovisionnement des stocks pour la Machine " + (machineId + 1) + ":")
    println("Combien de poudre de café souhaitez-vous ajouter ? (en grammes)")
    cafeStocks(machineId) += readLine().toInt

    println("Combien de lait souhaitez-vous ajouter ? (en millilitres)")
    laitStocks(machineId) += readLine().toDouble

    println("Combien de sucre souhaitez-vous ajouter ? (en grammes)")
    sucreStocks(machineId) += readLine().toInt

    println("Réapprovisionnement effectué avec succès !")
  }

  // Fonction pour afficher les stocks actuels dans le mode Admin
  def showStocks(machineId: Int): Unit = {
    println("Stocks actuels pour la Machine " + (machineId + 1) + ":")
    println("- Café : " + cafeStocks(machineId) + " g")
    println("- Sucre : " + sucreStocks(machineId) + " g")
    println("- Lait : " + laitStocks(machineId) + " ml")
  }

  // Fonction pour gérer les commandes en mode Client
  def serveClient(machineId: Int): Boolean = {
    println("Machine " + (machineId + 1) + " sélectionnée.")
    println("Veuillez sélectionner un café :")
    println("1) Expresso - 2.00 CHF ")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")

    var choixCafé = 0
    while (choixCafé != 1 && choixCafé != 2 && choixCafé != 3) {
      choixCafé = readLine("> ").toInt
      if (choixCafé != 1 && choixCafé != 2 && choixCafé != 3) {
        println("Entrée invalide, veuillez choisir 1, 2 ou 3.")
      }
    }

    if (choixCafé == 1 && cafeStocks(machineId) < 8 ||
      choixCafé == 2 && cafeStocks(machineId) < 6 ||
      choixCafé == 3 && cafeStocks(machineId) < 6) {
      println("Erreur : Quantité de café insuffisante.")
      return false
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10 CHF")
    println("3) Moyen (10g) - 0.20 CHF")
    println("4) Beaucoup (15g) - 0.30 CHF")

    var sucre = 0
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
      sucre = readLine("> ").toInt
      if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        println("Entrée invalide, veuillez choisir 1, 2, 3 ou 4.")
      }
    }

    var prix = 0.0
    if (choixCafé == 1) {
      cafeStocks(machineId) -= 8
      prix = 2.00
    } else if (choixCafé == 2) {
      cafeStocks(machineId) -= 6
      laitStocks(machineId) -= 100
      prix = 2.50
    } else if (choixCafé == 3) {
      println("Quelle taille de Latte ?")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")

      var tailleLatte = 0
      while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
        tailleLatte = readLine("> ").toInt
        if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
          println("Entrée invalide, veuillez choisir 1, 2 ou 3.")
        }
      }

      if (tailleLatte == 1) {
        cafeStocks(machineId) -= 6
        laitStocks(machineId) -= 120
        prix = 2.70
      } else if (tailleLatte == 2) {
        cafeStocks(machineId) -= 8
        laitStocks(machineId) -= 150
        prix = 3.20
      } else if (tailleLatte == 3) {
        cafeStocks(machineId) -= 12
        laitStocks(machineId) -= 200
        prix = 3.70
      }
    }

    if (sucre == 2) {
      sucreStocks(machineId) -= 5
      prix += 0.10
    } else if (sucre == 3) {
      sucreStocks(machineId) -= 10
      prix += 0.20
    } else if (sucre == 4) {
      sucreStocks(machineId) -= 15
      prix += 0.30
    }

    println("Prix total : " + "%.2f".format(prix) + " CHF")
    println("Veuillez payer en utilisant Twint...")
    println("Paiement accepté. Votre boisson est prête !")

    true
  }

  def main(args: Array[String]): Unit = {
    var mode = 0
    var execution = true

    while (execution) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode = readLine("> ").toInt

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Entrée invalide, veuillez choisir 1, 2 ou 3.")
        mode = readLine("> ").toInt
      }

      if (mode == 1) {
        println("Sélectionnez la machine (1-5) :")
        var machineId = 0
        while (machineId < 1 || machineId > nbMachines) {
          machineId = readLine("> ").toInt
          if (machineId < 1 || machineId > nbMachines) {
            println("Machine non valide. Veuillez choisir une machine entre 1 et 5.")
          }
        }
        if (!serveClient(machineId - 1)) {
          println("Erreur dans la commande. Essayez une autre machine.")
        }
      } else if (mode == 2) {
        println("Sélectionnez la machine (1-5) :")
        var machineIdAdmin = 0
        while (machineIdAdmin < 1 || machineIdAdmin > nbMachines) {
          machineIdAdmin = readLine("> ").toInt
          if (machineIdAdmin < 1 || machineIdAdmin > nbMachines) {
            println("Machine non valide. Veuillez choisir une machine entre 1 et 5.")
          }
        }
        if (validatePin(machineIdAdmin - 1)) {
          println("Accès administrateur accordé.")
          showStocks(machineIdAdmin - 1)
          println("1) Réapprovisionner les stocks")
          println("2) Mettre à jour le code PIN")

          var action = 0
          while (action != 1 && action != 2) {
            action = readLine("> ").toInt
            if (action != 1 && action != 2) {
              println("Option non valide, veuillez choisir 1 ou 2.")
            }
          }
          if (action == 1) {
            restockMachine(machineIdAdmin - 1)
          } else if (action == 2) {
            updatePin(machineIdAdmin - 1)
          }
        } else {
          println("Accès refusé en raison de trop de tentatives échouées.")
        }
      } else if (mode == 3) {
        execution = false
        println("Au revoir !")
      }
    }
  }
}
