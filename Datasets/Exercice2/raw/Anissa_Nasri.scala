import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val coffeeStocks = Array(50, 50, 50, 50, 50)
    val sugarStocks = Array(30, 30, 30, 30, 30)
    val milkStocks = Array(500, 500, 500, 500, 500)
    val machinePins = Array("434343", "434343", "434343", "434343", "434343")

    var programmeEnCours = true

    while (programmeEnCours) {
      println("Nospresso Café\n1) Client\n2) Admin\n3) Quitter\n>")
      val choixMode = readInt()

      if (choixMode == 3) {
        programmeEnCours = false
      } else if (choixMode == 1 || choixMode == 2) {
        var machineId = -1
        while (machineId < 0 || machineId >= nbMachines) {
          println("Machine sélectionnée (1-5) >")
          machineId = readInt() - 1
          if (machineId < 0 || machineId >= nbMachines) {
            println("Numéro de machine invalide. Veuillez réessayer.")
          }
        }

        if (choixMode == 1) {
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } else if (choixMode == 2) {
          if (validatePin(machineId, machinePins)) {
            adminMode(machineId, coffeeStocks, sugarStocks, milkStocks, machinePins)
          } else {
            println("Trop de tentatives échouées. Fin du programme.")
            programmeEnCours = false
          }
        }
      } else {
        println("Choix invalide. Veuillez réessayer et choisir entre 1 et 3.")
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val tentativesMax = 3
    var tentativesRestantes = tentativesMax

    while (tentativesRestantes > 0) {
      println("Entrez le code PIN pour la machine " + (machineId + 1) + "\n>")
      val pin = readLine()
      if (pin == machinePins(machineId)) {
        println("Accès accordé à la machine. " + (machineId + 1))
        return true
      } else {
        tentativesRestantes -= 1
        if (tentativesRestantes > 0) {
          println("Code PIN incorrect. Il vous reste " + tentativesRestantes + " tentative(s).")
        } else {
          println("Trop de tentatives échouées.")
        }
      }
    }
    false
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var continuerCommande = true

    while (continuerCommande) {
      var choixBoisson = 0

      // Choix de la boisson
      while (choixBoisson < 1 || choixBoisson > 3) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choixBoisson = readInt()

        if (choixBoisson < 1 || choixBoisson > 3) {
          println("Boisson invalide. Veuillez entrer une valeur entre 1 et 3.")
        }
      }

      var cafedose = 0
      var laitdose = 0
      var prixBoisson = 0.0
      var boissonFin = ""

      // Définition des doses et prix selon la boisson
      if (choixBoisson == 1) {
        cafedose = 8
        laitdose = 0
        prixBoisson = 2.0
        boissonFin = "Expresso"
      } else if (choixBoisson == 2) {
        cafedose = 6
        laitdose = 100
        prixBoisson = 2.5
        boissonFin = "Cappuccino"
      } else if (choixBoisson == 3) {
        println("Quelle taille de latte voulez-vous ?\n1) Petit\n2) Moyen\n3) Grand")
        var taille = 0
        while (taille < 1 || taille > 3) {
          taille = readInt()
          if (taille == 1) {
            cafedose = 6
            laitdose = 120
            prixBoisson = 2.7
            boissonFin = "Petit Latte"
          } else if (taille == 2) {
            cafedose = 8
            laitdose = 150
            prixBoisson = 3.2
            boissonFin = "Moyen Latte"
          } else if (taille == 3) {
            cafedose = 12
            laitdose = 200
            prixBoisson = 3.7
            boissonFin = "Grand Latte"
          } else {
            println("Taille invalide. Veuillez choisir entre 1 et 3.")
          }
        }
      }

      // Gestion du sucre
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      val choixSucre = readInt()
      var sucredose = 0
      var prixSucre = 0.0

      if (choixSucre == 2) {
        sucredose = 5
        prixSucre = 0.1
      } else if (choixSucre == 3) {
        sucredose = 10
        prixSucre = 0.2
      } else if (choixSucre == 4) {
        sucredose = 15
        prixSucre = 0.3
      }

      // Gestion du lait supplémentaire
      println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
      val choixLait = readInt()
      var laitSupplementaire = 0
      var prixLait = 0.0

      if (choixLait == 1 && choixBoisson != 1) {
        println("Combien de doses de lait ? (1 à 3 doses)")
        var doses = readInt()
        if (doses < 1) doses = 1
        if (doses > 3) doses = 3
        laitSupplementaire = doses * 50
        prixLait = doses * 0.10
        laitdose += laitSupplementaire
      } else if (choixLait == 1) {
        println("On ne peut pas ajouter de lait pour un expresso.")
      }

      // Vérification des stocks
      if (coffeeStocks(machineId) < cafedose) {
        println("Stock insuffisant de café.")
        return false
      } else if (sugarStocks(machineId) < sucredose) {
        println("Stock insuffisant de sucre.")
        return false
      } else if (milkStocks(machineId) < laitdose) {
        println("Stock insuffisant de lait.")
        return false
      } else {
        // Mise à jour des stocks
        coffeeStocks(machineId) -= cafedose
        sugarStocks(machineId) -= sucredose
        milkStocks(machineId) -= laitdose

        // Affichage du récapitulatif
        val prixTotal = prixBoisson + prixSucre + prixLait
        println("Boisson sélectionnée: " + boissonFin)
        if (prixSucre > 0) {
          println("Niveau de sucre: " + sucredose + "g")
        } else {
          println("Niveau de sucre: Sans sucre")
        }
        if (prixLait > 0) {
          println("Lait supplémentaire: " + (laitSupplementaire / 50) + " doses")
        } else {
          println("Lait supplémentaire: Aucun")
        }
        println("Prix total: CHF " + "%.2f".format(prixTotal))

        // Simulation du paiement
        val codeTwint = Random.alphanumeric.take(5).mkString
        println("Veuillez payer avec Twint. Votre code de paiement est: " + codeTwint)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("Paiement accepté. Merci pour votre achat.")

        println("Votre " + boissonFin + " est prêt! Bonne dégustation!")
        return true
      }
    }
    false
  }

  def adminMode(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], machinePins: Array[String]): Unit = {
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + coffeeStocks(machineId) + "g")
    println("Sucre : " + sugarStocks(machineId) + "g")
    printf("Lait : %.2f L\n", milkStocks(machineId) / 1000.0)

    println("1) Recharger les stocks\n2) Modifier le PIN")
    val choixAdmin = readInt()

    if (choixAdmin == 1) {
      restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
    } else if (choixAdmin == 2) {
      updatePin(machineId, machinePins)
    } else {
      println("Choix invalide.")
    }
  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Entrez les quantités à ajouter:")
    println("Poudre de café (en g) >")
    val cafeAjoute = readInt()
    println("Sucre (en g) >")
    val sucreAjoute = readInt()
    println("Lait (en L) >")
    val laitAjoute = (readDouble() * 1000).toInt // Conversion litres vers ml

    if (cafeAjoute >= 0 && sucreAjoute >= 0 && laitAjoute >= 0) {
      coffeeStocks(machineId) += cafeAjoute
      sugarStocks(machineId) += sucreAjoute
      milkStocks(machineId) += laitAjoute
      println("Les stocks ont été mis à jour avec succès.")
    } else {
      println("Les quantités doivent être positives.")
    }
  }


  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Entrez un nouveau code PIN (6 chiffres) :")
    var nouveauPin = readLine()

    var chiffres = true
    while (nouveauPin.length != 6 || {
      chiffres = true
      for (i <- 0 until nouveauPin.length) {
        if (nouveauPin(i) < '0' || nouveauPin(i) > '9') {
          chiffres = false
        }
      }
      !chiffres
    }) {
      println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer :")
      nouveauPin = readLine()
    }
    machinePins(machineId) = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
  }
}