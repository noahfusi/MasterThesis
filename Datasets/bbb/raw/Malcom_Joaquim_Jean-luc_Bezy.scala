import io.StdIn._
import math._
import scala.util.Random





object Main {
  def main(args: Array[String]): Unit = {
    // initialisation stocks et codes PIN
    val nbMachines = 5
    val cafeStock = Array.fill(nbMachines)(50)    // 50g de café
    val sucreStock = Array.fill(nbMachines)(30)   // 30g de sucre
    val laitStock = Array.fill(nbMachines)(500)   // 500ml de lait
    val pins = Array.fill(nbMachines)("434343")   // Codes PIN initiaux

    var continuer = true // boucle principale

    while (continuer) {
      // affichage du menu principal
      var mode = 0
      while (mode != 1 && mode != 2 && mode != 3) {
        println("        Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine("> ").toIntOption.getOrElse(0) // 0 si l'entrée est invalide
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Option invalide. Veuillez réessayer.")
        }
      }

      if (mode == 1) { // mode client
        var machineChoisie = -1
        var commandeReussie = false

        while (!commandeReussie) {
          // choix machine
          while (machineChoisie < 0 || machineChoisie >= nbMachines) {
            println("Choisissez une machine (1 à 5) :")
            machineChoisie = readLine("> ").toIntOption.getOrElse(-1) - 1
            if (machineChoisie < 0 || machineChoisie >= nbMachines) {
              println("Machine invalide. Veuillez réessayer.")
            }
          }

          // tenter de servir la commande
          commandeReussie = serveClient(machineChoisie, cafeStock, sucreStock, laitStock)

          // si commande échoue, retour sélection de machine
          if (!commandeReussie) {
            println("Veuillez sélectionner une autre machine.")
            machineChoisie = -1 // choisir autre machine
          }
        }
      } else if (mode == 2) { // mode admin
        var machineId = -1
        while (machineId < 0 || machineId >= nbMachines) {
          println("Choisissez une machine (1 à 5) :")
          machineId = readLine("> ").toIntOption.getOrElse(-1) - 1
          if (machineId < 0 || machineId >= nbMachines) {
            println("Machine invalide. Veuillez réessayer.")
          }
        }

        if (validatePin(machineId, pins)) {
          var choixAdmin = 0
          while (choixAdmin != 1 && choixAdmin != 2) {
            println("Que souhaitez-vous faire ?")
            println("1) Réapprovisionner les stocks")
            println("2) Modifier le code PIN")
            choixAdmin = readLine("> ").toIntOption.getOrElse(0)
            if (choixAdmin != 1 && choixAdmin != 2) {
              println("Option invalide. Veuillez réessayer.")
            }
          }

          if (choixAdmin == 1) restockMachine(machineId, cafeStock, sucreStock, laitStock)
          else if (choixAdmin == 2) updatePin(machineId, pins)
        }
      } else if (mode == 3) { // quitter le programme
        println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
        continuer = false
      }
    }
    println("Programme terminé.")
  }




  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val maxAttempts = 3
    var attempts = 0
    var isValid = false

    println(s"Machine ${machineId + 1}: Entrez le code PIN :") // une seule fois avant les tentatives

    while (attempts < maxAttempts && !isValid) {
      val pinEntered = readLine("> ")

      if (pinEntered == machinePins(machineId)) {
        println(s"Accès accordé à la Machine ${machineId + 1}.")
        isValid = true
      } else {
        attempts += 1
        val remainingAttempts = maxAttempts - attempts
        if (remainingAttempts > 0) {
          println(s"Code PIN incorrect. $remainingAttempts tentative(s) restante(s).")
        } else {
          println("Code PIN incorrect. 0 tentative restante.")
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(0) // terminer le programme
        }
      }
    }

    isValid
  }



  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    println("Mise à jour du code PIN pour la machine " + (machineId + 1) + ".")
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      newPin = readLine()
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))

    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels :")
    println(s"Poudre de café : ${coffeeStocks(machineId)}g")
    println(s"Sucre : ${sugarStocks(machineId)}g")
    println(f"Lait : ${milkStocks(machineId) / 1000.0}%.1fL")

    println("Entrez les quantités à ajouter :")
    print("Poudre de café > ")
    coffeeStocks(machineId) += readLine().toInt

    print("Sucre > ")
    sugarStocks(machineId) += readLine().toInt

    print("Lait > ")
    val laitAjoute = readLine().replace(",", ".").toDouble
    milkStocks(machineId) += (laitAjoute * 1000).toInt

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...\n")
  }



  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var cafeNecessaire = 0
    var laitNecessaire = 0
    var sucreNecessaire = 0
    var prixBase = 0.0
    var prixLaitSupp = 0.0
    var prixSucre = 0.0
    var prixTotal = 0.0
    var niveauSucre = "Sans sucre"
    var laitSupplementaire = "Non"

    // choix boisson
    var boisson = 0
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      boisson = readLine("> ").toIntOption.getOrElse(0)
    }

    if (boisson == 1) { // expresso
      cafeNecessaire = 8
      prixBase = 2.00
    } else if (boisson == 2) { // cappuccino
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.50
    } else if (boisson == 3) { // latte
      var taille = 0
      while (taille != 1 && taille != 2 && taille != 3) {
        println("Veuillez choisir la taille de votre Latte :")
        println("(1) Petit")
        println("(2) Moyen")
        println("(3) Grand")
        taille = readLine("> ").toIntOption.getOrElse(0)
      }

      if (taille == 1) {
        cafeNecessaire = 6
        laitNecessaire = 120
        prixBase = 2.70
      } else if (taille == 2) {
        cafeNecessaire = 8
        laitNecessaire = 150
        prixBase = 3.20
      } else if (taille == 3) {
        cafeNecessaire = 12
        laitNecessaire = 200
        prixBase = 3.70
      }
    }

    // vérifications des stocks
    if (coffeeStocks(machineId) < cafeNecessaire) {
      println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
      return false // retourner à la sélection de la machine
    }
    if (milkStocks(machineId) < laitNecessaire) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      return false
    }
    if (sugarStocks(machineId) < sucreNecessaire) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      return false
    }

    // sucre
    var sucre = 0
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("(1) Sans sucre")
      println("(2) Peu (5g)")
      println("(3) Moyen (10g)")
      println("(4) Beaucoup (15g)")
      sucre = readLine("> ").toIntOption.getOrElse(0)
    }

    if (sucre == 1) {
      niveauSucre = "Sans sucre"
      prixSucre = 0.0
      sucreNecessaire = 0
    } else if (sucre == 2) {
      niveauSucre = "Peu (5g)"
      prixSucre = 0.10
      sucreNecessaire = 5
    } else if (sucre == 3) {
      niveauSucre = "Moyen (10g)"
      prixSucre = 0.20
      sucreNecessaire = 10
    } else if (sucre == 4) {
      niveauSucre = "Beaucoup (15g)"
      prixSucre = 0.30
      sucreNecessaire = 15
    }

    if (sugarStocks(machineId) < sucreNecessaire) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      return false
    }

    // lait supplémentaire
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      var choixLait = 0
      while (choixLait != 1 && choixLait != 2) {
        choixLait = readLine("> ").toIntOption.getOrElse(0)
      }

      if (choixLait == 1) {
        var dosesLait = 0
        while (dosesLait < 1 || dosesLait > 3) {
          println("Combien de dose ? (1 à 3 doses)")
          dosesLait = readLine("> ").toIntOption.getOrElse(0)
        }

        if (milkStocks(machineId) >= dosesLait * 50) {
          laitSupplementaire = s"$dosesLait dose(s)"
          prixLaitSupp = dosesLait * 0.05
          milkStocks(machineId) -= dosesLait * 50
        } else {
          println("Erreur : Quantité de lait insuffisante pour le supplément.")
          laitSupplementaire = "Non"
          prixLaitSupp = 0.0
        }
      }
    }


    // mise à jour des stocks
    coffeeStocks(machineId) -= cafeNecessaire
    milkStocks(machineId) -= laitNecessaire
    sugarStocks(machineId) -= sucreNecessaire

    // calcul du prix total
    prixTotal = prixBase + prixSucre + prixLaitSupp

    // génération du code Twint
    val caracteres = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var codeTwint = ""
    for (i <- 1 to 5) {
      val choixCaracteres = (Math.random() * 62).toInt
      codeTwint += caracteres(choixCaracteres)
    }

    // affichage final
    println("Boisson sélectionnée : " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else s"Latte (${if (laitNecessaire == 120) "Petit" else if (laitNecessaire == 150) "Moyen" else "Grand"})"))
    println("Niveau de sucre : " + niveauSucre)
    println("Lait supplémentaire : " + laitSupplementaire)
    println("Prix total : CHF " + "%.2f".format(prixBase) + " + CHF " + "%.2f".format(prixSucre) + " + CHF " + "%.2f".format(prixLaitSupp) + " = CHF " + "%.2f".format(prixTotal))
    println("\nVeuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + codeTwint)
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("\nPaiement confirmé.")
    println("Préparation de votre boisson...")
    println("Votre " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte") + " est prêt ! Bonne dégustation !")

    true
  }
}




