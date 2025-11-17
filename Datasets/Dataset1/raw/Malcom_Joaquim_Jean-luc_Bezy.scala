import io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var stockCafe = 50 // en g
    var stockSucre = 30 // en g
    var stockLait = 500 // en ml
    var mode = 0
    var cafeNecessaire = 0 // en g
    var laitNecessaire = 0 // en ml
    var sucreNecessaire = 0 // en g
    var niveauSucre = "Sans sucre" // Niveau de sucre demandé
    var laitSupplementaire = "Non" // Information sur le supplément de lait
    var prixBase = 0.0 // Prix de base de la boisson
    var prixLaitSupp = 0.0 // Coût du lait supplémentaire
    var prixSucre = 0.0 // Coût du sucre ajouté
    var prixTotal = 0.0 // Prix total de la commande
    var continuer = true

    while (continuer) {
      // Affichage du menu principal
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      // Choix du mode
      mode = readLine("> ").toInt

      if (mode == 1) { // Mode Client
        var boisson = 0
        while (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          boisson = readLine("> ").toInt
        }

        // Gestion boisson
        if (boisson == 1) { // Expresso
          cafeNecessaire = 8
          laitNecessaire = 0
          prixBase = 2.00
        } else if (boisson == 2) { // Cappuccino
          cafeNecessaire = 6
          laitNecessaire = 100
          prixBase = 2.50
        } else if (boisson == 3) { // Latte
          var taille = 0
          while (taille != 1 && taille != 2 && taille != 3) {
            println("Veuillez choisir la taille de votre Latte :")
            println("(1) Petit")
            println("(2) Moyen")
            println("(3) Grand")
            taille = readLine("> ").toInt
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

        // Vérification des stocks
        var erreur = false

        if (stockCafe < cafeNecessaire) {
          println("Boisson sélectionnée : " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte"))
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          erreur = true
        } else if (boisson != 1 && stockLait < laitNecessaire) { // Lait insuffisant pour Cappuccino ou Latte
          println(
            "Boisson sélectionnée : " +
              (if (boisson == 3)
                "Latte (" +
                  (if (laitNecessaire == 120) "Petit"
                  else if (laitNecessaire == 150) "Moyen"
                  else "Grand") +
                  ")"
              else "Cappuccino")
          )
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          if (boisson == 3) {
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          } else {
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
          erreur = true
        }

        if (erreur) {
          println("Retour au menu principal...\n")
        } else {
          // Gestion sucre
          var sucre = 0
          while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("(1) Sans sucre")
            println("(2) Peu (5g)")
            println("(3) Moyen (10g)")
            println("(4) Beaucoup (15g)")
            sucre = readLine("> ").toInt
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

          if (stockSucre < sucreNecessaire) {
            println("Boisson sélectionnée : " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte"))
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            println("Retour au menu principal...\n")
          } else {
            stockSucre -= sucreNecessaire

            // doses de lait supplémentaire pour Cappuccino et Latte
            if (boisson == 2 || boisson == 3) {
              var choixLait = 0
              while (choixLait != 1 && choixLait != 2) {
                println("Souhaitez-vous ajouter du lait en supplément ?")
                println("1) Oui")
                println("2) Non")
                choixLait = readLine("> ").toInt
              }

              if (choixLait == 1) {
                var dosesLait = 0
                while (dosesLait < 1 || dosesLait > 3) {
                  println("Combien de dose ? (1 à 3 doses)")
                  dosesLait = readLine("> ").toInt
                }

                if (stockLait < dosesLait * 50) {
                  println("Erreur : Quantité de lait insuffisante pour le supplément.")
                  laitSupplementaire = "Non"
                  prixLaitSupp = 0.0
                } else {
                  laitSupplementaire = s"$dosesLait dose(s)"
                  prixLaitSupp = dosesLait * 0.05
                  stockLait -= dosesLait * 50
                }
              } else {
                laitSupplementaire = "Non"
                prixLaitSupp = 0.0
              }
            }

            // mise à jour des stocks après vérifications
            stockCafe -= cafeNecessaire
            stockLait -= laitNecessaire

            // Calcul et affichage des prix
            if (sucreNecessaire == 0 && laitSupplementaire == "Non") {
              prixTotal = prixBase
              println("Prix total : " + "%.2f".format(prixBase) + " CHF")
            } else if (sucreNecessaire > 0 && laitSupplementaire == "Non") {
              prixTotal = prixBase + prixSucre
              println("Prix total : %.2f".format(prixBase) + " + %.2f".format(prixSucre) + " CHF = " + "%.2f".format(prixTotal) + " CHF")
            } else if (sucreNecessaire == 0 && laitSupplementaire != "Non") {
              prixTotal = prixBase + prixLaitSupp
              println("Prix total : %.2f".format(prixBase) + " + %.2f".format(prixLaitSupp) + " CHF = " + "%.2f".format(prixTotal) + " CHF")
            } else {
              prixTotal = prixBase + prixSucre + prixLaitSupp
              println("Prix total : %.2f".format(prixBase) + " + %.2f".format(prixSucre) + " + %.2f".format(prixLaitSupp) + " CHF = " + "%.2f".format(prixTotal) + " CHF")
            }

            // Généré code twint
            val caracteres = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            var codeTwint = ""
            for (i <- 1 to 5) {
              val choixCaracteres = (Math.random() * 62).toInt
              codeTwint += caracteres(choixCaracteres)
            }

            // Affichage final
            println("Boisson sélectionnée : " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte"))
            println("Niveau de sucre : " + niveauSucre)
            println("Lait supplémentaire : " + laitSupplementaire)
            println("\nVeuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTwint)
            println("(En attente de paiement...)")
            Thread.sleep(3000)
            println("\nPaiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte") + " est prêt ! Bonne dégustation !")
          }
        }
      } else if (mode == 2) { // Mode admin
        val pinAdmin = 434343 // Code pin pour le mode admin
        var pin = 0

        while (pin != pinAdmin) {
          println("Mode Admin")
          println("Entrez le code PIN :")
          pin = readLine().toInt
        }

        println("Accès autorisé.")
        println("\nStocks :")
        println("Poudre de café: " + stockCafe + "g")
        println("Lait : " + (stockLait / 1000.0) + "L")
        println("Sucre : " + stockSucre + "g")
        println("\nRéapprovisionnement des stocks...")
        println("Ajout :")

        // Réapprovisionnement café
        print("Poudre de café : ")
        val ajoutCafe = readLine().toInt
        stockCafe += ajoutCafe

        // Réapprovisionnement lait
        print("Lait : ")
        val ajoutLait = (readLine().toDouble * 1000).toInt // Convertir les litres en ml
        stockLait += ajoutLait

        // Réapprovisionnement sucre
        print("Sucre : ")
        val ajoutSucre = readLine().toInt
        stockSucre += ajoutSucre

        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal...\n")
      }
      else if (mode == 3) { // Mode quitter
        continuer = false
      }
    }
  }
}









