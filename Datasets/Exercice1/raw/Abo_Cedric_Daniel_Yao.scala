import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    // Stocks initiales
    var Stockcafe = 50.0 // en grammes
    var Stocksucre = 30.0 // en grammes
    var Stocklait = 0.5 // en litres

    // Prix de base des boissons
    val prixExpresso = 2.0
    val prixCappuccino = 2.5
    val prixLattePetit = 2.7
    val prixLatteMoyen = 3.2
    val prixLatteGrand = 3.7

    // Quantités des ingrédients nécessaires pour chaques  boissons
    val cafeExpresso = 8.0
    val cafeCappuccino = 6.0
    val cafeLattePetit = 6.0
    val cafeLatteMoyen = 8.0
    val cafeLatteGrand = 12.0

    val laitCappuccino = 0.1 // en litres (100 ml)
    val laitLattePetit = 0.12 // en litres (120 ml)
    val laitLatteMoyen = 0.15 // en litres (150 ml)
    val laitLatteGrand = 0.2 // en litres (200 ml)

    // Quantité de sucre
    val sucrePeu = 5.0
    val sucreMoyen = 10.0
    val sucreBeaucoup = 15.0

    var mode: Int = 0
    var machineEnPanne = false

    // Menu principal
    while (mode != 3) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      mode = readInt()

      if (mode == 1) {
        // Mode Client
        if (machineEnPanne) {
          println("La machine est en panne, veuillez contacter le technicien pour réapprovisionner les stocks.")
          println("Vous ne pouvez pas passer de commande pour le moment.")
          println("Retour au mode client.")
        } else {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")

          val Choixdelaboisson = readInt()

          var prixFinal = BigDecimal(0)
          var boisson = ""
          var cafeNecessaire = 0.0
          var laitNecessaire = 0.0

          if (Choixdelaboisson == 1) {
            boisson = "Expresso"
            prixFinal = prixExpresso
            cafeNecessaire = cafeExpresso
          } else if (Choixdelaboisson == 2) {
            boisson = "Cappuccino"
            prixFinal = prixCappuccino
            cafeNecessaire = cafeCappuccino
            laitNecessaire = laitCappuccino
          } else if (Choixdelaboisson == 3) {
            println("Veuillez sélectionner la taille de votre Latte :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            print("> ")

            val tailleLatte = readInt()

            if (tailleLatte == 1) {
              boisson = "Latte (Petit)"
              prixFinal = prixLattePetit
              cafeNecessaire = cafeLattePetit
              laitNecessaire = laitLattePetit
            } else if (tailleLatte == 2) {
              boisson = "Latte (Moyen)"
              prixFinal = prixLatteMoyen
              cafeNecessaire = cafeLatteMoyen
              laitNecessaire = laitLatteMoyen
            } else if (tailleLatte == 3) {
              boisson = "Latte (Grand)"
              prixFinal = prixLatteGrand
              cafeNecessaire = cafeLatteGrand
              laitNecessaire = laitLatteGrand
            } else {
              println("Entrée invalide.")
            }
          } else {
            println("Entrée invalide.")
          }

          if (boisson != "") {
            println("Boisson sélectionnée : ")
            println(boisson)
            println("Prix de la boisson : ")
            println(prixFinal)

            // Sélection de la quantité de sucre
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            print("> ")

            val quantitedesucre = readInt()

            var sucreNecessaire = 0.0
            if (quantitedesucre  == 1) {
              sucreNecessaire = 0.0
            } else if (quantitedesucre  == 2) {
              sucreNecessaire = sucrePeu
            } else if (quantitedesucre  == 3) {
              sucreNecessaire = sucreMoyen
            } else if (quantitedesucre  == 4) {
              sucreNecessaire = sucreBeaucoup
            } else {
              println("Entrée invalide.")
            }

            // Evaluation du prix
            if (sucreNecessaire == sucrePeu) {
              prixFinal += 0.10
            } else if (sucreNecessaire == sucreMoyen) {
              prixFinal += 0.20
            } else if (sucreNecessaire == sucreBeaucoup) {
              prixFinal += 0.30
            }

            // Sélection dela quantité de lait
            if (boisson == "Cappuccino" ||boisson == "Latte") {
              println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
              println("1) Oui")
              println("2) Non")
              print("> ")
              val quantitedelait = readInt()

              if (quantitedelait == 1) {
                println("Combien de doses ? (1 dose = 50ml)")
                print("> ")
                val dosesLait = readInt()

                if (dosesLait > 3) {
                  println("Le maximum est de 3 doses.")
                } else {
                  laitNecessaire += dosesLait * 0.05
                  prixFinal += dosesLait * 0.05
                }
              }
            }

            // Vérification des stocks
            if (Stockcafe >= cafeNecessaire && Stocksucre >= sucreNecessaire && Stocklait >= laitNecessaire) {
              println("Prix total :")
              println(prixFinal)
              println("Veuillez payer en utilisant Twint.")

              // Génération du code Twint
              val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
              var code = ""
              for (_ <- 1 to 5) {
                val randomIndex = Random.nextInt(62)
                code += chars(randomIndex)
              }
              println("votre code de paiement est :")
              println(code)

              // Attente de 3 secondes pour le paiement
              Thread.sleep(3000)

              println("Paiement confirmé.")
              println("Préparation de votre boisson...")
              Thread.sleep(5000) // Attente de 5 secondes pour la préparation
              println("Votre boisson est prêt ! Bonne dégustation !")

              // Mise à jour des stocks
              Stockcafe -= cafeNecessaire
              Stocksucre -= sucreNecessaire
              Stocklait -= laitNecessaire
            } else {
              println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez vérifier les stocks ou choisir une autre boisson.")
            }
          }
        }
      } else if (mode == 2) {
        // Mode Admin
        println("Mode Admin")
        print("Entrez le code PIN : ")
        val pin = readLine()

        if (pin == "434343") {
          println("Accès autorisé.")

          // Réapprovisionnement des stocks
          println("Réapprovisionnement des stocks...")
          println("Quel élément souhaitez-vous réapprovisionner ?")
          println("1) Poudre de café")
          println("2) Sucre")
          println("3) Lait")
          print("> ")

          val choixdeReapprovisionnement  = readInt()

          if (choixdeReapprovisionnement == 1) {
            println("Veuillez entrer la quantité de poudre de café à ajouter (en grammes) : ")
            val Ajoutdecafe = readDouble()
            Stockcafe += Ajoutdecafe
            println("Stocks mis à jour :")
            println(Stockcafe)
          } else if (choixdeReapprovisionnement == 2) {
            println("Veuillez entrer la quantité de sucre à ajouter (en grammes) : ")
            val Ajoutdesucre = readDouble()
            Stocksucre += Ajoutdesucre
            println("Stocks mis à jour :")
            println(Stocksucre)
          } else if (choixdeReapprovisionnement == 3) {
            println("Veuillez entrer la quantité de lait à ajouter (en litres) : ")
            val Ajoutdelait = readDouble()
            Stocklait += Ajoutdelait
            println("Stocks mis à jour :")
            println(Stocklait)
          } else {
            println("Choix invalide.")
          }

          // Retour au mode client après réapprovisionnement
          machineEnPanne = false
          println("Réapprovisionnement terminé. Retour au mode client.")
        } else {
          println("Code PIN incorrect. Accès refusé.")
        }
      }
    }

    println("Merci d'avoir utilisé le distributeur Nospresso Café. À bientôt !")
  }
}