
import scala.io.StdIn.readInt
import scala.io.StdIn.readChar
import scala.io.StdIn.readLine
import scala.io.StdIn.readDouble
import scala.util.Random


object NespressoCafe {
  def main(args: Array[String]): Unit = {

    println("Bienvenue au Nespresso Café!")

    // Sélection du mode
    var mode = ' '
    var validMode = false

    while (!validMode) {
      println("Veuillez sélectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      mode = readChar()

      if (mode == '1' || mode == '2' || mode == '3') {
        validMode = true
      } else {
        println("Choix invalide. Veuillez choisir 1, 2 ou 3.")
      }
    }
    if (mode == '1') {

      println("Veuillez sélectionner votre boisson:")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")

      // Choix de la boisson
      var drinkChoice = readInt()

      // Vérification que l'entrée est valide
      while (drinkChoice < 1 || drinkChoice > 3) {
        println("Choix invalide. Veuillez choisir 1, 2 ou 3.")
        print("> ")
        drinkChoice = readInt()
      }

      // Stock initial
      var StockCafe: Int = 50
      var StockSucre: Int = 30
      var StockLait: Double = 0.500 // en litres

      // Consommation des ingrédients par boisson
      val ExpressoCafCons = 8 // 8g de café pour un expresso
      val CappuccinoCafCons = 6 // 6g de café pour un cappuccino
      val LatteCafConsSmall = 6 // 6g de café pour un Latte petit
      val LatteCafConsMedium = 8 // 8g de café pour un Latte moyen
      val LatteCafConsLarge = 12 // 12g de café pour un Latte grand

      val SucrePeu = 5 // 5g de sucre pour un peu
      val SucreMoyen = 10 // 10g de sucre pour un moyen
      val SucreBeaucoup = 15 // 15g de sucre pour un beaucoup

      val LaitPetit = 0.100 // 100ml pour un latte petit
      val LaitMoyen = 0.150 // 150ml pour un latte moyen
      val LaitGrand = 0.200 // 200ml pour un latte grand

      var StockCafeAjout = false
      var boissonPrix = 0.0


      // Traitement selon le choix de la boisson
      if (drinkChoice == 1) {
        boissonPrix = 2.00
        println("Vous avez choisi un Expresso - CHF 2.00")
        println(s"— Boisson sélectionnée : Expresso ")
        println(s"— Personnalisation : Sucre élevé")


        if (ExpressoCafCons >= StockCafe) {
          println("Préparation de votre Expresso...")

        } else {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer l'expresso.")
          println("Passons à une autre boisson.")

          // Proposer un autre choix
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")

          // Lire à nouveau le choix de la boisson
          drinkChoice = readInt()

          if (drinkChoice == 2) { // Choix du cappuccino
            boissonPrix = 2.50
            println("Vous avez choisi un Cappuccino - CHF 2.50")
            println(s"— Boisson sélectionnée : Cappuccino ")
          } else if (drinkChoice == 3) { // Choix du latte
            boissonPrix = 2.70
            println("Vous avez choisi un Latte - CHF 2.70")
            println(s"— Boisson sélectionnée : Latte ")
          } else {
            println("Choix invalide, retour au menu principal.")
          }
        }
      }

      else if (drinkChoice == 2) { // Choix du cappuccino
        boissonPrix = 2.50
        println("Vous avez choisi un Cappuccino - CHF 2.50")

        // Vérification du stock pour le cappuccino
        if (CappuccinoCafCons <= StockCafe && CappuccinoCafCons <= StockSucre) {
          StockCafe -= CappuccinoCafCons // Réduction du stock de café
          StockLait -= CappuccinoCafCons // Réduction du stock de lait
          StockCafeAjout = true

          // Affichage des détails de la commande
          println("Boisson Sélectionnée : Cappuccino.")
          println("Personnalisation : Sucre moyen, avec lait.")
          println("Stock: Suffisant.")

          // Affichage du prix total et des instructions de paiement
          println("Prix total : CHF 2.50 + CHF 0.20 = CHF 2.70")
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : JK9ZL")

          // Simulation de l'attente de paiement
          println("(En attente de paiement...)")
          Thread.sleep(5000) // Simule une attente pour le paiement
          println("Paiement confirmé.")

          // Préparation de la boisson
          println("Préparation de votre boisson...")
          Thread.sleep(5000) // Simule une attente pour la préparation
          println("Votre Cappuccino est prêt ! Bonne dégustation !")

        } else {
          println("Erreur : Quantité de café insuffisante pour préparer le cappuccino.")
          StockCafeAjout = false
        }
      }
      else if (drinkChoice == 3) { // Choix du latte
        println("Vous avez choisi un Latte. Sélectionnez la taille:")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")

        var sizeChoice = readInt()
        while (sizeChoice < 1 || sizeChoice > 3) {
          println("Choix invalide. Veuillez choisir 1, 2 ou 3.")
          print("> ")
          sizeChoice = readInt()
        }

        // Vérification du stock pour le latte petit
        if (sizeChoice == 1) {
          boissonPrix = 2.70
          println("Boisson séléctionnée: Latté (Petit)")
          println("Personnalisation : Peu de sucre")
          println("Stock: Suffisant")
          if (LatteCafConsSmall <= StockCafe && LaitPetit <= StockLait && SucrePeu <= StockSucre) {
            StockCafe -= LatteCafConsSmall // Réduction du stock de café
            StockLait -= LaitPetit // Réduction du stock de lait
            StockSucre -= SucrePeu // Réduction du stock de sucre

            println(" Niveau de sucre : Peu (5g)")
            println("Lait en supplément: Non")
            println("Prix total : CHF 2.70 + CHF 0.10 = CHF 2.80")
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : PQ3VM")
            Thread.sleep(2000) // Attente simulée pour le paiement
            println("En attente de paiement.")
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            Thread.sleep(3000) // Attente simulée pour la préparation
            println("Votre Latte est prêt ! Bonne dégustation !")
          } else {
            if (LatteCafConsSmall > StockCafe) println("Désolé, il n'y a pas assez de café pour un Latte Petit.")
            if (LaitPetit > StockLait) println("Désolé, il n'y a pas assez de lait pour un Latte Petit.")
            if (SucrePeu > StockSucre) println("Désolé, il n'y a pas assez de sucre pour un Latte Petit.")
          }
        }

        // Vérification du stock pour le latte moyen
        else if (sizeChoice == 2) {
          boissonPrix = 3.20
          println("Vous avez choisi un Latte Moyen - CHF 3.20")
          if (LatteCafConsMedium <= StockCafe && LaitMoyen <= StockLait && SucreMoyen <= StockSucre) {
            println("Il y a suffisemment de café, de lait et de sucre pour votre moyen latté.")
            StockCafe -= LatteCafConsMedium // Réduction du stock de café
            StockLait -= LaitMoyen // Réduction du stock de lait
            StockSucre -= SucreMoyen // Réduction du stock de sucre
            StockCafeAjout = true
          } else {
            if (LatteCafConsMedium > StockCafe) println("Désolé, il n'y a pas assez de café pour un Latte Moyen.")
            if (LaitMoyen > StockLait) println("Désolé, il n'y a pas assez de lait pour un Latte Moyen.")
            if (SucreMoyen > StockSucre) println("Désolé, il n'y a pas assez de sucre pour votre Latte Moyen.")
            StockCafeAjout = false
          }
        }

        // Vérification du stock pour le latte grand
        else if (sizeChoice == 3) { // Choix du latte grand
          boissonPrix = 3.70
          println("Vous avez choisi un Latte Grand - CHF 3.70")

          // Vérification du stock pour le latte grand
          if (LatteCafConsLarge <= StockCafe && LaitGrand >= StockLait && SucreBeaucoup <= StockSucre) {
            println("Il y a suffisemment de café, de lait et de sucre pour votre grand latté.")
            StockCafe -= LatteCafConsLarge // Réduction du stock de café
            StockLait -= LaitGrand // Réduction du stock de lait
            StockSucre -= SucreBeaucoup // Réduction du stock de sucre
            println("Prix total : CHF 3.70")
            println("Préparation en cours...")
            println("Votre Latte Grand est prêt ! Bonne dégustation !")
          } else {
            // Message d'erreur détaillé en cas de stock insuffisant
            println(s"— Boisson sélectionnée : Latte (Grand)")
            println(s"— Personnalisation : Sans sucre, avec lait")

            if (LaitGrand > StockLait) {
              println(s"— Stock : Lait insuffisant pour la taille choisie")
            }

            println(s"Boisson sélectionnée : Latte (Grand)")
            println(s"Niveau de sucre : Sans sucre")
            println(s"Lait en supplément: Non")

            if (LaitGrand > StockLait) {
              println(s"Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            }

            println(s"Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
        }
      }

      // Génération du code TWINT
      def GenerateTWINTCode(length: Int = 5) = {
        Random.alphanumeric.take(length).mkString
      }

      val TWINTCODE = GenerateTWINTCode()
      println(s"Votre code de paiement est : $TWINTCODE")

      // Attente de paiement (simulée par une pause de 5 secondes)
      println("(En attente de paiement...)")
      Thread.sleep(5000)  // Simuler un délai de 5 secondes pour le paiement

      // Confirmation de la commande
      println(s"Votre Expresso est prêt ! Bonne dégustation !")


      // Vérification du stock de café, sucre et lait après la commande
      if (StockCafe <= 0) println("Il n'y a plus de café disponible.")
      if (StockSucre <= 0) println("Il n'y a plus de sucre disponible.")
      if (StockLait <= 0) println("Il n'y a plus de lait disponible.")


      // Si la boisson peut être préparée (vérification du stock)
      if (StockCafeAjout) {
        // Demander si l'utilisateur veut ajouter du sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Oui")
        println("2) Non")
        var ajouterSucre = readInt()

        // Validation de la réponse (1 ou 2)
        while (ajouterSucre != 1 && ajouterSucre != 2) {
          println("Choix invalide. Veuillez choisir 1 pour Oui ou 2 pour Non.")
          ajouterSucre = readInt()
        }

        var sucreQuantite = 0

        // Si l'utilisateur choisit "Oui" pour ajouter du sucre
        if (ajouterSucre == 1) {
          println("Choisissez votre quantité de sucre :")
          println("1) Peu (5g)")
          println("2) Moyen (10g)")
          println("3) Beaucoup (15g)")
          println("4) Pas de sucre")
          print("> ")

          var sucreChoice = readInt()

          // Validation de l'entrée du sucre
          while (sucreChoice < 1 || sucreChoice > 4) {
            println("Choix invalide. Veuillez choisir 1, 2, 3, ou 4.")
            sucreChoice = readInt()
          }

          // Définition de la quantité de sucre en fonction du choix
          if (sucreChoice == 1) {
            sucreQuantite = SucrePeu
          } else if (sucreChoice == 2) {
            sucreQuantite = SucreMoyen
          } else if (sucreChoice == 3) {
            sucreQuantite = SucreBeaucoup
          }

          // Comparaison avec les quantités définies et affichage des messages
          if (sucreQuantite < SucrePeu) {
            println("Désolé, la quantité de sucre choisie est insuffisante.")
          } else if (sucreQuantite == SucrePeu) {
            println("Vous avez choisi peu de sucre (5g).")
          } else if (sucreQuantite == SucreMoyen) {
            println("Vous avez choisi un sucre moyen (10g).")
          } else if (sucreQuantite == SucreBeaucoup) {
            println("Vous avez choisi beaucoup de sucre (15g).")

          }

          if (drinkChoice == 2 || drinkChoice == 3) {
            println("Souhaitez-vous ajouter du lait en supplément (taper 1 pour Oui ou 2 pour Non) ? (Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            print("> ")

            var milkChoice = readInt()
            while (milkChoice < 1 || milkChoice > 2) {
              println("Choix invalide. Veuillez choisir entre 1 et 2.")
              print("> ")
              milkChoice = readInt()
            }

            if (milkChoice == 1) {
              println("Combien de doses de lait voulez-vous ajouter ? (1 à 3 doses)")
              print("> ")

              var milkDose = readInt()
              while (milkDose < 0 || milkDose > 3) {
                println("Choix invalide. Veuillez entrer entre 0 et 3 doses.")
                print("> ")
                milkDose = readInt()
              }

              val totalMilk = milkDose * 50 // Calcul du total en ml
              val pricePerDose = 0.50 // Prix d'une dose de lait (en EUR)
              val totalMilkCost = milkDose * pricePerDose // Calcul du coût total du lait

              if (milkDose == 0) {
                println("Vous n'avez ajouté aucune dose de lait.")
              } else {
                println(s"Vous avez choisi $milkDose dose(s) de lait, soit un total de $totalMilk ml.")
                println(f"Le coût total du lait ajouté est : $totalMilkCost%1.2f CHF.") // Affiche le prix total avec 2 décimales
              }
            } else {
              println("Vous ne voulez pas ajouter de lait.")
            }
          }
        }
      }

    }

    else if (mode == '2') {
      println("Mode : Admin")
      println("Action: Réapprovisionnement de la poudre de café et du lait.")
      println("Ce mode est protégé par un code PIN à 6 chiffres.")


      var pinCorrect = false
      var tentatives = 0

      // Vérification du PIN pour l'admin
      while (!pinCorrect && tentatives < 3) {
        print("Mode Admine : ")
        print("Entrez le code PIN : ****** ")
        val pinEntre = readLine()

        if (pinEntre == "434343") {
          pinCorrect = true
          println("Accès autorisé.")

          // Affichage des stocks et réapprovisionnement
          var continuerAdmin = true
          while (continuerAdmin) {
            println("\n— Mode: Admin")
            println("— Action : Réapprovisionnement des stocks")
            println("Stocks:")
            println("Poudre de café: 10g")
            println("Lait: 0.5L")
            println("Sucre: 30g")

            println("\nRéapprovisionnement des stocks...")
            print("Ajout: ")
            val PoudreQty = readInt()
            print("Poudre de café : " + 50)
            val cafeQty = readDouble()
            print("Lait (L) : " + 0.5)
            val laitQty = readDouble()
            print("Sucre: " + 0)
            val sucreQty = readInt()

            // Mise à jour des stocks
            println(s"\nAjout :")
            println(s"— Poudre de café: $PoudreQty g")
            println(s"— Lait : $laitQty L")
            println(s"— Sucre : $sucreQty g")
            println("Niveaux de stock mis à jour.")

            print("> ")
            val choixAdmin = readDouble()


            if (choixAdmin == 2) {
              println("Retour au menu principal...")
              continuerAdmin = false
            }
          }
        } else {
          tentatives += 1
          if (tentatives < 3) {
            println(s"Code PIN incorrect. Il vous reste ${3 - tentatives} tentative(s).")
          } else {
            println("Nombre maximal de tentatives atteint. Accès refusé.")
          }
        }
      }

    } else if (mode == '3') {
      println("Vous avez choisi de quitter. Au revoir!")
    }

  }

}