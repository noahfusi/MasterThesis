import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {

    //Valeurs initiales
    val stockCafe = 50
    val stockLait = 0.5
    val stockSucre = 30

    // Fonctionnalités principales
    var poudreCafe = 50 // gr
    var sucre = 30 // gr
    var lait = 0.5 // litres
    val pinAdmin = 434343


    // Boucle principale du menu - infinie
    while (true) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      val choixUtilisateur = readLine("> ").toInt
      if (choixUtilisateur == 1) {
        // Mode Client
        var commandeValide = false

        while (!commandeValide) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

          val boisson = readLine("> ").toInt

          // Initialisation des quantités nécessaires et du prix
          var nomBoisson = ""
          var resumeBoisson = ""
          var cafeNecessaire = 0
          var laitNecessaire = 0.0
          var prixBoisson = 0.0

          if (boisson == 1) { // Expresso
            nomBoisson = "Expresso"
            resumeBoisson = "Expresso"
            cafeNecessaire = 8
            laitNecessaire = 0
            prixBoisson = 2.00
          } else if (boisson == 2) { // Cappuccino
            nomBoisson = "Cappuccino"
            resumeBoisson = "Cappuccino"
            cafeNecessaire = 6
            laitNecessaire = 0.1
            prixBoisson = 2.50
          } else if (boisson == 3) { // Latte
            println("Choisissez la taille du Latte :")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")

            val choixLatte = readLine("> ").toInt
            if (choixLatte == 1) {
              nomBoisson = "Latte Petit"
              resumeBoisson = "Latte (Petit)"
              cafeNecessaire = 6
              laitNecessaire = 0.12
              prixBoisson = 2.70
            } else if (choixLatte == 2) {
              nomBoisson = "Latte Moyen"
              resumeBoisson = "Latte (Moyen)"
              cafeNecessaire = 8
              laitNecessaire = 0.15
              prixBoisson = 3.20
            } else if (choixLatte == 3) {
              nomBoisson = "Latte Grand"
              resumeBoisson = "Latte (Grand)"
              cafeNecessaire = 12
              laitNecessaire = 0.2
              prixBoisson = 3.70
            } else {
              println("Taille invalide. Veuillez réessayer.")
            }
          } else {
            println("Choix de boisson invalide. Veuillez réessayer.")
          }

          // Ajout de sucre
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")

          val choixSucre = readLine("> ").toInt
          var resumeSucre = ""
          var quantiteSucre = 0
          var prixSucre = 0.0

          if (choixSucre == 1) {
            resumeSucre = "Sans sucre"
            quantiteSucre = 0
            prixSucre = 0.0
          } else if (choixSucre == 2) {
            resumeSucre = "Peu (5g)"
            quantiteSucre = 5
            prixSucre = 0.10
          } else if (choixSucre == 3) {
            resumeSucre = "Moyen (10g)"
            quantiteSucre = 10
            prixSucre = 0.20
          } else if (choixSucre == 4) {
            resumeSucre = "Beaucoup (15g)"
            quantiteSucre = 15
            prixSucre = 0.30
          } else
            println("Quantité invalide. Veuillez réessayer.")

          // Ajout de lait en supplément
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")

          val choixdoses = readLine("> ").toInt
          var resumeLaitdose = ""
          var dosesLait = 0

          if (choixdoses == 1) {
            println("Combien de doses ?")
            dosesLait = readLine("> ").toInt
            if (dosesLait >= 1 && dosesLait <= 3) {
              resumeLaitdose = "Oui"
            } else {
              println("Erreur : Quantité de lait insuffisante pour ")
              println("préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les")
              println("stocks en mode Admin.")
              resumeLaitdose = "Non"
              dosesLait = 0
            }
          } else {
            resumeLaitdose = "Non"
          }

          val prixLait = if (dosesLait == 1) 0.05 else if (dosesLait == 2) 0.10 else if (dosesLait == 3) 0.15 else 0.0

          // Vérification des stocks et affichage des ingrédients manquants
          var stockInsuffisant = false
          if (poudreCafe < cafeNecessaire) {
            println("Erreur : Quantité de poudre de café insuffisante pour ")
            println("préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les")
            println("stocks en mode Admin.")
            stockInsuffisant = true
          }
          if (lait < laitNecessaire) {
            println("Erreur : Quantité de lait insuffisante pour ")
            println("préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les")
            println("stocks en mode Admin.")
            stockInsuffisant = true
          }
          if (sucre < quantiteSucre) {
            println("Erreur : Quantité de sucre insuffisante pour ")
            println("préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les")
            println("stocks en mode Admin.")
            stockInsuffisant = true
          }

          if (stockInsuffisant) {

          } else {
            // Résumé de la commande
            println("Boisson sélectionnée : " + resumeBoisson)
            println("Niveau de sucre : " + resumeSucre)
            println("Lait en supplément : " + resumeLaitdose)
            println()


            // Calcul du prix total
            val prixTotal = prixBoisson + prixSucre + prixLait
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre, prixLait, prixTotal)
            println()


            // Générer un code alphanumérique de 5 caractères dont 1 chifre et 4 majuscules
            var codeTwint = ""
            val chiffres = ('0' + (math.random * 10).toInt).toChar

            val chiffrePosition = (math.random * 5).toInt // Position aléatoire pour placer le chiffre (entre 0 et 4 inclus)

            // Générer les 5 caractères
            for (i <- 0 until 5) {
              val char =
                if (i == chiffrePosition) {
                  chiffres // Placer le chiffre à la position définie
                } else {
                  ('A' + (math.random * 26).toInt).toChar // Générer une lettre majuscule
                }
              codeTwint += char // Ajouter le caractère au code
            }

            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTwint)
            println("(En attente de paiement...)")
            println()
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

            Thread.sleep(5000)
            println()

            // Mise à jour des stocks
            poudreCafe -= cafeNecessaire
            lait -= laitNecessaire
            sucre -= quantiteSucre

            //Sortir de la boucle si la commande est réussie
            commandeValide = true
          }
        }


      } else if (choixUtilisateur == 2) {
        // Mode Admin
        println("Mode Admin")

        //PIN
        val pin = readLine("Entrez le code PIN : ").toInt
        println("Accès autorisé.")
        println()

        //Affichage des stocks
        println("Stocks:")
        if (pin == pinAdmin) {
          printf("Poudre de café : %dg\n" +
            "Lait : %.2fL \n" +
            "Sucre : %dg", poudreCafe, lait, sucre)

          println()

          println("Réapprovisionnement des stocks...")

          Thread.sleep(5000)

          val cafeManquant = stockCafe - poudreCafe
          val laitManquant = stockLait - lait
          val sucreManquant = stockSucre - sucre

          //Ajout stocks
          println("Ajout :")
          printf("Poudre de café : %dg\n" +
            "Lait : %.2fL \n" +
            "Sucre : %dg", cafeManquant, laitManquant, sucreManquant)
          println()
          println("Niveaux des stocks mis à jour.")
          poudreCafe += cafeManquant
          lait += laitManquant
          sucre += sucreManquant

          println("Retour au menu principal...")
          Thread.sleep(5000)
          println()
        } else {
          println("Code PIN incorrect.")
        }
      } else if (choixUtilisateur == 3) {
        println("Programme terminé.")

      } else {
        println("Ce chiffre n'est pas une entrée valide.")
      }
    }
  }
}
