import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    var mode = 0
    var stockCafe = 50.0 // Stock initial de poudre de café en grammes
    var stockLait = 0.500 // Stock initial de lait en litres
    var stockSucre = 30.0 // Stock initial de sucre en grammes
    var programmeActif = true // Contrôle l'exécution du programme

    while (programmeActif) {
      println("         Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode = readLine(">").toInt

      if (mode == 1) {
        // Mode client
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00 ")
        println("2) Cappuccino - CHF 2.50 ")
        println("3) Latte - CHF 3.20 ")
        val choix = readLine(">").toInt

        var prixBoisson = 0.0
        var cafesuffisant = 0.0
        var laitsuffisant = 0.0
        var sucresuffisant = 0.0
        var boissonRecap = ""

        // Déterminer les besoins en stock pour chaque boisson
        if (choix == 1) {
          prixBoisson = 2.00
          cafesuffisant = 8.0
          boissonRecap = "Expresso"
        } else if (choix == 2) {
          prixBoisson = 2.50
          cafesuffisant = 6.0
          laitsuffisant = 0.100
          boissonRecap = "Cappuccino"
        } else if (choix == 3) {
          println("Veuillez choisir la taille de votre Latte :")
          println("1) Petit CHF 2.70")
          println("2) Moyen CHF 3.20")
          println("3) Grand CHF 3.70")
          val taille = readLine(">").toInt

          if (taille == 1 ) {
            prixBoisson = 2.70
            cafesuffisant = 6
            laitsuffisant = 0.120
            boissonRecap = "Latte (Petit)"
          } else if (taille == 2) {
            prixBoisson = 3.20
            cafesuffisant = 8
            laitsuffisant = 0.150
            boissonRecap = "Latte (Moyen)"
          } else if (taille == 3) {
            prixBoisson = 3.70
            cafesuffisant = 12
            laitsuffisant = 0.200
            boissonRecap = "Latte (Grand)"
          }
        }

        // Sélection du sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        val sucre = readLine(">").toInt

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
        var prixLait = 0.0
        var laitsup = ""
        var lait = 4
        if (choix == 2 || choix == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ? (1) Oui (2) Non")
           lait = readLine(">").toInt

          if (lait == 1) {
            println("Combien de doses ? (max 3)")
            println("1) 50 mL (CHF 0.05)")
            println("2) 100 mL (CHF 0.10)")
            println("3) 150 mL (CHF 0.15)")
            val dose = readLine(">").toInt
            if (dose == 1 && stockLait >= 0.050) {
              prixLait = 0.05
              laitsuffisant = laitsuffisant + 0.050
              laitsup = " Oui (1 dose)"
            } else if (dose == 2 && stockLait >= 0.100) {
              prixLait = 0.10
              laitsuffisant = laitsuffisant + 0.100
              laitsup = " Oui ( 2 doses )"
            } else if (dose == 3 && stockLait >= 0.150) {
              prixLait = 0.15
              laitsuffisant = laitsuffisant + 0.150
              laitsup = " Oui ( 3 doses )"
            }
          }else if(lait == 2){
            laitsup = " Non"
          }
        }

        if (choix == 1) {
          // Récapitulatif de la commande expresso
          println("Récapitulatif de la commande :")

          println("Boisson : " + boissonRecap)
          println("Niveau de sucre : " + sucreRecap)


        }else{
          // Récapitulatif de la commande
          println("Récapitulatif de la commande :")

          println("Boisson : " + boissonRecap)
          println("Niveau de sucre : " + sucreRecap)
          println("lait supplémentaire : " + laitsup)


        }
        // Vérification des stocks disponibles
        if (stockCafe < cafesuffisant) {
          println("")
          println("Erreur : Quantité de poudre à café insuffisante pour préparer votre boisson.\n Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin")
          println("")
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 3.20 ")
          programmeActif = false
        } else if (stockLait < laitsuffisant) {
          println("")
          println("Erreur : Quantité de lait insuffisante pour préparer votre boisson.\n Veuillez choisir une taille plus petite ou essayer\n une autre boisson.")
          println("")
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 3.20 ")
          programmeActif = false
        } else if (stockSucre < sucresuffisant) {
          println("")
          println("Erreur : Quantité de sucre insuffisante pour préparer votre boisson.")
          println("")
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 3.20 ")
          programmeActif = false
        } else {
          if (choix == 1 && sucre == 1) {
            printf("Prix total : %.2f CHF = %.2f CHF\n",prixBoisson, prixBoisson)
            println("")
          }else if (choix == 1 && sucre != 1) {
            printf("Prix total : %.2f CHF + %.2f CHF = %.2f CHF\n",prixBoisson, prixSucre, prixBoisson + prixSucre)
            println("")
          }else if(lait == 2 && sucre == 1) {
            printf("Prix total : %.2f CHF = %.2f CHF\n", prixBoisson, prixBoisson)
            println("")
          }else if(sucre == 1 ){
            printf("Prix total : %.2f CHF + %.2f CHF = %.2f CHF\n",prixBoisson,prixLait, prixBoisson + prixLait)
            println("")
          }else if(lait == 2){
            printf("Prix total : %.2f CHF + %.2f CHF = %.2f CHF\n",prixBoisson, prixSucre,prixBoisson + prixSucre)
            println("")
          }else{
            printf("Prix total : %.2f CHF + %.2f CHF + %.2f CHF = %.2f CHF\n", prixBoisson, prixSucre, prixLait, prixBoisson + prixSucre + prixLait)
            println("")
          }
          // le paiement

            val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            var code = ""

            // Boucle for pour code de 5 caractères
            for (_ <- 1 to 5) {
              val charIndex = (Math.random() * alphabet.length).toInt
              code = code + alphabet(charIndex)
            }

          println("\nVeuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + code)
          println("(En attente de paiement...)")
          Thread.sleep(3000)
          println("")
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")
          println("Votre " + boissonRecap + " est prêt ! Bonne dégustation !")
          println("")
          Thread.sleep(5000)
          // Mise à jour des stocks
          stockCafe = stockCafe - cafesuffisant
          stockSucre = stockSucre - sucresuffisant
          stockLait = stockLait - laitsuffisant
        }

        } else if (mode == 2) {
        // Mode admin pour réapprovisionner
        println("Mode Admin")
        println("Entrez le code PIN : ")
        var pin = readLine(">").toInt
        while(pin != 434343){
          println("incorect")
          pin = readLine(">").toInt
        }
        if (pin == 434343) {
          println("Accès autorisé.")
          printf("Stocks actuels : \n Poudre de café : %.2f g \n Lait : %.3f L \n Sucre : %.2f g\n", stockCafe, stockLait, stockSucre)
          println("Réapprovisionnement des stocks...")

          println("Poudre de café : ")
          stockCafe = stockCafe + readLine().toDouble
          println("Lait  : ")
          stockLait = stockLait + readLine().toDouble
          println("Sucre  : ")
          stockSucre = stockSucre + readLine().toDouble

          printf("Stocks mis à jour : \n Poudre de café : %.2f g \n Lait : %.3f L \n Sucre : %.2f g\n", stockCafe, stockLait, stockSucre)

          println("Retour au menu principal ...")
          println("")
          Thread.sleep(5000)
        }
      } else if (mode == 3) {
        println("Merci de votre visite. À bientôt !")
        programmeActif = false
      }
    }


  }
}
