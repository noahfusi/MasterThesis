import scala.util.Random


object Main {
  def main(args: Array[String]): Unit = {

    var mode = ""
    var stock_poudre = 50
    var stock_sucre = 30
    var stock_lait = 0.5


    while (mode != "3") {

      println("\n        Nospresso Café")

      print("Veuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ")

      mode = scala.io.StdIn.readLine()

      while (mode != "1" && mode != "2" && mode != "3") {
        mode = scala.io.StdIn.readLine()
      }



      // client
      if (mode == "1") {

        var prix = 0.0
        var prix_total = ""

        var boisson = ""
        var sucre = ""
        var lait = ""

        var conso_poudre = 0
        var conso_sucre = 0
        var conso_lait = 0.0


        // séléction boisson
        print("\nVeuillez sélectionner votre boisson : \n1) Expresso - CHF 2,00 \n2) Cappuccino - CHF 2,50 \n3) Latte - CHF 2,70 (Petit), CHF 3,20 (Moyen), CHF 3,70 (Grand)\n> ")
        var choix_boisson = scala.io.StdIn.readLine()
        while (choix_boisson != "1" && choix_boisson != "2" && choix_boisson != "3") {
          choix_boisson = scala.io.StdIn.readLine()
        }

        if (choix_boisson == "1") {
          boisson = "Expresso"
          prix = prix + 2.0
          prix_total = "CHF 2,00"
          conso_poudre = 8

        } else if (choix_boisson == "2") {
          boisson = "Cappucino"
          prix = prix + 2.5
          prix_total = "CHF 2,50"
          conso_poudre = 6
          conso_lait = 0.1

        } else {

          // taille latte
          print("\nVeuillez sélectionner une taille de Latte : \n1) Petit - CHF 2,70\n2) Moyen - CHF 3,20\n3) Grand - CHF 3,70 \n> ")
          var choix_taille = scala.io.StdIn.readLine()
          while (choix_taille != "1" && choix_taille != "2" && choix_taille != "3") {
            choix_taille = scala.io.StdIn.readLine()
          }

          if (choix_taille == "1") {
            boisson = "Latte (Petit)"
            prix = prix + 2.7
            prix_total = "CHF 2,70"
            conso_poudre = 6
            conso_lait = 0.12

          } else if (choix_taille == "2") {
            boisson = "Latte (Moyen)"
            prix = prix + 3.2
            prix_total = "CHF 3,20"
            conso_poudre = 8
            conso_lait = 0.15

          } else {
            boisson = "Latte (Grand)"
            prix = prix + 3.7
            prix_total = "CHF 3,70"
            conso_poudre = 12
            conso_lait = 0.2

          }

        }


        // séléction sucre
        print("\nSouhaitez-vous ajouter du sucre ? : \n1) Sans sucre \n2) Peu (5g) - CHF 0,10 \n3) Moyen (10g) - CHF 0,20 \n4) Beaucoup (15g) - CHF 0,30\n> ")
        var choix_sucre = scala.io.StdIn.readLine()
        while (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre != "4") {
          choix_sucre = scala.io.StdIn.readLine()
        }

        if (choix_sucre == "2") {
          prix = prix + 0.1
          prix_total = prix_total + " + CHF 0,10"
          sucre = "Peu (5g)"
          conso_sucre = 5

        } else if (choix_sucre == "3") {
          prix = prix + 0.2
          prix_total = prix_total + " + CHF 0,20"
          sucre = "Moyen (10g)"
          conso_sucre = 10

        } else if (choix_sucre == "4") {
          prix = prix + 0.3
          prix_total = prix_total + " + CHF 0,30"
          sucre = "Beaucoup (15g)"
          conso_sucre = 15

        } else {
          sucre = "Sans sucre"
        }


        // séléction lait seulement si cappuccino (2) et latte (3)
        if (choix_boisson == "2" || choix_boisson == "3") {

          print("\nSouhaitez-vous ajouter du lait en supplément ? : \n1) Oui \n2) Non\n> ")
          var choix_lait = scala.io.StdIn.readLine()
          while (choix_lait != "1" && choix_lait != "2") {
            choix_lait = scala.io.StdIn.readLine()
          }

          if (choix_lait == "1") {
            print("\nCombien de dose ? (3 doses maximum à CHF 0,05 par dose) \n> ")
            var choix_doses = scala.io.StdIn.readLine()
            while (choix_doses != "1" && choix_doses != "2" && choix_doses != "3") {
              choix_doses = scala.io.StdIn.readLine()
            }

            if (choix_doses == "1") {
              prix = prix + 0.05
              prix_total = prix_total + " + CHF 0,05"
              lait = "1 dose"
              conso_lait = conso_lait + 0.05

            } else if (choix_doses == "2") {
              prix = prix + 0.1
              prix_total = prix_total + " + CHF 0,10"
              lait = "2 doses"
              conso_lait = conso_lait + 0.1

            } else {
              prix = prix + 0.15
              prix_total = prix_total + " + CHF 0,15"
              lait = "3 doses"
              conso_lait = conso_lait + 0.15
            }

          } else {
            lait = "Non"
          }
        }

        // récapitulatif :
        println("\nBoisson sélectionnée : " + boisson)
        println("Niveau de sucre : " + sucre)
        if (choix_boisson == "2" || choix_boisson == "3") {
          println("Lait en supplément : " + lait)
        }


        // vérification des stocks
        if(stock_poudre - conso_poudre < 0) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        } else if (stock_sucre - conso_sucre < 0) {
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir moins de sucre ou vérifier les stocks en mode Admin.\n")
        } else if (stock_lait - conso_lait < 0.0) {
          println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson ou vérifier les stocks en mode Admin.\n")
        } else {

          // si y'a du stock
          stock_poudre = stock_poudre - conso_poudre
          stock_sucre = stock_sucre - conso_sucre
          stock_lait = stock_lait - conso_lait

          if (choix_sucre == "1" && (choix_boisson == "1" || lait == "Non")) {
            println("Prix total : " + f"CHF $prix%.2f")
          } else {
            println("Prix total : " + prix_total + f" = CHF $prix%.2f")
          }

          // paiement
          val code_twint = Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString
          println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_twint + "\n(En attente de validation du paiement...)\n")
          Thread.sleep(3000)
          println("Paiement confirmé.\n" + "Préparation de votre boisson...")
          Thread.sleep(3000)
          println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
        }

        println("Retour au menu principal...\n\n")
        Thread.sleep(1000)

      }

      // admin
      else if (mode == "2") {

        // pin
        print("\nMode Admin \nEntrez le code PIN : ")
        var PIN = scala.io.StdIn.readLine()
        while (PIN != "434343") {
          print("PIN Incorrect. Réesayez : ")
          PIN = scala.io.StdIn.readLine()
        }
        println("\nAccès autorisé.\n")


        // stock initial
        println("Stocks :\n   Poudre de café : " + stock_poudre + "g\n   Lait           : " + stock_lait + "L\n   Sucre          : " + stock_sucre + "g")


        // réapprovisionnement
        print("Veuillez entrer la quantité de poudre à café (en g) à ajouter : ")
        var ajout_poudre = scala.io.StdIn.readLine()
        while (ajout_poudre.toInt < 0) {
          ajout_poudre = scala.io.StdIn.readLine()
        }

        print("\nVeuillez entrer la quantité de lait (en L) à ajouter : ")
        var ajout_lait = scala.io.StdIn.readLine()
        while (ajout_lait.toDouble < 0.0) {
          ajout_lait = scala.io.StdIn.readLine()
        }

        print("\nVeuillez entrer la quantité de sucre (en g) à ajouter : ")
        var ajout_sucre = scala.io.StdIn.readLine()
        while (ajout_sucre.toInt < 0) {
          ajout_sucre = scala.io.StdIn.readLine()
        }

        stock_poudre = stock_poudre + ajout_poudre.toInt
        stock_lait = stock_lait + ajout_lait.toDouble
        stock_sucre = stock_sucre + ajout_sucre.toInt
        println("\nRéapprovisionnement des stocks...")
        Thread.sleep(2000)
        println("\nAjout :\n   Poudre de café : " + stock_poudre + "g\n   Lait           : " + stock_lait + "L\n   Sucre          : " + stock_sucre + "g")
        println("Niveaux de stock mis à jour.\nRetour au menu principal...\n\n")
        Thread.sleep(1000)


      }
      else {
        println("\nMerci d'avoir utilisé Nospresso !")
        println("À bientôt !")


      }

    }

  }

}