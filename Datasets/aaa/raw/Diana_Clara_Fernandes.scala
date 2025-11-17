import io.StdIn._


object Main {
  def main(args: Array[String]): Unit = {
    val repetition = true
    var mode = 0
    var boisson = 0
    var taillelatte = 0
    val codePIN = "434343"
    var lecturecodePIN = " "
    var poudrecafe = 50
    var stocksucre = 30
    var sucreclient = 0
    var stocklait = 0.500
    var laitclient = 0.0
    var doselait = 4
    val prixsucre = 0.1
    val prixlait = 0.05
    var prix = 0.0
    var erreur = true
    var ajoutpoudrecafe = -1
    var ajoutsucre = -1
    var ajoutlait = -1.0

    while (repetition == true) {
      println("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")

      while (!(mode == 1 || mode == 2 || mode == 3)) {
        mode = readInt()
        if (!(mode == 1 || mode == 2 || mode == 3)) {
          println("Veuillez sélectionner un numéro de mode possible (1, 2 ou 3): ")
        }
      }

      if (mode == 1) {
        erreur = true
        while (erreur == true) {
          println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")

          while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
            boisson = readInt()
            if (!(boisson == 1 || boisson == 2 || boisson == 3)) {
              println("Veuillez sélectionner un numéro de boisson possible (1, 2 ou 3): ")
            }
          }

          if (boisson == 3) {
            println("Veuillez choisir une taille de Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>")
            while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
              taillelatte = readInt()
              if (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
                println("Veuillez sélectionner une taille de Latte possible (1, 2 ou 3): ")
              }
            }
          }

          println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4)Beaucoup (15g) - CHF 0.30\n>")
          while (!(sucreclient == 1 || sucreclient == 2 || sucreclient == 3 || sucreclient == 4)) {
            sucreclient = readInt()
            if (!(sucreclient == 1 || sucreclient == 2 || sucreclient == 3 || sucreclient == 4)) {
              println("Veuillez sélectionner une quantité de sucre possible (1, 2, 3 ou 4): ")
            }
          }
          sucreclient = (sucreclient - 1) * 5

          if (boisson == 2 || boisson == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>")
            while (!(laitclient == 1 || laitclient == 2)) {
              laitclient = readInt()
              if (!(laitclient == 1 || laitclient == 2)) {
                println("Veuillez choisir une option disponible (1 ou 2): ")
              }
            }
          }

          if (laitclient == 1) {
            println("Combien de dose ?\n>")
            while (doselait < 0 || doselait > 3) {
              doselait = readInt()
              if (doselait < 0 || doselait > 3) {
                println("Veuillez choisir une dose possible (0, 1, 2 ou 3): ")
              }
            }
            laitclient = doselait * 0.05
          }
          else {
            laitclient = 0
            doselait = 0
          }
          println

          if (boisson == 1) {
            println("Boisson sélectionnée : Expresso")
            if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
            else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
            else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
            else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")

            if ((poudrecafe - 8 >= 0) && (stocksucre - sucreclient >= 0)) {
              erreur = false
              poudrecafe = poudrecafe - 8
              stocksucre = stocksucre - sucreclient
              prix = 2.0 + (prixsucre * (sucreclient / 5))
              printf("Prix total : CHF 2.00 + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prix)
              println
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
              println("(En attente de paiement...)\n")
              Thread.sleep(3000)
              println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Expresso est prêt ! Bonne dégustation !")
            }

            else {
              println
              if (poudrecafe - 8 < 0) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocksucre - sucreclient < 0) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
          }

          if (boisson == 2) {
            println("Boisson sélectionnée : Cappuccino")
            if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
            else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
            else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
            else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
            if (doselait == 0) println("Lait en supplément : Non")
            else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
            else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
            else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

            if ((poudrecafe - 6 >= 0) && (stocksucre - sucreclient >= 0) && (stocklait - laitclient - 0.1 >= 0)) {
              erreur = false
              poudrecafe = poudrecafe - 6
              stocksucre = stocksucre - sucreclient
              stocklait = stocklait - laitclient - 0.1
              prix = 2.50 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
              printf("Prix total : CHF 2.50 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
              println
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
              println("(En attente de paiement...)\n")
              Thread.sleep(3000)
              println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Cappuccino est prêt ! Bonne dégustation !")
            }

            else {
              println
              if (poudrecafe - 6 < 0) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocksucre - sucreclient < 0) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocklait - laitclient - 0.1 < 0) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
          }

          if (taillelatte == 1) {
            println("Boisson sélectionnée : Latte (Petit)")
            if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
            else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
            else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
            else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
            if (doselait == 0) println("Lait en supplément : Non")
            else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
            else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
            else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

            if ((poudrecafe - 6 >= 0) && (stocksucre - sucreclient >= 0) && (stocklait - laitclient - 0.12 >= 0)) {
              erreur = false
              poudrecafe = poudrecafe - 6
              stocksucre = stocksucre - sucreclient
              stocklait = stocklait - laitclient - 0.12
              prix = 2.70 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
              printf("Prix total : CHF 2.70 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
              println
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
              println("(En attente de paiement...)\n")
              Thread.sleep(3000)
              println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
            }

            else {
              println
              if (poudrecafe - 6 < 0) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocksucre - sucreclient < 0) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocklait - laitclient - 0.12 < 0) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
          }

          if (taillelatte == 2) {
            println("Boisson sélectionnée : Latte (Moyen)")
            if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
            else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
            else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
            else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
            if (doselait == 0) println("Lait en supplément : Non")
            else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
            else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
            else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

            if ((poudrecafe - 8 >= 0) && (stocksucre - sucreclient >= 0) && (stocklait - laitclient - 0.15 >= 0)) {
              erreur = false
              poudrecafe = poudrecafe - 8
              stocksucre = stocksucre - sucreclient
              stocklait = stocklait - laitclient - 0.15
              prix = 3.20 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
              printf("Prix total : CHF 3.20 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
              println
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
              println("(En attente de paiement...)\n")
              Thread.sleep(3000)
              println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
            }

            else {
              println
              if (poudrecafe - 8 < 0) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocksucre - sucreclient < 0) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocklait - laitclient - 0.15 < 0) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.\n")
            }
          }

          if (taillelatte == 3) {
            println("Boisson sélectionnée : Latte (Grand)")
            if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
            else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
            else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
            else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
            if (doselait == 0) println("Lait en supplément : Non")
            else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
            else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
            else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

            if ((poudrecafe - 12 >= 0) && (stocksucre - sucreclient >= 0) && (stocklait - laitclient - 0.2 >= 0)) {
              erreur = false
              poudrecafe = poudrecafe - 12
              stocksucre = stocksucre - sucreclient
              stocklait = stocklait - laitclient - 0.2
              prix = 3.70 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
              printf("Prix total : CHF 3.70 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
              println
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
              println("(En attente de paiement...)\n")
              Thread.sleep(3000)
              println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
            }

            else {
              println
              if (poudrecafe - 12 < 0) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocksucre - sucreclient < 0) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n")
              if (stocklait - laitclient - 0.2 < 0) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.\n")
            }
          }
          boisson = 0
          taillelatte = 0
          doselait = 4
        }
      }

      if (mode == 2) {
        println("Mode Admin\nEntrez le code PIN : ")
        lecturecodePIN = readLine()
        while (lecturecodePIN != codePIN) {
          println("Code PIN erroné, veuillez réessayer : ")
          lecturecodePIN = readLine()
        }

        println("Accès autorisé.\n")
        printf("Stocks:\n\tPoudre de café: " + poudrecafe + "g\n\tLait\t\t  : " +  stocklait + "L\n\tSucre\t\t  : " + stocksucre + "g\n")
        println("Réapprovisionnement des stocks...\nAjout :")

        while (ajoutpoudrecafe < 0) {
          ajoutpoudrecafe = readLine("\tPoudre de café: ").toInt
        }
        poudrecafe = poudrecafe + ajoutpoudrecafe
        ajoutpoudrecafe = -1

        while (ajoutlait < 0) {
          ajoutlait = readLine("\tLait\t\t  : ").toDouble
        }
        stocklait = stocklait + ajoutlait
        ajoutlait = -1.0

        while (ajoutsucre < 0) {
          ajoutsucre = readLine("\tSucre\t\t  : ").toInt
        }
        stocksucre = stocksucre + ajoutsucre
        ajoutsucre = -1
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      }
      println
      mode = 0
    }
  }
}
