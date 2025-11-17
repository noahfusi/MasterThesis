import scala.io.StdIn._

object Nospresso {
  def main(args: Array[String]): Unit = {
    val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var reiteration = true
    var Poudre_de_cafe = 50
    var lait = 500
    var sucre = 30
    val code_PIN = 434343

    while(reiteration){

    var codedepaiement = ""
    for (_ <- 1 to 5) {
      val randomChar = alphanumerique((math.random * alphanumerique.length).toInt)
      codedepaiement += randomChar
    }

    var choixmode = 0
    while (choixmode < 1 || choixmode > 3) {
      print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
      choixmode = readInt()
      if (choixmode < 1 || choixmode > 3) {
        println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
      }
    }
    if (choixmode == 1) {
      var prixtotal = 0.00
      var choixboisson = 0
      while (choixboisson < 1 || choixboisson > 3 || choixboisson == 1 && Poudre_de_cafe < 8 || choixboisson == 2 && lait < 100 || choixboisson == 2 && Poudre_de_cafe < 6 || choixboisson == 3 && lait < 120 || choixboisson == 3 && Poudre_de_cafe < 6) {
        print("Veuillez selectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
        choixboisson = readInt()
        if (choixboisson < 1 || choixboisson > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
        }
        if (choixboisson == 1 && Poudre_de_cafe < 8){
          println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        if (choixboisson == 2 && Poudre_de_cafe < 6){
          println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        if (choixboisson == 2 && lait < 100){
          println("Erreur : quantité de lait insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        if (choixboisson == 3 && Poudre_de_cafe < 6){
          println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        if (choixboisson == 3 && lait < 120){
          println("Erreur : quantité de lait insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre boisson ou vérifier les stocks en mode Admin.\n")
        }
      }
      if (choixboisson == 1) {
        prixtotal += 2.00
        Poudre_de_cafe -= 8
        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre < 5 || choixsucre == 3 && sucre < 10 || choixsucre == 4 && sucre < 15) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            println("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.")
          }
          if (choixsucre == 2 && sucre < 5) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 3 && sucre < 10) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 4 && sucre < 15) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
        }
        if (choixsucre == 2) {
          prixtotal += 0.10
          sucre -= 5
        }
        if (choixsucre == 3) {
          prixtotal += 0.20
          sucre -= 10
        }
        if (choixsucre == 4) {
          prixtotal += 0.30
          sucre -= 15
        }
        printf("Prix total : %.2f CHF\n", prixtotal)

        println("Veuillez payer en utilisant Twint.")
        printf("Votre code de paiement est : %s\n(En attente de validation du paiement...)\n", codedepaiement)
        Thread.sleep(3000)
        printf("Merci, votre paiement a été accepté.\n\n")

        printf("Préparation de votre boisson...\n[...]\n")
        Thread.sleep(7000)
        printf("Votre Espresso est prêt ! Bonne dégustation !\n\n")

      }
      else if (choixboisson == 2) {
        prixtotal += 2.50
        Poudre_de_cafe -= 6
        lait -= 100

        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre < 5 || choixsucre == 3 && sucre < 10 || choixsucre == 4 && sucre < 15) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            println("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.")
          }
          if (choixsucre == 2 && sucre < 5) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 3 && sucre < 10) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 4 && sucre < 15) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
        }
        if (choixsucre == 2) {
          prixtotal += 0.10
          sucre -= 5
        }
        if (choixsucre == 3) {
          prixtotal += 0.20
          sucre -= 10
        }
        if (choixsucre == 4) {
          prixtotal += 0.30
          sucre -= 15
        }

        var choixlait = 0
        while (choixlait < 1 || choixlait > 2) {
          print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pout Cappucino et Latte)\n1) Oui\n2) Non\n>")
          choixlait = readInt()
          if (choixlait < 1 || choixlait > 2) {
            println("Choix invalide. Veuillez sélectionner 1 ou 2.")
          }
        }
        if (choixlait == 1) {
          var choixdose = 0
          while (choixdose < 1 || choixdose > 3 || choixdose == 1 && lait < 50 || choixdose == 2 && lait < 100 || choixdose == 3 && lait < 150) {
            print("Combien de doses ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) Trois doses (150ml) - CHF 0.15\n>")
            choixdose = readInt()
            if (choixdose < 1 || choixdose > 3) {
              println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
            }
            if (choixdose == 1 && lait < 50) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une autre boisson.\n")
            }
            if (choixdose == 2 && lait < 100) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
            if (choixdose == 3 && lait < 150) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
          }
          if (choixdose == 1) {
            prixtotal += 0.05
            lait -= 50
          }
          if (choixdose == 2) {
            prixtotal += 0.10
            lait -= 100
          }
          if (choixdose == 3) {
            prixtotal += 0.15
            lait -= 150
          }
        }
        printf("Prix total : %.2f CHF\n", prixtotal)

        println("Veuillez payer en utilisant Twint.")
        printf("Votre code de paiement est : %s\n(En attente de validation du paiement...)\n", codedepaiement)
        Thread.sleep(3000)
        printf("Merci, votre paiement a été accepté.\n\n")

        printf("Préparation de votre boisson...\n[...]\n")
        Thread.sleep(7000)
        printf("Votre Capuccino est prêt ! Bonne dégustation !\n\n")
      }
      else if (choixboisson == 3) {
        var choixtaille = 0
        while (choixtaille < 1 || choixtaille > 3 || choixtaille == 2 && Poudre_de_cafe < 8 || choixtaille == 2 && lait < 150 || choixtaille == 3 && Poudre_de_cafe < 12 || choixtaille == 2 && lait < 200) {
          print("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand -  CHF 3.70\n>")
          choixtaille = readInt()
          if (choixtaille < 1 || choixtaille > 3) {
            println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
          }
          if(choixtaille == 2 && lait < 150){
            println("Erreur : quantité de lait insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre taille ou essayer une autre boisson.\n")
          }
          if(choixtaille == 2 && Poudre_de_cafe < 8){
            println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre taille ou essayer une autre boisson.\n")
          }
          if(choixtaille == 3 && lait < 200){
            println("Erreur : quantité de lait insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre taille ou essayer une autre boisson.\n")
          }
          if(choixtaille == 3 && Poudre_de_cafe < 12){
            println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir un autre taille ou essayer une autre boisson.\n")
          }

          if (choixtaille == 1) {
            prixtotal += 2.70
            Poudre_de_cafe -= 6
            lait -= 120
          }
          if (choixtaille == 2) {
            prixtotal += 3.20
            Poudre_de_cafe -= 8
            lait -= 150
          }
          if (choixtaille == 3) {
            prixtotal += 3.70
            Poudre_de_cafe -= 12
            lait -= 200
          }
        }

        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre < 5 || choixsucre == 3 && sucre < 10 || choixsucre == 4 && sucre < 15) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            println("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.")
          }
          if (choixsucre == 2 && sucre < 5) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 3 && sucre < 10) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 4 && sucre < 15) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
        }
        if (choixsucre == 2) {
          prixtotal += 0.10
          sucre -= 5
        }
        if (choixsucre == 3) {
          prixtotal += 0.20
          sucre -= 10
        }
        if (choixsucre == 4) {
          prixtotal += 0.30
          sucre -= 15
        }

        var choixlait = 0
        while (choixlait < 1 || choixlait > 2) {
          print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pout Cappucino et Latte)\n1) Oui\n2) Non\n>")
          choixlait = readInt()
          if (choixlait < 1 || choixlait > 2) {
            println("Choix invalide. Veuillez sélectionner 1 ou 2.")
          }
        }
        if (choixlait == 1) {
          var choixdose = 0
          while (choixdose < 1 || choixdose > 3 || choixdose == 1 && lait < 50 || choixdose == 2 && lait < 100 || choixdose == 3 && lait < 150) {
            print("Combien de doses ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) Trois doses (150ml) - CHF 0.15\n>")
            choixdose = readInt()
            if (choixdose < 1 || choixdose > 3) {
              println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
            }
            if (choixdose == 1 && lait < 50) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une autre boisson.\n")
            }
            if (choixdose == 2 && lait < 100) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
            if (choixdose == 3 && lait < 150) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
          }
          if (choixdose == 1) {
            prixtotal += 0.05
            lait -= 50
          }
          if (choixdose == 2) {
            prixtotal += 0.10
            lait -= 100
          }
          if (choixdose == 3) {
            prixtotal += 0.15
            lait -= 150
          }
        }
        printf("Prix total : %.2f CHF\n", prixtotal)

        println("Veuillez payer en utilisant Twint.")
        printf("Votre code de paiement est : %s\n(En attente de validation du paiement...)\n", codedepaiement)
        Thread.sleep(3000)
        printf("Merci, votre paiement a été accepté.\n\n")

        printf("Préparation de votre boisson...\n[...]\n")
        Thread.sleep(7000)
        printf("Votre Latte est prêt ! Bonne dégustation !\n\n")
      }
    }
    else if (choixmode == 2) {
      var PIN = 0
      while (PIN != 434343) {
        print("Mode Admin\nEntrez le code PIN : ")
        PIN = readInt()
        if (PIN != 434343)
          println("Code faux. Veuillez réessayer.")
      }
      println("Accès autorisé.\n")
      println("Stocks : ")
      printf("Poudre de café : %d g\nLait : %d ml\nSucre : %d g\n", Poudre_de_cafe, lait, sucre)
      println("Réapprovisionnement des stocks.")

      var choixingredient = 0
      while (choixingredient < 1 || choixingredient > 3) {
        print("1) Ajouter de la poudre de café\n2) Ajouter du lait\n3) Ajouter du sucre\n>")
        choixingredient = readInt()
        if (choixingredient < 1 || choixingredient > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
        }
      }
      if (choixingredient == 1) {
        print("Veuillez indiquer la quantité rajoutée en grammes :\n>")
        Poudre_de_cafe += readInt()
        printf("Poudre de café = %d g\n\n", Poudre_de_cafe)
      }
      if (choixingredient == 2) {
        print("Veuillez indiquer la quantité rajoutée en millilitres :\n>")
        lait += readInt()
        printf("Lait = %d ml\n\n", lait)
      }
      if (choixingredient == 3) {
        print("Veuillez indiquer la quantité rajoutée en grammes :\n>")
        sucre += readInt()
        printf("Sucre = %d g\n\n", sucre)
      }
    }
    else if (choixmode == 3) {
      reiteration = false
      print("\t\tBye bye!")
    }
   }
 }
}