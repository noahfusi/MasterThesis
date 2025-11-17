import scala.io.StdIn._
import scala.util.Random

object Main {
  var stockCafe = 50.0
  var stockSucre = 30.0
  var stockLait = 500.0

  def main(args: Array[String]): Unit = {

    var continuer = true
    while (continuer) {
      println("Bienvenue dans NosprossoCafé  ")
      println("Veuillez sélectionner votre mode : ")
      println("1- Client ")
      println("2- Admin ")
      println("3- Quitter ")

      var Choix = 0
      while (Choix != 1 && Choix != 2 && Choix != 3) {
        Choix = scala.io.StdIn.readInt()
        if (Choix != 1 && Choix != 2 && Choix != 3) {
          println(" Veuillez entrer un choix Valide (1,2,3). ")
        }
      }
      if (Choix == 1) {
        println("Mode selectionné : Client ")
        println("Veuillez sélectionner votre boisson :")
        println(" 1-Expresso : 2.00 CHF ")
        println(" 2-Cappucino : 2.50 CHF ")
        println(" 3-Latte : \n-Petit: 2.70 CHF,\n-Moyen : 3.20 CHF,\n-Grand : 3.70 CHF ")

        var ChoixBoisson = 0
        var prixBoisson = 0.0
        var besoinCafe = 0.0
        var besoinLait = 0.0
        while (ChoixBoisson != 1 && ChoixBoisson != 2 && ChoixBoisson != 3) {
          ChoixBoisson = scala.io.StdIn.readInt()
          if (ChoixBoisson != 1 && ChoixBoisson != 2 && ChoixBoisson != 3) {
            println("Veuillez entrer votre choix valide (1,2,3).")
          }
        }

        var Boisson = ""
        if (ChoixBoisson == 1) {
          Boisson = "Expresso"
          prixBoisson = 2.00
          besoinCafe = 8
        } else if (ChoixBoisson == 2) {
          Boisson = "Cappuccino"
          prixBoisson = 2.50
          besoinCafe = 6
          besoinLait = 100
        } else if (ChoixBoisson == 3) {
          println("Sélectionnez la taille de votre Latte : 1-Petit, 2-Moyen, 3-Grand")
          var taille = 0
          while ((taille < 1) || (taille > 3)) {
            taille = readInt()
            if ((taille < 1) || (taille > 3)) {
              println("Taille invalide. Veuillez entre 1,2 ou 3.")
            }
          }
          if (taille == 1) {
            Boisson = "Latte Petit."
            prixBoisson = 2.70
            besoinCafe = 6
            besoinLait = 120
          } else if (taille == 2) {
            Boisson = "Latte Moyen"
            prixBoisson = 3.20
            besoinCafe = 8
            besoinLait = 150
          } else if (taille == 3) {
            Boisson = "Latte Grand"
            prixBoisson = 3.70
            besoinCafe = 12
            besoinLait = 200
          }
        }

        println("Souhaitez vous ajouter du sucre ? ")
        println(" 1- Sans sucre.")
        println(" 2- Peu (5g) à 0.10 CHF.")
        println(" 3- Moyen (10g) à 0.20 CHF.")
        println(" 4- Beaucoup (15g) à 0.30 CHF.")

        var ChoixSucre = 0
        var prixSucre = 0.0
        var besoinSucre = 0.0
        while ((ChoixSucre < 1) || (ChoixSucre > 4)) {
          ChoixSucre = readInt()
          if ((ChoixSucre < 1) || (ChoixSucre > 4)) {
            println("Veuillez entrer un choix valide (1,2,3 ou 4).")
          }
        }

        if (ChoixSucre == 2) {
          prixSucre = 0.10
          besoinSucre = 5
        } else if (ChoixSucre == 3) {
          prixSucre = 0.20
          besoinSucre = 10
        } else if (ChoixSucre == 4) {
          prixSucre = 0.30
          besoinSucre = 15
        }

        var DoseLait = 0
        var prixLait = 0.0
        var ajouterLait = 2
        if ((ChoixBoisson == 2) || (ChoixBoisson == 3)) {
          println("Souhaitez-vous ajouter du lait supplément ? (1-Oui, 2-Non)")
          ajouterLait = readInt()
          while ((ajouterLait != 1 )&&(ajouterLait !=2) ) {
            println ("Choix invalide, Entrez 1 pour Oui ou 2 pour Non.")
            ajouterLait = readInt()
          }
          if (ajouterLait == 1) {
            println("Combien de doses ? : Max 3 doses")
            DoseLait = readInt()
            while ((DoseLait < 0) || (DoseLait > 3)) {
              println("Nombre de doses invalide.Veuillez entrer une valeur entre 0 et 3. ")
              DoseLait = readInt()
            }
            prixLait = DoseLait * 0.05
            besoinLait += DoseLait * 50
          }
        }

        val prixTotal = prixBoisson + prixSucre + prixLait
        if (besoinCafe > stockCafe) {
          println("Erreur : Quantité de poudre café insuffisante. pour préparer la boisson sélectionnée.")
          println("Veuillez vérifier les stocks en mode Admin.")
        } else if (besoinSucre > stockSucre) {
          println("Erreur : Quantité de sucre insuffisante. pour préparer la boisson sélectionnée.")
          println("Veuillez vérifier les stocks en mode Admin.")
        } else if (besoinLait > stockLait) {
          println("Erreur : Quantité de lait insuffisante. pour préparer la boisson sélectionnée.")
          if (ChoixBoisson == 3) {
            println("Veuillez choisir une taille plus petite ou essayez un autre boisson.")
          } else if ((ChoixBoisson == 1) || (ChoixBoisson == 2)) {
            println("Veuillez essayer une autre boisson.")
          }
        }else {

          stockCafe -= besoinCafe
          stockSucre -= besoinSucre
          stockLait -= besoinLait

          println("Récapitulatif  de votre boisson")
          println("Boisson : " + Boisson)
          if (ChoixSucre == 1) {
            println("Sans sucre ")
          } else if (ChoixSucre == 2) {
            println("Peu de sucre (5g).")
          } else if (ChoixSucre == 3) {
            println("Moyen de sucre (10g).")
          } else {
            println("Beaucoup de sucre (15g).")
          }
          if (DoseLait > 0) {
            println("Lait en supplément : " + DoseLait + " dose(s)")
          } else {
            println("Pas de lait en supplément.")
          }
          if ((ChoixSucre == 1) && (ajouterLait == 2)) {
            printf("Prix Total : CHF %.2f CHF\n", prixBoisson)
          } else if (((ChoixSucre == 2) || (ChoixSucre == 3) || (ChoixSucre == 4)) && (ajouterLait == 2)) {
            printf("Prix Total : CHF %.2f + CHF %.2f  = CHF %.2f\n", prixBoisson, prixSucre, prixTotal)
          } else if ((ChoixSucre == 1) && (ajouterLait == 1)) {
            printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixLait, prixTotal)
          } else if (((ChoixSucre == 2) || (ChoixSucre == 3) || (ChoixSucre == 4)) && (ajouterLait == 1)) {
            printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixSucre,prixLait, prixTotal)
          }

          println("Veuillez payer en utilisant Twint.")
          val codeTwint = Random.alphanumeric.take(5).mkString
          println("Votre code de paiment est :" + codeTwint)
          println("(En attente de paiement...)")
          Thread.sleep(5000)
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")
          println("votre " + Boisson + " est pret ! Bonne dégustation !")
        }
      } else if (Choix == 2) {
        println("Mode selectionné : Admin ")
        println("Veuillez entrer le code PIN pour accéder au mode Admin")
        val codePIN = 434343
        var pinEntree = readInt()
        // verification du code PIN
        var essais = 3
        while (pinEntree != codePIN && essais > 1) {
          essais -= 1
          println("code incorrect. il vous reste " + essais + "essais.")
          pinEntree = readInt()
        }
        if (pinEntree == codePIN) {
          println("Accès autorisé.")
          println("Stocks actuels : ")
          printf("Poudre de café : %.2f g\n ", stockCafe)
          printf("Sucre : %.2f g\n", stockSucre)
          printf("Lait : %.2f l\n", stockLait)
          println("Réapprovisionnement des stocks...")
          println("Entrez la quantité à ajouter pour chaque ingrédient :")

          var ajoutCafe = -1.0
          var ajoutSucre = -1.0
          var ajoutLait = -1.0

          while (ajoutCafe < 0) {
            println("Poudre de café (en g) :")
            ajoutCafe = readDouble()
            if (ajoutCafe < 0) {
              println("Erreur : la quantité doit etre positive.Veuillez Réessayer ! ")
            }
          }
          while (ajoutSucre < 0) {
            println("Sucre (en g) :")
            ajoutSucre = readDouble()
            if (ajoutSucre < 0) {
              println("Erreur : la quantité doit etre positive.Veuillez réessayer ! ")
            }
          }

          while (ajoutLait < 0) {
            println("Lait (en L) :")
            ajoutLait = readDouble()
            if (ajoutLait < 0) {
              println("Erreur : la quantité doit etre positive.Veuillez réessayer ")
            }
          }

          stockCafe += ajoutCafe
          stockSucre += ajoutSucre
          stockLait += ajoutLait

          println("Ajout : ")
          printf("Poudre de café : %.2f g\n", ajoutCafe)
          printf("Sucre : %.2f g\n", ajoutSucre)
          printf("Lait : %.2f l\n", ajoutLait)
          println("Niveau de stocks mis à jour.")
          println("Retour au menu principal...")
        } else {
          println("Accès refusé.")
        }
      } else if (Choix == 3) {
        println("Programme Terminé. Au revoir ! ")
        continuer = false
      }
    }
  }
}