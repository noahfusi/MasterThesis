import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    //quantités initiales des ingrédients
    var cafe = 50
    var sucre = 30
    var lait = 0.5
    //initialisation du code admin
    val mdp = "434343"
    //initialisation des prix et quantités
    var prixcafe = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var taille = 0
    var doselait = 0
    var supplementlait = 0
    var supplementsucre = 0
    //quantités de café, sucre et lait nécessaire à la confection de la boisson
    var cafeNecessaire=0
    var laitNecessaire=0.0
    var sucreNecessaire=0
    //variables de réapprovisionnement des stocks
    var ajoutcafe=0
    var ajoutlait=0
    var ajoutsucre=0
    //initialisation des caractères pour le code
    var codepaie = ""
    val caractere = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    //initialisation de la machine
    var boucle = true
    while (boucle) {
      // affichage de la machine
      println("           Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var mode = readLine(">").toInt
      //assurer que le mode sélectionné soit valide
      while (mode < 1 || mode > 3) {
        println("le mode n'existe pas")
        mode = readLine(">").toInt
      }

      if (mode == 1) {
        //choix des boissons du client
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        var boisson = readLine(">").toInt
        //test de la valeur de boisson
        while (boisson < 0 || boisson > 3) {
          println("cette boisson n'est pas valide")
          boisson = readLine(">").toInt
        }
        //choix de la taille du Latte
        if (boisson == 3) {
          println("1) Petit\n2) Moyen\n3) Grand")
          taille = readLine(">").toInt
          while (taille<1 || taille>3){
            println("la taille n'est pas valide")
            taille = readLine(">").toInt
          }
        }

        // ajout sucre
        println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
        supplementsucre = readLine(">").toInt
        while (supplementsucre < 0 || supplementsucre > 4) {
          println("cette quantité n'est pas valide")
          supplementsucre = readLine(">").toInt
        }

        //choix du supplément en lait, restraint au cappucino et latte
        if (boisson == 2 || boisson == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
          supplementlait = readLine(">").toInt
          while (supplementlait < 0 || supplementlait > 2) {
            println("supplément de lait invalide")
            supplementlait = readLine(">").toInt
          }

          if (supplementlait == 1) {
            println("Combien de doses? - 1 à 3")
            doselait = readLine(">").toInt
          }

          while (doselait < 0 || doselait > 3) {
            if (doselait < 0 || doselait > 3) {
              println("ce nombre de dose de lait n'est pas valide")
              doselait = readLine(">").toInt
            } else {
              println("combien de dose? - 1 à 3")
              doselait = readLine(">").toInt
            }
          }
        }

        //afficher le choix de la boisson et déterminer les quantités nécessaire à sa confection
        if (boisson == 1) {
          println("Boisson sélectionnée : Expresso")
          cafeNecessaire = 8
          prixcafe += 2.0
        }
        if (boisson == 2) {
          println("Boisson sélectionnée : Cappuccino")
          cafeNecessaire = 6
          laitNecessaire= 0.1
          prixcafe += 2.5
        }

        if (boisson == 3) {
          if (taille == 1) {
            println("Boisson sélectionnée : Latte (Petit)")
            cafeNecessaire = 6
            laitNecessaire= 0.12
            prixcafe += 2.7
          }
          if (taille == 2) {
            println("Boisson sélectionnée : Latte (Moyen)")
            cafeNecessaire= 8
            laitNecessaire= 0.15
            prixcafe += 3.2
          }
          if (taille == 3) {
            println("Boisson sélectionnée : Latte (Grand)")
            cafeNecessaire= 12
            laitNecessaire= 0.2
            prixcafe += 3.7
          }
        }

        //afficher et déterminer la quantité de sucre
        if (supplementsucre == 1) {
          println("Niveau de sucre : Sans sucre")
        }
        if (supplementsucre == 2) {
          println("Niveau de sucre : Peu (5g)")
          sucreNecessaire= 5
          prixsucre += 0.1
        }
        if (supplementsucre == 3) {
          println("Niveau de sucre : Moyen (10g)")
          sucreNecessaire= 10
          prixsucre += 0.2
        }
        if (supplementsucre == 4) {
          println("Niveau de sucre : Beaucoup (15g)")
          sucreNecessaire= 15
          prixsucre += 0.3
        }

        //Afficher la dose de lait
        if (supplementlait == 2) {
          println("Lait en supplément: Non")
        }
        if (doselait == 1) {
          println("Lait en supplément: 1 dose")
          laitNecessaire+=0.05
          prixlait += 0.05
        }
        if (doselait == 2) {
          println("Lait en supplément: 2 dose")
          laitNecessaire+=0.1
          prixlait += 0.1
        }
        if (doselait == 3) {
          println("Lait en supplément: 3 dose")
          laitNecessaire+=0.15
          prixlait += 0.15
        }

        //indiquer si la quantité est insuffisante
        if (cafe < cafeNecessaire || sucre < sucreNecessaire || lait < laitNecessaire) {
          if (cafe < cafeNecessaire) {
            println("Erreur : Quantité de café insuffisante pour préparer\nla boisson sélectionnée.")
          }
          else if (sucre < sucreNecessaire) {
            println("Erreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.")
          }
          else if (lait < laitNecessaire) {
            println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.")
          }
          if(taille==2||taille==3){
            println("Veuillez choisir une taille plus petite ou essayer\nune autre boisson.")
          }else{
            println("Veuillez choisir une autre boisson ou v´erifier les\nstocks en mode Admin.")
          }
        }

        //affichage du prix total
        var prixtotal: Double = math.round((prixcafe + prixlait + prixsucre) * 100) / 100.0

        if (cafe >= cafeNecessaire && sucre >= sucreNecessaire && lait >= laitNecessaire) {
          printf("Prix total : CHF %.2f" , prixcafe)
          if (supplementsucre > 1 && supplementsucre < 5) {
            printf(" + CHF %.2f" , prixsucre)
          }
          if (doselait > 0 && doselait < 4) {
            printf(" + CHF %.2f" , prixlait)
          }
          printf(" = CHF %.2f" , prixtotal)
          //génération du code de paiement
          for (_ <- 1 to 5) {
            val randomChar = caractere(Random.nextInt(caractere.length))
            codepaie += randomChar
          }
          //paiement
          println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codepaie + "\n(En attente de paiement...)")
          Thread.sleep(3000)
          println("Paiement confirmé. \nen attente de votre boisson ...")
          Thread.sleep(5000)

          //message de fin
          if (boisson == 1) {
            println("Votre Expresso est prêt ! Bonne dégustation !")
          }
          if (boisson == 2) {
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          }
          if(boisson == 3)  {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
          if(cafe >= cafeNecessaire && sucre >= sucreNecessaire && lait >= laitNecessaire) {
            //réduction des stocks
            cafe -= cafeNecessaire
            lait -= laitNecessaire
            sucre -= sucreNecessaire
            //réinitialisation des valeurs
            prixcafe=0
            prixsucre=0
            prixlait=0
            prixtotal=0.0
            cafeNecessaire=0
            laitNecessaire=0
            sucreNecessaire=0
            supplementsucre = 0
            doselait=0
            taille=0
            supplementlait = 0
            codepaie=""
          }
        }

      }
      //affichage du mode admin
      if (mode == 2) {
        println("Mode Admin")
        val essaimdp = readLine("Entrez le code PIN : ")
        if (essaimdp == mdp) {
          println("accès autorisé.\nStocks:\nPoudre de café: "+cafe+"g\nLait          : "+lait+"L\nSucre         : "+sucre+"g\nAjout :")
          ajoutcafe=readLine("Poudre de café: ").toInt
          ajoutlait=readLine("Lait          : ").toInt
          ajoutsucre=readLine("Sucre         : ").toInt
          //remise à niveau des stocks
          cafe+=ajoutcafe
          lait+=ajoutlait
          sucre+=ajoutsucre
          ajoutcafe=0
          ajoutlait=0
          ajoutsucre=0
          println("Niveaux de stock mis à jour.\nRetour au menu principal...")
        } else {
          if(essaimdp!=mdp) {
            println("mot de passe incorrect")
          }
        }
      }
      //quitter l'interface
      if (mode == 3) {
        println("bonne journée!")
        boucle = false
      }
    }
  }
}