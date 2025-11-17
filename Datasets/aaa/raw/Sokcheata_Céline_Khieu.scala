import io.StdIn._
import math._

object Main {
  def main(args: Array[String]): Unit = {
    //déclaration variables: vérifier que tout est là
    //prix
    val prixespresso = 2.00
    val prixcappucino = 2.50
    val prixlattepetit = 2.70
    val prixlattemoyen = 3.20
    val prixlattegrand = 3.80
    val prixpassucre = 0.00
    val prixpeusucre = 0.10
    val prixmoyensucre = 0.20
    val prixbcpsucre = 0.30
    val prixlait = 0.00
    //consommation des ingrédients
    val consommationespresso = 8.0
    val consommationcappucino = 6.0
    val consommationcappucinolait = 0.100
    val consommationlattepetit = 6.0
    val consommationlattepetitlait = 0.120
    val consommationlattemoyen = 8.0
    val consommationlattemoyenlait = 0.150
    val consommationlattegrand = 12.0
    val consommationlattegrandlait = 0.200
    val consommationsucrepeu = 5.0
    val consommationsucremoyen = 10.0
    val consommationsucrebcp = 15.0
    //quantité stock initial
    var Qcafe = 50.0
    var Qsucre = 30.0
    var Qlait = 0.500

    //menu de sélection mode
    //boucle do while pour continuer le programme après les transactions
    var retourmenu = true
    do {
    println("         Nospresso Café          ")
    println("Veullez sélectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quittez")
    var choixmode = readInt()
    //(saut à la ligne entre 1) 2) 3))
    //(if client else if admin else quittez)
    //(gestion erreur : relancer jusqu'à entrée valide (boucle while))
    while (!(choixmode == 1 || choixmode == 2 || choixmode == 3)) {
      println("Votre sélection n'est pas correcte.")
      println("Choississez entre :")
      println("1) Client")
      println("2) Admin")
      println("3) Quittez")
      choixmode = readInt()
    }
    if (choixmode == 1) {
      //mode client: commander une boisson
      println("Mode Client")
      var prixtotal = 0.00
      var prixboisson = 0.00
      //choix utlisateur entre Espresso, Cappucino, Latte
      println("Veuillez sélectionner votre boisson :")
      println("1) Espresso - CHF 2.00")
      println("2) Cappucino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      var choixboisson = readInt()
      var boisson = " "
      while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3)) {
        println("Votre sélection n'est pas correcte.")
        println("Choississez entre :")
        println("1) Espresso - CHF 2.00")
        println("2) Cappucino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choixboisson = readInt()
      }

      //choix espresso 1 //ajouter déducation des stocks
      if (choixboisson == 1) {
        boisson = "Nespresso"
        prixboisson = prixespresso
        Qcafe -= consommationespresso
        //choix cappucino 2
      } else if (choixboisson == 2) {
        boisson = "Cappucino"
        prixboisson = prixcappucino
        Qcafe -= (consommationcappucino + consommationcappucinolait)
      } else {
        //choix latte 3
        println("Veuillez sélectionner la taille de votre latte :")
        println("1) Petit Latte - CHF 2.70")
        println("2) Moyen Latte - CHF 3.20")
        println("3) Grand Latte - CHF 3.70")
        var choixlatte = readInt()
        while (!(choixlatte == 1 || choixlatte == 2 || choixlatte == 3)) {
          println("Votre sélection n'est pas correcte.")
          println("Choississez entre :")
          println("1) Petit Latte - CHF 2.70")
          println("2) Moyen Latte - CHF 3.20")
          println("3) Grand Latte - CHF 3.70")
          choixlatte = readInt()
        }
        if (choixlatte == 1) {
          boisson = "Latte (Petit)"
          prixboisson = prixlattepetit
          Qcafe -= (consommationlattepetit + consommationlattepetitlait)
        } else if (choixlatte == 2) {
          boisson = "Latte (Moyen)"
          prixboisson = prixlattemoyen
          Qcafe -= (consommationlattemoyen + consommationlattemoyenlait)
        } else {
          boisson = "Latte (Grand)"
          prixboisson = prixlattegrand
          Qcafe -= (consommationlattegrand + consommationlattegrandlait)
        }
      }
      //choix de sucre : Pas de sucre, Peu de sucre (5g), Moyen (10g), Beaucoup (15g)
      var prixsucre = 0.00
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      var choixsucre = readInt()
      var sucre = " "
      while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4)) {
        println("Votre sélection n'est pas correcte.")
        println("Choississez entre :")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        choixsucre = readInt()
      }
      if (choixsucre == 1) {
        sucre = "Sans sucre"
        prixsucre = prixpassucre
      } else if (choixsucre == 2) {
        sucre = "Peu (5g)"
        prixsucre = prixpeusucre
        Qsucre -= consommationsucrepeu
      } else if (choixsucre == 3) {
        sucre = "Moyen (10g)"
        prixsucre = prixmoyensucre
        Qsucre -= consommationsucremoyen
      } else {
        sucre = "Beaucoup (15g)"
        prixsucre = prixbcpsucre
        Qsucre -= consommationsucrebcp
      }

      //choix lait (cappucino ou latte seulement)
      //si cappucino ou latte (if else) : ajouter 50ml de lait (3 doses max/boisson)
      //Si oui (sinon l'instruction n'est pas lu)
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappucino et Latte)")
      println("1) Oui")
      println("2) Non")
      var choixlait = readInt()
      var choixlaitsupplement = " "
      while (!(choixlait == 1 || choixlait == 2)) {
        println("Votre sélection n'est pas correcte.")
        println("Choississez entre :")
        println("1) Oui")
        println("2) Non")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        choixlaitsupplement = "Oui"
        print("Combien de dose ?")
        var choixlaitdose = readInt()
        while (!(choixlaitdose == 1) || (choixlaitdose == 2) || (choixlaitdose == 3)) {
          println("Votre sélection n'est pas correcte.")
          println("Vous avez le droit à 3 doses maximum par boisson")
          choixlaitdose = readInt()
        }
      } else {
        choixlaitsupplement = "Non"
      }

      //gestion des stocks //comment mettre quantité de... ??
      if ((Qcafe <= 0) || (Qsucre <= 0) || (Qlait <= 0)) {
        println("Erreur : Quantité d'ingrédient insuffisant pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      } else {
        //Paiement :
        //afficher prix total
        println("Boisson sélectionnée: " + boisson)
        println("Niveau de sucre: " + sucre)
        println("Lait en supplément: " + choixlaitsupplement) //(faire afficher : supplément lait OUI ou NON)
        var prixtotal = prixboisson + prixsucre + prixlait
        println("Prix total: CHF " + prixtotal)
        //demander code TWINT -> code aléatoire (math.random) //code alphanumérique pas trouvé dans la library scala ??
        var twint = " "
        for (i <- 1 to 5) {
          twint += (math.random() * 10).toInt
        }
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est: " + twint)
        println("(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre " + boisson + " est prêt ! Bonne dégustation !")
      }

    } else if (choixmode == 2) {
      //mode admin:
      //demande de code pin (if...else)
      println("Mode Admin")
      val mdp = 434343
      print("Entrez le code PIN:")
      var proposition = readInt()
      while (!(proposition == mdp)) {
        println("Accès non autorisé.")
        print("Entrez le code PIN:")
        proposition = readInt()
      }
      println("Accès autorisé.")

      //réaprovisation de stock -> ajout manuel de quantité (calcul addition dans stock)
      //affichage stock
      //choix de l'ingrédient à restocker
      //ajout manuel de quantité
      println("Stocks : ")
      println("Poudre de café: " + Qcafe + "g")
      println("Lait: " + Qlait + "L")
      println("Sucre: " + Qsucre + "g")
      println("Réaprovisionnement des stocks...")
      print("1) Poudre de café: ")
      val ajoutcafe = readDouble()
      Qcafe += ajoutcafe
      print("2) Lait: ")
      val ajoutlait = readDouble()
      Qlait += ajoutlait
      print("3) Sucre: ")
      val ajoutsucre = readDouble()
      Qsucre += ajoutsucre
      println("Niveaux de stock mis à jour.")
      println("Retour au menu principal...")
      //fin du programme ?

    } else {
      //fin du programme
      retourmenu = false

    }
    } while (retourmenu)
  }
}