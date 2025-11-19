import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var continuer = true
    var boissonchoisie: Int = 0
    var nomboisson = ""
    var prixboisson = 0.00
    var taillelatte = 0
    var nomniveauSucre = ""
    var nomdoseLaitsupplement = ""
    var modechoisi: Int = 0

    // Stocks :
    var stockCafe = 50.0
    var stockSucre = 30.0
    var stockLait = 0.5

    //Prix :

    // Boissons :
    var prixfinalboisson = 0.00
    val prixExpresso = 2.00
    val prixCapuccino = 2.50
    val prixLattepetit = 3.20
    val prixLattemoyen = 3.20
    val prixLattegrand = 3.70

    // Sucre :
    var niveauSucre: Int = 0
    var prixquantiteSucre: Double = 0.0
    val prixpasSucre = 0.00
    val prixPeuSucre = 0.10
    val prixMoyenSucre = 0.20
    val prixBeaucoupSucre = 0.30

    // Lait :
    val prixLaitsupplement = 0.05
    var prixLaitsupplementfinal = 0.0
    var doseLaitsupplement = 0

    // Quantités de café, lait et sucre :

    var quantiteCafenecessaire = 0.0
    val quantiteCafeExpresso = 8
    val quantiteCafeCapuccino = 6
    val quantiteCafeLattePetit = 6
    val quantiteCafeLatteMoyen = 8
    val quantiteCafeLatteGrand = 12

    var quantiteLaitsupplement = 0.05
    var quantiteLaitnecessaire = 0.0
    val quantiteLaitCapuccino = 0.1
    val quantiteLaitLattePetit = 0.12
    val quantiteLaitLatteMoyen = 0.15
    val quantiteLaitLatteGrand = 0.2
    var Laitsupplement = 0.00

    var quantiteSucrenecessaire = 0
    val quantiteSansSucre = 0
    val quantitePeuSucre = 5
    val quantiteMoyenSucre = 10
    val quantiteBeaucoupSucre = 15


    while (continuer) {
      var modechoisi = readLine("Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter \n>").toInt

      // vérification du mode choisi
      while (!(modechoisi == 1 || modechoisi == 2 || modechoisi == 3)) {
        println("Votre sélection n'est pas correcte, veuillez choisir entre : \n1) Client\n2) Admin\n3) Quitter \n>")
        modechoisi = readInt()
      }

      if (modechoisi == 1) {
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ")
        boissonchoisie = readInt()

        if (!(boissonchoisie == 1 || boissonchoisie == 2 || boissonchoisie == 3)) {
          println("Votre sélection n'est pas correcte, veuillez chosir entre :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>")
        }

      if (boissonchoisie == 1) {
        nomboisson = "Expresso"
        prixboisson = prixExpresso
        quantiteCafenecessaire = quantiteCafeExpresso
        quantiteLaitnecessaire = 0
      } else if (boissonchoisie == 2) {
        nomboisson = "Capuccino"
        prixboisson = prixCapuccino
        quantiteCafenecessaire = quantiteCafeCapuccino
        quantiteLaitnecessaire = quantiteLaitCapuccino
      } else {
        var taillelatte = readLine("Veuillez sélectionner la taille de votre Latte : \n1) Latte Petit\n2) Latte Moyen\n3) Latte Grand \n>").toInt
        while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
          println("Votre sélection n'est pas correcte, veuillez choisir entre : \n1) Latte Petit \n2) Latte Moyen \n3) Latte Grand \n>")
          taillelatte = readInt()
        }


        if (taillelatte == 1) {
          nomboisson = "Latte Petit"
          prixboisson = prixLattepetit
          quantiteCafenecessaire = quantiteCafeLattePetit
          quantiteLaitnecessaire = quantiteLaitLattePetit
        } else if (taillelatte == 2) {
          nomboisson = "Latte Moyen"
          prixboisson = prixLattemoyen
          quantiteCafenecessaire = quantiteCafeLatteMoyen
          quantiteLaitnecessaire = quantiteLaitLatteMoyen
        } else {
          nomboisson = "Latte Grand"
          prixboisson = prixLattegrand
          quantiteCafenecessaire = quantiteCafeLatteGrand
          quantiteLaitnecessaire = quantiteLaitLatteGrand
        }
      }


      if (stockCafe < quantiteCafenecessaire) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson séléctionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        boissonchoisie = readInt()
      } else if (stockLait < quantiteLaitnecessaire) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n>")
        boissonchoisie = readInt()
      } else {
        println("Quel niveau de sucre souhaitez-vous ? : \n1) Sans sucre \n2) Un peu de sucre (5g) - 0.10 CHF \n3) Moyen (10g) - 0.20 CHF \n4) Beaucoup (15g) - 0.30 CHF \n>")
        niveauSucre = readInt()
      }

      if (niveauSucre == 1) {
        nomniveauSucre = "Pas de sucre"
        quantiteSucrenecessaire = quantiteSansSucre
        prixquantiteSucre = prixpasSucre
      } else if (niveauSucre == 2) {
        nomniveauSucre = "Peu"
        quantiteSucrenecessaire = quantitePeuSucre
        prixquantiteSucre = prixPeuSucre

      } else if (niveauSucre == 3) {
        nomniveauSucre = "Moyen"
        quantiteSucrenecessaire = quantiteMoyenSucre
        prixquantiteSucre = prixMoyenSucre
      } else {
        nomniveauSucre = "Beaucoup"
        quantiteSucrenecessaire = quantiteBeaucoupSucre
        prixquantiteSucre = prixBeaucoupSucre
      }

      if (stockSucre < quantiteSucrenecessaire) {
        println("Erreur : Quantité de sucre insuffisante pour le niveau de sucre séléctionné. \nVeuillez choisir un autre niveau de sucre. \n>")
        niveauSucre = readInt()
      }

      if (boissonchoisie == 2 || boissonchoisie == 3) {
        println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Capuccino et Latte) : \n1) Oui \n2) Non \n>")
        val Laitsupplement = readInt()

        if (Laitsupplement == 1) {
          println("Combien de doses ? \n>")

          do {
            doseLaitsupplement = readInt()
            quantiteLaitsupplement = doseLaitsupplement * quantiteLaitsupplement
            prixLaitsupplementfinal = doseLaitsupplement * prixLaitsupplement
            nomdoseLaitsupplement = "Oui"
          } while (doseLaitsupplement < 1 || doseLaitsupplement > 3)
        }
        if (Laitsupplement == 2) {
          doseLaitsupplement = 0
          nomdoseLaitsupplement = "Non"
        }
      }

      if (stockLait < quantiteLaitnecessaire + quantiteLaitsupplement) {
        println("Erreur : Quantité de lait insuffisante pour la dose de lait supplémentaire séléctionnée. \nVeuillez choisir un autre niveau de supplément de lait. \n>")
        doseLaitsupplement = readInt()
      } else {
        prixfinalboisson = prixboisson + prixquantiteSucre + prixLaitsupplement
      }

      println("Boisson sélectionnée : " + nomboisson + "\nNiveau de sucre : " + nomniveauSucre + "(" + quantiteSucrenecessaire + "g)")
      if (boissonchoisie == 2 || boissonchoisie == 3) {
        println("Lait en supplément : " + nomdoseLaitsupplement)
      }
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prixboisson, prixquantiteSucre, prixLaitsupplementfinal, prixfinalboisson)
      // Paiement
      val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
      println("Veuillez payer en utilisant Twint")
      println("Votre code de paiement est :" + codeTwint)
      println("En attente de la validation du paiement...")
      Thread.sleep(3000)
      println("Paiement confirmé")
      println("Préparation de votre boisson...")
      println("Votre" + nomboisson + " est prêt ! Bonne dégustation !")

      stockCafe -= quantiteCafenecessaire
      stockLait -= quantiteLaitnecessaire + quantiteLaitsupplement
      stockSucre -= quantiteSucrenecessaire
    }

      if (modechoisi == 2) {
      println("Entrez le code PIN : \n>")
      var pinAdmin: Int = 0
      do {
        pinAdmin = readInt()
      } while (pinAdmin != 434343)

      println("Accès autorisé. \n\nStocks : \n")
       printf("Poudre de café : %.2f g\n", stockCafe)
        printf("Lait : %.2f L\n", stockLait)
        printf("Sucre : %.2f g\n", stockSucre)
      println("Réapprovisionnement des stocks...")
      println("Ajout :")
      var ajoutCafe = readLine("Poudre de café >").toInt
      var ajoutLait = readLine("Lait >").toDouble
      var ajoutSucre = readLine("Sucre  >").toInt
      stockCafe += ajoutCafe
      stockLait += ajoutLait
      stockSucre += ajoutSucre
      println("Poudre de café: " + ajoutCafe)
      println("Lait: " + ajoutLait)
      println("Sucre: " + ajoutSucre)

       println("Niveaux de stock mis à jour. \nRetour au menu principal...")
       }
        if (modechoisi == 3) {
        continuer = false
          }
      }
    }
  }
