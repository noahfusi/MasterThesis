import io.StdIn._
import scala.util.Random




//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Main {
  def main(args: Array[String]): Unit = {

    // Pour savoir quand on éteint le programme
    var onoff = true

    var code = 0

    // Les stocks de base
    var stock_cafe = 50
    var stock_sucre = 30
    var stock_lait = 0.50

    // Les quantités à soustraire par boisson et type d'ingrédient
    val achat_expresso_cafe = 8
    val achat_cappuccino_cafe = 6
    val achat_latte_petit_cafe = 6
    val achat_latte_moyen_cafe = 8
    val achat_latte_grand_cafe = 12

    val achat_cappuccino_lait = 0.100
    val achat_latte_petit_lait = 0.120
    val achat_latte_moyen_lait = 0.150
    val achat_latte_grand_lait = 0.200

    val sucre_peu = 5
    val sucre_moyen = 10
    val sucre_beaucoup = 15

    val dose_lait = 0.05


    // Les prix des boissons et suppléments
    val prix_expresso = 2.00
    val prix_cappuccino = 2.50
    val prix_latte_petit = 2.70
    val prix_latte_moyen = 3.20
    val prix_latte_grand = 3.70

    val prix_sucre_peu = 0.10
    val prix_sucre_moyen = 0.20
    val prix_sucre_beaucoup = 0.30

    val prix_dose_lait = 0.05



    var res_cafe = 0
    var res_sucre = 0
    var res_lait = 0.00



    // Pour afficher les bons prix
    var prix_boisson_simple = 0.00



    // Pour ajouter en mode admin
    var ajout_cafe = 0
    var ajout_lait = 0.00
    var ajout_sucre = 0




    // Pour gérer les modes et quand on termine le système
    while (onoff) {
      var mode = ""
      do {
        mode = readLine("\n        Nospresso Café \nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter\n")

      } while ((mode != "Client") && (mode != "Admin") && (mode != "Quitter"))



      // Les variables suivantes sont dans la boucle pour se réinitialiser
      var res_prix = 0.00
      var prix_lait_sup = 0.00
      var erreur = false   // Pour afficher le bon message d'erreur





      // Le mode client
      if (mode == "Client") {

        var boisson = ""
        var sucre = ""
        var lait = ""
        var nbr_de_doses = 0

        // Les queries à l'utilisateur
        do {
          boisson = readLine("\nVeuillez sélectionner votre boisson: \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50" +
            "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")

          if ((boisson != "Expresso") && (boisson != "Cappuccino") && (boisson != "Latte Petit") &&
            (boisson != "Latte Moyen") && (boisson != "Latte Grand")) {
            println("Veuillez choisir une boisson existante.")
          }
        } while ((boisson != "Expresso") && (boisson != "Cappuccino") && (boisson != "Latte Petit") &&
          (boisson != "Latte Moyen") && (boisson != "Latte Grand"))


        do {
          sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g)- CHF 0.10 \n3) Moyen (10g)- CHF 0.20 \n4) Beaucoup (15g)- CHF 0.30\n")

        } while ((sucre != "Sans sucre") && (sucre != "Peu") && (sucre != "Moyen") && (sucre != "Beaucoup"))


        if ((boisson == "Cappuccino") || (boisson == "Latte Petit") || (boisson == "Latte Moyen") || (boisson == "Latte Grand")) {
          do {
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui \n2) Non\n")
          } while ((lait != "Oui") && (lait != "Non"))
        }

        if (lait == "Oui") {
          do {
            nbr_de_doses = readLine("\nCombien de dose ?\n").toInt
          } while ((nbr_de_doses < 1) || (nbr_de_doses > 3))
        }

        // On fait les calculs des quantités pour l'achat de la boisson
        if (boisson == "Expresso") {
          res_cafe = stock_cafe - achat_expresso_cafe
          res_lait = stock_lait
          res_prix = res_prix + prix_expresso
          prix_boisson_simple = prix_expresso
          println("\nBoisson sélectionnée : Expresso")

        } else if (boisson == "Cappuccino") {
          res_cafe = stock_cafe - achat_cappuccino_cafe
          res_lait = stock_lait - achat_cappuccino_lait
          res_prix = res_prix + prix_cappuccino
          prix_boisson_simple = prix_cappuccino
          println("\nBoisson sélectionnée : Cappuccino")

        } else if (boisson == "Latte Petit") {
          res_cafe = stock_cafe - achat_latte_petit_cafe
          res_lait = stock_lait - achat_latte_petit_lait
          res_prix = res_prix + prix_latte_petit
          prix_boisson_simple = prix_latte_petit
          println("\nBoisson sélectionnée : Latte (Petit)")

        } else if (boisson == "Latte Moyen") {
          res_cafe = stock_cafe - achat_latte_moyen_cafe
          res_lait = stock_lait - achat_latte_moyen_lait
          res_prix = res_prix + prix_latte_moyen
          prix_boisson_simple = prix_latte_moyen
          println("\nBoisson sélectionnée : Latte (Moyen)")

        } else if (boisson == "Latte Grand") {
          res_cafe = stock_cafe - achat_latte_grand_cafe
          res_lait = stock_lait - achat_latte_grand_lait
          res_prix = res_prix + prix_latte_grand
          prix_boisson_simple = prix_latte_grand
          println("\nBoisson sélectionnée : Latte (Grand)")
        }

        // Les calculs du sucre
        if (sucre == "Peu") {
          res_sucre = stock_sucre - sucre_peu
          res_prix = res_prix + prix_sucre_peu
          println("Niveau de sucre : Peu (5g)")

        } else if (sucre == "Moyen") {
          res_sucre = stock_sucre - sucre_moyen
          res_prix = res_prix + prix_sucre_moyen
          println("Niveau de sucre : Moyen (10g)")

        } else if (sucre == "Beaucoup") {
          res_sucre = stock_sucre - sucre_beaucoup
          res_prix = res_prix + prix_sucre_beaucoup
          println("Niveau de sucre : Beaucoup (15g)")

        } else if (sucre == "Sans sucre") {
          res_sucre = stock_sucre // Pour garder le même niveau de sucre
          println("Niveau de sucre : Sans sucre")
        }

        // Les calculs du lait
        if (lait == "Oui") {
          res_lait = res_lait - (nbr_de_doses * dose_lait)
          res_prix = res_prix + (nbr_de_doses * prix_dose_lait)
          prix_lait_sup = nbr_de_doses * prix_dose_lait
          println("Lait supplémentaire: Oui\nNombres de doses: " + nbr_de_doses)

        } else if (lait == "Non") {
          println("Lait supplémentaire: Non")
        }





        // Les messages d'erreurs personnalisés en fonction du manque de café
        if (((boisson == "Cappuccino") || (boisson == "Latte Petit")) && (res_cafe < 0)) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez vérifier les stocks en mode Admin.\n")
          erreur = true

        } else if ((boisson == "Expresso") && (res_cafe < 0)) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
          erreur = true

        } else if (((boisson == "Latte Moyen") || (boisson == "Latte Grand")) && (res_cafe < 0)) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
          erreur = true


          // Les messages d'erreurs personnalisés en fonction du manque de lait
        } else if ((boisson == "Cappuccino") && (res_lait < 0) && !erreur) {
          if (lait == "Oui") {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
            erreur = true
          } else {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.\n")
            erreur = true
          }

        } else if ((boisson == "Latte Petit") && (res_lait < 0) && !erreur) {
          if (lait == "Oui") {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
            erreur = true
          } else {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.\n")
            erreur = true
          }

        } else if (((boisson == "Latte Moyen") || (boisson == "Latte Grand")) && (res_lait < 0) && !erreur) {
          if (lait == "Oui") {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
            erreur = true
          } else {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
            erreur = true
          }



          // Les messages d'erreurs personnalisés en fonction du manque de sucre
        } else if ((res_sucre < 0) && !erreur) {
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de sucre ou ou vérifier les stocks en mode Admin.\n")
          erreur = true
        }




        // Pour afficher les prix
        if (!erreur) {
          if ((sucre == "Sans sucre") && ((lait == "Non") || (lait == ""))) {
            printf("Prix total : CHF %.2f", res_prix)

          } else if ((sucre == "Sans sucre") && (lait == "Oui")) {
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, res_prix)

          } else if ((sucre == "Peu") && ((lait == "Non") || (lait == ""))) {
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_peu, res_prix)

          } else if ((sucre == "Moyen") && ((lait == "Non") || (lait == ""))) {
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_moyen, res_prix)

          } else if ((sucre == "Beaucoup") && ((lait == "Non") || (lait == ""))) {
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_beaucoup, res_prix)

          } else if ((sucre == "Peu") && (lait == "Oui")) {
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_peu, res_prix)

          } else if ((sucre == "Moyen") && (lait == "Oui")) {
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_moyen, res_prix)

          } else if ((sucre == "Beaucoup") && (lait == "Oui")) {
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_beaucoup, res_prix)
          }





          // generer un code twint avec util.random : est ce qu'on a le droit ? + afficher le message

          println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + new String(Random.alphanumeric.take(5).toArray) + "\n(En attente de paiement...)\n")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.")
          println("Paiement confirmé.\nPréparation de votre boisson... \nVotre " + boisson + " est prêt ! Bonne dégustation !")




          // On mets à jour les stocks après la validation de la transaction
          stock_cafe = res_cafe
          stock_lait = res_lait
          stock_sucre = res_sucre

          // On remets le prix à 0
          res_prix = 0.00


        }


        // Le mode Admin
      } else if (mode == "Admin"){


        do {
          code = readLine("\nMode Admin\nEntrez le code PIN : ******\n").toInt

          if (code != 434343) println("Code PIN incorrect, veuillez taper un code à 6 chiffres.")

        } while (code != 434343)


        if (code == 434343){
          println("Stocks:\n    Poudre de café: " + stock_cafe + "g\n    Lait          : " + stock_lait + "L\n    Sucre         : " + stock_sucre + "g\n")

          ajout_cafe = readLine("Veuillez indiquer la quantité de poudre à café à rajouter en grammes:\n").toInt
          ajout_lait = readLine("Veuillez indiquer la quantité de lait à rajouter en litres:\n").toDouble
          ajout_sucre = readLine("Veuillez indiquer la quantité de sucre à rajouter en grammes:\n").toInt




          println("\nRéapprovisionnement des stocks...")
          println("Ajout :\n    Poudre de café: " + ajout_cafe + "g\n    Lait          : " + ajout_lait + "L\n    Sucre         : " + ajout_sucre + "g\n")

          // On mets à jour les stocks
          stock_cafe = stock_cafe + ajout_cafe
          stock_lait = stock_lait + ajout_lait
          stock_sucre = stock_sucre + ajout_sucre

          println("Niveaux de stock mis à jour.\nRetour au menu principal...")


        }
        // Le mode quitter
      } else if (mode =="Quitter"){
        onoff = false
      }
    }
  }
}
