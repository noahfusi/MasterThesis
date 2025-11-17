import scala.io.StdIn.readInt
import scala.io.StdIn.readDouble
import scala.util.Random
object Main {
  // SI = stocks initiaux
  var SIpoudrecafe = 50.0 // grammes
  var SIsucre = 30.0 // grammes
  var SIlait = 500.0 // ml
  def main(args: Array[String]): Unit = {
    // mode à selectionner
    var mode = 0.0
    // définir type de boisson
    var boisson = 0.0
    // quantité sucre
    var nbsucre = 0.0
    // prix sucre
    var prixsucre = 0.0
    // prix lait
    var prixdoselait = 0.0

    // prix boisson
    var prixtaillelatte = 0.0
    var prixexpresso = 2.00
    var prixcappuccino = 2.50
    var taillelatte  = 0

    var poudrecafe = 0.0
    var quantitesucre = 0.0
    var lait = 0.0
    var doselait = 0.0
    var supplementlait = 0.0
    var quantitelait = 0.0
    var prixfinal = 0.0

    // Paiement Twint
    var code = ""
    val nbcaractere = 5
    val valeuralphanumeric = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    // Admin
    var codePIN = 0.0
    var remplissagecafe = 0.0
    var remplissagesucre = 0.0
    var remplissagelait = 0.0

     println ("Nospresso Café")
    println(" Veuillez sélectionner votre mode:")
    println("1)Client")
    println("2)Admin")
    println("3)Quitter")
    println (">")
     mode = readDouble()
    while ((mode != 1) && (mode != 2) && (mode != 3)) {
      println(" sélection incorrect, choissisez une autre valeur")
      println(">")
      mode = readDouble()
    }
     while ( mode != 3) {
      if (mode == 1) {
        doselait = 0
        quantitesucre = 0
        nbsucre = 0
        quantitelait = 0
        print("Vous avez sélectionner le mode client.")
        println(" Quelle boisson désirez-vous ?")
        println(" 1.1) Expresso - CHF 2.00")
        println(" 1.2) Cappuccino - CHF 2.50")
        println(" 1.3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand ")
        println(">")
        boisson = readDouble()
        while ((boisson != 1.1) && (boisson != 1.2) && (boisson != 1.3)) {
          println(" La valeur sélectionnée est incorrect, réssayer")
          println(">")
          boisson = readDouble()
        }
        // expresso
        if (boisson == 1.1) {
          poudrecafe = 8.0 // grammes
          quantitesucre = 0.0
          quantitelait = 0.0
          prixexpresso = 2.0 //CHF
            println("Quelle quantité de sucre desirez-vous?")
            println(" 1) un peu; 5g - 0.10CHF")
            println(" 2) moyen; 10g - 0.20CHF")
            println(" 3) beaucoup; 15g - 0.30CHF")
            println (" 4) pas de sucre ")
          println (">")
            quantitesucre = readDouble()
            while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              quantitesucre = readDouble()
            }
            if (quantitesucre == 1) {
              prixsucre = 0.10
              nbsucre = 5.0 // grammes
            }
            else if (quantitesucre == 2) {
              prixsucre = 0.20
              nbsucre = 10.0 // grammes
            }
            else if (quantitesucre == 3) {
              prixsucre = 0.30
              nbsucre = 15.0 // grammes
            }
            else if (quantitesucre == 4) {
              prixsucre = 0.0
            }
        }
        // cappuccino
        if (boisson == 1.2) {
          poudrecafe = 6 // grammes
          lait = 100.0 // ml
          nbsucre = 0.0
          doselait = 0.0
          prixcappuccino = 2.50 // CHF

            println(" Quelle quantité desirez-vous?")
            println("1) un peu 5g- 0.10CHF")
            println("2) moyen 10g - 0.20CHF")
            println("3) beaucoup 15g - 0.30CHF")
            println ("4) pas de sucre")
          println(">")
            quantitesucre = readDouble()
            while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              quantitesucre = readDouble()
            }
            if (quantitesucre == 1) {
              prixsucre = 0.10 // CHF
              nbsucre = 5.0 //grammes
            }
            else if (quantitesucre == 2) {
              prixsucre = 0.20 // CHF
              nbsucre = 10.0 // grammes
            }
            else if (quantitesucre == 3) {
              prixsucre = 0.30 // CHF
              nbsucre = 15.0 // grammes
            }
            else if (quantitesucre == 4){
              prixsucre = 0.0 // CHF
            }
          println(" Voulez vous un supplément lait ?")
          println("1) oui")
          println("2) non")
          println(">")
          supplementlait = readDouble()
          while ((supplementlait != 1) && (supplementlait != 2)) {
            println(" La valeur sélectionnée est incorrect, réssayer")
            println(">")
            supplementlait = readDouble()
          }
          if (supplementlait == 1) {
            println(" Combien de dose? (maximum 3)")
            println("1)une dose, 50ml- 0.05CHF")
            println("2) deux dose 100ml- 0.10 CHF")
            println("3) trois doses 150ml- 0.15 CHF")
            println(">")
            doselait = readDouble()
            while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              doselait = readDouble()
            }
            if (doselait == 1) {
              doselait = 50
              prixdoselait = 0.05
              quantitelait = doselait + lait
            }
            else if (doselait == 2) {
              doselait = 50 * 2
              prixdoselait = 0.1
              quantitelait = doselait + lait
            }
            else if (doselait == 3) {
              doselait = 50 * 3
              prixdoselait = 0.15
              quantitelait = doselait + lait
            }
          } else {
            prixdoselait = 0.0
            quantitelait = lait
          }
        }
        // latte
        if (boisson == 1.3) {
          println("Quelle taille desirez-vous ?")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          println(">")
          taillelatte = readInt()

          while ((taillelatte != 1) && (taillelatte != 2) && (taillelatte != 3)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            taillelatte = readInt()
          }
          if (taillelatte == 1) {
            poudrecafe = 6.0 //g
            lait = 120.0 //ml
            prixtaillelatte = 2.70 // CHF

          } else if (taillelatte == 2) {
            poudrecafe = 8 // grammes
            lait = 150.0 // ml
            prixtaillelatte = 3.20 // CHF
          }
          else if (taillelatte == 3) {
            poudrecafe = 12 //grammes
            lait = 200.0 //ml
            prixtaillelatte = 3.70 // CHF
          }
          quantitesucre = 0.0
          doselait = 0.0

            println(" Quelle quantité de sucre desirez-vous? ")
            println("1) un peu 5g- 0.10CHF")
            println("2) moyen 10g - 0.20CHF")
            println("3) beaucoup 15g - 0.30CHF")
            println ("4) pas de sucre")
          println(">")
            quantitesucre = readDouble()
            while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre !=4)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              quantitesucre = readDouble()
            }
            if (quantitesucre == 1) {
              prixsucre = 0.10
              nbsucre = 5.0
            }
            else if (quantitesucre == 2) {
              prixsucre = 0.20
              nbsucre = 10.0
            }
            else if (quantitesucre == 3) {
              prixsucre = 0.30
              nbsucre = 15.0
            }
            else if (quantitesucre == 4) {
              prixsucre = 0.0
            }
          println(" Voulez vous un supplément lait ?")
          println("1) oui ")
          println("2) non ")
          println(">")
          supplementlait = readDouble()
          while ((supplementlait != 1) && (supplementlait != 2)) {
            println(" la valeur sélectionnée est incorrect, réssayer")
            println(">")
            supplementlait = readDouble()
          }
          if (supplementlait == 1) {
            println(" Combien de dose? (maximum 3))")
            println("1) une dose (50ml)")
            println("2) deux dose (100ml)")
            println("3) trois doses (150ml)")
            println(">")
            doselait = readDouble()
            while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
              println(" la valeur sélectionnée est incorrect, réssayer")
              println(">")
              doselait = readDouble()
            }
            if (doselait == 1) {
              doselait = 50 //grammes
              prixdoselait = 0.05 // CHF
              quantitelait = doselait + lait
            }
            else if (doselait == 2) {
              doselait = 50 * 2 //grammes
              prixdoselait = 0.1 //CHF
              quantitelait = doselait + lait
            }
            else if (doselait == 3) {
              doselait = 50 * 3 //grammes
              prixdoselait = 0.15 // CHF
              quantitelait = doselait + lait
            }
          } else {
            prixdoselait = 0.0
          }
        }
        println()
        if (boisson ==1.1) println("Votre boisson: Expresso")
        if (boisson ==1.2) println("Votre boisson: Cappuccino")
        if (boisson ==1.3) println("Votre boisson: Latte")
        if (quantitesucre == 1) println("Niveau sucre: Un peu (5g)")
        if (quantitesucre == 2) println("Niveau sucre: Moyen (10g)")
        if (quantitesucre == 3) println("Niveau de sucre: Beaucoup (15g)")
        if (quantitesucre == 4) println("Niveau de sucre: Aucun")
        if ((boisson == 1.2) && (supplementlait == 1)) {
          println("Supplément lait: Oui")
        }
        if ((boisson == 1.2) && (supplementlait == 2)) {
           println("Supplément lait: Non")
        }
        if (( boisson == 1.3) && (supplementlait == 1)) {
           println("Supplément lait: Oui")
        }
        if ((boisson == 1.3 ) && (supplementlait == 2)) {
            println("Supplément lait: Non")
        }
        if (boisson ==1.1) {
          prixfinal = prixexpresso + prixsucre
          printf("Le prix de votre expresso est de %.2f + %.2f = %.2f CHF\n", prixexpresso, prixsucre, prixfinal)
        }
        if (boisson ==1.2) {
          prixfinal = prixcappuccino + prixsucre + prixdoselait
          printf("Le prix de votre cappuccino est de %.2f + %.2f + %.2f = %.2f CHF\n", prixcappuccino, prixsucre, prixdoselait, prixfinal)
        }
        if (boisson ==1.3) {
          prixfinal = prixtaillelatte + prixsucre + prixdoselait
          printf("Le prix de votre latte est de %.2f + %.2f  + %.2f = %.2f CHF\n", prixtaillelatte, prixsucre, prixdoselait, prixfinal)
        }

          if ((( boisson == 1.1) && (SIpoudrecafe >= poudrecafe) && (SIsucre >= nbsucre)) || (( boisson == 1.2) && (SIpoudrecafe >= poudrecafe) && (SIsucre >= nbsucre) && (SIlait >= quantitelait)) || (( boisson == 1.3) && (SIpoudrecafe >= poudrecafe) && (SIsucre >= nbsucre) && (SIlait >= quantitelait)))
              {
            println("Le stock est suffisant, veuillez payer en utilisant Twint")
            // paiement twint
            code = ""
              for( i <- 1 to nbcaractere) {
                var caracterealeatoire = valeuralphanumeric(Random.nextInt(valeuralphanumeric.length))
                code += caracterealeatoire
              }
               if ( (boisson == 1.1) || (boisson == 1.2) || (boisson == 1.3)) {
                 println(" Votre code de paiement est :" + code)
            println("En attente de validation du paiement...")
            Thread.sleep(3000)
            println("Paiement accepté, merci")
            println("Préparation de votre boisson...")
            if (boisson == 1.1) {
              println("Votre expresso est prêt! Bonne dégustation !")
            }
            if (boisson == 1.2) {
              println("Votre capppuccino est prêt! Bonne dégustation !")
            }
            if (boisson == 1.3) {
              println("Votre latte est prêt! Bonne dégustation! ")
            }
            // déduction stocks
            SIpoudrecafe -= poudrecafe
            SIsucre -= nbsucre
            SIlait -= quantitelait
          }
              }
          else {
            if (poudrecafe > SIpoudrecafe) {
              println("Erreur: Stock de café insuffisant")
              println("Veuillez choisir une autre boisson ou une boisson plus petite")
            }
            if (nbsucre > SIsucre) {
              println("Erreur: Stock de sucre insuffisant")
              println("Veuillez choisir une autre boisson ou moins de sucre")
            }
            if (quantitelait > SIlait) {
              println("Erreur: Stock de lait insuffisant")
              println("Veuillez choisir une autre boisson ou une boisson plus petite ou moins de lait")
            }
          }
          }
      // fin du mode client
      if (mode == 2) {
        println(" Vous avez choisi le mode admin.")
        println(" Entrez le code PIN: ******")
        codePIN = readDouble()
        while (codePIN != 434343) {
          println(" Le code incorrect, essayer à nouveau")
          codePIN = readDouble()
        }
        if (codePIN == 434343) {
          println("Accès autorisé.")
          println("Stocks :")
          println("Poudre de café:" + SIpoudrecafe + "g")
          println("Lait" + SIlait + "ml")
          println("Sucre:" + SIsucre + "g")
          println(" Réapprovisionnement des stocks...")
          println (" Ajout : ")
          println (" Poudre à café :")
          remplissagecafe = readInt()
          poudrecafe += remplissagecafe
          println (" Lait :")
          remplissagelait = readDouble()
          quantitelait += remplissagelait
          println (" Sucre :")
          remplissagesucre = readInt()
          nbsucre += remplissagesucre
          println("Niveau de stock mis à jour")

          println(" Veuillez sélectionner votre mode:")
          println("1)Client")
          println("2)Admin")
          println("3)Quitter")
          println (">")
          mode = readDouble()
          while ((mode != 1) && (mode != 2) && (mode != 3)) {
            println(" sélection incorrect, choissisez une autre valeur")
            println(">")
            mode = readDouble()
          }
        }
      }
      if (mode == 3) {
        println(" Vous avez choisi le mode quitter le programme ")

      }
    }
  }
}







