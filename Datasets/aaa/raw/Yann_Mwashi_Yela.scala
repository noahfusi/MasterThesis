import io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //stock initiaux ci-dessous
    var gdepoudredecafe = 50
    var gdesucre = 30
    var mldelait = 0.5
    val quitter = " Retour au menu principal..."
    val exit = "Votre Expresso est prêt ! Bonne dégustation !"
    val out = "Votre Capuccino est prêt ! Bonne dégustation !"
    val dehors = "Votre Latte est prêt ! Bonne dégustation !"
    val caract = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var twint = ""
    for (i <- 1 to 5) {
      val chxcaract = (Math.random() * 36).toInt
      twint += caract(chxcaract)
    }
    var modechoisi = 0
    while(quitter == " Retour au menu principal..." &&
      exit == "Votre Expresso est prêt ! Bonne dégustation !" &&
      out == "Votre Capuccino est prêt ! Bonne dégustation !" &&
      dehors == "Votre Latte est prêt ! Bonne dégustation !" && modechoisi != 3 ) {
      modechoisi = 0
      val caract = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var twint = ""
      for (i <- 1 to 5) {
        val chxcaract = (Math.random() * 36).toInt
        twint += caract(chxcaract)
      }
      while (modechoisi != 1 && modechoisi != 2 && modechoisi != 3) {
        println("          \nNospresso Café")
        println(" Veuillez sélectionner votre mode :")
        println(" 1) Client")
        println(" 2) Admin")
        println(" 3) Quitter")
        modechoisi = readLine(" >").toInt
      }
      var insuffisance1 = " "
      var insuffisance2 = " "
      var insuffisance3 = " "
      do {
        //Quand une valeur est choisie, lancer un menu
        val caract = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        var twint = ""
        for (i <- 1 to 5) {
          val chxcaract = (Math.random() * 36).toInt
          twint += caract(chxcaract)
        }
        var boisson = 0
        var dimensionlatte = 0
        var sucre = 0
        var dosedelait = 0
        var nbrededosesdelait = 0
        var prixtotal = 0.00
        val prixsucre1 = 0.00
        val prixsucre2 = 0.10
        val prixsucre3 = 0.20
        val prixsucre4 = 0.30
        val prixdosedelait1 = 0.05
        val prixdosedelait2 = 0.10
        val prixdosedelait3 = 0.15
        val prixexpresso = 2.00
        val prixcapuccino = 2.50
        val prixlatte1 = 2.70
        val prixlatte2 = 3.20
        val prixlatte3 = 3.70
        if (modechoisi == 1) {
          while (boisson != 1 && boisson != 3 && boisson != 2) {
            println("Veuillez sélectionner votre boisson :")
            println(" 1) Expresso - CHF 2.00")
            println(" 2) Capuccino - CHF 2.50")
            println(" 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            boisson = readLine(" >").toInt
          }

          //1 pour Expresso, veuillez le personnaliser juste en bas
          if (boisson == 1) {
            //8g de poudre de café
            while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
              println("Souhaitez-vous ajouter du sucre ?")
              println(" 1) Sans sucre")
              println(" 2) Peu (5g) - CHF 0.10")
              println(" 3) Moyen (10g) - CHF 0.20")
              println(" 4) Beaucoup (15g) - CHF 0.30")
              sucre = readLine(" >").toInt
            }
            if ((gdesucre < 5 && sucre == 2) || (gdesucre < 10 && sucre == 3) || (gdesucre < 15 && sucre == 4)) {
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              insuffisance1 = "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. "
              println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance1)
            }
            else if (gdepoudredecafe < 8) {
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              insuffisance1 = "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. "
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance1)
            }
            else {
              if (sucre == 1) {
                prixtotal = prixexpresso + prixsucre1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 0
              }
              if (sucre == 2) {
                prixtotal = prixexpresso + prixsucre2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 5
              }
              if (sucre == 3) {
                prixtotal = prixexpresso + prixsucre3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 10
              }
              if (sucre == 4) {
                prixtotal = prixexpresso + prixsucre4
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 15
              }
              println(" Boisson sélectionnée : Expresso")
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixexpresso, prixsucre1, prixtotal)
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixexpresso, prixsucre2, prixtotal)
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixexpresso, prixsucre3, prixtotal)
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixexpresso, prixsucre4, prixtotal)
              }
              println(" \nVeuillez payer en utilisant Twint")
              println(" Votre code de paiement est :" + twint)
              println(" En attente de validation du paiement ...")
              Thread.sleep(3000)
              println("\nPaiement confirmé.")
              println("Préparation de votre boisson ...")
              Thread.sleep(3000)
              insuffisance1 = " "
              insuffisance2 = " "
              insuffisance3 = " "
              println(exit)
            }


          }
          if (boisson == 2) { //6g de poudre de café et 100ml de lait
            while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
              println("Souhaitez-vous ajouter du sucre ?")
              println(" 1) Sans sucre")
              println(" 2) Peu (5g) - CHF 0.10")
              println(" 3) Moyen (10g) - CHF 0.20")
              println(" 4) Beaucoup (15g) - CHF 0.30")
              sucre = readLine(" >").toInt
            }
            if (sucre == 1) {
              prixtotal = prixcapuccino + prixsucre1
            }
            if (sucre == 2) {
              prixtotal = prixcapuccino + prixsucre2
            }
            if (sucre == 3) {
              prixtotal = prixcapuccino + prixsucre3
            }
            if (sucre == 4) {
              prixtotal = prixcapuccino + prixsucre4
            }
            while (dosedelait != 1 && dosedelait != 2) {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println(" 1) Oui")
              println(" 2) Non")
              dosedelait = readLine(" >").toInt
            }
            if (dosedelait == 1) {
              while (nbrededosesdelait != 1 && nbrededosesdelait != 2 && nbrededosesdelait != 3) {
                println("Combien de doses ?")
                nbrededosesdelait = readLine(" >").toInt
              }
            } //1 dose de lait contient 50 ml

            if ((gdesucre < 0 && sucre == 2) || (gdesucre < 10 && sucre == 3) || (gdesucre < 15 && sucre == 4)) {
              println(" Boisson sélectionnée : Capuccino")
              if (sucre == 1 && dosedelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              insuffisance2 = " Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. "
              println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance2)
            }
            else if (gdepoudredecafe < 6) {
              println(" Boisson sélectionnée : Capuccino")
              if (sucre == 1 && dosedelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              insuffisance2 = " Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. "
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance2)
            }
            else if ((mldelait < 0.1) || (nbrededosesdelait == 1 && mldelait < 0.15) || (nbrededosesdelait == 2 && mldelait < 0.2) || (nbrededosesdelait == 3 && mldelait < 0.25)) {
              println(" Boisson sélectionnée : Capuccino")
              if (sucre == 1 && dosedelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Non")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
              }
              insuffisance2 = " Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. "
              println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance2)
            }
            else {
              if (dosedelait == 2 && sucre == 1) {
                prixtotal = prixcapuccino + prixsucre1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.1
              }
              if (dosedelait == 2 && sucre == 2) {
                prixtotal = prixcapuccino + prixsucre2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.1
              }
              if (dosedelait == 2 && sucre == 3) {
                prixtotal = prixcapuccino + prixsucre3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.1
              }
              if (dosedelait == 2 && sucre == 4) {
                prixtotal = prixcapuccino + prixsucre4
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.1
              }
              if (nbrededosesdelait == 1 && sucre == 1) {
                prixtotal = prixcapuccino + prixsucre1 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.1 - 0.05
              }
              if (nbrededosesdelait == 1 && sucre == 2) {
                prixtotal = prixcapuccino + prixsucre2 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.1 - 0.05
              }
              if (nbrededosesdelait == 1 && sucre == 3) {
                prixtotal = prixcapuccino + prixsucre3 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.1 - 0.05
              }
              if (nbrededosesdelait == 1 && sucre == 4) {
                prixtotal = prixcapuccino + prixsucre4 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.1 - 0.05
              }
              if (nbrededosesdelait == 2 && sucre == 1) {
                prixtotal = prixcapuccino + prixsucre1 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.1 - 0.1
              }
              if (nbrededosesdelait == 2 && sucre == 2) {
                prixtotal = prixcapuccino + prixsucre2 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.1 - 0.1
              }
              if (nbrededosesdelait == 2 && sucre == 3) {
                prixtotal = prixcapuccino + prixsucre3 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.1 - 0.1
              }
              if (nbrededosesdelait == 2 && sucre == 4) {
                prixtotal = prixcapuccino + prixsucre4 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.1 - 0.1
              }
              if (nbrededosesdelait == 3 && sucre == 1) {
                prixtotal = prixcapuccino + prixsucre1 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.1 - 0.15
              }
              if (nbrededosesdelait == 3 && sucre == 2) {
                prixtotal = prixcapuccino + prixsucre2 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.1 - 0.15
              }
              if (nbrededosesdelait == 3 && sucre == 3) {
                prixtotal = prixcapuccino + prixsucre3 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.1 - 0.15
              }
              if (nbrededosesdelait == 3 && sucre == 4) {
                prixtotal = prixcapuccino + prixsucre4 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.1 - 0.15
              }
              println(" Boisson sélectionnée : Capuccino")
              if (sucre == 1 && dosedelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Non")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcapuccino, prixsucre1, prixtotal)
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcapuccino, prixsucre1, prixdosedelait1, prixtotal)
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcapuccino, prixsucre1, prixdosedelait2, prixtotal)
              }
              if (sucre == 1 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Sans sucre")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f  + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre1, prixdosedelait3, prixtotal)
              }
              if (sucre == 2 && dosedelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Non")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre2, prixtotal)
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre2, prixdosedelait1, prixtotal)
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre2, prixdosedelait2, prixtotal)
              }
              if (sucre == 2 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Peu (5g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre2, prixdosedelait3, prixtotal)
              }
              if (sucre == 3 && dosedelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Non")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre3, prixtotal)
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre3, prixdosedelait1, prixtotal)
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre3, prixdosedelait2, prixtotal)
              }
              if (sucre == 3 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Moyen (10g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre3, prixdosedelait3, prixtotal)
              }
              if (sucre == 4 && dosedelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Non")
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre4, prixtotal)
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 1) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre4, prixdosedelait1, prixtotal)
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 2) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre4, prixdosedelait2, prixtotal)
              }
              if (sucre == 4 && dosedelait == 1 && nbrededosesdelait == 3) {
                println(" Niveau de sucre : Beaucoup (15g)")
                println("Lait supplémentaire : Oui")
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixcapuccino, prixsucre4, prixdosedelait3, prixtotal)
              }

              println(" \nVeuillez payer en utilisant Twint")
              println(" Votre code de paiement est :" + twint)
              println(" En attente de validation du paiement ...")
              Thread.sleep(3000)
              println("\nPaiement confirmé.")
              println("Préparation de votre boisson ...")
              Thread.sleep(3000)
              insuffisance1 = " "
              insuffisance2 = " "
              insuffisance3 = " "
              println(out)
            }

          }
          if (boisson == 3) {
            while (dimensionlatte != 1 && dimensionlatte != 2 && dimensionlatte != 3) {
              println(" Dimension du Latte")
              println(" 1) Petit - CHF 2.70") //6g de poudre de café et 120ml de lait
              println(" 2) Moyen - CHF 3.20") //8g de poudre de café et 150ml de lait
              println(" 3) Grand - CHF 3.70") //12g de poudre de café et 200ml de lait
              dimensionlatte = readLine(" >").toInt
            }

            while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
              println("Souhaitez-vous ajouter du sucre ?")
              println(" 1) Sans sucre")
              println(" 2) Peu (5g) - CHF 0.10")
              println(" 3) Moyen (10g) - CHF 0.20")
              println(" 4) Beaucoup (15g) - CHF 0.30")
              sucre = readLine(" >").toInt
            }
            while (dosedelait != 1 && dosedelait != 2) {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println(" 1) Oui")
              println(" 2) Non")
              dosedelait = readLine(" >").toInt
            }
            if (dosedelait == 1) {
              while (nbrededosesdelait != 1 && nbrededosesdelait != 2 && nbrededosesdelait != 3) {
                println("Combien de doses ?")
                nbrededosesdelait = readLine(" >").toInt
              }
            } //1 dose de lait contient 50 ml
            if ((gdesucre < 5 && sucre == 2) || (gdesucre < 10 && sucre == 3) || (gdesucre < 15 && sucre == 4)) {
              if (dimensionlatte == 1) {
                println(" Boisson sélectionnée : Latte (Petit)")
              }
              if (dimensionlatte == 2) {
                println(" Boisson sélectionnée : Latte (Moyen)")
              }
              if (dimensionlatte == 3) {
                println(" Boisson sélectionnée : Latte (Grand)")
              }
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              if (dosedelait == 1) {
                println("Lait supplémentaire : Oui")
              }
              if (dosedelait == 2) {
                println("Lait supplémentaire : Non")
              }
              insuffisance3 = " Veuillez choisir une taille plus petite ou essayer une autre boisson. "
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance3)
            }
            else if ((gdepoudredecafe < 6 && dimensionlatte == 1) || (gdepoudredecafe < 8 && dimensionlatte == 2) || (gdepoudredecafe < 12 && dimensionlatte == 3)) {
              if (dimensionlatte == 1) {
                println(" Boisson sélectionnée : Latte (Petit)")
              }
              if (dimensionlatte == 2) {
                println(" Boisson sélectionnée : Latte (Moyen)")
              }
              if (dimensionlatte == 3) {
                println(" Boisson sélectionnée : Latte (Grand)")
              }
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              if (dosedelait == 1) {
                println("Lait supplémentaire : Oui")
              }
              if (dosedelait == 2) {
                println("Lait supplémentaire : Non")
              }
              insuffisance3 = " Veuillez choisir une taille plus petite ou essayer une autre boisson. "
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance3)
            }
            else if ((mldelait < 0.12 && dimensionlatte == 1) || (mldelait < 0.15 && dimensionlatte == 2) || (mldelait < 0.2 && dimensionlatte == 3) ||
              (dimensionlatte == 1 && nbrededosesdelait == 1 && mldelait < 0.17) || (dimensionlatte == 1 && nbrededosesdelait == 2 && mldelait < 0.22) || (dimensionlatte == 1 && nbrededosesdelait == 3 && mldelait < 0.27) ||
              (dimensionlatte == 2 && nbrededosesdelait == 1 && mldelait < 0.2) || (dimensionlatte == 2 && nbrededosesdelait == 2 && mldelait < 0.25) || (dimensionlatte == 2 && nbrededosesdelait == 3 && mldelait < 0.3) ||
              (dimensionlatte == 3 && nbrededosesdelait == 1 && mldelait < 0.25) || (dimensionlatte == 3 && nbrededosesdelait == 2 && mldelait < 0.3) || (dimensionlatte == 3 && nbrededosesdelait == 3 && mldelait < 0.35)) {
              if (dimensionlatte == 1) {
                println(" Boisson sélectionnée : Latte (Petit)")
              }
              if (dimensionlatte == 2) {
                println(" Boisson sélectionnée : Latte (Moyen)")
              }
              if (dimensionlatte == 3) {
                println(" Boisson sélectionnée : Latte (Grand)")
              }
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              if (dosedelait == 1) {
                println("Lait supplémentaire : Oui")
              }
              if (dosedelait == 2) {
                println("Lait supplémentaire : Non")
              }
              insuffisance3 = " Veuillez choisir une taille plus petite ou essayer une autre boisson. "
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println(insuffisance3)
            }
            else {
              if (dimensionlatte == 1) {
                println(" Boisson sélectionnée : Latte (Petit)")
              }
              if (dimensionlatte == 2) {
                println(" Boisson sélectionnée : Latte (Moyen)")
              }
              if (dimensionlatte == 3) {
                println(" Boisson sélectionnée : Latte (Grand)")
              }
              if (sucre == 1) {
                println(" Niveau de sucre : Sans sucre")
              }
              if (sucre == 2) {
                println(" Niveau de sucre : Peu (5g)")
              }
              if (sucre == 3) {
                println(" Niveau de sucre : Moyen (10g)")
              }
              if (sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g)")
              }
              if (dosedelait == 1) {
                println("Lait supplémentaire : Oui")
              }
              if (dosedelait == 2) {
                println("Lait supplémentaire : Non")
              }
              if (sucre == 1 && dimensionlatte == 1 && dosedelait == 2) {
                prixtotal = prixlatte1 + prixsucre1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.12
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre1, prixlatte1, prixtotal)
              }
              if (sucre == 1 && dimensionlatte == 2 && dosedelait == 2) {
                prixtotal = prixlatte2 + prixsucre1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre1, prixlatte2, prixtotal)
              }
              if (sucre == 1 && dimensionlatte == 3 && dosedelait == 2) {
                prixtotal = prixlatte3 + prixsucre1
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.2
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre1, prixlatte3, prixtotal)
              }
              if (sucre == 2 && dimensionlatte == 1 && dosedelait == 2) {
                prixtotal = prixlatte1 + prixsucre2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.12
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre2, prixlatte1, prixtotal)
              }
              if (sucre == 2 && dimensionlatte == 2 && dosedelait == 2) {
                prixtotal = prixlatte2 + prixsucre2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre2, prixlatte2, prixtotal)
              }
              if (sucre == 2 && dimensionlatte == 3 && dosedelait == 2) {
                prixtotal = prixlatte3 + prixsucre2
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.2
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre2, prixlatte3, prixtotal)
              }
              if (sucre == 3 && dimensionlatte == 1 && dosedelait == 2) {
                prixtotal = prixlatte1 + prixsucre3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.12
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre3, prixlatte1, prixtotal)
              }
              if (sucre == 3 && dimensionlatte == 2 && dosedelait == 2) {
                prixtotal = prixlatte2 + prixsucre3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre3, prixlatte2, prixtotal)
              }
              if (sucre == 3 && dimensionlatte == 3 && dosedelait == 2) {
                prixtotal = prixlatte3 + prixsucre3
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.2
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre3, prixlatte3, prixtotal)
              }
              if (sucre == 4 && dimensionlatte == 1 && dosedelait == 2) {
                prixtotal = prixlatte1 + prixsucre4
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.12
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre4, prixlatte1, prixtotal)
              }
              if (sucre == 4 && dimensionlatte == 2 && dosedelait == 2) {
                prixtotal = prixlatte2 + prixsucre4
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre4, prixlatte2, prixtotal)
              }
              if (sucre == 4 && dimensionlatte == 3 && dosedelait == 2) {
                prixtotal = prixlatte3 + prixsucre4
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.2
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixsucre4, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 1 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre1 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.12 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre1, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 1 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre1 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.15 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre1, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 1 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre1 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.2 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre1, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 2 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre2 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.12 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre2, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 2 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre2 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.15 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre2, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 2 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre2 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.2 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre2, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 3 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre3 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.12 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre3, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 3 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre3 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.15 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre3, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 3 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre3 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.2 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre3, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 4 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre4 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.12 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre4, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 4 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre4 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.15 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre4, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 1 && sucre == 4 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre4 + prixdosedelait1
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.2 - 0.05
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait1, prixsucre4, prixlatte3, prixtotal)
              }

              if (nbrededosesdelait == 2 && sucre == 1 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre1 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.12 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre1, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 1 && dimensionlatte == 2) {
                prixtotal = prixlatte3 + prixsucre1 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.15 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre1, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 1 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre1 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.2 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre1, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 2 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre2 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.12 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre2, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 2 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre2 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.15 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre2, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 2 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre2 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.2 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre2, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 3 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre3 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.12 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre3, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 3 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre3 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.15 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre3, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 3 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre3 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.2 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre3, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 4 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre4 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.12 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre4, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 4 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre4 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.15 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre4, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 2 && sucre == 4 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre4 + prixdosedelait2
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.2 - 0.1
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait2, prixsucre4, prixlatte3, prixtotal)
              }

              if (nbrededosesdelait == 3 && sucre == 1 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre1 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.12 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre1, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 1 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre1 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.15 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre1, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 1 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre1 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 0
                mldelait = mldelait - 0.2 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre1, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 2 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre2 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.12 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre2, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 2 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre2 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.15 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre2, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 2 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre2 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 5
                mldelait = mldelait - 0.2 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre2, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 3 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre3 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.12 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre3, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 3 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre3 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.15 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre3, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 3 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre3 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 10
                mldelait = mldelait - 0.2 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre3, prixlatte3, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 4 && dimensionlatte == 1) {
                prixtotal = prixlatte1 + prixsucre4 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 6
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.12 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre4, prixlatte1, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 4 && dimensionlatte == 2) {
                prixtotal = prixlatte2 + prixsucre4 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 8
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.15 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre4, prixlatte2, prixtotal)
              }
              if (nbrededosesdelait == 3 && sucre == 4 && dimensionlatte == 3) {
                prixtotal = prixlatte3 + prixsucre4 + prixdosedelait3
                gdepoudredecafe = gdepoudredecafe - 12
                gdesucre = gdesucre - 15
                mldelait = mldelait - 0.2 - 0.15
                printf(" Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixdosedelait3, prixsucre4, prixlatte3, prixtotal)
              }
              println(" \nVeuillez payer en utilisant Twint")
              println(" Votre code de paiement est :" + twint)
              println(" En attente de validation du paiement ...")
              Thread.sleep(3000)
              println("\nPaiement confirmé.")
              println("Préparation de votre boisson ...")
              Thread.sleep(3000)
              insuffisance1 = " "
              insuffisance2 = " "
              insuffisance3 = " "
              println(dehors)
            }


          }
        }
      }while(insuffisance1 == "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. " || insuffisance2 == " Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. " || insuffisance3 == " Veuillez choisir une taille plus petite ou essayer une autre boisson. ")


      if (modechoisi == 2) {
        println(" Mode Admin")
        var codeadmin = 0
        while(codeadmin != 434343){
          codeadmin = readLine(" Entrez le code PIN :").toInt
          if(codeadmin != 434343) println(" Accès non autorisé.")}
        println(" Accès autorisé.")
        println(" Stocks:")
        println(" Poudre de café:" + gdepoudredecafe + "g")
        printf(" Lait: %.2f l",mldelait)
        println(" ")
        println(" Sucre:" + gdesucre + "g")
        println(" Réapprovisionnement des stocks...")
        println(" Ajout :")
        var ajoutcafe = readLine("Poudre de café : ").toInt
        var ajoutlait = readLine("Lait : ").toDouble
        var ajoutsucre = readLine("Sucre : ").toInt
        gdepoudredecafe = gdepoudredecafe + ajoutcafe
        gdesucre = gdesucre + ajoutsucre
        mldelait = mldelait + ajoutlait
        println(" Niveaux de stock mis à jour.")
        println(quitter)
      }
      if(modechoisi == 3){
        modechoisi = 3
      }
    }
  }
}