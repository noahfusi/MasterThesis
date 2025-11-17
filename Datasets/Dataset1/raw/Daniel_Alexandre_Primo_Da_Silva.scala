import scala.io.StdIn._
import util.Random

object Main {
  def main(args: Array[String]): Unit = {

    println(" Nospresso Café")


    val modeclient = 1
    val modeadmin = 2
    val quitter = 3
    var choixsucre = 0
    var choixdumode = 0
    var Lait = 0
    var Dose = 0
    var SIcafe = 50
    var SIsucre = 30
    var SIlait = 500
    var consCafe = 0
    var consLait = 0
    var quantiteSucre = 0
    var prixboisson = 0.00
    var Prixfinal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    var sucreAjout = 0
    var cafeAjout = 0
    var laitAjout = 0


    //Séléction mode = quitter
    while (choixdumode != quitter) {

      println("Veuillez séléctionner votre mode :")
      println("1) Client ")
      println("2) Admin ")
      println("3) Quitter ")
      print("> ")

      choixdumode = readInt()

      // ERREUR SELECTION MODE
      while ((choixdumode != 1) && (choixdumode != 2) && (choixdumode != 3)) {
        println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
        println("Veuillez séléctionner votre mode :")
        println("1) Client ")
        println("2) Admin ")
        println("3) Quitter ")
        print("> ")
        choixdumode = readInt()
      }



      // Mode séléctionné = client
      if (choixdumode == modeclient) {
        println("Vous avez choisi le mode client.")


        // CHOIX DE LA BOISSON
        val Expresso = 1
        val Cappuccino = 2
        val Latte = 3
        println(" Veuillez séléctionner votre boisson :")
        println(" 1) Expresso (CHF 2.00) ")
        println(" 2) Cappuccino (CHF 2.50) ")
        println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
        print("> ")

        var choixboisson = readInt()


        // ERREUR CHOIX DE LA BOISSON
        while ((choixboisson != Expresso) && (choixboisson != Cappuccino) && (choixboisson != Latte)) {
          println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
          println(" Veuillez séléctionner votre boisson :")
          println(" 1) Expresso (CHF 2.00) ")
          println(" 2) Cappuccino (CHF 2.50) ")
          println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
          print("> ")
          choixboisson = readInt()
        }
        prixsucre = 0
        prixlait = 0

        // CHOIX BOISSON = EXPRESSO
        if (choixboisson == Expresso) {
          println("Vous avez séléctionné un Expresso. ")
          prixboisson = 2.00
          consCafe = 8

        }
        //CHOIX BOISSON = CAPPUCCINO
        if (choixboisson == Cappuccino) {
          println("Vous avez séléctionné un Cappuccino. ")
          prixboisson = 2.50
          consCafe = 6
          consLait = 100
        }
        // CHOIX BOISSON = LATTE
        if (choixboisson == Latte) {
          println("Vous avez séléctionné un Latte. ")
        }
        // CHOIX TAILLE LATTE
        if (choixboisson == Latte) {
          val LattePetit = 1
          val LatteMoyen = 2
          val LatteGrand = 3

          println("Quelle taille de Latte désirez-vous ?")
          println("1) Petit (CHF 2.70)")
          println("2) Moyen (CHF 3.20)")
          println("3) Grand (CHF 3.70)")
          print(">")

          var taille = readInt()

          // ERREUR CHOIX TAILLE LATTE
          while ((taille != 1) && (taille != 2) && (taille != 3)) {
            println("La valeur choisie est fausse, choisissez-en une nouvelle")
            println("Quelle taille de Latte désirez-vous ?")
            println("1) Petit (CHF 2.70)")
            println("2) Moyen (CHF 3.20)")
            println("3) Grand (CHF 3.70)")
            print(">")
            taille = readInt()
          }
          // LATTE TAILLE PETIT
          if (taille == LattePetit) {
            println("Vous avez choisi un Petit Latte (CHF 2.70). ")
            prixboisson = 2.70
            consCafe = 6
            consLait = 120
          }
          // LATTE TAILLE MOYEN
          if (taille == LatteMoyen) {
            println("Vous avez choisi un Latte Moyen (CHF 3.20). ")
            prixboisson = 3.20
            consCafe = 8
            consLait = 150
          }
          // LATTE TAILLE GRAND
          if (taille == LatteGrand) {
            println("Vous avez choisi un Grand Latte (CHF 3.70). ")
            prixboisson = 3.70
            consCafe = 12
            consLait = 200
          }
        }
        //CHOIX SUCRE
        val sucreOui = 1
        val sucreNon = 2

        println("Voulez-vous ajouter du sucre ?")
        println("1) oui")
        println("2) non")
        print(">")

        var choixsucre = readInt()

        //SI ERREUR CHOIX SUCRE
        while (!(choixsucre == sucreOui) && !(choixsucre == sucreNon)) {
          println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
          println("Voulez-vous ajouter du sucre ?")
          println("1) oui")
          println("2) non")
          print(">")
          choixsucre = readInt()
        }
        // SI AUCUN SUCRE
        if (choixsucre == sucreNon) {
          println("Vous ne souhaitez pas de sucre.")
          quantiteSucre = 0
        }
        val peudesucre = 1
        val moyensucre = 2
        val beaucoupsucre = 3

        // SI CHOIX SUCRE
        if (choixsucre == sucreOui) {
          println("Vous désirez du sucre en supplément. ")
          println("Quelle quantité de sucre désirez-vous ?")
          println("1) Peu (5g CHF 0.10)")
          println("2) Moyen (10g CHF 0.20)")
          println("3) Beaucoup (15g CHF 0.30).")
          print(">")

          choixsucre = readInt()

          // CHOIX SUCRE = PEU
          if (choixsucre == peudesucre) {
            println("Vous avez choisi peu de sucre. ")
            prixsucre = 0.10
            quantiteSucre = 5
          }
          // CHOIX SUCRE = MOYEN
          else if (choixsucre == moyensucre) {
            println("Vous avez choisi le niveau moyen de sucre. ")
            prixsucre = 0.20
            quantiteSucre = 10
          }
          // CHOIX SUCRE = BEAUCOUP
          else if (choixsucre == beaucoupsucre) {
            println("Vous avez choisi beaucoup de sucre. ")
            prixsucre = 0.30
            quantiteSucre = 15
          }
        }
        //SI ERREUR DOSE SUCRE
        while (!(choixsucre == peudesucre) && !(choixsucre == moyensucre) && !(choixsucre == beaucoupsucre)) {
          println("La valeur saisie est éronée, veuillez en choisir une adéquate .")
          println("Quelle quantité de sucre désirez-vous ?")
          println("1) Peu (5g CHF 0.10)")
          println("2) Moyen (10g CHF 0.20)")
          println("3) Beaucoup (15g CHF 0.30).")
          print(">")
          choixsucre = readInt()
        }

        //LAIT EN SUPPLEMENT (CAPPUCCINO ET LATTE)
        if ((choixboisson == Cappuccino) || (choixboisson == Latte)) {

          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print(">")

          var Lait = readInt()

          // SI ERREUR CHOIX LAIT
          while ((Lait != 1) && (Lait != 2)) {
            println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print(">")

            Lait = readInt()
          }

          // SI LAIT = NON
          if (Lait == 2) {
            println("Vous ne souhaitez pas de lait en supplément.")

          }
          // SI LAIT = OUI
          if (Lait == 1) {
            println("Vous souhaitez du lait en supplément.")
            println("Combien de doses de lait désirez-vous ?")
            println("1) 1 dose ")
            println("2) 2 doses")
            println("3) 3 doses")
            print(">")

            Dose = readInt()
          }
          var dose1 = 1
          var dose2 = 2
          var dose3 = 3

          if (Dose == 1) {
            println("Vous avez séléctionné 1 dose de lait.")
            prixlait = 0.05
          }
          if (Dose == 2) {
            println("Vous avez séléctionné 2 doses de lait.")
            prixlait = 0.10
          }
          if (Dose == 3) {
            println("Vous avez séléctionné 3 doses de lait.")
            prixlait = 0.15
          }
        }
        // PAIEMENT
        if ((SIsucre >= quantiteSucre) && (SIlait >= consLait) && (SIcafe >= consCafe)) {
          Prixfinal = prixboisson + prixsucre + prixlait
          printf ("Votre prix est %.2f + %.2f + %.2f = %.2f CHF ", prixboisson, prixsucre, prixlait, Prixfinal)
          println("Veuillez payer en utilisant TWINT.")
          var paiement = Random.alphanumeric.take(5).mkString
          println("Votre code de paiement est :" + paiement)
          println("(En attente de validation du paiement...)")
          Thread.sleep(5000)
          println(" Merci ! Votre paiement été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)

          if (choixboisson == Expresso) {
            println("Votre Expresso est prêt ! Bonne dégustation !")
          }
          if (choixboisson == Cappuccino) {
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          }
          if (choixboisson == Latte) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
        }
        SIcafe -= consCafe
        SIlait -= consLait
        SIsucre -= quantiteSucre

        // STOCKS INSUFFISANTS
        if ((SIsucre < quantiteSucre) || (SIlait < consLait) || (SIcafe < consCafe)) {
          println("Le stock est insuffisant. ")
          println("Veuillez choisir une autre boisson. ")
          println("Retour au menu principal... ")
        }
      }
      // Mode séléctionné = admin
      if (choixdumode == modeadmin) {
        println("Vous avez choisi le mode admin.")
        println("Veuillez entrer le code PIN.")
        print(">")
        var codepin = readInt()

        // PAS LE BON CODE PIN
        while (codepin != 434343) {
          println("Code PIN erroné. ")
          println("Veuillez entrer le code PIN. ")
          print(">")
          codepin = readInt()
        }

        // CODE PIN JUSTE
        if (codepin == 434343) {
          println("Code PIN correct : Accès autorisé.")

          // GESTION STOCK et AJOUTS
          println("Stocks : ")

          // SUCRE
          println("Sucre : " + SIsucre + "g")
          print("Ajout sucre >")
          sucreAjout = readInt()

          // CAFE
          println("Poudre de café : " + SIcafe + "g")
          print("Ajout café >")
          cafeAjout = readInt()

          // LAIT
          println("Lait : " + SIlait + "mL")
          print("Ajout Lait >")
          laitAjout = readInt()


          SIsucre += sucreAjout
          SIcafe += cafeAjout
          SIlait += laitAjout


          println("Niveaux de stocks mis à jour.")
          println("Retour au menu principal...")

        }
      }
    }
      // Mode séléctionné = quitter
      if(choixdumode == quitter) {
        println("Vous avez décidé de quitter.")
      }
    }
}


