import io.StdIn._
import math._
object Main {
  def main(args: Array[String]): Unit = {
    // stocks initials et variable bocle global
    var cafestocks = 50
    var sucrestocks = 30
    var laitstocks = 500 // je l'ai transforme en ml
    var boclemenu = true
    var prixtotal =0.0

    while (boclemenu) {
      // selectionner de mode menu pricipale
      println("         \n  Nospresso Cafe ")
      var mode = readLine("veuillez selectionner votre mode ( avec les chiffres ) :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("votre choix de mode n'est pas valide veuillez choisir un mode valide. ")
        mode = readLine("veuillez selectionner votre mode :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      }

      // mode 1


      if (mode == 1) {

        //selectionnement de boisson
        var boisson = readLine("veuillez selectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - 2.70 (Petite) , CHF 3.20 (Moyen) , CHF 3.70 (Grand)\n").toInt

        while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
          println("votre choix de boisson n'est pas valide. Veuillez choisir un boisson valide. ")
          boisson = readLine("veuillez selectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - 2.70 (Petite) , CHF 3.20 (Moyen) , CHF 3.70 (Grand)\n").toInt
        }


        //selectionnement de taille de latte

        var taillelatte = 0
        if (boisson == 3) {
          taillelatte = readLine("quel taille ? \n 1) Petite \n 2) Moyen \n 3) Grand\n").toInt
          while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
            println("votre choix de taille n'est pas valide. veuillez choisire une taille disponible: ")
            taillelatte = readLine("quel taille ? \n 1) Petite \n 2) Moyen \n 3) Grand\n").toInt
          }
        }

        // selectionnement de niveau de sucre

        var niveausucre = readLine("Souhaitez-Vous ajouter du sucre ? \n 1) sans sucre \n 2) peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n ").toInt
        while (!(niveausucre == 1 || niveausucre == 2 || niveausucre == 3 || niveausucre == 4)) {
          println("votre choix de niveau sucre n'est pas valide. Veuillez selectionner une choix valide.")
          niveausucre = readLine("Souhaitez-Vous ajouter du sucre ? \n 1) sans sucre \n 2) peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n ").toInt
        }

        // selectionnement de lait suplementaire disponible uniquement pour capaccino et latte

        var choixlaitsuplementaire = 0
        if (boisson == 2 || boisson == 3) {
          choixlaitsuplementaire = readLine("Souhaitez-Vous ajouter du lait en supplement ? \n (Disponible uniquement pour Cappacino et Latte) \n 1) Oui \n 2) Non \n").toInt
          while (!(choixlaitsuplementaire == 1 || choixlaitsuplementaire == 2)) {
            println("votre choix de lait n'est pas valide. Veuillez selectionner une choix valide. ")
            choixlaitsuplementaire = readLine("Souhaitez-Vous ajouter du lait en supplement ? \n (Disponible uniquement pour Cappacino et Latte) \n 1) Oui \n 2) Non \n").toInt
          }
        }



        // les ingrediant cafe et lait et les prix

        var cafeconsomme = 0
        var laitconsomme = 0
        var prixbase = 0.0
        var boissonselectionne = ""

        if (boisson == 1) {
          cafeconsomme = 8
          laitconsomme = 0
          prixbase = 2.00
          boissonselectionne = " Expresso "
        }
        else if (boisson == 2) {

          cafeconsomme = 6
          laitconsomme = 100
          prixbase = 2.50
          boissonselectionne = " Cappaccino "
        }
        else if (boisson == 3) {
          if (taillelatte == 1) {
            cafeconsomme = 6
            laitconsomme = 120
            prixbase = 2.70
            boissonselectionne = " Latte (petit) "
          }
          if (taillelatte == 2) {
            cafeconsomme = 8
            laitconsomme = 150
            prixbase = 3.20
            boissonselectionne = " Latte ( Moyen ) "
          }
          else if (taillelatte == 3) {
            cafeconsomme = 12
            laitconsomme = 200
            prixbase = 3.70
            boissonselectionne = " Latte ( Grand ) "
          }
        }


        /// la consommation de sucre
        var sucreconsomme = 0
        var nivsucreaiffichealafin = ""
        var prixsucre = 0.0

        if (niveausucre == 2) {
          sucreconsomme = 5
          prixsucre = 0.10
          nivsucreaiffichealafin = "peu (5g)"
        }
        else if (niveausucre == 3) {
          sucreconsomme = 10
          prixsucre = 0.20
          nivsucreaiffichealafin = "Moyen (10g)"
        }
        else if (niveausucre == 4) {
          sucreconsomme = 15
          prixsucre = 0.30
          nivsucreaiffichealafin = "Beaucoup (15g)"
        }

        // lait suplementaire


        var prixlaitsupplementaire = 0.0
        var laitafficher = ""
        if (choixlaitsuplementaire == 1) {
          laitafficher = " oui"
          println("combien de dose de lait ? (max 3 )")
          var doselait = readInt()
          while (doselait < 1 || doselait > 3) {
            println("vous avez entrez une nombre de dose invalide. Veuillez choisir une dose entre 1 et 3 . ")
            doselait = readLine("combien de dose de lait ? (max 3 ) ").toInt
          }
          prixlaitsupplementaire = doselait * 0.05
          laitconsomme += doselait * 50
        }
        else if (choixlaitsuplementaire == 2) {
          laitafficher = " Non"
        }


        prixtotal = prixbase + prixsucre + prixlaitsupplementaire


        //mettre a jour le stocks

        if (cafeconsomme <= cafestocks && laitconsomme <= laitstocks && sucreconsomme <= sucreconsomme) {

          cafestocks -= cafeconsomme
          laitstocks -= laitconsomme
          sucrestocks -= sucreconsomme

          println(" \n \n Boisson selectionee : " + boissonselectionne + "\n Niveau de sucre : " + nivsucreaiffichealafin)
          if (boisson == 2 || boisson == 3) {
            println("lait suplementaire : " + laitafficher)
          }

          //printf("prixtotal :  %.2f ", prixtotal)
          printf("\nPrix total : " + prixbase + " + " + prixsucre + " + " + prixlaitsupplementaire + " =  %.2f " , prixtotal)

          println("\nVeuillez payer en utilisant Twint.\n Votre code de paiement est : AB12X \n (En attente de validation du paiement....")
          Thread.sleep(5000)
          println("\n Paiment confirme. \n \nPreparation de votre boisson... ")
          Thread.sleep(5000)
          println("votre " + boissonselectionne + " est pret ! Bonne degustation ! ")


        }
        else {
          if (cafeconsomme > cafestocks) {
            println(" Erreur : Quantite de paudre de cafe insuffisant pour préparer la boisson selectionne. \n Veuillez choisir une autre boisson au verifier les stocks en mode admin")
          }
          else if (sucreconsomme > sucrestocks) {
            println("Erreur : Quantite de sucre insuffisant pour preparer la boisson selectionne.\n Veuillez choisir une autre boisson au verifier les stocks en mode admin  ")
          }
          else if (laitconsomme > laitstocks) {
            println("Erreur : Quantite de lait insuffisant pour preparer la boisson selectionne. \n Veuillez choisir une autre boisson au verifier les stocks en mode admin")
          }
        }
      }


      else if (mode == 2) {
        var PIN = readLine("Entrez le code PIN : ").toInt
        while (PIN != 434343) {
          println("le code n'est pas valide. \n Ressayez. ")
          PIN = readLine("Entrez le code PIN : ").toInt
        }
        println("Acces autorise. \n\n Stocks :")
        println("paudre de cafe: " + cafestocks + "\n laite: " + laitstocks + "\n sucre: " + sucrestocks)
        println("Reapprovisionnement des stocks:..... \n Ajout ")
        cafestocks += readLine("\n paudre de cafe: ").toInt
        laitstocks += readLine("\n lait : ").toInt
        sucrestocks += readLine("\n Sucre : ").toInt
        println("Niveaux de Stocks mis a jour.")
        println("Retour au menu principal......")

      }
      else if (mode == 3) {
        println(" vous avez quitter le programme.")
        boclemenu = false
      }
    }

    }


}