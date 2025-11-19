import scala.io.StdIn._
import math._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var choixmode = 0
    var prixsucre = 0.0
    var boisson = 0
    var prixboisson = 0.0
    var dose = 0
    var stockinicafe = 50.0 // en gr
    var stocksucreini = 30.0
    var stocklaitini = 0.5 // en L
    var stocktot = true
    var taillela = 0
    var sucre = 0
    var lait = 0
    var prixdosedelait = 0.05
    var prixbaseexpresso = 2.00 // en CHF
    var prixbasecappucino = 2.50
    var prixbaselattepetit = 2.70
    var prixbaselattemoyen = 3.20
    var prixbaselattegrand = 3.70
    var prixfinal = 0.0
    var laitdose = 0.0
    var laitdosecappucino = 0.10
    var laitdoselattepetit = 0.12
    var laitdoselattemoyen = 0.15
    var laitdoselattegrand = 0.20
    var sucredose = 0.0
    var cafedose = 0.0
    var laitsupp = 0.0
    var prixlait = 0
    var prixtot = 0.0
    var boissonfin = "boisson"
    var niveaudesucre = "Sans sucre"
    var stockcafeajout = 0.0
    var stocklaitajout = 0.0
    var stocksucreajout = 0.0
    var programmeencours = true

    while (programmeencours) {
      choixmode = readLine("Nospresso Café\nVeuillez sélectionner votre mode\n1)Client\n2)Admin\n3)Quitter\n>").toInt
      while (!(choixmode == 1 || choixmode == 2 || choixmode == 3)) {
        println("Choix invalide. Veuillez entrer 1,2 ou 3")
        choixmode = readLine("Nospresso Café\nVeuillez sélectionner votre mode\n1)Client\n2)Admin\n3)Quitter\n>").toInt
      }

      //si le mode vaut 1, on rentre ds le mode client
      if (choixmode == 1) {
        var continuercommande = true
        while (continuercommande) {
          boisson = readLine("Veuillez sélectionner votre boisson:\n1)Expresso - CHF 2.00\n2)Cappucino - CHF 2.50\n3)Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
          while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
            boisson = readLine("Veuillez sélectionner une boisson valide").toInt
          }
          //Si Expresso
          if (boisson == 1) {
            cafedose = 8
            laitdose = 0
            prixboisson = prixbaseexpresso
            boissonfin = "Expresso"
          } else if (boisson == 2) { // si cappucino
            cafedose = 6
            laitdose = laitdosecappucino
            boissonfin = "Cappuccino"
            prixboisson = prixbasecappucino
          } else if (boisson == 3) { // si latte
            taillela = readLine("Quelle taille de latte voulez-vous?\n1)Petit\n2)Moyen\n3)Grand\n>").toInt
            while (!(taillela == 1 || taillela == 2 || taillela == 3))
              taillela = readLine("Veuillez entrer une taille valide").toInt
          }
          if (taillela == 1) {
            cafedose = 6
            laitdose = laitdoselattepetit
            prixboisson = prixbaselattepetit
            boissonfin = "Petit latte"
          } else if (taillela == 2) {
            cafedose = 8
            laitdose = laitdoselattemoyen
            prixboisson = prixbaselattemoyen
            boissonfin = "Moyen latte"
          } else if (taillela == 3) {
            cafedose = 12
            laitdose = laitdoselattegrand
            prixboisson = prixbaselattegrand
            boissonfin = "Grand latte"
          }


          // personnalisation sucre
          sucre = readLine("Souhaitez-vous ajouter du sucre?\n1)Sans sucre\n2)Peu (5g) - CHF 0.10\n3)Moyen (10g) - CHF 0.20\n4)Beaucoup (15g) - CHF 0.30\n>").toInt
          while (!(sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4)) {
            sucre = readLine("Veuillez sélectionner une réponse valide").toInt
          }
          if (sucre == 2) {
            sucredose = 5
            prixsucre = 0.10
            niveaudesucre = "Peu (5g)"
            prixfinal += 0.10
          } else if (sucre == 3) {
            sucredose = 10
            prixsucre = 0.20
            niveaudesucre = "Moyen (10g)"
            prixfinal += 0.20
          } else if (sucre == 4) {
            sucredose = 15
            prixsucre = 0.30
            niveaudesucre = "Beaucoup (15g)"
            prixfinal += 0.30
          } else {
            niveaudesucre = "Sans sucre"
          }

          //personnalisation lait
          lait = readLine("Souhaitez-vous ajouter du lait en supplément?\n(Disponible uniquement pour Cappucino et Latte)\n1)Oui\n2)Non\n>").toInt
          while (lait != 1 && lait != 2) {
            println("Veuillez choisir 1(oui) ou 2(non)")
            lait = readLine("Souhaitez-vous ajouter du lait en supplément?\n(Disponible uniquement pour Cappucino et Latte)\n1)Oui\n2)Non\n>").toInt
          }
          if (lait == 1) {
            if (boisson == 1) {
              println("On ne peut pas ajouter de lait pour un expresso.")
            } else {
              dose = readLine("Combien de dose ?\n>").toInt
              while (dose < 1 || dose > 3) {
                println("Vous ne pouvez ajouter que entre 1 et 3 doses de lait")
                dose = readLine("Combien de dose ?\n>").toInt
              }
              laitsupp = (dose * 0.05).toDouble
              laitdose += laitsupp
              prixfinal += dose * prixdosedelait
            }
          } else {
            println(" Pas de lait supplémentaire ajouté.")
          }

          // gestion stock
          stocklaitini = Math.round(stocklaitini * 100) / 100.0
          laitdose = Math.round(laitdose * 100) / 100.0
          if (stockinicafe < cafedose) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          } else if (stocksucreini < sucredose) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          } else if (stocklaitini < laitdose) {
            println("Erreur : Quantité de lait insuffisant pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            stocktot = true
          } else {
            continuercommande = false //stock suffisant, sort de la boucle
          }
        }

        //Affichage du prix
        val prixlait = dose * prixdosedelait
        //calcul commande ac prix tot
        val prixtot = prixboisson + prixlait + prixsucre
        println("Boisson sélectionnée: " + boissonfin + "\nNiveau de sucre: " + niveaudesucre + "\nLait supplémentaire : " + dose + " doses")
        printf("Prix total : CHF %.2f ", prixboisson.toDouble)
        if (sucre != 1) {
          printf(" + CHF %.2f", prixsucre.toDouble)
        }
        if (lait ==1) {
          printf(" + CHF %.2f", prixlait.toDouble)
        }
        printf(" = CHF  %.2f", prixtot.toDouble)

        //paiement twint
        var twint = Random.alphanumeric.take(5).mkString
        println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est: " + twint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")

        // préparation de la boisson
        println("Préparation de votre boisson...\n[...]\nVotre " + boissonfin + " est prêt ! Bonne dégustation !")

        //mise à jour stock
        stockinicafe -= cafedose
        stocksucreini -= sucredose
        stocklaitini -= laitdose
        stocklaitini = Math.round(stocklaitini * 100) / 100.00

      }

      // si mode 2 admin
      else if (choixmode == 2) {
        var pin = readLine("Entrer le code PIN: ****** ").toInt
        while (pin != 434343) {
          println("Code PIN incorrect, veuillez réessayer: ******")
          pin = readLine("Veuillez entrer le code PIN").toInt
        }
        println("Accès autorisé.")
        println("Stock :")
        println("Poudre de café : " + stockinicafe + "g")
        println("Lait : " + stocklaitini + "L")
        println("Sucre : " + stocksucreini + "g")
        //réapprovisionnement
        print("Réapprovisionnement des stocks...\nAjout :")
        stockcafeajout = readLine("\nPoudre de café (en g): ").toDouble
        stocklaitajout = readLine("Lait (en L): ").toDouble
        stocksucreajout = readLine("Sucre (en g): ").toDouble
        stockinicafe += stockcafeajout
        stocksucreini += stocksucreajout
        stocklaitini += stocklaitajout
        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal...")
      }
      //mode quitter
      else if (choixmode == 3) {
        programmeencours = false //sortir de la boucle
      }
    }
  }
}
