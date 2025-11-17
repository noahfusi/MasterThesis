import scala.io.StdIn._

object Nospresso {
  def main(args: Array[String]): Unit = {
    var stockcafe = 50 // grammes
    var stocksucre = 30  // grammes
    var stocklait = 0.5 // litres
    val adminPin = "434343"  //valeur par défaut
    var eteindre = false

    while (!eteindre) {      // le distributeur commence allumé
      println("Nospresso Café \n Veuillez sélectionner votre mode : \n 1) Client \n 2) Admin \n 3) Quitter"  )


      val mode = readInt()
      if (mode == 1) {  // mode client
        println("Veuillez sélectionner votre boisson : \n 1) Expresso - CHF 2.00  \n 2) Cappuccino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit),\n CHF 3.20 (Moyen) \n CHF 3.70 (Grand) ")
        var boisson = readInt()
        var typeboisson = ""
        var qtecafe = 0
        var qtelait = 0.0
        var prix = 0.0
        if (boisson == 1) {
          qtecafe = 8
          prix = 2.0
          typeboisson = "Expresso"
        } else if (boisson == 2) {
          qtecafe = 6
          qtelait = 0.1
          prix = 2.5
          typeboisson = "Cappucino"
        } else if (boisson == 3) {
          println("Choisissez la taille : \n 1) Petit - 120ml \n 2) Moyen - 150ml \n 3) Grand - 200ml ")
          var taille = readInt()
          if (taille == 1) {
            qtecafe = 6
            qtelait = 0.12
            prix = 2.7
          } else if (taille == 2) {
            qtecafe = 8
            qtelait = 0.15
            prix = 3.2
          } else if (taille == 3) {
            qtecafe = 12
            qtelait = 0.2
            prix = 3.7
          }
          typeboisson = "Latte"
        } else {
          println("Choix invalide, retour au menu principal.")
        }

        println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 ")


        val taillesucre = readInt()
        var qtesucre = 0
        var coutsucre = 0.0
        if (taillesucre == 2) {
          qtesucre = 5
          coutsucre = 0.10
        } else if (taillesucre == 3) {
          qtesucre = 10
          coutsucre = 0.20
        } else if (taillesucre == 4) {
          qtesucre = 15
          coutsucre = 0.30
        }

        var supplait = 0.0
        if (boisson == 2 || boisson == 3) { // Cappuccino ou Latte
          println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")


          val choixsupplait = readInt()
          if (choixsupplait == 1) {
            println("Combien de dose ? ")
            print("> ")
            var doses = readInt()
            if (doses >= 1 && doses <= 3) {
              supplait = doses * 0.05 // Chaque dose = 0.05L
              prix += doses * 0.05 // Chaque dose coûte CHF 0.05
            } else {
              println("Nombre de doses invalide, aucun lait ajouté.")
            }
          }
        }

        var prixtotal = prix + coutsucre
        if (stockcafe >= qtecafe && stocksucre >= qtesucre && stocklait >= (qtelait + supplait)) {
          println("Prix total : CHF " + prixtotal)
          println("\nVeuillez payer en utilisant Twint.")
          val valeur = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var twintcode = ""
          var index = 0
          while (index < 5){
            twintcode += valeur(scala.util.Random.nextInt(valeur.length))
            index = index + 1
          }
          println("Votre code de paiement est : " + twintcode)
          var twintcodeentre = readLine("Veuillez entrer votre code : ")
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000) // Pause pour simuler le paiement
          //while (twintcodeentre != twintcode){
          //  twintcodeentre =readLine("Le code saisi est erroné, veuillez le saisir à nouveau : ")
          // }
          println("\nMerci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson ...")
          Thread.sleep(2000)
          println("[...]")
          println("Votre " + typeboisson + " est prêt ! Bonne dégustation !")

          stockcafe -= qtecafe
          stocksucre -= qtesucre
          stocklait -= qtelait + supplait
        } else {
          if (stockcafe < qtecafe) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparér la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          }
          if (stocksucre < qtesucre) {
            println("Erreur : Quantité de sucre insuffisante pour préparér la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          }
          if (stocklait < qtelait) {
            println("Erreur : Quantité de poudre de lait insuffisante pour préparér la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
          }
        }
      } else if (mode == 2) {
        println("Mode Admin")
        print("Entrez le code PIN : ")
        var pin = readLine()
        if (pin == adminPin) {
          println("Accès autorisé.")
          println("\nStocks :")
          println("    Poudre de café: " + stockcafe + "g")
          println("    Lait          : " + stocklait + "L")
          println("    Sucre         : " + stocksucre + "g")
          println("\nRéapprovisionnement des stocks... ")
          println("Ajout :")
          print("    Poudre de café: ")
          stockcafe += readInt()
          print("    Lait          : ")
          stocklait += readDouble()
          print("    Sucre         : ")
          stocksucre += readInt()

          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
        } else {
          println("Code PIN incorrect.")
        }

      } else if (mode == 3) {
        eteindre = true
      } else {
        println("Entrée invalide, veuillez réessayer.")

      }
    }
  }
}