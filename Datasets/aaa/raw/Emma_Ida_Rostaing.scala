
import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var stockcafe = 50.0
    var stocksucre = 30.0
    var stocklait = 0.5 // litre

    var retour_erreur = false

    var continuer = true
    while (continuer == true){
      var mode: Int = 0

      var doselait = 0.05 // litre
      var dosesucre = 5 // g / dose


      println("   Nospresso Cafe   \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter")

      do {
        mode = readLine("> ").toInt
      } while (mode < 1 || mode > 3)

      if (mode == 2) {
        println("Mode Admin")
        println("Entrez le code PIN : ")

        var PIN: Int = 0

        do {
          PIN = readLine("> ").toInt
        } while (PIN != 434343)

        println("Accès autorisé.")
        println() //Saut de ligne
        println("Stocks:")

        printf("Poudre de café : %.2f g\n", stockcafe)
        printf("Lait : %.2f L\n", stocklait)
        printf("Sucre: %.2f g\n", stocksucre)

        println("Réapprovisionnement des stocks...\n Ajout : \n  Poudre de café : ")
        stockcafe += readInt()
        println("Lait :")
        stocklait += readDouble()
        println("Sucre :")
        stocksucre += readInt()

        println("Niveaux de stock mis à jour.\n Retour au menu principal...")

      } else if (mode == 1) {
        do {
          var prixdosesucre = 0.10
          var prixdoselait = 0.0
          var prixexpresso = 2.00
          var prixcappuccino = 2.50
          var prixlattepetit = 2.70
          var prixlattemoyen = 3.20
          var prixlattegrand = 3.70

          println("Veuillez sélectionner votre boisson : \n1) Expresso – CHF 2.00 \n2) Cappuccino – CHF 2.50 \n3) Latte – CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          var boisson: Int = 0
          var taillelatte: Int = 0
          var prixboisson = 0.0

          do {
            boisson = readLine("> ").toInt
          } while (boisson < 1 || boisson > 3)

          if (boisson == 1) {
          prixboisson = prixexpresso
          } else if (boisson == 2) {
          prixboisson = prixcappuccino
          } else if (boisson == 3) {
          println("Choisissez la taille de votre Latte : \n 1) Petit - CHF 2.70 \n 2) Moyen - CHF 3.20 \n 3) Grand - CHF 3.70)")
            do {
              taillelatte = readLine("> ").toInt
            } while (taillelatte < 1 || taillelatte > 3)
          }
          var nomsucre = ""
          var sucre: Int = 0
          var prixsucre = 0.0
          var sans_sucre = 0
          var peu_sucre = 5
          var moyen_sucre = 10
          var beaucoup_sucre = 15

          var besoincafe: Double = 0
          var besoinsucre: Double = 0
          var besoinlait: Double = 0

          println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30")

          do {
            sucre = readLine("> ").toInt
          } while (sucre < 1 || sucre > 4)

          if (sucre == 1) {
            nomsucre = "Sans sucre"
            besoinsucre = sans_sucre
            prixsucre = 0.0
          } else if (sucre == 2) {
            nomsucre ="Peu"
            besoinsucre = peu_sucre
            prixsucre = prixdosesucre
          } else if (sucre == 3) {
            nomsucre ="Moyen"
            besoinsucre = moyen_sucre
            prixsucre = prixdosesucre * 2
          } else if (sucre == 4) {
            nomsucre ="Beaucoup"
            besoinsucre = beaucoup_sucre
            prixsucre = prixdosesucre * 3
          }

          //vérification des stocks
          if (stockcafe < besoincafe) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            retour_erreur = true
          } else if (stocklait < besoinlait) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            retour_erreur = true
          } else if (stocksucre < besoinsucre) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            retour_erreur = true
          } else{

            var nomboisson: String = ""
            var nomchoixlaitsupp : String = ""
            var lait: Int = 0
            var doselait: Int = 0
            var prixlait = 0.0
            var laitsupplement = 0.0
            var prixlaitsupplement : Double = 0
            var quantitelaitsupplement = doselait * lait

            if (boisson == 2 || boisson == 3) {
              println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte) \n1) Oui\n2) Non")
              do {
                lait = readLine("> ").toInt
              } while (lait < 1 || lait > 2)
              if (lait < 1 || lait > 2) println("Erreur. Veuillez sélectionner votre choix (1 ou 2).")

              if (lait == 1) {
                nomchoixlaitsupp = "Oui"
                println("Combien de dose ? max 3 doses, CHF 0.05 une dose de lait supplémentaire.")
                do {
                  doselait = readLine("> ").toInt
                } while (doselait < 1 || doselait > 3)

                if (doselait < 1 || doselait > 3) {
                  println("Erreur. Veuillez sélectionner une valeur entre 1 et 3.")
                }

                laitsupplement = doselait * 0.05
                prixlaitsupplement = doselait * 0.05

                if (doselait == 1) {
                  laitsupplement = 0.05
                  prixlaitsupplement = 0.05
                } else if (doselait == 2 ) {
                  laitsupplement = 0.10
                  prixlaitsupplement = 0.10
                } else if (doselait == 3){
                  laitsupplement = 0.15
                  prixlaitsupplement = 0.15
                }

              } else if (lait==2){
                nomchoixlaitsupp = "Non"
                laitsupplement = 0
                prixlaitsupplement = 0
              }
            }

            if (boisson == 1) {
              nomboisson = "Expresso"
              besoincafe = 8
              besoinsucre = (sucre - 1) * 5
              println("Boisson sélectionnée : Expresso")
            } else if (boisson == 2) {
              nomboisson = "Cappuccino"
              besoincafe = 6
              besoinsucre = (sucre - 1) * 5
              besoinlait = 0.1
              println("Boisson sélectionnée : Cappuccino")
            } else if (boisson == 3) {
              nomboisson = "Latte"

              if (taillelatte == 1) {
                nomboisson = "Latte Petit"
                prixboisson = prixlattepetit
                besoincafe = 6
                besoinsucre = (sucre - 1) * 5
                besoinlait = 0.1
                println("Boisson sélectionnée : Latte (Petit)")
              } else if (taillelatte == 2) {
                nomboisson = "Latte Moyen "
                prixboisson = prixlattemoyen
                besoincafe = 6
                besoinsucre = (sucre - 1) * 5
                besoinlait = 0.1
                println("Boisson sélectionnée : Latte (Moyen)")
              } else if (taillelatte == 3) {
                nomboisson ="Latte Grand"
                prixboisson = prixlattegrand
                besoincafe = 6
                besoinsucre = (sucre - 1) * 5
                besoinlait = 0.1
                println("Boisson sélectionnée : Latte (Grand)")
              }
            }

            if (stocklait < besoinlait + laitsupplement) {
              println ("Boisson sélectionnée : " + nomboisson)
              println ("Niveau de sucre : " + nomsucre)
              println ("Lait en supplément : " + nomchoixlaitsupp)
              println ("\n Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              retour_erreur = true
            } else {
              if (! retour_erreur){
                println("Niveau de sucre : "+ nomsucre + "(" + besoinsucre + "g)")

                if (boisson == 2 || boisson == 3){
                  println("Lait supplémentaire : " + nomchoixlaitsupp )
                }
                val total = prixboisson + prixsucre + prixlaitsupplement
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prixboisson, prixsucre, prixlaitsupplement, total)
                println ("\n")


                //Payement
                val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
                println("Veuillez payer en utilisant Twint.")
                println("Votre code de paiement est :" + codeTwint)
                println("(En attente de paiement...)")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté..")
                println("Préparation de votre boisson...")
                println("Votre " + nomboisson + " est prêt ! Bonne dégustation !")

                stockcafe -= besoincafe
                stocksucre -= besoinsucre
                stocklait -= besoinlait + quantitelaitsupplement

              }
            }
          }
        } while (retour_erreur)

      } else if (mode == 3) {
        continuer = false
      }
    }
  }
}



