import io.StdIn._
import math._
import scala.util.Random

object main {
  def main(args: Array[String]) = {
    var etatDeDepart = 0
    var stockSucre = 15
    var stockPourdreCafe = 50
    var stockLait = 0.5

    var prixBoisson = 0.0
    var prixSucre = 0.0
    var prixLait = 0.0
    var prixTotal = prixBoisson + prixSucre + prixLait
    var boucle = true
    var paiement = true
    var dose = true

    while (etatDeDepart == 0) {


      var choix = readLine(
        "        Nospresso Café\n" +
          "Veuillez sélectionner votre mode :\n" +
          "1) Client\n" +
          "2) Admin\n" +
          "3) Quitter\n" +
          ">"
      ).toInt

      if (choix == 1) {
        var boisson = readLine(
          "Veuillez sélectionner votre boisson : \n" +
            "1) Expresso - CHF 2.00\n" +
            "2) Cappuchino - CHF 2.50\n" +
            "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" +
            ">"
        ).toInt

        while (boucle) {


          if (boisson == 1) {
            prixBoisson += 2.00
            stockPourdreCafe -= 8
            if (stockPourdreCafe < 0) {
              println(
                "Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
              )
              boucle = false
              paiement = false
              dose = false
            }
          }

          if (boisson == 2) {
            prixBoisson += 2.50
            stockPourdreCafe -= 6
            stockLait -= 0.1
            if (stockPourdreCafe < 0) {
              println(
                "Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
              )
              boucle = false
              paiement = false
              dose = false
            }
            if (stockLait < 0) {
              println(
                "Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
              )
              boucle = false
              paiement = false
              dose = false
            }
          }

          if (boisson == 3) {
            var taille = readLine(
              "Quelle taille désirez-vous ? :  \n" +
                "1) Petit \n" +
                "2) Moyen \n" +
                "3) Grand\n" +
                ">"
            ).toInt

            if (taille == 1) {
              prixBoisson += 2.70
              stockPourdreCafe -= 6
              stockLait -= 0.120
              if (stockPourdreCafe < 0 || stockLait < 0) {
                println(
                  "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
                )
                boucle = false
                paiement = false
                dose = false
              }
            } else if (taille == 2) {
              prixBoisson += 3.20
              stockPourdreCafe -= 8
              stockLait -= 0.150
              if (stockPourdreCafe < 0 || stockLait < 0) {
                println(
                  "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
                )
                boucle = false
                paiement = false
                dose = false
              }
            } else if (taille == 3) {
              prixBoisson += 3.70
              stockPourdreCafe -= 12
              stockLait -= 0.200
              if (stockPourdreCafe < 0 || stockLait < 0) {
                println(
                  "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
                )
                boucle = false
                paiement = false
                dose = false
              }
            }
          }

          if (boisson == 1 || boisson == 2 || boisson == 3) {
            if (boucle) {
              var sucre = readLine(
                "Souhaitez-vous ajouter du sucre ? \n" +
                  "1) Sans sucre\n" +
                  "2) Peu (5g) - CHF 0.10\n" +
                  "3) Moyen (10g) - CHF 0.20\n" +
                  "4) Beaucoup (15g) - CHF 0.30\n" +
                  ">"
              ).toInt




              if (sucre ==1) {
                boucle = false
                prixSucre += 0.0
              }
              if (sucre ==2) {
                stockSucre -= 5
                prixSucre += 0.10
                boucle = false
              }
              if (sucre==3) {
                stockSucre -= 10
                prixSucre += 0.20
                boucle = false
              }
              if (sucre==4) {
                stockSucre -= 15
                prixSucre += 0.30
                boucle = false
              }

              if (stockSucre < 0) {
                println(
                  "Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
                )
                boucle = false
                paiement = false
                dose = false
              }
            }
          }

          if (boisson == 2 || boisson == 3 ) {
            if (dose){
            var laitSupplement = readLine(
              "Souhaitez-vous ajouter du lait en supplément ?\n" +
                "(Disponible uniquement pour Cappuccino et Latte)\n" +
                "1) Oui\n" +
                "2) Non\n" +
                ">"
            ).toInt

            if (laitSupplement == 1) {
              var nbdose = readLine("Combien de doses ?\n>").toInt
              while (nbdose < 1 || nbdose > 3) {
                println("Erreur : Les doses doivent être comprises entre 1 et 3")
                nbdose = readLine("Combien de doses ?\n>").toInt
              }
              stockLait -= nbdose * 0.05
              prixLait += nbdose * 0.05
              boucle = false


              if (stockLait < 0) {
                println(
                  "Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
                )
                boucle = false
                paiement = false
                dose = false
              }
            }
            else if (laitSupplement==2){
              boucle = false
            }
          }
          }
        }


        if (paiement) {
          prixTotal = prixBoisson + prixSucre + prixLait
          printf("Prix total: %.2f CHF ",prixTotal)

          val characters = ('A' to 'Z') ++ ('0' to '9')
          val length = 5
          var randomString = ""

          for (_ <- 1 to length) {
            randomString += Random.shuffle(characters).head
          }


          println("Veuillez payer en utilisant Twint." + "\n" + "Votre code de paiement est :" + randomString)
          println("(En attente de validation du paiement...)")
          println("\n" + "Merci ! Votre paiement a été accepté.")

          println("Préparation de votre boisson...")
          println("[...]")
          Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)

          if (boisson == 1)
            println("Votre expresso est prêt ! Bonne dégustation ! ")

          if (boisson == 2)
            println("Votre cappuchino est prêt ! Bonne dégustation ! ")

          if (boisson == 3)
            println("Votre Latte est prêt ! Bonne dégustation ! ")

          boucle = false

        }
      }
      else if (choix == 2) {
        var codePIN = readLine("Mode Admin" + "\n" + "Entrez le code PIN :  ").toInt
        if (codePIN == 434343) {
          println("Accès autorisé.")
          println("\n" + "Stocks: " + "\n" + "    Poudre à café : " + stockPourdreCafe + "g" + "\n" + "    Lait: " + stockLait + "L" + "\n" + "    Sucre : " + stockSucre + "g")
          var SucreAjoute = readLine("Sucre : ").toInt
          var LaitAjoute = readLine("Lait : ").toDouble
          var PoudreCafeAjoute = readLine("Poudre à café : ").toInt
          println("Réapprovisionnement des stocks... " + "\n" + "Ajout : ")
          printf("   Pourdre à café : " + PoudreCafeAjoute + "\n" + "   Lait :  %.2f\n" + "   Sucre : " + SucreAjoute,LaitAjoute)
          println("Niveaux de stocks mis à jour. ")

          stockSucre += SucreAjoute
          stockLait += LaitAjoute
          stockPourdreCafe += PoudreCafeAjoute
        }
        else {
          println("Accès refusé")
        }

      }
      else if (choix == 3) {

      }
      else {
        println("")
      }

      prixBoisson = 0.0
      prixSucre = 0.0
      prixLait = 0.0
      prixTotal = 0.0

      boucle = true
      paiement = true
      println("Retour au menu Principal...")
    }
  }
}