import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var stockCafe = 50
    var stockSucre = 30
    var stockLait = 0.5
    val prixExpresso = 2.00
    val prixCappuccino = 2.50
    val prixLattePetit = 2.70
    val prixLatteMoyen = 3.20
    val prixLatteGrand = 3.70
    val PINADMIN = 434343
    var quitter = false

    while (!quitter) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      var choixMode = 0
      do {
        print("> ")
        choixMode = readLine().toInt
        if (choixMode != 1 && choixMode != 2 && choixMode != 3) {
          println("Choix invalide. Veuillez entrer un chiffre entre 1 et 3.")
        }
      } while (choixMode != 1 && choixMode != 2 && choixMode != 3)

      if (choixMode == 1) {
        var boissonPreparee = false
        while (!boissonPreparee) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

          var choixBoisson = 0
          do {
            choixBoisson = readLine(">").toInt
            if (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3) {
              println("Choix invalide. Veuillez entrer 1, 2 ou 3.")
            }
          } while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)

          var prixFinal = 0.0
          var quantitePoudreCafe = 0
          var quantiteLait = 0.0
          var tailleLatte = 0
          var nomBoisson = ""

          if (choixBoisson == 1) {
            prixFinal = prixExpresso
            quantitePoudreCafe = 8
            quantiteLait = 0
            nomBoisson = "Expresso"
          } else if (choixBoisson == 2) {
            prixFinal = prixCappuccino
            quantitePoudreCafe = 6
            quantiteLait = 0.1
            nomBoisson = "Cappuccino"
          } else if (choixBoisson == 3) {
            println("Sélectionnez la taille du Latte :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")

            do {
              tailleLatte = readLine(">").toInt
              if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
                println("Choix invalide. Veuillez entrer 1, 2 ou 3.")
              }
            } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)

            if (tailleLatte == 1) {
              prixFinal = prixLattePetit
              quantitePoudreCafe = 6
              quantiteLait = 0.12
              nomBoisson = "Latte (Petit)"
            } else if (tailleLatte == 2) {
              prixFinal = prixLatteMoyen
              quantitePoudreCafe = 8
              quantiteLait = 0.15
              nomBoisson = "Latte (Moyen)"
            } else if (tailleLatte == 3) {
              prixFinal = prixLatteGrand
              quantitePoudreCafe = 12
              quantiteLait = 0.2
              nomBoisson = "Latte (Grand)"
            }
          }

          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")

          var choixSucre = 0
          var quantiteSucre = 0
          var prixSucre = 0.0
          var niveauSucre = ""

          do {
            choixSucre = readLine(">").toInt
            if (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4) {
              println("Choix invalide. Veuillez entrer un chiffre entre 1 et 4.")
            }
          } while (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4)

          if (choixSucre == 1) {
            quantiteSucre = 0
            prixSucre = 0.0
            niveauSucre = "Sans sucre"
          } else if (choixSucre == 2) {
            quantiteSucre = 5
            prixSucre = 0.10
            niveauSucre = "Peu (5g)"
          } else if (choixSucre == 3) {
            quantiteSucre = 10
            prixSucre = 0.20
            niveauSucre = "Moyen (10g)"
          } else if (choixSucre == 4) {
            quantiteSucre = 15
            prixSucre = 0.30
            niveauSucre = "Beaucoup (15g)"
          }

          prixFinal += prixSucre

          if (stockCafe < quantitePoudreCafe) {
            println("Boisson sélectionnée :" + nomBoisson)
            println("Niveau de sucre : " + niveauSucre)
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson.")
          } else if (stockSucre < quantiteSucre) {
            println("Boisson sélectionnée : " + nomBoisson)
            println("Niveau de sucre : " + niveauSucre)
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson.")
          } else if (stockLait < quantiteLait) {
            println("Boisson sélectionnée : " + nomBoisson)
            println("Niveau de sucre : " + niveauSucre)
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou une autre boisson.")
          } else {
            val codePaiement = Random.alphanumeric.take(5).mkString
            println("Boisson sélectionnée : " + nomBoisson)
            println("Niveau de sucre : " + niveauSucre)
            printf("Prix total : CHF %.2f\n", prixFinal)
            println("Veuillez payer en utilisant Twint. Votre code de paiement est : " + codePaiement)
            println("(En attente de paiement...)")
            Thread.sleep(3000)
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            println("Votre" + nomBoisson + "est prêt ! Bonne dégustation !")
            Thread.sleep(5000)

            stockCafe -= quantitePoudreCafe
            stockSucre -= quantiteSucre
            stockLait -= quantiteLait
            boissonPreparee = true
          }
        }
      } else if (choixMode == 2) {
        println("Entrez le code PIN :")
        val pin = readLine("> ").toInt
        if (pin == PINADMIN) {
          println("Réapprovisionnement des stocks :")
          println("Stock actuel - Café : " + stockCafe + "(g)", "Lait :" + stockLait + "(L)", "Sucre :" + stockSucre + "(g)")
          print("Quantité de café à ajouter (g) : ")
          stockCafe += readLine().toInt
          print("Quantité de sucre à ajouter (g) : ")
          stockSucre += readLine().toInt
          print("Quantité de lait à ajouter (litres) : ")
          stockLait += readLine().toDouble
          println("Stocks mis à jour.")
          println("Stock modifié - Café : " + stockCafe + "(g)", "Lait :" + stockLait + "(L)", "Sucre :" + stockSucre + "(g)")
        } else {
          println("Code PIN incorrect.")
        }
      } else if (choixMode == 3) {
        println("Merci d'avoir utilisé Nospresso Café !")
        quitter = true
      }
    }
  }
}
