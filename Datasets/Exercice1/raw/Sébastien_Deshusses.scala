import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var mode = ""
    var continue = true // utile pour permettre à l'utilisateur de quitter le programme s'il ne peut pas commander sa boisson.

    // Code pin du mode Admin
    val codePIN = "434343"

    // Stocks initiaux d'ingrédients (ne s'affiche pas à l'utilisateur)
    var stockCafe = 50.0 // exprimé en grammes
    var stockSucre = 30.0 // exprimé en grammes
    var stockLait = 0.500 // expérimé en litres

    // Fonction pour afficher les stocks
    def afficherStocks(): Unit = {
      println("Mode Admin")
      println("Stock de poudre de café: " + stockCafe + " gm")
      println("Stock de lait : " + stockLait + " litre")
      println("Stock de sucre: " + stockSucre + " gm")
    }

    // Fonction réapprovisionnement
    def majStock(): Unit = {
      println("Voulez-vous réapprovisionner les stocks ?")
      println("1) Oui")
      println("2) Non")
      val choixMajStock = readLine ("Votre choix: ").toInt

      if (choixMajStock == 1) {
        println("Ajout poudre de café en gramme: ")
        val majCafe = readLine().toDouble
        stockCafe += majCafe

        println("Ajout lait en litre: ")
        val majLait = readLine().toDouble
        stockLait += majLait

        println("Ajout sucre en gramme: ")
        val majSucre = readLine().toDouble
        stockSucre += majSucre

        println("Niveaux de stock mis à jour. Retour au menu principal.")
      }
    }

    // Choix du mode d'utilisateur
    println("Nospresso Café")
    while (continue) {
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode = readLine("Votre mode: ")

      //Mode client: sélection de boisson
      if (mode == "1") {
        var newChoixBoisson = true // permet de faire revenir l'utilisateur à cet endroit en cas d'indisponibillité des ingrédients et en fonction de son choix: choisir une nouvelle boisson ou quitter le programme)

        while (newChoixBoisson) {
          println("Veuillez sélectionner votre boisson: ")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte petit - CHF 2.70")
          println("4) Latte moyen - CHF 3.20")
          println("5) Latte grand - CHF 3.70")

          var boissonCommandee = readLine("Indiquez le numéro correspondant à votre boisson (1-5): ").toInt

          var prixBoisson = 0.0
          var nomBoisson = ""
          var cafeBase = 0.0
          var laitBase = 0.0
          var sucreBase = 0.0

          if (boissonCommandee == 1) {
            prixBoisson = 2.00
            nomBoisson = "Expresso"
            cafeBase = 8.0
          } else if (boissonCommandee == 2) {
            prixBoisson = 2.50
            nomBoisson = "Cappucino"
            cafeBase = 6.0
            laitBase = 0.1
          } else if (boissonCommandee == 3) {
            prixBoisson = 2.70
            nomBoisson = "Latte petit"
            cafeBase = 6.0
            laitBase = 0.12
          } else if (boissonCommandee == 4) {
            prixBoisson = 3.20
            nomBoisson = "Latte moyen"
            cafeBase = 8.0
            laitBase = 0.15
          } else if (boissonCommandee == 5) {
            prixBoisson = 3.70
            nomBoisson = "Latte grand"
            cafeBase = 12.0
            laitBase = 0.20
          }

          // Vérification du stock de café
          if (boissonCommandee == 1 && stockCafe < cafeBase) {
            println("Erreur: quantié de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en monde Admin")
            println("Que voulez-vous faire ?")
            println("1) Choisir une nouvelle boisson")
            println("2) Accéder au mode Admin du distributeur")

            val choixUtilisateur = readLine("Indiquez votre choix (1 ou 2): ").toInt

            if (choixUtilisateur == 1) {
              newChoixBoisson = true
            } else if (choixUtilisateur == 2) {
              newChoixBoisson = false
            }

            // Avant d'afficher la boisson, il faut vérifier si les quantités de lait, de base pour faire la boisson choisie sont disponibles

            if (stockLait < laitBase) {
              println("Erreur: quantié de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson ou quitter le programme.")
              println("1) Changer de choix de boisson")
              println("2) Quitter le programme")

              val choixUtilisateur = readLine("Votre choix (1 ou 2): ").toInt

              if (choixUtilisateur == 1) {
                newChoixBoisson = true
              } else {
                println("Vous avez quitté le programme. A bientôt.")
                newChoixBoisson = false
                continue = false
              }
            } else {
              newChoixBoisson = false
            }
          }

          // Afficher la boisson commandée

          println("Vous avez choisi un " + nomBoisson + " - CHF " + prixBoisson)

          if (stockCafe >= cafeBase && stockLait >= laitBase) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
          }
          val choixSucre = readLine("Indiquez votre choix de scure avec 1, 2, 3, 4: ").toInt

          if (choixSucre == 2) sucreBase = 5.0
          else if (choixSucre == 3) sucreBase = 10.0
          else if (choixSucre == 4) sucreBase = 15.0

          if (stockSucre >= sucreBase) {

            // Ajouter le prix du sucre
            if (choixSucre == 1) {
              println("Vous avez choisi sans sucre, le prix reste le même")
            } else if (choixSucre == 2) {
              println("Vous avez choisi peu de sucre (5g) - CHF 0.10")
              prixBoisson += 0.10 // permet d'ajouter le prix de la dose de sucre au prix de la boisson
            } else if (choixSucre == 3) {
              println("Vous avez choisi moyen de sucre (10g) - CHF 0.20")
              prixBoisson += 0.20 // permet d'ajouter le prix de la dose de sucre au prix de la boisson
            } else if (choixSucre == 4) {
              println("Vous avez choisi beaucoup de sucre (15g) - CHF 0.30")
              prixBoisson += 0.30 // permet d'ajouter le prix de la dose de sucre au prix de la boisson
            }

            if (boissonCommandee == 2 || boissonCommandee == 3 || boissonCommandee == 4 || boissonCommandee == 5) {
              println("Souhaitez-vous ajouter du lait en suppllément ?")
              println("1) Oui")
              println("2) Non")

              val choixLait = readLine("Indiquez si vous souhaitez ajouter du lait avec 1 ou 2: ").toInt


              if (choixLait == 1) {
                println("Vous pouvez ajouter 1, 2 ou 3 doses de lait supplémentaire. 1 dose coûte CHF 0.05.")
                val quantiteLait = readLine("Indiquez combien de doses supplémentaires vous souhaitez ajouter avec 1, 2, 3 ?: ").toInt
                val doseLait = 0.0
                if (quantiteLait >= 1 && quantiteLait <= 3) {
                }
                val prixLait = quantiteLait * 0.05
                prixBoisson += prixLait

                // Calculer la qauntité totale de lait nécessaire, avec l'ajout, pour savoir si le stock est suffisant
                val laitBaseplusDose = laitBase + doseLait
                if (stockLait >= laitBaseplusDose) {
                  println("Votre " + nomBoisson + "va être préparée")
                }
                // Mise à jour des stocks après chaque commande
                stockCafe -= cafeBase
                stockSucre -= sucreBase
                stockLait -= laitBaseplusDose
              }
            }
          }
          //Affichage du prix total de la boisson
          printf("Le prix total de votre %s est : CHF %.2f\n", nomBoisson, prixBoisson)

          //Interface de paiement
          printf("Veuillez payer le montant de CHF %.2f avec Twint en saisissant le code de paiement suivant: \n", prixBoisson)
          val codeTwint = Random.alphanumeric.filter(_.isLetterOrDigit).take(5).mkString.toUpperCase
          println(codeTwint)

          println("En attente de validation du paiement...")
          Thread.sleep(3000)

          println("Merci ! Votre paiement a été accepté.")

          println("Préparation de votre boisson...")
          Thread.sleep(5000)

          println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

          // Sortir de la boucle de choix de boisson et retourner au menu principal
          newChoixBoisson = false
          continue = true
        }

      } else if (mode == "2") {
        println("Votre mode est Admin. Vous allez entrer dans la console d'administration du distributeur.")
        println("Mode Admin")
        println("Entrez le code PIN: ")
        val pinUtilisateur = readLine()
        if (pinUtilisateur == codePIN) {
          Thread.sleep(3000)
          println("Accès autorisé.")

          afficherStocks()
          majStock()
        }
        else if (mode == "3") {
          println("Vous avez sélectionné quitter. A bientôt !")
        } else
          {
            println("Votre saisie n'est pas juste. Veuillez entrer le numéro correspondant à votre mode d'utilisation.")
          }
      }
    }
  }
}