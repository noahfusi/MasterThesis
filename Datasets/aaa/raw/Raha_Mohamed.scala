import io.StdIn._
import io.StdIn.readLine
import scala.util.Random
import math._

object NospressoCafe {
  def main(args: Array[String]): Unit = {
    // Initialisation des variables de stock
    var stockCafe = 50
    var stockSucre = 30
    var stockLait = 0.500 // conversion en litre
    val pinAdmin = "434343"


    // Variable pour contrôler la boucle principale
    var running = true

    // Boucle principale
    while (running) {
      // Ici on affiche le menu principal
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val input = io.StdIn.readLine()

      if (input == "1") {
        // Le Mode Client
        println("Mode Client")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")

        val boisson = io.StdIn.readLine()
        var prixBase = 0.0

        if (boisson == "1") {
          println("Vous avez choisi : Expresso")
          prixBase = 2.00
          stockCafe -= 8
        } else if (boisson == "2") {
          println("Vous avez choisi : Cappuccino")
          prixBase = 2.50
          stockCafe -= 6
          stockLait -= 0.1
        } else if (boisson == "3") {
          println("Vous avez choisi : Latte")
          println("Choisissez la taille :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")

          val taille = io.StdIn.readLine()

          if (taille == "1") {
            prixBase = 2.70
            stockCafe -= 6
            stockLait -= 0.12
          } else if (taille == "2") {
            prixBase = 3.20
            stockCafe -= 8
            stockLait -= 0.15
          } else if (taille == "3") {
            prixBase = 3.70
            stockCafe -= 12
            stockLait -= 0.2
          } else {
            println("Choix invalide.")
          }
        }

        // Personnalisation
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre - CHF 0.0")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")

        val sucreChoix = io.StdIn.readLine()
        var sucrePrix = 0.0
        if (sucreChoix == "1" && stockSucre >= 0) {
          stockSucre = 0
          sucrePrix = 0.00

        } else if (sucreChoix == "2" && stockSucre >= 5) {
          stockSucre -= 5
          sucrePrix = 0.10
        } else if (sucreChoix == "3" && stockSucre >= 10) {
          stockSucre -= 10
          sucrePrix = 0.20
        } else if (sucreChoix == "4" && stockSucre >= 15) {
          stockSucre -= 15
          sucrePrix = 0.30
        } else {
          println("Erreur, quantité insuffisante de sucre, veuillez refaire un choix")
        }

        // Ajout de lait supplémentaire
        if (boisson == "2" || boisson == "3") {
          println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          print("> ")
          val laitChoix = io.StdIn.readLine()
          if (boisson == "1") {
            println("Aucun lait supplémentaire ajouté")
          }



          else if (laitChoix == "1") {
            var dosesLait = 0
            var laitSupplementPrix:Double =0.0
            println("Combien de doses ? (1 dose = 0.05l, max 3 doses)")
            print("> ")
            val dosesInput = io.StdIn.readLine()
            dosesLait = dosesInput.toInt

            if (dosesLait > 0 && dosesLait <= 3) {
              laitSupplementPrix = dosesLait * 0.05
              if (stockLait >= dosesLait * 0.05) {
                stockLait -= dosesLait * 0.05 // 0.05l par dose
              } else {
                println("Erreur : stock de lait insuffisant")
              }
            } else {
              println("Erreur : Nombre de doses invalide. Aucune dose ajoutée.")
              dosesLait = 0
            }
          }
        }



        var laitSupplementPrix:Double= 0.0


        val prixFinal = prixBase + sucrePrix + laitSupplementPrix

        // Paiement
        println("Prix total : CHF " + prixFinal)
        println("Veuillez payer en utilisant Twint.")
        val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        val taille = 6 // Nombre total de caractères dans le code
        var codePaiement = "" // Variable pour accumuler les caractères

        for (i <- 1 to taille) {
          if (i % 2 == 0) {

            codePaiement += caracteres(Random.nextInt(10) + 52)
          } else {

            codePaiement += caracteres(Random.nextInt(52))
          }
        }

        println("Votre code de paiement est : " + codePaiement)
        println("(En attente de paiement...)")
        Thread.sleep(5000)
        println("Paiement confirmé.")

        // Préparation de la boisson
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("(Votre boisson est prête ! Bonne dégustation !)")
      } else if (input == "2") {
        // Mode Admin
        println("Mode Admin")
        print("Entrez le code PIN : ")
        val pin = io.StdIn.readLine()

        if (pin == pinAdmin) {
          println("Accès autorisé.")
          println("Stocks actuels : Poudre de café : " + stockCafe +" g, Sucre : " + stockSucre +" g, Lait : " + stockLait +" l")
          println("Entrez les quantités à ajouter :")
          print("Poudre de café (g) : ")
          val ajoutCafe = io.StdIn.readInt()
          print("Sucre (g) : ")
          val ajoutSucre = io.StdIn.readInt()
          print("Lait (l) : ")
          val ajoutLait = io.StdIn.readDouble()

          stockCafe += ajoutCafe
          stockSucre += ajoutSucre
          stockLait += ajoutLait

          println("Stocks mis à jour : Poudre de café : " + stockCafe +" g, Sucre : " + stockSucre +" g, Lait : " + stockLait +" l")
        } else {
          println("Code PIN incorrect.")
        }
      } else if (input == "3") {
        // Quitter le programme
        println("Merci d'avoir utilisé Nospresso Café !")
        running = false
      } else {
        println("Choix invalide. Veuillez essayer de nouveau.")
      }
    }
  }
}