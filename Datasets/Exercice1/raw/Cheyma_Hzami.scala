import scala.io.StdIn.readLine
import scala.util.Random

object Nospresso {

  // Stock initial
  var stockCafe = 50.0 // en grammes
  var stockSucre = 30.0 // en grammes
  var stockLait = 0.5 // en litres

  // Code PIN pour le mode admin
  val adminPin = "434343"

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      // Menu principal
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val choice = readLine()

      if (choice == "1") {
        // Mode Client
        println("\n--- Mode Client ---")
        println("Stocks actuels :")
        println("Café : " + stockCafe + "g")
        println("Sucre : " + stockSucre + "g")
        println("Lait : " + stockLait + "L")
        println("\nVeuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        val choixBoisson = readLine()

        var prixBoisson = 0.0
        var cafeUtilise = 0.0
        var laitUtilise = 0.0
        var nomBoisson = ""
        var prixSucre = 0.0
        var prixLaitSupplementaire = 0.0
        var total = 0.0

        // Préparation boissons
        if (choixBoisson == "1") {
          cafeUtilise = 8.0
          if (stockCafe >= cafeUtilise) {
            prixBoisson = 2.00
            nomBoisson = "Expresso"
            stockCafe -= cafeUtilise
          } else {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          }
        } else if (choixBoisson == "2") {
          cafeUtilise = 6.0
          laitUtilise = 0.1
          if (stockCafe < cafeUtilise) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
          } else if (stockLait < laitUtilise) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          } else {
            prixBoisson = 2.50
            nomBoisson = "Cappuccino"
            stockCafe -= cafeUtilise
            stockLait -= laitUtilise

            //supplément de lait
            println("\nSouhaitez-vous ajouter un supplément de lait ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            val reponseLait = readLine()
            if (reponseLait == "1") {
              println("Combien de doses de lait souhaitez-vous ajouter ? (1 dose = 0.05 litre)")
              print("> ")
              val dosesLait = readLine().toInt
              val laitSupplementaire = dosesLait * 0.05
              if (stockLait >= laitSupplementaire) {
                stockLait -= laitSupplementaire
                prixLaitSupplementaire = dosesLait * 0.10 // 0.10 CHF par dose supplémentaire
                println("Vous avez ajouté " + dosesLait + " dose(s) de lait.")
              } else {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
              }
            }
          }
        } else if (choixBoisson == "3") {
          println("Veuillez choisir la taille :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          val taille = readLine()
          if (taille == "1") {
            cafeUtilise = 6.0
            laitUtilise = 0.12
            prixBoisson = 2.70
            nomBoisson = "Petit Latte"
          } else if (taille == "2") {
            cafeUtilise = 8.0
            laitUtilise = 0.15
            prixBoisson = 3.20
            nomBoisson = "Moyen Latte"
          } else if (taille == "3") {
            cafeUtilise = 12.0
            laitUtilise = 0.2
            prixBoisson = 3.70
            nomBoisson = "Grand Latte"
          }

          if (stockCafe < cafeUtilise) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          } else if (stockLait < laitUtilise) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson")
          } else {
            stockCafe -= cafeUtilise
            stockLait -= laitUtilise

            //supplément de lait
            println("\nSouhaitez-vous ajouter un supplément de lait ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            val reponseLait = readLine()
            if (reponseLait == "1") {
              println("Combien de doses de lait souhaitez-vous ajouter ? (1 dose = 0.05 litre)")
              print("> ")
              val dosesLait = readLine().toInt
              val laitSupplementaire = dosesLait * 0.05
              if (stockLait >= laitSupplementaire) {
                stockLait -= laitSupplementaire
                prixLaitSupplementaire = dosesLait * 0.10 // 0.10 CHF par dose supplémentaire
                println("Vous avez ajouté " + dosesLait + " dose(s) de lait.")
              } else {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
              }
            }
          }
        }

        if (nomBoisson != "") {
          println("\nSouhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          val choixSucre = readLine()

          var sucreUtilise = 0.0
          if (choixSucre == "2") {
            sucreUtilise = 5.0
            prixSucre = 0.10
          } else if (choixSucre == "3") {
            sucreUtilise = 10.0
            prixSucre = 0.20
          } else if (choixSucre == "4") {
            sucreUtilise = 15.0
            prixSucre = 0.30
          }

          if (stockSucre < sucreUtilise) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
          } else {
            stockSucre -= sucreUtilise
            total = prixBoisson + prixSucre + prixLaitSupplementaire

            // détail avant paiement
            println("\nVotre commande :")
            println(nomBoisson + " - CHF " + prixBoisson)

            if (prixLaitSupplementaire > 0) {
              println("Supplément lait - CHF " + prixLaitSupplementaire)
            }
            if (prixSucre > 0) {
              println("Sucre - CHF " + prixSucre)
            }

            println("Prix total : CHF " + total)

            // code TWINT
            var twintCode = ""
            var i = 0
            val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            while (i < 5) {
              val randomIndex = Random.nextInt(caracteres.length)
              twintCode += caracteres(randomIndex)
              i += 1
            }

            println("\nVeuillez payer en utilisant Twint. Votre code de paiement est : " + twintCode)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("\nMerci! Votre paiement a été accepté")
            println("\nPréparation de votre boisson...")
            println("[...]")
            println("Votre " + nomBoisson + " est prêt! Bonne dégustation!")
          }
        }
      } else if (choice == "2") {
        // Mode Admin
        println("\n--- Mode Admin ---")
        print("Veuillez entrer le code PIN : ")
        val inputPin = readLine()

        if (inputPin == adminPin) {
          println("Code PIN valide. Accès au mode admin.")
          println("Stock actuel :")
          println("Café : " + stockCafe + "g")
          println("Sucre : " + stockSucre + "g")
          println("Lait : " + stockLait + "L")

          println("\nSouhaitez-vous recharger les stocks ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          val recharger = readLine()

          if (recharger == "1") {
            println("\nQuel stock voulez-vous recharger ?")
            println("1) Café")
            println("2) Sucre")
            println("3) Lait")
            print("> ")
            val choixRecharge = readLine()

            if (choixRecharge == "1") {
              println("Combien de grammes de café souhaitez-vous ajouter ?")
              val qteCafe = readLine().toDouble
              stockCafe += qteCafe
            } else if (choixRecharge == "2") {
              println("Combien de grammes de sucre souhaitez-vous ajouter ?")
              val qteSucre = readLine().toDouble
              stockSucre += qteSucre
            } else if (choixRecharge == "3") {
              println("Combien de litres de lait souhaitez-vous ajouter ?")
              val qteLait = readLine().toDouble
              stockLait += qteLait
            }
          }
        } else {
          println("Code PIN invalide.")
        }
      } else if (choice == "3") {
        // Quitter
        println("Merci d'avoir utilisé Nospresso Café. À bientôt!")
        running = false
      }
    }
  }
}
