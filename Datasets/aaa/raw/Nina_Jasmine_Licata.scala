import io.StdIn._
import util.Random

object Main {

  // Déclaration des stocks initiaux
  var stockCafe = 50 // en g
  var stockSucre = 30 // en g
  var stockLait = 0.5 // en L

  // Liste des boissons et des prix (en CHF)
  val listeBoissons = Array("Expresso", "Cappuccino", "Latte")
  val prixExpresso = 2.00
  val prixCappuccino = 2.50
  val prixLatte = Array(2.70, 3.20, 3.70)

  // Quantités d'ingrédients par boisson
  val poudreCafeExpresso = 8 // coût en g pour faire un Expresso
  val poudreCafeCappuccino = 6
  val poudreCafeLatte = Array(6, 8, 12)
  val laitCappuccino = 0.1 // en L
  val laitLatte = Array(0.12, 0.15, 0.2)

  // Quantités et prix des suppléments
  val quantiteSucre = Array(0, 5, 10, 15) // en g
  val prixSucre = Array(0.00, 0.10, 0.20, 0.30) // en CHF
  val prixDoseLaitSupplement = 0.05 // en CHF


  def main(args: Array[String]): Unit = {
    println("Nospresso Café")

    // Sélection du mode
    var quitter = false
    while (!quitter) {
      println("\nVeuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      var choixMode = readInt()
      while (choixMode < 1 || choixMode > 3) {
        println("Erreur : veuillez entrer un nombre entre 1 et 3.")
        choixMode = readInt()
      }
      // Mode client
      if (choixMode == 1) {

      // Détail des prix
        println("\nVeuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")

        // Détails des ingrédients selon la boisson et la taille
        var choixBoisson = readInt()
        while (choixBoisson < 1 || choixBoisson > 3) {
          println("Erreur : veuillez entrer un nombre entre 1 et 3.")
          choixBoisson = readInt()
        }
        var poudreCafeRequise = 0
        var laitRequis = 0.0
        var prixBoisson = 0.0

        if (choixBoisson == 1) {
          poudreCafeRequise = poudreCafeExpresso
          laitRequis = 0.0
          prixBoisson = prixExpresso

        } else if (choixBoisson == 2) {
          poudreCafeRequise = poudreCafeCappuccino
          laitRequis = laitCappuccino
          prixBoisson = prixCappuccino

        } else if (choixBoisson == 3) {
          println("\nVeuillez choisir une taille pour le Latte :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")

          var choixTailleLatte = readInt()
          while (choixTailleLatte < 1 || choixTailleLatte > 3) {
            println("Erreur : veuillez entrer un nombre entre 1 et 3.")
            choixTailleLatte = readInt()
          }

          poudreCafeRequise = poudreCafeLatte(choixTailleLatte - 1)
          laitRequis = laitLatte(choixTailleLatte - 1)
          prixBoisson = prixLatte(choixTailleLatte - 1)
        }

         // message d'erreur (ingrédients insuffisants)
        if (stockCafe < poudreCafeRequise) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        } else if (stockLait < laitRequis) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")

          // Supplément sucre
        } else {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")

          var choixSucre = readInt()
          while (choixSucre < 1 || choixSucre > 4) {
            println("Erreur : veuillez entrer un nombre entre 1 et 4.")
            choixSucre = readInt()
          }

          val quantiteSucreChoisi = quantiteSucre(choixSucre - 1)
          val prixSucreChoisi = prixSucre(choixSucre - 1)

          if (quantiteSucreChoisi > stockSucre) {
            println("Erreur : Pas assez de sucre en stock.")
          } else {

            // Supplément lait
            var laitSupplement = 0.0
            var prixLaitSupplement = 0.0
            if (choixBoisson == 2 || choixBoisson == 3) {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("1) Oui")
              println("2) Non")
              print("> ")

              var choixLaitSupplement = readInt()
              while (choixLaitSupplement < 1 || choixLaitSupplement > 2) {
                println("Erreur : veuillez entrer un nombre entre 1 et 2.")
                choixLaitSupplement = readInt()
              }

              if (choixLaitSupplement == 1) {
                println("Combien de dose ?")
                print("> ")
                var choixNombreDoses = readInt()
                while (choixNombreDoses < 1 || choixNombreDoses > 3) {
                  println("Erreur : veuillez entrer un nombre entre 1 et 3.")
                  choixNombreDoses = readInt()
                }

                laitSupplement = choixNombreDoses * 0.05
                prixLaitSupplement = choixNombreDoses * prixDoseLaitSupplement
              }
            }

            if (stockLait < laitRequis + laitSupplement) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")

              // Code twint
            } else {
              val prixTotal = prixBoisson + prixSucreChoisi + prixLaitSupplement
              println(f"Prix total : CHF $prixTotal%.2f")
              println("Veuillez payer en utilisant Twint.")
              val caracteresDispo = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
              var codeTwint = ""
              val random = new Random()
              for (_ <- 1 to 5) {
                val caractereAleatoire = caracteresDispo(random.nextInt(caracteresDispo.length))
                codeTwint = codeTwint + caractereAleatoire
              }
              println(s"Votre code de paiement est : $codeTwint")
              println("(En attente de validation du paiement...)")

              println("\nMerci ! Votre paiement a été accepté.")
              println(s"Préparation de votre boisson...")
              stockCafe = stockCafe - poudreCafeRequise
              stockLait = stockLait - (laitRequis + laitSupplement)
              stockSucre = stockSucre - quantiteSucreChoisi
              println(s"Votre ${listeBoissons(choixBoisson - 1)} est prêt ! Bonne dégustation !")
            }
          }
        }
        // Mode Admin
      } else if (choixMode == 2) {

        // Code PIN + affichage des stocks
        println("\nMode Admin")
        println("Entrez le code PIN : ")
        val pin = readLine()

        if (pin == "434343") {
          println("Accès autorisé.")
          println(s"Stocks:\nPoudre de café: $stockCafe g\nLait: ${stockLait} L\nSucre: $stockSucre g")
          println("Réapprovisionnement des stocks...")

          // Rajout de stock
          println("Poudre de café:")
          val ajoutPoudreCafe = readInt()
          println("Lait (en L):")
          val ajoutLait = readDouble()
          println("Sucre:")
          val ajoutSucre = readInt()

          stockCafe = stockCafe + ajoutPoudreCafe
          stockLait = stockLait + ajoutLait
          stockSucre = stockSucre + ajoutSucre

          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
        } else {
          println("Code PIN incorrect.")
        }

        // Mode Quitter
      } else if (choixMode == 3) {

        quitter = true
      }
    }
  }
}

