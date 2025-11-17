object Main {
  def main(args: Array[String]): Unit = {

    import io.StdIn._
    import math._
    import scala.util.Random


    // modalités
    val client = 1
    val admin = 2
    val quitter = 3
    var mode = 0

    // stock
    var poudreCafeStock = 50 // grammes
    var sucreStock = 30 //grammes
    var laitStock = 500 //millilitres

    var stockSuffisantCafe = true
    var stockSuffisantLait = true
    var stockSuffisantSucre = true

    //éléments nécessaires
    var poudreCafeEx = 8

    var poudreCafeCap = 6
    var laitCap = 100

    var poudreCafeLat1 = 6
    var laitLat1 = 120
    var poudreCafeLat2 = 8
    var laitLat2 = 150
    var poudreCafeLat3 = 12
    var laitLat3 = 200


    // itération principale
    do {
      // bienvenue
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      mode = readInt()

      if (mode != 1 && mode != 2 && mode != 3) {
        println("Le mode sélectionné n'est pas valable.")
        println("Veuillez entrer un nombre valable: " + client + ", " + admin + " ou " + quitter + ".")
      }


      val expresso = 1
      val cappuccino = 2
      val latte = 3
      var boisson = 0
      var sucreSup = 0
      var tailleLatte = 0


      if (mode == client) {

        do {
          println("=====Mode Client=====")
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          println(">")
          boisson = readInt()
          if (boisson != 1 && boisson != 2 && boisson != 3) {
            println("La boisson sélectionnée n'est pas valable.")
            println("Veuillez entrer une boisson valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
          }
        } while (boisson != 1 && boisson != 2 && boisson != 3)


          if (boisson == latte) {
            do {
              println("Quelle taille voulez-vous?")
              println("1) Petit")
              println("2) Moyen")
              println("3) Grand")
              println(">")
              tailleLatte = readInt()
              if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
                println("La taille sélectionnée n'est pas valable.")
                println("Veuillez entrer une taille valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
              }
            }while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
          }


        if (boisson == expresso || boisson == cappuccino || boisson == latte) {
          do {
            println("Souhaitez-vous ajouter du sucre?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println(">")
            sucreSup = readInt()
            if (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4) {
              println("La quantité sélectionnée n'est pas valable.")
              println("Veuillez entrer une quantité valable: " + 1 + ", " + 2 + ", " + 3 + " ou " + 4 + ".")
            }
          }while (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4)
        }


        var laitSup = 0
        var doseLaitSup = 0
        if (boisson == cappuccino || boisson == latte) {
          do {
            println("Souhaitez-vous ajouter du lait en supplément? (Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            println(">")
            laitSup = readInt()
            if (laitSup != 1 && laitSup != 2) {
              println("La touche sélectionnée n'est pas valable.")
              println("Veuillez entrer une touche valable: " + 1 + " ou " + 2 + ".")
            }
          }while (laitSup != 1 && laitSup != 2)
        }


        if (laitSup == 1) {
          println("Combien de doses?")
          println(">")
          doseLaitSup = readInt()
          while (doseLaitSup < 1 || doseLaitSup > 3) {
            println("La dose sélectionnée n'est pas valable.")
            println("Veuillez entrer une dose valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
            doseLaitSup = readInt()
          }
        }


        // vérification stock
        if (boisson == 1) {
          if (poudreCafeStock > poudreCafeEx) {
            stockSuffisantCafe = true
          } else {
            stockSuffisantCafe = false
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
          if (sucreStock > ((sucreSup - 1) * 5)) {
            stockSuffisantSucre = true
          } else {
            stockSuffisantSucre = false
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
        }
        else if (boisson == 2) {
          if (poudreCafeStock > poudreCafeCap) {
            stockSuffisantCafe = true
          } else {
            stockSuffisantCafe = false
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
          if (laitStock > ((laitCap) + (doseLaitSup * 50))) {
            stockSuffisantLait = true
          } else {
            stockSuffisantLait = false
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
          if (sucreStock > ((sucreSup - 1) * 5)) {
            stockSuffisantSucre = true
          } else {
            stockSuffisantSucre = false
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          }
        }

        if (boisson == 3) {
          if (tailleLatte == 1) {
            if (poudreCafeStock > poudreCafeLat1) {
              stockSuffisantCafe = true
            } else {
              stockSuffisantCafe = false
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (laitStock > ((laitLat1) + (doseLaitSup * 50))) {
              stockSuffisantLait = true
            } else {
              stockSuffisantLait = false
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (sucreStock > ((sucreSup - 1) * 5)) {
              stockSuffisantSucre = true
            } else {
              stockSuffisantSucre = false
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          } else if (tailleLatte == 2) {
            if (poudreCafeStock > poudreCafeLat2) {
              stockSuffisantCafe = true
            } else {
              stockSuffisantCafe = false
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (laitStock > ((laitLat2) + (doseLaitSup * 50))) {
              stockSuffisantLait = true
            } else {
              stockSuffisantLait = false
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (sucreStock > ((sucreSup - 1) * 5)) {
              stockSuffisantSucre = true
            } else {
              stockSuffisantSucre = false
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          } else if (tailleLatte == 3) {
            if (poudreCafeStock > poudreCafeLat3) {
              stockSuffisantCafe = true
            } else {
              stockSuffisantCafe = false
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (laitStock > ((laitLat3) + (doseLaitSup * 50))) {
              stockSuffisantLait = true
            } else {
              stockSuffisantLait = false
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if (sucreStock > ((sucreSup - 1) * 5)) {
              stockSuffisantSucre = true
            } else {
              stockSuffisantSucre = false
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          }

        }

        // paiement
        if (stockSuffisantCafe && stockSuffisantLait && stockSuffisantSucre) {
          var prix = 0.0
          if (boisson == expresso) {
            prix = 2
          }
          else if (boisson == cappuccino) {
            prix = 2.50
          }
          else if (tailleLatte == 1) {
            prix = 2.70
          }
          else if (tailleLatte == 2) {
            prix = 3.20
          }
          else if (tailleLatte == 3) {
            prix = 3.70
          }

          val prixLait = (doseLaitSup) * 0.05
          val prixSucre = (sucreSup - 1) * 0.10
          val prixTotal = prix + prixSucre + prixLait


          println("Prix total: " + "CHF " + prix.toFloat + " +" + " CHF " + prixSucre.toFloat + " +" + " CHF " + prixLait.toFloat + " = CHF " + prixTotal.toFloat + ".")
          val twintCode = Random.alphanumeric.take(5).mkString.toUpperCase() //méthode trouvé dans la librairie Scala
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + twintCode + ".")
          println("En attente de validation du paiement...")
          Thread.sleep(3000)
          println("Merci! Votre paiement a été accepté.")


          // deductionm stock
          if (boisson == expresso) {
            poudreCafeStock -= poudreCafeEx
          }
          if (boisson == cappuccino) {
            poudreCafeStock -= poudreCafeCap
            laitStock -= laitCap
          }
          if (boisson == latte && tailleLatte == 1) {
            poudreCafeStock -= poudreCafeLat1
            laitStock -= laitLat1
          }
          if (boisson == latte && tailleLatte == 2) {
            poudreCafeStock -= poudreCafeLat2
            laitStock -= laitLat2
          }
          if (boisson == latte && tailleLatte == 3) {
            poudreCafeStock -= poudreCafeLat3
            laitStock -= laitLat3
          }
          if (sucreSup == 1) {
            sucreStock -= 0
          }
          if (sucreSup == 2) {
            sucreStock -= 5
          }
          if (sucreSup == 3) {
            sucreStock -= 10
          }
          if (sucreSup == 4) {
            sucreStock -= 15
          }
          if (doseLaitSup == 1) {
            laitStock -= 50
          }
          if (doseLaitSup == 2) {
            laitStock -= 100
          }
          if (doseLaitSup == 3) {
            laitStock -= 150
          }

          //preparation boisson
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          if (boisson == 1) {
            println("Votre Expresso est prêt! Bonne dégustation!")
          }
          else if (boisson == 2) {
            println("Votre Cappuccino est prêt! Bonne dégustation!")
          }
          else if (boisson == 3) {
            println("Votre Latte est prêt! Bonne dégustation!")
          }

        }
        mode = quitter
      }

      if (mode == admin) {
        var pin = 0
        println("==========Mode Admin==========")
        pin = readLine("Entrez le code PIN:").toInt

        if (pin == 434343) {
          println("Accès autorisé.")
          println("Stocks:")
          println("Poudre de café : " + poudreCafeStock)
          println("Lait : " + laitStock)
          println("Sucre : " + sucreStock)

          print("Poudre de café: ")
          poudreCafeStock += readInt()
          print("Lait: ")
          laitStock += readInt()
          print("Sucre: ")
          sucreStock += readInt()

          println("Réapprovisionnement des stocks...")
          Thread.sleep(1000)
          println("Stocks mis à jour:")
          println("Poudre de café : " + poudreCafeStock)
          println("Lait : " + laitStock)
          println("Sucre : " + sucreStock)
          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
          Thread.sleep(1000)

        } else if (pin != 434343 && mode == 2) {
          println("Le PIN inséré n'est pas valide!")
        }
        mode = quitter
      }
      Thread.sleep(1000)
    } while (mode != client && mode != admin)


  }
}