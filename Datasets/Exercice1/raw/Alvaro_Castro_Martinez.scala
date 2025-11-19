
    import io.StdIn._
    import scala.util.Random
    object Main {
      def main(args: Array[String]): Unit = {

        def demanderOptionValide(optionsValides: Set[String]): String = {
          var saisie = ""
          do {
            saisie = readLine(">")
            if (!optionsValides.contains(saisie)) {
              println("Option invalide. Veuillez réessayer.")
            }
          } while (!optionsValides.contains(saisie))
          saisie
        }

        //Stock Intitial
        var stockSucre = 30
        var stockCafe = 50
        var stockLait = 500

        var prix = 0.0

        var continuer = true


        while (continuer) {
          println("")
          println("")
          println("Nospresso Café")
          println("veuillez sélectioner votre mode: ")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter")
          val choix = demanderOptionValide(
            Set("1", "2", "3"))

          if (choix == "3") {
            continuer = false
            println("vous quittez le programme.")

          } else if (choix == "1") {
            println("Veuillez sélectionner une boisson : ")
            println("1) Expresso - CHF 2.00")
            println("2) Capuccino - CHF 2.50")
            println("3) Latte Petit - CHF 2.70")
            println("4) Latte Moyen - CHF 3.20")
            println("5) Latte Grand - CHF 3.70")
            val boisson = demanderOptionValide(Set("1", "2", "3", "4", "5")
            )

            var sucreNec = 0
            var cafeNec = 0
            var laitNec = 0
            var prixS = 0.0

            if (boisson == "1") {
              cafeNec = 8
              laitNec = 0
              prix = 2.0
            } else if (boisson == "2") {
              cafeNec = 6
              laitNec = 100
              prix = 2.5
            } else if (boisson == "3") {
              cafeNec = 6
              laitNec = 120
              prix = 2.7
            } else if (boisson == "4") {
              cafeNec = 8
              laitNec = 150
              prix = 3.2
            } else if (boisson == "5") {
              cafeNec = 12
              laitNec = 200
              prix = 3.7
            }

            println("Souhaitez-vous ajouter du sucre ? ")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            val sucre = demanderOptionValide(Set("1", "2", "3", "4"))
            if (sucre == "1") {
              sucreNec = 0
            } else if (sucre == "2") {
              sucreNec = 5
              prixS = 0.1
            } else if (sucre == "3") {
              sucreNec = 10
              prixS = 0.2
            } else if (sucre == "4") {
              sucreNec = 15
              prixS = 0.3
            }


            if (boisson == "1")
              println("")
            if (boisson == "2" || boisson == "3" || boisson == "4" || boisson == "5") {
              println("Souhaitez-vous ajouter du lait en supplément ? ")
              println("(Disponible uniquement pour Cappuccino et Latte)")
              println("1) Oui")
              println("2) Non")

              val laitSup = demanderOptionValide(Set("1", "2"))
              if (laitSup == "1") {
                println("Combien de dose ? ")
                println("1) 1 dose (50ml)")
                println("2) 2 doses(100ml)")
                println("3) 3 doses (150ml)")
                val lait = demanderOptionValide(Set("1", "2", "3"))
                if (lait == "1") {
                  laitNec += 50
                } else if (lait == "2") {
                  laitNec += 100
                } else if (lait == "3") {
                  laitNec += 150
                }
              }
            }


            if (stockSucre <= sucreNec) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              if (stockSucre >= 5) {
                println("Veuillez ajouter moins de sucre.")
              }else println("Nous ne pouvons malheureusement pas rajouter de sucre dans votre boisson")
            }else if (stockLait <= laitNec) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }else if (stockCafe <= cafeNec) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (stockSucre >= sucreNec && stockCafe >= cafeNec && stockLait >= laitNec) {
              val prixT = prixS + prix
              println("prix total: CHF " + prix + " + CHF " + prixS + " =  CHF " + prixT)
              println("")
              println("Veuillez payer en utilisant Twint.")

              val codeTwint = Random.alphanumeric.take(5).mkString
              println("Votre code de paiement est: " + codeTwint)
              println("(En attente de validation du paiement...)")

              Thread.sleep(3000)
              println("")
              println("Merci! Votre paiement a été accepté.")


              stockSucre -= sucreNec
              stockCafe -= cafeNec
              stockLait -= laitNec
              //println("Stocks restants: ")
              //println("Café: " + stockCafe)
              //println("Sucre: " + stockSucre)
              //println("Lait: " + stockLait)
              println("Préparation de votre boisson...")
              Thread.sleep(2000)
              if (boisson == "1") {
                println("Votre Expresso est prêt ! Bonne dégustation !")
              } else if (boisson == "2") {
                println("Votre Capuccino est prêt ! Bonne dégustation !")
              } else if (boisson == "3" || boisson == "4" || boisson == "5") {
                println("Votre Latte est prêt ! Bonne dégustation !")
              }
            }
          }



            else if (choix == "2") {

            var codePIN = ""
            while (codePIN != "434343" && codePIN.length != 6) {
              println("Veuillez entrer le code PIN")
              codePIN = readLine()
              if (codePIN != "434343" && codePIN.length != 6) {
                println("Accès refusé")
                println("Veuillez réessayer")
              }
            }
            println("Accès autorisé.")
            println("Affichage des stocks: ")
            println("Sucre: " + stockSucre + " g")
            println("café: " + stockCafe + " g")
            println("Lait: " + stockLait + " ml")
            println("")
            println("Réapprovisionnement des socks...")
            println("Ajout: ")
            println("Sucre: ")
            val sucreAjoute = readLine().toInt
            stockSucre += sucreAjoute
            println("Poudre de café: ")
            val cafeAjoute = readLine().toInt
            stockCafe += cafeAjoute
            println("Lait: ")
            val laitAjoute = readLine().toInt
            stockLait += laitAjoute
            println("Niveaux de stock mis à jour.")
            println("Affichage des stocks: ")
            println("Sucre: " + stockSucre + " g")
            println("café: " + stockCafe + " g")
            println("Lait: " + stockLait + " ml")
            println("Retour au menu principal...")

          }
          }
        }
      }







































