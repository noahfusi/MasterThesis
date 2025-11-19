import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    // Pour le lait, l'unité est en l. Pour le reste, l'unité est en grammes (g).
    var stockPoudreDeCafe = 50
    var stockSucre = 30
    var stockLait = 0.5

    val stockPoudreDeCafeInitial = stockPoudreDeCafe
    val stockSucreInitial = stockSucre
    val stockLaitInitial = stockLait


    var nomBoissonChoisie = ""
    var nomNiveauSucre = ""
    var nomLaitSupp = ""
    var boissonPrete = ""


    val a = true
    while (a == true) {
//      println("Nouveau stock de cafe : " + stockPoudreDeCafe)
//      println("Nouveau stock de sucre : " + stockSucre)
//      println("Nouveau stock de lait : " + stockLait)
      println()


      print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      var mode = readInt()
      while (mode != 1 && mode != 2 && mode != 3) {
        print("L'entrée clavier n'est pas une valeur autorisée.\n\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readInt()
      }

      // Mode Client
      if (mode == 1) {
        var prixBoissonChoisie = 0.00
        var prixNiveauSucre = 0.00
        var prixDoseLait = 0.00
        var prixFinal = 0.00


        var choixBoisson = 0

        while (choixBoisson != 3 && choixBoisson != 2 && choixBoisson != 1) {
          print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
          choixBoisson = readInt()
          if (choixBoisson == 1) {
            // Boisson sélectionnée : Expresso
            prixBoissonChoisie = 2.00
            stockPoudreDeCafe -= 8
            nomBoissonChoisie = "Expresso"
            boissonPrete = "Expresso"
          }
          else if (choixBoisson == 2) {
            // Boisson sélectionné : Cappuccino
            prixBoissonChoisie += 2.50
            stockPoudreDeCafe -= 6
            stockLait -= 0.1
            nomBoissonChoisie = "Cappuccino"
            boissonPrete = "Cappuccino"
          }
          else if (choixBoisson == 3) {
            // 3 Latte différents
            var choixLatte = 0
            boissonPrete = "Latte"

            while (choixLatte != 1 && choixLatte != 2 && choixLatte != 3) {
              print("1) CHF 2.70 (Petit), 2) CHF 3.20 (Moyen), 3) CHF 3.70 (Grand)\n> ")
              choixLatte = readInt()
              if (choixLatte == 1) {
                // Boisson sélectionnée : Latte (Petit)
                prixBoissonChoisie += 2.70
                stockPoudreDeCafe -= 6
                stockLait -= 0.12
                nomBoissonChoisie = "Latte (Petit)"
              }
              else if (choixLatte == 2) {
                // Boisson sélectionnée : Latte (Moyen)
                prixBoissonChoisie += 3.20
                stockPoudreDeCafe -= 8
                stockLait -= 0.15
                nomBoissonChoisie = "Latte (Moyen)"
              }
              else if (choixLatte == 3) {
                // Boisson sélectionnée : Latte (Grand)
                prixBoissonChoisie += 3.70
                stockPoudreDeCafe -= 12
                stockLait -= 0.2
                nomBoissonChoisie = "Latte (Grand)"
              }
              else {
                print("L'entrée clavier n'est pas une valeur autorisée.\n")
              }
            }
          } else {
            print("L'entrée clavier n'est pas une valeur autorisée.\n")
          }
        }

        var choixSucre = 0
        while (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4) {

          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
          choixSucre = readInt()
          if (choixSucre == 1) {
            prixNiveauSucre = prixNiveauSucre
            nomNiveauSucre = "Sans sucre"
          } else if (choixSucre == 2) {
            prixNiveauSucre = 0.10
            stockSucre -= 5
            nomNiveauSucre = "Peu (5g)"
          } else if (choixSucre == 3) {
            prixNiveauSucre += 0.20
            stockSucre -= 10
            nomNiveauSucre = "Moyen (10g)"
          } else if (choixSucre == 4) {
            prixNiveauSucre += 0.30
            stockSucre -= 15
            nomNiveauSucre = "Beaucoup (15g)"
          } else {
            print("L'entrée clavier n'est pas une valeur autorisée.\n")
          }
        }

        if (choixBoisson == 2 || choixBoisson == 3) {
          var choixLait = 0
          do {
            print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
            choixLait = readInt()

            if (choixLait == 1) {
              // Oui, dose supp
              var nbdose = 0
              do {
                print("Combien de dose ?\n> ")
                nbdose = readInt()
                if (nbdose < 1 || nbdose > 3) {
                  println("L'entrée clavier n'est pas une valeur autorisée.")
                }
              } while (nbdose < 1 || nbdose > 3)

              if (stockLait >= nbdose * 0.05) {
                stockLait -= nbdose * 0.05
                prixDoseLait = nbdose * 0.05
                nomLaitSupp = "Oui, " + nbdose + " dose(s)"
              }
            } else if (choixLait == 2) {
              // Pas de dose supp
              nomLaitSupp = "Non"
              prixDoseLait = 0.0
            } else {
              println("L'entrée clavier n'est pas une valeur autorisée.")
            }
          } while (choixLait != 1 && choixLait != 2)
        }

        // Erreur de stock
        if (stockPoudreDeCafe < 0) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockSucre < 0) {
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une quantité plus petite.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockLait < 0.0) {
          println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockPoudreDeCafe < 0 && stockLait < 0.0) {
          println("\nErreur : Quantité de lait insuffisante et quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockPoudreDeCafe < 0 && stockSucre < 0) {
          println("\nErreur : Quantité de sucre insuffisante et quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockLait < 0.0 && stockSucre < 0) {
          println("\nErreur : Quantité de sucre insuffisante et quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        } else if (stockPoudreDeCafe < 0 && stockLait < 0 && stockSucre < 0) {
          println("\nErreur : Quantité insuffisante de stock pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson.")
          stockPoudreDeCafe = stockPoudreDeCafeInitial
          stockSucre = stockSucreInitial
          stockLait = stockLaitInitial
        }



        else {
        println()
        println("Boisson sélectionée : " + nomBoissonChoisie)
        println("Niveau de sucre : " + nomNiveauSucre)
        println("Lait supplémentaire : " + nomLaitSupp)
        prixFinal = (prixBoissonChoisie + prixNiveauSucre + prixDoseLait)
        //println("Prix total : CHF " + prixBoissonChoisie + " + CHF " + prixNiveauSucre + " = CHF " + prixFinal)
        //println(f"Prix total : CHF $prixBoissonChoisie%.2f + CHF $prixNiveauSucre%.2f + CHF $prixDoseLait%.2f = CHF $prixFinal%.2f")
        printf("Prix total : %.2f CHF ", prixBoissonChoisie)
        printf("+ %.2f CHF ", prixNiveauSucre)
        //printf("+ %.2f CHF ", prixDoseLait)
        printf("= %.2f CHF ", prixFinal)
        println("\n")


        // Paiement
        println("Veuillez payer en utilisant Twint.")
        print("Votre code de paiement est : ")
        for (i <- 1 to 5) { // fais 5 fois car code de longeur 5
          var rand = (math.random() * 36).toInt // On prend un nombre aléatoire entre 0 et 36 (0-9 + A-Z) (0à9 = 10 "cases" et AàZ = 26 cases)
          if (rand >= 10) { // Si le nombre aléatoire est entre 10 et 36, on a donc une lettre
            rand -= 10 // on lui enlève 10 -> 0 et 26
            print(('A' + rand).toChar) // 'A' quand on additionne, on décalle du nombre rand depuis la lettre A
            // ('A' + rand) ca c'est un nombre en ascii, on le transforme en char et on l' imprime
          } else { // si on est la, on print direct parce qu'on a un chiffre de 0 a 9
            print(rand)
          }
        }

        print("\n")

        // Attente de 3 secondes avant validation du paiement
        println("(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println()
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre " + boissonPrete + " est prêt ! Bonne dégustation !")
      }
    } // Fin Mode Client



      // Mode Admin
      else if (mode == 2) {
          var PIN = 0
          println("Mode Admin")
          while (PIN != 434343) {
            print("Entrez le code PIN : ")
            PIN = readInt()
            if (PIN == 434343) {
              println("Accès autorisé.")
              //          for (i <- 1 to 6) {
              //            print("*")
              //          }
            } else {
              println("Code PIN erroné")
            }
          }

          println()
          println("Stocks :\n\tPoudre de café: " + stockPoudreDeCafe + " g")
          println(f"\tLait\t\t  : $stockLait%.2fL")
          println("\tSucre\t\t  : " + stockSucre + " g")
          println("Réapprovionnement des stocks...\nAjout :\n")
          var ajoutPoudreCafe = 0
          do {
            print("\tPoudre de café: ")
            ajoutPoudreCafe = readInt()
            if (ajoutPoudreCafe < 0) {
              println("L'entrée clavier n'est pas une valeur autorisée.")
            }
          } while (ajoutPoudreCafe < 0)
          stockPoudreDeCafe += ajoutPoudreCafe

          var ajoutLait = 0.0
          do {
            print("\tLait\t\t  : ")
            ajoutLait = readDouble()
            if (ajoutLait < 0.0) {
              println("L'entrée clavier n'est pas une valeur autorisée.")
            }
          } while (ajoutLait <0)
          stockLait += ajoutLait

          var ajoutSucre = 0
          do {
            print("\tSucre\t\t  : ")
            ajoutSucre = readInt()
            if (ajoutSucre < 0) {
              println("L'entrée clavier n'est pas une valeur autorisée.")
            }
          } while (ajoutSucre < 0)
          stockSucre += ajoutSucre

          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")


        } else if (mode == 3) {
          println("Le programme va quitter...")
        } else {
          print("L'entrée clavier n'est pas une valeur autorisée.")
        }

//        println()
//        println("Stock de poudre de café : " + stockPoudreDeCafe)
//        println("Stock de sucre : " + stockSucre)
//        printf("Stock de lait : %.2f\n", stockLait)
//        println()

    }
  }
} // Fin du programme
