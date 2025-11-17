import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {

    var nomBoissonChoisie = ""
    var nomNiveauSucre = ""
    var nomLaitSupp = ""
    var boissonPrete = ""

    val nbMachines = 5

    val machinesId = new Array[Int](nbMachines)

    for (i <- 0 until nbMachines) machinesId(i) = i

    val coffeeStocks = new Array[Int](nbMachines)
    val sugarStocks = new Array[Int](nbMachines)
    val milkStocks = new Array[Int](nbMachines)

    val machinesPins = new Array[String](nbMachines)

    //    Initialisation des pins à 434343
    for (i <- 0 until nbMachines) machinesPins(i) = "434343"

    //    Initialisation des stocks des machines
    for (i <- 0 until nbMachines) {
      coffeeStocks(i) = 50
      sugarStocks(i) = 30
      milkStocks(i) = 500
    }

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var PIN = 0
      var attemptsLeft = 3
      val machinePIN = machinePins(machineId).toInt
      print("Entrez le code PIN :\n")
      while (attemptsLeft > 0) {
        //print("Entrez le code PIN :\n> ")
        print("> ")
        PIN = readInt()
        if (PIN == machinePIN) {
          println("Accès accordé.")
          return true
        }
        else {
          attemptsLeft -= 1
          println("Code PIN incorrect. " + attemptsLeft + " tentative(s) restante(s).")
        }
      }
      println("\nTrop de tentatives échouées. Fin du programme.")
      false
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var newPIN = "0"
      var containsOnlyDigits = false;
      //    Vérifie que le code PIN contient exactement 6 chiffres
      while (newPIN.length != 6 || !containsOnlyDigits) {
        print("Entrez un nouveau code PIN à 6 chiffres > ")
        newPIN = readLine()
        containsOnlyDigits = true
        for (char <- newPIN) {
          if (!char.isDigit) containsOnlyDigits = false
        }
      }
      machinePins(machineId) = newPIN
      println("\nLe code PIN a été modifié avec succès.\nRetour au menu principal...")
    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println()
      println("Niveaux de stock actuels :\n\tPoudre de café : " + coffeeStocks(machineId) + " g")
      println("\tSucre : " + sugarStocks(machineId) + " g")
      printf("\tLait : %.2f L\n", milkStocks(machineId).toDouble / 1000)

      println("\nEntrez les quantités à ajouter :")
      var ajoutPoudreCafe = 0
      do {
        print("\tPoudre de café > ")
        ajoutPoudreCafe = readInt()
        if (ajoutPoudreCafe < 0) {
          println("L'entrée clavier n'est pas une valeur autorisée.")
        }
      } while (ajoutPoudreCafe < 0)
      coffeeStocks(machineId) += ajoutPoudreCafe

      var ajoutSucre = 0
      do {
        print("\tSucre > ")
        ajoutSucre = readInt()
        if (ajoutSucre < 0) {
          println("L'entrée clavier n'est pas une valeur autorisée.")
        }
      } while (ajoutSucre < 0)
      sugarStocks(machineId) += ajoutSucre

      var ajoutLait = 0
      do {
        print("\tLait > ")
        ajoutLait = (readDouble() * 1000).toInt
        if (ajoutLait < 0) {
          println("L'entrée clavier n'est pas une valeur autorisée.")
        }
      } while (ajoutLait < 0)
      milkStocks(machineId) += ajoutLait

      println("Les stocks ont été mis à jour avec succès.")
      println("Retour au menu principal...")
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      val stockPoudreDeCafeInitial = coffeeStocks(machineId)
      val stockSucreInitial = sugarStocks(machineId)
      val stockLaitInitial = milkStocks(machineId)

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
          coffeeStocks(machineId) -= 8
          nomBoissonChoisie = "Expresso"
          boissonPrete = "Expresso"
        }
        else if (choixBoisson == 2) {
          // Boisson sélectionné : Cappuccino
          prixBoissonChoisie += 2.50
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 100
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
              coffeeStocks(machineId) -= 6
              milkStocks(machineId) -= 120
              nomBoissonChoisie = "Latte (Petit)"
            }
            else if (choixLatte == 2) {
              // Boisson sélectionnée : Latte (Moyen)
              prixBoissonChoisie += 3.20
              coffeeStocks(machineId) -= 8
              milkStocks(machineId) -= 150
              nomBoissonChoisie = "Latte (Moyen)"
            }
            else if (choixLatte == 3) {
              // Boisson sélectionnée : Latte (Grand)
              prixBoissonChoisie += 3.70
              coffeeStocks(machineId) -= 12
              milkStocks(machineId) -= 200
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
          sugarStocks(machineId) -= 5
          nomNiveauSucre = "Peu (5g)"
        } else if (choixSucre == 3) {
          prixNiveauSucre += 0.20
          sugarStocks(machineId) -= 10
          nomNiveauSucre = "Moyen (10g)"
        } else if (choixSucre == 4) {
          prixNiveauSucre += 0.30
          sugarStocks(machineId) -= 15
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

            if (milkStocks(machineId) >= nbdose * 50) {
              milkStocks(machineId) -= nbdose * 50
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
      if (coffeeStocks(machineId) < 0) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (sugarStocks(machineId) < 0) {
        println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une quantité plus petite. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (milkStocks(machineId) < 0) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (coffeeStocks(machineId) < 0 && milkStocks(machineId) < 0) {
        println("\nErreur : Quantité de lait insuffisante et quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (coffeeStocks(machineId) < 0 && sugarStocks(machineId) < 0) {
        println("\nErreur : Quantité de sucre insuffisante et quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (milkStocks(machineId) < 0 && sugarStocks(machineId) < 0) {
        println("\nErreur : Quantité de sucre insuffisante et quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
      } else if (coffeeStocks(machineId) < 0 && milkStocks(machineId) < 0 && sugarStocks(machineId) < 0) {
        println("\nErreur : Quantité insuffisante de stock pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson. Vous pouvez également essayer une autre machine.")
        coffeeStocks(machineId) = stockPoudreDeCafeInitial
        sugarStocks(machineId) = stockSucreInitial
        milkStocks(machineId) = stockLaitInitial
        false
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
        true
      }
    }


    while (true) {

      print("\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      var mode = readInt()
      while (mode != 1 && mode != 2 && mode != 3) {
        print("L'entrée clavier n'est pas une valeur autorisée.\n\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readInt()
      }

      if (mode == 3) {
        println("Fin du programme.")
        return
      }

      print("\t\tNospresso Café\nVeuillez sélectionner une machine (1-5) > ")
      var machineChoice = readInt() - 1
      while (machineChoice < 0 || machineChoice >= nbMachines) {
        print("Veuillez sélectionner une machine (1-5) > ")
        machineChoice = readInt() - 1
      }

      if (mode == 1) {
        serveClient(machineChoice, coffeeStocks, sugarStocks, milkStocks)
      }

      else if (mode == 2) {
        if(!validatePin(machineChoice, machinesPins)) return
        print("\t\tMode admin\nQue voulez-vous faire ? :\n1) Ajouter du stock\n2) Modifier le PIN\n3) Quitter\n> ")
        var choice = readInt()
        while (choice != 1 && choice != 2 && choice != 3) {
          print("L'entrée clavier n'est pas une valeur autorisée.\n\nQue voulez-vous faire ? :\n1) Ajouter du stock\n2) Modifier le PIN\n3) Quitter\n> ")
          choice = readInt()
        }
        if (choice == 1) {
          restockMachine(machineChoice, coffeeStocks, sugarStocks, milkStocks)
        }
        else if (choice == 2) {
          updatePin(machineChoice, machinesPins)
        }
      }

    }
  }
} // Fin du programme
