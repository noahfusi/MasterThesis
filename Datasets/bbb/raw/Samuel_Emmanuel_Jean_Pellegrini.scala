import scala.io.StdIn._
import scala.util.Random



object Main {

  def CodePaiement(): String = {
    Random.alphanumeric.take(5).mkString
  }
  
  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    val machinePins: Array[String] = Array.fill(nbMachines)("434343")
    val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50)
    val sugarStocks: Array[Int] = Array.fill(nbMachines)(30)
    val milkStocks: Array[Int] = Array.fill(nbMachines)(500)

    //variables de mode 
    var ChoixMode = 0

    //variable pour le fonctionnement du mode Client
    var ChoixBoisson = 0

    var ChoixTailleLatte = 0

    var ChoixSucre = 0

    var ChoixLait = 0

    var PrixBoisson = 0.00

    var PrixSucre = 0.00

    var PrixTotal = 0.00

    var NbDoseLait = 0

    var CodeTwint = CodePaiement()

    var RemoteStockCafe = true

    //variables de stock
    var StockCafe = 50

    var StockSucre = 30

    var StockLait = 500

    var UtilisationCafe = 0

    var UtilisationLait = 0

    var UtilisationSucre = 0

    var Remote = true

    //variables pour le fonctionnement du mode Admin

    val CodeAdmin = 434343

    var CodeEcrit = 0

    var AjoutCafe = -1

    var AjoutLait = -1

    var AjoutSucre = -1

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var attempts = 3
      while (attempts > 0) {
        println(s"Entrez le code PIN:")
        print("> ")
        val pin = readLine()
        if (pin == machinePins(machineId)) {
          println(s"Accès accordé à la Machine $machineId.")
          return true
        } else {
          attempts -= 1
          println(s"Code PIN incorrect. $attempts tentatives restantes.")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      false
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println(s"Mise à jour du code PIN pour la Machine $machineId.")
      var newPin = ""
      do {
        println("Entrez un nouveau code PIN à 6 chiffres:")
        newPin = readLine()
        if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
          println("Le code PIN doit comporter exactement 6 chiffres.")
        }
      } while (newPin.length != 6 || !newPin.forall(_.isDigit))
      machinePins(machineId) = newPin
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      StockCafe = coffeeStocks(machineId)

      StockSucre = sugarStocks(machineId)

      StockLait = milkStocks(machineId)

      while (Remote == true) {

        //Choix de la boisson
        print("Veuillez sélectionner votre boisson : " + "\n" +
          "1) Expresso - CHF 2.00" + "\n" +
          "2) Cappuccino - CHF 2.50" + "\n" +
          "3) Latte - CHF 2.70 (Petit), CHF 3.20(Moyen), CHF 3.70 (Grand)" + "\n" +
          "> ")

        //Input du choix de la boisson
        ChoixBoisson = readInt()
        print("\n")

        //Boucle qui empêche un mauvais caractère dans l'input
        while (ChoixBoisson != 1 && ChoixBoisson != 2 && ChoixBoisson != 3) {

          println("Votre choix n'est pas valide")
          print("\n")

          //Choix de la boisson
          print("Veuillez sélectionner votre boisson : " + "\n" +
            "1) Expresso - CHF2.00" + "\n" +
            "2) Cappuccino - CHF 2.50" + "\n" +
            "3) Latte - CHF2.70 (Petit), CHF 3.20(Moyen), CHF3.70 (Grand)" + "\n" +
            "> ")

          //Input du choix de la boisson
          ChoixBoisson = readInt()
          print("\n")
        }

        //Choix = Expresso
        if (ChoixBoisson == 1) {
          PrixBoisson = 2.00
          UtilisationCafe = 8
          UtilisationLait = 0
        }

        //Choix = Cappuccino
        else if (ChoixBoisson == 2) {
          PrixBoisson = 2.50
          UtilisationCafe = 6
          UtilisationLait = 10
        }

        //Choix = Latte
        else if (ChoixBoisson == 3) {

          if (StockLait < 12) {
            UtilisationLait = 12
          }
          else {
            //Choix de la taille
            print("Quelle taille de latte souhaitez-vous ?" + "\n" +
              "1) Petit - CHF2.70" + "\n" +
              "2) Moyen - CHF 3.20" + "\n" +
              "3) Grand - CHF3.70" + "\n" +
              "> ")

            //Input du choix de la taille
            ChoixTailleLatte = readInt()
            print("\n")

            //Boucle qui empêche un mauvais caractère dans l'input
            while (ChoixTailleLatte != 1 && ChoixTailleLatte != 2 && ChoixTailleLatte != 3) {

              println("Votre choix n'est pas valide")
              print("\n")

              //Choix de la taille
              print("Quelle taille de latte souhaitez-vous ?" + "\n" +
                "1) Petit - CHF2.70" + "\n" +
                "2) Moyen - CHF 3.20" + "\n" +
                "3) Grand - CHF3.70" + "\n" +
                "> ")


              //Input du choix de la taille
              ChoixTailleLatte = readInt()
              print("\n")

            }
          }

          if (ChoixTailleLatte == 1) {
            UtilisationCafe = 6
            UtilisationLait = 12
          }
          else if (ChoixTailleLatte == 2) {
            UtilisationCafe = 8
            UtilisationLait = 15
          }
          else if (ChoixTailleLatte == 3) {
            UtilisationCafe = 12
            UtilisationLait = 20
          }

          //Taille de latte = Petit
          if (ChoixTailleLatte == 1) {
            PrixBoisson = 2.70
          }

          //Taille de latte = Moyen
          else if (ChoixTailleLatte == 2) {
            PrixBoisson = 3.20
          }

          //Taille de latte = Grand
          else if (ChoixTailleLatte == 3) {
            PrixBoisson = 3.70
          }

        }

        if ((UtilisationCafe > StockCafe) || (UtilisationLait > StockLait)) {
          if (UtilisationCafe > StockCafe) {
            if (UtilisationLait > StockLait) {
              println("Erreur : Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.")
            }
            else {
              println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
            }
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            Remote = false
            print(RemoteStockCafe)
          }
          else if (UtilisationLait > StockLait) {
            if (ChoixBoisson == 1 || ChoixBoisson == 2) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson.")
            }
            else if (ChoixBoisson == 3) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
          }
          print("\n")
        }
        else if ((UtilisationCafe <= StockCafe) && (UtilisationLait <= StockLait)) {
          Remote = false
          RemoteStockCafe = false
        }

      }
      Remote = true


      RemoteStockCafe = true
      if ((UtilisationCafe <= StockCafe) && (UtilisationLait <= StockLait)) {
        //Ajout du prix au coût total
        PrixTotal += PrixBoisson

        //Utilisation du stock
        StockCafe -= UtilisationCafe
        StockLait = StockLait-UtilisationLait
        UtilisationLait = 0

        //réinitialisation de la télécommande pour la boucle
        Remote = true

        while (Remote == true) {

          //Choix sucre
          print("Voulez-vous du sucre ?" + "\n" +
            "1) Sans sucre" + "\n" +
            "2) Peu (5g) - CHF 0.10" + "\n" +
            "3) Moyen (10g) - CHF 0.20" + "\n" +
            "4) Beaucoup (15g) - CHF 0.30" + "\n" +
            "> ")

          //Input du choix du sucre
          ChoixSucre = readInt()
          print("\n")

          //Boucle qui empêche un mauvais caractère dans l'input
          while (ChoixSucre != 1 && ChoixSucre != 2 && ChoixSucre != 3 && ChoixSucre != 4) {

            println("Votre choix n'est pas valide")
            print("\n")

            //Choix sucre
            print("Voulez-vous du sucre ?" + "\n" +
              "1) Sans sucre" + "\n" +
              "2) Peu (5g) - CHF 0.10" + "\n" +
              "3) Moyen (10g) - CHF 0.20" + "\n" +
              "4) Beaucoup (15g) - CHF 0.30" + "\n" +
              "> ")

            //Input du choix du sucre
            ChoixSucre = readInt()
            print("\n")

          }

          //Pas assez de sucre en stock
          if (UtilisationSucre > StockSucre) {
            print("Stock de sucre insuffisant")
            ChoixSucre = 1
            UtilisationSucre = 0
          }

          //Choix = Sans sucre
          if (ChoixSucre == 1) {
            PrixSucre = 0.00
            UtilisationSucre = 0
          }


          //Quantité de sucre = Peu
          else if (ChoixSucre == 2) {
            PrixSucre = 0.10
            UtilisationSucre = 5
          }

          //Quantité de sucre = Moyen
          else if (ChoixSucre == 3) {
            PrixSucre = 0.20
            UtilisationSucre = 10
          }

          //Quantité de sucre = Beaucoup
          else if (ChoixSucre == 4) {
            PrixSucre = 0.30
            UtilisationSucre = 15
          }


          Remote = false
        }


        //réinitialisation de la télécommande pour la boucle
        Remote = true

        //Utilisation du stock
        StockSucre -= UtilisationSucre

        //Ajout du prix au coût total
        PrixTotal += PrixSucre

        while (Remote == true) {
          if (StockLait < 5 || ChoixBoisson == 1) {
            print("Stock de lait insuffisant")
            ChoixLait = 2
            Remote = false
          }
          //Ajout de lait supplémentaire pour Cappuccino et Latte uniquement
          else if (ChoixBoisson == 2 || ChoixBoisson == 3) {

            //Choix ajout du lait
            print("Souhaitez-vous ajouter du lait en supplément ?" + "\n" +
              "(Disponible uniquement pour Cappuccino et Latte)" + "\n" +
              "1) Oui" + "\n" +
              "2) Non" + "\n" +
              "> ")

            //Input du choix du lait
            ChoixLait = readInt()
            print("\n")

            while (ChoixLait != 1 && ChoixLait != 2) {

              println("Votre choix n'est pas valide")
              print("\n")

              //Choix ajout du lait
              print("Souhaitez-vous ajouter du lait en supplément ?" + "\n" +
                "(Disponible uniquement pour Cappuccino et Latte)" + "\n" +
                "1) Oui" + "\n" +
                "2) Non" + "\n" +
                "> ")

              //Input du choix du lait
              ChoixLait = readInt()
              print("\n")

            }
            if (ChoixLait == 1) {

              //Choix nb de dose de lait
              print("Combien de dose ?" + "\n" +
                "(maximum 3 doses)" + "\n" +
                "> ")

              //Input nb de dose de lait
              NbDoseLait = readInt()
              print("\n")

              while (NbDoseLait < 1 || NbDoseLait > 3) {

                println("Votre choix n'est pas valide")
                print("\n")

                //Choix nb de dose de lait
                print("Combien de dose ?" + "\n" +
                  ">")

                //Input nb de dose de lait
                NbDoseLait = readInt()
                print("\n")

              }
              UtilisationLait = 5 * NbDoseLait
              while (NbDoseLait < 1 || NbDoseLait > 3 || UtilisationLait > StockLait) {
                if (UtilisationLait > StockLait) {

                  print("Stock de lait insuffisant")
                  print("\n")
                  print("Veuillez prendre moins de dose.")

                  //Choix nb de dose de lait
                  print("Combien de dose ?" + "\n" +
                    ">")

                  //Input nb de dose de lait
                  NbDoseLait = readInt()
                  print("\n")
                }
                else if (NbDoseLait < 1 || NbDoseLait > 3) {

                  //Choix nb de dose de lait
                  print("Combien de dose ?" + "\n" +
                    ">")

                  //Input nb de dose de lait
                  NbDoseLait = readInt()
                  print("\n")
                }
              }
            }
            UtilisationLait = 5 * NbDoseLait
            Remote = false
          }
        }

        UtilisationLait = 0

        //réinitialisation de la télécommande pour la boucle
        Remote = true

        //Récapitulatif de la commande

        //Choix boisson
        print("Boisson sélectionnée : ")
        if (ChoixBoisson == 1) {
          println("Expresso")
        }
        else if (ChoixBoisson == 2) {
          println("Cappuccino")
        }
        else if (ChoixBoisson == 3) {
          println("Latte")
        }

        //Choix sucre
        print("Niveau de sucre : ")
        if (ChoixSucre == 1) {
          println("Sans sucre")
        }
        else if (ChoixSucre == 2) {
          println("Peu, 5g")
        }
        else if (ChoixSucre == 3) {
          println("Moyen, 10g")
        }
        else if (ChoixSucre == 4) {
          println("Beaucoup, 15g")
        }

        //Choix lait
        if (ChoixBoisson == 1) {
          println("Non, pas de lait supplémentaire pour les expressos")
        }
        else {
          print("Lait en supplément : ")
          if (ChoixLait == 1) {
            println("Non")
          }
          else if (ChoixLait == 2) {
            println("Oui, " + NbDoseLait + " dose(s)")
          }
        }

        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", PrixBoisson, PrixSucre, PrixTotal)
        PrixTotal = 0
        print("\n")

        //Paiement
        println("Veuillez payer en utilisant Twint." + "\n" +
          "Votre code de paiement est : " + CodeTwint + "\n" +
          "(En attente de validation du paiement...)")

        Thread.sleep(3000)

        print("\n")
        println("Merci ! Votre paiement a été accepté.")
        println("\n")

        //Boisson en cours de préparation
        println("Préparation de votre boisson..." + " \n" +
          "[...]")

        Thread.sleep(5000)

        print("\n")
        if (ChoixBoisson == 1) {
          println("Votre expresso est prêt ! Bonne dégustation !")
        }
        else if (ChoixBoisson == 2) {
          println("Votre cappuccino est prêt ! Bonne dégustation !")
        }
        else if (ChoixBoisson == 3) {
          println("Votre latte est prêt ! Bonne dégustation !")
        }
        println("\n")
        coffeeStocks(machineId) = StockCafe
        sugarStocks(machineId) = StockSucre
        milkStocks(machineId) = StockLait
        return true
      }
      true
    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      StockCafe = coffeeStocks(machineId)
      StockSucre = sugarStocks(machineId)
      StockLait = milkStocks(machineId)
      print("Niveau de stock actuels :" + "\n")
      print("   Poudre de café > " + StockCafe + "g" + "\n")
      print("   Sucre > " + StockSucre + "g" + "\n")
      print("   Lait > " + StockLait + "mL" + "\n")
      print("\n")
      println("Entrez les quantités à ajouter :")
      print("Poudre de café > ")
      while (AjoutCafe < 0) {
        AjoutCafe = readInt()
        if (AjoutCafe < 0) {
          println("Veuillez entrer une valeur positive : ")
          AjoutCafe = readInt()
        }
      }
      StockCafe += AjoutCafe
      AjoutCafe = -1
      print("Lait > ")
      while (AjoutLait < 0) {
        AjoutLait = readInt()
        if (AjoutLait < 0) {
          println("Veuillez entrer une valeur positive : ")
          AjoutLait = readInt()
        }
      }
      StockLait = AjoutLait + StockLait
      AjoutLait = -1

      print("Sucre > ")
      while (AjoutSucre < 0) {
        AjoutSucre = readInt()
        if (AjoutSucre < 0) {
          println("Veuillez entrer une valeur positive : ")
          AjoutSucre = readInt()
        }
      }
      StockSucre += AjoutSucre
      AjoutSucre = -1
      print("\n")

      coffeeStocks(machineId) = StockCafe
      sugarStocks(machineId) = StockSucre
      milkStocks(machineId) = StockLait
      print("Les stocks ont été mis à jour avec succès." + "\n")
      print("Retour au menu principal..." + "\n")
    }



    //début du code de la machine
    while (ChoixMode != 3){

      while (RemoteStockCafe == true) {
        while (Remote == true) {
          //Choix du mode      
          print("    Nospresso Café" + "\n" +
            "Veuillez sélectionner votre mode :" + "\n" +
            "1) Client" + "\n" +
            "2) Admin" + "\n" +
            "3) Quitter" + "\n" +
            "> ")

          //Input du choix      
          ChoixMode = readInt()
          print("\n")

          //Boucle qui empêche un mauvais caractère dans l'input      
          while (ChoixMode != 1 && ChoixMode != 2 && ChoixMode != 3) {

            println("Votre choix n'est pas valide")
            print("\n")

            //Choix du mode        
            println("    Nospresso Café" + "\n" +
              "Veuillez sélectionner votre mode :" + "\n" +
              "1) Client" + "\n" +
              "2) Admin" + "\n" +
              "3) Quitter" + "\n" +
              "> ")

            //Input du choix        
            ChoixMode = readInt()
            print("\n")

          }
          if (ChoixMode == 2 || StockCafe >= 6) {
            Remote = false
          }
        }

        if (ChoixMode == 1 || ChoixMode == 2) {
          //réinitialisation de la télécommande pour la boucle
          Remote = true
          if (ChoixMode == 2) {
            RemoteStockCafe = false
          }
        }
        else if (ChoixMode == 3) {
          println("Fin d'utilisation de la machine")
          RemoteStockCafe = false
        }



        //Mode client
        if (ChoixMode == 1) {
          print("Machine selectionée (0-4) > ")
          var machineId = readInt()
          print("\n")
          if (machineId >= 0 && machineId < nbMachines) {
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
          else {
            println("ID de machine invalide." + "\n")
          }
        }
      }
      //Mode admin
      if (ChoixMode == 2){
        print("Machine selectionée (0-4) > ")
        var machineId = readInt()
        if (machineId >= 0 && machineId < nbMachines && validatePin(machineId, machinePins)) {
          print("\n")
          println("1) Réapprovisionner les stocks")
          println("2) Mettre à jour le code PIN")
          print("> ")
          var adminChoice = readInt()
          if(adminChoice != 1 || adminChoice != 2){
            println("Votre choix n'est pas valide")
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            print("> ")
            adminChoice = readInt()
          }
          if (adminChoice == 1) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
          if (adminChoice == 2) {
            updatePin(machineId, machinePins)
          }
        }
        else {
          ChoixMode = 3
        }
      }
    }
  }
}