object Main {

  import io.StdIn._
  import collection.mutable.ArrayBuffer
  import java.io.{FileWriter, PrintWriter}
  import io.Source

  def main(args: Array[String]): Unit = {
    var infini = true
    var choix = 0
    var choixClient = 0
    var choixSucre = 0
    var choixLait = 0
    var tailleLatte = 0
    /** Sucre et café en grammes, lait en millilitres */
    var qtSucre = 0
    var doseLait = 0
    val erreurCafe = "\nErreur : Quantité de poudre de café insuffisante pour\n" +
      "préparer la boisson sélectionnée.\n" +
      "Veuillez choisir une autre machine ou vérifier les\n" +
      "stocks en mode Admin.\n"
    val erreurSucre = "\nErreur : Quantité de sucre insuffisante pour préparer\n" +
      "la boisson sélectionnée.\n" +
      "Veuillez choisir une quantité de sucre inférieure ou choisir une autre machine.\n"
    val erreurLait = "\nErreur : Quantité de lait insuffisante pour préparer\n" +
      "la boisson sélectionnée.\n" +
      "Veuillez choisir une taille plus petite ou essayer\n" +
      "une autre machine.\n"
    val erreurFichier = "Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme."
    var succes = false
    var prixTotal: Double = 0
    var boisson = ""
    var nivSucre = ""
    var suppLait = ""
    var codePay = ""
    val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var ajoutCafe = 0
    var ajoutLait: Double = 0
    var ajoutSucre = 0
    /** Début Exercice 2 */
    var id = 0
    var machine = 0
    var userpin = ""
    var pinValide = false
    var TestPin = true
    var tentative = 3
    var choixAdmin = 0

    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
      def validatePin(): Boolean = {
        println("Entrez le code PIN :")
        userpin = readLine()
        if (userpin == pincode) {
          println("Accès accordé à la machine " + machine)
          true
        }
        else {
          false
        }
      }

      def updatePin(): Unit = {
        println("Mise à jour du code PIN pour la Machine " + machine)
        do {
          TestPin = true
          println("Entrez un nouveau code PIN à 6 chiffres : ")
          pincode = readLine
          if (pincode.length != 6) {
            println("Le code Pin doit comporter exactement 6 chiffres")
            TestPin = false
          }
          else {
            for (t <- 0 to 5) {
              if (pincode(t) < '0' || pincode(t) > '9') {
                TestPin = false
              }
            }
            if (!TestPin) println("Le code PIN ne doit comporter que des chiffres")
          }
        } while (!TestPin)
        println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
      }

      def addIngredient(ingredient: String, amount: Int): Unit = {
        if (amount < 0) println("Erreur : entrez une valeur nulle ou positive")
        else {
          if (ingredient == "coffee") {
            coffee += amount
          }
          if (ingredient == "sugar") {
            sugar += amount
          }
          if (ingredient == "milk") {
            milk += amount
          }
        }
      }

      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        if (ingredient == "coffee") {
          if (coffee - amount >= 0) {
            coffee -= amount
            true
          }
          else false
        } else if (ingredient == "sugar") {
          if (sugar - amount >= 0) {
            sugar -= amount
            true
          }
          else false
        } else if (ingredient == "milk") {
          if (milk - amount >= 0) {
            milk -= amount
            true
          }
          else false
        } else false
      }

      /** Méthode Client */
      def serveClient(): Boolean = {
        do {
          println("Veuillez sélectionner votre boisson :\n" +
            "1) Expresso - CHF 2.00\n" +
            "2) Cappuccino - CHF 2.50\n" +
            "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choixClient = readInt()
          if (choixClient > 3 || choixClient < 1) println("Erreur : veuillez entrer une valeur entre 1 et 3")
          if (choixClient == 3) {
            do {
              println("Quelle taille de Latte voulez-vous ?\n" +
                "1) Petit\n" +
                "2) Moyen\n" +
                "3) Grand")
              tailleLatte = readInt()
              if (tailleLatte > 3 || tailleLatte < 1) println("Erreur : veuillez entrer une valeur entre 1 et 3")
            } while (tailleLatte > 3 || tailleLatte < 1)
          }
        } while (choixClient > 3 || choixClient < 1)

        do {
          println("Souhaitez-vous ajouter du sucre ?\n" +
            "1) Sans sucre\n" +
            "2) Peu (5g) - CHF 0.10\n" +
            "3) Moyen (10g) - CHF 0.20\n" +
            "4) Beaucoup (15g) - CHF 0.30")
          choixSucre = readInt()
          if (choixSucre > 4 || choixSucre < 1) println("Erreur : veuillez choisir une valeur entre 1 et 4")
          if (choixSucre == 1) {
            qtSucre = 0
            nivSucre = "Sans sucre"
          }
          else if (choixSucre == 2) {
            qtSucre = 5
            nivSucre = "Peu (5g)"
          }
          else if (choixSucre == 3) {
            qtSucre = 10
            nivSucre = "Moyen (10g)"
          }
          else if (choixSucre == 4) {
            qtSucre = 15
            nivSucre = "Beaucoup (15g)"
          }
        } while (choixSucre > 4 || choixSucre < 1)

        if (choixClient != 1) {
          do {
            println("Souhaitez-vous ajouter du lait en supplément ?\n" +
              "(Disponible uniquement pour Cappuccino et Latte)\n" +
              "1) Oui\n" +
              "2) Non")
            choixLait = readInt()
            if (choixLait < 1 || choixLait > 2) println("Erreur : veuillez entrer une valeur entre 1 et 2")
            if (choixLait == 1) {
              do {
                suppLait = "Oui"
                println("Combien de doses ?")
                doseLait = readInt()
                if (doseLait < 1 || doseLait > 3) println("Erreur : veuillez entrer une valeur entre 1 et 3")
              } while (doseLait < 1 || doseLait > 3)
            } else {
              doseLait = 0
              suppLait = "Non"
            }
          } while (choixLait > 2 || choixLait < 1)
        }
        else doseLait = 0

        if (choixClient == 1) boisson = "Expresso"
        else if (choixClient == 2) boisson = "Cappuccino"
        else if (tailleLatte == 1) boisson = "Latte (Petit)"
        else if (tailleLatte == 2) boisson = "Latte (Moyen)"
        else boisson = "Latte (Grand)"
        println("Boisson sélectionnée : " + boisson)
        println("Niveau de sucre : " + nivSucre)
        if (choixClient == 2 || choixClient == 3) println("Lait en supplément : " + suppLait)

        /** Expresso */
        if (choixClient == 1) {
          if (coffee < 8) println(erreurCafe)
          else if (sugar < qtSucre) println(erreurSucre)
          else succes = true
        }

        /** Cappuccino */
        else if (choixClient == 2) {
          if (coffee < 6) println(erreurCafe)
          else if (milk < (100 + (doseLait * 50))) println(erreurLait)
          else if (sugar < qtSucre) println(erreurSucre)
          else succes = true
        }

        /** Latte */
        else if (tailleLatte == 1) {
          if (coffee < 6) println(erreurCafe)
          else if (milk < (120 + (doseLait * 50))) println(erreurLait)
          else if (sugar < qtSucre) println(erreurSucre)
          else succes = true
        }
        else if (tailleLatte == 2) {
          if (coffee < 8) println(erreurCafe)
          else if (milk < (150 + (doseLait * 50))) println(erreurLait)
          else if (sugar < qtSucre) println(erreurSucre)
          else succes = true
        }
        else {
          if (coffee < 12) println(erreurCafe)
          else if (milk < (200 + (doseLait * 50))) println(erreurLait)
          else if (sugar < qtSucre) println(erreurSucre)
          else succes = true
        }


        /** Prix boisson */
        if (succes) {
          if (choixClient == 1) {
            if (choixSucre == 1) println("Prix total : CHF 2,00")
            else if (choixSucre == 2) println("Prix total : CHF 2,00 + CHF 0,10 = CHF 2,10")
            else if (choixSucre == 3) println("Prix total : CHF 2,00 + CHF 0,20 = CHF 2,20")
            else println("Prix total : CHF 2,00 + CHF 0,30 = CHF 2,30")
          }

          /** Prix Cappuccino */
          else if (choixClient == 2) {
            if (choixSucre == 1) {
              if (choixLait != 1) println("Prix total : CHF 2,50")
              else {
                prixTotal = 2.50 + (doseLait * 0.05)
                print("Prix total : CHF 2,50 + CHF ")
                printf("%.2f", doseLait * 0.05)
                printf(" = CHF " + "%.2f \n", prixTotal)
              }
            }
            else if (choixSucre == 2) {
              if (choixLait != 1) println("Prix total : CHF 2,50 + CHF 0,10 = CHF 2,60")
              else {
                prixTotal = 2.60 + (doseLait * 0.05)
                print("Prix total : CHF 2,50 + CHF 0,10 + CHF ")
                printf("%.2f", doseLait * 0.05)
                printf(" = CHF " + "%.2f \n", prixTotal)
              }
            }
            else if (choixSucre == 3) {
              if (choixLait != 1) println("Prix total : CHF 2,50 + CHF 0,20 = CHF 2,70")
              else {
                prixTotal = 2.70 + (doseLait * 0.05)
                print("Prix total : CHF 2,50 + CHF 0,20 + CHF ")
                printf("%.2f", doseLait * 0.05)
                printf(" = CHF " + "%.2f \n", prixTotal)
              }
            }
            else {
              if (choixLait != 1) println("Prix total : CHF 2,50 + CHF 0,30 = CHF 2,80")
              else {
                prixTotal = 2.80 + (doseLait * 0.05)
                print("Prix total : CHF 2,50 + CHF 0,30 + CHF ")
                printf("%.2f", doseLait * 0.05)
                printf(" = CHF " + "%.2f \n", prixTotal)
              }
            }
          }

          /** Prix Latte (Petit) */
          else if (choixClient == 3) {
            if (tailleLatte == 1) {
              if (choixSucre == 1) {
                if (choixLait != 1) println("Prix total : CHF 2,70")
                else {
                  prixTotal = 2.70 + (doseLait * 0.05)
                  print("Prix total : CHF 2,70 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 2) {
                if (choixLait != 1) println("Prix total : CHF 2,70 + CHF 0,10 = CHF 2,80")
                else {
                  prixTotal = 2.80 + (doseLait * 0.05)
                  print("Prix total : CHF 2,70 + CHF 0,10 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 3) {
                if (choixLait != 1) println("Prix total : CHF 2,70 + CHF 0,20 = CHF 2,90")
                else {
                  prixTotal = 2.90 + (doseLait * 0.05)
                  print("Prix total : CHF 2,70 + CHF 0,20 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else {
                if (choixLait != 1) println("Prix total : CHF 2,70 + CHF 0,30 = CHF 3,00")
                else {
                  prixTotal = 3.00 + (doseLait * 0.05)
                  print("Prix total : CHF 2,70 + CHF 0,30 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
            }

            /** Prix Latte (Moyen) */
            else if (tailleLatte == 2) {
              if (choixSucre == 1) {
                if (choixLait != 1) println("Prix total : CHF 3,20")
                else {
                  prixTotal = 3.20 + (doseLait * 0.05)
                  print("Prix total : CHF 3,20 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 2) {
                if (choixLait != 1) println("Prix total : CHF 3,20 + CHF 0,10 = CHF 3,30")
                else {
                  prixTotal = 3.30 + (doseLait * 0.05)
                  print("Prix total : CHF 3,20 + CHF 0,10 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 3) {
                if (choixLait != 1) println("Prix total : CHF 3,20 + CHF 0,20 = CHF 3,40")
                else {
                  prixTotal = 3.40 + (doseLait * 0.05)
                  print("Prix total : CHF 3,20 + CHF 0,20 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else {
                if (choixLait != 1) println("Prix total : CHF 3,20 + CHF 0,30 = CHF 3,50")
                else {
                  prixTotal = 3.50 + (doseLait * 0.05)
                  print("Prix total : CHF 3,20 + CHF 0,30 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
            }

            /** Prix Latte (Grand) */
            else {
              if (choixSucre == 1) {
                if (choixLait != 1) println("Prix total : CHF 3,70")
                else {
                  prixTotal = 3.70 + (doseLait * 0.05)
                  print("Prix total : CHF 3,70 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 2) {
                if (choixLait != 1) println("Prix total : CHF 3,70 + CHF 0,10 = CHF 3,80")
                else {
                  prixTotal = 3.80 + (doseLait * 0.05)
                  print("Prix total : CHF 3,70 + CHF 0,10 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else if (choixSucre == 3) {
                if (choixLait != 1) println("Prix total : CHF 3,70 + CHF 0,20 = CHF 3,90")
                else {
                  prixTotal = 3.90 + (doseLait * 0.05)
                  print("Prix total : CHF 3,70 + CHF 0,20 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
              else {
                if (choixLait != 1) println("Prix total : CHF 3,70 + CHF 0,30 = CHF 4,00")
                else {
                  prixTotal = 4.00 + (doseLait * 0.05)
                  print("Prix total : CHF 3,70 + CHF 0,30 + CHF ")
                  printf("%.2f", doseLait * 0.05)
                  printf(" = CHF " + "%.2f \n", prixTotal)
                }
              }
            }
          }

          /** Message paiement */

          codePay = ""
          for (i <- 1 to 5) codePay += alphanumerique(scala.util.Random.nextInt(alphanumerique.length))

          println("\nVeuillez payer en utilisant Twint.\n" +
            "Votre code de paiement est : " + codePay +
            "\n(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("\nMerci ! Votre paiement a été accepté.\n" +
            "Préparation de votre boisson...")
          println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")

          /** Diminution des Stocks */

          if (choixClient == 1) removeIngredient("coffee", 8)
          else if (choixClient == 2) {
            removeIngredient("coffee", 6)
            removeIngredient("milk", 100)
          }
          else if (tailleLatte == 1) {
            removeIngredient("coffee", 6)
            removeIngredient("milk", 120)
          }
          else if (tailleLatte == 2) {
            removeIngredient("coffee", 8)
            removeIngredient("milk", 150)
          }
          else {
            removeIngredient("coffee", 12)
            removeIngredient("milk", 200)
          }
          var addSugar = (choixSucre - 1) * 5
          removeIngredient("sugar", addSugar)
          var addMilk = doseLait * 50
          removeIngredient("milk", addMilk)
          true
        } else false
      }

      /** Méthode Admin */
      def restockMachine(): Unit = {
        var doubleMilk = milk.toDouble
        println("Accès autorisé.\n\n" +
          "Stocks:\n" +
          " Poudre de café: " + coffee + "g" +
          "\n Lait: " + (doubleMilk / 1000) + "L" +
          "\n Sucre: " + sugar + "g")
        do {
          ajoutCafe = 0
          ajoutLait = 0
          ajoutSucre = 0
          println("Combien de grammes de poudre de café voulez-vous ajouter ?")
          ajoutCafe = readInt()
          addIngredient("coffee", ajoutCafe)
          println("Combien de litres de lait voulez-vous ajouter ?")
          ajoutLait = readDouble()
          var suppLait = (ajoutLait * 1000).toInt
          addIngredient("milk", suppLait)
          println("Combien de grammes de sucre voulez-vous ajouter ?")
          ajoutSucre = readInt()
          addIngredient("sugar", ajoutSucre)
        } while (ajoutCafe < 0 || ajoutLait < 0 || ajoutSucre < 0)
        println("Réapprovisionnement des stocks...\n" +
          "Ajout:\n" +
          " Poudre de café: " + ajoutCafe + "g" +
          "\n Lait: " + ajoutLait + "L" +
          "\n Sucre: " + ajoutSucre + "g")
        println("Niveaux de stock mis à jour.\n" +
          "Retour au menu principal...\n")
      }
    }

    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()
      println("Chargement des machines depuis " + filename + "...")
      try {
        val csv = Source.fromFile(filename)
        var id = 0
        val lignecsv = csv.getLines()
        lignecsv.next()
        lignecsv.foreach { ligne =>
          val colonne = ligne.split(",")
          id += 1
          var pincode = colonne(0)
          val milk = colonne(1).toInt
          val sugar = colonne(2).toInt
          val coffee = colonne(3).toInt
          machines += new Machine(id, pincode, milk, sugar, coffee)
          var doubleMilk = milk.toDouble
          println("Machine " + id + " chargée :\n" +
            "ID: " + id + "\n" +
            "Code PIN: " + pincode + "\n" +
            "Lait: " + (doubleMilk / 1000) + "L\n" +
            "Sucre: " + sugar + "g\n" +
            "Café: " + coffee + "g\n")
        }
        csv.close()
        println(machines.length + " machine(s) chargée(s) avec succès.")
      } catch {
        case _ => println("Erreur : le fichier " + filename + " est introuvable. Vérifiez le chemin d’accès et réessayez.")
          println(erreurFichier)
          infini = false
      }
      machines
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      println("Sauvegarde de " + machines.length + " machines dans " + filename + "...")
      try {
        val csv = new PrintWriter(new FileWriter(filename))
        csv.println("PINCODE,MILK,SUGAR,COFFEE")
        machines.foreach { ligne =>
          csv.println(ligne.pincode + "," + ligne.milk + "," + ligne.sugar + "," + ligne.coffee)
        }
        csv.close()
        println("Fichier sauvegardé avec succès.")
      }
      catch {
        case _ => println("Erreur : échec de l'écriture dans " + filename + ".\nLe fichier peut être vérouillé ou en lecture seule.")
          println(erreurFichier)
          infini = false
      }
    }

    val filename = "machines.csv"
    val machines = loadcsv(filename)

    /** Début programme */
    while (infini) {
      do {
        println("         Nospresso Café\n" +
          "Veuillez sélectionner votre mode :\n" +
          "1) Client\n" +
          "2) Admin\n" +
          "3) Quitter")
        succes = false
        choix = readInt()
        if (choix > 3 || choix < 1) println("Erreur : veuillez entrer une valeur entre 1 et 3")
      } while (choix > 3 || choix < 1)

      if (choix == 1) {
        do {
          do {
            print("Mode Client\n" + "Machine sélectionnée (1-" + machines.length + ") : ")
            id = readInt
            if (id < 1 || id > machines.length) println("Erreur : veuillez entrer une valeur entre 1 et " + machines.length)
          } while (id < 1 || id > machines.length)
          id -= 1
          machine = id + 1
          machines(id).serveClient()
        } while (!succes)
      }

      /** Admin */

      if (choix == 2) {
        println("Mode Admin :")
        do {
          print("Machine sélectionnée (1-" + machines.length + ") : ")
          id = readInt
          if (id < 1 || id > machines.length) println("Erreur : veuillez entrer une valeur entre 1 et " + machines.length)
        } while (id < 1 || id > machines.length)
        id -= 1
        machine = id + 1
        println("Que voulez-vous faire ?\n" +
          "1) Réapprovisionner les stocks \n2) Changer le code PIN")
        do {
          tentative = 3
          choixAdmin = readInt
          if (choixAdmin < 1 || choixAdmin > 2) println("Erreur : veuillez entrer une valeur entre 1 et 2")
        } while (choixAdmin < 1 || choixAdmin > 2)
        do {
          pinValide = machines(id).validatePin()
          if (!pinValide) {
            tentative -= 1
            println("Code PIN incorrect. " + tentative + " tentatives restantes.")
          }
        } while (tentative != 0 && !pinValide)
        if (tentative == 0) {
          println("Trop de tentatives échouées. Fin du programme.")
          savecsv(filename, machines)
          infini = false
        }
        else if (choixAdmin == 2) machines(id).updatePin()
        else machines(id).restockMachine()
      }

      /** Quitter */

      if (choix == 3) {
        savecsv(filename, machines)
        infini = false
      }
    }
  }
}