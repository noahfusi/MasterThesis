import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.util.Random
  object Main {

    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

      def addIngredient(ingredient: String, amount: Int): Unit = {
        ingredient match {
          case "milk" => milk += amount
          case "sugar" => sugar += amount
          case "coffee" => coffee += amount
          case _ => println("Ingrédient inconnu :" + ingredient)
        }
      }

      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        ingredient match {
          case "milk" if milk >= amount =>
            milk -= amount
            true
          case "sugar" if sugar >= amount =>
            sugar -= amount
            true
          case "coffee" if coffee >= amount =>
            coffee -= amount
            true
          case _ =>
            println("Quantité insuffisante pour" + ingredient + " ou ingrédient inconnu")
            false
        }
      }

      def printMachineDetails(): Unit = {
        println("ID:" + id)
        println("Code PIN:" + pincode)
        println("Lait:" + {milk / 1000.0} + "L")
        println("Sucre:" + sugar + "g")
        println("Café:" + coffee + "g")
      }
    }


    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()

      try {
        val source = Source.fromFile(filename)
        val lines = source.getLines().drop(1)
        for (line <- lines) {
          val cols = line.split(",")
          val machine = new Machine(
            id = machines.size + 1,
            pincode = cols(0),
            milk = cols(1).toInt,
            sugar = cols(2).toInt,
            coffee = cols(3).toInt)

          machines += machine
        }
        source.close()
        println("Chargement des machines depuis" + filename + "...")
        println({
          machines.size
        } + "machine(s) chargée(s) avec succès.")
      } catch {
        case _: FileNotFoundException =>
          println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
          sys.exit(1)
        case ex: IOException =>
          println("Erreur : Échec de la lecture du fichier." + ex)
          sys.exit(1)
      }

      machines
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        val writer = new PrintWriter(new File(filename))
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          writer.println({
            machine.pincode
          } + "," + {
            machine.milk
          } + "," + {
            machine.sugar
          } + "," + {
            machine.coffee
          })
        }
        writer.close()
        println("Sauvegarde de" + {machines.size} + "machine(s) dans " + filename + "...")
        println("Fichier sauvegardé avec succès.")
      } catch {
        case ex: IOException =>
          println("Erreur : Échec de l’écriture dans " + filename + ". Le fichier peut être verrouillé ou en lecture seule.")
          sys.exit(1)
      }
    }

    def main(args: Array[String]): Unit = {
      var machines = loadcsv("machines.csv")
      val filename = "machines.csv"
      machines.foreach(_.printMachineDetails())
      var running = true
      while (running) {
        var modeContinuer = true
        while (modeContinuer) {
          println("")
          println("Nospresso Café")
          println("Veuillez sélectionner votre mode :")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter le programme")
          val choix = demanderOptionValide(Set("1", "2", "3"))

          if (choix == "3") {
            running = false
            modeContinuer = false
            println("Vous quittez le programme.")
          }

          if (choix == "1") {
            val machineId = choisirMachine(machines)
            if (serveClient(machineId, machines)) {
              println("Transaction échouée pour la machine." + machineId)
            }
            modeContinuer = false
          }

          if (choix == "2") {
            val machineId = choisirMachine(machines)
            if (validatePin(machineId, machines)) {
              println("Accès Administrateur validé.")
              println("1) Réapprovisionner la machine")
              println("2) Mettre à jour le code PIN")
              val action = demanderOptionValide(Set("1", "2"))

              if (action == "1") {
                afficherStocks(machineId, machines)
                restockMachine(machineId, machines)
              } else if (action == "2") {
                updatePin(machineId, machines)
              }
            } else {
              println("Code PIN invalide après 3 tentatives. Le programme se termine.")
              modeContinuer = false
            }
          }
        }
      }
      savecsv(filename, machines)
      println("Toutes les données ont été sauvegardées dans " + filename)
    }

    def choisirMachine(machines: ArrayBuffer[Machine]): Int = {
      println("Liste des machines disponibles :")
      machines.zipWithIndex.foreach { case (machine, idx) =>
        println({
          idx + 1
        } + ") Machine" + {
          machine.id
        })
      }
      println("Veuillez choisir une machine par son numéro :")
      val choix = demanderOptionValide((1 to machines.length).map(_.toString).toSet)
      choix.toInt - 1
    }

    def afficherStocks(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
      val machine = machines(machineId)
      println("Stock actuel pour la machine " + machineId + ":")
      println("- Café:" + {
        machine.coffee
      } + "g")
      println("- Sucre:" + {
        machine.sugar
      } + "g")
      println("- Lait:" + {
        machine.milk
      } + "ml")
    }


    def demanderOptionValide(optionsValides: Set[String]): String = {
      var saisie = ""
      do {
        saisie = scala.io.StdIn.readLine("> ")
        if (!optionsValides.contains(saisie)) {
          println("Option invalide. Veuillez réessayer.")
        }
      } while (!optionsValides.contains(saisie))
      saisie
    }

    def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
      var tentatives = 3
      while (tentatives > 0) {
        println("Veuillez entrer le code PIN pour la machine " + machineId + " (il vous reste " + tentatives + " tentative(s)) :")
        val pin = scala.io.StdIn.readLine()
        if (pin == machines(machineId).pincode) {
          return true
        }
        tentatives -= 1
      }
      false
    }

    def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
      var valide = false
      while (!valide) {
        println("Entrez le nouveau code PIN (6 chiffres) :")
        val nouveauPin = scala.io.StdIn.readLine()
        if (nouveauPin.matches("\\d{6}")) {
          machines(machineId).pincode = nouveauPin
          println("Le code PIN pour la machine " + machineId + " a été mis à jour.")
          valide = true
        } else {
          println("Code PIN invalide. Il doit comporter exactement 6 chiffres.")
        }
      }
    }

    def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
      val machine = machines(machineId)
      println("Veuillez sélectionner une boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Capuccino - CHF 2.50")
      println("3) Latte Petit - CHF 2.70")
      println("4) Latte Moyen - CHF 3.20")
      println("5) Latte Grand - CHF 3.70")
      val boisson = demanderOptionValide(Set("1", "2", "3", "4", "5"))
      var cafeNec = 0
      var laitNec = 0
      var prix = 0.0

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
            prix += 0.05
          } else if (lait == "2") {
            laitNec += 100
            prix += 0.1
          } else if (lait == "3") {
            laitNec += 150
            prix += 0.15
          }
        }
      }

      println("Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      val sucre = demanderOptionValide(Set("1", "2", "3", "4"))

      var sucreNec = 0
      var prixSucre = 0.0

      if (sucre == "1") {
        sucreNec = 0
        prixSucre = 0.0
      } else if (sucre == "2") {
        sucreNec = 5
        prixSucre = 0.1
      } else if (sucre == "3") {
        sucreNec = 10
        prixSucre = 0.2
      } else if (sucre == "4") {
        sucreNec = 15
        prixSucre = 0.3
      }

      val total = prix + prixSucre

      if (machine.coffee < cafeNec) {
        println("Quantité de café insuffisante.")
        return false
      } else if (machine.sugar < sucreNec) {
        println("Quantité de sucre insuffisante.")
        return false
      } else if (machine.milk < laitNec) {
        println("Quantité de lait insuffisante.")
        return false
      }

      if (!machine.removeIngredient("coffee", cafeNec) ||
        !machine.removeIngredient("sugar", sucreNec) ||
        !machine.removeIngredient("milk", laitNec)) {
        println("Échec de la mise à jour des stocks.")
        return false
      }


        // Mise à jour des stocks
        machine.coffee -= cafeNec
        machine.sugar -= sucreNec
        machine.milk -= laitNec

        println("Veuillez payer CHF" + total % .2f + "via Twint.")
        val codeTwint = Random.alphanumeric.take(5).mkString
        println("Code de paiement :" + codeTwint + "(Attente de validation...)")
        Thread.sleep(2000)
        println("Paiement accepté. Préparation de votre boisson...")
        Thread.sleep(2000)
        println("Votre boisson est prête. Bonne dégustation!")
        true
      }

      def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
        val machine = machines(machineId)
        println("Réapprovisionnement pour la machine " + machineId + "")
        println("Ajout de café (grammes) :")
        val cafeAmount = scala.io.StdIn.readLine().toInt
        machine.addIngredient("coffee", cafeAmount)
        println("Ajout de sucre (grammes) :")
        val sucreAmount = scala.io.StdIn.readLine().toInt
        machine.addIngredient("sugar", sucreAmount)
        println("Ajout de lait (millilitres) :")
        val laitAmount = scala.io.StdIn.readLine().toInt
        machine.addIngredient("milk", laitAmount)
        println("Réapprovisionnement terminé.")
      }
    }

