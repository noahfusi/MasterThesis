 import scala.io.StdIn._
    import scala.util.Random
    import scala.collection.mutable.ArrayBuffer
    import java.io.{FileWriter,PrintWriter}
    import scala.io.Source




    object Main {
      def main(args: Array[String]): Unit = {
        //ARRAYS AND NBMACHINES NO LONGER NEEDED HOWEVER OR AREAS WHERE NBMACHINES ARE MENTIONED I NEED TO REPLACE IT SO IT INDICATES THE CORRECT MACHINE ID
        //val nbMachines = 5
        //var machineId = Array.fill(nbMachines)(0)

        //STOCK POUR DIFFERENT CAFé
        val CafeEspresso = 8.0 //g
        val CafeCappuccino = 6.0 //g
        val LaitCappuccino = 0.100 //L
        val CaffeLattePetit = 6.0
        val LaitLattePetit = 0.120
        val CaffeLatteMoyen = 8.0
        val LaitLatteMoyen = 0.150
        val CaffeLatteGrand = 12.0
        val LaitLatteGrand = 0.200

        //stock sucre x-tra
        val SucrePeu = 5.0
        val SucreMoyen =10.0
        val SucreBeaucoup = 15.0

    //stock lait x-tra
        val LaitSup1 = 0.050
        val LaitSup2 = 0.100
        val LaitSup3 = 0.150

    //PRIX
    // Lait
    val PrixLait1 = 0.05
    val PrixLait2 = 0.10
    val PrixLait3 = 0.15

    //sucre
    val PrixSucrePeu = 0.10
    val PrixSucreMoyen = 0.20
    val PrixSucreBeaucoup = 0.30

    //prix type de café
    //Expresso
    val PrixEspresso = 2.0

    //Cappuccino
    val PrixCappuccino = 2.5

    //Latte
    val PrixLattePetit = 2.7
    val PrixLatteMoyen = 3.2
    val PrixLatteGrand = 3.7


//Annonce de la class machine
    case class Machine(
                      id: Int, //Id machine
                      var pincode: String, //code 6 chiffres
                      var milk: Int, //en mL
                      var sugar: Int, //en g
                      var coffee: Int, //en g
                      ) {

  //ajout d'ingredient à la machine
      def addIngredient(ingredient: String, amount: Int): Unit = {
        //Ajoute une quantité spécifiée d’un ingrédient au stock de la machine.
        if (ingredient == "coffee") {
          coffee += amount
        } else if (ingredient == "milk") {
          milk += amount
        } else if (ingredient == "sugar") {
          sugar += amount
        }
      }

      def removeIngredient(ingredient: String, amount: Double): Boolean = {
        //Retire une quantité spécifié d’un ingrédient du stock de la machines ilestock est suffisant. Retourne true si l’opération réussit, false sinon.
        ingredient match {
          case "coffee" if coffee >= amount => coffee -= amount; true
          case "milk" if milk >= amount => milk -= amount; true
          case "sugar" if sugar >= amount => sugar -= amount; true
          case _ => false
        }
      }
    }

        //pour ouvrire/fermer csv
object csvGestion {
    def loadcsv(csvFichier: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()
      try {
        val source = Source.fromFile(csvFichier)
        val lignes = source.getLines()
        if (!lignes.isEmpty) lignes.next() //prend pas en compte la première ligne du csv
        var id = 1
        for (line <- lignes) {
          val valeurCSV = line.split(",")
          Machine(
            pincode = valeurCSV(1) //doit etre en string 1ere
            milk = valeurCSV(2).toInt // 2eme
            sugar = valeurCSV(3).toInt // 3 eme
            coffee = valeurCSV(4).toInt //4eme
           val machine = new Machine(id, pincode, milk, sugar, coffee);
          machines += machine
          println("Chargement des machines depuis machines.csv...")
          id += 1 )
        }
        source.close()
        println(machines.length + "machine(s) chargée(s) avec succès.")
      } catch {
        case _: java.io.FileNotFoundException =>

          println("Erreur : Fichier introuvable. Ve ́rifiez le chemin d’acce`s et r ́eessayez..")
          return machines
         }
      machines
    }

    //val machines: ArrayBuffer[Machine] = loadcsv(csvFichier)

    def savecsv(csvFichier: String, machines: ArrayBuffer[Machine]): Unit = {
        try {
          val writer = new PrintWriter(new FileWriter(csvFichier))
          writer.println(s"Chargement des machines depuis machines.csv...\n \n Machine {index+1} chargée: \n ID: {machine.id}\n Code PIN: {machines.pinCode}\n  ")
          writer.println(f"Lait: ${machine.milk}L")
          writer.println(f"Sucre: ${machine.sugar} g")
          writer.println(f"Café: ${machine.coffee} g")
          writer.println("Sauvegarde des machines dans machines.csv...")
          writer.close()
        } catch {
          case _: Exception => println("Erreur :  ́Echec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        }
      }
  }
      //Right after this it should ask about the to select their Nospresso mode
  val csvFichier = "machines.csv"
  val machines = csvGestion.loadcsv(csvFichier)
  var continuerProgram = true

    while (continuerProgram) {
      println("Bienvenue à Nospresso! Veuillez sélectionner une de nos machines >")
      val machineId = readInt()

      if (machineId == 0) {
        csvGestion.savecsv(csvFichier, machines)
        println("Au revoir.")
        continuerProgram = false
      } else {
        machines.find(_.id == machineId) match {
          case machine =>
        println ("Nospresso Café\nVeuillez s'electionner votre mode :\n1) Client\n2) Admin\n 3) Quitter\n>")
        val mode = readInt ()
        if (mode == 1) {
        serveClient (machineId)
        } else if (mode == 2) {
        if (validatePin (machineId) ) {
        admin (machine)
        } else {
        println ("Accès refusé.")
        }
        } else if (mode == 3) {
        continuerProgram = false
        println ("Erreur.")
        }
        }
      }


    //validatePin
    def validatePin(machine: Machine): Boolean = {

      println("Veuillez entrer le code PIN (6 chiffres) :")
      var essaie = 0
      while (essaie < 3) {
        val motDePasse = readLine
        if (motDePasse == machine.pincode {
          println("Accès autorisé.")
          return true
        } else {
          essaie += 1
          var tentativesRestantes = 3 - essaie
          println("Code PIN incorrect. " + tentativesRestantes + "tentatives restantes. >")
        }
      } else if(essaie > 3) {
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }
    }


    def admin(machine: Machine): Unit = {
    var continueAdmin = true
      while (continueAdmin) {
      println("Admin: \n 1) Réapprovionnement de la machine \n 2) Modifier le code PIN")
      var choixAdmin = readInt()
        if (choixAdmin == 1) {
          restockMachine(machine)
        } else if (choixAdmin == 2) {
          updatePin(machine)
        }
      }
    }


    def updatePin(machine: Machine): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + "\n Entrez un nouveau code PIN à 6 chiffres >")
      var nouveauPIN = ""
      while(nouveauPIN.length < 6) {
        println("Entrez un nouveau code PIN à 6 chiffres >")
        nouveauPIN = readLine()
      }
      machine.pincode = nouveauPIN
      println("Le code PIN a  été mis à jour avec succès.\nRetour au menu principal...")
    }


    def serveClient(machine: Machine): Unit = {
      var PrixClient = 0.00
      var PrixCalcul = ""
      var stockSuffisant = true //pour vérifier que le stock de produit = suffisant
      var NomBoisson = ""
      var NiveauSucre = ""
      var NiveauLait = ""

      println("Veuillez s´electionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
      var selectionboisson = readInt()

      if (selectionboisson == 1) {
        if (machine.coffee >= CafeEspresso) {
          machine.coffee -= CafeEspresso
          PrixClient += PrixEspresso
          NomBoisson = "Espresso"
          PrixCalcul += "CHF 2.00 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (selectionboisson == 2) {
        if (machine.coffee >= CafeCappuccino && machine.milk >= LaitCappuccino) {
          machine.coffee -= CafeCappuccino
          machine.milk -= LaitCappuccino
          PrixClient += PrixCappuccino
          NomBoisson = "Cappuccino"
          PrixCalcul += "CHF 2.50 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      }
      if (selectionboisson == 3) {
        println("Veuillez choisir la taille du latte :\n1) Petit - CHF 2.70\n2)Moyen - CHF 3.20\n3) Grand - CHF 3.70")
        var taillelatte = readInt()
        if (taillelatte == 1) {
          if (machine.coffee >= CaffeLattePetit && machine.milk >= LaitLattePetit) {
            machine.coffee -= CaffeLattePetit
            machine.milk -= LaitLattePetit
            PrixClient += PrixLattePetit
            NomBoisson = "Latte Petit"
            PrixCalcul += "CHF 2.70 "
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        } else if (taillelatte == 2) {
          if (machine.coffee >= CaffeLatteMoyen && machine.milk >= LaitLatteMoyen) {
            machine.coffee -= CaffeLatteMoyen
            machine.milk -= LaitLatteMoyen
            PrixClient += PrixLatteMoyen
            NomBoisson = "Latte Moyen"
            PrixCalcul += "CHF 3.20 "
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        } else if (taillelatte == 3) {
          if (machine.coffee >= CaffeLatteGrand && machine.milk >= LaitLatteGrand) {
            machine.coffee -= CaffeLatteGrand
            machine.milk -= LaitLatteGrand
            PrixClient += PrixLatteGrand
            NomBoisson = "Latte Grand"
            PrixCalcul += "CHF 3.70"
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        }
      }
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
      var Qsucre = readInt()
      if (Qsucre == 2) {
        if (machine.sugar >= SucrePeu) {
          machine.sugar -= SucrePeu
          PrixClient += PrixSucrePeu
          NiveauSucre = "Peu (5g)"
          PrixCalcul += " + CHF 0.10 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 3) {
        if (machine.sugar >= SucreMoyen) {
          machine.sugar -= SucreMoyen
          PrixClient += PrixSucreMoyen
          NiveauSucre = "Moyen (10g)"
          PrixCalcul += " + CHF 0.20 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 4) {
        if (machine.sugar >= SucreBeaucoup) {
          machine.sugar -= SucreBeaucoup
          PrixClient += PrixSucreBeaucoup
          NiveauSucre = "Beaucoup (15g)"
          PrixCalcul += " + CHF 0.30 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 1) {
        NiveauSucre = "Non"
      }

      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>")
      var choixLait = readInt()

      if (choixLait == 1) {
        println("Combien de dose ?\n>")
        var QLait = readInt()

        while (QLait > 3) {
          if (QLait == 1 && machine.milk >= LaitSup1) {
            machine.milk -= LaitSup1
            PrixClient += PrixLait1
            NiveauLait = "Une dose (50ml)"
            PrixCalcul += " + CHF 0.05 "
            stockSuffisant = true

          } else if (QLait == 2 && machine.milk >= LaitSup2) {
            machine.milk -= LaitSup2
            PrixClient += PrixLait2
            NiveauLait = "Deux doses (100ml)"
            PrixCalcul += " + CHF 0.10 "
            stockSuffisant = true

          } else if (QLait == 3 && machine.milk >= LaitSup3) {
            machine.milk -= LaitSup3
            PrixClient += PrixLait3
            NiveauLait = "Trois doses(150ml)"
            PrixCalcul += " + CHF 0.15 "
            stockSuffisant = true

          } else if (QLait > 3) {
            println("Choisisez une quantité de lait plus bas (3 doses ou moin)\n>")
            var QLait = readInt()
          } else if (choixLait == 2) {
            NiveauLait = "Non"
          }
        }
        //Annonce de la commande (selon si stock = suffisant ou pas)
        if (stockSuffisant == true) {
          printf("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire: " + NiveauLait + "\n Prix total: " + PrixCalcul + " = %.2f", PrixClient)
        } else if (!stockSuffisant) {
          print("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire:" + NiveauLait + "\n Erreur : Stock insuffisante pour préparer le boisson sélectionnée. \nVeuillez choisir une tailleplus petite ou essayer une autre boisson.")
        }
      }


      println(" Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + Random.alphanumeric.take(5).mkString + "\n (En attente de validation du paiement...)")

      Thread.sleep(5000)
      println("Merci ! Votre paiement a été accepté. \n ")
      println("Préparation de votre boisson... \n[...]")
      println("votre " + NomBoisson + " est prêt ! Bonne dégustation !")
    }


    def restockMachine(machine: Machine ): Unit = {
      println("Stock actuels: \n Poudre de café: " + machine.coffee + "g \n Lait: " + machine.milk + "L \n Sucre: " + machine.sugar + "g")
      println("Entrez les quantités à ajouter: ")
      println("Poudre de café > ")
      machine.addIngredient("coffee", readDouble())
      //val AjoutCafe = readDouble()

      println("Sucre >")
      machine.addIngredient("sugar", readDouble())

      println("Lait >")
      machine.addIngredient("milk", readDouble())

    }
  }
      }
    }





