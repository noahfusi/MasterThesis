import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._

class Machine(val Id: Int, var codeAdmin: String, var stockLait: Int, var stockSucre: Int, var stockCafe: Int) {

  def display(): Unit = {
    println()
    println("Machine " + Id + " chargée:")
    println("    ID: " + Id)
    println("    PIN: " + codeAdmin)
    println("    Lait: " + "%.3f".format(stockLait / 1000.0) + " L")
    println("    Sucre: " + stockSucre + " g")
    println("    Café: " + stockCafe + " g")
  }

  def printCSV(): String = {
    codeAdmin + "," + stockLait + "," + stockSucre + "," + stockCafe
  }

  def CSVreading (filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis " + filename + "...")

    try {
      val file = Source.fromFile(filename)
      val lignesSansEntete = file.getLines().drop(1)
      lignesSansEntete.zipWithIndex.foreach { case (ligne, index) =>
        val machineIngredient = ligne.split(",")
        if (machineIngredient.length == 4) {
          val machine = new Machine(index + 1, machineIngredient(0), machineIngredient(1).toInt, machineIngredient(2).toInt, machineIngredient(3).toInt)
          machines += machine
          machine.display()
        }
      }
      file.close()
      println(machines.length + " machine(s) chargée(s).")
    }
    catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fin du programme.")
        System.exit(0)
    }
    machines
  }

  def CSVwriting(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val printWriter = new PrintWriter(new FileWriter(filename, false))
      printWriter.println("PINCODE,LAIT,SUCRE,CAFE")

      for (machine <- machines) {
        printWriter.println(machine.printCSV())
      }

      println("Sauvegarde de " + machines.size + " machine(s) dans " + filename + "...")
      printWriter.close()
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans " + filename + ". Le fichier est en lecture seule ou verrouillé.")
        println("Erreur : Échec de la sauvegarde ou du chargement des machines.")
        println("Fin du programme.")
        System.exit(0)
    }
  }

  def addIngredient(stock: String, quantity: Int): Unit = {
    if (stock.toLowerCase == "stockcafe") {
      stockCafe += quantity
    }
    if (stock.toLowerCase == "stocklait") {
      stockLait += quantity
    }
    if (stock.toLowerCase == "stocksucre") {
      stockSucre += quantity
    }
  }

  def removeIngredient(stock: String, quantity: Int): Boolean = {
    if (stock.toLowerCase == "stockcafe" && stockCafe >= quantity) {
      stockCafe -= quantity
    }
    if (stock.toLowerCase == "stocklait" && stockLait >= quantity) {
      stockLait -= quantity
    }
    if (stock.toLowerCase == "stocksucre" && stockSucre >= quantity) {
      stockSucre -= quantity
      true
    }
    else {
      false
    }
  }
}

object Main {

  val machines = ArrayBuffer[Machine]()
  val CSV = "machines.csv"

  def main(args: Array[String]): Unit = {

    machines ++= new Machine(0, "", 0, 0, 0).CSVreading(CSV)

    var running = true

    while (running) {

      var cafeNecessaire = 0
      var laitNecessaire = 0.0
      var sucreNecessaire = 0

      var nomBoisson = ""
      var nomSucre = ""
      var nomLait = "Non"

      var prixBoisson = 0.00
      var prixSucre = 0.00
      var prixLait = 0.00

      def selectionMode(): String = {
        var mode = ""
        while (mode != "1" && mode != "2" && mode != "3") {
          println("Nospresso Café")
          println("Veuillez sélectionner votre mode :")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter")
          print("> ")
          mode = readLine()
          if (mode != "1" && mode != "2" && mode != "3") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
        }
        mode
      }

      def selectionMachine(): Machine = {
        var choix = -1
        println("Entrez l'identifiant de la machine:")
        for (nombre <- machines.indices) {
          println("Machine " + (nombre + 1))
        }
        print("> ")
        choix = readInt()
        while (choix < 1 || choix > machines.size) {
          println("Entrée non valide, veuillez entrer une valeur autorisée.")
          print(">")
          choix = readInt()
        }
        machines(choix - 1)
      }

      def serveClient(machine: Machine): Unit = {

        var boisson = ""

        while (boisson != "1" && boisson != "2" && boisson != "3") {
          println("Veuillez séléctionner votre boisson : ")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print(">")
          boisson = readLine()
          if (boisson != "1" && boisson != "2" && boisson != "3") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
        }
        if (boisson == "1") {
          nomBoisson = "Expresso"
          prixBoisson += 2.00
          cafeNecessaire = 8
        }
        else if (boisson == "2") {
          nomBoisson = "Cappuccino"
          prixBoisson += 2.50
          cafeNecessaire = 6
          laitNecessaire = 100
        }
        else if (boisson == "3") {
          var tailleLatte = ""
          while (tailleLatte != "1" && tailleLatte != "2" && tailleLatte != "3") {
            println("1) Petit Latte - CHF 2.70")
            println("2) Moyen Latte - CHF 3.20")
            println("3) Grand Latte - CHF 3.70")
            print(">")
            tailleLatte = readLine()
            if (tailleLatte != "1" && tailleLatte != "2" && tailleLatte != "3") {
              println("Entrée non valide, veuillez taper une valeur autorisée.")
            }
          }
          if (tailleLatte == "1") {
            nomBoisson = "Latte (Petit)"
            prixBoisson += 2.70
            cafeNecessaire = 6
            laitNecessaire = 120
          }
          else if (tailleLatte == "2") {
            nomBoisson = "Latte (Moyen)"
            prixBoisson += 3.20
            cafeNecessaire = 8
            laitNecessaire = 150
          }
          else if (tailleLatte == "3") {
            nomBoisson = "Latte (Grand)"
            prixBoisson += 3.70
            cafeNecessaire = 12
            laitNecessaire = 200
          }
        }
        var sucre = ""
        while (sucre != "1" && sucre != "2" && sucre != "3" && sucre != "4") {
          println("Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre ")
          println("2) Peu (5g) - CHF 0.10 ")
          println("3) Moyen (10g) - CHF 0.20 ")
          println("4) Beaucoup (15g) - CHF 0.30 ")
          print(">")
          sucre = readLine()
          if (sucre != "1" && sucre != "2" && sucre != "3" && sucre != "4") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
          if (sucre == "1") {
            nomSucre = "Sans Sucre"
            prixSucre += 0.00
            sucreNecessaire = 0
          }
          else if (sucre == "2") {
            nomSucre = "Peu (5g)"
            prixSucre += 0.10
            sucreNecessaire = 5
          }
          else if (sucre == "3") {
            nomSucre = "Moyen (10g)"
            prixSucre += 0.20
            sucreNecessaire = 10
          }
          else if (sucre == "4") {
            nomSucre = "Beaucoup (15g)"
            prixSucre += 0.30
            sucreNecessaire = 15
          }
        }
        if (boisson == "2" || boisson == "3") {
          var lait = ""
          while (lait != "1" && lait != "2") {
            println("Souhaitez-vous ajouter du lait en supplément ? ")
            println("1) Oui")
            println("2) Non")
            print(">")
            lait = readLine()
            if (lait != "1" && lait != "2") {
              println("Entrée non valide, veuillez taper une valeur autorisée.")
            }
            if (lait == "1") {
              nomLait = "Oui"
              var doses = ""
              while (doses != "1" && doses != "2" && doses != "3") {
                println("Combien de doses ? (3 maximum)")
                print(">")
                doses = readLine()
                if (doses == "1" || doses == "2" || doses == "3") {
                  laitNecessaire += 50 * doses.toInt
                  prixLait += 0.05 * doses.toInt
                }
                else {
                  println("Entrée non valide, veuillez taper une valeur autorisée.")
                }
              }
            }
            else if (lait == "2") {
              nomLait = "Non"
              prixLait += 0.00
            }
          }
        }
        println("Boisson sélectionnée : " + nomBoisson)
        println("Niveau de sucre : " + nomSucre)
        if (boisson == "2" || boisson == "3") {
          println("Lait en supplément : " + nomLait)
        }
        if (machine.stockCafe >= cafeNecessaire && machine.stockLait >= laitNecessaire && machine.stockSucre >= sucreNecessaire) {
          machine.removeIngredient("stockCafe", cafeNecessaire)
          machine.removeIngredient("stockLait", laitNecessaire.toInt)
          machine.removeIngredient("stockSucre", sucreNecessaire)

          var prixTotal = prixBoisson + prixSucre + prixLait
          printf("Prix total: CHF %.2f CHF %.2f CHF %.2f = CHF %.2f\n", prixBoisson, prixSucre, prixLait, prixTotal)
          println()

          var alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var code = ""
          for (i <- 0 to 4) {
            code += alphanumericChars((math.random() * alphanumericChars.length).toInt)
          }
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + code)
          println("(En attente de paiement...)")
          Thread.sleep(5000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          println("[...]")
          println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")
        }
        else {
          if (machine.stockCafe < cafeNecessaire) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")

            if (boisson == "1" || boisson == "2") {
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (boisson == "3") {
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
          }
          if (machine.stockLait < laitNecessaire) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")

            if (boisson == "1" || boisson == "2") {
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else if (boisson == "3") {
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            }
          }
          if (machine.stockSucre < sucreNecessaire) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez rajouter moins de sucre dans votre boisson.")
          }
        }
      }

      def verificationCode(machine: Machine): Boolean = {
        println("Mode Admin")
        var tentatives = 0
        val tentativesMax = 3
        var codeEntre = ""

        while (tentatives < tentativesMax) {
          println("Entrez le code PIN : ")
          codeEntre = readLine()

          if (codeEntre == machine.codeAdmin) {
            println("Accès autorisé.")
            return true
          }
          else {
            tentatives += 1
            if (tentatives < tentativesMax) {
              println(s"Code PIN incorrect. Il vous reste " + (tentativesMax - tentatives) + " tentative(s).")
            }
            else {
              println("Trop de tentatives échouées. Fin du programme.")
              running = false
            }
          }
        }
        false
      }

      def menuAdmin(): String = {
        var choixAdmin = ""
        while (choixAdmin != "1" && choixAdmin != "2") {
          println("1. Mettre à jour le code PIN")
          println("2. Réapprovisionner les ingrédients")
          print("> ")
          choixAdmin = readLine()
          if (choixAdmin != "1" && choixAdmin != "2") {
            println("Entrée non valide, veuillez taper une valeur autorisée.")
          }
        }
        choixAdmin
      }

      def nouveauCode(machine: Machine): Unit = {
        var nouveauCode = ""
        do {
          println("Mise à jour du code PIN ")
          println("Entrez un nouveau code PIN à 6 chiffres")
          print(">")
          nouveauCode = readLine()
          if (nouveauCode.length != 6 || !nouveauCode.forall(_.isDigit)) {
            println("Le nouveau code doit comporter 6 chiffres.")
          }
        }
        while (nouveauCode.length != 6 || !nouveauCode.forall(_.isDigit))
        machine.codeAdmin = nouveauCode
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
      }

      def reapprovisionnerMachine(machine: Machine): Unit = {
        println("   Poudre de café : " + machine.stockCafe + "g")
        printf("   Lait : %.2f L\n", machine.stockLait.toDouble/1000)
        println("   Sucre : " + machine.stockSucre + "g")
        println("Réapprovisionnement des stocks...")
        println("Ajout : ")

        print("Poudre de café (g): ")
        var cafeAjoute = readInt()
        machine.addIngredient("stockCafe", cafeAjoute)

        print("Lait          (ml): ")
        var laitAjoute = readDouble()
        machine.addIngredient("stockLait", laitAjoute.toInt)

        print("Sucre          (g): ")
        var sucreAjoute = readInt()
        machine.addIngredient("stockSucre", sucreAjoute)

        if (cafeAjoute >= 0 && laitAjoute.toInt >= 0 && sucreAjoute >= 0) {
          println("Niveaux des stocks mis à jour.")
          println("Retour au menu principal...")
        }
      }

      def quitter(): Unit = {
        println("Vous avez quitté.")
        running = false
      }

      val mode = selectionMode()

      if (mode == "1") {
        val machine = selectionMachine
        serveClient(machine)
      }
      else if (mode == "2") {
        val machine = selectionMachine()
        if (verificationCode(machine)) {
          var adminChoix = menuAdmin()
          if (adminChoix == "1") {
            nouveauCode(machine)
          }
          else if (adminChoix == "2") {
            reapprovisionnerMachine(machine)
          }
        }
      }
      else if (mode == "3") {
        new Machine(0, "", 0, 0, 0).CSVwriting(CSV, machines)
        quitter()
      }
    }
  }
}