import scala.io.Source
import java.io.{FileWriter, PrintWriter}
import scala.util.Try

object NospressoCafe {
  val filename = "machines.csv"


  val nbMachines = 5

  case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def updateStock(ingredient: String, amount: Int): Boolean = {
      ingredient.toLowerCase match {
        case "milk" if milk + amount >= 0 => milk += amount; true
        case "sugar" if sugar + amount >= 0 => sugar += amount; true
        case "coffee" if coffee + amount >= 0 => coffee += amount; true
        case _ => println("Erreur : Quantité insuffisante ou ingrédient invalide."); false
      }
    }

    def hasSufficientStock(milkReq: Int, sugarReq: Int, coffeeReq: Int): Boolean =
      milk >= milkReq && sugar >= sugarReq && coffee >= coffeeReq

    def toCSV: String = s"$id,$pincode,$milk,$sugar,$coffee"
  }

  var machines: Array[Machine] = loadcsv(filename)

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client\n2) Admin\n3) Quitter")
      val choice = readIntSafe("> ")

      choice match {
        case 1 => clientMode()
        case 2 => adminMode()
        case 3 =>
          savecsv(filename, machines)
          running = false
        case _ => println("Option invalide. Veuillez réessayer.")
      }
    }
  }

  def clientMode(): Unit = {
    val machineId = readIntSafe("Machine sélectionnée (1-5) > ") - 1

    if (isValidMachine(machineId)) {
      val machine = machines(machineId)
      println("Chargement des machines depuis machines.csv...")
      afficherDetailsMachine(machine)

      println("""
                |Veuillez sélectionner votre boisson :
                |1) Expresso - CHF 2.00
                |2) Cappuccino - CHF 2.50
                |3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)""".stripMargin)
      val choixBoisson = readIntSafe("> ")


      val (baseMilk, baseSugar, baseCoffee, prixBoissonBase) = choixBoisson match {
        case 1 => (0, 0, 8, 2.00)
        case 2 => (10, 5, 6, 2.50)
        case 3 =>
          println("Veuillez sélectionner la taille de votre Latte :")
          println("1) Petit - 2.70 CHF")
          println("2) Moyen - 3.20 CHF")
          println("3) Grand - 3.70 CHF")
          val tailleLatte = readIntSafe("> ")
          tailleLatte match {
            case 1 => (12, 5, 6, 2.70)
            case 2 => (15, 5, 6, 3.20)
            case 3 => (18, 5, 6, 3.70)
            case _ => println("Taille invalide."); return
          }
        case _ => println("Boisson invalide."); return
      }


      println("""Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)
                |1) Oui
                |2) Non""".stripMargin)
      val extraMilkChoice = readIntSafe("> ")

      val extraMilk = if (extraMilkChoice == 1) {
        print("Nombre de doses (50ml par dose) > ")
        readIntSafe("").max(0) * 50
      } else {
        0
      }
      val prixLait = (extraMilk / 50) * 0.20


      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")

      val extraSugar = readIntSafe("> ").max(0) * 5
      val prixSucre = (extraSugar / 5) * 0.10

      val totalMilk = baseMilk + extraMilk
      val totalSugar = baseSugar + extraSugar
      val prixTotal = BigDecimal(prixBoissonBase + prixLait + prixSucre).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble


      println(f"Prix total : CHF $prixBoissonBase%.2f + CHF $prixLait%.2f + CHF $prixSucre%.2f = CHF $prixTotal%.2f")

      if (machine.hasSufficientStock(totalMilk, totalSugar, baseCoffee)) {
        machine.updateStock("milk", -totalMilk)
        machine.updateStock("sugar", -totalSugar)
        machine.updateStock("coffee", -baseCoffee)

        val codePaiement = genererCodePaiement()
        println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codePaiement \n(En attente de paiement...)")
        Thread.sleep(3000)
        println("\nPaiement confirmé..")
        println("Préparation de votre boisson...")

        println("Votre " + (choixBoisson match {
          case 1 => "Expresso"
          case 2 => "Cappuccino"
          case 3 => "Latte"
          case _ => "Boisson"
        }) + " est prêt ! Bonne dégustation !\n")
      } else {
        println("Stock insuffisant pour préparer votre commande.")
      }
    } else {
      println("Machine invalide.")
    }
  }





  def readDoubleSafe(prompt: String): Double = {
    print(prompt)
    Try(scala.io.StdIn.readDouble()).getOrElse {
      println("Entrée invalide. Veuillez entrer un nombre décimal.")
      readDoubleSafe(prompt)
    }
  }


  def afficherDetailsMachine(machine: Machine): Unit = {
    println(f"Machine ${machine.id} chargée :")
    println(f"ID: ${machine.id}")
    println(f"Code PIN: ${machine.pincode}")
    println(f"Lait: ${machine.milk / 1000.0}%.3f L")
    println(f"Sucre: ${machine.sugar}g")
    println(f"Café: ${machine.coffee}g")
  }

  def adminMode(): Unit = {
    val machineId = readIntSafe("Machine sélectionnée (1-5) > ") - 1

    if (isValidMachine(machineId)) {
      val machine = machines(machineId)
      println("Chargement des machines depuis machines.csv...")
      var attempts = 0

      while (attempts < 3) {
        val pin = readStringSafe("Entrez le code PIN :\n> ")
        if (pin == machine.pincode) {
          println(s"Accès accordé à la Machine ${machine.id}.")


          println("Niveaux de stock actuels :")
          println(f"Poudre de café : ${machine.coffee}g")
          println(f"Sucre : ${machine.sugar}g")
          println(f"Lait : ${machine.milk / 1000.0}%.1fL")


          println("\nSouhaitez-vous ajouter des stocks ?\n1) oui\n2) non")
          val stockChoix = readLine("> ").trim
          if (stockChoix == "1") {
            restockMachine(machineId)
          }


          println("\nSouhaitez-vous mettre à jour le PIN ?\n1) oui\n2) non")
          val pinChoix = readLine("> ").trim
          if (pinChoix == "1") {
            updatePin(machineId)
          }

          println("Retour au menu principal...")
          return
        } else {
          attempts += 1
          println(s"Code PIN incorrect. ${3 - attempts} tentative(s) restante(s).")
        }
      }


      println("Trop de tentatives échouées. Fin du programme.")
      sys.exit(1)
    } else {
      println("Machine invalide.")
    }
  }

  def updatePin(machineId: Int): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${machineId + 1}")
    var newPin = ""
    do {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPin = scala.io.StdIn.readLine()
    } while (!newPin.matches("\\d{6}"))
    machines(machineId).pincode = newPin
    println("Le code PIN a été mis à jour avec succès.")
  }




  def restockMachine(machineId: Int): Unit = {
    val machine = machines(machineId)
    println("Entrez les quantités à ajouter :")

    val coffee = readIntSafe("Poudre de café > ")
    val sugar = readIntSafe("Sucre > ")
    val milk = (readDoubleSafe("Lait > ") * 1000).toInt

    machine.updateStock("coffee", coffee)
    machine.updateStock("sugar", sugar)
    machine.updateStock("milk", milk)

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def demarrerProgramme(): Unit = {
    main(Array.empty)
  }





  def genererCodePaiement(): String = {
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val random = new scala.util.Random
    var codePaiement = ""

    for (_ <- 1 to 5) {
      val indexAleatoire = random.nextInt(chars.length)
      codePaiement += chars(indexAleatoire)
    }

    codePaiement
  }

  def loadcsv(filename: String): Array[Machine] = {
    println(s"Chargement des machines depuis $filename...")
    Try(Source.fromFile(filename)).map { source =>
      val lines = source.getLines().drop(1).toArray
      source.close()
      val loadedMachines = lines.zipWithIndex.map { case (line, index) =>
        val Array(pin, milk, sugar, coffee) = line.split(",")
        Machine(index + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
      }
      println(s"${loadedMachines.length} machine(s) chargée(s) avec succès.")
      loadedMachines
    }.recover {
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(1)
        Array.empty[Machine]
      case e: Exception =>
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(1)
        Array.empty[Machine]
    }.get
  }



  def savecsv(filename: String, machines: Array[Machine]): Unit = {
    println(s"Sauvegarde de ${machines.length} machine(s) dans $filename...")
    Try(new PrintWriter(new FileWriter(filename))).map { writer =>
      writer.println("ID,PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach(machine => writer.println(machine.toCSV))
      writer.close()
      println(s"Fichier sauvegardé avec succès.") 
    }.recover {
      case e: java.io.IOException =>
        println("Erreur : Échec de l’écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        System.exit(1)
      case e: Exception =>
        println("Erreur : Échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        System.exit(1)
    }
  }






  def isValidMachine(id: Int): Boolean = id >= 0 && id < nbMachines

  def readIntSafe(prompt: String): Int = {
    print(prompt)
    Try(scala.io.StdIn.readInt()).getOrElse {
      println("Entrée invalide. Veuillez entrer un nombre.")
      readIntSafe(prompt)
    }
  }

  def readStringSafe(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine().trim
  }
}