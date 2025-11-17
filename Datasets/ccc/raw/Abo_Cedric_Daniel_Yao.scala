object Main {

  import scala.io.StdIn._
  import scala.collection.mutable.ArrayBuffer
  import scala.io.Source
  import java.io.{File, PrintWriter}
  import scala.util.{Try, Success, Failure}

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println("Ingrédient invalide.")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient.toLowerCase match {
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
          println("Quantité insuffisante ou ingrédient invalide.")
          false
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    Try {
      for (line <- Source.fromFile(filename).getLines().drop(1)) {
        val cols = line.split(",").map(_.trim)
        val machine = new Machine(
          id = machines.length + 1, // Assignation automatique des ID
          pincode = cols(0),
          milk = cols(1).toInt,
          sugar = cols(2).toInt,
          coffee = cols(3).toInt
        )
        machines += machine
      }
    } match {
      case Success(_) => println(s"${machines.length} machine(s) chargée(s) avec succès.")
      case Failure(e) => println(s"Erreur lors du chargement du fichier CSV: ${e.getMessage}")
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    Try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
    } match {
      case Success(_) => println("Fichier sauvegardé avec succès.")
      case Failure(e) => println(s"Erreur lors de la sauvegarde du fichier CSV: ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    // Charger les machines depuis le fichier CSV
    val machines = loadcsv("machines.csv")

    // Menu principal
    var mode: Int = 0
    while (mode != 3) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      mode = readInt()

      if (mode == 1) {
        // Mode Client
        println("Veuillez sélectionner une machine :")
        machines.zipWithIndex.foreach { case (machine, index) =>
          println(s"${index + 1}) Machine ${machine.id}")
        }
        print("> ")

        val machineIndex = readInt() - 1

        if (machineIndex < 0 || machineIndex >= machines.length) {
          println("Identifiant de machine invalide.")
        } else {
          serveClient(machineIndex, machines)
        }

      } else if (mode == 2) {
        // Mode Admin
        println("Mode Admin")
        println("Veuillez sélectionner une machine :")
        machines.zipWithIndex.foreach { case (machine, index) =>
          println(s"${index + 1}) Machine ${machine.id}")
        }
        print("> ")

        val machineIndex = readInt() - 1

        if (machineIndex < 0 || machineIndex >= machines.length) {
          println("Identifiant de machine invalide.")
        } else {
          if (validatePin(machineIndex, machines)) {
            println("Accès autorisé.")
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            print("> ")

            val choixAdmin = readInt()

            if (choixAdmin == 1) {
              restockMachine(machineIndex, machines)
            } else if (choixAdmin == 2) {
              updatePin(machineIndex, machines)
            } else {
              println("Choix invalide.")
            }
          } else {
            println("Code PIN incorrect. Accès refusé.")
          }
        }
      }
    }

    // Sauvegarder les machines dans le fichier CSV
    savecsv("machines.csv", machines)

    println("Merci d'avoir utilisé le distributeur Nospresso Café. À bientôt !")
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val choixBoisson = readInt()

    var prixFinal = BigDecimal(0)
    var boisson = ""
    var cafeNecessaire = 0
    var laitNecessaire = 0

    choixBoisson match {
      case 1 =>
        boisson = "Expresso"
        prixFinal = 2.0
        cafeNecessaire = 8
      case 2 =>
        boisson = "Cappuccino"
        prixFinal = 2.5
        cafeNecessaire = 6
        laitNecessaire = 100
      case 3 =>
        println("Veuillez sélectionner la taille de votre Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")

        readInt() match {
          case 1 =>
            boisson = "Latte (Petit)"
            prixFinal = 2.7
            cafeNecessaire = 6
            laitNecessaire = 120
          case 2 =>
            boisson = "Latte (Moyen)"
            prixFinal = 3.2
            cafeNecessaire = 8
            laitNecessaire = 150
          case 3 =>
            boisson = "Latte (Grand)"
            prixFinal = 3.7
            cafeNecessaire = 12
            laitNecessaire = 200
          case _ =>
            println("Entrée invalide.")
            return
        }
      case _ =>
        println("Entrée invalide.")
        return
    }

    println(s"Boisson sélectionnée : $boisson")
    println(s"Prix de la boisson : $prixFinal CHF")

    // Gestion du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val sucreNecessaire = readInt() match {
      case 1 => 0
      case 2 =>
        prixFinal += 0.10
        5
      case 3 =>
        prixFinal += 0.20
        10
      case 4 =>
        prixFinal += 0.30
        15
      case _ =>
        println("Entrée invalide.")
        return
    }

    // Vérification des stocks
    if (machine.coffee >= cafeNecessaire && machine.sugar >= sucreNecessaire && machine.milk >= laitNecessaire) {
      // Mise à jour des stocks
      machine.removeIngredient("coffee", cafeNecessaire)
      machine.removeIngredient("sugar", sucreNecessaire)
      machine.removeIngredient("milk", laitNecessaire)

      // Simulation du paiement et de la préparation
      println(s"Prix total : $prixFinal CHF")
      println("Veuillez payer en utilisant Twint.")
      Thread.sleep(3000)
      println("Paiement confirmé.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre boisson est prête ! Bonne dégustation !")

    } else {
      println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez vérifier les stocks ou choisir une autre boisson.")
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val machine = machines(machineId)
    var attempts = 0
    while (attempts < 3) {
      print("Entrez le code PIN : ")

      if (readLine() == machine.pincode) return true
      println("Code PIN incorrect.")
      attempts += 1
    }
    false
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    print("Entrez le nouveau code PIN (6 chiffres) : ")
    val newPin = readLine()
    if (newPin.length == 6 && newPin.forall(_.isDigit)) {
      machine.pincode = newPin
      println("Code PIN mis à jour avec succès.")
    } else {
      println("Entrée invalide. Le code PIN doit comporter exactement 6 chiffres.")
    }
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    println("Réapprovisionnement des stocks...")
    println("Quel élément souhaitez-vous réapprovisionner ?")
    println("1) Poudre de café")
    println("2) Sucre")
    println("3) Lait")
    print("> ")

    readInt() match {
      case 1 =>
        println("Veuillez entrer la quantité de poudre de café à ajouter (en grammes) : ")
        machine.addIngredient("coffee", readInt())
        println(s"Stocks mis à jour : ${machine.coffee} g de café")
      case 2 =>
        println("Veuillez entrer la quantité de sucre à ajouter (en grammes) : ")
        machine.addIngredient("sugar", readInt())
        println(s"Stocks mis à jour : ${machine.sugar} g de sucre")
      case 3 =>
        println("Veuillez entrer la quantité de lait à ajouter (en millilitres) : ")
        machine.addIngredient("milk", readInt())
        println(s"Stocks mis à jour : ${machine.milk} ml de lait")
      case _ => println("Choix invalide.")
    }
  }
}