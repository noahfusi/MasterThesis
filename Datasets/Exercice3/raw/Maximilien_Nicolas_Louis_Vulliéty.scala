import scala.io.Source
import scala.io.StdIn.readLine
import java.io.{File, PrintWriter, FileNotFoundException, IOException}
import java.nio.file.AccessDeniedException
import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") milk += amount
    else if (ingredient == "sugar") sugar += amount
    else if (ingredient == "coffee") coffee += amount
    else println("Ingrédient inconnu.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println(f"Stock insuffisant pour $ingredient.")
      false
    }
  }

  def displayStocks(): Unit = {
    println(f"Stocks actuels de la machine ${id}%d:")
    println(f"- Lait: ${milk / 1000.0}%.3f L")
    println(f"- Sucre: ${sugar}%d g")
    println(f"- Café: ${coffee}%d g")
  }
}

object Main {

  val machines: ArrayBuffer[Machine] = ArrayBuffer()
  val csvFileName = "machines.csv"

  def main(args: Array[String]): Unit = {
    if (!validateCsv(csvFileName)) {
      println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    } else {
      machines ++= loadcsv(csvFileName)
      if (machines.isEmpty) {
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      } else {
        println("      Nospresso Café")
        mainMenu()
        savecsv(csvFileName, machines)
      }
    }
  }

  def validateCsv(filename: String): Boolean = {
    try {
      val sourceOption = openFile(filename)
      if (sourceOption.nonEmpty) {
        val source = sourceOption.get
        val lines = source.getLines().toArray
        source.close()
        if (lines.isEmpty) {
          println("Le fichier CSV est vide.")
          false
        } else {
          val headers = lines.head.split(",")
          if (headers.mkString(",") != "PINCODE,MILK,SUGAR,COFFEE") {
            println("Le fichier CSV contient des en-têtes incorrectes.")
            false
          } else {
            true
          }
        }
      } else {
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        false
      }
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        false
      case _: IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        false
    }
  }

  def mainMenu(): Unit = {
    var running = true
    while (running) {
      println("\nVeuillez sélectionner votre mode:")
      println("1) Client\n2) Admin\n3) Quitter\n>")
      val userRole = readIntInput(1, 3)
      if (userRole == 1) serveClient(selectMachine())
      else if (userRole == 2) adminMenu(selectMachine())
      else if (userRole == 3) running = false
    }
  }

  def selectMachine(): Machine = {
    println("Choisissez une machine :")
    for (i <- machines.indices) {
      println(f"${i + 1}%d) Machine ${machines(i).id}%d")
    }
    println(">")
    val choice = readIntInput(1, machines.size)
    machines(choice - 1)
  }

  def serveClient(machine: Machine): Unit = {
    var isOrderComplete = false
    while (!isOrderComplete) {
      println(f"\nMachine ${machine.id}%d - Choisissez votre boisson:")
      println("1) Expresso - 2.70 CHF\n2) Cappuccino - 3.20 CHF\n3) Latte (Petit - 2.70 CHF, Moyen - 3.20 CHF, Grand - 3.70 CHF)\n>")
      val choice = readIntInput(1, 3)

      val (coffeeNeeded, milkNeeded, basePrice, drinkName) =
        if (choice == 1) (8, 0, 2.70, "Expresso")
        else if (choice == 2) (6, 100, 3.20, "Cappuccino")
        else {
          val latteSize = latteSizeSelection()
          (latteSize._1, latteSize._2, latteSize._3, "Latte")
        }

      println("\nChoisissez la quantité de sucre :")
      println("1) Sans sucre (0g - Gratuit)\n2) Peu de sucre (5g - 0.10 CHF)\n3) Sucre moyen (10g - 0.20 CHF)\n4) Beaucoup de sucre (15g - 0.30 CHF)\n>")
      val sugarChoice = readIntInput(1, 4)
      val sugarNeeded = if (sugarChoice == 1) 0 else if (sugarChoice == 2) 5 else if (sugarChoice == 3) 10 else 15
      val sugarPrice = sugarNeeded / 50.0

      var milkExtra = 0
      if (choice == 2 || choice == 3) {
        println("\nVoulez-vous ajouter du lait supplémentaire ?")
        println("0) Aucun\n1) Une dose (50 mL - 0.05 CHF)\n2) Deux doses (100 mL - 0.10 CHF)\n3) Trois doses (150 mL - 0.15 CHF)\n>")
        milkExtra = readIntInput(0, 3)
      }
      val milkExtraPrice = milkExtra * 0.05

      val totalPrice = basePrice + sugarPrice + milkExtraPrice

      val ingredientsAvailable =
        machine.removeIngredient("coffee", coffeeNeeded) &&
          machine.removeIngredient("milk", milkNeeded + milkExtra * 50) &&
          machine.removeIngredient("sugar", sugarNeeded)

      if (ingredientsAvailable) {
        val twintCode = generateTwintCode()
        println(f"Prix total: $totalPrice%.2f CHF. Paiement via Twint...")
        println(f"Votre code Twint: $twintCode%s")
        Thread.sleep(2000)
        println("Paiement accepté. Préparation en cours...")
        Thread.sleep(3000)
        println(f"Votre $drinkName%s est prêt!")
        isOrderComplete = true
      } else {
        println("Commande impossible en raison d'un stock insuffisant. Veuillez choisir une autre boisson.")
      }
    }
  }

  def latteSizeSelection(): (Int, Int, Double) = {
    println("Choisissez la taille:\n1) Petit (2.70 CHF)\n2) Moyen (3.20 CHF)\n3) Grand (3.70 CHF)")
    val choice = readIntInput(1, 3)
    if (choice == 1) (6, 120, 2.70)
    else if (choice == 2) (8, 150, 3.20)
    else (12, 200, 3.70)
  }

  def adminMenu(machine: Machine): Unit = {
    if (validatePin(machine)) {
      println("Mode admin. Que voulez-vous faire ?\n1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN\n>")
      val choice = readIntInput(1, 2)
      if (choice == 1) restockMachine(machine)
      else if (choice == 2) updatePin(machine)
    } else println("Accès refusé.")
  }

  def restockMachine(machine: Machine): Unit = {
    machine.displayStocks()
    println("Quantité de lait à ajouter (en litres):\n>")
    val milkLitres = readPositiveDouble()
    machine.addIngredient("milk", (milkLitres * 1000).toInt)
    println("Quantité de sucre à ajouter (g):\n>")
    machine.addIngredient("sugar", readPositiveInt())
    println("Quantité de café à ajouter (g):\n>")
    machine.addIngredient("coffee", readPositiveInt())
    println("Stocks mis à jour!")
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez le nouveau code PIN (6 chiffres):\n>")
    var newPin = ""
    var valid = false
    while (!valid) {
      newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machine.pincode = newPin
        println("Code PIN mis à jour!")
        valid = true
      } else println("Format invalide. Réessayez.\n>")
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var attempts = 3
    var isValid = false
    while (attempts > 0 && !isValid) {
      println("Entrez le code PIN :")
      val inputPin = readLine()
      if (inputPin.length == 6 && inputPin.forall(_.isDigit) && inputPin == machine.pincode) {
        isValid = true
      } else {
        attempts -= 1
        println(f"Code PIN incorrect. Il vous reste $attempts%d tentative(s).\n>")
      }
    }
    isValid
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println(f"Chargement des machines depuis $filename...")
    val loadedMachines = ArrayBuffer[Machine]()
    try {
      val sourceOption = openFile(filename)
      if (sourceOption.nonEmpty) {
        val source = sourceOption.get
        val lines = source.getLines().toArray
        source.close()
        if (lines.nonEmpty) {
          for ((line, index) <- lines.drop(1).zipWithIndex) {
            val cols = line.split(",")
            if (cols.length == 4 && cols(0).forall(_.isDigit) && cols.tail.forall(_.forall(_.isDigit))) {
              val machine = new Machine(index + 1, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
              loadedMachines += machine
              println(f"Machine ${machine.id}%d chargée :")
              println(f"ID: ${machine.id}%d")
              println(f"Code PIN: ${machine.pincode}%s")
              println(f"Lait: ${machine.milk / 1000.0}%.3fL")
              println(f"Sucre: ${machine.sugar}%dg")
              println(f"Café: ${machine.coffee}%dg")
            } else {
              println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
            }
          }
          println(f"${loadedMachines.size}%d machine(s) chargée(s).")
        } else {
          println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        }
      } else {
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
      }
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
      case _: AccessDeniedException =>
        println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
      case _: IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
    loadedMachines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(f"Sauvegarde des machines dans $filename...")
    try {
      val file = new File(filename)
      val writer = new PrintWriter(file)
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Sauvegarde réussie.")
    } catch {
      case _: AccessDeniedException =>
        println("Erreur : Échec de l’écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule.")
      case _: IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }

  def generateTwintCode(): String = {
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    (1 to 5).map(_ => chars(scala.util.Random.nextInt(chars.length))).mkString
  }

  def readIntInput(min: Int, max: Int): Int = {
    var input = -1
    while (input < min || input > max) {
      input = readLine().toIntOption.getOrElse(-1)
      if (input < min || input > max) println(f"Entrée invalide, veuillez choisir entre $min%d et $max%d.")
    }
    input
  }

  def readPositiveInt(): Int = {
    var value = -1
    while (value < 0) {
      value = readLine().toIntOption.getOrElse(-1)
      if (value < 0) println("Veuillez entrer une valeur positive.")
    }
    value
  }

  def readPositiveDouble(): Double = {
    var value = -1.0
    while (value < 0) {
      value = readLine().toDoubleOption.getOrElse(-1.0)
      if (value < 0) println("Veuillez entrer une valeur positive.")
    }
    value
  }

  def openFile(filename: String): Option[Source] = {
    try {
      if (new File(filename).exists) Some(Source.fromFile(filename)) else None
    } catch {
      case _: FileNotFoundException => None
    }
  }
}