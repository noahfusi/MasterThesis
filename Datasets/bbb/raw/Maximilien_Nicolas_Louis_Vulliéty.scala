import scala.io.StdIn.readLine
import scala.util.Random

object Main {

  val nbMachines = 5
  val machineIds = (0 until nbMachines).toArray // Identifiants uniques pour chaque machine
  val coffeeStocks = Array.fill(nbMachines)(50) // Stocks initiaux
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(500)
  val machinePins = Array.fill(nbMachines)("434343") // Codes PIN initiaux

  def main(args: Array[String]): Unit = {
    println("Bienvenue chez Nospresso!\n")
    mainMenu()
  }

  def mainMenu(): Unit = {
    var running = true
    while (running) {
      println("\nBienvenue chez Nospresso! Choisissez votre rôle:")
      println("1) Client\n2) Admin\n3) Quitter\n>")
      val userRole = readIntInput(1, 3)

      if (userRole == 1) {
        val machineId = selectMachine()
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      } else if (userRole == 2) {
        val machineId = selectMachine()
        if (validatePin(machineId, machinePins)) adminMenu(machineId)
      } else if (userRole == 3) {
        println("Merci d'avoir utilisé Nospresso! Au revoir.")
        running = false
      }
    }
  }

  def selectMachine(): Int = {
    println("Veuillez choisir une machine (1 à 5):\n>")
    val machineNumber = readIntInput(1, nbMachines, allowQuit = false)
    val machineId = machineNumber - 1
    println(s"Vous avez sélectionné la Machine $machineNumber.")
    machineId
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var isOrderCompleted = false
    while (!isOrderCompleted) {
      println(s"\nMachine ${machineId + 1} - Choisissez votre boisson:")
      println("1) Expresso (2.00 CHF)\n2) Cappuccino (2.50 CHF)\n3) Latte (2.70-3.70 CHF)\n>")
      val choice = readIntInput(1, 3)

      val (coffeeNeeded, milkNeeded, basePrice) = if (choice == 1) {
        (8, 0, 2.00)
      } else if (choice == 2) {
        (6, 100, 2.50)
      } else {
        latteSizeSelection()
      }

      println("Voulez-vous ajouter du sucre ?")
      println("1) Sans\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n>")
      val sugarChoice = readIntInput(1, 4)
      val sugarNeeded = if (sugarChoice == 2) 5 else if (sugarChoice == 3) 10 else if (sugarChoice == 4) 15 else 0
      val sugarPrice = sugarNeeded / 50.0

      val milkExtra = if (choice == 2 || choice == 3) {
        println("Ajouter du lait supplémentaire ?")
        println("0) Aucun\n1) Une dose (50 mL)\n2) Deux doses (100 mL)\n3) Trois doses (150 mL)\n>")
        readIntInput(0, 3)
      } else 0
      val milkExtraPrice = milkExtra * 0.05

      val totalPrice = basePrice + sugarPrice + milkExtraPrice

      if (coffeeStocks(machineId) >= coffeeNeeded &&
        sugarStocks(machineId) >= sugarNeeded &&
        milkStocks(machineId) >= (milkNeeded + milkExtra * 50)) {
        println(f"Prix total: $totalPrice%.2f CHF.\nPayer via Twint...")
        Thread.sleep(2000)
        println(s"Votre code de paiement: ${generateTwintCode()}")
        Thread.sleep(3000)
        println("Paiement confirmé. Préparation en cours...")
        Thread.sleep(5000)
        coffeeStocks(machineId) -= coffeeNeeded
        sugarStocks(machineId) -= sugarNeeded
        milkStocks(machineId) -= (milkNeeded + milkExtra * 50)
        println("Votre boisson est prête. Bonne dégustation!\nRetour au menu principal...")
        isOrderCompleted = true
      } else {
        println("Stocks insuffisants pour cette commande. Veuillez choisir une autre boisson.")
      }
    }
    isOrderCompleted
  }

  def latteSizeSelection(): (Int, Int, Double) = {
    println("Choisissez la taille: \n1) Petit (2.70 CHF) \n2) Moyen (3.20 CHF) \n3) Grand (3.70 CHF)\n>")
    val size = readIntInput(1, 3)
    if (size == 1) (6, 120, 2.70)
    else if (size == 2) (8, 150, 3.20)
    else (12, 200, 3.70)
  }

  def adminMenu(machineId: Int): Unit = {
    println(s"\nMode Admin - Machine ${machineId + 1}")
    println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN\n>")
    val choice = readIntInput(1, 2)
    if (choice == 1) restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
    else if (choice == 2) updatePin(machineId, machinePins)
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"Stocks actuels de la Machine ${machineId + 1} :\n - Poudre de café: ${coffeeStocks(machineId)} g\n - Sucre: ${sugarStocks(machineId)} g\n - Lait: ${milkStocks(machineId) / 1000.0} L")
    println("Entrez les quantités à ajouter:")
    println("Poudre de café (g):\n>")
    coffeeStocks(machineId) += readPositiveInt()
    println("Sucre (g):\n>")
    sugarStocks(machineId) += readPositiveInt()
    println("Lait (L):\n>")
    val milkLitres = readPositiveDouble()
    milkStocks(machineId) += (milkLitres * 1000).toInt // Conversion litres en millilitres
    println("Stocks mis à jour avec succès!\nRetour au menu principal...")
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}\nEntrez un nouveau code PIN (6 chiffres):\n>")
    var newPin = ""
    do {
      newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.\nRetour au menu principal...")
      } else {
        println("Format invalide. Entrez exactement 6 chiffres.")
      }
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la Machine ${machineId + 1}:\n> ")
      val inputPin = readLine()
      if (inputPin.length == 6 && inputPin == machinePins(machineId)) {
        println("Accès accordé.")
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. ${attempts} tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Retour au menu principal...")
    false
  }

  def generateTwintCode(): String = Random.alphanumeric.take(5).mkString

  def readIntInput(min: Int, max: Int, allowQuit: Boolean = false): Int = {
    var input = -1
    do {
      val line = readLine()
      if (allowQuit && line == "-1") return -1
      input = if (line.forall(_.isDigit)) line.toInt else -1
      if (input < min || input > max) println(s"Entrée invalide. Veuillez entrer un nombre entre $min et $max.")
    } while (input < min || input > max)
    input
  }

  def readPositiveInt(): Int = {
    var value = -1
    do {
      val line = readLine()
      value = if (line.forall(_.isDigit)) line.toInt else -1
      if (value < 0) println("Entrez une valeur positive.")
    } while (value < 0)
    value
  }

  def readPositiveDouble(): Double = {
    var value = -1.0
    do {
      val line = readLine()
      value = if (line.forall(c => c.isDigit || c == '.')) line.toDouble else -1.0
      if (value < 0) println("Entrez une valeur positive.")
    } while (value < 0)
    value
  }
}