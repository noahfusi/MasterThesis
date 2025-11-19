import scala.io.StdIn.readLine

object Nospresso {

  val nbMachines = 5

  // Stocks initiaux (valeurs simples pour l'exemple)
  val coffeeStocks = Array(50, 50, 50, 50, 50)
  val sugarStocks  = Array(30, 30, 30, 30, 30)
  val milkStocks   = Array(20, 20, 20, 20, 20)

  // PIN initiaux
  val machinePins = Array.fill(nbMachines)("434343")

  def main(args: Array[String]): Unit = {
    var running = true

    while (running) {
      println("=== Nospresso ===")
      println("1. Mode Client")
      println("2. Mode Administrateur")
      println("3. Quitter")
      print("> ")

      val choix = readLine()

      if (choix == "1") {
        runClientMode()
      } else if (choix == "2") {
        runAdminMode()
      } else if (choix == "3") {
        println("Fin du programme.")
        running = false
      } else {
        println("Choix invalide.")
      }
    }
  }

  // ------------------------------
  // -------- MODE CLIENT ---------
  // ------------------------------
  def runClientMode(): Unit = {
    val id = selectMachine()
    if (id < 0) return

    val ok = serveClient(id, coffeeStocks, sugarStocks, milkStocks)
    if (!ok) {
      println("Transaction échouée.")
    }
  }

  // ------------------------------
  // ------- MODE ADMIN -----------
  // ------------------------------
  def runAdminMode(): Unit = {
    val id = selectMachine()
    if (id < 0) return

    val valid = validatePin(id, machinePins)
    if (!valid) {
      println("Trop de tentatives échouées. Fin du programme.")
      System.exit(0)
    }

    var back = false
    while (!back) {
      println(s"\n=== Admin Machine $id ===")
      println("1. Réapprovisionner")
      println("2. Mettre à jour le PIN")
      println("3. Retour au menu principal")
      print("> ")

      readLine() match {
        case "1" => restockMachine(id, coffeeStocks, sugarStocks, milkStocks)
        case "2" => updatePin(id, machinePins)
        case "3" => back = true
        case _   => println("Choix invalide.")
      }
    }
  }

  // ------------------------------
  // -------- SELECT MACHINE ------
  // ------------------------------
  def selectMachine(): Int = {
    println(s"Sélectionnez une machine (0 à ${nbMachines - 1})")
    print("> ")
    val s = readLine()
    if (!isInt(s)) return -1
    val id = s.toInt
    if (id < 0 || id >= nbMachines) {
      println("ID invalide.")
      return -1
    }
    id
  }

  // ------------------------------
  // -------- VALIDATE PIN --------
  // ------------------------------
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3

    while (attempts > 0) {
      println("Entrez le code PIN :")
      print("> ")
      val pin = readLine()

      if (pin == machinePins(machineId)) {
        println("Accès accordé.")
        return true
      } else {
        attempts -= 1
        println("Code PIN incorrect. Tentatives restantes : " + attempts)
      }
    }
    false
  }

  // ------------------------------
  // -------- UPDATE PIN ----------
  // ------------------------------
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du PIN pour la machine $machineId.")

    var ok = false
    while (!ok) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      print("> ")
      val newPin = readLine()

      if (newPin.length == 6 && isInt(newPin)) {
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.")
        ok = true
      } else {
        println("Format invalide.")
      }
    }
  }

  // ------------------------------
  // -------- SERVE CLIENT --------
  // ------------------------------
  def serveClient(
    machineId: Int,
    coffeeStocks: Array[Int],
    sugarStocks: Array[Int],
    milkStocks: Array[Int]
  ): Boolean = {

    println(s"Machine $machineId : Commandez votre boisson.")
    println("1. Espresso (10g café)")
    println("2. Café sucré (10g café, 5g sucre)")
    println("3. Latte (10g café, 5g sucre, 5ml lait)")
    print("> ")

    val choice = readLine()

    var needCoffee = 0
    var needSugar  = 0
    var needMilk   = 0

    if (choice == "1") {
      needCoffee = 10
    } else if (choice == "2") {
      needCoffee = 10
      needSugar = 5
    } else if (choice == "3") {
      needCoffee = 10
      needSugar = 5
      needMilk = 5
    } else {
      println("Choix invalide.")
      return false
    }

    if (coffeeStocks(machineId) < needCoffee ||
        sugarStocks(machineId) < needSugar ||
        milkStocks(machineId) < needMilk) {

      println("Stock insuffisant.")
      return false
    }

    println("Paiement via Twint (simulé)... OK.")

    coffeeStocks(machineId) -= needCoffee
    sugarStocks(machineId)  -= needSugar
    milkStocks(machineId)   -= needMilk

    println("Votre boisson est prête !")
    true
  }

  // ------------------------------
  // ------- RESTOCK MACHINE ------
  // ------------------------------
  def restockMachine(
    machineId: Int,
    coffeeStocks: Array[Int],
    sugarStocks: Array[Int],
    milkStocks: Array[Int]
  ): Unit = {

    println("Réapprovisionnement des stocks.")

    print("Ajouter café (g) : ")
    val c = readLine()
    print("Ajouter sucre (g) : ")
    val s = readLine()
    print("Ajouter lait (ml) : ")
    val m = readLine()

    if (isInt(c) && isInt(s) && isInt(m)) {
      coffeeStocks(machineId) += c.toInt
      sugarStocks(machineId)  += s.toInt
      milkStocks(machineId)   += m.toInt
      println("Stocks mis à jour.")
    } else {
      println("Valeurs invalides.")
    }
  }

  // ------------------------------
  // ------- UTIL : INT CHECK -----
  // ------------------------------
  def isInt(str: String): Boolean = {
    var i = 0
    while (i < str.length) {
      if (str.charAt(i) < '0' || str.charAt(i) > '9') return false
      i += 1
    }
    if (str.length == 0) return false
    true
  }
}
