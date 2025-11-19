
import scala.io.StdIn.readLine



object Main {



  val nbMachines = 5

  val machinePins = Array.fill(nbMachines)("434343")

  val coffeeStocks = Array.fill(nbMachines)(50)

  val sugarStocks = Array.fill(nbMachines)(30)

  val milkStocks = Array.fill(nbMachines)(500)



  def main(args: Array[String]): Unit = {

    var continuer = true



    while (continuer) {

      println("\n        Nospresso Café        ")

      println("Veuillez sélectionner votre mode :")

      println("1) Client")

      println("2) Admin")

      println("3) Quitter")

      print("> ")



      val modeInput = readLine()

      var mode = 0



      if (modeInput != "") {

        var valid = true

        for (c <- modeInput) {

          if (c < '0' || c > '9') valid = false

        }

        if (valid) {

          mode = modeInput.toInt

        } else {

          mode = 0

        }

      }



      if (mode == 1) {

        println("Sélectionnez une machine (1-" + nbMachines + ") :")

        val machineId = selectMachine()

        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)

      } else if (mode == 2) {

        println("Sélectionnez une machine (1-" + nbMachines + ") :")

        val machineId = selectMachine()

        if (validatePin(machineId, machinePins)) {

          adminMenu(machineId, coffeeStocks, sugarStocks, milkStocks)

        } else {

          println("Trop de tentatives échouées. Fin du programme.")

          continuer = false

        }

      } else if (mode == 3) {

        println("Au revoir !")

        continuer = false

      } else {

        println("Choix invalide. Veuillez réessayer.")

      }

    }

  }



  def selectMachine(): Int = {

    var machineId = -1

    while (machineId < 0 || machineId >= nbMachines) {

      print("> ")

      val input = readLine()

      if (input != "") {

        var valid = true

        for (c <- input) {

          if (c < '0' || c > '9') valid = false

        }

        if (valid) {

          machineId = input.toInt - 1

        } else {

          machineId = -1

        }

      }

      if (machineId < 0 || machineId >= nbMachines) {

        println("Machine invalide. Veuillez réessayer.")

      }

    }

    machineId

  }



  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {

    var attempts = 3

    while (attempts > 0) {

      println(s"Entrez le code PIN pour la Machine ${machineId + 1} :")

      print("> ")

      val pinInput = readLine()



      if (pinInput == machinePins(machineId)) {

        println("Accès accordé.")

        return true

      } else {

        attempts -= 1

        println(s"Code PIN incorrect. ${attempts} tentative(s) restante(s).")

      }

    }

    false

  }



  def adminMenu(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    println("Mode Administrateur")

    println("1) Réapprovisionner les stocks")

    println("2) Mettre à jour le code PIN")

    print("> ")



    val choiceInput = readLine()

    var choice = 0



    if (choiceInput != "") {

      var valid = true

      for (c <- choiceInput) {

        if (c < '0' || c > '9') valid = false

      }

      if (valid) {

        choice = choiceInput.toInt

      } else {

        choice = 0

      }

    }



    if (choice == 1) {

      restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)

    } else if (choice == 2) {

      updatePin(machineId, machinePins)

    } else {

      println("Retour au menu principal.")

    }

  }



  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    println(s"Stocks actuels pour la Machine ${machineId + 1} :")

    println(s"Café : ${coffeeStocks(machineId)}g, Sucre : ${sugarStocks(machineId)}g, Lait : ${milkStocks(machineId)}ml")



    println("Entrez la quantité de café à ajouter (g) :")

    val coffeeInput = readLine()

    var coffeeAmount = 0

    if (coffeeInput != "") {

      var valid = true

      for (c <- coffeeInput) {

        if (c < '0' || c > '9') valid = false

      }

      if (valid) {

        coffeeAmount = coffeeInput.toInt

      }

    }

    coffeeStocks(machineId) += coffeeAmount



    println("Entrez la quantité de sucre à ajouter (g) :")

    val sugarInput = readLine()

    var sugarAmount = 0

    if (sugarInput != "") {

      var valid = true

      for (c <- sugarInput) {

        if (c < '0' || c > '9') valid = false

      }

      if (valid) {

        sugarAmount = sugarInput.toInt

      }

    }

    sugarStocks(machineId) += sugarAmount



    println("Entrez la quantité de lait à ajouter (ml) :")

    val milkInput = readLine()

    var milkAmount = 0

    if (milkInput != "") {

      var valid = true

      for (c <- milkInput) {

        if (c < '0' || c > '9') valid = false

      }

      if (valid) {

        milkAmount = milkInput.toInt

      }

    }

    milkStocks(machineId) += milkAmount



    println(s"Stocks mis à jour : Café : ${coffeeStocks(machineId)}g, Sucre : ${sugarStocks(machineId)}g, Lait : ${milkStocks(machineId)}ml")

  }



  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}")

    var newPin = ""

    while (newPin.length != 6 || !isAllDigits(newPin)) {

      println("Entrez un nouveau code PIN à 6 chiffres :")

      newPin = readLine()

      if (newPin.length != 6 || !isAllDigits(newPin)) {

        println("Code PIN invalide. Veuillez réessayer.")

      }

    }

    machinePins(machineId) = newPin

    println(s"Le code PIN pour la Machine ${machineId + 1} a été mis à jour.")

  }



  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    println(s"Machine ${machineId + 1} : Mode Client")

    println("Veuillez sélectionner votre boisson :")

    println("1) Expresso - CHF 2.00")

    println("2) Cappuccino - CHF 2.50")

    println("3) Latte")

    print("> ")



    val choiceInput = readLine()

    var choice = 0



    if (choiceInput != "") {

      var valid = true

      for (c <- choiceInput) {

        if (c < '0' || c > '9') valid = false

      }

      if (valid) {

        choice = choiceInput.toInt

      }

    }



    if (choice == 1) {

      if (coffeeStocks(machineId) >= 8) {

        coffeeStocks(machineId) -= 8

        println("Votre Expresso est prêt ! Bonne dégustation !")

        true

      } else {

        println("Stock de café insuffisant.")

        false

      }

    } else if (choice == 2) {

      if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {

        coffeeStocks(machineId) -= 6

        milkStocks(machineId) -= 100

        println("Votre Cappuccino est prêt ! Bonne dégustation !")

        true

      } else {

        println("Ingrédients insuffisants.")

        false

      }

    } else if (choice == 3) {

      println("Veuillez choisir une taille :")

      println("1) Petit (6g café, 120ml lait)")

      println("2) Moyen (8g café, 150ml lait)")

      println("3) Grand (12g café, 200ml lait)")

      print("> ")



      val sizeInput = readLine()

      var size = 0



      if (sizeInput != "") {

        var valid = true

        for (c <- sizeInput) {

          if (c < '0' || c > '9') valid = false

        }

        if (valid) {

          size = sizeInput.toInt

        }

      }



      var coffeeReq = 0

      var milkReq = 0



      if (size == 1) {

        coffeeReq = 6

        milkReq = 120

      } else if (size == 2) {

        coffeeReq = 8

        milkReq = 150

      } else if (size == 3) {

        coffeeReq = 12

        milkReq = 200

      } else {

        println("Taille invalide.")

        return false

      }



      if (coffeeStocks(machineId) >= coffeeReq && milkStocks(machineId) >= milkReq) {

        coffeeStocks(machineId) -= coffeeReq

        milkStocks(machineId) -= milkReq

        println(s"Votre Latte ${size} est prêt ! Bonne dégustation !")

        true

      } else {

        println("Ingrédients insuffisants.")

        false

      }

    } else {

      println("Choix invalide.")

      false

    }

  }



  def isAllDigits(s: String): Boolean = {

    for (c <- s) {

      if (c < '0' || c > '9') {

        return false

      }

    }

    true

  }

}