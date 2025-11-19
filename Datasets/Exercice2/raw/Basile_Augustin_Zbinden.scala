import scala.io.StdIn.readLine
import scala.util.Random

object NospressoCafe {

  // Quantités initiales et constantes
  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50)
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(500)

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      readLine().trim match {
        case "1" => modeClient()
        case "2" => modeAdmin()
        case "3" =>
          println("Merci d'avoir utilisé Nospresso Café. Au revoir !")
          System.exit(0)
        case _ => println("Entrée invalide. Veuillez réessayer.")
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      print("Entrez le code PIN : ")
      val pinEntre = readLine().trim
      if (pinEntre == machinePins(machineId - 1)) {
        println("Accès autorisé.")
        return true
      } else {
        attempts -= 1
        if (attempts > 0) println(s"Code PIN incorrect. $attempts tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var valid = false
    while (!valid) {
      print("Entrez un nouveau code PIN à 6 chiffres : ")
      val newPin = readLine().trim
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId - 1) = newPin
        println("Code PIN mis à jour avec succès.")
        valid = true
      } else {
        println("Code PIN invalide. Veuillez réessayer.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println(s"Machine $machineId : Veuillez sélectionner votre boisson :")
    println("1) Expresso - 8g café, 0 lait")
    println("2) Cappuccino - 6g café, 100ml lait")
    println("3) Latte - 6g café, 120ml lait")
    print("> ")
    val choice = readLine().trim
    val (cafe, lait) = choice match {
      case "1" => (8, 0)
      case "2" => (6, 100)
      case "3" => (6, 120)
      case _ =>
        println("Entrée invalide. Retour au menu principal.")
        return false
    }

    if (coffeeStocks(machineId - 1) >= cafe && milkStocks(machineId - 1) >= lait) {
      coffeeStocks(machineId - 1) -= cafe
      milkStocks(machineId - 1) -= lait
      println(s"Votre boisson a été préparée avec succès !")
      true
    } else {
      println("Stock insuffisant pour préparer cette boisson.")
      false
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"Machine $machineId : Reapprovisionnement des stocks.")
    print("Ajout de café (g) : ")
    coffeeStocks(machineId - 1) += readLine().toIntOption.getOrElse(0)
    print("Ajout de sucre (g) : ")
    sugarStocks(machineId - 1) += readLine().toIntOption.getOrElse(0)
    print("Ajout de lait (ml) : ")
    milkStocks(machineId - 1) += readLine().toIntOption.getOrElse(0)
    println("Stocks mis à jour avec succès.")
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner une machine (1-5) :")
    print("> ")
    val machineId = readLine().toIntOption.getOrElse(-1)
    if (machineId >= 1 && machineId <= nbMachines) {
      serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
    } else {
      println("Machine invalide. Retour au menu principal.")
    }
  }

  def modeAdmin(): Unit = {
    println("Veuillez sélectionner une machine (1-5) :")
    print("> ")
    val machineId = readLine().toIntOption.getOrElse(-1)
    if (machineId >= 1 && machineId <= nbMachines) {
      if (validatePin(machineId, machinePins)) {
        println("1) Reapprovisionner les stocks")
        println("2) Mettre à jour le code PIN")
        print("> ")
        readLine().trim match {
          case "1" => restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          case "2" => updatePin(machineId, machinePins)
          case _ => println("Entrée invalide. Retour au menu principal.")
        }
      }
    } else {
      println("Machine invalide.")
    }
  }
}
