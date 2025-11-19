import scala.io.StdIn._
import scala.util.Random

object VendingMachineManager {

  // Gérer les stocks pour une machine spécifique
  def manageStocks(machineIndex: Int, coffeeLevels: Array[Int], sugarLevels: Array[Int], milkLevels: Array[Int]): Unit = {
    println("Stock actuel :")
    val idx = machineIndex - 1
    println(s"   Café : ${coffeeLevels(idx)} g")
    println(f"   Lait : ${milkLevels(idx) / 1000.0}%.2f L")
    println(s"   Sucre : ${sugarLevels(idx)} g")
    println("Voulez-vous recharger les stocks ? (1: Oui, 2: Non)")
    var option = 0
    do {
      option = readInt()
    } while (option != 1 && option != 2)
    if (option == 1) {
      // Mise à jour des stocks
      print("Quantité de café à ajouter (g) : ")
      val coffeeToAdd = math.max(readInt(), 0)
      print("Quantité de lait à ajouter (L) : ")
      val milkToAdd = math.max(readDouble(), 0.0)
      print("Quantité de sucre à ajouter (g) : ")
      val sugarToAdd = math.max(readInt(), 0)

      coffeeLevels(idx) += coffeeToAdd
      milkLevels(idx) += (milkToAdd * 1000).toInt
      sugarLevels(idx) += sugarToAdd

      println("Stocks mis à jour avec succès.")
    }
  }

  // Validation du code PIN pour une machine
  def checkPin(machineIndex: Int, pinCodes: Array[String]): Boolean = {
    val enteredPin = readLine("Veuillez entrer le code PIN : ")
    pinCodes(machineIndex - 1) == enteredPin
  }

  // Mise à jour du code PIN
  def changePin(machineIndex: Int, pinCodes: Array[String]): Unit = {
    val idx = machineIndex - 1
    var newPin = ""
    do {
      newPin = readLine("Entrez un nouveau code PIN à 6 chiffres : ")
    } while (newPin.length != 6)
    pinCodes(idx) = newPin
    println("Le code PIN a été modifié avec succès.")
  }

  // Préparer une boisson pour le client
  def prepareDrink(machineIndex: Int, coffeeLevels: Array[Int], sugarLevels: Array[Int], milkLevels: Array[Int]): Boolean = {
    println("Choisissez une boisson :")
    println("1) Expresso\n2) Cappuccino\n3) Latte")
    var selection = 0
    do {
      selection = readInt()
    } while (selection < 1 || selection > 3)

    // Déterminer les quantités nécessaires pour la boisson
    val (requiredCoffee, requiredMilk, price) = selection match {
      case 1 => (8, 0, 2.0) // Expresso
      case 2 => (6, 100, 2.5) // Cappuccino
      case 3 => (12, 200, 3.2) // Latte
    }

    val idx = machineIndex - 1
    if (coffeeLevels(idx) >= requiredCoffee && milkLevels(idx) >= requiredMilk) {
      coffeeLevels(idx) -= requiredCoffee
      milkLevels(idx) -= requiredMilk
      println(f"Merci pour votre achat. Votre boisson est prête ! Prix : $$${price}%.2f")
      false
    } else {
      println("Désolé, stocks insuffisants pour préparer la boisson.")
      true
    }
  }

  def main(args: Array[String]): Unit = {
    val totalMachines = 5
    val coffeeLevels = Array.fill(totalMachines)(50) // Café initial en grammes
    val milkLevels = Array.fill(totalMachines)(500) // Lait initial en millilitres
    val sugarLevels = Array.fill(totalMachines)(30) // Sucre initial en grammes
    val pinCodes = Array.fill(totalMachines)("123456") // PIN initial

    var isRunning = true
    while (isRunning) {
      println("Sélectionnez le mode :")
      println("1) Client\n2) Administrateur\n3) Quitter")
      val mode = readInt()
      if (mode == 3) isRunning = false
      else {
        println("Choisissez une machine (1 à 5) :")
        val machineIndex = readInt()
        if (mode == 1) {
          prepareDrink(machineIndex, coffeeLevels, sugarLevels, milkLevels)
        } else if (mode == 2) {
          if (checkPin(machineIndex, pinCodes)) {
            println("1) Gérer les stocks\n2) Modifier le code PIN")
            val adminChoice = readInt()
            if (adminChoice == 1) {
              manageStocks(machineIndex, coffeeLevels, sugarLevels, milkLevels)
            } else {
              changePin(machineIndex, pinCodes)
            }
          } else {
            println("Code PIN incorrect.")
          }
        }
      }
    }
    println("Merci d'avoir utilisé notre système.")
  }
}
