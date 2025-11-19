
import scala.io.StdIn._
import scala.util.Random

object NospressoSimple {
  // Nombre de machines disponibles
  val nbMachines = 5

  // Initialisation des stocks pour chaques machines
  // quantités en grammes
  var coffeeStocks = Array.fill(nbMachines)(50.0)// stock café
  var sugarStocks = Array.fill(nbMachines)(30.0)//stock sucre
  //quantité en litres
  var milkStocks = Array.fill(nbMachines)(0.5)// stock lait
  //code PIN pour les machines
  var machinePins = Array.fill(nbMachines)("434343")

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      // Menu principal
      println("\n=== Nospresso Café ===")
      println("1) Mode Client")
      println("2) Mode Administrateur")
      println("3) Quitter\n>")
      val choice = readLine()

      //Choix du mode d'utilisation
      if (choice == "1") clientMode()// choix du mode client
      else if (choice == "2") adminMode()//choix du mode admin
      else if (choice == "3") running = false//choix quitter le programme
      else println("Choix invalide, veillez réessayer:")
    }
    // Message de fin de programme
    println("Merci d'avoir utilisé Nospresso. Au revoir.")
  }

  def clientMode(): Unit = {
    var machineId = selectValidMachine()
    // sélection d'une machine
    printf("Bienvenue au mode client pour la machine %d.\n", machineId + 1)

    // Menu boisson
    println("1) Expresso (CHF 2.00)")
    println("2) Cappuccino (CHF 2.50)")
    println("3) Latte (Petit: CHF 2.70, Moyen: CHF 3.20, Grand: CHF 3.70)")
    val drinkChoice = getValidChoice(List("1", "2", "3"))

    //initialisation par défaut pour les boissons
    var coffeeNeeded = 0.0
    var milkNeeded = 0.0
    var price = 0.0

    //Calcul des besoins en quantité et prix selon le choix
    //Expresso
    if (drinkChoice == "1") {
      coffeeNeeded = 8.0
      milkNeeded = 0.0
      price = 2.0
      //Cappucino
    } else if (drinkChoice == "2") {
      coffeeNeeded = 6.0
      milkNeeded = 0.1
      price = 2.5
      //Latte
    } else if (drinkChoice == "3") {
      println("Choisissez la taille du Latte : 1) Petit 2) Moyen 3) Grand")
      val sizeChoice = getValidChoice(List("1", "2", "3"))
      if (sizeChoice == "1") {
        coffeeNeeded = 6.0
        milkNeeded = 0.12
        price = 2.7
      } else if (sizeChoice == "2") {
        coffeeNeeded = 8.0
        milkNeeded = 0.15
        price = 3.2
      } else if (sizeChoice == "3") {
        coffeeNeeded = 12.0
        milkNeeded = 0.2
        price = 3.7
      }
    }

    //choix d'ajout de sucre
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    val sugarChoice = getValidChoice(List("1", "2", "3", "4"))

    var sugarNeeded = 0.0
    var sugarPrice = 0.0

    //calcul quantité et prix sucre
    if (sugarChoice == "1") {
      sugarNeeded = 0.0
      sugarPrice = 0.0
    } else if (sugarChoice == "2") {
      sugarNeeded = 5.0
      sugarPrice = 0.1
    } else if (sugarChoice == "3") {
      sugarNeeded = 10.0
      sugarPrice = 0.2
    } else if (sugarChoice == "4") {
      sugarNeeded = 15.0
      sugarPrice = 0.3
    }

    //choix d'ajout lait
    if (drinkChoice == "2" || drinkChoice == "3") { // Lait supplémentaire uniquement pour Cappuccino et Latte
      println("Souhaitez-vous du lait supplémentaire ?")
      println("1) Oui")
      println("2) Non")
      val extraMilkChoice = getValidChoice(List("1", "2"))

      if (extraMilkChoice == "1") {
        println("Choisissez la dose de lait supplémentaire (1: 0.05L - CHF 0.05, 2: 0.10L - CHF 0.10, 3: 0.15L - CHF 0.15)")
        val doseChoice = getValidChoice(List("1", "2", "3"))
        if (doseChoice == "1") {
          milkNeeded += 0.05
          price += 0.05
        } else if (doseChoice == "2") {
          milkNeeded += 0.1
          price += 0.1
        } else if (doseChoice == "3") {
          milkNeeded += 0.15
          price += 0.15
        }
      }
    }

    //calcul prix total
    val totalPrice = price + sugarPrice
    serveClient(machineId, coffeeNeeded, sugarNeeded, milkNeeded, totalPrice, drinkChoice, sugarChoice)
  }

  def serveClient(machineId: Int, coffeeNeeded: Double, sugarNeeded: Double, milkNeeded: Double, totalPrice: Double, drinkChoice: String, sugarChoice: String): Unit = {
    //verification stocks avant préparation café
    if (coffeeStocks(machineId) >= coffeeNeeded && sugarStocks(machineId) >= sugarNeeded && milkStocks(machineId) >= milkNeeded) {
      //mise à jour stock apres préparation
      coffeeStocks(machineId) -= coffeeNeeded
      sugarStocks(machineId) -= sugarNeeded
      milkStocks(machineId) -= milkNeeded
      //paiement
      printf("Prix total : CHF %.2f. Merci de payer via Twint\nvotre code de paiement est : %s\n", totalPrice, generatePaymentCode())
      Thread.sleep(3000) // Simulation du paiement
      println("Paiement confirmé.")
      prepareDrink(drinkChoice, sugarChoice, milkNeeded > 0)
    } else {
      println("Stock insuffisant dans cette machine.")
      Thread.sleep(500)
      //recherche des autres machines avec stock suffisant
      val remainingMachines = (0 until nbMachines).filter(id =>
        id != machineId && coffeeStocks(id) >= coffeeNeeded && sugarStocks(id) >= sugarNeeded && milkStocks(id) >= milkNeeded
      )

      if (remainingMachines.nonEmpty) {
        println("Voici les autres machines disponibles :")
        remainingMachines.foreach(id => println(s"Machine ${id + 1}"))
        println("Veuillez choisir une autre machine parmi celles disponibles :")
        val newMachineId = selectValidMachineFromList(remainingMachines.map(_ + 1).toList)
        serveClient(newMachineId - 1, coffeeNeeded, sugarNeeded, milkNeeded, totalPrice, drinkChoice, sugarChoice)
      } else {
        println("Aucune machine ne dispose de suffisamment de stock pour préparer votre boisson. Désolé pour le désagrément.")
      }
    }
  }

  def prepareDrink(drink: String, sugar: String, withMilk: Boolean): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000)//simulation de temps de préparation
    val drinkName = if (drink == "1") "Expresso"
    else if (drink == "2") "Cappuccino"
    else if (drink == "3") "Latte"
    else "Boisson inconnue"
    printf("Votre %s est prêt ! Bonne dégustation !\n", drinkName)
    Thread.sleep(2000)//simulation temps avant de servir
  }

  def selectValidMachineFromList(availableMachines: List[Int]): Int = {
    var machineId = -1
    //selection d'une machines parmis celle disponible
    while (!availableMachines.contains(machineId)) {
      printf("Entrez le numéro de machine (%s) : ", availableMachines.mkString(", "))
      machineId = readLine().toIntOption.getOrElse(-1)
      if (!availableMachines.contains(machineId)) println("Choix invalide.")
    }
    machineId
  }

  def adminMode(): Unit = {
    //mode admin pour gérer stock
    val machineId = selectValidMachine()
    printf("Mode Administrateur pour la machine %d.\n", machineId + 1)

    //vérification code PIN
    if (validatePin(machineId)) {
      println("1) Réapprovisionner")
      println("2) Changer le code PIN")
      val adminChoice = getValidChoice(List("1", "2"))

      if (adminChoice == "1") restockMachine(machineId)//Réapprovisionnement
      else if (adminChoice == "2") updatePin(machineId)//Mise a jour code
    } else {
      println("Accès refusé.")//accès refusé code faux
    }
  }

  def validatePin(machineId: Int): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      print("Entrez le code PIN : ")
      if (readLine() == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      }
      attempts -= 1
      if (attempts > 0) {
        printf("Code PIN incorrect. Tentatives restantes : %d\n", attempts)
      } else {
        println("Code PIN incorrect. 0 tentatives restantes.")
        println("Trop de tentatives échouées. Fin du programme")
        System.exit(1)//fermeture programme
      }
    }
    false
  }


  def updatePin(machineId: Int): Unit = {
    var updated = false
    while (!updated) {
      print("Entrez un nouveau code PIN (6 chiffres) : ")
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.")
        updated = true
      } else {
        println("Erreur : le code PIN doit comporter 6 chiffres.")//message d'erreur
      }
    }
    println("Retour au menu principal...")
  }

  def restockMachine(machineId: Int): Unit = {
    //affichage stocks
    println("Niveaux de stock actuels :")
    printf("Poudre de café : %.0fg\n", coffeeStocks(machineId))
    printf("Sucre : %.0fg\n", sugarStocks(machineId))
    printf("Lait : %.1fL\n", milkStocks(machineId))

    // demande quantité a ajouter
    print("Poudre de café > ")
    val coffeeAdded = getValidDouble("")
    print("Sucre > ")
    val sugarAdded = getValidDouble("")
    print("Lait > ")
    val milkAdded = getValidDouble("")

    //mise à jour stocks
    coffeeStocks(machineId) += coffeeAdded
    sugarStocks(machineId) += sugarAdded
    milkStocks(machineId) += milkAdded

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def selectValidMachine(): Int = {
    var machineId = -1
    while (machineId == -1) {
      printf("Sélectionnez une machine (1 à %d) : ", nbMachines)
      machineId = readLine().toIntOption.filter(id => id >= 1 && id <= nbMachines).map(_ - 1).getOrElse(-1)
      if (machineId == -1) println("Choix invalide.")
    }
    machineId
  }

  def getValidChoice(validChoices: List[String]): String = {
    var choice = ""
    //vérification choix valide
    while (!validChoices.contains(choice)) {
      choice = readLine()
      if (!validChoices.contains(choice)) println("Choix invalide.")
    }
    choice
  }

  def getValidDouble(prompt: String): Double = {
    var value = -1.0
    //vérification entrée valide
    while (value < 0) {
      if (prompt.nonEmpty) print(prompt)
      value = readLine().toDoubleOption.getOrElse(-1.0)
      if (value < 0) println("Choix invalide.")
    }
    value
  }

  def generatePaymentCode(): String = Random.alphanumeric.take(5).mkString
}
