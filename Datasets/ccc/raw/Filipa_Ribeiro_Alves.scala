import scala.io.Source
import scala.io.StdIn._
import scala.util.{Try, Using, Random}
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

// Classe Machine qui représente une machine avec ses stocks et son code PIN.
case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Méthode pour ajouter une quantité spécifique d'un ingrédient au stock.
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount // Ajoute du lait.
      case "sugar" => sugar += amount // Ajoute du sucre.
      case "coffee" => coffee += amount // Ajoute du café.
      case _ => println(Machine.invalidIngredientMessage) // Si l'ingrédient est inconnu.
    }
  }

  // Méthode pour retirer une quantité d'un ingrédient du stock (si disponible).
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true // Retire du lait si suffisant.
      case "sugar" if sugar >= amount => sugar -= amount; true // Retire du sucre si suffisant.
      case "coffee" if coffee >= amount => coffee -= amount; true // Retire du café si suffisant.
      case _ => false // Retourne false si le stock est insuffisant.
    }
  }
}

object Machine {
  // Messages pour centraliser les messages d'erreur et éviter la répétition.
  val invalidIngredientMessage = "Ingrédient non valide."
  val insufficientStockMessage = "Stock insuffisant pour la commande."
  val invalidPINMessage = "Le code PIN doit contenir exactement 6 chiffres."
}

object NospressoSimple {
  val filename = "machines.csv" // Fichier CSV pour charger et sauvegarder les machines.
  var machines: ArrayBuffer[Machine] = loadcsv(filename) // Chargement initial des machines depuis le CSV.

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis le fichier CSV...")

    if (machines.isEmpty) {
      println("Erreur : Aucune machine chargée. Vérifiez le contenu du fichier CSV.")
      return // Quitte le programme si le fichier CSV est vide ou invalide.
    } else {
      printf("%d machine(s) chargée(s) avec succès.\n", machines.size)
      machines.zipWithIndex.foreach { case (machine, index) =>
        printf("Machine %d chargée :\n", index + 1)
        printf("ID: %d\n", machine.id)
        printf("Code PIN: %s\n", machine.pincode)
        printf("Lait : %.3f L\n", machine.milk / 1000.0) // Affiche le stock de lait en litres.
        printf("Sucre : %.1f g\n", machine.sugar.toDouble) // Affiche le stock de sucre en grammes.
        printf("Café : %.1f g\n", machine.coffee.toDouble) // Affiche le stock de café en grammes.
      }
    }

    var running = true
    while (running) {
      println("\n=== Nospresso Café ===")
      println("1) Mode Client")
      println("2) Mode Administrateur")
      println("3) Quitter\n>")
      val choice = readLine() // Lecture du choix de l'utilisateur.

      choice match {
        case "1" => clientMode() // Lance le mode client.
        case "2" => adminMode() // Lance le mode administrateur.
        case "3" =>
          running = false // Arrête le programme.
          savecsv(filename, machines) // Sauvegarde l'état des machines avant de quitter.
          println("Merci d'avoir utilisé Nospresso. Au revoir.")
        case _ => println("Choix invalide.") // Si le choix est incorrect.
      }
    }
  }

  // Mode Client : permet à l'utilisateur de commander une boisson.
  def clientMode(): Unit = {
    val machineId = selectValidMachine() // Permet de sélectionner une machine.
    val machine = machines(machineId)

    printf("Bienvenue au mode client pour la machine %d.\n", machine.id)
    println("1) Expresso (CHF 2.00)")
    println("2) Cappuccino (CHF 2.50)")
    println("3) Latte (Petit: CHF 2.70, Moyen: CHF 3.20, Grand: CHF 3.70)")
    val drinkChoice = getValidChoice(List("1", "2", "3")) // Lecture du choix de la boisson.

    // Variables pour stocker les quantités d'ingrédients nécessaires.
    var coffeeNeeded = 0
    var milkNeeded = 0
    var sugarNeeded = 0
    var price = 2.0 // Prix par défaut pour l'expresso.

    drinkChoice match {
      case "1" => coffeeNeeded = Math.min(8, machine.coffee) // Expresso.
      case "2" => // Cappuccino.
        coffeeNeeded = Math.min(6, machine.coffee)
        milkNeeded = Math.min(100, machine.milk)
        price = 2.5
      case "3" => // Latte avec plusieurs tailles.
        println("Choisissez la taille du Latte : 1) Petit 2) Moyen 3) Grand")
        getValidChoice(List("1", "2", "3")) match {
          case "1" => coffeeNeeded = Math.min(6, machine.coffee); milkNeeded = Math.min(120, machine.milk); price = 2.7
          case "2" => coffeeNeeded = Math.min(8, machine.coffee); milkNeeded = Math.min(150, machine.milk); price = 3.2
          case "3" => coffeeNeeded = Math.min(12, machine.coffee); milkNeeded = Math.min(200, machine.milk); price = 3.7
        }
    }

    println("Ajoutez du sucre ? 1) Non 2) Peu (5g) 3) Moyen (10g) 4) Beaucoup (15g)")
    sugarNeeded = getValidChoice(List("1", "2", "3", "4")).toInt match {
      case 1 => 0
      case 2 => Math.min(5, machine.sugar)
      case 3 => Math.min(10, machine.sugar)
      case 4 => Math.min(15, machine.sugar)
    }

    // Vérification des stocks avant de préparer la boisson.
    if (machine.removeIngredient("coffee", coffeeNeeded) &&
      machine.removeIngredient("milk", milkNeeded) &&
      machine.removeIngredient("sugar", sugarNeeded)) {
      printf("Prix total : CHF %.2f. Merci de payer via Twint.\n", price)
      printf("Votre code de paiement est : %s\n", Random.alphanumeric.take(5).mkString) // Génère un code de paiement aléatoire.
      prepareDrink(drinkChoice, sugarNeeded.toString, milkNeeded > 0)
    } else {
      println(Machine.insufficientStockMessage) // Message si les stocks sont insuffisants.
    }
  }

  // Mode Administrateur : permet d'accéder aux options de gestion des stocks et du code PIN.
  def adminMode(): Unit = {
    val machineId = selectValidMachine()
    val machine = machines(machineId)

    if (validatePin(machineId)) { // Vérification du code PIN avec 3 tentatives.
      println("1) Réapprovisionner")
      println("2) Changer le code PIN")
      getValidChoice(List("1", "2")) match {
        case "1" =>
          // Réapprovisionnement des stocks.
          print("Quantité de café à ajouter : ")
          machine.addIngredient("coffee", getIntFromUser())
          print("Quantité de lait à ajouter : ")
          machine.addIngredient("milk", getIntFromUser())
          print("Quantité de sucre à ajouter : ")
          machine.addIngredient("sugar", getIntFromUser())
          println("Stock mis à jour.")
        case "2" =>
          updatePin(machineId) // Permet de changer le code PIN.
      }
    } else {
      println("Accès refusé. Trop de tentatives échouées.") // Si les 3 tentatives échouent.
    }
  }

  // Fonction pour valider le code PIN avec 3 tentatives.
  def validatePin(machineId: Int): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      print("Entrez le code PIN : ")
      val enteredPin = readLine()
      if (enteredPin == machines(machineId).pincode) {
        println("Accès autorisé.")
        return true // Code PIN correct.
      }
      attempts -= 1
      if (attempts > 0) {
        printf("Code PIN incorrect. Tentatives restantes : %d\n", attempts)
      } else {
        println("Code PIN incorrect. 0 tentatives restantes.")
        println("Trop de tentatives échouées. Accès bloqué.")
        return false
      }
    }
    false
  }

  // Fonction pour mettre à jour le code PIN.
  def updatePin(machineId: Int): Unit = {
    var updated = false
    while (!updated) {
      print("Entrez un nouveau code PIN (6 chiffres) : ")
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machines(machineId).pincode = newPin
        println("Code PIN mis à jour avec succès.")
        updated = true
      } else {
        println("Erreur : le code PIN doit comporter 6 chiffres.")
      }
    }
    println("Retour au menu principal...")
  }

  // Fonction pour sélectionner une machine valide.
  def selectValidMachine(): Int = {
    machines.indices.foreach(i => printf("%d) Machine %d\n", i + 1, i + 1))
    printf("Sélectionnez une machine (1 à %d) : \n", machines.size)
    val choix = Try(readLine().toInt - 1).getOrElse {
      println("Choix invalide. Sélectionnez une option valide.")
      selectValidMachine() // Relance la sélection si le choix est invalide.
    }
    choix
  }

  // Simule la préparation d'une boisson.
  def prepareDrink(drink: String, sugar: String, withMilk: Boolean): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000) // Pause pour simuler la préparation.
    val drinkName = drink match {
      case "1" => "Expresso"
      case "2" => "Cappuccino"
      case "3" => "Latte"
      case _ => "Boisson inconnue"
    }
    printf("Votre %s est prêt ! Bonne dégustation !\n", drinkName)
    Thread.sleep(2000)
  }

  def getValidChoice(validChoices: List[String]): String = {
    var choice = ""
    while (!validChoices.contains(choice)) {
      choice = readLine()
      if (!validChoices.contains(choice)) println("Choix invalide.")
    }
    choice
  }

  def getIntFromUser(): Int = {
    Try(readLine().toInt).getOrElse {
      println("Veuillez entrer un nombre valide.")
      getIntFromUser()
    }
  }

  // Chargement des machines depuis le fichier CSV.
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    Try {
      val source = Source.fromFile(filename)
      try {
        val lines = source.getLines().drop(1) // Ignore l'en-tête.
        lines.foreach { line =>
          printf("Lecture de la ligne : %s\n", line)
          val Array(pin, milk, sugar, coffee) = line.split(",")
          buffer.append(Machine(buffer.size + 1, pin, milk.toInt, sugar.toInt, coffee.toInt))
        }
      } finally {
        source.close()
      }
      buffer
    } match {
      case scala.util.Success(machines) => machines
      case scala.util.Failure(_) =>
        println("Erreur : Fichier introuvable ou corrompu.")
        ArrayBuffer.empty[Machine]
    }
  }

  // Sauvegarde des machines dans le fichier CSV.
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    Try {
      Using(new PrintWriter(new File(filename))) { writer =>
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        machines.foreach(m => writer.printf("%s,%d,%d,%d\n", m.pincode, m.milk, m.sugar, m.coffee))
        printf("Sauvegarde réussie : %d machine(s) enregistrée(s) dans %s.\n", machines.size, filename)
      }
    } match {
      case scala.util.Success(_) =>
        println("Fichier sauvegardé avec succès.")
      case scala.util.Failure(exception) =>
        printf("Erreur lors de la sauvegarde du fichier : %s\n", exception.getMessage)
    }
  }
}
