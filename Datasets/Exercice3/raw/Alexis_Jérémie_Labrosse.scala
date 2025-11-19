import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}

case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = ingredient match {
    case "milk" => milk += amount
    case "sugar" => sugar += amount
    case "coffee" => coffee += amount
    case _ => println("Ingrédient invalide.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient match {
    case "milk" if milk >= amount => milk -= amount; true
    case "sugar" if sugar >= amount => sugar -= amount; true
    case "coffee" if coffee >= amount => coffee -= amount; true
    case _ => println("Stock insuffisant ou ingrédient invalide."); false
  }

  def prepareDrink(drink: Drink): Boolean = {
    val canPrepare = drink.ingredients.forall {
      case ("milk", qty) => milk >= qty
      case ("sugar", qty) => sugar >= qty
      case ("coffee", qty) => coffee >= qty
      case _ => false
    }

    if (canPrepare) {
      drink.ingredients.foreach {
        case ("milk", qty) => removeIngredient("milk", qty)
        case ("sugar", qty) => removeIngredient("sugar", qty)
        case ("coffee", qty) => removeIngredient("coffee", qty)
        case _ => // Ignorer
      }
      println(s"Votre ${drink.name} est prêt. Bonne dégustation !")
      true
    } else {
      println(s"Impossible de préparer ${drink.name} : stock insuffisant.")
      false
    }
  }

  override def toString: String =
    f"ID: $id | PIN: $pincode | Lait: ${milk / 1000.0}%.2f L | Sucre: ${sugar}g | Café: ${coffee}g"
}

// Classe Drink (boisson)
case class Drink(name: String, ingredients: Map[String, Int], basePrice: Double)

object Nospresso {
  val filename = "machines.csv"
  var machines: ArrayBuffer[Machine] = loadcsv(filename)

  // Liste des boissons disponibles avec prix en CHF
  val drinks = Seq(
    Drink("Expresso", Map("coffee" -> 50), 1.50),
    Drink("Cappuccino", Map("coffee" -> 50, "milk" -> 100), 2.50),
    Drink("Latte", Map("coffee" -> 50, "milk" -> 150, "sugar" -> 10), 3.00)
  )

  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("\nNospresso Café - Menu Principal")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")
      StdIn.readLine() match {
        case "1" => clientMode()
        case "2" => adminMode()
        case "3" =>
          println("Sauvegarde des machines...")
          savecsv(filename, machines)
          println("Merci de votre visite !")
          continue = false
        case _ => println("Option invalide.")
      }
    }
  }

  def clientMode(): Unit = {
    selectMachine() match {
      case Some(machine) =>
        println("Mode client sélectionné. Choisissez votre boisson :")
        drinks.zipWithIndex.foreach { case (drink, index) =>
          println(s"${index + 1}) ${drink.name} - ${drink.basePrice} CHF")
        }
        println(s"${drinks.length + 1}) Retour")
        print("> ")
        try {
          val choice = StdIn.readInt()
          if (choice > 0 && choice <= drinks.length) {
            val selectedDrink = drinks(choice - 1)
            println(s"Vous avez choisi : ${selectedDrink.name} (${selectedDrink.basePrice} CHF)")

            // Lait en supplément 
            println("Souhaitez-vous ajouter du lait supplémentaire ? (0 pour non, quantité en ml sinon)")
            print("> ")
            val extraMilk = StdIn.readInt()
            val extraMilkCost = if (extraMilk > 0) (extraMilk / 50) * 0.10 else 0.0

            // Préparation de la boisson
            val totalCost = selectedDrink.basePrice + extraMilkCost
            if (machine.prepareDrink(selectedDrink)) {
              if (extraMilk > 0 && machine.removeIngredient("milk", extraMilk)) {
                println(s"Lait supplémentaire ajouté : ${extraMilk} ml (Supplément : ${extraMilkCost} CHF)")
              } else if (extraMilk > 0) {
                println("Stock insuffisant pour le lait supplémentaire.")
              }
              println(f"Prix total : $totalCost%.2f CHF. Bonne dégustation !")
            }
          } else if (choice == drinks.length + 1) {
            println("Retour au menu principal.")
          } else {
            println("Option invalide.")
          }
        } catch {
          case _: Exception => println("Entrée invalide.")
        }
      case None => println("Aucune machine sélectionnée.")
    }
  }

  def adminMode(): Unit = {
    selectMachine() match {
      case Some(machine) =>
        println(s"Accès à la Machine ${machine.id} (Admin).")
        if (validatePin(machine)) {
          println("1) Réapprovisionner")
          println("2) Modifier le PIN")
          println("3) Retour")
          print("> ")
          StdIn.readLine() match {
            case "1" => restockMachine(machine)
            case "2" => updatePin(machine)
            case "3" => println("Retour au menu principal.")
            case _ => println("Option invalide.")
          }
        }
      case None => println("Aucune machine sélectionnée.")
    }
  }

  def validatePin(machine: Machine): Boolean = {
    println("Entrez le code PIN :")
    val inputPin = StdIn.readLine()
    if (inputPin == machine.pincode) true
    else {
      println("Code PIN incorrect.")
      false
    }
  }

  def selectMachine(): Option[Machine] = {
    println(s"Veuillez choisir une machine (1-${machines.length}) :")
    try {
      val id = StdIn.readInt()
      machines.find(_.id == id)
    } catch {
      case _: Exception =>
        println("Sélection invalide.")
        None
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    println(s"Chargement des machines depuis $filename...")
    try {
      val lines = Source.fromFile(filename).getLines()
      val header = lines.next()
      for ((line, index) <- lines.zipWithIndex) {
        val data = line.split(",").map(_.trim)
        if (data.length == 4) {
          buffer += Machine(index + 1, data(0), data(1).toInt, data(2).toInt, data(3).toInt)
        }
      }
      println(s"${buffer.length} machine(s) chargée(s) avec succès.")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(1)
      case e: Exception =>
        println(s"Erreur lors du chargement : ${e.getMessage}")
        System.exit(1)
    }
    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Sauvegarde de ${machines.length} machine(s) dans $filename...")
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur lors de la sauvegarde : ${e.getMessage}")
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println(s"Réapprovisionnement de la Machine ${machine.id}.")
    try {
      println("Ajoutez du lait (ml) :")
      machine.addIngredient("milk", StdIn.readInt())
      println("Ajoutez du sucre (g) :")
      machine.addIngredient("sugar", StdIn.readInt())
      println("Ajoutez du café (g) :")
      machine.addIngredient("coffee", StdIn.readInt())
      println("Stocks mis à jour.")
    } catch {
      case _: Exception => println("Erreur : entrée invalide.")
    }
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN (6 chiffres) :")
    val newPin = StdIn.readLine()
    if (newPin.matches("\\d{6}")) {
      machine.pincode = newPin
      println("PIN mis à jour.")
    } else {
      println("Code PIN invalide. Le PIN doit contenir exactement 6 chiffres.")
    }
  }
}