// Nospresso System Implementation in Scala

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.Random

// Define the Machine class
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Add ingredient to stock
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient invalide!")
    }
  }

  // Remove ingredient from stock
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {

      case "milk" if (milk >= amount) => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println("Stock insuffisant ou ingrédient invalide."); false
    }
  }

  override def toString: String = {
    f"ID: $id, Pincode: $pincode, Milk: ${milk / 1000.0}%.3f L, Sugar: $sugar g, Coffee: $coffee g"
  }
}

object NospressoSystem {

  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  // Load machines from a CSV file
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val lines = Source.fromFile(filename).getLines().toList
      lines.tail.foreach { line =>
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        machines += new Machine(machines.size + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
      machines
    } catch {
      case _: Exception =>
        println("Erreur: Impossible de charger le fichier CSV.")
        sys.exit(1)
    }
  }

  // Save machines to a CSV file
  def savecsv(filename: String): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Machines sauvegardés avec succès.")
    } catch {
      case _: Exception => println("Erreur. Impossible de sauvegarder le ficher CSV")
    }
  }

  // Main program loop
  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    loadcsv(filename)
    println("   Nespresso Café")

    var running = true
    while (running) {
      print("Veuillez selectionner votre mode: \n1) Client \n2) Admin \n3) Quitter \n> ")
      scala.io.StdIn.readInt() match {
        case 1 => clientMode()
        case 2 => adminMode()
        case 3 =>
          savecsv(filename)
          running = false
        case _ => println("Veuillez soumettre une entrée valide.")
      }
    }
  }

  // Client mode
  def clientMode(): Unit = {
    println("Mode Client selectionné.")
    val machine = selectMachine()
    if (machine != null) {
      print("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Capuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ")
      val drink = scala.io.StdIn.readInt()
      var (coffeeAmount, milkAmount, basePrice) = drink match {
        case 1 => (8, 0, 2.0) // Espresso
        case 2 => (6, 100, 2.5) // Cappuccino
        case 3 =>
          print("Quelle taille de Latte désirez-vous ? \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n>")
          scala.io.StdIn.readInt() match {
            case 1 => (6, 120, 2.7) // Latte Small
            case 2 => (8, 150, 3.2) // Latte Medium
            case 3 => (12, 200, 3.7) // Latte Large
            case _ => println("Invalid size."); return
          }
        case _ => println("Sélection de boisson incorrecte."); return
      }
      print("Souhaitez-vous ajouter du sucre en supplément (Une dose correspond à 50ml) ? \n1) Non \n2) Peu (5g) \n3) Moyen (10g) \n4) Beaucoup (15g) \n>")
      var sugarAmount = scala.io.StdIn.readInt() match {
        case 2 => 5
        case 3 => 10
        case 4 => 15
        case _ => 0
      }
      val sugarPrice = sugarAmount match {
        case 5 => 0.1
        case 10 => 0.2
        case 15 => 0.3
      }
      print("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ")
      val extraMilkDoses = scala.io.StdIn.readInt() match {
        case 1 =>
          print("Combien de dose ? (1-3) \n> ")
          scala.io.StdIn.readInt() match {
            case n if n >= 1 && n <= 3 => n
            case _ => println("Nombre de doses invalide"); return
          }
        case _ => 0
      }
      val extraMilkAmount = extraMilkDoses * 50
      val extraMilkPrice = extraMilkDoses * 0.05

      val totalPrice = basePrice + sugarPrice + extraMilkPrice

      val canPrepare =
        machine.milk >= (milkAmount + extraMilkAmount) &&
          machine.coffee >= coffeeAmount &&
          machine.sugar >= sugarAmount

      if (canPrepare) {
        machine.removeIngredient("coffee", coffeeAmount)
        machine.removeIngredient("milk", milkAmount + extraMilkAmount)
        machine.removeIngredient("sugar", sugarAmount)

        println(f"Votre boisson est prête. Prix total: CHF $totalPrice%.2f")
        val chars: String = "ABCDEFGHIJKLMNOPQRATUVWXYZ0123456789"
        val codetwint = (1 to 5).map(_ => chars(Random.nextInt(chars.length))).mkString
        println(s"Veuillez payer en utilisant Twint. \nVotre code de paiement est : $codetwint\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson... \n[...] \nVotre boisson est prête ! Bonne dégustation !\n \n")
      } else {
        println("Impossible de préparer la boisson. Vérifiez les ingrédients et réessayez.")
      }

    }
  }

  // Admin mode
  def adminMode(): Unit = {
    println("Mode Admin sélectionné.")
    val machine = selectMachine()
    if (machine != null && validatePin(machine)) {
      println(f"Lait: ${machine.milk / 1000.0}%.3f L, Sucre: ${machine.sugar} g, Poudre de café: ${machine.coffee} g")
      print("1) Restocker les ingrédients\n2) Mise à jour du PIN \n> ")
      scala.io.StdIn.readInt() match {
        case 1 => restockIngredients(machine)
        case 2 => updatePin(machine)
        case _ => println("Sélection invalide.")
      }
    }
  }

  // Select a machine
  def selectMachine(): Machine = {
    print("Veuillez sélectionner l'ID de machine. \n> ")
    val id = scala.io.StdIn.readInt()
    machines.find(_.id == id).getOrElse({ println("ID de machine invalide."); null })
  }

  // Validate PIN
  def validatePin(machine: Machine): Boolean = {
    var attempts = 0
    while (attempts < 3) {
      println("Entrer le pin de la machine choisie: \n> ")
      val pin = scala.io.StdIn.readLine()
      if (pin == machine.pincode) return true
      else {
        attempts += 1
        println(s"Code PIN incorrect. ${3 - attempts} essai(s) restant(s).")
      }
    }
    println("Nombre de tentatives dépassées. Le programme se termine.")
    sys.exit(1)
  }

  // Update PIN
  def updatePin(machine: Machine): Unit = {
    print("Entrez un code PIN de 6 chiffres. \n> ")
    val newPin = scala.io.StdIn.readLine()
    if (newPin.matches("\\d{6}")) {
      machine.pincode = newPin
      print(s"Le code PIN de la machine a été mis à jour avec succès. \n Retour au menu principal... \n")
    } else {
      print("Le code PIN doit comportement exactement 6 chiffres.")
    }
  }

  // Restock ingredients
  def restockIngredients(machine: Machine): Unit = {
    print("Entrez le volume de lait à ajouter (en litres): \n> ")
    val milkLiters = scala.io.StdIn.readDouble()
    machine.addIngredient("milk", (milkLiters * 1000).toInt)
    print("Entrez la masse de sucre à ajouter: \n> ")
    machine.addIngredient("sugar", scala.io.StdIn.readInt())
    print("Entrez la masse de poudre de café en poudre à ajouter: \n> ")
    machine.addIngredient("coffee", scala.io.StdIn.readInt())
    print("Ingrédients restockés avec succès.")
  }

  // Prepare drink
  def prepareDrink(machine: Machine, ingredient: String, coffeeAmount: Int, milkAmount: Int): Unit = {
    if (machine.removeIngredient("coffee", coffeeAmount) && machine.removeIngredient("milk", milkAmount)) {
      println("Votre boisson est prête.")
    } else {
      println("Impossible de préparer la boisson. Vérifiez les ingrédients et réessayez.")
    }
  }
}