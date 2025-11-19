
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{BufferedWriter, FileWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Méthode pour ajouter un ingrédient
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient non reconnu: $ingredient")
    }
  }

  // Méthode pour retirer un ingrédient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient match {
      case "milk" =>
        if (milk >= amount) {
          milk -= amount
          true
        } else {
          println("Stock de lait insuffisant.")
          false
        }
      case "sugar" =>
        if (sugar >= amount) {
          sugar -= amount
          true
        } else {
          println("Stock de sucre insuffisant.")
          false
        }
      case "coffee" =>
        if (coffee >= amount) {
          coffee -= amount
          true
        } else {
          println("Stock de café insuffisant.")
          false
        }
      case _ =>
        println(s"Ingrédient non reconnu: $ingredient")
        false
    }
  }
}

object Nospresso {

  // Charger les machines à partir du fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()

    try {
      val source = Source.fromFile(filename)
      for ((line, index) <- source.getLines().zipWithIndex if index > 0) {  // On saute la première ligne (en-tête)
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        val machine = new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine
      }
      source.close()
    } catch {
      case _: java.io.FileNotFoundException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur : ${e.getMessage}")
        sys.exit(1)
    }

    println(s"${machines.size} machine(s) chargée(s) avec succès.")
    machines
  }

  // Sauvegarder les machines dans le fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))
      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")  // En-tête du fichier CSV
      machines.foreach { machine =>
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur : Échec de l’écriture dans $filename. Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
    }
  }

  val machines: ArrayBuffer[Machine] = loadcsv("machines.csv")  // Chargement des machines au démarrage

  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("Sélectionnez un mode :")
      println("1) Mode Client")
      println("2) Mode Administrateur")
      println("3) Quitter")
      val choice = scala.io.StdIn.readInt()

      choice match {
        case 1 =>
          clientMode()
        case 2 =>
          if (adminMenu()) {
            println("Retour au menu principal...")
          }
        case 3 =>
          println("Sauvegarde des machines dans machines.csv...")
          savecsv("machines.csv", machines)
          println("Au revoir !")
          continue = false
        case _ =>
          println("Choix invalide. Réessayer.")
      }
    }
  }

  def clientMode(): Unit = {
    val machineId = selectMachine()
    if (machineId != -1) {
      if (!serveClient(machineId)) {
        println("Transaction échouée. Vérifiez les stocks ou réessayez.")
      }
    }
  }

  def adminMenu(): Boolean = {
    val machineId = selectMachine()
    if (machineId == -1) return false

    if (validatePin(machineId)) {
      println("Accès accordé.")
      println("Que voulez-vous faire ?")
      println("1. Réapprovisionner la machine")
      println("2. Mettre à jour le code PIN")
      val adminChoice = scala.io.StdIn.readInt()

      adminChoice match {
        case 1 =>
          restockMachine(machineId)
        case 2 =>
          updatePin(machineId)
        case _ =>
          println("Choix invalide.")
      }
      true
    } else {
      println("Échec d'accès. Retour au menu principal.")
      false
    }
  }

  def selectMachine(): Int = {
    println(s"Sélectionnez une machine (1-${machines.size}):")
    val machineId = scala.io.StdIn.readInt() - 1
    if (machineId >= 0 && machineId < machines.size) {
      machineId
    } else {
      println("Machine invalide.")
      -1
    }
  }

  def validatePin(machineId: Int): Boolean = {
    val machine = machines(machineId)
    var attempts = 0
    while (attempts < 3) {
      println("Entrez le code PIN :")
      val inputPin = scala.io.StdIn.readLine()
      if (inputPin == machine.pincode) {
        return true
      } else {
        attempts += 1
        println(s"Code PIN incorrect. ${3 - attempts} tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    sys.exit(0)
  }

  def updatePin(machineId: Int): Unit = {
    var valid = false
    while (!valid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = scala.io.StdIn.readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machines(machineId).pincode = newPin
        println("Le code PIN a été mis à jour avec succès.")
        valid = true
      } else {
        println("Code PIN invalide. Essayez encore.")
      }
    }
  }

  def serveClient(machineId: Int): Boolean = {
    val machine = machines(machineId)
    println("Sélectionnez une boisson :")
    println("1. Expresso - CHF 2.00")
    println("2. Cappuccino - CHF 2.50")
    println("3. Latte (Petite - CHF 2.70, Moyenne - CHF 3.20, Grande - CHF 3.70)")
    val choice = scala.io.StdIn.readInt()

    choice match {
      case 1 =>
        if (machine.removeIngredient("coffee", 50)) {
          println("Expresso servi.")
          true
        } else {
          println("Stock insuffisant de café.")
          false
        }
      case 2 =>
        if (machine.removeIngredient("coffee", 50) && machine.removeIngredient("milk", 100)) {
          println("Cappuccino servi.")
          true
        } else {
          println("Stock insuffisant.")
          false
        }
      case 3 =>
        println("Choisissez la taille du Latte :")
        println("1. Petite (CHF 2.70)")
        println("2. Moyenne (CHF 3.20)")
        println("3. Grande (CHF 3.70)")
        val sizeChoice = scala.io.StdIn.readInt()
        val latteSize = sizeChoice match {
          case 1 => "Petite"
          case 2 => "Moyenne"
          case 3 => "Grande"
          case _ => "Invalide"
        }
        if (latteSize != "Invalide" && machine.removeIngredient("coffee", 50) && machine.removeIngredient("milk", 150)) {
          println(s"Latte $latteSize servi.")
          true
        } else {
          println("Stock insuffisant.")
          false
        }
      case _ =>
        println("Choix invalide.")
        false
    }
  }

  def restockMachine(machineId: Int): Unit = {
    val machine = machines(machineId)
    println("Quantité de café à ajouter (en grammes) :")
    val coffee = scala.io.StdIn.readInt()
    println("Quantité de sucre à ajouter (en grammes) :")
    val sugar = scala.io.StdIn.readInt()
    println("Quantité de lait à ajouter (en millilitres) :")
    val milk = scala.io.StdIn.readInt()

    if (coffee >= 0 && sugar >= 0 && milk >= 0) {
      machine.addIngredient("coffee", coffee)
      machine.addIngredient("sugar", sugar)
      machine.addIngredient("milk", milk)
      println("Les stocks ont été mis à jour avec succès.")
    } else {
      println("Quantités invalides.")
    }
  }
}






