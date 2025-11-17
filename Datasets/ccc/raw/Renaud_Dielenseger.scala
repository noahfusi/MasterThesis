import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{BufferedWriter, FileWriter}
import scala.util.{Try, Using}

case class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient inconnu : $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println(s"Stock insuffisant ou ingrédient inconnu : $ingredient"); false
    }
  }
}

object MultiMachineNospresso {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()
  val filename = "machines.csv"
  var continue = true

  def main(args: Array[String]): Unit = {
    machines = loadcsv(filename).getOrElse {
      println("Erreur lors du chargement du fichier CSV. Fin du programme.")
      sys.exit(1)
    }

    while (continue) {
      println("\nNospresso Café - Gestion des Machines")
      println("Veuillez sélectionner un mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      scala.io.StdIn.readLine() match {
        case "1" => clientMode()
        case "2" => adminMode()
        case "3" =>
          println("Sauvegarde des données...")
          savecsv(filename, machines)
          println("Merci de votre visite. À bientôt !")
          continue = false
        case _ => println("Sélection invalide, veuillez réessayer.")
      }
    }
  }

  def loadcsv(filename: String): Option[ArrayBuffer[Machine]] = {
    Try {
      val buffer = ArrayBuffer[Machine]()
      Using(Source.fromFile(filename)) { file =>
        file.getLines().drop(1).foreach { line =>
          val Array(pincode, milk, sugar, coffee) = line.split(",")
          buffer += Machine(buffer.size, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        }
      }
      buffer
    }.toOption
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    Try {
      Using(new BufferedWriter(new FileWriter(filename))) { writer =>
        writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
        machines.foreach(machine =>
          writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
        )
      }
    }.recover { case e => println(s"Erreur lors de la sauvegarde du fichier CSV : ${e.getMessage}") }
  }

  def selectionMachine(): Option[Machine] = {
    println(s"Veuillez sélectionner une machine (0-${machines.size - 1}):")
    Try(scala.io.StdIn.readInt()).toOption.flatMap { id =>
      if (id >= 0 && id < machines.size) Some(machines(id))
      else {
        println("Numéro de machine invalide. Essayez à nouveau.")
        None
      }
    }
  }

  def clientMode(): Unit = {
    selectionMachine() match {
      case Some(machine) =>
        println(s"\nBienvenue au mode client de la Machine ${machine.id}.")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 à 3.70")
        print("> ")

        scala.io.StdIn.readLine() match {
          case "1" => preparationBoisson(machine, "Expresso", 2.00, 8, 0)
          case "2" => preparationBoisson(machine, "Cappuccino", 2.50, 6, 100)
          case "3" => preparationLatte(machine)
          case _ => println("Sélection invalide.")
        }
      case None => println("Échec de la sélection de la machine.")
    }
  }

  def preparationBoisson(machine: Machine, nomBoisson: String, prixBoisson: Double, cafeNecessaire: Int, laitNecessaire: Int): Unit = {
    if (machine.removeIngredient("coffee", cafeNecessaire) && machine.removeIngredient("milk", laitNecessaire)) {
      println(s"Préparation de votre $nomBoisson en cours...")
      Thread.sleep(3000)
      println(s"Votre $nomBoisson est prêt ! Bonne dégustation.")
    } else {
      println("Stock insuffisant pour préparer cette boisson.")
    }
  }

  def preparationLatte(machine: Machine): Unit = {
    println("Veuillez sélectionner la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    print("> ")

    val (prix, cafe, lait) = scala.io.StdIn.readLine() match {
      case "1" => (2.70, 6, 120)
      case "2" => (3.20, 8, 150)
      case "3" => (3.70, 12, 200)
      case _ => println("Sélection invalide."); return
    }

    preparationBoisson(machine, "Latte", prix, cafe, lait)
  }

  def adminMode(): Unit = {
    selectionMachine() match {
      case Some(machine) =>
        println(s"\nMode administrateur pour la machine ${machine.id}")
        println("Entrez le code PIN pour accéder :")
        val pin = scala.io.StdIn.readLine()
        if (pin == machine.pincode) {
          println("1) Réapprovisionner")
          println("2) Mettre à jour le PIN")
          println("3) Retour")
          print("> ")
          scala.io.StdIn.readLine() match {
            case "1" => restockMachine(machine)
            case "2" => updatePin(machine)
            case _ => println("Retour au menu principal.")
          }
        } else println("PIN invalide.")
      case None => println("Échec de la sélection de la machine.")
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println("Quantité de lait à ajouter (ml) :"); machine.addIngredient("milk", scala.io.StdIn.readInt())
    println("Quantité de sucre à ajouter (g) :"); machine.addIngredient("sugar", scala.io.StdIn.readInt())
    println("Quantité de café à ajouter (g) :"); machine.addIngredient("coffee", scala.io.StdIn.readInt())
    println("Réapprovisionnement réussi.")
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    val newPin = scala.io.StdIn.readLine()
    if (newPin.matches("\\d{6}")) {
      machine.pincode = newPin
      println("Le PIN a été mis à jour avec succès.")
    } else println("Format du PIN invalide.")
  }
}