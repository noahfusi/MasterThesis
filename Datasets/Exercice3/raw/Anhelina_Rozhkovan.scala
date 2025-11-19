import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.control.Exception._

// Class definition for Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient non valide.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println("Stock insuffisant ou ingrédient non valide."); false
    }
  }

  override def toString: String = {
    f"ID: $id\nCode PIN: $pincode\nLait: ${milk / 1000.0}%.3f L\nSucre: ${sugar}g\nCafé: ${coffee}g"
  }
}

object NospressoApp {
  val filename = "src/main/resources/machines.csv"
  val machines: ArrayBuffer[Machine] = ArrayBuffer()


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    catching(classOf[Exception]).either {
      val source = Source.fromFile(filename)
      try {
        val lines = source.getLines().drop(1)
        for ((line, index) <- lines.zipWithIndex) {
          val cols = line.split(",").map(_.trim)
          if (cols.length == 4) {
            buffer += new Machine(index + 1, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
          }
        }
      } finally {
        source.close()
      }
    } match {
      case Left(ex) => println(s"Erreur : Impossible de charger les machines. ${ex.getMessage}"); sys.exit(1)
      case Right(_) =>
    }
    buffer
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    catching(classOf[Exception]).either {
      val writer = new PrintWriter(new File(filename))
      try {
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
        }
      } finally {
        writer.close()
      }
    } match {
      case Left(ex) => println(s"Erreur : Impossible de sauvegarder les machines. ${ex.getMessage}")
      case Right(_) => println("Fichier sauvegardé avec succès.")
    }
  }

  def mainMenu(): Unit = {
    println("Nospresso Café")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")
  }

  def adminMenu(): Unit = {
    println("1) Réapprovisionner les stocks")
    println("2) Mettre à jour le code PIN")
  }

  def main(args: Array[String]): Unit = {
    machines ++= loadcsv(filename)
    println("Chargement des machines depuis machines.csv...")
    println(s"${machines.size} machine(s) chargée(s) avec succès.")

    var running = true
    while (running) {
      mainMenu()
      val choice = scala.io.StdIn.readLine()
      choice match {
        case "1" =>
          println("Mode Client - Non implémenté dans cet exercice.")
        case "2" =>
          println("Sélectionnez une machine (1 à ${machines.size}):")
          val machineId = scala.io.StdIn.readInt()
          if (machineId > 0 && machineId <= machines.size) {
            val machine = machines(machineId - 1)
            println(s"Accès à la machine $machineId:")
            println(machine)
            println("Entrez le code PIN:")
            val pin = scala.io.StdIn.readLine()
            if (pin == machine.pincode) {
              adminMenu()
              val adminChoice = scala.io.StdIn.readLine()
              adminChoice match {
                case "1" =>
                  println("Réapprovisionnement des stocks.")
                  println("Ajout de lait (ml):")
                  machine.addIngredient("milk", scala.io.StdIn.readInt())
                  println("Ajout de sucre (g):")
                  machine.addIngredient("sugar", scala.io.StdIn.readInt())
                  println("Ajout de café (g):")
                  machine.addIngredient("coffee", scala.io.StdIn.readInt())
                  println("Stocks mis à jour.")
                case "2" =>
                  println("Entrez un nouveau code PIN (6 chiffres):")
                  val newPin = scala.io.StdIn.readLine()
                  if (newPin.matches("\\d{6}")) {
                    machine.pincode = newPin
                    println("Code PIN mis à jour.")
                  } else {
                    println("Code PIN invalide.")
                  }
                case _ => println("Option invalide.")
              }
            } else {
              println("Code PIN incorrect.")
            }
          } else {
            println("ID de machine invalide.")
          }
        case "3" =>
          println("Sauvegarde des machines dans machines.csv...")
          savecsv(filename, machines)
          running = false
        case _ => println("Option invalide.")
      }
    }
  }
}
