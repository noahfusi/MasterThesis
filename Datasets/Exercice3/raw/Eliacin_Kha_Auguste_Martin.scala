
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer

// Classe Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = ingredient.toLowerCase match {
    case "milk" => milk += amount
    case "sugar" => sugar += amount
    case "coffee" => coffee += amount
    case _ => println("Ingrédient non valide.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient.toLowerCase match {
    case "milk" if milk >= amount => milk -= amount; true
    case "sugar" if sugar >= amount => sugar -= amount; true
    case "coffee" if coffee >= amount => coffee -= amount; true
    case _ => false
  }
}

object Nospresso {
  val csvFile: String = "machines.csv"
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    try {
      machines = loadcsv(csvFile)
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      case ex: Exception =>
        println(s"Erreur : ${ex.getMessage}")
        return
    }

    var continuer = true
    while (continuer) {
      println("Nospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = scala.io.StdIn.readLine("> ")

      choix match {
        case "1" => handleClientMode()
        case "2" => handleAdminMode()
        case "3" =>
          try {
            savecsv(csvFile, machines)
            println("Fichier sauvegardé avec succès.")
          } catch {
            case ex: Exception => println(s"Erreur lors de la sauvegarde : ${ex.getMessage}")
          }
          continuer = false
        case _ => println("Choix invalide. Veuillez réessayer.")
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    val source = Source.fromFile(filename)
    try {
      for ((line, index) <- source.getLines().drop(1).zipWithIndex) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        buffer += new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
    } finally {
      source.close()
    }
    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val writer = new PrintWriter(new File(filename))
    try {
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
    } finally {
      writer.close()
    }
  }

  def handleClientMode(): Unit = {
    println("Sélectionnez l'ID de la machine :")
    val id = scala.io.StdIn.readLine("> ").toInt - 1
    if (id >= 0 && id < machines.size) {
      val machine = machines(id)
      println("1) Expresso - 8g de café")
      println("2) Cappuccino - 6g de café, 100ml de lait")
      println("3) Latte - 6 à 12g de café, 120 à 200ml de lait")
      val choix = scala.io.StdIn.readLine("> ")

      choix match {
        case "1" =>
          if (machine.removeIngredient("coffee", 8)) println("Votre expresso est prêt !")
          else println("Stock insuffisant !")
        case "2" =>
          if (machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100))
            println("Votre cappuccino est prêt !")
          else println("Stock insuffisant !")
        case "3" =>
          println("1) Petit (6g café, 120ml lait)")
          println("2) Moyen (8g café, 150ml lait)")
          println("3) Grand (12g café, 200ml lait)")
          val taille = scala.io.StdIn.readLine("> ")
          taille match {
            case "1" if machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 120) =>
              println("Votre latte petit est prêt !")
            case "2" if machine.removeIngredient("coffee", 8) && machine.removeIngredient("milk", 150) =>
              println("Votre latte moyen est prêt !")
            case "3" if machine.removeIngredient("coffee", 12) && machine.removeIngredient("milk", 200) =>
              println("Votre latte grand est prêt !")
            case _ => println("Stock insuffisant ou choix invalide !")
          }
        case _ => println("Choix invalide !")
      }
    } else {
      println("ID de machine invalide.")
    }
  }

  def handleAdminMode(): Unit = {
    println("Entrez le mot de passe administrateur :")
    val password = scala.io.StdIn.readLine("> ")
    if (password == "admin2024") {
      println("Sélectionnez l'ID de la machine :")
      val id = scala.io.StdIn.readLine("> ").toInt - 1
      if (id >= 0 && id < machines.size) {
        val machine = machines(id)
        println("1) Réapprovisionner")
        println("2) Changer le PIN")
        val choix = scala.io.StdIn.readLine("> ")

        choix match {
          case "1" =>
            println("Quantité de lait à ajouter :")
            machine.addIngredient("milk", scala.io.StdIn.readLine("> ").toInt)
            println("Quantité de sucre à ajouter :")
            machine.addIngredient("sugar", scala.io.StdIn.readLine("> ").toInt)
            println("Quantité de café à ajouter :")
            machine.addIngredient("coffee", scala.io.StdIn.readLine("> ").toInt)
            println("Stocks mis à jour.")
          case "2" =>
            println("Nouveau PIN (6 chiffres) :")
            val newPin = scala.io.StdIn.readLine("> ")
            if (newPin.matches("\\d{6}")) {
              machine.pincode = newPin
              println("PIN mis à jour.")
            } else {
              println("Format de PIN invalide.")
            }
          case _ => println("Choix invalide.")
        }
      } else {
        println("ID de machine invalide.")
      }
    } else {
      println("Mot de passe incorrect.")
    }
  }
}
