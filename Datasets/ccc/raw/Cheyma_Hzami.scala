import scala.io.StdIn._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Méthode pour ajouter un ingrédient
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient inconnu.")
    }
  }

  // Méthode pour retirer un ingrédient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk") {
      if (milk >= amount) {
        milk -= amount
        true
      } else false
    } else if (ingredient == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else false
    } else if (ingredient == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else false
    } else {
      println("Ingrédient inconnu.")
      false
    }
  }
}

object Nospresso {
  var machines: ArrayBuffer[Machine] = loadcsv("machines.csv")

  def main(args: Array[String]): Unit = {
    var enCours = true
    while (enCours) {
      println("\nBienvenue sur Nospresso !")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")

      val choix = readLine()

      if (choix == "1") {
        modeClient()
      } else if (choix == "2") {
        modeAdmin()
      } else if (choix == "3") {
        println("Merci d'avoir utilisé Nospresso ! À bientôt.")
        savecsv("machines.csv", machines)
        enCours = false
      } else {
        println("Option invalide. Veuillez réessayer.")
      }
    }
  }

  // Mode Client
  def modeClient(): Unit = {
    println("\nMachine sélectionnée (1-" + machines.size + ") :")
    val idMachine = obtenirIdMachine() - 1

    if (idMachine >= 0 && idMachine < machines.size) {
      afficherStocks(idMachine)
    } else {
      println("ID de machine invalide.")
    }
  }

  // Mode Admin
  def modeAdmin(): Unit = {
    println("\nMachine sélectionnée (1-" + machines.size + ") :")
    val idMachine = obtenirIdMachine() - 1

    if (idMachine >= 0 && idMachine < machines.size) {
      val machine = machines(idMachine)
      println("Code PIN de la machine " + machine.id + " :")
      val pin = readLine()
      if (pin == machine.pincode) {
        println("\nAccès accordé à la machine " + machine.id + ":")
        afficherStocks(idMachine)

        println("1) Réapprovisionner les stocks")
        println("2) Mettre à jour le code PIN")
        print("> ")
        val choixAdmin = readLine()

        if (choixAdmin == "1") {
          restockMachine(idMachine)
        } else if (choixAdmin == "2") {
          updatePin(idMachine)
        } else {
          println("Option invalide.")
        }
      } else {
        println("Code PIN incorrect. Accès refusé.")
      }
    } else {
      println("ID de machine invalide.")
    }
  }

  def restockMachine(idMachine: Int): Unit = {
    val machine = machines(idMachine)
    println("Entrez la quantité de lait à ajouter (ml) :")
    val lait = readInt()
    machine.addIngredient("milk", lait)

    println("Entrez la quantité de sucre à ajouter (g) :")
    val sucre = readInt()
    machine.addIngredient("sugar", sucre)

    println("Entrez la quantité de café à ajouter (g) :")
    val cafe = readInt()
    machine.addIngredient("coffee", cafe)

    println("Les stocks ont été mis à jour avec succès.")
  }

  def updatePin(idMachine: Int): Unit = {
    println("Entrez un nouveau code PIN :")
    val nouveauPin = readLine()
    machines(idMachine).pincode = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def afficherStocks(idMachine: Int): Unit = {
    val machine = machines(idMachine)
    println("Stocks de la machine " + machine.id + ":")
    println("Café : " + machine.coffee + " g")
    println("Sucre : " + machine.sugar + " g")
    println("Lait : " + machine.milk + " ml")
  }

  def obtenirIdMachine(): Int = {
    var idMachine = -1
    while (idMachine < 1 || idMachine > machines.size) {
      println("Veuillez entrer un identifiant de machine entre 1 et " + machines.size + " :")
      idMachine = readInt()
      if (idMachine < 1 || idMachine > machines.size) {
        println("Identifiant invalide. Veuillez essayer à nouveau.")
      }
    }
    idMachine
  }

  // Méthode pour charger les machines depuis un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machinesBuffer = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1) // Ignore the header
      for (line <- lines) {
        val columns = line.split(",").map(_.trim)
        if (columns.length == 4) {
          val id = machinesBuffer.length + 1
          val pincode = columns(0)
          val milk = columns(1).toInt
          val sugar = columns(2).toInt
          val coffee = columns(3).toInt
          machinesBuffer += new Machine(id, pincode, milk, sugar, coffee)
        }
      }
      source.close()
    } catch {
      case e: Exception => println("Erreur lors de la lecture du fichier : " + e.getMessage)
    }
    machinesBuffer
  }

  // Méthode pour sauvegarder les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      writer.close()
    } catch {
      case e: Exception => println("Erreur lors de l'écriture dans le fichier : " + e.getMessage)
    }
  }
}