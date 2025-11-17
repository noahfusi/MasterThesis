import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}

class Machine(val id: Int, var pincode: String, var lait: Int, var sucre: Int, var cafe: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "lait") {
      lait += amount
    } else if (ingredient == "sucre") {
      sucre += amount
    } else if (ingredient == "café") {
      cafe += amount
    } else {
      println("ingrédient inconnu.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "lait" && lait >= amount) {
      lait -= amount
      true
    } else if (ingredient == "sucre" && sucre >= amount) {
      sucre -= amount
      true
    } else if (ingredient == "cafe" && cafe >= amount) {
      cafe -= amount
      true
    } else {
      println(s"$ingredient insuffisant.")
      false
    }
  }
}

object Nospresso {
  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    if (!loadCSV("machines.csv")) {
      println("Veuillez réssayer.")
      return
    }

    var continuer = true // check befor submission
    while (continuer) {
      println("\nNospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quit")
      print("> ")

      val choice = scala.io.StdIn.readLine()
      if (choice == "1") clientMode()
      else if (choice == "2") adminMode()
      else if (choice == "3") {
        saveCSV("machines.csv")
        println("Merci!")
        continuer = false
      } else {
        println("Option invalide, veuillez réessayer.")
      }
    }
  }

  def clientMode(): Unit = {
    println("Choisisez une machine (1 to " + machines.length + "):")
    val machineIdx = scala.io.StdIn.readLine().toInt - 1

    if (machineIdx < 0 || machineIdx >= machines.length) {
      println("Invalide.")
      return
    }

    val machine = machines(machineIdx)
    println("1) Espresso (8g coffee, CHF 2.00)")
    println("2) Cappuccino (6g coffee, 150ml milk, CHF 2.50)")
    println("3) Latte (10g coffee, 200ml milk, CHF 3.00)")
    print("> ")

    val choice = scala.io.StdIn.readLine()
    var cafe = 0
    var lait = 0
    var prix = 0.0

    if (choice == "1") {
      cafe = 8; prix = 2.00
    } else if (choice == "2") {
      cafe = 6; lait = 150; prix = 2.50
    } else if (choice == "3") {
      cafe = 10; lait = 200; prix = 3.00
    } else {
      println("Choix invalide.")
      return
    }

    if (machine.removeIngredient("cafe", cafe) && machine.removeIngredient("lait", lait)) {
      println(s"Merci! Totale: CHF $prix")
    }
  }

  def adminMode(): Unit = {
    println("Choisisez une machine(1 to " + machines.length + "):")
    val machineIdx = scala.io.StdIn.readLine().toInt - 1

    if (machineIdx < 0 || machineIdx >= machines.length) {
      println("Machine invalide.")
      return
    }

    val machine = machines(machineIdx)
    println("PIN:")
    val pinInput = scala.io.StdIn.readLine()

    if (pinInput != machine.pincode) {
      println("Incorrect, veuillez réessayer")
      return
    }

    println("1) Réapprovisionner")
    println("2) Changer le PIN")
    println("3) Voir toutes les machines")
    print("> ")

    val choice = scala.io.StdIn.readLine()
    if (choice == "1") {
      println("Quantité à ajouter pour chaque ingrédient - lait, sucre, café:")
      print("lait: "); machine.addIngredient("lait", scala.io.StdIn.readLine().toInt)
      print("sucre: "); machine.addIngredient("sucre", scala.io.StdIn.readLine().toInt)
      print("café: "); machine.addIngredient("café", scala.io.StdIn.readLine().toInt)
      println("Réapprovisionnement effectué.")
    } else if (choice == "2") {
      println("Nouveau code PIN (6 chiffres)")
      machine.pincode = scala.io.StdIn.readLine()
      println("PIN mise à jour.")
    } else if (choice == "3") {
      println(s"Machine ${machine.id}: PIN=${machine.pincode}, Milk=${machine.lait}ml, Sugar=${machine.sucre}g, Coffee=${machine.cafe}g")
    } else {
      println("Choix invalide.")
    }
  }

  def loadCSV(filename: String): Boolean = {
    try {
      val lines = Source.fromFile(filename).getLines().toArray
      for (i <- 1 until lines.length) {
        val data = lines(i).split(",")
        machines += new Machine(i, data(0), data(1).toInt, data(2).toInt, data(3).toInt)
      }
      println(s"${machines.length} machine enregistré.")
      true
    } catch {
      case _: Exception =>
        println("Erreur.")
        false
    }
  }

  def saveCSV(filename: String): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,lait, sucre, café")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.lait},${machine.sucre},${machine.cafe}")
      }
      writer.close()
      println("Machines enregistrés .")
    } catch {
      case _: Exception =>
        println("Erreur.")
    }
  }
}