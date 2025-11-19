import scala.io.StdIn
import scala.util.{Random, Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer
import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.io.Source

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("L'ingrédient demandé n'est pas offert.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount =>
        milk -= amount
        true
      case "sugar" if sugar >= amount =>
        sugar -= amount
        true
      case "coffee" if coffee >= amount =>
        coffee -= amount
        true
      case _ =>
        println("Il n'y a pas assez de " + ingredient + ".")
        false
    }
  }
}

def loadcsv(filename: String): ArrayBuffer[Machine] = {
  val machines = ArrayBuffer[Machine]()
  try {
    for (line <- Source.fromFile(filename).getLines().drop(1)) {
      val Array(pincode, milk, sugar, coffee) = line.split(",")
      machines += new Machine(machines.length + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
    }
    println(machines.length + " machine(s) chargée(s) avec succès.")
  } catch {
    case _: java.io.FileNotFoundException =>
      println("Le fichier demandé n'a pas été trouvé. Vérifiez votre entrée et réessayez.")
      System.exit(1)
    case e: Exception =>
      println("Erreur au cours du chargement du fichier : " + e.getMessage)
      System.exit(1)
  }
  machines
}

def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
  try {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(filename)))
    writer.println("PINCODE,MILK,SUGAR,COFFEE")
    for (machine <- machines) {
      writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
    }
    writer.close()
    println("Fichier sauvegardé avec succès.")
  } catch {
    case e: Exception =>
      println("Erreur lors de la modification du le fichier : " + e.getMessage)
  }
}

object NespresseEx3 {
  val nbMachines: Int = 5
  var machines: ArrayBuffer[Machine] = loadcsv("machines.csv")

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Nespresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n")
      print("> ")
      val mode: Byte = StdIn.readLine().toByte

      mode match {
        case 1 => modeClient()
        case 2 => modeAdmin()
        case 3 =>
          savecsv("machines.csv", machines)
          println("À bientôt")
          System.exit(0)
        case _ => println("Veuillez entrer une valeur valide")
      }
    }
  }

  def modeClient(): Unit = {
    var processus = true
    while (processus) {
      println("Veuillez sélectionner une machine de 1 à " + nbMachines + ":")
      print("> ")

      val machineId: Int = StdIn.readLine().toInt - 1
      if (machineId < 0 || machineId >= nbMachines) {
        println("La machine sélectionnée n'existe pas.")
        processus = false
      } else {
        val machine = machines(machineId)
        println("Sélectionnez votre produit :\n1) Expresso\n2) Cappuccino\n3) Latte\n4) Retour")
        print("> ")
        val produit: Byte = StdIn.readLine().toByte

        produit match {
          case 1 => serveExpresso(machine)
          case 2 => serveCappuccino(machine)
          case 3 => serveLatte(machine)
          case 4 => processus = false
          case _ => println("Veuillez entrer une valeur valide")
        }
      }
    }
  }

  def serveExpresso(machine: Machine): Unit = {
    if (machine.removeIngredient("coffee", 8)) {
      println("Prix à payer: 2 CHF")
      processPayment()
    }
  }

  def serveCappuccino(machine: Machine): Unit = {
    if (machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)) {
      println("Prix à payer: 2.50 CHF")
      processPayment()
    }
  }

  def serveLatte(machine: Machine): Unit = {
    if (machine.removeIngredient("coffee", 8) && machine.removeIngredient("milk", 150)) {
      println("Prix à payer: 3.20 CHF")
      processPayment()
    }
  }

  def processPayment(): Unit = {
    val twintCode = Random.alphanumeric.take(5).mkString
    println("Code Twint généré : " + twintCode)
    println("Paiement en cours...")
    Thread.sleep(3000)
    println("Paiement validé. Merci !")
  }

  def modeAdmin(): Unit = {
    println("Mode Administrateur\nVeuillez entrer le code PIN :")
    print("> ")
    val pin: Short = StdIn.readLine().toShort

    if (pin == 434343) {
      println("Accès autorisé.")
      showStock()
      println("Souhaitez-vous réapprovisionner les Stocks?\n1) Oui\n2) Non")
      var approvisionnement: Byte = StdIn.readLine().toByte
      if (approvisionnement == 1) {
        reapprovisionner()
      }
    } else {
      println("Code erroné.")
    }
  }

  def showStock(): Unit = {
    for (machine <- machines) {
      println("Machine ID: " + machine.id + ", Lait: " + machine.milk + "ml, Sucre: " + machine.sugar + "g, Café: " + machine.coffee + "g")
    }
  }

  def reapprovisionner(): Unit = {
    println("Souhaitez-vous réapprovisionner :\n1) Poudre de café\n2) Lait\n3) Sucre")
    var produit: Byte = StdIn.readLine().toByte

    println("Indiquez la quantité à ajouter :")
    var apport: Int = StdIn.readLine().toInt

    produit match {
      case 1 => machines.foreach(_.addIngredient("coffee", apport))
      case 2 => machines.foreach(_.addIngredient("milk", apport))
      case 3 => machines.foreach(_.addIngredient("sugar", apport))
      case _ => println("Veuillez entrer une valeur valide")
    }
  }
}