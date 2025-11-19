import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.io.StdIn.readLine


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient inconnu : $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient match {
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
        println(s"Stock insuffisant pour $ingredient.")
        false
    }
  }
}


object NospressoCafe {
  val machines = ArrayBuffer[Machine]()

  def loadcsv(filename: String): Unit = {
    println("Chargement des machines depuis machines.csv...")
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1)
      lines.zipWithIndex.foreach { case (line, index) =>
        val Array(pincode, milk, sugar, coffee) = line.split(",").map(_.trim)
        val machine = new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine
        println(s"Machine ${index + 1} chargée : ID: ${index + 1} Code PIN: $pincode Lait: ${milk.toDouble / 1000}L Sucre: ${sugar}g Café: ${coffee}g")
      }
      println(s"${machines.length} machine(s) chargée(s) avec succès.")
      source.close()
    } catch {
      case e: java.io.FileNotFoundException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(1)
      case e: Exception =>
        println(s"Erreur lors du chargement des machines : ${e.getMessage}")
        System.exit(1)
    }
  }

  def savecsv(filename: String): Unit = {
    println("Sauvegarde des machines dans machines.csv...")
    try {
      val writer = new java.io.PrintWriter(new java.io.File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println(s"Fichier sauvegardé avec succès.")
    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename. Le fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        System.exit(1)
      case e: Exception =>
        println(s"Erreur lors de la sauvegarde des machines : ${e.getMessage}")
        System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    loadcsv("machines.csv")

    while (true) {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      readLine().trim match {
        case "1" => modeClient()
        case "2" => modeAdmin()
        case "3" =>
          println("Merci d'avoir utilisé Nospresso Café. Au revoir !")
          savecsv("machines.csv")
          System.exit(0)
        case _ => println("Entrée invalide. Veuillez réessayer.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner une machine (1-5) :")
    print("> ")
    val machineId = readLine().toIntOption.getOrElse(-1)
    if (machineId >= 1 && machineId <= machines.length) {
      serveClient(machineId)
    } else {
      println("Machine invalide. Retour au menu principal.")
    }
  }

  def modeAdmin(): Unit = {
    println("Veuillez sélectionner une machine (1-5) :")
    print("> ")
    val machineId = readLine().toIntOption.getOrElse(-1)
    if (machineId >= 1 && machineId <= machines.length) {
      val machine = machines(machineId - 1)
      println("1) Reapprovisionner les stocks")
      println("2) Mettre à jour le code PIN")
      print("> ")
      readLine().trim match {
        case "1" => restockMachine(machine)
        case "2" => updatePin(machine)
        case _ => println("Entrée invalide. Retour au menu principal.")
      }
    } else {
      println("Machine invalide.")
    }
  }

  def serveClient(machineId: Int): Unit = {
    val machine = machines(machineId - 1)
    println(s"Machine ${machine.id} : Veuillez sélectionner votre boisson :")
    println("1) Expresso - 8g café, 0 lait")
    println("2) Cappuccino - 6g café, 100ml lait")
    println("3) Latte - 6g café, 120ml lait")
    print("> ")
    val choice = readLine().trim
    val (cafe, lait) = choice match {
      case "1" => (8, 0)
      case "2" => (6, 100)
      case "3" => (6, 120)
      case _ => println("Entrée invalide. Retour au menu principal."); return
    }
    if (machine.removeIngredient("coffee", cafe) && machine.removeIngredient("milk", lait)) {
      println("Votre boisson a été préparée avec succès !")
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println(s"Machine ${machine.id} : Reapprovisionnement des stocks.")
    print("Ajout de café (g) : ")
    val coffee = readLine().toIntOption.getOrElse(0)
    print("Ajout de sucre (g) : ")
    val sugar = readLine().toIntOption.getOrElse(0)
    print("Ajout de lait (ml) : ")
    val milk = readLine().toIntOption.getOrElse(0)
    machine.addIngredient("coffee", coffee)
    machine.addIngredient("sugar", sugar)
    machine.addIngredient("milk", milk)
    println("Stocks mis à jour avec succès.")
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN :")
    val newPin = readLine().trim
    machine.pincode = newPin
    println("Code PIN mis à jour avec succès.")
  }
}
