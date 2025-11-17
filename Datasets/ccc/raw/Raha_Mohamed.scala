import io.StdIn._
import scala.util.Random
import java.io.{File, PrintWriter}
import scala.io.Source
import collection.mutable.ArrayBuffer

// Définition de la Classe Machine
class Machine(var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Pour sauvegarder machines dans fichier CSV
  def saveCSV: String = {
    s"$id,$pincode,${milk / 1000.0},$sugar,$coffee"
  }

  // Ajouter un ingrédient
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient.toLowerCase == "lait" || ingredient.toLowerCase == "milk") milk += amount
    else if (ingredient.toLowerCase == "sucre" || ingredient.toLowerCase == "sugar") sugar += amount
    else if (ingredient.toLowerCase == "café" || ingredient.toLowerCase == "coffee") coffee += amount
    else println(s"Ingrédient non valide : $ingredient")
  }

  // Retirer un ingrédient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if ((ingredient.toLowerCase == "lait" || ingredient.toLowerCase == "milk") && milk >= amount) {
      milk -= amount
      true
    } else if ((ingredient.toLowerCase == "sucre" || ingredient.toLowerCase == "sugar") && sugar >= amount) {
      sugar -= amount
      true
    } else if ((ingredient.toLowerCase == "café" || ingredient.toLowerCase == "coffee") && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println(s"Stock insuffisant ou ingrédient non valide : $ingredient")
      false
    }
  }
}

object Main {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  // Charger les machines depuis un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    try {
      val file = Source.fromFile(filename)
      val lines = file.getLines().toList.drop(1) // Ignorer l'en-tête

      var idCounter = 1
      for (line <- lines) {
        val data = line.split(",")
        if (data.length == 4) {
          val machine = new Machine(
            id = idCounter,
            pincode = data(1),
            milk = (data(2).toDouble * 1000).toInt,
            sugar = data(3).toInt,
            coffee = data(4).toInt
          )
          buffer += machine
          println(s"\nMachine $idCounter chargée avec succès.")
          idCounter += 1
        } else {
          println(s"Ligne ignorée : $line (format incorrect).")
        }
      }
      file.close()
    } catch {
      case _: Exception => println(s"Erreur lors du chargement du fichier $filename.")
    }
    buffer
  }

  // Sauvegarder les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("ID, PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.saveCSV)
      }
      writer.close()
      println("Les données ont été sauvegardées avec succès.")
    } catch {
      case _: Exception => println(s"Erreur lors de la sauvegarde dans $filename.")
    }
  }

  // Validation du PIN
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le PIN pour la machine ${machineId + 1}:")
      val enteredPin = readLine()
      if (enteredPin == machines(machineId).pincode) {
        println("Accès autorisé.")
        return true
      }
      attempts -= 1
      println(s"PIN incorrect. Il vous reste $attempts tentative(s).")
    }
    println("Trop de tentatives échouées. Retour au menu principal.")
    false
  }

  // Mettre à jour le PIN
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var valid = false
    while (!valid) {
      println("Entrez un nouveau PIN (6 chiffres) :")
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machines(machineId).pincode = newPin
        println("Le PIN a été mis à jour avec succès.")
        valid = true
      } else {
        println("PIN invalide. Veuillez réessayer.")
      }
    }
  }

  // Mode client
  def serveClient(machine: Machine): Boolean = {
    println("Sélectionnez une boisson :")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")

    val choice = readInt()
    var price = 0.0
    var coffeeNeeded = 0
    var milkNeeded = 0

    if (choice == 1) {
      price = 2.00
      coffeeNeeded = 8
    } else if (choice == 2) {
      price = 2.50
      coffeeNeeded = 6
      milkNeeded = 100
    } else if (choice == 3) {
      println("Choisissez la taille : 1) Petit, 2) Moyen, 3) Grand")
      val size = readInt()
      if (size == 1) {
        price = 2.70
        coffeeNeeded = 6
        milkNeeded = 120
      } else if (size == 2) {
        price = 3.20
        coffeeNeeded = 8
        milkNeeded = 150
      } else if (size == 3) {
        price = 3.70
        coffeeNeeded = 12
        milkNeeded = 200
      }
    }

    // Gestion des stocks
    if (machine.coffee >= coffeeNeeded && machine.milk >= milkNeeded) {
      machine.removeIngredient("coffee", coffeeNeeded)
      machine.removeIngredient("milk", milkNeeded)
      println(f"Votre boisson est prête ! Prix total : CHF $price%.2f")
      true
    } else {
      println("Ingrédients insuffisants.")
      false
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "src/machines.csv"
    machines = loadcsv(filename)

    var running = true
    while (running) {
      println("\n1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      val choice = readInt()

      if (choice == 1) {
        println("Sélectionnez une machine (1 à " + machines.length + ") :")
        val machineId = readInt() - 1
        if (machineId >= 0 && machineId < machines.length) {
          serveClient(machines(machineId))
        } else {
          println("Machine invalide.")
        }
      } else if (choice == 2) {
        println("Entrez l'ID de la machine (1 à " + machines.length + ") :")
        val machineId = readInt() - 1
        if (machineId >= 0 && machineId < machines.length && validatePin(machineId, machines)) {
          println("1) Réapprovisionner")
          println("2) Mettre à jour le PIN")
          val adminChoice = readInt()
          if (adminChoice == 1) {
            println("Entrez les quantités à ajouter (Lait, Sucre, Café) :")
            val milk = readInt()
            val sugar = readInt()
            val coffee = readInt()
            machines(machineId).addIngredient("milk", milk)
            machines(machineId).addIngredient("sugar", sugar)
            machines(machineId).addIngredient("coffee", coffee)
            println("Réapprovisionnement effectué.")
          } else if (adminChoice == 2) {
            updatePin(machineId, machines)
          }
        } else {
          println("Machine invalide ou PIN incorrect.")
        }
      } else if (choice == 3) {
        savecsv(filename, machines)
        running = false
        println("Programme terminé. Merci.")
      } else {
        println("Choix invalide.")
      }
    }
  }
}