import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.{Try, Success, Failure}
import scala.io.StdIn._
import scala.util.Random

// Classe Machine
class Machine(val id: Int, val pincode: String, private var milk: Int, private var sugar: Int, private var coffee: Int) {
  def displayStatus(): Unit = {
    println("Machine " + id + ": Milk = " + milk + " ml, Sugar = " + sugar + " g, Coffee = " + coffee + " g")
  }

  def getMilk: Int = milk
  def getSugar: Int = sugar
  def getCoffee: Int = coffee

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient non valide: " + ingredient)
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println("Quantité insuffisante ou ingrédient non valide: " + ingredient); false
    }
  }
}

object Main {
  val machines = ArrayBuffer[Machine]()

  def loadcsv(filename: String): Unit = {
    Try {
      println("Chargement des machines depuis " + filename + "...")
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1) // Ignorer l'en-tête

      for ((line, index) <- lines.zipWithIndex) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        val machine = new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine
        println("Machine " + machine.id + " chargée : ID: " + machine.id + " Code PIN: " + machine.pincode)
        machine.displayStatus()
      }
      source.close()
      println("\n" + machines.length + " machine(s) chargée(s) avec succès.\n")
    } match {
      case Success(_) => println("Chargement terminé.")
      case Failure(ex) => println("Erreur : " + ex.getMessage)
    }
  }

  def serveClient(machine: Machine): Unit = {
    println("Veuillez sélectionner votre boisson: ")
    println("1) Expresso - 8g de café")
    println("2) Cappuccino - 6g de café, 100ml de lait")
    println("3) Latte petit - 6g de café, 120ml de lait")
    println("4) Latte moyen - 8g de café, 150ml de lait")
    println("5) Latte grand - 12g de café, 200ml de lait")

    val choice = readLine().toInt
    val (coffeeNeeded, milkNeeded, basePrice) = choice match {
      case 1 => (8, 0, 2.0)
      case 2 => (6, 100, 2.5)
      case 3 => (6, 120, 2.7)
      case 4 => (8, 150, 3.2)
      case 5 => (12, 200, 3.7)
      case _ =>
        println("Choix invalide.")
        return
    }

    if (!machine.removeIngredient("coffee", coffeeNeeded)) {
      println("Café insuffisant.")
    } else if (!machine.removeIngredient("milk", milkNeeded)) {
      println("Lait insuffisant.")
    } else {
      var extraMilk = 0
      if (choice != 1) {
        println("Souhaitez-vous ajouter du lait supplémentaire ? (1: Oui, 2: Non)")
        val addMilk = readLine().toInt
        if (addMilk == 1) {
          println("Combien de doses supplémentaires voulez-vous ajouter ? (10ml par dose, max 3 doses)")
          val doses = readLine().toInt
          extraMilk = doses * 10
          if (!machine.removeIngredient("milk", extraMilk)) {
            println("Lait insuffisant pour ajouter les doses supplémentaires.")
            return
          }
        }
      }

      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g)")
      println("3) Moyen (10g)")
      println("4) Beaucoup (15g)")

      val sugarChoice = readLine().toInt
      val sugarNeeded = sugarChoice match {
        case 2 => 5
        case 3 => 10
        case 4 => 15
        case _ => 0
      }

      if (!machine.removeIngredient("sugar", sugarNeeded)) {
        println("Sucre insuffisant.")
        return
      }

      val totalPrice = basePrice + (extraMilk * 0.05) + (sugarNeeded match {
        case 5 => 0.10
        case 10 => 0.20
        case 15 => 0.30
        case _ => 0.0
      })

      println("Le prix total de votre boisson est : CHF " + totalPrice)
      println("Veuillez payer le montant de CHF " + totalPrice + " avec Twint en saisissant le code de paiement suivant: ")
      val codeTwint = Random.alphanumeric.filter(_.isLetterOrDigit).take(5).mkString.toUpperCase
      println(codeTwint)

      println("En attente de validation du paiement...")
      Thread.sleep(3000)

      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(3000)
      println("Votre boisson est prête ! Bonne dégustation !")
    }
  }

  def savecsv(filename: String): Unit = {
    Try {
      println("Sauvegarde des machines dans " + filename + "...")
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.getMilk + "," + machine.getSugar + "," + machine.getCoffee)
      }
      writer.close()
      println("Sauvegarde réussie.")
    } match {
      case Success(_) => println("Fichier sauvegardé avec succès.")
      case Failure(ex) => println("Erreur lors de la sauvegarde : " + ex.getMessage)
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    loadcsv(filename)

    var continue = true
    while (continue) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val mode = readLine().trim

      mode match {
        case "1" =>
          println("Veuillez sélectionner votre machine (1 - " + machines.length + "):")
          val machineId = readLine().toInt - 1
          if (machineId >= 0 && machineId < machines.length) {
            serveClient(machines(machineId))
          } else {
            println("Numéro de machine invalide.")
          }

        case "2" =>
          println("Veuillez sélectionner votre machine (1 - " + machines.length + "):")
          val machineId = readLine().toInt - 1
          if (machineId >= 0 && machineId < machines.length) {
            var attempts = 0
            val maxAttempts = 3
            var accessGranted = false

            while (attempts < maxAttempts && !accessGranted) {
              println("Entrez le code PIN: ")
              val pin = readLine()
              if (machines(machineId).pincode == pin) {
                accessGranted = true
                println("Accès accordé. Que voulez-vous faire ?")
                println("1) Réapprovisionner les stocks")
                println("2) Voir l'état de la machine")
                val action = readLine().trim
                action match {
                  case "1" =>
                    println("Ajout de lait (en ml): ")
                    val milk = readLine().toInt
                    machines(machineId).addIngredient("milk", milk)

                    println("Ajout de sucre (en g): ")
                    val sugar = readLine().toInt
                    machines(machineId).addIngredient("sugar", sugar)

                    println("Ajout de café (en g): ")
                    val coffee = readLine().toInt
                    machines(machineId).addIngredient("coffee", coffee)

                  case "2" => machines(machineId).displayStatus()

                  case _ => println("Action non valide.")
                }
              } else {
                attempts += 1
                if (attempts < maxAttempts) {
                  println("Code PIN incorrect. " + (maxAttempts - attempts) + " tentative(s) restante(s).")
                } else {
                  println("Trop de tentatives échouées. Retour au menu principal.")
                }
              }
            }
          } else {
            println("Numéro de machine invalide.")
          }

        case "3" =>
          println("Vous avez sélectionné quitter. À bientôt !")
          savecsv(filename)
          continue = false

        case _ => println("Mode non valide. Veuillez réessayer.")
      }
    }
  }
}
