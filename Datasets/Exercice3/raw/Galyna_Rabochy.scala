import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.io.StdIn._
import scala.util.Random


class Machine(val id: Int, var pincode: String, var milk: Double, var sugar: Int, var coffee: Int) {

  def display(): Unit = {
    println("Machine"+id+"chargée :")
    println("ID:"+id+"")
    println("Code PIN:"+pincode+"")
    println("Lait:"+milk / 1000.0+"L")
    println("Sucre:"+sugar+"g")
    println("Café:"+coffee+"g")
  }

  def toCSV: String = pincode+","+milk+","+sugar+","+coffee

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" =>
        milk += amount
        println(+amount+" ml de lait ajoutés. Nouveau stock :"+milk / 1000.0+"L")
      case "sugar" =>
        sugar += amount
        println(+amount+" g de sucre ajoutés. Nouveau stock :"+sugar+"g")
      case "coffee" =>
        coffee += amount
        println(+amount+ "g de café ajoutés. Nouveau stock :"+coffee+"g")
      case _ =>
        println("Erreur : Ingrédient invalide. Les ingrédients valides sont 'milk', 'sugar' et 'coffee'.")
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" =>
        if (milk >= amount) {
          milk -= amount
          println(+amount+" ml de lait retirés. Nouveau stock :"+milk / 1000.0+"%.3f L")
          true
        } else {
          println("Erreur : Quantité insuffisante de lait.")
          false
        }
      case "sugar" =>
        if (sugar >= amount) {
          sugar -= amount
          println(+amount+" g de sucre retirés. Nouveau stock :"+sugar+" g")
          true
        } else {
          println("Erreur : Quantité insuffisante de sucre.")
          false
        }
      case "coffee" =>
        if (coffee >= amount) {
          coffee -= amount
          println(+amount+" g de café retirés. Nouveau stock :"+coffee+"g")
          true
        } else {
          println("Erreur : Quantité insuffisante de café.")
          false
        }
      case _ =>
        println("Erreur : Ingrédient invalide. Les ingrédients valides sont 'milk', 'sugar' et 'coffee'.")
        false
    }
  }
}


object Nospresso {
  val NB_MACHINES = 5


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val lines = Source.fromFile(filename).getLines().drop(1)
      for ((line, index) <- lines.zipWithIndex) {
        val cols = line.split(",").map(_.trim)
        machines += new Machine(index + 1, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
        machines.last.display()
      }
      println(+machines.size+ " machine(s) chargée(s) avec succès")
    } catch {
      case _: Exception => println("Erreur : Fichier introuvable ou impossible à lire.")
    }
    machines
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.toCSV)
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case _: Exception => println("Erreur : Échec de la sauvegarde du fichier.")
    }
  }


  def clientMode(machines: ArrayBuffer[Machine]): Unit = {
    var machineId = -1
    do {
      println("Veuillez sélectionner une machine (1 à " + machines.size + ")")
      print("> ")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        machineId = input.toInt - 1
      } else {
        machineId = -1
      }


      if (machineId < 0 || machineId >= machines.size) {
        println("ID de machine invalide. Veuillez recommencer.")
      }
    } while (machineId < 0 || machineId >= machines.size)


    val machine = machines(machineId)
    println(s"Bienvenue au mode Client pour la machine ${machine.id}.")
    if (serveClient(machine)) {
      println("Commande servie avec succès.")
    } else {
      println("Erreur : Commande non servie en raison de stocks insuffisants ou d'une erreur d'entrée.")
    }
  }


  def serveClient(machine: Machine): Boolean = {
    var drinkChoice = ""
    do {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      drinkChoice = readLine()
      if (drinkChoice != "1" && drinkChoice != "2" && drinkChoice != "3") {
        println("Option invalide. Veuillez entrer 1, 2 ou 3.")
      }
    } while (drinkChoice != "1" && drinkChoice != "2" && drinkChoice != "3")


    val (basePrice, coffeeNeeded, milkNeeded) = drinkChoice match {
      case "1" => (2.00, 8, 0)
      case "2" => (2.50, 6, 100)
      case "3" =>
        var sizeChoice = ""
        do {
          println("Choisissez une taille pour le Latte :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          sizeChoice = readLine()
          if (sizeChoice != "1" && sizeChoice != "2" && sizeChoice != "3") {
            println("Option invalide. Veuillez entrer 1, 2 ou 3.")
          }
        } while (sizeChoice != "1" && sizeChoice != "2" && sizeChoice != "3")
        sizeChoice match {
          case "1" => (2.70, 6, 120)
          case "2" => (3.20, 8, 150)
          case "3" => (3.70, 12, 200)
          case _ =>
            println("Option invalide.")
            return false
        }
      case _ =>
        println("Option invalide.")
        return false
    }


    var sugarChoice = ""
    do {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      sugarChoice = readLine()
      if (sugarChoice != "1" && sugarChoice != "2" && sugarChoice != "3" && sugarChoice != "4") {
        println("Option invalide. Veuillez entrer 1, 2, 3 ou 4.")
      }
    } while (sugarChoice != "1" && sugarChoice != "2" && sugarChoice != "3" && sugarChoice != "4")


    val (sugarNeeded, sugarPrice) = sugarChoice match {
      case "1" => (0, 0.00)
      case "2" => (5, 0.10)
      case "3" => (10, 0.20)
      case "4" => (15, 0.30)
    }


    var extraMilk = 0.0
    var extraMilkPrice = 0.0
    if (drinkChoice == "2" || drinkChoice == "3") {
      var validMilk = false
      do {
        println("Souhaitez-vous ajouter du lait supplémentaire ? (1=Oui, 2=Non)")
        print("> ")
        val addMilk = readLine()
        if (addMilk == "1") {
          println("Combien de doses supplémentaires (0-3) ?")
          val doses = readLine().toIntOption.getOrElse(-1)
          if (doses >= 0 && doses <= 3) {
            extraMilk = doses * 50.0
            extraMilkPrice = doses * 0.05
            validMilk = true
          } else {
            println("Quantité de doses invalide. Veuillez réessayer.")
          }
        } else if (addMilk == "2") {
          validMilk = true
        } else {
          println("Option invalide. Veuillez entrer 1 ou 2.")
        }
      } while (!validMilk)
    }


    if (coffeeNeeded > machine.coffee) {
      println("Erreur : Quantité insuffisante de café.")
      return false
    }
    if ((milkNeeded + extraMilk) > machine.milk) {
      println("Erreur : Quantité insuffisante de lait.")
      return false
    }
    if (sugarNeeded > machine.sugar) {
      println("Erreur : Quantité insuffisante de sucre.")
      return false
    }

    machine.coffee -= coffeeNeeded
    machine.milk -= (milkNeeded + extraMilk)
    machine.sugar -= sugarNeeded

    val totalPrice = basePrice + sugarPrice + extraMilkPrice
    println("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f".format(basePrice,sugarPrice,extraMilkPrice,totalPrice))
    println("Veuillez payer en utilisant Twint.")
    val paymentCode = Random.alphanumeric.take(5).mkString.toUpperCase()
    println("Votre code de paiement est :" + paymentCode + " ")
    println("(En attente de validation du paiement...)")
    Thread.sleep(5000)
    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...\n [...]\n")
    Thread.sleep(5000)
    println("Votre " + drinkChoice + " est prêt ! Bonne dégustation !\n")


    true
  }


  def adminMode(machines: ArrayBuffer[Machine]): Unit = {
    val machine = selectMachine(machines)
    println(s"Mode Admin pour la machine ${machine.id}.")

    var attempts = 3
    var accessGranted = false

    while (attempts > 0 && !accessGranted) {
      println("Entrez le code PIN :")
      val inputPin = readLine()
      if (inputPin == machine.pincode) {
        println("Accès autorisé.")
        accessGranted = true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. Tentatives restantes : $attempts")
      }
    }


    if (accessGranted) {
      machine.display()


      var choice = ""
      do {
        println(s"Souhaitez-vous réinitialiser le code PIN de la machine ${machine.id} ?")
        println("1) Oui")
        println("2) Non")
        print("> ")
        choice = readLine()
        if (choice != "1" && choice != "2") {
          println("Option invalide. Veuillez entrer 1 ou 2.")
        }
      } while (choice != "1" && choice != "2")


      if (choice == "1") {
        var newPin = ""
        do {
          println(s"Entrez un nouveau code PIN pour la machine ${machine.id} (6 chiffres) :")
          newPin = readLine()
          if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
            println("Erreur : le code doit être composé exactement de 6 chiffres. Veuillez réessayer.")
          }
        } while (newPin.length != 6 || !newPin.forall(_.isDigit))

        machine.pincode = newPin
        println(s"Code PIN de la machine ${machine.id} mis à jour avec succès.")
      } else {
        println(s"Réinitialisation du code PIN pour la machine ${machine.id} annulée.")
      }


      var recharge = ""
      do {
        println("Souhaitez-vous réapprovisionner les stocks ? (1=Oui, 2=Non)")
        print("> ")
        recharge = readLine()
        if (recharge != "1" && recharge != "2") {
          println("Option invalide. Veuillez entrer 1 ou 2.")
        }
      } while (recharge != "1" && recharge != "2")


      if (recharge == "1") {
        println("Quantité de lait à ajouter (ml) :")
        machine.milk += readLine().toInt


        println("Quantité de sucre à ajouter (g) :")
        machine.sugar += readLine().toInt


        println("Quantité de café à ajouter (g) :")
        machine.coffee += readLine().toInt


        println("Stocks mis à jour avec succès.")
      } else {
        println("Réapprovisionnement annulé.")
      }
    } else {
      println("Accès refusé après 3 tentatives échouées.")
    }
  }

  def selectMachine(machines: ArrayBuffer[Machine]): Machine = {
    var machineId = -1
    do {
      println("Veuillez sélectionner une machine par son ID :")
      for (m <- machines) println(s"${m.id}) Machine ${m.id}")
      print("> ")
      machineId = readLine().toIntOption.getOrElse(-1)
      if (!machines.exists(_.id == machineId)) {
        println("ID de machine invalide. Veuillez réessayer.")
        machineId = -1
      }
    } while (machineId == -1)


    machines.find(_.id == machineId).get
  }


  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    println("Chargement des machines depuis machines.csv...")
    val machines = loadcsv(filename)


    var running = true
    while (running) {
      var modeChoice = ""
      do {
        println("Nospresso Café")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")


        modeChoice = readLine()
        if (modeChoice != "1" && modeChoice != "2" && modeChoice != "3") {
          println("Option invalide. Veuillez entrer 1, 2 ou 3.")
        }
      } while (modeChoice != "1" && modeChoice != "2" && modeChoice != "3")


      if (modeChoice == "1") {
        clientMode(machines)
      } else if (modeChoice == "2") {
        adminMode(machines)
      } else if (modeChoice == "3") {
        println(s"Sauvegarde de ${machines.size} machine(s) dans $filename...")
        savecsv(filename, machines)
        running = false
      }
    }
  }
}

