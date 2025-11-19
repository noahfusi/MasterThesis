import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter, IOException}

case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    val lowerIngredient = ingredient.toLowerCase
    if (lowerIngredient == "milk") {
      milk += amount
    } else if (lowerIngredient == "sugar") {
      sugar += amount
    } else if (lowerIngredient == "coffee") {
      coffee += amount
    } else {
      println(s"Invalid ingredient: $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    val lowerIngredient = ingredient.toLowerCase
    if (lowerIngredient == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (lowerIngredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (lowerIngredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println(s"Insufficient $ingredient stock.")
      false
    }
  }
}


object Main {
  val filename = "machines.csv"
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val lines = Source.fromFile(filename).getLines().drop(1)
      for ((line, index) <- lines.zipWithIndex) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        val machine = Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine

        println(s"Machine ${machine.id} chargée :")
        println(s"\tID: ${machine.id}")
        println(s"\tCode PIN: ${machine.pincode}")
        println(f"\tLait: ${machine.milk / 1000.0}%.3fL")
        println(s"\tSucre: ${machine.sugar}g")
        println(s"\tCafé: ${machine.coffee}g\n")
      }
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      case _: IOException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        sys.exit(1)
      case ex: Exception =>
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println(s"Fermeture du programme.\n")
        sys.exit(1)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val file = new File(filename)
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
      machines.foreach { machine =>
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      writer.close()
      println("Fichier sauvegardé avec succes.")
    } catch {
      case _: IOException =>
        println(s"Error: Echec de l’ecriture dans $filename.")
        println(s"Le fichier peut etre verrouille ou en lecture seule.")
      case ex: Exception =>
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println(s"Fermeture du programme.\n")
        sys.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Chargement des machines depuis $filename...\n")
    loadcsv(filename)

    var execution = true
    while (execution) {
      println("\n\t\tNospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quit")
      print("> ")
      val mode = scala.io.StdIn.readInt()

      if (mode == 1) {
        clientMode()
      } else if (mode == 2) {
        adminMode()
      } else if (mode == 3) {
        savecsv(filename, machines)
        execution = false
        println("Au revoir !")
      } else {
        println("Choix invalide.")
      }
    }
  }


  def clientMode(): Unit = {
    println(s"Sélectionnez la machine (1-${machines.length}) :")
    print("> ")
    val machineId = scala.io.StdIn.readInt()

    val machineOpt = machines.find(_.id == machineId)
    if (machineOpt.isDefined) {
      val machine = machineOpt.get
      println(s"Machine ${machine.id} sélectionnée.")
      println("Veuillez sélectionner un café :")
      println("1) Expresso - 2.00 CHF")
      println("2) Cappuccino - 2.50 CHF")
      println("3) Latte - 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")

      var choixCafé = 0
      while (choixCafé != 1 && choixCafé != 2 && choixCafé != 3) {
        choixCafé = scala.io.StdIn.readLine("> ").toInt
        if (choixCafé != 1 && choixCafé != 2 && choixCafé != 3) {
          println("Entrée invalide, veuillez choisir 1, 2 ou 3.")
        }
      }

      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - 0.10 CHF")
      println("3) Moyen (10g) - 0.20 CHF")
      println("4) Beaucoup (15g) - 0.30 CHF")

      var sucre = 0
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        sucre = scala.io.StdIn.readLine("> ").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Entrée invalide, veuillez choisir 1, 2, 3 ou 4.")
        }
      }

      val sucreAmount = if (sucre == 2) 5 else if (sucre == 3) 10 else if (sucre == 4) 15 else 0
      if (sucreAmount > 0 && !machine.removeIngredient("sugar", sucreAmount)) {
        println("Erreur : Quantité de sucre insuffisante.")
        return
      }

      var tailleLatte = 0
      if (choixCafé == 3) {
        while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
          println("Quelle taille de Latte ?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          tailleLatte = scala.io.StdIn.readLine("> ").toInt
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Entrée invalide, veuillez choisir 1, 2 ou 3.")
          }
        }
      }

      val (requiredCoffee, requiredMilk) =
        if (choixCafé == 1) (8, 0)
        else if (choixCafé == 2) (6, 100)
        else handleLatteSelection(machine, tailleLatte)

      if (!machine.removeIngredient("coffee", requiredCoffee)) {
        println("Erreur : Quantité de café insuffisante.")
        return
      }
      if (requiredMilk > 0 && !machine.removeIngredient("milk", requiredMilk)) {
        println("Erreur : Quantité de lait insuffisante.")
        return
      }

      val prixBase =
        if (choixCafé == 1) 2.00
        else if (choixCafé == 2) 2.50
        else handleLattePricing(tailleLatte)

      val prixSucre =
        if (sucre == 2) 0.10
        else if (sucre == 3) 0.20
        else if (sucre == 4) 0.30
        else 0.00

      val prixTotal = prixBase + prixSucre

      println(f"Prix total : $prixTotal%.2f CHF")
      println("Veuillez payer en utilisant Twint...")
      println("Paiement accepté. Votre boisson est prête !")
    } else {
      println("ID de machine invalide.")
    }
  }


  def handleLatteSelection(machine: Machine, tailleLatte: Int): (Int, Int) = {
    if (tailleLatte == 1) {
      (6, 120)
    } else if (tailleLatte == 2) {
      (8, 150)
    } else if (tailleLatte == 3) {
      (12, 200)
    } else {
      throw new IllegalArgumentException("Taille de Latte invalide.")
    }
  }

  def handleLattePricing(tailleLatte: Int): Double = {
    if (tailleLatte == 1) {
      2.70
    } else if (tailleLatte == 2) {
      3.20
    } else if (tailleLatte == 3) {
      3.70
    } else {
      throw new IllegalArgumentException("Taille de Latte invalide.")
    }
  }



  def adminMode(): Unit = {
    println(s"Sélectionnez la machine (1-${machines.length}) :")
    print("> ")
    val machineId = scala.io.StdIn.readInt()

    val machineOpt = machines.find(_.id == machineId)
    if (machineOpt.isDefined) {
      val machine = machineOpt.get
      println("Enter admin PIN:")
      val enteredPin = scala.io.StdIn.readLine("> ")
      if (enteredPin == machine.pincode) {
        println("Admin access granted.")
        println("1) Restock Ingredients")
        println("2) Remove Ingredients")
        println("3) Update PIN")
        print("> ")
        val action = scala.io.StdIn.readInt()

        if (action == 1) {
          println("Enter ingredient (milk/sugar/coffee) and amount to add:")
          val ingredient = scala.io.StdIn.readLine("> ")
          print("> ")
          val amount = scala.io.StdIn.readInt()
          machine.addIngredient(ingredient, amount)

        } else if (action == 2) {
          println("Enter ingredient (milk/sugar/coffee) and amount to remove:")
          val ingredient = scala.io.StdIn.readLine("> ")
          print("> ")
          val amount = scala.io.StdIn.readInt()
          if (machine.removeIngredient(ingredient, amount)) {
            println(s"$amount units of $ingredient removed successfully.")
          }

        } else if (action == 3) {
          println("Enter new 6-digit PIN:")
          var newPin = scala.io.StdIn.readLine("> ")
          while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
            println("Invalid PIN. Enter a 6-digit numeric PIN:")
            newPin = scala.io.StdIn.readLine("> ")
          }
          machine.pincode = newPin
          println("PIN updated successfully.")

        } else {
          println("Invalid action.")
        }
      } else {
        println("Incorrect PIN.")
      }
    } else {
      println("Invalid machine ID.")
    }
  }

}
