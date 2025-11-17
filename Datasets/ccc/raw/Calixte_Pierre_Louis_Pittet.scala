import scala.io.StdIn
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.{File, FileNotFoundException, PrintWriter}
import scala.io.Source



class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "lait" => milk += amount
      case "sucre" => sugar += amount
      case "cafe" => coffee += amount
      case _ => println("Ingrédient invalide")
    }
  }


  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient.equalsIgnoreCase("Lait")) {
      if (milk >= amount) {
        milk -= amount
        true
      } else {
        false
      }
    } else if (ingredient.equalsIgnoreCase("Sucre")) {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else {
        false
      }
    } else if (ingredient.equalsIgnoreCase("Cafe")) {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else {
        false
      }
    } else {
      false
    }
  }
}


object NospressoSystem {
  val MAX_PIN_ATTEMPTS = 3
  var machines = ArrayBuffer[Machine]()


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().toList
      source.close()


      if (lines.isEmpty) throw new Exception("Fichier vide")


      val machinesData = lines.tail.zipWithIndex.map { case (line, index) =>
        val fields = line.split(",")
        if (fields.length != 4) throw new Exception(s"Données invalides ${index + 2}")


        try {
          new Machine(
            id = index + 1,
            pincode = fields(0),
            milk = fields(1).toInt,
            sugar = fields(2).toInt,
            coffee = fields(3).toInt
          )
        } catch {
          case e: Throwable =>
            if (e.isInstanceOf[NumberFormatException]) {
              throw new Exception(s"Invalide ${index + 2}")
            } else {
              throw e
            }
        }
      }


      ArrayBuffer(machinesData: _*)
    } catch {
      case e: Throwable =>
        if (e.isInstanceOf[FileNotFoundException]) {
          println("Erreur: Fichier introuvable. Verifier le chemin d'accès et réessayer.")
          throw e
        } else {
          println(s"Erreur : Echec de l'écriture dans machine.cdv. Le fichier peut être verrouillé ou en lecture seule. ${e.getMessage}")
          throw e
        }
    }
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("CodePIN,Lait,Sucre,Cafe")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines. Fermeture du programme. ${e.getMessage}")
        throw e
    }
  }


  def validatePin(machine: Machine): Boolean = {
    var remainingAttempts = MAX_PIN_ATTEMPTS
    while (remainingAttempts > 0) {
      println("Entrer le code PIN:")
      val enteredPin = StdIn.readLine()


      if (enteredPin == machine.pincode) {
        return true
      }


      remainingAttempts -= 1
      if (remainingAttempts > 0) {
        println(s"Code PIN incorrect. $remainingAttempts essais restants.")
      }
    }


    println("Trop d'essais échoués. Access refusé.")
    false
  }


  def displayMachineInfo(machine: Machine): Unit = {
    println(s"CoffeeMachine ${machine.id} Chargement............")
    println(s"ID: ${machine.id}")
    println(f"Lait: ${machine.milk / 1000.0}%.1fL")
    println(f"Sucre: ${machine.sugar}%.1fg")
    println(f"Poudre de cafe: ${machine.coffee}%.1fg")
  }


  def updatePin(machine: Machine): Unit = {
    var newPin = ""
    do {
      println("Entrer un nouveau PIN à 6 chiffres")
      newPin = StdIn.readLine()
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))


    machine.pincode = newPin
    println("PIN mis à jour avec succès.")
  }


  def getIngredientRequirements(beverage: String, size: String, doselait: String): (Int, Int, Int) = {
    if (beverage == "Espresso") {
      (8, 0, 0)
    } else if (beverage == "Cappuccino") {
      if(doselait == "1"){
        (6,150,10)
      } else if (doselait == "2"){
        (6,200, 10)
      } else if(doselait == "3"){
        (6, 250, 10)
      } else {(6, 100, 10)}
    } else if (beverage == "Latte") {
      if (size == "Petit") {
        if(doselait == "1"){
          (6,170,5)
        } else if (doselait == "2"){
          (6,220, 5)
        } else if(doselait == "3"){
          (6, 270, 5)
        } else {(6, 120, 5)}
      } else if (size == "Moyen") {
        if(doselait == "1"){
          (8,200,10)
        } else if (doselait == "2"){
          (8,270, 10)
        } else if(doselait == "3"){
          (8, 320, 10)
        } else {(8, 150, 5)}
      } else if (size == "Grand") {
        if(doselait == "1"){
          (12,250,15)
        } else if (doselait == "2"){
          (12,300, 15)
        } else if(doselait == "3"){
          (12, 350, 15)
        } else {(12, 200, 15)}
      } else {
        (0, 0, 0)
      }
    } else {
      (0, 0, 0)
    }
  }


  def getBasePrice(beverage: String, size: String): Double = {
    if (beverage == "Espresso") {
      2.00
    } else if (beverage == "Cappuccino") {
      2.50
    } else if (beverage == "Latte") {
      if (size == "Petit") {
        2.70
      } else if (size == "Moyen") {
        3.20
      } else if (size == "Grand") {
        3.70
      } else {
        0.0
      }
    } else {
      0.0
    }
  }


  def serveClient(machine: Machine): Boolean = {
    println("Veuillez choisir votre boisson:")
    println("1. Espresso - CHF 2.00")
    println("2. Cappuccino - CHF 2.50")
    println("3. Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")


    val beverageChoice = StdIn.readLine()

    var baverageb = ""
    var sizou = ""
    var dosou = ""
      if (beverageChoice == "1") {
      baverageb = "Espresso"
      sizou = "standard"} else if (beverageChoice == "2"){
        baverageb = "Cappuccino"
        sizou = "Standard"
        println("voulez vous du extra lait ? 0) 0 dose, 1) 1 dose 2) 2 dose 3) 3 doses : \n")
        var dose = StdIn.readLine()
        if(dose == "1"){
          dosou = "1"
        }
        else if(dose == "2"){
          dosou = "2"
        }
        else if(dose == "3"){
          dosou = "3"
        }
      } else if (beverageChoice == "3"){
        baverageb = "Latte"
        println("\nVeuillez choisir votre taille:")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        val sizeChoice = StdIn.readLine()
        if (sizeChoice == "1") {
          sizou = "Petit"
        } else if (sizeChoice == "2") {
           sizou = "Moyen"
        } else if (sizeChoice == "3") {
          sizou = "Grand"
        }
        println("voulez vous du extra lait ? 0) 0 dose, 1) 1 dose 2) 2 dose 3) 3 doses : \n")
        var dose = StdIn.readLine()
        if(dose == "1"){
          dosou = "1"
        }
        else if(dose == "2"){
          dosou = "2"
        }
        else if(dose == "3"){
          dosou = "3"
        }
      }

    val (requiredCoffee, requiredMilk, baseRequiredSugar) = getIngredientRequirements(baverageb, sizou, dosou)



    if (!machine.removeIngredient("Cafe", requiredCoffee)) {
      println("Erreur: Quantité de poudre de cafe insuffisante.")
      return false
    }
    if (!machine.removeIngredient("Lait", requiredMilk)) {
      machine.addIngredient("cafe", requiredCoffee)
      println("Erreur: Quantité de lait insuffisante.")
      return false
    }


    println("\nSouhaitez-vous ajouter du sucre?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")


    val sugarAmount = {
      val input = StdIn.readLine()
      if (input == "1") {
        0
      } else if (input == "2") {
        5
      } else if (input == "3") {
        10
      } else if (input == "4") {
        15
      } else {
        return false
      }
    }


    if (!machine.removeIngredient("Sucre", sugarAmount)) {
      // Restore other ingredients
      machine.addIngredient("Cafe", requiredCoffee)
      machine.addIngredient("Lait", requiredMilk)
      println("Erreur: Quantité de sucre insuffisante.")
      return false
    }


    val basePrice = getBasePrice(baverageb, sizou)
    val sugarPrice = {
      if (sugarAmount == 0) {
        0.00
      } else if (sugarAmount == 5) {
        0.10
      } else if (sugarAmount == 10) {
        0.20
      } else if (sugarAmount == 15) {
        0.30
      } else {
        0.00
      }
    }


    val totalPrice = basePrice + sugarPrice


    println(s"\nboisson sélectionnée: $baverageb ${if(sizou != "Standard") s"($sizou)" else ""}")
    val tempo = dosou.toDouble * 0.05
    println(f"Prix total: $basePrice + $sugarPrice + $tempo%.2f =  $totalPrice%.2f CHF")


    // Process payment
    val code = Random.alphanumeric.take(5).mkString
    println(s"\nVeuillez payer avec TWINT.")
    println(s"Votre code de paiemetn est: $code")
    println("(En attente de confiramtion...)")
    Thread.sleep(2000)
    println("Paiement reçu!")


    println(s"\nPreparation de votre $baverageb ${if(sizou != "Standard") s"($sizou)" else ""}...")
    Thread.sleep(2000)
    println("Votre boisson est prête! Bonne dégustation!\n")


    true
  }


  def restockMachine(machine: Machine): Unit = {
    println("Niveau de stock actuel")
    println(f"Lait: ${machine.milk / 1000.0}%.2fL")
    println(f"Sucre: ${machine.sugar}%.1fg")
    println(f"Poudre de cafe: ${machine.coffee}%.1fg")


    println("Entrer les quantités à ajouter:")
    print("Poudre de cafe (g) > ")
    val coffeeAdd = StdIn.readInt()
    print("Sucre (g) > ")
    val sugarAdd = StdIn.readInt()
    print("Lait (ml) > ")
    val milkAdd = StdIn.readInt()


    machine.addIngredient("Cafe", coffeeAdd)
    machine.addIngredient("Sucre", sugarAdd)
    machine.addIngredient("Lait", milkAdd/1000)


    println("Stocks mis à jour avec succès.")
  }


  def main(args: Array[String]): Unit = {
    try {
      println("Chargement des machines depuis machines.csv...")
      machines = loadcsv("src/machines.csv")
      println(s"${machines.length} Machine chargé avec succès.")


      while (true) {

        println("\nVeuillez choisir votre mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")


        val modeInput = StdIn.readLine()

        if (modeInput == "3") {
          println("Sauvegarde des machines dans machines.csv...")
          savecsv("src/machines.csv", machines)
          System.exit(1)
        } else if (modeInput == "1") {
          var machineChoisi = StdIn.readLine("Choisissez une machine ").toInt
          for (machine <- machines){
            if(machine.id == machineChoisi){
              serveClient(machine)
            }
          }
        } else if (modeInput == "2") {
          var machineChoisi = StdIn.readLine("Choisissez une machine ").toInt
          for (machine <- machines){
            if(machine.id == machineChoisi){
              if (validatePin(machine)) {
                println("\nAdmin Options:")
                println("1. Restock")
                println("2. Mise à jour du PIN")
                println("3. Retour au menu")


                val adminOption = StdIn.readLine()
                if (adminOption == "1") {
                  restockMachine(machine)
                } else if (adminOption == "2") {
                  updatePin(machine)
                } else if (adminOption == "3") {
                } else {
                  println("Option invalide. Retour au menu.")
                }
              }
            }
          }

        } else {
          println("Mode invalide. Veuillez réessayer.")
        }
      }

    } catch {
      case e: Exception =>
        println("Erreur: " + e.getMessage)
        println("Fermeture du programme.")
        System.exit(1)
    }
  }
}