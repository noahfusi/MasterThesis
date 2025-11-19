import scala.io.StdIn.{readInt, readLine}
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}
import scala.io.Source






class Machine(val id: Int, var pincode: String, var milk: Double, var sugar: Double, var coffee: Double) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "lait" =>
        milk += amount
        println(s"$amount de lait ajouté, nouveau stock de lait : $milk mL")
      case "sucre" =>
        sugar += amount
        println(s"$amount de sucre ajouté, nouveau stock de sucre: $sugar g")
      case "cafe" =>
        coffee += amount
        println(s"$amount de cafe ajouté, nouveau stock de cafe: $coffee g")
      case _ =>
        println(s"erreur sur l'ingrediant : $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "lait" if milk >= amount =>
        milk -= amount
        println(s"$amount de lait utiliser, il reste : $milk mL")
        true
      case "sucre" if sugar >= amount =>
        sugar -= amount
        println(s"$amount de sucre utiliser, il reste : $sugar g")
        true
      case "cafe" if coffee >= amount =>
        coffee -= amount
        println(s"$amount de cafe utiliser, il reste : $coffee g")
        true
      case _ =>
        println(s"plus assez de $ingredient ou ingrédient ingalide.")
        false
    }
  }
}

object Nospresso {



  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________

  def validatePIN(Nummachine: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var PIN2 = ""
    var tryPIN = 0

    while (!(PIN2 == machines(Nummachine).pincode || tryPIN == 3)) {
      tryPIN += 1
      println("essaye NB: " + tryPIN)
      println("PIN  s'il vous plais :")
      print("> ")
      PIN2 = readLine()
      if (!(PIN2 == machines(Nummachine).pincode)) println("correct")

      if (PIN2 == machines(Nummachine).pincode) {
        println("Admin confirmé")
        return true
      }
      if (tryPIN == 3) {
        println("plsu d'essaye, redirection vers menu principale")
        return false
      }
    }
    false
  }

  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________


  def updatePIN(Nummachine: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("entrer nouveau PIN a 6 chiffre:")
    var validPin = false
    while (!validPin) {
      val newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machines(Nummachine).pincode = newPin
        println("PIN entrer sauvegardé.")
        validPin = true
      } else {
        println("recommencer, le code PIN doit corresponde a 6 chiffres")
      }
    }
  }

  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________



  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println(s"machine a restoquer ${machineId + 1}:")
    println(s"stock actuelle: lait- ${machines(machineId).milk} mL, sucre - ${machines(machineId).sugar} g, Caffe - ${machines(machineId).coffee} g")

    var validInput = false

    while (!validInput) {
      println("quantité de lait a stocker (mL):")
      val input = readLine()

      if (input.forall(_.isDigit)) {
        machines(machineId).addIngredient("lait", input.toInt)
        validInput = true
      } else {
        println("erreur rentrer un nombre valide.")
      }
    }

    validInput = false
    while (!validInput) {
      println("quantité de sucre a stocker (g):")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        machines(machineId).addIngredient("sucre", input.toInt)
        validInput = true
      } else {
        println("erreur rentrer un nombre valide.")
      }
    }

    validInput = false
    while (!validInput) {
      println("quantité de cafe a stocker (g):")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        machines(machineId).addIngredient("cafe", input.toInt)
        validInput = true
      } else {
        println("erreur rentrer un nombre valide.")
      }
    }

    println(s"stockage de la machine ${machineId + 1}. terminer")
  }


  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________




  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var validInput = false
    var choice = 0

    while (!validInput) {
      println("boisson:")
      println("1 Espresso 2.00 CHF")
      println("2 Cappuccino 2.50 CHF")
      println("3 Latte 2.70 CHF (petit), 3.20 CHF (Moyen), 3.70 CHF (grand)")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        choice = input.toInt
        if (choice >= 1 && choice <= 3) {
          validInput = true
        } else {
          println("erreur choississez (1, 2, or 3).")
        }
      } else {
        println("erreur rentrer un nombre valide")
      }
    }

    choice match {
      case 1 =>
        boissonprep(machineId, machines, "Espresso", 8, 0, 2.00)
      case 2 =>
        boissonprep(machineId, machines, "Cappuccino", 6, 100, 2.50)
      case 3 =>
        var size = 0
        validInput = false

        while (!validInput) {
          println("choissiser la taille :")
          println("1. petit - 2.70 CHF")
          println("2. Moyen - 3.20 CHF")
          println("3. grand - 3.70 CHF")
          val sizeInput = readLine()

          if (sizeInput.forall(_.isDigit)) {
            size = sizeInput.toInt
            if (size >= 1 && size <= 3) {
              validInput = true
            } else {
              println("erreur choississez 1, 2, or 3.")
            }
          } else {
            println("erreur rentrer un nombre valide")
          }
        }

        val (coffee, milk, price) = size match {
          case 1 => (6, 120, 2.70)
          case 2 => (8, 150, 3.20)
          case 3 => (12, 200, 3.70)
        }
        boissonprep(machineId, machines, s"latte (taille $size)", coffee, milk, price)
    }
  }


  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________



  def boissonprep(machineId: Int, machines: ArrayBuffer[Machine], name: String, coffee: Int, milk: Int, price: Double): Unit = {
    var validInput = false
    var sucreChoice = 0



    while (!validInput) {
      println("rajouté du sucre ?")
      println("1. non")
      println("2. un peut (5g) - 0.10 CHF")
      println("3. moyen(10g) - 0.20 CHF")
      println("4. beaucoup (15g) - 0.30 CHF")
      val input = readLine()
      if (input.forall(_.isDigit)) {
        sucreChoice = input.toInt
        if (sucreChoice >= 1 && sucreChoice <= 4) {
          validInput = true
        } else {
          println("erreur choississez 1, 2, 3, or 4.")
        }
      } else {
        println("erreur rentrer un nombre valide")
      }
    }


    val (sugar, sugarPrice) = sucreChoice match {
      case 1 => (0, 0.0)
      case 2 => (5, 0.10)
      case 3 => (10, 0.20)
      case 4 => (15, 0.30)
    }


    val totalPrice = price + sugarPrice

    if (machines(machineId).coffee >= coffee && machines(machineId).milk >= milk && machines(machineId).sugar >= sugar) {
      machines(machineId).removeIngredient("cafe", coffee)
      machines(machineId).removeIngredient("lait", milk)
      machines(machineId).removeIngredient("sucre", sugar)


      println(f"TOTAL: ${totalPrice}%.2f CHF")
      println("Veuillez payer en utilisant Twint")

      var code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

      print("Votre code de paiement est : ")
      for (x <- 1 to 5) {
        print((code((math.random() * 61).toInt)))
      }
      println(" ")
      println("(En attente de validation du paiement...)")
      Thread.sleep(1000) // Attend pendant 3000 millisecondes (3 secondes)
      println("Merci ! Votre paiement a été́ accepte ́.")


      println(s"preparation de :  $name...")
      Thread.sleep(2000)

      println(f"votre $name est prets. TOTAL payer: ${totalPrice}%.2f CHF.")
    } else {
      println("erreur stock inssufisant, choisire autre machine ou restocker.")
    }
  }


  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________






  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)

    println("Machines charger et prets:")
    machines.foreach { machine =>
      println(s"Machine ID: ${machine.id}, PIN: ${machine.pincode}, lait: ${machine.milk} mL, sucre: ${machine.sugar} g, cafe: ${machine.coffee} g")
    }

    var end = false

    while (!end) {
      println("bonjour, machine Nospresso!")
      println("choisire votre mode")
      println("1. Client")
      println("2. Admin")
      println("3. Exit")

      var validInput = false
      var mode = 0

      while (!validInput) {
        val input = readLine()
        if (input.forall(_.isDigit)) {
          mode = input.toInt
          if (mode >= 1 && mode <= 3) {
            validInput = true
          } else {
            println("erreur veuillez choisire 1, 2, or 3.")
          }
        } else {
          println("erreur veuillez choisire un valeur numerique.")
        }
      }

      mode match {
        case 1 =>
          println("sélectionner la machine :")
          validInput = false
          var machineId = -1

          while (!validInput) {
            val input = readLine()
            if (input.forall(_.isDigit)) {
              machineId = input.toInt - 1
              if (machineId >= 0 && machineId < machines.length) {
                validInput = true
              } else {
                println("erreur veuillez choisire un ID valide.")
              }
            } else {
              println("erreur veuillez choisire un valeur numerique")
            }
          }
          serveClient(machineId, machines)
        case 2 =>
          println("sélectionner la machine :")
          validInput = false
          var machineId = -1

          while (!validInput) {
            val input = readLine()
            if (input.forall(_.isDigit)) {
              machineId = input.toInt - 1
              if (machineId >= 0 && machineId < machines.length) {
                validInput = true
              } else {
                println("erreur veuillez choisire un ID valide.")
              }
            } else {
              println("erreur veuillez choisire un valeur numerique")
            }
          }
          if (validatePIN(machineId, machines)) {
            var adminChoice = 0
            validInput = false

            while (!validInput) {
              println("1. Restock Ingredients")
              println("2. modifier PIN")
              val input = readLine()
              if (input.forall(_.isDigit)) {
                adminChoice = input.toInt
                if (adminChoice >= 1 && adminChoice <= 2) {
                  validInput = true
                } else {
                  println("erreur veuillez choisire 1 or 2.")
                }
              } else {
                println("erreur veuillez choisire un valeur numerique.")
              }
            }

            adminChoice match {
              case 1 => restockMachine(machineId, machines)
              case 2 => updatePIN(machineId, machines)
            }
          }
        case 3 =>
          savecsv(filename, machines)
          println("bonne journée!")
          end = true
      }
    }
  }



  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________



  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      println(s"chargement des machine $filename...")
      val lines = Source.fromFile(filename).getLines()
      lines.next() // Skip the header line
      for ((line, index) <- lines.zipWithIndex) {
        val Array(pin, milk, sugar, coffee) = line.split(",")
        val machine = new Machine(index + 1, pin.trim, milk.trim.toDouble, sugar.trim.toDouble, coffee.trim.toDouble)
        machines += machine
      }
      println(s"chargement réussis de ${machines.size} machines.")
    } catch {
      case _: Exception =>
        println("erreur chemine de la machine non trouver.")
        System.exit(1)
    }
    machines
  }

  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________
  //__________________________________________________________________________________________________




  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val file = new PrintWriter(new File(filename))
      file.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        file.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      file.close()
      println(s"sauvegarder réussis ${machines.size} machine(s) a $filename")
    } catch {
      case _: Exception =>
        println("erreur, machine non sauvegarder.")
    }
  }
}