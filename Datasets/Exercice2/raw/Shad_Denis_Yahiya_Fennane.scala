import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Main {

  val nbMachines = 5

  val stock_coffee = Array(50, 50, 50, 50, 50)

  val stock_sugar = Array(30, 30, 30, 30, 30)

  val stock_milk = Array(500, 500, 500, 500, 500)

  val machinePins = Array.fill(nbMachines)("434343")


  def main(args: Array[String]): Unit = {
    var mode = 0
    var repeat = false
    while(!repeat) {
      println("Choisir une machine (1-5):")

      var machineId = readLine().toInt
      if (machineId >= 1 && machineId < nbMachines + 1) {
        machineId -= 1
        println("1) Client\n2) Admin\n3) Sortie")
        mode = readLine().toInt
        if (mode == 1) {
          serveClient(machineId, stock_coffee, stock_sugar, stock_milk)
        }
        if (mode == 2) {
          if (validatePin(machineId, machinePins)) {
            println("1) Stockage de la machine\n2) Mise a jour du code PIN")
            val adminChoice = readLine().toInt
            if (adminChoice == 1) {
              restockMachine(machineId, stock_coffee, stock_sugar, stock_milk)
            }
            else if (adminChoice == 2) {
              updatePin(machineId, machinePins)
            }
            else {
              println("Option invalide. Retour au menu principal")
            }
          }
        }
        if (mode == 3) {
          println("Merci a bientot!")
          repeat = true
        }
      }
    }
  }


  def serveClient(machineId: Int, stock_coffee: Array[Int], stock_sugar: Array[Int], stock_milk: Array[Int]): Boolean = {

    val prixExpresso    = 2.0
    val prixCapuccino   = 2.5
    val prixPetitLatte  = 2.7
    val prixMoyenLatte  = 3.2
    val prixGrandLatte  = 3.7


    val supplementPetitLait  = 0.05
    val supplementMoyenLait  = 0.10
    val supplementGrandLait  = 0.15


    val supplementPetitSucre = 0.10
    val supplementMoyenSucre = 0.20
    val supplementGrandSucre = 0.30


    val usageCafeEspresso    = 8
    val usageMilkEspresso    = 0


    val usageCafeCappuccino  = 8
    val usageMilkCappuccino  = 100


    val usageCafeLattePetit  = 6
    val usageCafeLatteMoyen  = 8
    val usageCafeLatteGrand  = 12

    val usageMilkLattePetit  = 120
    val usageMilkLatteMoyen  = 150
    val usageMilkLatteGrand  = 200


    val plusPetitLait = 50
    val plusMoyenLait = 100
    val plusGrandLait = 150


    val usageSugarSmall  = 5
    val usageSugarMedium = 10
    val usageSugarLarge  = 15


    var totalPrice      = 0.0
    var coffeeUsed      = 0
    var milkUsed        = 0
    var extraMilkUsed   = 0
    var sugarUsed       = 0

    var priceExtraMilk  = 0.0
    var priceExtraSugar = 0.0


    val alphaNumeric  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val code_twint    = (1 to 5).map(_ => alphaNumeric((math.random * alphaNumeric.length).toInt)).mkString


    println("Quelle boisson voulez-vous? \n1) Espresso \n2) Cappuccino \n3) Latte")
    val boisson_choisi = readLine().toInt


    if (boisson_choisi == 1) {
      totalPrice = prixExpresso
      coffeeUsed = usageCafeEspresso
      milkUsed   = usageMilkEspresso
    }


    if (boisson_choisi == 2) {
      totalPrice = prixCapuccino
      coffeeUsed = usageCafeCappuccino
      milkUsed   = usageMilkCappuccino


      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n(1) Oui\n(2) Non")
      val extraMilkChoice = readInt()
      if (extraMilkChoice == 1) {
        println("Combien de dose voulez-vous ? (1 = petit, 2 = moyen, 3 = grand)")
        val dose = readInt()
        if (dose == 1) {
          extraMilkUsed = plusPetitLait
          priceExtraMilk = supplementPetitLait
        } else if (dose == 2) {
          extraMilkUsed = plusMoyenLait
          priceExtraMilk = supplementMoyenLait
        } else if (dose == 3) {
          extraMilkUsed = plusGrandLait
          priceExtraMilk = supplementGrandLait
        } else {
          extraMilkUsed = 0
          priceExtraMilk = 0.0
        }
      }
    }


    if (boisson_choisi == 3) {
      println("De quelle taille souhaitez-vous votre latte?\n" +
        "(1) Petit\n(2) Moyen\n(3) Grand")
      val tailleLatte = readInt()

      if (tailleLatte == 1) {
        totalPrice = prixPetitLatte
        coffeeUsed = usageCafeLattePetit
        milkUsed   = usageMilkLattePetit
      } else if (tailleLatte == 2) {
        totalPrice = prixMoyenLatte
        coffeeUsed = usageCafeLatteMoyen
        milkUsed   = usageMilkLatteMoyen
      } else if (tailleLatte == 3) {
        totalPrice = prixGrandLatte
        coffeeUsed = usageCafeLatteGrand
        milkUsed   = usageMilkLatteGrand
      }


      println("Voulez-vous ajouter encore plus de lait?\n" +
        "(0) Non\n(1) Petit supplément\n(2) Moyen supplément\n(3) Grand supplément")
      val plusLaitChoice = readLine().toInt
      if (plusLaitChoice == 1) {
        extraMilkUsed = plusPetitLait
        priceExtraMilk = supplementPetitLait
      } else if (plusLaitChoice == 2) {
        extraMilkUsed = plusMoyenLait
        priceExtraMilk = supplementMoyenLait
      } else if (plusLaitChoice == 3) {
        extraMilkUsed = plusGrandLait
        priceExtraMilk = supplementGrandLait
      } else {
        extraMilkUsed = 0
        priceExtraMilk = 0.0
      }
    }


    println("Voulez-vous ajouter du sucre?\n" +
      "(0) Non\n(1) Petit sucre\n(2) Moyen sucre\n(3) Grand sucre")
    val sucreChoice = readInt()
    if (sucreChoice == 1) {
      sugarUsed = usageSugarSmall
      priceExtraSugar = supplementPetitSucre
    } else if (sucreChoice == 2) {
      sugarUsed = usageSugarMedium
      priceExtraSugar = supplementMoyenSucre
    } else if (sucreChoice == 3) {
      sugarUsed = usageSugarLarge
      priceExtraSugar = supplementGrandSucre
    } else {
      sugarUsed = 0
      priceExtraSugar = 0.0
    }


    totalPrice += priceExtraMilk + priceExtraSugar


    val totalMilkNeeded = milkUsed + extraMilkUsed
    if (
      stock_coffee(machineId) >= coffeeUsed &&
        stock_sugar(machineId) >= sugarUsed &&
        stock_milk(machineId) >= totalMilkNeeded
    ) {
      println("Le prix total à payer est : " + totalPrice + " CHF")
      println("Paiement par Twint\nVotre code pour le paiement est : " + code_twint)
      Thread.sleep(1000)
      Thread.sleep(1000)
      println("Le paiement a bien été accepté.\nPréparation de votre boisson...\n[...]\nVotre Café est prêt ! Bonne dégustation !")


      stock_coffee(machineId) -= coffeeUsed
      stock_milk(machineId)   -= totalMilkNeeded
      stock_sugar(machineId)  -= sugarUsed
      return true
    } else {
      println("Désolé, la machine n'a pas assez de stock pour cette commande.")
      return false
    }
  }


  def validatePin(machineId: Int, machinePins: Array[String]): Boolean =  {
    println("Entrez un PIN pour la machine: " + (machineId+1) + ":")
    var attempts = 3
    while (attempts > 0) {
      val inputPin = readLine()
      if (inputPin == machinePins(machineId)) {
        println("Code PIN bon.")
        return true
      }
      else {
        attempts -= 1
        println("PIN incorrect. " + attempts + " essaies restants")
      }
    }
    println("Vous avez depasser le nombre d'essaies maximum")
    false
  }


  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise a jour du code PIN de la machine: " + (machineId + 1))

    def isValidPin(pin: String): Boolean = {
      if (pin.length != 6) return false
      for (char <- pin) {
        if (!char.isDigit) return false
      }
      true
    }

    var codeValide = false
    while (!codeValide) {
      println("Entrez un code PIN de 6 chiffres")
      val pinAsString = readLine()

      if (isValidPin(pinAsString)) {
        machinePins(machineId) = pinAsString
        println("Code PIN mis a jour avec succes.")
        codeValide = true
      } else {
        println("Code PIN invalide. Assurez-vous qu'il contient exactement 6 chiffres.")
      }
    }
  }



  def restockMachine(machineId: Int, stock_coffee: Array[Int], stock_sugar: Array[Int], stock_milk: Array[Int]): Unit = {

    println(" voici les stock de cafe : " + stock_coffee(machineId))
    println(" voici les stock de sucre : " + stock_sugar(machineId))
    println(" voici les stock de lait : " + (stock_milk(machineId)/1000.0))

    println("combien de g de sucre souhaite tu ajouter au stock")
    stock_sugar(machineId) += readInt()

    println("combien de g de cafe souhaite tu ajouter au stock")
    stock_coffee(machineId) += readInt()

    println("combien de lait en L souhaite tu ajouter au stock")
    val lait_ajoute = readLine().toDouble
    stock_milk(machineId) = (lait_ajoute * 1000.0).toInt +stock_milk(machineId)

  }

}