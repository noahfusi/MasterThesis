import scala.io.StdIn
import scala.util.Random

object ex2 {
  val nbMachines: Int = 5
  val codePin: String = "434343"

  var cafe: Array[Int] = Array.fill(nbMachines)(50)
  var sucre: Array[Int] = Array.fill(nbMachines)(30)
  var lait: Array[Double] = Array.fill(nbMachines)(0.5)
  var machinePins: Array[String] = Array.fill(nbMachines)(codePin)

  def Twint(): String = Random.alphanumeric.take(5).mkString


  def ModeClient(): Unit = {
    var processus = true
    while (processus) {
      println("Veuillez sélectionner une machine de 1 à 5):")
      print("> ")
      val machineId: Int = StdIn.readLine().toInt - 1

      if (machineId < 0 || machineId >= nbMachines) {
        println("La machine sélectionnée n'existe pas.")
        processus = false
      } else {
        processus = serveClient(machineId, cafe, sucre, lait)
      }
    }
  }

  def ModeAdmin(): Unit = {
    println("Veuillez sélectionner une machine de 1 à 5):")
    print("> ")
    val machineId: Int = StdIn.readLine().toInt - 1

    if (machineId < 0 || machineId >= nbMachines) {
      println("La machine sélectionnée n'existe pas.")
    } else {
      if (validatePin(machineId, machinePins)) {
        println("Accès autorisé à la machine " + (machineId + 1))
        println("Souhaitez-vous :\n1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN")
        print("> ")
        val choice: Byte = StdIn.readLine().toByte

        if (choice == 1) {
          restockMachine(machineId, cafe, sucre, lait)
        } else if (choice == 2) {
          updatePin(machineId, machinePins)
        } else {
          println("Veuillez entrer une valeur valide")
        }
      } else {
        println("Trop de tentatives échouées. Fin du programme.")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Nespresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n")
      print("> ")
      val mode: Byte = StdIn.readLine().toByte

      if (mode == 1) {
        ModeClient()
      } else if (mode == 2) {
        ModeAdmin()
      } else if (mode == 3) {
        println("À bientôt")
        return
      } else {
        println("Veuillez entrer une valeur valide")
      }
    }
  }



  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentative = 3
    while (tentative > 0) {
      println("Entrez le code PIN :")
      print("> ")
      val pin: String = StdIn.readLine()

      if (pin == machinePins(machineId)) {
        return true
      } else {
        tentative -= 1
        println("Code PIN incorrect. " + tentative + " tentatives restantes.")
      }
    }
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var validPin = false
    while (!validPin) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      print("> ")
      val newPin: String = StdIn.readLine()

      if (newPin.length == 6 && newPin.forall(c => c >= '0' && c <= '9')) {
        machinePins(machineId) = newPin
        println("Mise à jour du code PIN réussie.")
        validPin = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer.")
      }
    }
  }

  def Expresso(machineId: Int, coffeeStocks: Array[Int]): Unit = {
    if (coffeeStocks(machineId) >= 8) {
      coffeeStocks(machineId) -= 8
      println("Prix à payer: 2 CHF")
      Payment()
    } else {
      println("Produit momentanément indisponible.")
    }
  }

  def Cappuccino(machineId: Int, coffeeStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 0.1) {
      coffeeStocks(machineId) -= 6
      milkStocks(machineId) -= 0.1
      println("Prix à payer: 2.50 CHF")
      Payment()
    } else {
      println("Produit momentanément indisponible.")
    }
  }

  def Latte(machineId: Int, coffeeStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("Souhaitez-vous un café ? :\n1) Petit\n2) Moyen\n3) Grand\n4) Retour")
    print("> ")
    val taille: Byte = StdIn.readLine().toByte

    if (taille == 1) {
      Taille (machineId, coffeeStocks, milkStocks, 6, 0.12, 2.70)
    } else if (taille == 2) {
      Taille(machineId, coffeeStocks, milkStocks, 8, 0.15, 3.20)
    } else if (taille == 3) {
      Taille(machineId, coffeeStocks, milkStocks, 12, 0.20, 3.70)
    } else {
      println("Veuillez entrer une valeur valide")
    }
  }

  def Taille(machineId: Int, coffeeStocks: Array[Int], milkStocks: Array[Double], quantiteeC: Int, quantiteeL: Double, price: Double): Unit = {
    if (coffeeStocks(machineId) >= quantiteeC && milkStocks(machineId) >= quantiteeL) {
      coffeeStocks(machineId) -= quantiteeC
      milkStocks(machineId) -= quantiteeL
      println("Prix à payer: " + price + " CHF")
      Payment()
    } else {
      println("Produit momentanément indisponible.")
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
    println("Veuillez sélectionner votre produit :\n1) Expresso\n2) Cappuccino\n3) Latte\n4) Retour")
    print("> ")
    val produit: Byte = StdIn.readLine().toByte

    if (produit == 1) {
      Expresso(machineId, coffeeStocks)
    } else if (produit == 2) {
      Cappuccino(machineId, coffeeStocks, milkStocks)
    } else if (produit == 3) {
      Latte(machineId, coffeeStocks, milkStocks)
    } else if (produit == 4) {
      return false
    } else {
      println("Veuillez entrer une valeur valide")
      return true
    }
    true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("Entrez les quantités à ajouter :")

    println("Poudre de café :")
    print("> ")
    val CafeSup: Int = StdIn.readLine().toInt
    coffeeStocks(machineId) += CafeSup

    println("Sucre :")
    print("> ")
    val SucreSup: Int = StdIn.readLine().toInt
    sugarStocks(machineId) += SucreSup

    println("Lait :")
    print("> ")
    val LaitSup: Double = StdIn.readLine().toDouble
    milkStocks(machineId) += LaitSup

    println("Les stocks ont été mis à jour avec succès.")
  }

  def Payment(): Unit = {
    val codeTwint = Twint()
    println("Code Twint généré : " + codeTwint)
    println("Paiement en cours...")
    Thread.sleep(3000)
    println("Paiement validé. Merci !")
  }
}