import scala.io.StdIn._
import scala.util.Random

object Nospresso {

  def readValidatedInt(validChoices: Array[Int]): Int = {
    var choix = -1
    do {
      println("Veuillez entrer un choix parmi : " + validChoices.mkString(", "))
      choix = readLine().toInt
      if (!validChoices.contains(choix)) {
        println("Choix invalide. Veuillez réessayer.")
      }
    } while (!validChoices.contains(choix))
    choix
  }

  def displayStocks(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels pour la machine : " + (machineId + 1))
    println("Poudre de café : " + coffeeStocks(machineId) + "g")
    println("Sucre : " + sugarStocks(machineId) + "g")
    printf("Lait : %.1f L\n", milkStocks(machineId) / 1000.0)
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    def readPositiveInt(message: String): Int = {
      var valeur = -1
      do {
        print(message)
        valeur = readLine().toInt
        if (valeur < 0) println("Veuillez entrer une valeur positive.")
      } while (valeur < 0)
      valeur
    }

    coffeeStocks(machineId) += readPositiveInt("Quantité de café à ajouter (g) : ")
    sugarStocks(machineId) += readPositiveInt("Quantité de sucre à ajouter (g) : ")
    milkStocks(machineId) += readPositiveInt("Quantité de lait à ajouter (ml) : ")

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def validatePin(machineId: Int, machinePins: Array[String], coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN :")
      val pin = readLine()
      if (pin == machinePins(machineId)) {
        println("Code PIN correct. Accès accordé.")
        displayStocks(machineId, coffeeStocks, sugarStocks, milkStocks)
        return true
      } else {
        tentatives -= 1
        println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Vous retournez au menu principal.")
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId + 1))
    var newPin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      newPin = readLine()
      if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        println("Code PIN invalide. Veuillez entrer exactement 6 chiffres.")
      }
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var boissonPreparee = false
    while (!boissonPreparee) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

      val choixBoisson = readValidatedInt(Array(1, 2, 3))
      var prixFinal = 0.0
      var quantitePoudreCafe = 0
      var quantiteLait = 0
      var nomBoisson = ""

      if (choixBoisson == 1) {
        prixFinal = 2.00
        quantitePoudreCafe = 8
        quantiteLait = 0
        nomBoisson = "Expresso"
      } else if (choixBoisson == 2) {
        prixFinal = 2.50
        quantitePoudreCafe = 6
        quantiteLait = 100
        nomBoisson = "Cappuccino"
      } else if (choixBoisson == 3) {
        println("Sélectionnez la taille du Latte :\n1) Petit\n2) Moyen\n3) Grand")
        val tailleLatte = readValidatedInt(Array(1, 2, 3))
        if (tailleLatte == 1) {
          prixFinal = 2.70
          quantitePoudreCafe = 6
          quantiteLait = 120
          nomBoisson = "Latte (Petit)"
        } else if (tailleLatte == 2) {
          prixFinal = 3.20
          quantitePoudreCafe = 8
          quantiteLait = 150
          nomBoisson = "Latte (Moyen)"
        } else if (tailleLatte == 3) {
          prixFinal = 3.70
          quantitePoudreCafe = 12
          quantiteLait = 200
          nomBoisson = "Latte (Grand)"
        }
      }

      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      val choixSucre = readValidatedInt(Array(1, 2, 3, 4))
      var quantiteSucre = 0
      var prixSucre = 0.0

      if (choixSucre == 1) {
        quantiteSucre = 0
        prixSucre = 0.0
      } else if (choixSucre == 2) {
        quantiteSucre = 5
        prixSucre = 0.10
      } else if (choixSucre == 3) {
        quantiteSucre = 10
        prixSucre = 0.20
      } else if (choixSucre == 4) {
        quantiteSucre = 15
        prixSucre = 0.30
      }

      prixFinal += prixSucre

      println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
      val choixLait = readValidatedInt(Array(1, 2))
      var prixLaitSup = 0.0

      if (choixLait == 1) {
        println("Combien de doses de lait supplémentaire ? (1 à 3 doses, 0.05L par dose à CHF 0.05 chacune)")
        val doses = readValidatedInt(Array(1, 2, 3))
        quantiteLait += (doses * 50)
        prixLaitSup = doses * 0.05
      }

      prixFinal += prixLaitSup

      if (coffeeStocks(machineId) < quantitePoudreCafe || sugarStocks(machineId) < quantiteSucre || milkStocks(machineId) < quantiteLait) {
        println("Stock insuffisant pour préparer votre boisson. Veuillez choisir une autre machine.")
        return false
      } else {
        coffeeStocks(machineId) -= quantitePoudreCafe
        sugarStocks(machineId) -= quantiteSucre
        milkStocks(machineId) -= quantiteLait

        val codePaiement = Random.alphanumeric.take(5).mkString
        printf("Votre boisson est prête : %s\nPrix total : CHF %.2f\nCode de paiement : %s\n", nomBoisson, prixFinal, codePaiement)
        boissonPreparee = true
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var coffeeStocks = Array.fill(nbMachines)(50)
    var sugarStocks = Array.fill(nbMachines)(30)
    var milkStocks = Array.fill(nbMachines)(500) //  attention c en millilitres
    var machinePins = Array.fill(nbMachines)("434343")
    var quitter = false

    while (!quitter) {
      println("Menu Principal:\n1) Client\n2) Admin\n3) Quitter")
      val choixPrincipal = readValidatedInt(Array(1, 2, 3))

      if (choixPrincipal == 3) {
        quitter = true
      } else if (choixPrincipal == 1 || choixPrincipal == 2) {
        println("Veuillez sélectionner une machine (1 à " + nbMachines + ") :")
        val machineId = readValidatedInt((1 to nbMachines).toArray) - 1

        if (choixPrincipal == 1) {
          if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
            println("Choisissez une autre machine.")
          }
        } else if (choixPrincipal == 2) {
          if (validatePin(machineId, machinePins, coffeeStocks, sugarStocks, milkStocks)) {
            println("Mode Admin:\n1) Réapprovisionner\n2) Modifier le code PIN")
            val choixAdmin = readValidatedInt(Array(1, 2))

            if (choixAdmin == 1) {
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            } else if (choixAdmin == 2) {
              updatePin(machineId, machinePins)
            }
          }
        }
      }
    }
    println("Merci d'avoir utilisé Nospresso!")
  }
}
