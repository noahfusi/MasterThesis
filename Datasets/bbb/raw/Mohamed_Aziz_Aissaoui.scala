import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object NospressoMultiMachine {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val initialPin = "434343"
    val machinePins = Array.fill(nbMachines)(initialPin)
    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)

    var running = true
    while (running) {
      println("Nospresso Café - Gestion Multi-Machines")
      println("1) Client")
      println("2) Administrateur")
      println("3) Quitter")
      print("> ")

      val choix = readLine()
      if (choix == "1") {
        println("Sélectionnez une machine (1 à 5) :")
        val machineId = readInt()
        if (machineId >= 1 && machineId <= nbMachines) {
          serveClient(machineId - 1, coffeeStocks, sugarStocks, milkStocks)
        } else {
          println("Machine invalide.")
        }
      } else if (choix == "2") {
        println("Mode Administrateur")
        println("Sélectionnez une machine (1 à 5) :")
        val machineId = readInt()
        if (machineId >= 1 && machineId <= nbMachines) {
          if (validatePin(machineId - 1, machinePins)) {
            adminMenu(machineId - 1, machinePins, coffeeStocks, sugarStocks, milkStocks)
          } else {
            println("Accès refusé. Trop de tentatives échouées.")
            running=false
          }
        } else {
          println("Machine invalide.")
        }
      } else if (choix == "3") {
        println("Au revoir !")
        running = false
      } else {
        println("Choix invalide.")
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      printf("Entrez le code PIN pour la machine %d :\n", machineId + 1)
      val enteredPin = readLine()
      if (enteredPin == machinePins(machineId)) {
        println("Code PIN correct. Accès accordé.")
        return true
      } else {
        attempts -= 1
        printf("Code PIN incorrect. %d tentative(s) restante(s).\n", attempts)
      }
    }
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    printf("Mise à jour du code PIN pour la machine %d.\n", machineId + 1)
    var validPin = false
    while (!validPin) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = readLine()
      if (newPin.length==6&& newPin.forall(_.isDigit)){
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.")
        validPin = true
      } else {
        println("Le code PIN doit contenir exactement 6 chiffres.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    printf("Vous utilisez la machine %d.\n", machineId + 1)
    println("Veuillez sélectionner votre boisson :\n1) Expresso\n2) Cappuccino\n3) Latte")
    val choixBoisson = readLine()
    if (choixBoisson == "1") {
      Gestionprixstocks(machineId, coffeeStocks, sugarStocks, milkStocks, 8, 0, "Expresso", 2.0, false)
    } else if (choixBoisson == "2") {
      Gestionprixstocks(machineId, coffeeStocks, sugarStocks, milkStocks, 6, 100, "Cappuccino", 2.5, true)
    } else if (choixBoisson == "3") {
      println("Quelle taille voulez-vous pour votre Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
      val tailleChoix = readLine()
      if (tailleChoix == "1") {
        Gestionprixstocks(machineId, coffeeStocks, sugarStocks, milkStocks, 6, 120, "Latte (Petit)", 2.7, true)
      } else if (tailleChoix == "2") {
        Gestionprixstocks(machineId, coffeeStocks, sugarStocks, milkStocks, 8, 150, "Latte (Moyen)", 3.2, true)
      } else if (tailleChoix == "3") {
        Gestionprixstocks(machineId, coffeeStocks, sugarStocks, milkStocks, 12, 200, "Latte (Grand)", 3.7, true)
      } else {
        println("Choix invalide.")
      }
    } else {
      println("Choix invalide.")
    }
  }
  def Gestionprixstocks(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], coffeeUsed: Int, milkUsed: Int, drink: String, basePrice: Double, allowExtraMilk: Boolean): Unit = {
    if (coffeeStocks(machineId) >= coffeeUsed && milkStocks(machineId) >= milkUsed) {
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      val sugarChoice = readLine()
      var sugarUsed = 0
      var sugarPrice = 0.0

      if (sugarChoice == "2") {
        sugarUsed = 5
        sugarPrice = 0.10
      } else if (sugarChoice == "3") {
        sugarUsed = 10
        sugarPrice = 0.20
      } else if (sugarChoice == "4") {
        sugarUsed = 15
        sugarPrice = 0.30
      }

      if (sugarStocks(machineId) < sugarUsed) {
        println("Quantité de sucre insuffisante pour préparer votre boisson.")
        return
      }

      var extraMilkUsed = 0
      var extraMilkPrice = 0.0
      if (allowExtraMilk) {
        println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
        val extraMilkChoice = readLine()
        if (extraMilkChoice == "1") {
          println("Combien de doses (1 à 3, 50ml par dose) ?")
          val doses = readInt()
          if (doses >= 1 && doses <= 3) {
            extraMilkUsed = doses * 50
            extraMilkPrice = doses * 0.05
            if (milkStocks(machineId) < milkUsed + extraMilkUsed) {
              println("Quantité de lait insuffisante pour ajouter du lait supplémentaire.")
              return
            }
          } else {
            println("Nombre de doses invalide.")
            return
          }
        }
      }

      coffeeStocks(machineId) -= coffeeUsed
      milkStocks(machineId) -= (milkUsed + extraMilkUsed)
      sugarStocks(machineId) -= sugarUsed

      val totalPrice = basePrice + sugarPrice + extraMilkPrice
      val caracteresPossibles = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      val longueurCode = 5
      val codeTwint = (1 to longueurCode).map(_ => caracteresPossibles(Random.nextInt(caracteresPossibles.length))).mkString

      printf("Votre commande : %s - CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", drink, basePrice, sugarPrice, extraMilkPrice, totalPrice)
      printf("Veuillez payer en utilisant Twint. Votre code de paiement est : %s\n", codeTwint)
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      printf("Votre %s est prêt ! Bonne dégustation !\n", drink)
    } else {
      println("Stocks insuffisants pour préparer votre boisson.")
    }
  }


  def adminMenu(machineId: Int, machinePins: Array[String], coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("1) Voir les stocks")
    println("2) Réapprovisionner les stocks")
    println("3) Mettre à jour le code PIN")
    print("> ")

    val adminChoice = readLine()
    if (adminChoice == "1") {
      printf("Stocks actuels pour la machine %d :\n", machineId + 1)
      printf("Café : %dg\n", coffeeStocks(machineId))
      printf("Sucre : %dg\n", sugarStocks(machineId))
      val milkInLiters = milkStocks(machineId).toDouble / 1000
      printf("Lait : %.2f l\n", milkInLiters)
    } else if (adminChoice == "2") {
      println("Réapprovisionnement :")
      println("Quantité de café à ajouter (g) :")
      coffeeStocks(machineId) += readInt()
      println("Quantité de sucre à ajouter (g) :")
      sugarStocks(machineId) += readInt()
      println("Quantité de lait à ajouter (ml) :")
      milkStocks(machineId) += readInt()
      println("Stocks mis à jour.")
    } else if (adminChoice == "3") {
      updatePin(machineId, machinePins)
    } else {
      println("Choix invalide.")
    }
  }

}