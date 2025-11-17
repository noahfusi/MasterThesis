import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var pinMachines = Array.fill(nbMachines)("434343")
    var stocksCafe = Array.fill(nbMachines)(50)
    var stocksLait = Array.fill(nbMachines)(500)
    var stocksSucre = Array.fill(nbMachines)(30)
    val TotalTentatives = 3
    var running = true

    while (running) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val choix = readLine()

      if (choix == "1") {
        println("Veuillez entrer le numéro de la machine (1-5) :")
        print("> ")
        val machineId = readLine().toInt

        if (machineId < 1 || machineId > nbMachines) {
          println("Numéro de machine invalide.")
        } else {
          serveClient(machineId, stocksCafe, stocksSucre, stocksLait)
        }

      } else if (choix == "2") {
        println("Veuillez entrer le numéro de la machine (1-5) :")
        print("> ")
        val machineId = readLine().toInt

        if (machineId < 1 || machineId > nbMachines) {
          println("Numéro de machine invalide.")
        } else {
          if (validatePin(machineId, pinMachines)) {
            println("Entrez un nouveau code PIN (6 chiffres) :")
            print("> ")
            val newPin = readLine()
            if (newPin.length == 6) {
              pinMachines(machineId - 1) = newPin
              println("Le code PIN a été mis à jour.")
            } else {
              println("Le code PIN doit comporter exactement 6 chiffres.")
            }
            restockMachine(machineId, stocksCafe, stocksSucre, stocksLait)
          }
        }
      } else if (choix == "3") {
        running = false
        println("...Au revoir.")
      } else {
        println("Choix invalide. Réessayez.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    val index = machineId - 1
    var prix = 0.00
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    val choixBoisson = readLine()

    if (choixBoisson == "1") {
      if (coffeeStocks(index) >= 8) {
        coffeeStocks(index) -= 8
        prix += 2.00
        println("Veuillez payer via Twint .")
        Thread.sleep(5000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre Expresso est prêt. Bonne dégustation !")
        true
      } else {
        println("Stock de café insuffisant. Veuillez choisir une autre boisson ou vérifier les stocks.")
        false
      }
    } else if (choixBoisson == "2") {
      if (coffeeStocks(index) >= 6 && milkStocks(index) >= 100) {
        coffeeStocks(index) -= 6
        milkStocks(index) -= 100
        prix += 2.50
        println("Veuillez payer via Twint .")
        Thread.sleep(5000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre Cappuccino est prêt. Bonne dégustation !")
        true
      } else {
        println("Ingrédients insuffisants. Veuillez choisir une autre boisson ou vérifier les stocks.")
        false
      }
    } else if (choixBoisson == "3") {
      println("Sélectionnez la taille de votre Latte :")
      println("1) Petit (CHF 2.70)")
      println("2) Moyen (CHF 3.20)")
      println("3) Grand (CHF 3.70)")
      print("> ")
      val tailleLatte = readLine()

      if (tailleLatte == "1" && coffeeStocks(index) >= 6 && milkStocks(index) >= 120) {
        coffeeStocks(index) -= 6
        milkStocks(index) -= 120
        prix += 2.70
        println("Latte Petit sélectionné. CHF 2.70")
      } else if (tailleLatte == "2" && coffeeStocks(index) >= 8 && milkStocks(index) >= 150) {
        coffeeStocks(index) -= 8
        milkStocks(index) -= 150
        prix += 3.20
        println("Latte Moyen sélectionné. CHF 3.20")
      } else if (tailleLatte == "3" && coffeeStocks(index) >= 12 && milkStocks(index) >= 200) {
        coffeeStocks(index) -= 12
        milkStocks(index) -= 200
        prix += 3.70
        println("Latte Grand sélectionné. CHF 3.70")
      } else {
        println("Ingrédients insuffisants pour préparer la boisson choisie.")
        false
      }
      if (choixBoisson == "2" || choixBoisson == "3") {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print("> ")
        val ajoutLait = readLine()
        if (ajoutLait == "1") {
          println("Combien de doses ?")
          print("> ")
          val doseLait = readLine()
          if (doseLait == "1" && milkStocks(index) >= 50) {
            milkStocks(index) -= 50
            prix += 0.10
          } else if (doseLait == "2" && milkStocks(index) >= 100) {
            milkStocks(index) -= 100
            prix += 0.20
          } else if (doseLait == "3" && milkStocks(index) >= 150) {
            milkStocks(index) -= 150
            prix += 0.30
          } else {
            println("Stock de lait insuffisant ou choix invalide.")
            return false
          }
        }
      }
      var sugar = 0
      if (sugarStocks(index) >= 5) {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        sugar = readLine().toInt

        if (sugar == 2 && sugarStocks(index) >= 5) {
          sugarStocks(index) -= 5
          prix += 0.10
        } else if (sugar == 3 && sugarStocks(index) >= 10) {
          sugarStocks(index) -= 10
          prix += 0.20
        } else if (sugar == 4 && sugarStocks(index) >= 15) {
          sugarStocks(index) -= 15
          prix += 0.30
        } else if (sugar != 1) {
          println("Stock de sucre insuffisant ou choix invalide.")
        }
      } else {
        println("Pas assez de sucre")
        return false
      }
      val prixSucre = if (sugar == 2) 0.10 else if (sugar == 3) 0.20 else if (sugar == 4) 0.30 else 0.00
      val prixBoisson = prix - prixSucre
      println(f"Prix total : CHF $prixBoisson%.2f + CHF $prixSucre%.2f = CHF ${prixBoisson + prixSucre}%.2f")
      println("Veuillez payer via Twint .")
      Thread.sleep(5000)
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      println("Votre Latte est prêt. Bonne dégustation !")
      true
    } else {
      println("Choix invalide. Réessayez.")
      false
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    val index = machineId - 1
    println(s"\nNiveaux de stock actuels pour la machine $machineId :")
    println(s"Poudre de café : ${coffeeStocks(index)}g")
    println(s"Sucre : ${sugarStocks(index)}g")
    println(f"Lait : ${milkStocks(index) / 1000.0}%.1fL\n")

    println("Entrez les quantités à ajouter :")
    print("Poudre de café > ")
    val ajoutCafe = readLine().toInt
    print("Sucre > ")
    val ajoutSucre = readLine().toInt
    print("Lait > ")
    val ajoutLait = (readLine().toDouble * 1000).toInt

    coffeeStocks(index) += ajoutCafe
    sugarStocks(index) += ajoutSucre
    milkStocks(index) += ajoutLait

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val index = machineId - 1
    var tentative = 0
    while (tentative < 3) {
      println(s"Entrez le code PIN pour la machine $machineId :")
      print("> ")
      val pin = readLine()

      if (pin == machinePins(index)) {
        println("Accès autorisé.")
        return true
      } else {
        tentative += 1
        println(s"Code PIN incorrect. ${3 - tentative} tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Accès refusé.")
    false
  }
}
