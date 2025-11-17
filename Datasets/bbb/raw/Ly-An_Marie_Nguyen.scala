import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    var mode = 0
    var machineSelected = 0
    val nbMachines = 5
    val coffeeStock = new Array[Int](nbMachines)
    val sugarStock = new Array[Int](nbMachines)
    val milkStock = new Array[Int](nbMachines)
    val machinePins = new Array[String](nbMachines)

    // Initialisation des stocks
    for (i <- 0 until nbMachines ){
      coffeeStock(i) = 50
      sugarStock(i) = 30
      milkStock(i) = 500
      machinePins(i) = "434343"
    }


    do {
      // ------------------ Séléction de la machine et du mode ---------------------
      println("       Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client\n2) Admin\n3) Quitter")
      mode = readLine("> ").toInt
      while (mode != 1 && mode != 2 && mode != 3) {
        println("Veuillez entrer un mode valable")
        mode = readLine("> ").toInt
      }
      if (mode != 3) {
        println("Veuillez sélectionner une machine (1-" + nbMachines + ") :")
        machineSelected = readLine("> ").toInt
        while (machineSelected > nbMachines || machineSelected < 1) {
          println("Veuillez entrer une machine valable : (1-" + nbMachines + ") :")
          machineSelected = readLine("> ").toInt
        }
      }
      // -------------------- Mode Client --------------------------
      if (mode == 1) {
        serveClient(machineSelected-1, coffeeStock, sugarStock, milkStock)
      }
      // -------------------- Mode Admin --------------------------
      else if (mode == 2) {
        if (validatePin(machineSelected-1, machinePins)) {
          println("1) Mettre à jour les stocks\n2) Changer le code PIN")
          var admin = readLine("> ").toInt
          while (admin != 1 && admin != 2){
            println("Veuillez entrer un mode valide.")
            admin = readLine("> ").toInt
          }
          if (admin == 1) {
            restockMachine(machineSelected-1, coffeeStock, sugarStock, milkStock)
          }
          else if (admin == 2) {
            updatePin(machineSelected-1, machinePins)
          }
        }
        else {
          mode = 3
        }
      }
    } while (mode != 3)

  }

  def validatePin(machineId : Int, machinePins : Array[String]) : Boolean = {
    var nbEssai = 3
    var pin = "000000"
    println("Entrez le code PIN : ")
    pin = readLine("> ")
    while (machinePins(machineId) != pin && nbEssai > 0) {
      if (nbEssai > 1) {
        println("Code PIN incorrect. " + nbEssai + " tentatives restantes.")
      }
      else {
        println("Code PIN incorrect. " + nbEssai + " tentative restante.")
      }
      pin = readLine("> ")
      nbEssai -= 1
    }
    if (nbEssai == 0) {
      println("Trop de tentatives échouée. Fin du Programme")
      return false
    }
    else {
      println("Accès accordé à la machine " + (machineId+1) + ".")
      return true
    }
  }

  def updatePin(machineId : Int, machinePins : Array[String]) : Unit = {
      val machineNb = machineId + 1
      var newPin = "0"
      println("Mise à jour du code PIN pour la machine " + machineNb + ".\n")
      while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      machinePins(machineId) = newPin
      println("\nLe code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...\n")
      Thread.sleep(500)
  }

  def serveClient(machineId : Int, coffeeStocks : Array[Int], sugarStocks : Array[Int], milkStocks : Array[Int]) : Boolean = {
    var boisson = 0
    var latteTaille = 0
    var sucre = 0
    var lait = 0
    var doseLait = 0

    println("Veuillez sélectionner votre boisson :")
    println("1) Espresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte")
    boisson = readLine("> ").toInt
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      println("Veuillez entrer une boisson valable")
      boisson = readLine("> ").toInt
    }
    // Séléction de la taille du Latte
    if (boisson == 3) {
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")

      latteTaille = readLine("> ").toInt
      while (latteTaille != 1 && latteTaille != 2 && latteTaille != 3) {
        println("Veuillez entrer une taille valable")
        latteTaille = readLine("> ").toInt
      }
    }

    // Sélection du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    sucre = readLine("> ").toInt
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
      println("Veuillez entrer une quantité de sucre valable")
      sucre = readLine("> ").toInt
    }

    // Sélection du Lait
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui\n2) Non")
      lait = readLine("> ").toInt
      while (lait != 1 && lait != 2) {
        println("Veuillez entrer une valeur valable")
        lait = readLine("> ").toInt
      }
      if (lait == 1) {
        println("Combien de dose ?")
        doseLait = readLine("> ").toInt
        while (doseLait > 3) {
          println("Maximum 3 doses de lait")
          doseLait = readLine("> ").toInt
        }
      }
    }

    // variable pour faciliter les prints (convertit les chiffres en leur nom en strings)
    // déclaration des prix
    // Manipulation des stocks des ingrédients
    var boissonName = ""
    var sucreName = ""
    var laitName = ""
    var tailleName = ""

    var boissonPrix = 0.0
    var sucrePrix = 0.0
    var laitSuppPrix = 0.0
    var total = 0.0

    // Tyype de boisson
    if (boisson == 1) {
      boissonName = "Expresso"
      boissonPrix = 2.00
      coffeeStocks(machineId) = coffeeStocks(machineId) - 8
    }
    else if (boisson == 2) {
      boissonName = "Cappuccino"
      boissonPrix = 2.50
      coffeeStocks(machineId) = coffeeStocks(machineId) - 6
      milkStocks(machineId) = milkStocks(machineId) - 100
    }
    else boissonName = "Latte"

    // Quantité de sucre
    if (sucre == 1) sucreName = "Sans sucre"
    else if (sucre == 2) {
      sucreName = "Peu (5g)"
      sugarStocks(machineId) = sugarStocks(machineId) - 5
      sucrePrix = 0.10
    }
    else if (sucre == 3) {
      sucreName = "Moyen (10g)"
      sugarStocks(machineId) = sugarStocks(machineId) - 10
      sucrePrix = 0.20
    }
    else {
      sucreName = "Beaucoup (15g)"
      sugarStocks(machineId) = sugarStocks(machineId) - 15
      sucrePrix = 0.30
    }

    // Quantité de lait
    if (lait == 2) laitName = "Non"
    else {
      laitName = "Oui (" + doseLait + ")"
      milkStocks(machineId) = milkStocks(machineId) - (doseLait * 50)
    }

    // Taille du Latte
    if (latteTaille == 1) {
      tailleName = "petit"
      boissonPrix = 2.70
      coffeeStocks(machineId) = coffeeStocks(machineId) - 6
      milkStocks(machineId) = milkStocks(machineId) - 120
    }
    else if (latteTaille == 2) {
      tailleName = "moyen"
      boissonPrix = 3.20
      coffeeStocks(machineId) = coffeeStocks(machineId) - 8
      milkStocks(machineId) = milkStocks(machineId) - 150
    }
    else if (latteTaille == 3) {
      tailleName = "grand"
      boissonPrix = 3.70
      coffeeStocks(machineId) = coffeeStocks(machineId) - 12
      milkStocks(machineId) = milkStocks(machineId) - 200
    }

    // Résumé de la commande
    println("Machine sélectionnée : " + (machineId+1))
    print("Boisson sélectionnée : " + boissonName)
    if (boisson == 3)
      println(" (" + tailleName + ")")
    else println()
    println("Niveau de sucre : " + sucreName)
    if (boisson == 2 || boisson == 3) {
      println("Lait en supplément : " + laitName)
    }

    // Vérification des stocks
    if (sugarStocks(machineId) >= 0 && coffeeStocks(machineId) >= 0 && milkStocks(machineId) >= 0) {
      // Paiement
      if (lait == 1) {
        laitSuppPrix = 0.05 * doseLait
        total = boissonPrix + sucrePrix + laitSuppPrix
        printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, laitSuppPrix, total)
      }
      else {
        total = boissonPrix + sucrePrix
        printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, total)
      }

      println()
      println("Veuillez payer en utilisant Twint.")

      print("Votre code de paiement est : ")
      val codeChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var index = 0
      for (_ <- 0 to 4) {
        index = (math.random * 36).toInt
        print(codeChars(index))
      }
      println()
      println("(En attente de validation du paiement...)")
      // programme attend 3 secondes
      Thread.sleep(3000)
      println()
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre " + boissonName + " est prêt ! Bonne Dégustation !\n")
      return true
    }
    else {
      if (sugarStocks(machineId) < 0) {
        println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine.\n")
        sugarStocks(machineId) = 0
      }
      else if (coffeeStocks(machineId) < 0) {
        println("\nErreur : Quantité de poucre de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine.\n")
        coffeeStocks(machineId) = 0
      }
      else if (coffeeStocks(machineId) < 0 && (latteTaille == 2 || latteTaille == 3)) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite, essayer une autre boisson ou changer de machine\n")
      }
      else if (milkStocks(machineId) < 0) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine\n")
        milkStocks(machineId) = 0
      }
      return false
    }
  }

  def restockMachine(machineId : Int, coffeeStocks : Array[Int], sugarStocks : Array[Int], milkStocks : Array[Int]) : Unit = {
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + coffeeStocks(machineId) + "g")
    println("Sucre :          " + sugarStocks(machineId) + "g")
    var milk = milkStocks(machineId).toDouble
    milk = milk / 1000
    println("Lait :           " + milk + "L")
    println("\nEntrez les quantités à ajouter : ")
    coffeeStocks(machineId) = coffeeStocks(machineId) + readLine("Poudre de Café > ").toInt
    sugarStocks(machineId) = sugarStocks(machineId) + readLine("Sucre > ").toInt
    milk = readLine("Lait > ").toDouble
    milk = milk * 1000
    milkStocks(machineId) = milkStocks(machineId) + milk.toInt
    println("\nLes stock ont été mis à jour avec succés.")
    println("Retour au menu principal...\n")
    Thread.sleep(500)
  }

}