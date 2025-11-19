import io.StdIn._

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var lecturecodePIN = " "
    print("Entrez le code PIN :\n> ")
    lecturecodePIN = readLine()
    if (lecturecodePIN == machinePins(machineId)) return true
    else {
      print("Code PIN incorrect. 2 tentatives restantes.\n> ")
      lecturecodePIN = readLine()
      if (lecturecodePIN == machinePins(machineId)) return true
      else {
        print("Code PIN incorrect. 1 tentative restante.\n> ")
        lecturecodePIN = readLine()
        if (lecturecodePIN == machinePins(machineId)) return true
        else {
          print("Code PIN incorrect. 0 tentatives restantes.\n\nTrop de tentatives échouées. Fin du programme.")
          return false
        }
      }
    }
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var entreePIN = ""
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    while (entreePIN.length != 6) {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      entreePIN = readLine()
    }
    machinePins(machineId) = entreePIN
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    entreePIN = ""
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var boisson = 0
    var taillelatte = 0
    var sucreclient = 0
    var laitclient = 0
    var doselait = 4
    val prixsucre = 0.1
    val prixlait = 0.05
    var prix = 0.0
    var erreur = false

    print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
      boisson = readInt()
      if (!(boisson == 1 || boisson == 2 || boisson == 3)) {
        print("Veuillez sélectionner un numéro de boisson possible (1, 2 ou 3) > ")
      }
    }

    if (boisson == 3) {
      print("Veuillez choisir une taille de Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
      while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
        taillelatte = readInt()
        if (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
          print("Veuillez sélectionner une taille de Latte possible (1, 2 ou 3) > ")
        }
      }
    }

    print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4)Beaucoup (15g) - CHF 0.30\n> ")
    while (!(sucreclient == 1 || sucreclient == 2 || sucreclient == 3 || sucreclient == 4)) {
      sucreclient = readInt()
      if (!(sucreclient == 1 || sucreclient == 2 || sucreclient == 3 || sucreclient == 4)) {
        print("Veuillez sélectionner une quantité de sucre possible (1, 2, 3 ou 4) > ")
      }
    }
    sucreclient = (sucreclient - 1) * 5

    if (boisson == 2 || boisson == 3) {
      print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
      while (!(laitclient == 1 || laitclient == 2)) {
        laitclient = readInt()
        if (!(laitclient == 1 || laitclient == 2)) {
          print("Veuillez choisir une option disponible (1 ou 2) > ")
        }
      }
    }

    if (laitclient == 1) {
      print("Combien de dose ?\n> ")
      while (doselait < 0 || doselait > 3) {
        doselait = readInt()
        if (doselait < 0 || doselait > 3) {
          print("Veuillez choisir une dose possible (0, 1, 2 ou 3) > ")
        }
      }
      laitclient = doselait * 50
    }
    else {
      laitclient = 0
      doselait = 0
    }

    if (boisson == 1) {
      println("Boisson sélectionnée : Expresso")
      if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
      else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
      else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
      else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")

      if ((coffeeStocks(machineId) - 8 >= 0) && (sugarStocks(machineId) - sucreclient >= 0)) {
        coffeeStocks(machineId) = coffeeStocks(machineId) - 8
        sugarStocks(machineId) = sugarStocks(machineId) - sucreclient
        prix = 2.0 + (prixsucre * (sucreclient / 5))
        printf("Prix total : CHF 2.00 + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prix)
        println
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
        println("(En attente de paiement...)\n")
        Thread.sleep(3000)
        println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Expresso est prêt ! Bonne dégustation !")
      }

      else {
        println("Erreur : Stocks insuffisants. Veuillez sélectionner une autre machine.\n")
        erreur = true
      }
    }

    if (boisson == 2) {
      println("Boisson sélectionnée : Cappuccino")
      if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
      else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
      else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
      else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
      if (doselait == 0) println("Lait en supplément : Non")
      else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
      else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
      else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

      if ((coffeeStocks(machineId) - 6 >= 0) && (sugarStocks(machineId) - sucreclient >= 0) && (milkStocks(machineId) - laitclient - 100 >= 0)) {
        coffeeStocks(machineId) = coffeeStocks(machineId) - 6
        sugarStocks(machineId) = sugarStocks(machineId) - sucreclient
        milkStocks(machineId) = milkStocks(machineId) - laitclient - 100
        prix = 2.50 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
        printf("Prix total : CHF 2.50 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
        println
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
        println("(En attente de paiement...)\n")
        Thread.sleep(3000)
        println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Cappuccino est prêt ! Bonne dégustation !")
      }

      else {
        println("Erreur : Stocks insuffisants. Veuillez sélectionner une autre machine.\n")
        erreur = true
      }
    }

    if (taillelatte == 1) {
      println("Boisson sélectionnée : Latte (Petit)")
      if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
      else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
      else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
      else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
      if (doselait == 0) println("Lait en supplément : Non")
      else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
      else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
      else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

      if ((coffeeStocks(machineId) - 6 >= 0) && (sugarStocks(machineId) - sucreclient >= 0) && (milkStocks(machineId) - laitclient - 120 >= 0)) {
        coffeeStocks(machineId) = coffeeStocks(machineId) - 6
        sugarStocks(machineId) = sugarStocks(machineId) - sucreclient
        milkStocks(machineId) = milkStocks(machineId) - laitclient - 120
        prix = 2.70 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
        printf("Prix total : CHF 2.70 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
        println
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
        println("(En attente de paiement...)\n")
        Thread.sleep(3000)
        println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
      }

      else {
        println("Erreur : Stocks insuffisants. Veuillez sélectionner une autre machine.\n")
        erreur = true
      }
    }

    if (taillelatte == 2) {
      println("Boisson sélectionnée : Latte (Moyen)")
      if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
      else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
      else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
      else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
      if (doselait == 0) println("Lait en supplément : Non")
      else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
      else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
      else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

      if ((coffeeStocks(machineId) - 8 >= 0) && (sugarStocks(machineId) - sucreclient >= 0) && (milkStocks(machineId) - laitclient - 150 >= 0)) {
        coffeeStocks(machineId) = coffeeStocks(machineId) - 8
        sugarStocks(machineId) = sugarStocks(machineId) - sucreclient
        milkStocks(machineId) = milkStocks(machineId) - laitclient - 150
        prix = 3.20 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
        printf("Prix total : CHF 3.20 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
        println
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
        println("(En attente de paiement...)\n")
        Thread.sleep(3000)
        println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
      }

      else {
        println("Erreur : Stocks insuffisants. Veuillez sélectionner une autre machine.\n")
        erreur = true
      }
    }

    if (taillelatte == 3) {
      println("Boisson sélectionnée : Latte (Grand)")
      if (sucreclient == 0) println("Niveau de sucre : Sans sucre")
      else if (sucreclient == 5) println("Niveau de sucre : Peu (5g)")
      else if (sucreclient == 10) println("Niveau de sucre : Moyen (10g)")
      else if (sucreclient == 15) println("Niveau de sucre : Beaucoup (15g)")
      if (doselait == 0) println("Lait en supplément : Non")
      else if (doselait == 1) println("Lait en supplément : 1 dose (50ml)")
      else if (doselait == 2) println("Lait en supplément : 2 doses (100ml)")
      else if (doselait == 3) println("Lait en supplément : 3 doses (150ml)")

      if ((coffeeStocks(machineId) - 12 >= 0) && (sugarStocks(machineId) - sucreclient >= 0) && (milkStocks(machineId) - laitclient - 200 >= 0)) {
        coffeeStocks(machineId) = coffeeStocks(machineId) - 12
        sugarStocks(machineId) = sugarStocks(machineId) - sucreclient
        milkStocks(machineId) = milkStocks(machineId) - laitclient - 200
        prix = 3.70 + (prixsucre * (sucreclient / 5)) + (prixlait * doselait)
        printf("Prix total : CHF 3.70 + CHF %.2f + CHF %.2f = CHF %.2f \n", prixsucre * (sucreclient / 5), prixlait * doselait, prix)
        println
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + util.Random.alphanumeric.take(5).mkString)
        println("(En attente de paiement...)\n")
        Thread.sleep(3000)
        println("Paiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne dégustation !")
      }

      else {
        println("Erreur : Stocks insuffisants. Veuillez sélectionner une autre machine.\n")
        erreur = true
      }
    }
    boisson = 0
    taillelatte = 0
    doselait = 4
    if (erreur == false) return true
    else return false
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var ajoutpoudrecafe = -1
    var ajoutsucre = -1
    var ajoutlait = -1.0
    var ajoutlaitInt = 0

    println("Niveaux de stock actuels :\n\tPoudre de café: " + coffeeStocks(machineId) + "g\n\tSucre\t\t  : " + sugarStocks(machineId) + "g\n\tLait\t\t  : " + milkStocks(machineId).toDouble / 1000 + "L\n")

    println("Entrez les quantités à ajouter :")

    while (ajoutpoudrecafe < 0) {
      ajoutpoudrecafe = readLine("Poudre de café > ").toInt
    }
    coffeeStocks(machineId) = coffeeStocks(machineId) + ajoutpoudrecafe
    ajoutpoudrecafe = -1

    while (ajoutsucre < 0) {
      ajoutsucre = readLine("Sucre > ").toInt
    }
    sugarStocks(machineId) = sugarStocks(machineId) + ajoutsucre
    ajoutsucre = -1

    while (ajoutlait < 0.0) {
      ajoutlait = readLine("Lait > ").toDouble
    }
    ajoutlaitInt = (ajoutlait * 1000).toInt
    milkStocks(machineId) = milkStocks(machineId) + ajoutlaitInt
    ajoutlait = -1.0
    ajoutlaitInt = 0

    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var repetition = true
    var mode = 0
    val codePIN = Array.fill(nbMachines)("434343")
    var poudrecafe = Array.fill(nbMachines)(50)
    var stocksucre = Array.fill(nbMachines)(30)
    var stocklait = Array.fill(nbMachines)(500)
    var machine = 6
    var choixadmin = 0
    var erreurmainclient = true

    while (repetition == true) {
      print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        mode = readInt()
        if (!(mode == 1 || mode == 2 || mode == 3)) {
          print("Veuillez sélectionner un numéro de mode possible (1, 2 ou 3) > ")
        }
      }

      if (mode == 3) {
        repetition = false
      }

      if (mode == 1) {
        while (erreurmainclient == true) {
          print("Machine sélectionée (1-5) > ")
          while (!(machine == 1 || machine == 2 || machine == 3 || machine == 4 || machine == 5)) {
            machine = readInt()
            if (!(machine == 1 || machine == 2 || machine == 3 || machine == 4 || machine == 5)) {
              print("Veuillez sélectionner un numéro de machine possible (1, 2, 3, 4 ou 5) > ")
            }
          }
          machine = machine - 1

          var client = serveClient(machine, poudrecafe, stocksucre, stocklait)

          if (client == true) erreurmainclient = false
          else erreurmainclient = true

          machine = 6
        }
        erreurmainclient = true
      }

      if (mode == 2) {
        print("Que souhaitez-vous faire?\n1) Ajouter des stocks\n2) Modifier le code PIN\n> ")
        while (!(choixadmin == 1 || choixadmin == 2)) {
          choixadmin = readInt()
          if (!(choixadmin == 1 || choixadmin == 2)) {
            print("Veuillez choisir une option disponible (1 ou 2) > ")
          }
        }

        print("Machine sélectionée (1-5) > ")
        while (!(machine == 1 || machine == 2 || machine == 3 || machine == 4 || machine == 5)) {
          machine = readInt()
          if (!(machine == 1 || machine == 2 || machine == 3 || machine == 4 || machine == 5)) {
            print("Veuillez sélectionner un numéro de machine possible (1, 2, 3, 4 ou 5) > ")
          }
        }
        machine = machine - 1

        var codevalide = validatePin(machine, codePIN)
        if (codevalide == true) {
          println("Accès accordé à la Machine " + (machine + 1) + ".")

          if (choixadmin == 1) {
            restockMachine(machine, poudrecafe, stocksucre, stocklait)
          }
          else {
            println
            updatePin(machine, codePIN)
          }
        }

        else {
          repetition = false
        }
        choixadmin = 0
      }
      println
      mode = 0
      machine = 6
    }
  }
}
