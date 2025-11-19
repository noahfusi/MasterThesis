import scala.io.StdIn._
import scala.util._

object Main {
  def main(args: Array[String]): Unit = {
    var running = true // si running est true, alors on continue, si running est false alors on finit le programme (cas du mode 3)
    var mode = ""
    var machineId = 0

    //Stocks machines
    var coffeeStocks = Array(50, 50, 50, 50, 50) // C'est en g
    var sugarStocks = Array(30, 30, 30, 30, 30) // C'est en g
    var milkStocks = Array(500, 500, 500, 500, 500) // C'est en mL

    //Codes Pins des machines
    var machinePins = Array("434343", "434343", "434343", "434343", "434343")

    while (running) {
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode = readLine("> ")

      while ((mode != "1") && (mode != "2") && (mode != "3")) {
        mode = readLine("Insérez une valeur correcte : ")
      } //cette boucle while va tourner tant que l'utilisateur ne va pas donner une valeur égale à 1 ou 2 ou 3

      if (mode == "1") {
        machineId = selectionMachine
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      }
      if (mode == "2") {
        serveAdmin(machinePins, coffeeStocks, sugarStocks, milkStocks)
      }
      if (mode == "3") {
        print("Au revoir !")
        running = false
      }
    }

    //Méthode de sélection d'une machine (ca permet de nous simplifier la vie :) )
    def selectionMachine : Int = {
      var machineId = readLine("Machine sélectionnée (1-5) > ")
      while ((machineId != "1") && (machineId != "2") && (machineId != "3") && (machineId != "4") && (machineId != "5")) {
        machineId = readLine("Insérez une valeur correcte : ")
      }
      // vu que dans les tableaux des stocks, il  y a des cellules allant de 0 à 4 et non de 1 à 5, il faut faire -1
      return machineId.toInt - 1
    }

    // Méthode permettant de valider un PIN
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      val vraiPIN = machinePins(machineId).toString
      var tentatives = 3
      var tentativePin = readLine("Entrez le code PIN : ")
      while ((vraiPIN != tentativePin) && (tentatives > 0)) {
        tentatives -= 1
        if (tentatives == 0) {
          println("Trop de tentatives échouées. Fin du programme.")
          running = false
        } else {
          println("Code PIN incorrect. " + tentatives + " tentative restante.")
          tentativePin = readLine("> ")
        }
      }
      if (vraiPIN == tentativePin) {
        running = true
      }
      return running
    }

    // Méthode permettant de vérifier si les stocks sont suffisant pour valider la préparation d'une boisson
    def validateStock(Stocks: Int, Necessaire: Int, Ingrediants: String): Boolean = {
      if (Necessaire > Stocks) {
        println("Erreur : Quantité de " + Ingrediants + " insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez sélectionner une autre machine")
        return false
      } else {
        return true
      }
    }

    // Méthode permettant de mettre à jour un mot de passe
    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var nouveauPin = ""
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while (nouveauPin.toIntOption.isEmpty && (nouveauPin.length != 6)) {
        nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      machinePins(machineId) = nouveauPin
      Thread.sleep(2000)
      println("Le code PIN a été mis à jour avec succès.")
      Thread.sleep(2000)
      println("Retour au menu principal...")
      Thread.sleep(1000)
    }

    // Méthode permettant d'interagir avec un client
    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      // Initialisation des besoins
      var cafeNecessaire = 0
      var laitNecessaire = 0
      var sucreNecessaire = 0

      // Code de paiement
      var codePaiement = ""

      var prixFinal = 0.0
      var prixCafe = 0.0
      var prixSucre = 0.0
      val prixLait = 0.05

      var prixLaitSupp = 0.0
      var nomBoisson = "" // permet d'afficher le nom de la boisson
      var Boisson = "" //ca sert pour quand on donne le choix et il faut donner 1, 2 ou 3
      var QuantiteSucre = ""

      var nbDosesLaitSup = 0
      var laitSupp = "non"

      // Expresso
      val prixExpresso = 2.00 // en CHF
      val quantiteCafeExpresso = 8 // en gramme

      // Cappucino
      val prixCappuccino = 2.50 // en CHF
      val quantiteCafeCappuccino = 6 // en gramme
      val quantiteLaitCappuccino = 100  // en ml

      // Latte (Petit)
      val prixLattePetit = 2.70 // en CHF
      val quantiteCafeLattePetit = 6 // en gramme
      val quantiteLaitLattePetit = 120 // en ml

      // Latte (Moyen)
      val prixLatteMoyen = 3.20 // en CHF
      val quantiteCafeLatteMoyen = 8 // en gramme
      val quantiteLaitLatteMoyen = 150 // en ml

      // Latte (Grand)
      val prixLatteGrand = 3.70 // en CHF
      val quantiteCafeLatteGrand = 12 // en gramme
      val quantiteLaitLatteGrand = 200 // en ml


      println("        Mode Client")
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      Boisson = readLine("> ")
      while (Boisson != "1" && Boisson != "2" && Boisson != "3") {
        Boisson = readLine("Insérez une valeur correcte : ")
      }

      if (Boisson == "1") {
        nomBoisson = "Expresso"
        cafeNecessaire += quantiteCafeExpresso
        prixCafe += prixExpresso
      }

      if (Boisson == "2") {
        nomBoisson = "Cappuccino"
        cafeNecessaire += quantiteCafeCappuccino
        laitNecessaire += quantiteLaitCappuccino
        prixCafe += prixCappuccino
      }

      if (Boisson == "3") {
        println("De quelle taille souhaitez-vous votre Latte ?")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        var tailleLatte = readLine("> ")

        while ((tailleLatte != "1") && (tailleLatte != "2") && (tailleLatte != "3")) {
          tailleLatte = readLine("Insérez une valeur correcte : ")
        }

        if (tailleLatte == "1"){
          nomBoisson = "Latte (Petit)"
          cafeNecessaire += quantiteCafeLattePetit
          laitNecessaire += quantiteLaitLattePetit
          prixCafe += prixLattePetit
        }
        if (tailleLatte == "2"){
          nomBoisson = "Latte (Moyen)"
          cafeNecessaire += quantiteCafeLatteMoyen
          laitNecessaire += quantiteLaitLatteMoyen
          prixCafe += prixLatteMoyen
        }
        if (tailleLatte == "3"){
          nomBoisson = "Latte (Grand)"
          cafeNecessaire += quantiteCafeLatteGrand
          laitNecessaire += quantiteLaitLatteGrand
          prixCafe += prixLatteGrand
        }
      }

      // Ajouter du sucre
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      var choixSucre = readLine("> ")

      while ((choixSucre != "1") && (choixSucre != "2") && (choixSucre != "3") && (choixSucre != "4")) {
        choixSucre = readLine("Insérez une valeur correcte : ")
      }

      if (choixSucre == "1") {
        QuantiteSucre = "Sans sucre"
        sucreNecessaire = 0
        prixSucre = 0
      }
      if (choixSucre == "2" ){
        QuantiteSucre = "Peu (5g)"
        sucreNecessaire = 5
        prixSucre = 0.1
      }
      if (choixSucre == "3") {
        QuantiteSucre = "Moyen (10g)"
        sucreNecessaire = 10
        prixSucre = 0.2
      }
      if (choixSucre == "4") {
        QuantiteSucre = "Beaucoup (15g)"
        sucreNecessaire = 15
        prixSucre = 0.3
      }
      if ((Boisson == "2") || (Boisson == "3")) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        var choixLaitSup = readLine("> ")
        while ((choixLaitSup != "1") && (choixLaitSup != "2")) {
          println("Entrée invalide, veuillez réessayer : ")
          choixLaitSup = readLine("> ")
        }
        if (choixLaitSup == "1") {
          println("Combien de doses ?")
          nbDosesLaitSup = readLine("> ").toInt
          while ((nbDosesLaitSup != 1) && (nbDosesLaitSup != 2) && (nbDosesLaitSup != 3)) {
            println("Erreur : Vous pouvez ajouter entre 1 et 3 doses de lait seulement")
            println("Combien de doses ?")
            nbDosesLaitSup = readLine("> ").toInt
          }
          // Réduction des stocks de lait en fonction des doses
          laitSupp = "Oui"
          laitNecessaire += nbDosesLaitSup * 50 // chacune des doses mesure 0.05 l
          prixLaitSupp += nbDosesLaitSup * 0.05
        }
      }
      running = true

      // Vérification des stocks
      val cafeOK = validateStock(coffeeStocks(machineId), cafeNecessaire, "poudre de café")
      val sucreOK = validateStock(sugarStocks(machineId), sucreNecessaire, "sucre")
      val laitOK = validateStock(milkStocks(machineId), laitNecessaire, "lait")

      if (cafeOK && sucreOK && laitOK) {
        running = true
        println("Boisson sélectionnée : "+ nomBoisson)
        println("Niveau de sucre : "+ QuantiteSucre)
        println("Lait en supplément : "+ laitSupp)

        //Retrait des quantité dans les stocks
        coffeeStocks(machineId) -= cafeNecessaire
        sugarStocks(machineId) -= sucreNecessaire
        milkStocks(machineId) -= laitNecessaire

        // Calcul et affichage du prix total
        prixLaitSupp = (prixLait * nbDosesLaitSup).toFloat
        prixFinal = (prixCafe + prixSucre + prixLaitSupp).toFloat
        var affichagePrix = "Prix total : CHF " + prixCafe + "0"

        if (prixSucre > 0) {
          affichagePrix += " + CHF " + prixSucre + "0"
        }
        if (prixLaitSupp > 0) {
          affichagePrix += " + CHF " + (prixLaitSupp.toFloat + "0").take(4)
        }

        val affichagePrixTotale = " = CHF " + (prixFinal.toFloat + "0").take(4) // .take(4)m limite l'affichage du prixFinal à 4 éléments
        if (prixSucre > 0 || prixLaitSupp > 0) {
          println(affichagePrix + affichagePrixTotale)
        }
        else {
          println(affichagePrix)
        }

        codePaiement = Random.alphanumeric.take(5).mkString.toUpperCase() //génère un code aléatoire de 5 nombres ou chiffres
        println("Veuillez payer avec Twint")
        println("Votre code de paiement est :"+ codePaiement)
        println("(En attente de validation du paiement...)")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (2 secondes)
        println("Merci, votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        Thread.sleep(2000) // Attend pendant 2000 millisecondes (2 secondes)
        if (Boisson == "3") {
          println("Votre Latte est prêt ! Bonne dégustation !")
        } else {
          println("Votre "+nomBoisson+" est prêt ! Bonne dégustation !")
        }
      } else {
        running = false
      }
      return running
    }

    // Méthode permettant d'interagir avec un administrateur
    def serveAdmin(machinePins: Array[String], coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      var actionAdmin = ""
      println("        Mode Admin")
      println("Quelle opération voulez vous faire? ")
      println("1) Restock")
      println("2) Changement du PIN")
      actionAdmin = readLine("> ")
      while ((actionAdmin != "1") && (actionAdmin != "2")) {
        actionAdmin = readLine("Insérez une valeur correcte : ")
      }
      machineId = selectionMachine

      if (validatePin(machineId, machinePins)) {
        if (actionAdmin == "1") {
          println("Accès accordé à la Machine " + (machineId+1) + ".")
          restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        }
        if (actionAdmin == "2") {
          println("Accès accordé.")
          updatePin(machineId, machinePins)
        }
      }
    }

    // Méthode permettant d'augmenter les stocks
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Niveaux de stock actuels:")
      println("Poudre de café : " + coffeeStocks(machineId) + "g")
      println("Sucre : " + sugarStocks(machineId) + "g")
      println("Lait : " + milkStocks(machineId).toFloat/1000 + "L")
      println("Entrez les quantités à ajouter :")
      print("Poudre de café > ")
      coffeeStocks(machineId) += readInt()
      print("Lait > ")
      milkStocks(machineId) += (readFloat()*1000).toInt // Valeur à donner en L
      print("Sucre > ")
      sugarStocks(machineId) += readInt()
      Thread.sleep(2000)
      println("Les stocks ont été mis à jour avec succès.")
      Thread.sleep(2000)
      println("Retour au menu principal...")
      Thread.sleep(1000)
    }
  }
}