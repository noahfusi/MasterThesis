import io.StdIn._
import scala.util.Random
// Programme Exercice 2 fonctionnel
object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var testPin = ""
    var tentatives = 3
    var access = false

    do {
      testPin = readLine("Entrez le code PIN : \n> ")
      tentatives -= 1
      if (testPin != machinePins(machineId))
        println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
    } while (testPin != machinePins(machineId) && tentatives != 0)

    if (testPin == machinePins(machineId)) {
      access = true
      println("Accès accordé à la machine " + (machineId+1))
    } else if (testPin != machinePins(machineId) && tentatives == 0) {
      access = false
      println("\nTrop de tentatives échouées. Fin du programme.")
    }
    access
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    println("Mise à jour du code PIN pour la machine " + (machineId+1))

    do {
      newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    while (newPin.length != 6 || !newPin.forall(_.isDigit))
    if (newPin.length == 6) {
      machinePins(machineId) = newPin
      println("Le code PIN a été mis à jour avec succès.")
      Thread.sleep(500)
      println("\nRetour au menu principal...\n")
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var cafe = 0
    var prix_cafe = 0.0
    var nom_cafe = ""
    var taille = 0
    var statut_commande = true
    var choix_sucre = 0
    var nom_sucre = ""
    var prix_suppsucre = 0.0
    var choix_lait = 0
    var nbr_doses = 0
    var nom_lait = ""
    var prix_supplait = 0.05 * nbr_doses
    var prix_final = 0.0

    do {
      cafe = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
    }
    while (cafe < 1 || cafe > 3)

    if (cafe == 1 && coffeeStocks(machineId) >= 8) {
      prix_cafe = 2.00
      coffeeStocks(machineId) -= 8
      nom_cafe = "Expresso"
    } else if (cafe == 2 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {
      prix_cafe = 2.50
      coffeeStocks(machineId) -= 6
      milkStocks(machineId) -= 100
      nom_cafe = "Cappuccino"
    } else if (cafe == 3) {
      do {
        taille = readLine("Sélectionnez la taille de votre Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
      }
      while (taille < 1 || taille > 3)

      if (taille == 1 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120) {
        prix_cafe = 2.70
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 120
        nom_cafe = "Latte (Petit)"
      }
      else if (taille == 2 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150) {
        prix_cafe = 3.20
        coffeeStocks(machineId) -= 8
        milkStocks(machineId) -= 150
        nom_cafe = "Latte (Moyen)"
      }
      else if (taille == 3 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200) {
        prix_cafe = 3.70
        coffeeStocks(machineId) -= 12
        milkStocks(machineId) -= 200
        nom_cafe = "Latte (Grand)"
      } else {statut_commande = false
        println("Erreur. Quantité de poudre de café ou de lait insuffisante. \nSélectionnez une autre machine ou vérifiez les stocks en mode Admin.")
      }
    } else {statut_commande = false
      println("Erreur. Quantité de poudre de café ou de lait insuffisante. \nSélectionnez une autre machine ou vérifiez les stocks en mode Admin.")
    }

    if (statut_commande) {
      do {
        choix_sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      }
      while (choix_sucre < 1 || choix_sucre > 4)

      if (choix_sucre == 1) {
        nom_sucre = "Sans sucre"
      } else if (choix_sucre == 2) {
        sugarStocks(machineId) -= 5
        nom_sucre = "Peu (5g)"
      } else if (choix_sucre == 3) {
        sugarStocks(machineId) -= 10
        nom_sucre = "Moyen (10g)"
      } else if (choix_sucre == 4) {
        sugarStocks(machineId) -= 15
        nom_sucre = "Beaucoup (15g)"
      }
      prix_suppsucre = 0.10 * (choix_sucre - 1)
      if ((cafe == 2 || cafe == 3) && milkStocks(machineId) >= 50) {
        do {
          choix_lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n> ").toInt
        }
        while (choix_lait < 1 || choix_lait > 2)

        if (choix_lait == 1) {
          nom_lait = "Lait en supplément : Oui"
          nbr_doses = readLine("Combien de doses souhaitez-vous (1 dose : 50 mL) ? \n> ").toInt
          while (nbr_doses < 0 || nbr_doses > 3) {
            nbr_doses = readLine("Combien de doses souhaitez-vous (maximum 3) ? \n> ").toInt
          }
          milkStocks(machineId) -= nbr_doses * 50
          prix_supplait = nbr_doses * 0.05
        } else {
          nom_lait = "Lait en supplément : Non"
        }
      }
      // Détails de la commande et paiement
      prix_final = prix_cafe + prix_suppsucre + prix_supplait
      println("Boisson sélectionnée : " + nom_cafe + "\nNiveau de sucre : " + nom_sucre + "\n" + nom_lait)
      printf("Prix total : CHF %.2f " + " + CHF %.2f (Sucre)" + " + CHF %.2f (Lait)" + " = CHF %.2f \n", prix_cafe, prix_suppsucre, prix_supplait, prix_final)
      println("\nVeuillez payer en utilisant Twint.")

      val caracteres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var code = ""

      for (i <- 1 to 5) {
        val i = Random.nextInt(caracteres.length)
        val chaine = caracteres(i)
        code = code + chaine
      }
      print("Le code est : " + code)
      println("\n(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
      Thread.sleep(1000)
      println("[...]")
      Thread.sleep(3000)
      println("Votre " + nom_cafe + " est prêt ! Bonne dégustation !")
      Thread.sleep(500)
      println("\nRetour au menu principal...\n")
      Thread.sleep(1000)

      return true     // L'instruction return est facultative mais la laisser permet de mieux visualiser la valeur retournée.
    } else {return false}
  }
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Entrez les quantités à ajouter : ")

      var ajout_cafe = readLine("Poudre de café (g) > ").toInt
      while(ajout_cafe < 0){ajout_cafe = readLine("Erreur, saisissez à nouveau la quantité. \nPoudre de café (g) > ").toInt}

      var ajout_lait = readLine("Lait (L) > ").toInt * 1000
      while(ajout_lait < 0){ajout_lait = readLine("Erreur, saisissez à nouveau la quantité. \nLait (L) > ").toInt * 1000}

      var ajout_sucre = readLine("Sucre (g) > ").toInt
      while(ajout_sucre < 0){ajout_sucre = readLine("Erreur, saisissez à nouveau la quantité. \nSucre (g) > ").toInt}

      coffeeStocks(machineId) += ajout_cafe
      milkStocks(machineId) += ajout_lait
      sugarStocks(machineId) += ajout_sucre
      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
      Thread.sleep(1000)
    }

    def main(args: Array[String]): Unit = {

      val nbMachines = 5
      var machineId = Array.tabulate(nbMachines)(m => m)
      var num = 0
      var machinePins = Array.fill(nbMachines)("434343")
      var mode = 0
      var access = false
      var coffeeStocks = Array.fill(nbMachines)(50)
      var sugarStocks = Array.fill(nbMachines)(30)
      var milkStocks = Array.fill(nbMachines)(500) // En mL

      do {
        mode = readLine("         Nospresso Café \nSélectionnez le mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt

        if(mode < 1 || mode > 3){
        do {mode = readLine("Erreur. Sélectionnez le mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt}
        while (mode < 1 || mode > 3)
        }

        if(mode == 1 || mode == 2){
        do {num = readLine("\nVeuillez sélectionnez la machine (1-5) > ").toInt}
        while (num < 1 || num > nbMachines)
          println("Numéro de machine : " + num)
        }
        if (mode == 1) {
          serveClient(machineId(num-1), coffeeStocks, sugarStocks, milkStocks)
        }

        if (mode == 2) {
          println("L'accès au mode Admin requiert un code.")
          access = validatePin(num-1, machinePins)

          if (!access) {mode = 3}
          if (access) {
            println("\n( Information : le PIN est " + machinePins(num-1) + " )")
            // Indication : Montrer le PIN une fois que l'accès au mode Admin a été validé permet non seulement de vérifier le PIN actuel de la machine, mais aussi le cas échéant,
            // de vérifier après une modification du PIN qu'il est attribué à la bonne machine.
            var choix = 0
            do {choix = readLine("\nQue souhaitez-vous faire ? \n1) Réapprovisionner les stocks \n2) Changer le PIN \n3) Quitter\n> ").toInt}
            while (choix < 1 || choix > 3)

            if(choix == 1){
              println("Stocks : \nPoudre de café : " + coffeeStocks(machineId(num-1)) + " g\nLait : " + milkStocks(machineId(num-1))/1000.0 + " L\nSucre : " + sugarStocks(machineId(num-1)) + " g")
              restockMachine(machineId(num-1), coffeeStocks, sugarStocks, milkStocks)
            }
            if (choix == 2) {
              updatePin(num-1, machinePins)
            }
            if (choix == 3) {
              println("Retour au menu principal...")
              Thread.sleep(1000)
            }
          }
        }
      } while (mode != 3)
      if(mode == 3){println()}
    }
}
