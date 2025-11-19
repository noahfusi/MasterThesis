import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {

    val nbMachines = 5

    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)
    val machinePins = Array.fill(nbMachines)("434343")

    var continuer = true

    while (continuer) {

      var mode = readLine("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("Nospresso Café\nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readInt()
      }

      if (mode == 1) {
        var machineselectionnee = 0
        var commandevalidee = false

        while (!commandevalidee) {
          machineselectionnee = readLine("Machine sélectionnée (1-5) > ").toInt
          while (machineselectionnee < 1 || machineselectionnee > nbMachines) {
            println("Machine sélectionnée (1-5) > ")
            machineselectionnee = readInt()
          }
          val machineId = machineselectionnee - 1

          commandevalidee = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)

          if (!commandevalidee) {
          }
        }

      } else if (mode == 2) {

        var adminchoix = readLine("\nSéléctionnez votre choix en mode Administarteur :\n1) Réapprovisionner le stock ou\n2) Mettre à jour le code PIN ?\n> ").toInt

        while(!(adminchoix==1 || adminchoix==2)){
          println("\nSéléctionnez votre choix en mode Administarteur :\n1) Réapprovisionner\n2) Mettre à jour le code PIN\n> ")
          adminchoix = readInt()
        }

        var machineselectionnee = readLine("Machine sélectionnée (1-5) > ").toInt
        while(machineselectionnee < 1 || machineselectionnee > nbMachines) {
          println("Machine sélectionnée (1-5) > ")
          machineselectionnee = readInt()
        }
        var machineId = machineselectionnee - 1
        if (!(machineselectionnee < 1 || machineselectionnee > nbMachines)) {
          if (validatePin(machineId, machinePins)) {
            println("Accès accordé à la Machine " + machineselectionnee)

            if (adminchoix == 1) {
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            } else if (adminchoix == 2) {
              updatePin(machineId, machinePins)
            }

          } else {
            println("Trop de tentatives échouées. Fin du programme.\n")
          }
        }
      } else if (mode == 3) {
        continuer = false
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    val prixexpresso = 2.00
    val prixcappuccino = 2.50
    val prixlattePetit = 2.70
    val prixlatteMoyen = 3.20
    val prixlatteGrand = 3.70

    val cafeexpresso = 8
    val cafecappuccino = 6
    val cafelattePetit = 6
    val cafelatteMoyen = 8
    val cafelatteGrand = 12

    val laitcappuccino = 100
    val laitlattePetit = 120
    val laitlatteMoyen = 150
    val laitlatteGrand = 200

    var choixboisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ").toInt
    while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3)) {
      println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) > ")
      choixboisson = readInt()
    }

    var nomboisson = ""
    var prixboisson = 0.0
    var cafebesoin = 0
    var laitbesoin = 0

    if (choixboisson == 1) {
      nomboisson = "Expresso"
      prixboisson = prixexpresso
      cafebesoin = cafeexpresso
    } else if (choixboisson == 2) {
      nomboisson = "Cappuccino"
      prixboisson = prixcappuccino
      cafebesoin = cafecappuccino
      laitbesoin = laitcappuccino
    } else if (choixboisson == 3) {
      var taillelate = readLine("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ").toInt
      while (!(taillelate == 1 || taillelate == 2 || taillelate == 3)) {
        print("Veuillez sélectionner la taille :\n1) Petit\n2) Moyen\n3) Grand> ")
        taillelate = readInt()
      }
      if (taillelate == 1) {
        nomboisson = "Latte(Petit)"
        prixboisson = prixlattePetit
        cafebesoin = cafelattePetit
        laitbesoin = laitlattePetit
      } else if (taillelate == 2) {
        nomboisson = "Latte(Moyen)"
        prixboisson = prixlatteMoyen
        cafebesoin = cafelatteMoyen
        laitbesoin = laitlatteMoyen
      } else {
        nomboisson = "Latte(Grand)"
        prixboisson = prixlatteGrand
        cafebesoin = cafelatteGrand
        laitbesoin = laitlatteGrand
      }
    }

    var choixsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
    while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4)) {
      println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
      choixsucre = readInt()
    }
    var prixsucre = 0.0
    var sucrebesoin = 0
    var nomsucre = ""

    if (choixsucre == 2) {
      prixsucre = 0.10
      sucrebesoin = 5
      nomsucre = "Peu (5g)"
    } else if (choixsucre == 3) {
      prixsucre = 0.20
      sucrebesoin = 10
      nomsucre = "Moyen (10g)"
    } else if (choixsucre == 4) {
      prixsucre = 0.30
      sucrebesoin = 15
      nomsucre = "Beaucoup (15g)"
    } else if (choixsucre ==1){
      nomsucre = "Sans sucre"
    }
    var laitsupplement = 0
    var prixlaitsupplement = 0.0
    var nomchoixlaitsupp = ""

    if (choixboisson == 2 || choixboisson == 3) {
      var choixlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
      while (!(choixlait == 1 || choixlait == 2)) {
        println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        nomchoixlaitsupp = "Oui"
        var choixdose = readLine("Combien de dose ?\n> ").toInt
        while (!(choixdose == 1 || choixdose == 2 || choixdose == 3)) {
          println("Combien de dose ?\n> ")
          choixdose = readInt()
        }
        laitsupplement = choixdose * 50
        prixlaitsupplement = choixdose * 0.05

        if (choixdose ==1 && milkStocks(machineId) >= laitbesoin + 0.05){
          choixdose = 1
          laitsupplement = 50
          prixlaitsupplement = 0.05
        } else if (choixdose == 2 && milkStocks(machineId) >= laitbesoin + 0.10) {
          choixdose = 2
          laitsupplement = 100
          prixlaitsupplement = 0.10
        } else if (choixdose == 3 && milkStocks(machineId) >= laitbesoin + 0.15) {
          choixdose = 3
          laitsupplement = 150
          prixlaitsupplement = 0.15
        }

      } else if(choixlait==2){
        nomchoixlaitsupp = "Non"
      }
    }

    if (coffeeStocks(machineId) < cafebesoin) {
      println("Boisson séléctionnée : " + nomboisson)
      println("Niveau de sucre: " + nomsucre)
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    } else if (milkStocks(machineId) < laitbesoin + laitsupplement) {
      println("Boisson séléctionnée : " + nomboisson)
      println("Niveau de sucre : " + nomsucre)
      println("Lait en supplément :" + nomchoixlaitsupp)
      println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    } else if (sugarStocks(machineId) < sucrebesoin) {
      println("Boisson séléctionée : " + nomboisson)
      println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    } else {
      val total = prixboisson + prixsucre + prixlaitsupplement
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n\n", prixboisson, prixsucre, prixlaitsupplement, total)

      val chaine = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var codepaiementtwint = ""
      println("Veuillez payer en utilisant Twint.")
      for (i <- 1 to 5) {
        val caractere = (Math.random() * chaine.length).toInt
        codepaiementtwint = codepaiementtwint + chaine(caractere)
      }
      println("Votre code de paiement est : " + codepaiementtwint + "\n(En attente de paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé.\nPréparation de votre boisson...")
      println("Votre " + nomboisson + " est prêt ! Bonne dégustation !\n")

      coffeeStocks(machineId) -= cafebesoin
      milkStocks(machineId) -= (laitbesoin + laitsupplement)
      sugarStocks(machineId) -= sucrebesoin

      return true
    }
  }
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN :")
      val codepin = readLine()
      if (codepin == machinePins(machineId)) {
        return true
      } else {
        tentatives -= 1
        println("Code PIN incorrect. " + tentatives + " tentative(s) restante(s).")
      }
    }
    false
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour pour du code PIN pour la machine " + (machineId + 1))
    println("Entrez un nouveau code PIN à 6 chiffres > ")
    val nouveauPIN = readLine()
    if (nouveauPIN.length == 6 && nouveauPIN.forall(_.isDigit)) {
      machinePins(machineId) = nouveauPIN
      println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
    } else {
      println("Le code PIN n'a pas été mis à jour.\nRetour au menu principal\n")
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    printf("Niveaux de stock actuels :\nPoudre de café : %d g\nSucre : %d g\nLait : %.2f L\n\n", coffeeStocks(machineId), sugarStocks(machineId), milkStocks(machineId)/1000.0)

    println("Entrez les quantités à ajouter : ")
    println("Poudre de café > ")
    coffeeStocks(machineId) += readInt()
    println("Sucre > ")
    sugarStocks(machineId) += readInt()
    println("Lait > ")
    val laitenlitres = readDouble()
    milkStocks(machineId) += (laitenlitres*1000).toInt

    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
  }
}
