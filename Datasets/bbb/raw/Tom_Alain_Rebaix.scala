import io.StdIn._

object Main {
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var code = " "
    var essaie = 0
    var tentative = 2

    while (essaie < 3) {
      print(" Veuillez saisir le mot-de-passe de la machine :\n >")
      code = readLine()
      essaie += 1
      if (machinePins(machineId) == code) {
        println("mot-de-passe vailde")
        essaie = 4
        return true

      } else if (essaie < 3 && machinePins(machineId) != code) {
        println("code invalide veuillez réessayer. Tentative restante : " + tentative  )
        tentative -= 1

      }
    }
    println("Maximun de tentaive atteint.")
    println("La machine est bloqué")
    sys.exit(0)
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var nouveaucode = " "

    var changement = false
    println("Changement de mot-de-passe pour la machine " + (machineId + 1))

    while (!changement) {
      print("Entrez le nouveau mot-de-passe à 6 chiffres\n > ")
      nouveaucode = readLine()
      var tableaucode = nouveaucode.toCharArray

      if (tableaucode.length == 6){
        println("Le mot-de-passe a été changé avec succès")
        return  machinePins(machineId) = nouveaucode
      }else {
        println ("Le mot-de passe doit contenir 6 caractères ")
      }

    }

  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var choix = 0

    while (choix < 1 || choix > 3) {
      print("Veuillez sélectionner une boisson: \n1) Expresso - 2.00 CHF \n2) Cappucino - 2.50 CHF \n3) Latte - Petit 2.70 CHF, Moyen 3.20 CHF, Grand 3.70 CHF \n >")
      choix = readInt()
    }
    var laitboucle = false
    var choixlait = 0
    var lait = 0
    var sucre = 0
    var prixcafe = 0.0
    var choixlatte = 0
    var prix = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var aleatoire = 0
    if (choix == 3) {
      print("1) petit - 2.70 CHF  \n2) moyen - 3.20 CHF \n3) grand - 3.70 CHF \n >") // choix des différents latte
      choixlatte = readInt()
     }
    if (choix == 1) {
      laitboucle = true
    }
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) { // choix sucre
      print("Quel quantité de sucre :\n 1) pas de sucre\n 2) peu de sucre, 5g - 0.10 CHF  \n 3) moyen, 10g - 0.20 CHF \n 4) Beaucoup, 15g - 0.30 CHF \n >")
      sucre = readInt()
    }

    while (!laitboucle) {
      if (choix == 2 || choix == 3) {
        print("Vous voulez ajouter une dose supplémentaire de lait ? \n1) oui \n2) non \n >")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        while (lait != 1 && lait != 2 && lait != 3) { // choix lait
          laitboucle = true
          print("Quel quantité de lait ? (maximun 3 doses)\n >")
          lait = readInt()
        }
      } else if (choixlait == 2) {
        laitboucle = true
      }}





    if (choix == 1) {
      laitboucle = true
      if (coffeeStocks(machineId) >= 8) {
        coffeeStocks(machineId) -= 8
        prixcafe = 2.00
        println("Boisson sélectionée: Expresso")
      } else {
        println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n")
        return true
      }
    } else if (choix == 2 && (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100)) { // choix cappucino
      coffeeStocks(machineId) = coffeeStocks(machineId) - 6
      milkStocks(machineId) -= 100
      println("Boisson sélectionée: Cappucino")
      prixcafe = 2.50

    } else if (coffeeStocks(machineId) < 6 && choix == 2) {
      println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    } else if (milkStocks(machineId) < 100 && choix == 2) {
      println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false}

    else if (choixlatte == 1 && (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120)) { //lattes
      coffeeStocks(machineId) -= 6
      milkStocks(machineId) -= 120
      prixcafe = 2.70
      println("Boisson sélectionée: Petit latte")

    }

    if (choixlatte == 2 && (coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150)) {
      coffeeStocks(machineId) -= 8
      milkStocks(machineId) -= 150
      println("Boisson sélectionée: Moyen latte")
      prixcafe = 3.20
      prix = prixcafe

    }
    else if (choixlatte == 3 && (coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200)) {
      coffeeStocks(machineId) -= 12
      milkStocks(machineId) -= 200
      prixcafe = 3.70
      println("Boisson sélectionée: Grand latte ")

    }
    else if ((choixlatte == 3 && coffeeStocks(machineId) < 12) || (choixlatte == 2 && coffeeStocks(machineId) < 8) || (choixlatte == 1 && coffeeStocks(machineId) < 6)) {
      println("Erreur: Quantité de poudre de café insuffisante\n Veuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    }
    else if ((choixlatte == 1 && milkStocks(machineId) < 120) || (choixlatte == 2 && milkStocks(machineId) < 150) || (choixlatte == 3 && milkStocks(machineId) < 200)) {
      println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    }


    if (sucre == 4 && sugarStocks(machineId) < 15 || sucre == 3 && sugarStocks(machineId) < 10 || sucre == 2 && sugarStocks(machineId) < 5) {
      println("Erreur: Quantité de sucre insuffisante\nVeuillez sélectionner une plus petite quantité de sucre ou contrôler le stock en mode admin\n ")
      return false
    }

    if (sucre == 1) { // sucre
      println("Quantité de sucre : pas de sucre")
    }
    else if (sucre == 2 && sugarStocks(machineId) >= 5) {
      sugarStocks(machineId) -= 5
      prixsucre = 0.10
      println("Quantité de sucre : peu (5g)")
    }
    else if (sucre == 3 && sugarStocks(machineId) >= 10) {
      sugarStocks(machineId) -= 10
      prixsucre = 0.20
      println("Quantité de sucre : moyen (10g) ")
    }

    else if (sucre == 4 && sugarStocks(machineId) >= 15) {
      sugarStocks(machineId) -= 15
      prixsucre = 0.30
      println("Quantité de sucre : beaucoup (15g)")
    }

    else if (choixlait == 2) { // lait
      println("Lait: pas de dose")
    }

   if (lait == 1 && milkStocks(machineId) >= 50) {
      milkStocks(machineId) -= 50
      prixlait = 0.05
      println("Lait: Une dose ")
    }

    else if (lait == 2 && milkStocks(machineId) >= 100) {
      milkStocks(machineId) -= 100
      prixlait = 0.1
      println("Lait: deux dose")
    }
    else if (lait == 3 && milkStocks(machineId) >= 150) {
      println("Lait: trois doses")
      prixlait = 0.15
      milkStocks(machineId) -= 150
    }
    else if ((lait == 3 && milkStocks(machineId) < 100) || (lait == 2 && milkStocks(machineId) < 100) || (lait == 1 && milkStocks(machineId) < 100)) {
      println("Erreur: Quantité lait insuffisante pour la dose sélectionné \nVeuillez sélectionner une plus petite quantité de dose de lait ou contrôler le stock en mode admin\n ")
    return false
    }

    if (prixcafe > 0.00 || prixlait > 0.00 || prixsucre > 0.00) { // le prix
      prix = prixcafe + prixlait + prixsucre
    }
    if (prixsucre > 0.00 && prixlait > 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) + %.2f CHF (lait)\n", prix, prixcafe, prixsucre, prixlait)
    }
    if (prixsucre > 0.00 && prixlait == 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) \n", prix, prixcafe, prixsucre)
    }
    if (prixsucre == 0.00 && prixlait > 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (lait) \n", prix, prixcafe, prixlait)
    }
    if (prixsucre == 0.00 && prixlait == 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF (café) \n", prixcafe)
    }

    println("\nVeuillez procéder au paiement avec Twint")
    print("Votre code pour le paiment est : ")
    for (i <- 0 to 4) {
      while ((aleatoire < 49) || (aleatoire > 90) || ((aleatoire < 65) && (aleatoire > 57))) {
        aleatoire = (Math.random() * 90).toInt
      }
      print(aleatoire.toChar)
      aleatoire = 0
    }
    println("\nEn attente de la confirmation du paiement...")
    Thread.sleep(3000)
    println("Paiement validé ! Merci pour votre commande")
    println("\nVotre commande est cour de préparation...")

    if (choix == 1) {
      println("Voici votre expresso ! ")
      return true
    }
    if (choix == 2) {
      println("Voici votre cappuccino !")
      return true
    }
    if (choix == 3) {
      println("Voici votre latte !")
      return true

    }
    else {
      return false
    }
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit  = {
    var laitaffiche = milkStocks(machineId)/ 1000.0
    println ("Quantités de stock:\n1) Poudre de café: " + coffeeStocks(machineId)  +"g \n2) Sucre: " +sugarStocks(machineId)  + "g\n3) Lait: " + laitaffiche + "L" )

    var poudreajoute =  -1
    while (poudreajoute < 0){
      print(" Quel quantité de poudre à café à rajouter \n >")
      poudreajoute = readInt()
    }
    coffeeStocks(machineId) +=   poudreajoute

    var sucreajoute = -1
    while(sucreajoute < 0){
      print(" Quel quantité de sucre à rajouter \n >")
      sucreajoute = readInt()
    }
    sugarStocks(machineId) += sucreajoute

    var laitajoute = -1.0
    while ( laitajoute < 0 ){
      print(" Quel quantité de lait à rajouter \n >")
      laitajoute = readDouble() * 1000

    }
    var laitajoute2 = (laitajoute ).toInt
    milkStocks(machineId) += laitajoute2
    laitaffiche = milkStocks(machineId)/ 1000.0
    println ("réapprovisionnement des stocks ...")
    println ("Quantités de stock:\n1) Poudre de café: " + coffeeStocks(machineId) + "g \n2) Sucre: " + sugarStocks(machineId) + "g\n3) Lait: " +  laitaffiche + "L" )

  }


  def main(args: Array[String]): Unit = {
    var stockcaffe = Array.fill(5)(50)
    var stocklait = Array.fill(5)(500)
    var sucre = Array.fill(5)(30)
    var entre = 0

    var codemachines = Array.fill(5)("434343")

    while (entre != 1 && entre != 2 && entre != 3) {
      println("Sasie non-valide, veuillez réessayer \n ")
      print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n>")
      entre = readInt()}

    while (entre == 1 || entre == 2) {
      var choixmachine = false
      var nbrmachines = 0
      var chagementmdp = 0
      if (entre == 1) {
        while (!choixmachine) {
          print("Veuillez séléectionner une machine entre 1 et 5  \n >")

          nbrmachines = readInt()
          if (nbrmachines <= 5 && nbrmachines >= 1) {
            nbrmachines -= 1
            choixmachine = true
          } else {
            println("saisie non-valide")
          }
        }
        serveClient(nbrmachines, stockcaffe, sucre, stocklait)
        print("\n\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        entre = readInt()
        choixmachine = false
      }

      if (entre == 2) {
        while (!choixmachine) {
          print("Veuillez séléectionner une machine entre 1 et 5  \n >")
          nbrmachines = readInt() - 1
          if (nbrmachines <= 4 && nbrmachines >= 0) {
            choixmachine = true
          } else {
            println("saisie non-valide")
          }
        }

        validatePin(nbrmachines, codemachines)

        while (chagementmdp <= 0 || chagementmdp > 2) {
          print("Que voulez-vous faire ? \n1) Changer de mot-de-passe \n2) Vérifier les stocks \n>")
          chagementmdp = readInt()
        }
        if (chagementmdp == 1) {
          updatePin(nbrmachines, codemachines)
        }else{
          restockMachine(nbrmachines, stockcaffe, sucre,  stocklait)
        }

        println("Retour au meunu...  ")
        print("\n\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        entre = readInt()

      }

    while (entre != 1 && entre != 2 && entre != 3) {
      println("Sasie non-valide, veuillez réessayer \n ")
      print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n>")
      entre = readInt()
    } }
  }
}
