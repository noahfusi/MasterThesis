import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  //initialisation du nombre de machines
  val nbMachines = 5
  //quantités initiales des ingrédients
  var coffeeStocks = Array.fill(nbMachines)(50)
  var milkStocks = Array.fill(nbMachines)(0.5)
  var sugarStocks = Array.fill(nbMachines)(30)

  //initialisation du code admin
  var machinePins = Array.fill(nbMachines)("434343")
  val mdp = "434343"

  var machineId = 0
  var EssaiCode = ""
  var NewCode = ""

  //initialisation des prix et quantités
  var prixcafe = 0.0
  var prixsucre = 0.0
  var prixlait = 0.0
  var taille = 0
  var doselait = 0
  var supplementlait = 0
  var supplementsucre = 0

  //variables de réapprovisionnement des stocks
  var ajoutcafe = 0
  var ajoutlait = 0.0
  var ajoutsucre = 0

  //quantités de café, sucre et lait nécessaire à la confection de la boisson
  var cafeNecessaire = 0
  var laitNecessaire = 0.0
  var sucreNecessaire = 0
  //initialisation des caractères pour le code
  var codepaie = ""
  val caractere = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  var NbeTentative = 3

  //initialisation des méthodes
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    while (NbeTentative > 0) {
      println("Entrez le code PIN :")
      EssaiCode = readLine("> ")
      if (EssaiCode == machinePins(machineId-1)) {
        println("Accès accordé.")
        NbeTentative = 3
        return true
      } else {
        NbeTentative -= 1
        if (NbeTentative > 0) {
          println(s"Code PIN incorrect. $NbeTentative tentatives restantes.")
        } else {
          println("Trop de tentatives échouées. Fin du programme.")

        }
      }
    }
    NbeTentative = 3
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + machineId)
    NewCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    if (NewCode.length == 6 && NewCode.forall(_.isDigit)) {
      machinePins(machineId - 1) = NewCode
      println("Code PIN enregistré avec succès.")
    } else {
      while (NewCode.length != 6 || !NewCode.forall(_.isDigit)) {
        NewCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        if (NewCode.length == 6 && NewCode.forall(_.isDigit)) {
          machinePins(machineId - 1) = NewCode
          println("Code PIN enregistré avec succès.")
        }
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
    //choix des boissons du client
    println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var boisson = readLine(">").toInt
    //test de la valeur de boisson
    while (boisson < 1 || boisson > 3) {
      println("cette boisson n'est pas valide")
      boisson = readLine(">").toInt
    }
    //choix de la taille du Latte
    if (boisson == 3) {
      println("1) Petit\n2) Moyen\n3) Grand")
      taille = readLine(">").toInt
      while (taille < 1 || taille > 3) {
        println("la taille n'est pas valide")
        taille = readLine(">").toInt
      }
    }
    // ajout sucre
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    supplementsucre = readLine(">").toInt
    while (supplementsucre < 1 || supplementsucre > 4) {
      println("cette quantité n'est pas valide")
      supplementsucre = readLine(">").toInt
    }

    //choix du supplément en lait, restraint au cappucino et latte
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
      supplementlait = readLine(">").toInt
      while (supplementlait < 0 || supplementlait > 2) {
        println("supplément de lait invalide")
        supplementlait = readLine(">").toInt
      }

      if (supplementlait == 1) {
        println("Combien de doses? - 1 à 3")
        doselait = readLine(">").toInt
      }

      while (doselait < 1 || doselait > 3) {
        if (doselait < 1 || doselait > 3) {
          println("ce nombre de dose de lait n'est pas valide")
          doselait = readLine(">").toInt
        } else {
          println("combien de dose? - 1 à 3")
          doselait = readLine(">").toInt
        }
      }
    }

    //afficher le choix de la boisson et déterminer les quantités nécessaire à sa confection
    if (boisson == 1) {
      println("Boisson sélectionnée : Expresso")
      cafeNecessaire = 8
      prixcafe += 2.0
    }
    if (boisson == 2) {
      println("Boisson sélectionnée : Cappuccino")
      cafeNecessaire = 6
      laitNecessaire = 0.1
      prixcafe += 2.5
    }

    if (boisson == 3) {
      if (taille == 1) {
        println("Boisson sélectionnée : Latte (Petit)")
        cafeNecessaire = 6
        laitNecessaire = 0.12
        prixcafe += 2.7
      }
      if (taille == 2) {
        println("Boisson sélectionnée : Latte (Moyen)")
        cafeNecessaire = 8
        laitNecessaire = 0.15
        prixcafe += 3.2
      }
      if (taille == 3) {
        println("Boisson sélectionnée : Latte (Grand)")
        cafeNecessaire = 12
        laitNecessaire = 0.2
        prixcafe += 3.7
      }
    }

    //afficher et déterminer la quantité de sucre
    if (supplementsucre == 1) {
      println("Niveau de sucre : Sans sucre")
    }
    if (supplementsucre == 2) {
      println("Niveau de sucre : Peu (5g)")
      sucreNecessaire = 5
      prixsucre += 0.1
    }
    if (supplementsucre == 3) {
      println("Niveau de sucre : Moyen (10g)")
      sucreNecessaire = 10
      prixsucre += 0.2
    }
    if (supplementsucre == 4) {
      println("Niveau de sucre : Beaucoup (15g)")
      sucreNecessaire = 15
      prixsucre += 0.3
    }

    //Afficher la dose de lait
    if (supplementlait == 2) {
      println("Lait en supplément: Non")
    }
    if (doselait == 1) {
      println("Lait en supplément: 1 dose")
      laitNecessaire += 0.05
      prixlait += 0.05
    }
    if (doselait == 2) {
      println("Lait en supplément: 2 dose")
      laitNecessaire += 0.1
      prixlait += 0.1
    }
    if (doselait == 3) {
      println("Lait en supplément: 3 dose")
      laitNecessaire += 0.15
      prixlait += 0.15
    }

    //indiquer si la quantité est insuffisante
    if (coffeeStocks(machineId-1) < cafeNecessaire || sugarStocks(machineId-1) < sucreNecessaire || milkStocks(machineId-1) < laitNecessaire) {
      if (coffeeStocks(machineId-1) < cafeNecessaire) {
        println("Erreur : Quantité de café insuffisante pour préparer\nla boisson sélectionnée.")
      }
      else if (sugarStocks(machineId-1) < sucreNecessaire) {
        println("Erreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.")
      }
      else if (milkStocks(machineId-1) < laitNecessaire) {
        println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.")
      }
      if (taille == 2 || taille == 3) {
        println("Veuillez choisir une taille plus petite ou essayer\nune autre boisson.")
      } else {
        println("Veuillez choisir une autre boisson ou v´erifier les\nstocks en mode Admin.")
      }
      return false
    }

    //affichage du prix total
    var prixtotal: Double = math.round((prixcafe + prixlait + prixsucre) * 100) / 100.0

    if (coffeeStocks(machineId-1) >= cafeNecessaire && sugarStocks(machineId-1) >= sucreNecessaire && milkStocks(machineId-1) >= laitNecessaire) {
      printf("Prix total : CHF %.2f", prixcafe)
      if (supplementsucre > 1 && supplementsucre < 5) {
        printf(" + CHF %.2f", prixsucre)
      }
      if (doselait > 0 && doselait < 4) {
        printf(" + CHF %.2f", prixlait)
      }
      printf(" = CHF %.2f", prixtotal)

      //génération du code de paiement
      for (_ <- 1 to 5) {
        val randomChar = caractere(Random.nextInt(caractere.length))
        codepaie += randomChar
      }
      //paiement
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codepaie + "\n(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé. \nen attente de votre boisson ...")
      Thread.sleep(5000)

      //message de fin
      if (boisson == 1) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
      }
      if (boisson == 2) {
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
      }
      if (boisson == 3) {
        println("Votre Latte est prêt ! Bonne dégustation !")
      }

      if (coffeeStocks(machineId-1) >= cafeNecessaire && sugarStocks(machineId-1) >= sucreNecessaire && milkStocks(machineId-1) >= laitNecessaire) {
        //réduction des stocks
        coffeeStocks(machineId-1) -= cafeNecessaire
        milkStocks(machineId-1) -= laitNecessaire
        sugarStocks(machineId-1) -= sucreNecessaire
        //réinitialisation des valeurs
        prixcafe = 0
        prixsucre = 0
        prixlait = 0
        prixtotal = 0.0
        cafeNecessaire = 0
        laitNecessaire = 0
        sucreNecessaire = 0
        supplementsucre = 0
        doselait = 0
        taille = 0
        supplementlait = 0
        codepaie = ""
      }
       true
    } else {
       false
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("Niveaux de stock actuels :\n  Poudre de café : " + coffeeStocks(machineId-1) + "g  \n Sucre :  " + sugarStocks(machineId-1) + "g \n  Lait : " + milkStocks(machineId-1)+"L")

    println("Entrez les quantités à ajouter :")
    ajoutcafe = readLine("Poudre de café > ").toInt
    ajoutsucre = readLine("Sucre         > ").toInt
    ajoutlait = readLine("Lait          > ").toDouble
    coffeeStocks(machineId-1) += ajoutcafe
    sugarStocks(machineId-1) += ajoutsucre
    milkStocks(machineId-1) += ajoutlait
    //réinitialisation des ajouts
    ajoutcafe = 0
    ajoutlait = 0.0
    ajoutsucre = 0
    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }


  def main(args: Array[String]): Unit = {
    //initialisation de la machine
    var boucle = true
    while (boucle) {
      //choix de la machine
      machineId = readLine("Machine sélectionnée (1-5) >").toInt
      while(machineId>5 || machineId<1){
        println("le la machine n'existe pas")
        machineId = readLine("Machine sélectionnée (1-5) >").toInt
      }
      // affichage de la machine
      println("           Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var mode = readLine(">").toInt
      //assurer que le mode sélectionné soit valide
      while (mode < 1 || mode > 3) {
        println("le mode n'existe pas")
        mode = readLine(">").toInt
      }
      if (mode == 1){
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      }
      if(mode == 2){
        var BoucleAdmin = true

          if(validatePin(machineId, machinePins)){
            while(BoucleAdmin){
            println("que voulez vous faire? \n1) changer code pin\n2) changer les stocks\n3) Quitter")
            var choixaAdmin =readLine(">").toInt
            while (choixaAdmin < 1 || choixaAdmin > 3) {
              println("le choix n'existe pas")
              choixaAdmin = readLine(">").toInt
            }
            if (choixaAdmin == 1) {
              updatePin(machineId: Int, machinePins: Array[String])
              BoucleAdmin = false
            }
            if(choixaAdmin==2){
                restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double])
              BoucleAdmin = false
            }
            else{
            if (choixaAdmin==3){
              BoucleAdmin=false
           }
          }
          }

        }else{
            boucle=false
          }
      }
      if (mode==3){
        println("bonne journée!")
        boucle=false
      }
    }
  }

}