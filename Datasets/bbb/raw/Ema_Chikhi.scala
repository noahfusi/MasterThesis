import io.StdIn._
import math._

object Main {

 //Déclaration de la méthode de validation du code PIN
  def validatePin(machineId:Int, machinePins:Array[String]):Boolean = {
    var tentative = 3
    while(tentative > 0) {
      var PinSaisi = readLine("Entrez le code PIN : \n > ").toString
      if (PinSaisi == machinePins (machineId-1)) {
        return true
      }else {
        tentative -=1
        println("Code PIN incorrect. " + tentative + " tentative(s) restante(s).")
      }
    }
    false
  }

 // Déclaration méthode Mise à jour du code PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var NewPin = "."
    println("\nMise à jour du code PIN pour la Machine " + machineId + ".")
    do{
      NewPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }while(NewPin.length != 6 || !NewPin.forall(_.isDigit))
    machinePins(machineId - 1) = NewPin
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }

 //Déclaration de la methode de reapprovisionnement des machines
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

   // Réinitialiser valeurs des variables fonctionnelles
    var ajouterstock = 0
    var ajoutcafe = 0
    var ajoutsucre = 0
    var ajoutlait = 0.000
    var ajoutlaitInt = 0
    var affichageLait = 0.000

   // Affichage des stocks
    println("\nStocks:\n" +
      "\tPoudre de café: " + coffeeStocks(machineId-1) + "g")
    affichageLait = milkStocks(machineId-1)/1000.toDouble
    printf("\tLait          : %.3fL \n",affichageLait)
    println("\tSucre         : " + sugarStocks(machineId-1) + "g\n")


   //Validation de "ajouterstock" -> Quel stock réapprovisionner?
    do{ var ajouterstockSaisi = readLine("Réapprovisionnement des stocks ...\n" +
      "Ajout :\n" +
      "\t1) Poudre de café\n"+
      "\t2) Lait\n" +
      "\t3) Sucre\n" +
      "> ")
      if(ajouterstockSaisi.forall(_.isDigit)){
        ajouterstock = ajouterstockSaisi.toInt
      }else{
        ajouterstock = 0 //Assigne une valeur hors plage pour déclancher la boucle
      }
    }while(ajouterstock < 1 || ajouterstock > 3)

   //Quantité à ajouter
    if(ajouterstock == 1){
      do {
        var ajoutcafeSaisi = readLine("Combien de grammes (g) de poudre de café ajoutez-vous?\n> ")
        if(ajoutcafeSaisi.forall(_.isDigit)){
          ajoutcafe = ajoutcafeSaisi.toInt
        }else{
          ajoutcafe = -1 //Assigne une valeur hors plage pour déclancher la boucle
        }
      }while(ajoutcafe < 0)
      coffeeStocks(machineId-1) += ajoutcafe
    }else if(ajouterstock == 2){
      do {
        ajoutlait = readLine("Combien de litres (L) de lait ajoutez-vous?\n> ").toDouble
        ajoutlait = 1000*ajoutlait
        ajoutlaitInt = ajoutlait.toInt
        var ajoutlaitString = ajoutlaitInt.toString
        if(ajoutlaitString.forall(_.isDigit)){
        }else{
          ajoutlaitInt = -1
        }
      }while(ajoutlaitInt < 0)
      milkStocks(machineId-1) += ajoutlaitInt
    }else{
      do {
        var ajoutsucreSaisi = readLine("Combien de grammes (g) de sucre ajoutez-vous?\n> ")
        if(ajoutsucreSaisi.forall(_.isDigit)){
          ajoutsucre = ajoutsucreSaisi.toInt
        }else{
          ajoutsucre = -1 //Assigne une valeur hors plage pour déclancher la boucle
        }
      }while(ajoutsucre < 0)
      sugarStocks(machineId-1) += ajoutsucre
    }

   //Afficher Stocks mis à jour
    println("\nNiveaux de stock mis à jour.\n" +
            "Voici les stocks à jour :\n" +
            "\tPoudre de café: " + coffeeStocks(machineId-1) + "g")
    affichageLait = milkStocks(machineId-1)/1000.toDouble
    printf("\tLait          : %.3fL \n",affichageLait)
    println("\tSucre         : " + sugarStocks(machineId-1) + "g")
  }

 //Déclaration de la methode Mode Client
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    //Initialisation des variables fonctionnelles
    var boissonSaisi = "."
    var boisson = 0
    var tailleLatteSaisi = "."
    var tailleLatte = 0
    var sucreSaisi = "."
    var sucre = 0
    var laitSaisi = "."
    var lait = 0
    var doselaitSaisi = "."
    var doselait = 5
    var QtotalLait = 0
    var cafeSuffisant = false
    var laitSuffisant = false
    var sucreSuffisant = false
    var SuffisanceStocks = false
    var boissonSelectionnee ="jsp"
    var niveauSucre = "jsp"
    var laitSupplementaire = 0
    var MessErreur = "jsp"
    var prixCafe = 0.00
    var prixSucre = 0.00
    var prixLait = 0.00
    var PrixTotal = 0.00
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    val chiffres = "0123456789"
    var typecaractere = 0
    var a = "/"
    var b = "/"
    var c = "/"
    var d = "/"
    var e = "/"
    var CodeTwint = "_____"

    //Choix de la boisson
    do {
      boissonSaisi = readLine("\nVeuillez sélectionner votre boisson :\n" +
        "1) Expresso - CHF 2.00 \n" +
        "2) Cappuccino - CHF 2.50 \n" +
        "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" +
        ">  ")
      if (boissonSaisi.forall(_.isDigit)) {
        boisson = boissonSaisi.toInt
      } else {
        boisson = 0 //Assigne une valeur hors plage pour déclancher la boucle
      }
    } while (boisson < 1 || boisson > 3)

    //Si a choisi un Latte, quelle taille?
    if (boisson == 3) {
      do {
        tailleLatteSaisi = readLine("\nVeuillez sélectionner votre taille de Latte: \n" +
          "1) Petit - CHF 2.70\n" +
          "2) Moyen - CHF 3.20\n" +
          "3) Grand - CHF 3.70\n>  ")
        if (tailleLatteSaisi.forall(_.isDigit)) {
          tailleLatte = tailleLatteSaisi.toInt
        } else {
          tailleLatte = 0 //Assigne une valeur hors plage pour déclancher la boucle
        }
      } while (tailleLatte < 1 || tailleLatte > 3)
    }

  //Personalisation de la boisson
   //Sucre?
    do {
      sucreSaisi = readLine("\nSouhaitez-vous ajouter du sucre?\n" +
        "1) Sans sucre\n" +
        "2) Peu (5g) - CHF 0.10\n" +
        "3) Moyen (10g) - CHF 0.20\n" +
        "4) Beaucoup (15g) - CHF 0.30\n>  ")
      if (sucreSaisi.forall(_.isDigit)) {
        sucre = sucreSaisi.toInt
      } else {
        sucre = 0 //Assigne une valeur hors plage pour déclancher la boucle
      }
    } while (sucre < 1 || sucre > 4)

    //Lait? (Attention -> uniquement si a choisi Cappuccino ou Latte!!)
    if (boisson == 2 || boisson == 3) {
      do {
        laitSaisi = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n" +
          "1) Oui\n" +
          "2) Non\n>  ")
        if (laitSaisi.forall(_.isDigit)) {
          lait = laitSaisi.toInt
        } else {
          lait = 0 //Assigne une valeur hors plage pour déclancher la boucle
        }
      } while (lait < 1 || lait > 2)
    }

   //Combien de dose de lait?
    if (lait == 1) {
      do {
        doselaitSaisi = readLine("Combien de dose de lait supplémentaire souhaitez-vous? (Max.3) (0.05 CHF/dose)\n>  ")
        if (doselaitSaisi.forall(_.isDigit)) {
          doselait = doselaitSaisi.toInt
        } else {
          doselait = 5 //Assigne une valeur hors plage pour déclancher la boucle
        }
      } while (doselait < 0 || doselait > 3)
    }


  //Suffisance des stocks
   //Suffisance de café?
    if (boisson == 1) {
      if (coffeeStocks(machineId - 1) >= 8) {
        cafeSuffisant = true
      }
    } else if (boisson == 2) {
      if (coffeeStocks(machineId - 1) >= 6) {
        cafeSuffisant = true
      }
    } else { // (donc c'est la boisson 3 --> latte mais quelle taille?)
      if (tailleLatte == 1) {
        if (coffeeStocks(machineId - 1) >= 6) {
          cafeSuffisant = true
        }
      } else if (tailleLatte == 2) {
        if (coffeeStocks(machineId - 1) >= 8) {
          cafeSuffisant = true
        }
      } else { //Donc Latte grande taille (3)
        if (coffeeStocks(machineId - 1) >= 12) {
          cafeSuffisant = true
        }
      }
    }

   //Suffisance de sucre?
    if (sucre == 1) {
      sucreSuffisant = true
    } else if (sucre == 2) {
      if (sugarStocks(machineId - 1) >= 5) {
        sucreSuffisant = true
      }
    } else if (sucre == 3) {
      if (sugarStocks(machineId - 1) >= 10) {
        sucreSuffisant = true
      }
    } else { //Donc c'est 15g de sucre (sucre =4)
      if (sugarStocks(machineId - 1) >= 15) {
        sucreSuffisant = true
      }
    }

   //Suffisance de lait?
    if (boisson == 1) {
      QtotalLait = 0
    } else if (boisson == 2) {
      QtotalLait = (50 * doselait) + 100
    } else if (boisson == 3 && tailleLatte == 1){
      QtotalLait = (50 * doselait) + 120
    } else if (boisson == 3 && tailleLatte == 2){
      QtotalLait = (50 * doselait) + 150
    } else if (boisson == 3 && tailleLatte == 3){
      QtotalLait = (50 * doselait) + 200
    }

    if((milkStocks(machineId-1) >= QtotalLait) || (boisson ==1)){
      laitSuffisant = true
    }else{
      laitSuffisant = false
    }

   //Suffisance générale(total) des stocks
    if((cafeSuffisant == true) && (sucreSuffisant == true) && (laitSuffisant == true)){
      SuffisanceStocks = true
    }else{
      SuffisanceStocks = false
    }


   //Nommer ce qui a été sélectionnée
    if(boisson == 1){
      boissonSelectionnee = "Expresso"
    } else if (boisson == 2) {
      boissonSelectionnee = "Cappuccino"
    } else if (boisson == 3 && tailleLatte == 1) {
      boissonSelectionnee = "Latte (Petit)"
    } else if (boisson == 3 && tailleLatte == 2) {
      boissonSelectionnee = "Latte (Moyen)"
    } else if (boisson == 3 && tailleLatte == 3) {
      boissonSelectionnee = "Latte (Grand)"
    }

    if (sucre == 1) {
      niveauSucre = "Sans sucre"
    } else if (sucre == 2) {
      niveauSucre = "Peu (5g)"
    } else if (sucre == 3) {
      niveauSucre = "Moyen (10g)"
    } else if (sucre == 4) {
      niveauSucre = "Beaucoup (15g)"
    }

    if (lait == 1) {
      laitSupplementaire = doselait
    } else if (lait == 2) {
      laitSupplementaire = 0
    }

   //Message d'erreur
    if (cafeSuffisant == false) {
      MessErreur = "Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\n" +
        "Veuillez choisir une autre machine."
    } else if (sucreSuffisant == false) {
      MessErreur = "Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.\n" +
        "Veuillez choisir une autre machine."
    } else if (laitSuffisant == false) {
      MessErreur = "Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\n" +
        "Veuillez choisir une autre machine."
    }

   //Affichage d'erreur(Attention, l'affichage change selon si c'est un cafe, cappuccino ou latte)
    if (SuffisanceStocks == false) {
      if (boisson == 1) {
        println("\nBoisson sélectionnée : " + boissonSelectionnee + '\n' +
          "Niveau de sucre : " + niveauSucre + '\n' +
          '\n' +
          "Erreur : " + MessErreur)
      } else if ((boisson == 2) || (boisson == 3)) {
        println("\nBoisson sélectionnée : " + boissonSelectionnee + '\n' +
          "Niveau de sucre : " + niveauSucre + '\n' +
          "Lait en supplément : " + laitSupplementaire + '\n' +
          '\n' +
          "Erreur : " + MessErreur)
      }
    }//Fin du if stocks sont insuffisants

   //Si les stocks sont suffisant
    if(SuffisanceStocks == true) {

    //Prix de la commande
     //Prix Café
      if (boisson == 1) {
        prixCafe = 2.00
      } else if (boisson == 2) {
        prixCafe = 2.50
      } else if ((boisson == 3) && (tailleLatte == 1)) {
        prixCafe = 2.70
      } else if ((boisson == 3) && (tailleLatte == 2)) {
        prixCafe = 3.20
      } else if ((boisson == 3) && (tailleLatte == 3)) {
        prixCafe = 3.70
      }

     //Prix Sucre
      if (sucre == 1) {
        prixSucre = 0.00
      } else if (sucre == 2) {
        prixSucre = 0.10
      } else if (sucre == 3) {
        prixSucre = 0.20
      } else { // Sucre=4
        prixSucre = 0.30
      }

     //Prix Lait
      prixLait = laitSupplementaire * 0.05

     //Prix total de la commande
      PrixTotal = prixCafe + prixSucre + prixLait


     //Générer le code du paiement twint
      typecaractere = 0
      a = "/"
      b = "/"
      c = "/"
      d = "/"
      e = "/"
      CodeTwint = "_____"

      typecaractere = (math.random()*2).toInt
      if(typecaractere == 0){
        a = alphabet((math.random() * alphabet.length - 1).toInt).toString
      }else{
        a = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random()*2).toInt
      if(typecaractere == 0) {
        b = alphabet((math.random() * alphabet.length - 1).toInt).toString
      }else{
        b = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random()*2).toInt
      if(typecaractere == 0) {
        c = alphabet((math.random() * alphabet.length - 1).toInt).toString
      }else{
        c = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random()*2).toInt
      if(typecaractere == 0) {
        d = alphabet((math.random() * alphabet.length - 1).toInt).toString
      }else{
        d = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random()*2).toInt
      if(typecaractere == 0) {
        e = alphabet((math.random() * alphabet.length - 1).toInt).toString
      }else{
        e = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      CodeTwint = a + b + c + d + e




     //Affichage de la commande + préparation + Twint
      if (boisson == 1) {
        println("\nBoisson sélectionnée : " + boissonSelectionnee + '\n' +
          "Niveau de sucre : " + niveauSucre)
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixCafe, prixSucre, PrixTotal)
        println("Veuillez payer en utilisant Twint.\n" +
          "Votre code de paiement est : " + CodeTwint + '\n' +
          "(En attente de paiement)\n")
        // Attendre 5000 millisecondes (5 secondes)
        Thread.sleep(5000)
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson ...\n" +
          "Votre Expresso est prêt ! Bonne dégustation !")
      } else if (boisson == 2) {
        println("\nBoisson sélectionnée : " + boissonSelectionnee + '\n' +
          "Niveau de sucre : " + niveauSucre + '\n' +
          "Lait supplémentaire : " + laitSupplementaire)
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixCafe, prixSucre, prixLait, PrixTotal)
        println("Veuillez payer en utilisant Twint.\n" +
          "Votre code de paiement est : " + CodeTwint + '\n' +
          "(En attente de paiement)\n")
        // Attendre 5000 millisecondes (5 secondes)
        Thread.sleep(5000)
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson ...\n" +
          "Votre Cappuccino est prêt ! Bonne dégustation !")
      } else {
        println("\nBoisson sélectionnée : " + boissonSelectionnee + '\n' +
          "Niveau de sucre : " + niveauSucre + '\n' +
          "Lait supplémentaire : " + laitSupplementaire)
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixCafe, prixSucre, prixLait, PrixTotal)
        println("Veuillez payer en utilisant Twint.\n" +
          "Votre code de paiement est : " + CodeTwint + '\n' +
          "(En attente de paiement)\n")
        // Attendre 5000 millisecondes (5 secondes)
        Thread.sleep(5000)
        println("Paiement confirmé.\n" +
          "Préparation de votre boisson ...\n" +
          "Votre Latte est prêt ! Bonne dégustation !")
      }


    //Retirer du stock
     //Retirer de la poudre de café du stock
      if(boisson == 1){
        coffeeStocks(machineId-1) -= 8
      }else if(boisson == 2){
        coffeeStocks(machineId-1) -= 6
      }else if(boisson == 3 && tailleLatte == 1){
        coffeeStocks(machineId-1) -= 6
      }else if(boisson == 3 && tailleLatte == 2){
        coffeeStocks(machineId-1) -= 8
      }else if(boisson == 3 && tailleLatte == 3){
        coffeeStocks(machineId-1) -= 12
      }

     //Retirer du sucre du stock
      if(sucre == 2){
        sugarStocks(machineId-1) -= 5
      }else if(sucre == 3){
        sugarStocks(machineId-1) -= 10
      }else if(sucre == 4){
        sugarStocks(machineId-1) -= 15
      }

     //Retirer du lait du stocks
      milkStocks(machineId-1) -= QtotalLait

    }//Fin du if Stocks sont suffisant

    if(SuffisanceStocks == false){
      return false
    }else{ // Boisson réussi - servie - argent encaissé et stock retirés
      return true
    }
  }


  def main(args: Array[String]): Unit = {

   // Déclaration de variables
    val nbMachines = 5
    var machinePins = Array[String]("434343", "434343", "434343", "434343", "434343")
    var coffeeStocks = Array[Int](50, 50, 50, 50, 50)
    var sugarStocks = Array[Int](30, 30, 30, 30, 30)
    var milkStocks = Array[Int](500, 500, 500, 500, 500)
    var ModeRéussi = false


  //Boucle du programme Nospresso
    do {
     // Réinicialise variable
      var machineIdSaisi = "."
      var machineId = 0
      var NewPin = "//////"
      var modeSaisi = "."
      var mode = 0
      var adminSaisi = "."
      var admin = 0
      ModeRéussi = false

     // Affichage du menu - Choix du mode
      do {
        modeSaisi = readLine("\n\n\n\tNospresso Café\n" +
          "Veuillez sélectionner votre mode:\n" +
          "1) Client\n" +
          "2) Admin\n" +
          "3) Quitter\n>  ")
        if (modeSaisi.forall(_.isDigit)) {
          mode = modeSaisi.toInt
        } else {
          mode = 0 //Assigne une valeur hors plage pour déclancher la boucle
        }
      } while (mode < 1 || mode > 3)


      if (mode == 1) { // Mode Client
        do {
          // Quelle machine utiliser? Validation de l'ID de la machine
          do {
            machineIdSaisi = readLine("\nMachine séléctionnée (1-5)  > ")
            if (machineIdSaisi.forall(_.isDigit)) {
              machineId = machineIdSaisi.toInt
            } else {
              machineId = 0 //Assigne une valeur hors plage pour déclancher la boucle
            }
          } while (machineId < 1 || machineId > nbMachines)
          ModeRéussi = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } while (ModeRéussi == false)
      } else if (mode == 2) { // Mode Admin
       // Quelle machine utiliser? Validation de l'ID de la machine
        do {
          machineIdSaisi = readLine("\nMachine séléctionnée (1-5)  > ")
          if (machineIdSaisi.forall(_.isDigit)) {
            machineId = machineIdSaisi.toInt
          } else {
            machineId = 0 //Assigne une valeur hors plage pour déclancher la boucle
          }
        } while (machineId < 1 || machineId > nbMachines)

       // Validation du code PIN (Utilisation de la méthode validatePin)
        if (validatePin(machineId, machinePins)) {
          println("Accès accordé à la Machine " + machineId + ".\n")

         //Réapprovisionner ou changer de code PIN?
          do {
            adminSaisi = readLine("Que souhaitez-vous faire?:\n" +
              "1) Réaprovisionner les stocks\n" +
              "2) Changer le code PIN\n>  ")
            if (adminSaisi.forall(_.isDigit)) {
              admin = adminSaisi.toInt
            } else {
              admin = 0 //Assigne une valeur hors plage pour déclancher la boucle
            }
          } while (admin < 1 || admin > 2)

         //Admin Mode
          if (admin == 1) { //Réapprovisionner
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else { // Mise à jour du code PIN (Utilisation de la méthode updatePin)
            updatePin(machineId, machinePins)
          }

          ModeRéussi = true
        } else {
          println("\nTrop de tentatives échouées. Fin du programme.")
        }
      }else{ // Mode Quitter
        println("\nVous avez quitté le programme Nospresso ...")
      }
    // Attendre 5000 millisecondes (5 secondes)
     Thread.sleep(5000)
    }while(ModeRéussi == true)
  }
}