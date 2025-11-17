import io.StdIn._
import scala.io.Source
import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer

class Machine(id: Int, pincode: Int, milk: Int, sugar: Int, coffee: Int) {
  var idMachine = id
  var pincodeMachine = pincode
  var milkMachine = milk
  var sugarMachine = sugar
  var coffeeMachine = coffee


  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "café") {
      Machine.this.coffeeMachine += amount
    } else if (ingredient == "sucre") {
      Machine.this.sugarMachine += amount
    } else {
      Machine.this.milkMachine += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if(ingredient == "café") {
      if (Machine.this.coffeeMachine >= amount) {
        Machine.this.coffeeMachine -= amount
        return true
      } else {
        return false
      }
    }else if(ingredient == "sucre"){
      if(Machine.this.sugarMachine >= amount){
        Machine.this.sugarMachine -= amount
        return true
      }else{
        return false
      }
    }else{
      if(Machine.this.milkMachine >= amount){
        Machine.this.milkMachine -= amount
        return true
      }else{
        return false
      }
    }
  }
}


object Main {

  var stocksuffisant = true
  var ingredient = "kk"
  var amountSaisi = "kk"
  var amount = 0


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    println("\n\nChargement des machines depuis " + filename + "...")
    try {
      val fr = Source.fromFile(filename)
      val lignefr = fr.getLines().toArray
      var index = 0
      for (ligne <- lignefr.drop(1)) {  //Laisse tombé l'en-tête
        val Array(pincodeMachine, milkMachine, sugarMachine, coffeeMachine) = ligne.split(",")
        val machine = new Machine(index + 1, pincodeMachine.toInt, milkMachine.toInt, sugarMachine.toInt, coffeeMachine.toInt)
        machines += machine
        println("\nMachine " + machine.idMachine + " chargée:\nID: " + machine.idMachine + "\nPin: " + machine.pincodeMachine)
        printf("Lait: " + "%.3f L", machine.milkMachine.toDouble/1000)
        println("\nSucre: " + machine.sugarMachine + " g\nCafé: " + machine.coffeeMachine + " g")
        index += 1
      }
      println("\n" + machines.length + " machine(s) chargée(s) avec succès.")
      fr.close
      return machines
    } catch {
      case ex: java.io.FileNotFoundException
      => println("Erreur: Fichier " + filename + " introuvable. Vérifiez le chemin d'accès et réessayez.")
        new ArrayBuffer[Machine]()

      case ex: java.io.IOException
      => println("Erreur dans la lecture du fichier.")
        new ArrayBuffer[Machine]()
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val fw = new PrintWriter("machines.csv.")
    fw.println("PINCODE,MILK,SUGAR,COFFEE")
    for(i <- 0 to machines.length-1){
      fw.println(machines(i).pincodeMachine + "," + machines(i).milkMachine + "," + machines(i).sugarMachine + "," +
        machines(i).coffeeMachine)
    }
    fw.close
  }


 //Déclaration de la méthode de validation du code PIN
  def validatePin(id:Int, pincodeMachine: String):Boolean = {
    var tentative = 3
    var PinSaisi ="..."
    while(tentative > 0) {
      PinSaisi = readLine("Entrez le code PIN : \n > ").toString
      if (PinSaisi == pincodeMachine) {
        return true
      }else {
        tentative -=1
        println("Code PIN incorrect. " + tentative + " tentative(s) restante(s).")
      }
    }
    return false
  }

  // Déclaration méthode Mise à jour du code PIN
  def updatePin(id: Int): String = {
    var NewPin = "."
    println("\nMise à jour du code PIN pour la Machine " + id + ".")
    do{
      NewPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }while(NewPin.length != 6 || !NewPin.forall(_.isDigit))
    return NewPin
  }

 //Déclaration de la methode Mode Client
  def serveClient(id: Int, coffeeStocks: Int,
                  sugarStocks: Int, milkStocks: Int): Boolean = {

    val filename = "machines.csv."
    val machines = loadcsv("machines.csv.")

   //Initialisation des variables fonctionnelles
    var idSaisi = "."
    var id = 0
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
    var boissonSelectionnee = "jsp"
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
    var amountcafe = 0
    var amountsucre = 0
    var amountlait = 0

   // Quelle machine utiliser? Validation de l'ID de la machine
    do {
      idSaisi = readLine("\nMachine séléctionnée (1-5)  > ")
      if (idSaisi.forall(_.isDigit)) {
        id = idSaisi.toInt
      } else {
        id = 0 //Assigne une valeur hors plage pour déclancher la boucle
      }
    } while (id < 1 || id > machines.length)

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
      if (coffeeStocks >= 8) {
        cafeSuffisant = true
      }
    } else if (boisson == 2) {
      if (coffeeStocks >= 6) {
        cafeSuffisant = true
      }
    } else { // (donc c'est la boisson 3 --> latte mais quelle taille?)
      if (tailleLatte == 1) {
        if (coffeeStocks >= 6) {
          cafeSuffisant = true
        }
      } else if (tailleLatte == 2) {
        if (coffeeStocks >= 8) {
          cafeSuffisant = true
        }
      } else { //Donc Latte grande taille (3)
        if (coffeeStocks >= 12) {
          cafeSuffisant = true
        }
      }
    }

    //Suffisance de sucre?
    if (sucre == 1) {
      sucreSuffisant = true
    } else if (sucre == 2) {
      if (sugarStocks >= 5) {
        sucreSuffisant = true
      }
    } else if (sucre == 3) {
      if (sugarStocks >= 10) {
        sucreSuffisant = true
      }
    } else { //Donc c'est 15g de sucre (sucre =4)
      if (sugarStocks >= 15) {
        sucreSuffisant = true
      }
    }

    //Suffisance de lait?
    if (boisson == 1) {
      QtotalLait = 0
    } else if (boisson == 2) {
      QtotalLait = (50 * doselait) + 100
    } else if (boisson == 3 && tailleLatte == 1) {
      QtotalLait = (50 * doselait) + 120
    } else if (boisson == 3 && tailleLatte == 2) {
      QtotalLait = (50 * doselait) + 150
    } else if (boisson == 3 && tailleLatte == 3) {
      QtotalLait = (50 * doselait) + 200
    }

    if ((milkStocks >= QtotalLait) || (boisson == 1)) {
      laitSuffisant = true
    } else {
      laitSuffisant = false
    }

    //Suffisance générale(total) des stocks
    if ((cafeSuffisant == true) && (sucreSuffisant == true) && (laitSuffisant == true)) {
      SuffisanceStocks = true
    } else {
      SuffisanceStocks = false
    }


    //Nommer ce qui a été sélectionnée
    if (boisson == 1) {
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
    } //Fin du if stocks sont insuffisants

    //Si les stocks sont suffisants
    if (SuffisanceStocks == true) {

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

      typecaractere = (math.random() * 2).toInt
      if (typecaractere == 0) {
        a = alphabet((math.random() * alphabet.length - 1).toInt).toString
      } else {
        a = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random() * 2).toInt
      if (typecaractere == 0) {
        b = alphabet((math.random() * alphabet.length - 1).toInt).toString
      } else {
        b = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random() * 2).toInt
      if (typecaractere == 0) {
        c = alphabet((math.random() * alphabet.length - 1).toInt).toString
      } else {
        c = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random() * 2).toInt
      if (typecaractere == 0) {
        d = alphabet((math.random() * alphabet.length - 1).toInt).toString
      } else {
        d = chiffres((math.random() * chiffres.length - 1).toInt).toString
      }

      typecaractere = (math.random() * 2).toInt
      if (typecaractere == 0) {
        e = alphabet((math.random() * alphabet.length - 1).toInt).toString
      } else {
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
      if (boisson == 1) {
        amountcafe = 8
      } else if (boisson == 2) {
        amountcafe = 6
      } else if (boisson == 3 && tailleLatte == 1) {
        amountcafe = 6
      } else if (boisson == 3 && tailleLatte == 2) {
        amountcafe = 8
      } else if (boisson == 3 && tailleLatte == 3) {
        amountcafe = 12
      }


     //Retirer du sucre du stock
      if (sucre == 2) {
        amountsucre = 5
      } else if (sucre == 3) {
        amountsucre = 10
      } else if (sucre == 4) {
        amountsucre = 15
      }

      //Retirer du lait du stocks
      amountlait = QtotalLait

//Mettre un ArrayBuffer lire fichier, le modifier et l'enregistrer
      machines(id-1).removeIngredient("café", amountcafe)
      machines(id-1).removeIngredient("sucre", amountsucre)
      machines(id-1).removeIngredient("lait", amountlait)
      savecsv(filename, machines)

    } //Fin du if Stocks sont suffisant
    savecsv(filename, machines)
    if (SuffisanceStocks == false) {
      return false
    } else { // Boisson réussi - servie - argent encaissé et stock retirés
      return true
    }
  }


  def main(args: Array[String]): Unit = {

    val filename = "machines.csv."
    val machines = loadcsv("machines.csv.")
    savecsv(filename, machines)
    val fw = new PrintWriter("machines.csv.")
    val pw = new PrintWriter(new FileWriter("machines.csv.", true))
    var stocksuffisant = true
    var idSaisi = "kk"
    var id = 0
    var ModeRéussi = false
    var pinvalide = false


   //Boucle du programme Nospresso
    do {
     // Réinicialise variable
      var idSaisi = "."
      var id = 0
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

       // Quelle machine utiliser? Validation de l'ID de la machine
        do {
          idSaisi = readLine("\nMachine séléctionnée (1-5)  > ")
          if (idSaisi.forall(_.isDigit)) {
            id = idSaisi.toInt
          } else {
            id = 0 //Assigne une valeur hors plage pour déclancher la boucle
          }
        } while (id < 1 || id > machines.length)

        do {
          ModeRéussi = serveClient(id, machines(id - 1).coffeeMachine, machines(id - 1).sugarMachine, machines(id - 1).milkMachine)
        } while (ModeRéussi == false)

      } else if (mode == 2) { // Mode Admin
       // Quelle machine utiliser? Validation de l'ID de la machine
        do {
          idSaisi = readLine("\nMachine séléctionnée (1-5)  > ")
          if (idSaisi.forall(_.isDigit)) {
            id = idSaisi.toInt
          } else {
            id = 0 //Assigne une valeur hors plage pour déclancher la boucle
          }
        } while (id < 1 || id > machines.length)

       // Validation du code PIN (Utilisation de la méthode validatePin)
        pinvalide = validatePin(id, machines(id-1).pincodeMachine.toString)
        if (pinvalide) {
          println("Accès accordé à la Machine " + id + ".\n")

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

         //Admin Mode -> reapprovisionnement
          if (admin == 1) { //Réapprovisionner - méthode addingredient

           //Afficher les stocks de la machine
            println("\nStocks de ma machine " + id)
            println("\tPoudre de café: " + machines(id-1).coffeeMachine + " g")
            printf("\tLait:           %.3f L", machines(id-1).milkMachine.toDouble/1000)
            println("\n\tSucre:          " + machines(id-1).sugarMachine + " g")

            do {
              ingredient = readLine("\nQuel ingredient souhaitez-vous ajouter (café, lait, sucre) ?\n> ")
            } while (ingredient != "café" && ingredient != "sucre" && ingredient != "lait")
            do {
              if((ingredient == "café")||(ingredient == "sucre")) {
                amountSaisi = readLine("Quelle quantité souhaitez-vous ajouter de " + ingredient + " en grammes (g).\n> ")
              }else{
                amountSaisi = readLine("Quelle quantité souhaitez-vous ajouter de " + ingredient + " en mililitres (ml).\n> ")
              }
              if (amountSaisi.forall(_.isDigit)) {
                amount = amountSaisi.toInt
              } else {
                amount = -1
              }
            } while (amount < 0)

            if (id == 1) {
              machines(0).addIngredient(ingredient, amount)
            } else if (id == 2) {
              machines(1).addIngredient(ingredient, amount)
            } else if (id == 3) {
              machines(2).addIngredient(ingredient, amount)
            } else if (id == 4) {
              machines(3).addIngredient(ingredient, amount)
            } else {
              machines(4).addIngredient(ingredient, amount)
            }
            savecsv(filename, machines)

           //Afficher les stocks mis à jour de la machine
            println("\nNiveaux de stock mis à jour.\nVoici les stocks à jour de la machine " + id)
            println("\tPoudre de café: " + machines(id-1).coffeeMachine + " g")
            printf("\tLait:           %.3f L", machines(id-1).milkMachine.toDouble/1000)
            println("\n\tSucre:          " + machines(id-1).sugarMachine + " g")

          } else { // Mise à jour du code PIN (Utilisation de la méthode updatePin)
            machines(id-1).pincodeMachine = updatePin(id).toInt
            savecsv(filename, machines)
            println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
          }
          ModeRéussi = true

        } else {
          println("\nTrop de tentatives échouées. Fin du programme.")
        }
      }else{ // Mode Quitter
        println("\nSauvegarde des machines dans machines.csv ...")
        savecsv(filename, machines)
        println("Fichier sauvgardé avec succès.\n\nVous avez quitté le programme Nospresso ...")
      }
      // Attendre 5000 millisecondes (5 secondes)
      Thread.sleep(5000)

    }while (ModeRéussi)
  }
}