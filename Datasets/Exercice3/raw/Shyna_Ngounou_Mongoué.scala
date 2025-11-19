import io.StdIn._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.sys.exit

class Machines(val idC: Int, var pincodeC: String, var milkC: Int, var sugarC: Int, var coffeeC:Int ){
  val id = idC
  var pincode = pincodeC
  var milk = milkC
  var sugar = sugarC
  var coffee = coffeeC

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if(ingredient == "milk") {
      milk = milk + amount
    }
    if(ingredient == "sugar"){
      sugar = sugar + amount
    }
    if(ingredient == "coffee"){
      coffee = coffee + amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if(ingredient == "milk") {
      if (milk >= amount){
        milk = milk - amount
        return true
      }
    }
    if(ingredient == "sugar"){
      if (sugar >= amount){
        sugar = sugar - amount
        return true
      }
    }
    if(ingredient == "coffee"){
      if (coffee >= amount){
        coffee = coffee - amount
        return true
      }
    }
    return false
  }
}


object Main {
  var machinePins = Array("0", "434343","434343","434343","434343","434343")
  var coffeeStocks = Array(0, 50, 50, 50, 50, 50)
  var sugarStocks = Array(0, 30, 30, 30, 30, 30)
  var milkStocks = Array(0, 500, 500, 500, 500, 500)

  // Methode pour lire le fichier machines
  def loadcsv(filename: String): ArrayBuffer[Machines] ={

    var touteslesmachines: ArrayBuffer[Machines] = ArrayBuffer()
    try {
      val fichier = Source.fromFile(filename)
      var lignes = fichier.reset.getLines
      lignes = lignes.drop(1)
      var i = 1

      while(!lignes.isEmpty){
        var ligne = lignes.next

        var machine = ligne.split(",")
        touteslesmachines += new Machines(i.toInt,
          machine(0),machine(1).toInt,
          machine(2).toInt, machine(3).toInt )
        i+=1
      }
      fichier.close
    }
    catch {
      case ex : java.io.FileNotFoundException =>
        println("Fichier introuvable. Vérifier le chemin d'accès et réessayer")
        exit(1)
      case ex : java.io.IOException =>
        println("Erreur d'entrée sortie")
        exit(1)
    }
    finally {
      println("Chargement des machines depuis machines.csv")
    }
    return touteslesmachines

  }

  //Methode pour enregistrer un fichier
  def savecsv(filename: String, machines: ArrayBuffer[Machines]): Unit = {
    println("Sauvegarde machines dans machines.csv...")
    try {
      val fw = new PrintWriter(filename)
      fw.println("ID,PINCODE,MILK,SUGAR,COFFEE")
      for(machine <- machines){
        fw.println(machine.pincode + "," + machine.milk + "," + machine.sugar+ "," + machine.coffee)

      }
      fw.close
    }
    catch {
      case ex : java.io.IOException =>
        println("Échec de l'écriture dans machines.csv. Le fichier peut être vérrouillé ou lecture seule.")
    }
    finally {
      println("Fichier sauvegardé avec succès.")
    }
  }

  // Méthode pour valider le code PIN
  def validePin(machineId: Int, machines: ArrayBuffer[Machines]): Boolean = {
    var tentative = 0
    val maxTentatives = 3
    for (machine <- machines){
      if (machine.id == machineId){
        while (tentative < maxTentatives ) {
          println(" Machine sélectionnée (1-5) > " + machineId)
          println("Entrez le code PIN :")
          val PIN_entre = readLine
          if (PIN_entre == machine.pincode) {
            println("Accès accordé à la machine " + machineId)
            return true
          } else {
            tentative += 1
            println("Code PIN incorrect. " + (maxTentatives -tentative)+ " tentatives restantes")


          }
        }

        println("Trop de tentatives échouées. Fin du programme")

        false
      }
    }
    print("machine non trouver")
    return false


  }
  // Méthode pour mettre à jour le code PIN
  def updatePin(machineId: Int, machines: ArrayBuffer[Machines]) : Unit ={



    println("Mise à jour du code PIN pour la machine " + machineId)
    println ("Entrez un nouveau code PIN à 6 chiffres >")
    val nouveauPin = readLine()

    // Vérification que le code PIN comporte 6 chiffres
    if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)){
      for (machine <- machines){
        if (machine.id == machineId){
          machine.pincode = nouveauPin
          println(" Le code PIN a été mis à jour avec succès. ")
          println ("Retour au menu principal ...")
        }
      }
    }

    else {
      println("Entrez un nouveau code PIN à 6 chiffres valide >")
    }
  }


  def restockMachine(machineId : Int, machines: ArrayBuffer[Machines]): Unit ={

    for (machine <- machines){
      if (machine.id == machineId){
        println("Réapprovisionnement de la machine" +machineId)
        println("Niveaux de stocks actuels:")
        println("Poudre de café:" + machine.coffee + "g")
        println("Lait :" + machine.milk+ "mL")
        println("Sucre:" + machine.sugar + "g")
        println("\n")
        println("Entrez les quantité à ajouter")
        println("Poudre de café : ")
        var ajoutcafe = readInt
        machine.addIngredient("coffee", ajoutcafe)
        //machine.coffee = machine.coffee+ readInt
        println("Lait:" )
        var ajoutlait = readInt
        machine.addIngredient("milk", ajoutlait)
        //machine.milk= machine.milk + readInt
        println("Sucre:")
        var ajoutsucre = readInt
        machine.addIngredient("sugar", ajoutsucre)
        //machine.sugar= machine.sugar + readInt
        println("Les stocks ont été mis à jour avec succès.")
        println("Retour au menu principal...")

      }
    }

  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machines]): Boolean = {
    var TypeBoisson = 0
    var Boisson_Selectionnee = ""
    var Niveau_Sucre = ""
    var Lait_Supp = ""
    var TailleLatte = 0
    var Sucre = 0
    var DoseSucre = 5
    var Quantite_Sucre_Add = 0
    var lait = 0
    var DoseLait = 0
    var Quantite_Lait_Cappuccino = 0
    var Quantite_Lait_Petit_Latte = 0
    var Quantite_Lait_Moyen_Latte = 0
    var Quantite_Lait_Grand_Latte = 0

    // Prix
    var PrixTotal = 0.0
    var PrixBaseExpresso = 2.00
    var PrixSucre = 0.10
    var PrixLait = 0.05
    var PrixBaseCappuccino = 2.50
    var PrixPetitLatte = 2.70
    var PrixMoyenLatte = 3.20
    var PrixGrandLatte = 3.70

    //Stock initial des ingrédients
    for (machine <- machines){
      if(machine.id == machineId){
        while (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
          println(" Veuillez sélectionner votre boisson : \n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit) , CHF 3.20 (Moyen) , CHF 3.70 (Grand) \n >")
          TypeBoisson = readInt()
          if (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
            println("Veuillez choisir un nombre valide")
          }
        }
        // Gestion des boissons Latte
        if (TypeBoisson == 3) {
          TailleLatte = 0
          while (TailleLatte!= 1 && TailleLatte != 2 && TailleLatte!= 3 ) {
            println("Quelle taille de Latte voulez-vous ? \n 1) Petit \n 2) Moyen \n 3)Grand\n > ")
            TailleLatte = readInt()
            if (TailleLatte!= 1 && TailleLatte != 2 && TailleLatte != 3) {
              println("Veuillez choisir un nombre valide")
            }
          }
        }
        Sucre = 0
        // Gestion du sucre
        while (Sucre!= 1 && Sucre != 2 && Sucre != 3 && Sucre != 4) {
          Sucre=0
          println(" Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n >")
          Sucre = readInt()
          if (Sucre!= 1 && Sucre != 2 && Sucre != 3 && Sucre != 4) {
            println("Veuillez choisir un nombre valide")
          }
        }
        if (Sucre == 1) {
          Niveau_Sucre = "Sans Sucre"
        } else if (Sucre == 2) {
          Niveau_Sucre = "Peu (5g)"
        } else if (Sucre == 3) {
          Niveau_Sucre = " Moyen (10g)"
        } else if (Sucre == 4) {
          Niveau_Sucre = "Beaucoup (15g)"
        }

        // Gestion quantité sucre ajouté
        if (Sucre == 1) { // Si l'utilisateur chosit "Sans sucre", aucun sucre n'est ajouté . Quantité_Sucre_add = 0
          Quantite_Sucre_Add = 0

        }
        else {
          Quantite_Sucre_Add = (Sucre - 1) * DoseSucre

        }

        //Gestion des boissons Expresso
        if (TypeBoisson == 1) {

          Boisson_Selectionnee = "Expresso"
          if (machine.coffee >= 8 && (machine.sugar >= Quantite_Sucre_Add)) {
            machine.removeIngredient("coffee", 8)
            machine.removeIngredient("sugar", Quantite_Sucre_Add)
            //machine.coffee-= 8
            //machine.sugar-= Quantite_Sucre_Add
            println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre)
            PrixSucre *= (Sucre - 1)
            PrixTotal = PrixBaseExpresso + PrixSucre
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseExpresso, PrixSucre, PrixTotal)
            // Interface paiement
            println("Veuillez payer en utilsant Twint.")
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
            val charsSize = chars.length
            var pin = ""
            for (i <- 1 to 5) {
              val twint = (Math.random() * charsSize).toInt
              pin += chars(twint)
            }
            println("Votre code de paiement est :" + pin)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
            println("Merci ! Votre paiement a été accepté.")

            // Préparation de la boisson
            println("Préparation de votre boisson ... \n [...] \n Votre " + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")
            PrixSucre = 0.10
            return true
          }
          else {
            if (machine.coffee < 8) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.sugar < Quantite_Sucre_Add) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionée.\n Veuillez choisir une autre boisson ou vérifier les stocks en Mode Admin ou choisir une autre machine.")
              return false
            }
          }
        }

        // Cas si boisson est un cappuccino ou un latte : Demande supp lait
        if (TypeBoisson == 2 || TypeBoisson == 3) {

          lait = 0
          DoseLait = 0

          while (lait!= 1 && lait != 2 ) {
            println(" Souhaitez-vous ajouter du lait en supplément ? \n ( Disponible uniquemnt pour Cappucino et Latte)\n 1) Oui \n 2) Non\n >")
            lait = readInt()
            if (lait!= 1 && lait != 2 ) {
              println("Veuillez choisir un nombre valide")
            }
          }
        }
        if (lait == 1) {
          lait = 0
          Lait_Supp = "Oui"
          while (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {

            println("Combien de dose ? \n >")
            DoseLait = readInt()
            if (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {
              println("Veuillez choisir une dose de lait supplémentaire valide")

            }
          }




          // Gestion quantité lait ajouté

          if (DoseLait == 1 && TypeBoisson == 2) {
            Quantite_Lait_Cappuccino = 150

          }
          else if (DoseLait == 2 && TypeBoisson == 2) {
            Quantite_Lait_Cappuccino = 200

          }
          else if (DoseLait == 3 && TypeBoisson == 2) {
            Quantite_Lait_Cappuccino = 250

          }
          else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 1) {
            Quantite_Lait_Petit_Latte = 170

          }
          else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 2) {
            Quantite_Lait_Moyen_Latte = 200

          }
          else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 3) {
            Quantite_Lait_Grand_Latte = 250

          }
          else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 1) {
            Quantite_Lait_Grand_Latte = 220

          }
          else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 2) {
            Quantite_Lait_Moyen_Latte = 250

          }
          else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 3) {
            Quantite_Lait_Grand_Latte = 300

          }
          else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 1) {
            Quantite_Lait_Grand_Latte = 270

          }
          else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 2) {
            Quantite_Lait_Moyen_Latte = 300

          }
          else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 3) {
            Quantite_Lait_Grand_Latte = 350

          }
        }

        if (lait == 2) {
          Lait_Supp = "Non"
          if (TypeBoisson == 2) {
            Quantite_Lait_Cappuccino = 100

          }
          else if (TypeBoisson == 3 && TailleLatte == 1) {
            Quantite_Lait_Petit_Latte = 120

          }
          else if (TypeBoisson == 3 && TailleLatte == 2) {
            Quantite_Lait_Moyen_Latte = 150

          }
          else if (TypeBoisson == 3 && TailleLatte == 3) {
            Quantite_Lait_Grand_Latte = 200

          }

        }


        // Gestion des boissons Cappuccino
        if (TypeBoisson == 2) {

          Boisson_Selectionnee = "Cappuccino"
          if (machine.coffee >= 6 && (machine.sugar >= Quantite_Sucre_Add) && (machine.milk>= Quantite_Lait_Cappuccino)) {
            machine.removeIngredient("coffee", 6)
            machine.removeIngredient("milk", Quantite_Lait_Cappuccino)
            machine.removeIngredient("sugar", Quantite_Sucre_Add)
            //machine.coffee-= 6
            //machine.milk -= Quantite_Lait_Cappuccino
            //machine.sugar-= Quantite_Sucre_Add
            println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
            PrixSucre *= (Sucre - 1)
            PrixLait *= DoseLait
            PrixTotal = PrixBaseCappuccino + PrixSucre + PrixLait
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseCappuccino, PrixSucre, PrixLait, PrixTotal)
            // Interface paiement
            println("Veuillez payer en utilsant Twint.")
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
            val charsSize = chars.length
            var pin = ""
            for (i <- 1 to 5) {
              val twint = (Math.random() * charsSize).toInt
              pin += chars(twint)
            }
            println("Votre code de paiement est :" + pin)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
            println("Merci ! Votre paiement a été accepté.")

            // Préparation de la boisson
            println("Préparation de votre boisson ... \n [...] \n Votre " + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

            PrixSucre = 0.10
            PrixLait = 0.05
            return true
          }

          else {
            if (machine.coffee< 6) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.sugar < Quantite_Sucre_Add) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.milk< Quantite_Lait_Cappuccino) {
              Quantite_Lait_Cappuccino = 0
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
          }

        }


        // Gestion commande petit Latte
        if (TailleLatte == 1 && TypeBoisson == 3) {

          Boisson_Selectionnee = "Latte (petit)"
          if (machine.coffee >= 6 && (machine.sugar>= Quantite_Sucre_Add) && (machine.milk>= Quantite_Lait_Petit_Latte)) {
            TypeBoisson = 0
            machine.removeIngredient("coffee", 6)
            machine.removeIngredient("milk", Quantite_Lait_Petit_Latte)
            machine.removeIngredient("sugar", Quantite_Sucre_Add)
            //machine.coffee-= 6
            //machine.milk-= Quantite_Lait_Petit_Latte
            //machine.sugar-= Quantite_Sucre_Add
            println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
            PrixSucre *= (Sucre - 1)
            PrixLait *= DoseLait
            PrixTotal = PrixPetitLatte + PrixSucre + PrixLait
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixPetitLatte, PrixSucre, PrixLait, PrixTotal)
            // Interface paiement
            println("Veuillez payer en utilsant Twint.")
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
            val charsSize = chars.length
            var pin = ""
            for (i <- 1 to 5) {
              val twint = (Math.random() * charsSize).toInt
              pin += chars(twint)
            }
            println("Votre code de paiement est :" + pin)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
            println("Merci ! Votre paiement a été accepté.")

            // Préparation de la boisson
            println("Préparation de votre boisson ... \n [...] \n Votre " + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

            PrixSucre = 0.10
            PrixLait = 0.05
            return true
          }

          else {
            Quantite_Lait_Petit_Latte = 0
            if (machine.coffee < 6) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.sugar < Quantite_Sucre_Add) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.milk< Quantite_Lait_Petit_Latte) {
              Quantite_Lait_Petit_Latte = 0
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }

          }
        }

        // Gestion commande moyen Latte
        if (TailleLatte == 2 && TypeBoisson == 3) {

          Boisson_Selectionnee = "Latte (moyen)"
          if (machine.coffee >= 8 && (machine.sugar >= Quantite_Sucre_Add) && (machine.milk>= Quantite_Lait_Moyen_Latte)) {
            TypeBoisson = 0
            machine.removeIngredient("coffee", 8)
            machine.removeIngredient("milk", Quantite_Lait_Moyen_Latte)
            machine.removeIngredient("sugar", Quantite_Sucre_Add)
            //machine.coffee -= 8
            //machine.milk -= Quantite_Lait_Moyen_Latte
            //machine.sugar-= Quantite_Sucre_Add
            println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
            PrixSucre *= (Sucre - 1)
            PrixLait *= DoseLait
            PrixTotal = PrixMoyenLatte + PrixSucre + PrixLait
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixMoyenLatte, PrixSucre, PrixLait, PrixTotal)
            // Interface paiement
            println("Veuillez payer en utilsant Twint.")
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
            val charsSize = chars.length
            var pin = ""
            for (i <- 1 to 5) {
              val twint = (Math.random() * charsSize).toInt
              pin += chars(twint)
            }
            println("Votre code de paiement est :" + pin)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
            println("Merci ! Votre paiement a été accepté.")

            // Préparation de la boisson
            println("Préparation de votre boisson ... \n [...] \n Votre " + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

            PrixSucre = 0.10
            PrixLait = 0.05
            return true
          }

          else {
            if (machine.coffee < 8) {

              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.sugar< Quantite_Sucre_Add) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.milk< Quantite_Lait_Moyen_Latte) {
              Quantite_Lait_Moyen_Latte = 0
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
          }
        }

        // Gestion commande Grand Latte
        while (TailleLatte == 3 && TypeBoisson == 3) {
          Boisson_Selectionnee = "Latte (grand)"

          if ((machine.coffee>= 12 )&& (machine.sugar >= Quantite_Sucre_Add) && (machine.milk >= Quantite_Lait_Grand_Latte)) {
            TypeBoisson = 0
            machine.removeIngredient("coffee", 12)
            machine.removeIngredient("milk", Quantite_Lait_Grand_Latte)
            machine.removeIngredient("sugar", Quantite_Sucre_Add)
            //machine.coffee-= 12
            //machine.milk -= Quantite_Lait_Grand_Latte
            //machine.sugar -= Quantite_Sucre_Add
            println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
            PrixSucre *= (Sucre - 1)
            PrixLait *= DoseLait
            PrixTotal = PrixGrandLatte + PrixSucre + PrixLait
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixGrandLatte, PrixSucre, PrixLait, PrixTotal)
            // Interface paiement
            println("Veuillez payer en utilsant Twint.")
            val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
            val charsSize = chars.length
            var pin = ""
            for (i <- 1 to 5) {
              val twint = (Math.random() * charsSize).toInt
              pin += chars(twint)
            }
            println("Votre code de paiement est :" + pin)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
            println("Merci ! Votre paiement a été accepté.")

            // Préparation de la boisson
            println("Préparation de votre boisson ... \n [...] \n Votre " + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

            PrixSucre = 0.10
            PrixLait = 0.05
            return true
          }

          else {
            if (machine.coffee< 12) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.sugar < Quantite_Sucre_Add) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
            else if (machine.milk < Quantite_Lait_Grand_Latte) {
              Quantite_Lait_Grand_Latte = 0
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin ou choisir une autre machine.")
              return false
            }
          }
        }

      }
    }



    return true
  }




  def main(args: Array[String]): Unit = {
    var continuer = true
    var machines : ArrayBuffer[Machines] = loadcsv("machines.csv")

    for(machine<- machines){

      println("Machine " + (machine.id).toString + " chargée:")
      println("\t ID: " + (machine.id).toString())
      println("\t Code PIN: " + (machine.pincode).toString())
      println("\t Lait: " + (machine.milk).toString() + "ml")
      println("\t Sucre: " + (machine.sugar).toString() + "g")
      println("\t Café: " + (machine.coffee).toString() + "g")
      println("\n")
    }
    while (continuer) {
      var machineId = 0
      var mode = 0
      var choixModeAdmin = 0


      println( (machines.length).toString + " machine(s) chargée(s) avec succès.")



      while (mode < 1 && mode != 2 && mode != 3) {
        println("Nospresso Café \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        mode = readInt()
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez choisir un nombre valide")
        }

        // Mode client
        if (mode == 1) {
          machineId = 0
          while (machineId < 1 || machineId >= machines.length) {
            println("Machine sélectionnée (1-5)\n>")
            machineId = readInt()
          }
          serveClient(machineId, machines)

        }
        // Mode Admin
        if (mode == 2) {
          machineId = 0
          while (machineId < 1 || machineId >= machines.length) {
            println("Machine sélectionnée (1-5)\n>")
            machineId = readInt()
          }

          if (validePin(machineId, machines)) {
            choixModeAdmin = 0
            while (choixModeAdmin != 1 && choixModeAdmin != 2) {
              println(" Que voulez vous faire ?: \n 1) Mettre à jour le code Pin de la machine " + machineId + "\n 2) Réapprovisionner les stocks de la machine " + machineId)
              choixModeAdmin = readInt
            }
            if (choixModeAdmin == 1) {
              updatePin(machineId, machines)
            }
            if (choixModeAdmin == 2) {
              restockMachine(machineId, machines)
            }
          }
          mode = 0

        }
        // Mode Quitter
        if (mode == 3) {
          continuer = false
          savecsv("machines.csv", machines)
        }

      }
    }
  }
}