import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.{FileWriter, PrintWriter}
import scala.io.Source

object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") {
        milk += amount
      }
      if (ingredient == "sugar") {
        sugar += amount
      }
      if (ingredient == "coffee") {
        coffee += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "milk" && milk >= amount) {
        milk -= amount
        return true
      }
      if (ingredient == "sugar" && sugar >= amount) {
        sugar -= amount
        return true
      }
      if (ingredient == "coffee" && coffee >= amount) {
        coffee -= amount
        return true
      }

      println(" Stock insuffisant de : " + ingredient)
      false
    }

    def afficherinfos(): Unit = {
      println("Machine " + id + " chargée :")
      println("ID: " + id)
      println("Code Pin: " + pincode)
      println("Lait: " + milk)
      println("Sucre: " + sugar)
      println("Café: " + coffee)
      println()
    }
  }
  // lire les machines depuis les fichiers
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val plmachines = new ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile("machines.csv")
      val lignes = source.getLines().drop(1) // on saute la première ligne
      for (ligne <- lignes) {
        val Array(pincode, milk, sugar, coffee) = ligne.split(",")
        plmachines += new Machine(plmachines.length + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
      source.close()
      println("Chargement des machines depuis " + filename)
      for (i <- 0 until plmachines.length) {
        plmachines(i).afficherinfos()
      }
      println (plmachines.length + " machine(s) chargée(s) avec succès.")
    }
    catch {
      case exception : java.io.FileNotFoundException =>
        println("Erreur : Le fichier filename est introuvable")
    }
    plmachines
  }

  // sauvagarder les machines dans les fichiers CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    var printWriter: PrintWriter = null
    try {
      printWriter = new PrintWriter(new FileWriter(filename, false))

      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")

      for (machine <- machines) {
        printWriter.println(machine.pincode + "," + machine.milk + "," +  machine.sugar + "," +  machine.coffee)
      }
      println("Fichier sauvagardé avec succès dans " + filename)
    } catch {
      case exception : java.io.IOException =>
        println("Erreur d'écriture dans le fichier filename ")
    }
    finally {
      if (printWriter != null) {
        printWriter.close()
      }
    }
  }

  def fonctionclient (machineId : Int, plmachines: ArrayBuffer[Machine]): Unit = {
    var machine = plmachines(machineId)
    var validationdesstocks : Boolean = false
    do {
      validationdesstocks = serveClient(machine)
      if (validationdesstocks){
        return
      }
      println ("Choisissez une machine (de 1 à " + plmachines.length + ")")
      var choixmachine = readInt - 1
      while (choixmachine < 0 || (choixmachine > plmachines.length - 1 )) {
        println("La valeur sélectionnée est incorrect, réssayer")
        println(">")
        choixmachine = readInt - 1
      }
      machine = plmachines(choixmachine)
    } while (!validationdesstocks)
  }

  def fonctionadmin (machine : Machine) : Boolean = {
    if (!validatePin(machine)){
      return false
    }
    var changerlecode = 0
    println("Voulez-vous changez de code PIN ? 1)oui 2)non ")
    println(">")
    changerlecode = readInt()
    while ((changerlecode != 1) && (changerlecode != 2)) {
      println("Valeur incorrect, veuillez saisir une autre valeur ")
      println(">")
      changerlecode = readInt()
    }
    if (changerlecode == 1) {
      // Méthode pour changer le code PIN
      updatePin(machine)
    }
    var approvisionnementstock = 0
    println(" Voulez-vous réapprovisionner les stocks ? 1) oui 2) non")
    println(">")
    approvisionnementstock = readInt()
    while ((approvisionnementstock != 1) && (approvisionnementstock != 2)) {
      println("Valeur incorrect, veuillez saisir une autre valeur ")
      println(">")
      approvisionnementstock = readInt()
    }
    if (approvisionnementstock == 1) {
      // Méthode pour remplir les stocks
      restockMachine(machine)
    }
    true
  }

  // Gestion des codes PIN
  def validatePin(machine: Machine): Boolean = {
    var nbdetentative = 3
    var tentativerestantes = 0
    while (nbdetentative > 0) {
      println(" Entrez le code PIN: ******")
      println("<")
      var codePIN = readLine()
      if (codePIN == machine.pincode) {
        println("Code Pin correct")
        return true
      }
      else {
        nbdetentative -= 1
        tentativerestantes = nbdetentative
        if (tentativerestantes > 0) {
          println(" Code Pin incorrect, il vous reste " + tentativerestantes + " tentatives")
          println("<")
        }
      }
    }
    println("Trop de tentative échouées.")
    return false
  }

  // Méthode pour changer le code PIN
  def updatePin(machine: Machine): Unit = {
    println("Veuillez entrer le nouveau code Pin à 6 chiffres")
    println("<")
    var nouveaucodePIN = readLine()
    while (nouveaucodePIN.length != 6 || !nouveaucodePIN.forall(_.isDigit)) {
      println("Saisie invalide : Veuillez sélectionner un autre mot de passe à 6 chiffres")
      println("<")
      nouveaucodePIN = readLine()
    }

    machine.pincode = nouveaucodePIN
    println(" Le code PIN a été mis à jour")
  }

  // Méthode pour remplir les stocks
  def restockMachine(machine: Machine): Unit = {
    var choixaction = 0
    println("Affichage des stocks :")
    println("Poudre de café: " + machine.coffee + "g")
    println("Lait: " + ( machine.milk.toDouble / 1000) + "L")
    println("Sucre: " + machine.sugar + "g")
    println(" Réapprovisionnement des stocks...")
    println("1) Voulez-vous ajouter un ingrédient?")
    println("2) Voulez-vous retirer un ingrédient?")
    println(">")
    choixaction = readInt()
    while ((choixaction != 1) && (choixaction != 2)) {
      println("Valeur incorrect, veuillez saisir une autre valeur ")
      choixaction = readInt()
    }
    if (choixaction == 1) {
      println ("Combien de quantités désirez-vous ajouter ?")
      println(" Poudre à café (en grammes):")
      var remplissagecafe = readInt()
      while (remplissagecafe < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        remplissagecafe = readInt()
      }
      machine.addIngredient ("coffee", remplissagecafe)

      println(" Lait (en litres):")
      var remplissagelait = (readDouble() * 1000).toInt
      while (remplissagelait < 0) {
        println(" Veuillez entrer une valeur positive")
        println(">")
        remplissagelait = (readDouble() * 1000).toInt
      }
      machine.addIngredient ("milk", remplissagelait)

      println(" Sucre (en grammes):")
      var remplissagesucre = readInt()
      while (remplissagesucre < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        remplissagesucre = readInt()
      }
      machine.addIngredient ("sugar", remplissagesucre)
      println("Niveau de stock mis à jour")}

    if (choixaction == 2) {
      println("Combien de quantités désirez-vous retirer?")
      println(" Poudre café : (en grammes) ")
      var deductioncafe = readInt()
      while (deductioncafe < 0) {
        println(" Veuillez entrer une valeur positive")
        println (">")
        deductioncafe = readInt()
      }
      machine.removeIngredient("coffee", deductioncafe)
      while (! machine.removeIngredient("coffee", deductioncafe)) {
        println ("Veuillez retirer moins d'ingrédient ")
        println(">")
        deductioncafe = readInt()
      }

      println(" Lait : (en litres) ")
      var deductionlait = (readDouble()*1000).toInt
      while (deductionlait < 0) {
        println(" Veuillez entrer une valeur positive")
        println (">")
        deductionlait = (readDouble()*1000).toInt
      }
      machine.removeIngredient("milk", deductionlait)
      while (! machine.removeIngredient("milk", deductionlait)) {
        println ("Veuillez retirer moins d'ingrédient ")
        println(">")
        deductionlait = (readDouble()*1000).toInt
      }

      println(" Sucre : (en grammes) ")
      var deductionsucre = readInt()
      while (deductionsucre < 0) {
        println(" Veuillez entrer une valeur positive")
        println (">")
        deductionsucre = readInt()
      }
      machine.removeIngredient("sugar", deductionsucre)
      while (! machine.removeIngredient("sugar", deductionsucre)) {
        println ("Veuillez retirer moins d'ingrédient ")
        println(">")
        deductionsucre = readInt()
      }
    }
  }

  // Méthode pour servir le client
  def serveClient(machine: Machine ): Boolean = {
    var result = true
    // mode à selectionner
    var mode = 0.0
    // définir type de boisson
    var boisson = 0.0
    // quantité sucre
    var nbsucre = 0
    // prix sucre
    var prixsucre = 0.0
    // prix lait
    var prixdoselait = 0.0

    // prix boisson
    var prixtaillelatte = 0.0
    var prixexpresso = 2.00
    var prixcappuccino = 2.50
    var taillelatte = 0

    var poudrecafe = 0
    var quantitesucre = 0
    var lait = 0
    var doselait = 0
    var supplementlait = 0.0
    var quantitelait = 0
    var prixfinal = 0.0

    // Paiement Twint
    var code = ""
    val nbcaractere = 5
    val valeuralphanumeric = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    doselait = 0
    quantitesucre = 0
    nbsucre = 0
    quantitelait = 0
    println("Vous avez sélectionner le mode client.")
    println(" Quelle boisson désirez-vous ?")
    println(" 1.1) Expresso - CHF 2.00")
    println(" 1.2) Cappuccino - CHF 2.50")
    println(" 1.3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand ")
    println(">")
    boisson = readDouble()
    while ((boisson != 1.1) && (boisson != 1.2) && (boisson != 1.3)) {
      println(" La valeur sélectionnée est incorrect, réssayer")
      println(">")
      boisson = readDouble()
    }
    // expresso
    if (boisson == 1.1) {
      poudrecafe = 8 // grammes
      quantitesucre = 0
      quantitelait = 0
      prixexpresso = 2.0 //CHF
      println("Quelle quantité de sucre desirez-vous?")
      println(" 1) un peu; 5g - 0.10CHF")
      println(" 2) moyen; 10g - 0.20CHF")
      println(" 3) beaucoup; 15g - 0.30CHF")
      println(" 4) pas de sucre ")
      println(">")
      quantitesucre = readInt()
      while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
        println(" la valeur sélectionnée est incorrect, réssayer")
        println(">")
        quantitesucre = readInt()
      }
      if (quantitesucre == 1) {
        prixsucre = 0.10
        nbsucre = 5 // grammes
      }
      else if (quantitesucre == 2) {
        prixsucre = 0.20
        nbsucre = 10 // grammes
      }
      else if (quantitesucre == 3) {
        prixsucre = 0.30
        nbsucre = 15 // grammes
      }
      else if (quantitesucre == 4) {
        prixsucre = 0.0
      }
    }
    // cappuccino
    if (boisson == 1.2) {
      poudrecafe = 6 // grammes
      lait = 100 // ml
      nbsucre = 0
      doselait = 0
      prixcappuccino = 2.50 // CHF

      println(" Quelle quantité desirez-vous?")
      println("1) un peu 5g- 0.10CHF")
      println("2) moyen 10g - 0.20CHF")
      println("3) beaucoup 15g - 0.30CHF")
      println("4) pas de sucre")
      println(">")
      quantitesucre = readInt()
      while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
        println(" la valeur sélectionnée est incorrect, réssayer")
        println(">")
        quantitesucre = readInt()
      }
      if (quantitesucre == 1) {
        prixsucre = 0.10 // CHF
        nbsucre = 5 //grammes
      }
      else if (quantitesucre == 2) {
        prixsucre = 0.20 // CHF
        nbsucre = 10 // grammes
      }
      else if (quantitesucre == 3) {
        prixsucre = 0.30 // CHF
        nbsucre = 15 // grammes
      }
      else if (quantitesucre == 4) {
        prixsucre = 0.0 // CHF
      }
      println(" Voulez vous un supplément lait ?")
      println("1) oui")
      println("2) non")
      println(">")
      supplementlait = readInt()
      while ((supplementlait != 1) && (supplementlait != 2)) {
        println(" La valeur sélectionnée est incorrect, réssayer")
        println(">")
        supplementlait = readInt()
      }
      if (supplementlait == 1) {
        println(" Combien de dose? (maximum 3)")
        println("1)une dose, 50ml- 0.05CHF")
        println("2) deux dose 100ml- 0.10 CHF")
        println("3) trois doses 150ml- 0.15 CHF")
        println(">")
        doselait = readInt()
        while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
          println(" la valeur sélectionnée est incorrect, réssayer")
          println(">")
          doselait = readInt()
        }
        if (doselait == 1) {
          doselait = 50
          prixdoselait = 0.05
          quantitelait = doselait + lait
        }
        else if (doselait == 2) {
          doselait = 50 * 2
          prixdoselait = 0.1
          quantitelait = doselait + lait
        }
        else if (doselait == 3) {
          doselait = 50 * 3
          prixdoselait = 0.15
          quantitelait = doselait + lait
        }
      } else {
        prixdoselait = 0.0
        quantitelait = lait
      }
    }
    // latte
    if (boisson == 1.3) {
      println("Quelle taille desirez-vous ?")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      println(">")
      taillelatte = readInt()

      while ((taillelatte != 1) && (taillelatte != 2) && (taillelatte != 3)) {
        println(" la valeur sélectionnée est incorrect, réssayer")
        println(">")
        taillelatte = readInt()
      }
      if (taillelatte == 1) {
        poudrecafe = 6 //g
        lait = 120 //ml
        prixtaillelatte = 2.70 // CHF

      } else if (taillelatte == 2) {
        poudrecafe = 8 // grammes
        lait = 150 // ml
        prixtaillelatte = 3.20 // CHF
      }
      else if (taillelatte == 3) {
        poudrecafe = 12 //grammes
        lait = 200 //ml
        prixtaillelatte = 3.70 // CHF
      }
      quantitesucre = 0
      doselait = 0

      println(" Quelle quantité de sucre desirez-vous? ")
      println("1) un peu 5g- 0.10CHF")
      println("2) moyen 10g - 0.20CHF")
      println("3) beaucoup 15g - 0.30CHF")
      println("4) pas de sucre")
      println(">")
      quantitesucre = readInt()
      while ((quantitesucre != 1) && (quantitesucre != 2) && (quantitesucre != 3) && (quantitesucre != 4)) {
        println(" la valeur sélectionnée est incorrect, réssayer")
        println(">")
        quantitesucre = readInt()
      }
      if (quantitesucre == 1) {
        prixsucre = 0.10
        nbsucre = 5
      }
      else if (quantitesucre == 2) {
        prixsucre = 0.20
        nbsucre = 10
      }
      else if (quantitesucre == 3) {
        prixsucre = 0.30
        nbsucre = 15
      }
      else if (quantitesucre == 4) {
        prixsucre = 0.0
      }
      println(" Voulez vous un supplément lait ?")
      println("1) oui ")
      println("2) non ")
      println(">")
      supplementlait = readDouble()
      while ((supplementlait != 1) && (supplementlait != 2)) {
        println(" la valeur sélectionnée est incorrect, réssayer")
        println(">")
        supplementlait = readDouble()
      }
      if (supplementlait == 1) {
        println(" Combien de dose? (maximum 3))")
        println("1) une dose (50ml)")
        println("2) deux dose (100ml)")
        println("3) trois doses (150ml)")
        println(">")
        doselait = readInt()
        while ((doselait != 1) && (doselait != 2) && (doselait != 3)) {
          println(" la valeur sélectionnée est incorrect, réssayer")
          println(">")
          doselait = readInt()
        }
        if (doselait == 1) {
          doselait = 50 //ml
          prixdoselait = 0.05 // CHF
          quantitelait = doselait + lait
        }
        else if (doselait == 2) {
          doselait = 50 * 2 //ml
          prixdoselait = 0.1 //CHF
          quantitelait = doselait + lait
        }
        else if (doselait == 3) {
          doselait = 50 * 3 //ml
          prixdoselait = 0.15 // CHF
          quantitelait = doselait + lait
        }
      } else {
        quantitelait = lait
        prixdoselait = 0.0
      }
    }
    println()
    if (boisson == 1.1) println("Votre boisson: Expresso")
    if (boisson == 1.2) println("Votre boisson: Cappuccino")
    if (boisson == 1.3) println("Votre boisson: Latte")
    if (quantitesucre == 1) println("Niveau sucre: Un peu (5g)")
    if (quantitesucre == 2) println("Niveau sucre: Moyen (10g)")
    if (quantitesucre == 3) println("Niveau de sucre: Beaucoup (15g)")
    if (quantitesucre == 4) println("Niveau de sucre: Aucun")
    if ((boisson == 1.2) && (supplementlait == 1)) {
      println("Supplément lait: Oui")
    }
    if ((boisson == 1.2) && (supplementlait == 2)) {
      println("Supplément lait: Non")
    }
    if ((boisson == 1.3) && (supplementlait == 1)) {
      println("Supplément lait: Oui")
    }
    if ((boisson == 1.3) && (supplementlait == 2)) {
      println("Supplément lait: Non")
    }
    if (boisson == 1.1) {
      prixfinal = prixexpresso + prixsucre
      printf("Le prix de votre expresso est de %.2f + %.2f = %.2f CHF\n", prixexpresso, prixsucre, prixfinal)
    }
    if (boisson == 1.2) {
      prixfinal = prixcappuccino + prixsucre + prixdoselait
      printf("Le prix de votre cappuccino est de %.2f + %.2f + %.2f = %.2f CHF\n", prixcappuccino, prixsucre, prixdoselait, prixfinal)
    }
    if (boisson == 1.3) {
      prixfinal = prixtaillelatte + prixsucre + prixdoselait
      printf("Le prix de votre latte est de %.2f + %.2f  + %.2f = %.2f CHF\n", prixtaillelatte, prixsucre, prixdoselait, prixfinal)
    }

    if (((boisson == 1.1) && machine.removeIngredient ("coffee", poudrecafe) && machine.removeIngredient ("sugar", nbsucre)) || ((boisson == 1.2) && machine.removeIngredient ("coffee", poudrecafe) && machine.removeIngredient ("sugar", nbsucre) && machine.removeIngredient ("milk", quantitelait)) || ((boisson == 1.3) && machine.removeIngredient ("coffee", poudrecafe) && machine.removeIngredient ("sugar", nbsucre) && machine.removeIngredient ("milk", quantitelait))) {
      println("Le stock est suffisant, veuillez payer en utilisant Twint")
      result = true

      // paiement twint
      code = ""
      for (i <- 1 to nbcaractere) {
        var caracterealeatoire = valeuralphanumeric(Random.nextInt(valeuralphanumeric.length))
        code += caracterealeatoire
      }
      if ((boisson == 1.1) || (boisson == 1.2) || (boisson == 1.3)) {
        println(" Votre code de paiement est :" + code)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("Paiement accepté, merci")
        println("Préparation de votre boisson...")
        if (boisson == 1.1) {
          println("Votre expresso est prêt! Bonne dégustation !")
        }
        if (boisson == 1.2) {
          println("Votre capppuccino est prêt! Bonne dégustation !")
        }
        if (boisson == 1.3) {
          println("Votre latte est prêt! Bonne dégustation! ")
        }
      }
    }
    else {
      result = false
      if (! machine.removeIngredient ("coffee", poudrecafe)) {
        println()
        println("Veuillez choisir une autre boisson, une plus petite ou une autre machine")
      }
      if (! machine.removeIngredient ("sugar", nbsucre)) {
        println()
        println("Veuillez choisir une autre boisson, moins de sucre ou une autre machine")
      }
      if (! machine.removeIngredient ("milk", quantitelait)) {
        println()
        println("Veuillez choisir une autre boisson, une plus petite, moins de lait ou une autre machine")
      }
    }
    return result
  }
  def main(args: Array[String]): Unit = {
    // mode à selectionner
    var machineId = 0
    var mode = 0
    val filename = "machine.csv"
    var plmachines = loadcsv(filename) // charger les machines depuis le fichier CSV

    var verification = false
    while (mode != 3)  {
      verification = false
      println()
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode:")
      println("1)Client")
      println("2)Admin")
      println("3)Quitter")
      println(">")
      mode = readInt()
      while ((mode != 1) && (mode != 2) && (mode != 3)) {
        println(" sélection incorrect, choissisez une autre valeur")
        println(">")
        mode = readInt()
      }
      // mode client
      if (mode == 1) {
        println("Choisissez une machine (de 1 à 5)")

        machineId = readInt() - 1
        while ((machineId != 0) && (machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4)) {
          println(" La valeur sélectionnée est incorrect, réssayer")
          println(">")
          machineId = readInt() - 1
        }
        fonctionclient(machineId, plmachines)
      }
      // mode admin
      if (mode == 2) {
        println("Choisissez une machine (de 1 à 5)")

        machineId = readInt() - 1
        while ((machineId != 0) && (machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4)) {
          println(" La valeur sélectionnée est incorrect, réssayer")
          println(">")
          machineId = readInt() - 1
        }
        println("Vous avez choisi le mode admin.")
        fonctionadmin(plmachines(machineId))
      }

      if (mode == 3) {
        savecsv("machines.csv", plmachines)
        println(" Fin du programme ")
      }
    }
  }
}