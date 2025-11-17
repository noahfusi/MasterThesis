
// Exercice 3 (fonctionnel)

import scala.io.Source
import java.io.{PrintWriter,FileWriter}
import collection.mutable.ArrayBuffer
import io.StdIn._
import scala.util.Random

object Main {

  var machines = ArrayBuffer[Machine]()

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int){

    def addIngredient(ingredient: String, amount: Int): Unit = {

      if(ingredient == "milk"){milk += amount}
      else if(ingredient == "sugar"){sugar += amount}
      else if(ingredient == "coffee"){coffee += amount}
      else {println("Erreur.")}
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {

      if(ingredient == "milk" && milk >= amount){
        milk -= amount
        return true
      }
      if(ingredient == "sugar" && sugar >= amount){
        sugar -= amount
        return true
      }
      if(ingredient == "coffee" && coffee >= amount){
        coffee -= amount
        return true
      } else {
        println("Quantité de l'ingrédient insuffisante.")
        return false
      }
    }
    def ecrirecsv: String = {pincode + "," +  milk + "," + sugar + "," + coffee}
  }

  def loadcsv(filename: String) : ArrayBuffer[Machine] = {
    try {
    val fr = Source.fromFile(filename)
    var ligne = fr.reset.getLines()
    val lignefr = ligne.next()

    println("\nChargement des machines depuis machines.csv...")
    var id = 0

    for (lignefr <- ligne) {

      id += 1
      println("\nMachine " + id + " chargée : ")
      var pincode = lignefr.split(",")(0)
      var milk = lignefr.split(",")(1).toInt
      var sugar = lignefr.split(",")(2).toInt
      var coffee = lignefr.split(",")(3).toInt
      machines += new Machine(id, pincode, milk, sugar, coffee)

      println("ID : " + id + "\nCode PIN : " + pincode + "\nLait : " + milk.toDouble / 1000 + "L\nSucre : " + sugar + "g\nCafé : " + coffee + "g")
    }
    println("\n" + id + " machine(s) chargées avec succès.\n")
    fr.close()
  } catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")}
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    println("Sauvegarde des machines dans " + filename + "...")
    try {
      val sauv = new PrintWriter(filename)
      sauv.println("PINCODE, MILK, SUGAR, COFFEE")
      for (machinex <- machines) {
        sauv.println(machinex.ecrirecsv)
      }
      sauv.close()
      println("\nFichier sauvegardé avec succès.")
    } catch {case ex : java.io.IOException => println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
    case ex: java.nio.file.AccessDeniedException => println(" Erreur : Échec de l’écriture dans " + filename + ". \nLe fichier peut être verrouillé ou en lecture seule.")
    }
  }

  def validatePin(machineId: Int): Boolean = {
    var testPin = ""
    var tentatives = 3
    var access = false

    do {
      testPin = readLine("Entrez le code PIN : \n> ")
      tentatives -= 1
      if (testPin != machines(machineId).pincode)
        println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
    } while (testPin != machines(machineId).pincode && tentatives != 0)

    if (testPin == machines(machineId).pincode) {
      access = true
      println("Accès accordé à la machine " + (machineId+1))
    } else if (testPin != machines(machineId).pincode && tentatives == 0) {
      access = false
      println("\nTrop de tentatives échouées. Fin du programme.")
    }
    access
  }

  def updatePin(machineId: Int): Unit = {
    var newPin = ""
    println("Mise à jour du code PIN pour la machine " + (machineId+1))

    do {
      newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    while (newPin.length != 6 || !newPin.forall(_.isDigit))
    if (newPin.length == 6) {
      machines(machineId).pincode = newPin
      println("Le code PIN a été mis à jour avec succès.")
      Thread.sleep(500)
      println("\nRetour au menu principal...\n")
    }
  }

  def serveClient(machineId: Int): Boolean = {
    val machine = machines(machineId)
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

    if (cafe == 1 && machine.removeIngredient("coffee", 8)) {
      prix_cafe = 2.00
      nom_cafe = "Expresso"
    } else if (cafe == 2 && machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)) {
      prix_cafe = 2.50
      nom_cafe = "Cappuccino"
    } else if (cafe == 3) {
      do {
        taille = readLine("Sélectionnez la taille de votre Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
      }
      while (taille < 1 || taille > 3)

      if (taille == 1 && machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 120)) {
        prix_cafe = 2.70
        nom_cafe = "Latte (Petit)"
      }
      else if (taille == 2 && machine.removeIngredient("coffee", 8) && machine.removeIngredient("milk", 150)) {
        prix_cafe = 3.20
        nom_cafe = "Latte (Moyen)"
      }
      else if (taille == 3 && machine.removeIngredient("coffee", 12) && machine.removeIngredient("milk", 200)) {
        prix_cafe = 3.70
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
      } else if (choix_sucre == 2 && machine.removeIngredient("sugar", 5)) {
        nom_sucre = "Peu (5g)"
      } else if (choix_sucre == 3 && machine.removeIngredient("sugar", 10)) {
        nom_sucre = "Moyen (10g)"
      } else if (choix_sucre == 4 && machine.removeIngredient("sugar", 15)) {
        nom_sucre = "Beaucoup (15g)"
      }
      prix_suppsucre = 0.10 * (choix_sucre - 1)
      if ((cafe != 1) && machines(machineId).milk >= 50) {
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
          machine.removeIngredient("milk", 50 * nbr_doses)
          prix_supplait = nbr_doses * 0.05
        } else if (choix_lait == 2){
          nom_lait = "Lait en supplément : Non"
        } else {
          println("Stock de lait insuffisant")
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
      Thread.sleep(500)
      println("[...]")
      Thread.sleep(1000)
      println("Votre " + nom_cafe + " est prêt ! Bonne dégustation !")
      Thread.sleep(500)
      println("\nRetour au menu principal...\n")
      Thread.sleep(1000)

      return true     // L'instruction return est facultative mais la laisser permet de mieux visualiser la valeur retournée.
    } else {return false}
  }
  def restockMachine(machineId: Int): Unit = {
    val machine = machines(machineId-1)
    println("Entrez les quantités à ajouter : ")

    var ajout_cafe = readLine("Poudre de café (g) > ").toInt
    while(ajout_cafe < 0){ajout_cafe = readLine("Erreur, saisissez à nouveau la quantité. \nPoudre de café (g) > ").toInt}

    var ajout_lait = readLine("Lait (L) > ").toInt * 1000
    while(ajout_lait < 0){ajout_lait = readLine("Erreur, saisissez à nouveau la quantité. \nLait (L) > ").toInt * 1000}

    var ajout_sucre = readLine("Sucre (g) > ").toInt
    while(ajout_sucre < 0){ajout_sucre = readLine("Erreur, saisissez à nouveau la quantité. \nSucre (g) > ").toInt}

    machine.addIngredient("milk", ajout_lait)
    machine.addIngredient("sugar", ajout_sucre)
    machine.addIngredient("coffee", ajout_cafe)

    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
    Thread.sleep(1000)
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    loadcsv(filename)
    var machineId = 0
    var mode = 0
    var access = false

    do {
      mode = readLine("         Nospresso Café \nSélectionnez le mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt

      if(mode < 1 || mode > 3){
        do {mode = readLine("Erreur. Sélectionnez le mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt}
        while (mode < 1 || mode > 3)
      }

      if(mode == 1 || mode == 2){
        do {machineId = readLine("\nVeuillez sélectionnez la machine (1-"+ machines.size+ ")\n > ").toInt}
        while (machineId < 1 || machineId > machines.size)
        println("Numéro de machine : " + machineId)
      }
      if (mode == 1) {
        serveClient(machineId-1)
      }

      if (mode == 2) {
        println("L'accès au mode Admin requiert un code.")
        access = validatePin(machineId-1)

        if (!access) {mode = 3}
        if (access) {
          println("\n-- Information : le PIN est " + machines(machineId-1).pincode + " --")
          // Indication : Montrer le PIN une fois que l'accès au mode Admin a été validé permet non seulement de vérifier le PIN actuel de la machine, mais aussi le cas échéant,
          // de vérifier après une modification du PIN qu'il est attribué à la bonne machine.
          var choix = 0
          do {choix = readLine("\nQue souhaitez-vous faire ? \n1) Réapprovisionner les stocks \n2) Changer le PIN \n3) Quitter\n> ").toInt}
          while (choix < 1 || choix > 3)

          if(choix == 1){
            println("Stocks : \nPoudre de café : " + machines(machineId-1).coffee + " g\nLait : " + machines(machineId-1).milk/1000.0 + " L\nSucre : " + machines(machineId-1).sugar + " g")
            restockMachine(machineId)
          }
          else if (choix == 2) {
            updatePin(machineId-1)
          }
          if (choix == 3) {
            println("\nRetour au menu principal... \n")
            Thread.sleep(1000)
          }
        }
      }
    } while (mode != 3)
    if(mode == 3){
      savecsv(filename, machines)
      println("Fermeture du programme.")}
  }
}
