import collection.mutable.ArrayBuffer
import scala.io.StdIn._
import java.io.{FileWriter, PrintWriter}
import scala.io.Source._
import util._

object Main {
  //Twint
  var twint = ""
  val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  //validation des entrées
  var valide = false

  //vérification des stocks
  var suffit = false

  //commande réussie
  var successucre = false
  var succescafe = false
  var succeslait = false

  //choix de l'utilisateur
  var mode = 0
  var cafe = 0
  var sucre = 0
  var lait = 0
  var dose = 0
  var taille = 0

  //affichage des choix
  var boisson = ""
  var qtesucre = ""
  var laitsupp = ""
  var chxtaille = ""

  var varcafe = 0
  var varlait = 0
  var varsucre = 0
  var prixcafe = 0.0
  var prixsucre = 0.0
  var prixlait = 0.0
  var nummachine = 0
  var machineId = 0
  var error = false

  //sortie du programme
  var fin = false
  class Machine(val id : Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def affichage(): Unit = {
      val liter: Double = milk / 1000.0
      println("Machine " + id + " chargée :")
      println("ID: " + id)
      println("Code PIN: " + pincode)
      println("Lait: " + liter + "L")
      println("Sucre: " + sugar + "g")
      println("Café: " + coffee + "g")
      println()
    }

    def imprimerCSV(): String = {
      pincode + "," + milk + "," + sugar + "," + coffee
    }

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "café") {
        if (amount >= 0) {
          coffee += amount
          println(amount + " g de café ont été ajoutés.")
        } else {
          println("La quantité de café doit être positive.")
        }
      }
      else if (ingredient == "sucre") {
        if (amount >= 0) {
          sugar += amount
          println(amount + " g de sucre ont été ajoutés.")
        } else {
          println("La quantité de sucre doit être positive.")
        }
      }
      else if (ingredient == "lait") {
        if (amount >= 0) {
          milk += amount
          println(amount + " ml de lait ont été ajoutés.")
        } else {
          println("La quantité de lait doit être positive.")
        }
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "café") {
        if (amount >= 0 && coffee >= amount) {
          coffee -= amount
          println(amount + " g de café ont été retirés.")
          true
        } else {
          println("La quantité de café à retirer doit être positive et inférieure ou égale à la quantité disponible.")
          false
        }
      }
      else if (ingredient == "sucre") {
        if (amount >= 0 && sugar >= amount) {
          sugar -= amount
          println(amount + " g de sucre ont été retirés.")
          true
        } else {
          println("La quantité de sucre à retirer doit être positive et inférieure ou égale à la quantité disponible.")
          false
        }
      }
      else if (ingredient == "lait") {
        if (amount >= 0 && milk >= amount) {
          milk -= amount
          println(amount + " ml de lait ont été retirés.")
          true
        } else {
          println("La quantité de lait à retirer doit être positive et inférieure ou égale à la quantité disponible.")
          false
        }

      }
      else {
        println("ingrédient non reconnu")
        false
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = fromFile(filename)
      val machines = new ArrayBuffer[Machine]()
      var idCounter = 1

      for (line <- fr.getLines().drop(1)) {
        println("Reading line: "+line)
        val machine = line.split(",")
        val id = idCounter
        val pincode = machine(0)
        val milk = machine(1).toInt
        val sugar = machine(2).toInt
        val coffee = machine(3).toInt
        machines += new Machine(idCounter, pincode, milk, sugar, coffee)
        idCounter += 1
        }
      fr.close()
      machines

    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez")
        ArrayBuffer.empty[Machine]
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(filename, false))
      writer.println("Pincode,Milk,Sugar,Coffee")
      machines.foreach { machine =>
        writer.println(machine.imprimerCSV())
      }
      writer.close()
      println("Les données des machines ont été sauvegardées avec succès dans le fichier machines.csv.")
    } catch {
        case _: Exception =>
        println("Erreur : Échec de l'écriture dans machines.csv. \n Le fichier peut être verrouillé ou en lecture seule.")
        error = true

    }
  }


  def validationentree(entreeclient: Int, valeurmax: Int): Boolean = {
    if ((entreeclient < 1) || (entreeclient > valeurmax)) {
      println("Votre entrée ne correspond pas aux options, veuillez sélectionner une valeur entre 1 et " + valeurmax)
      valide = false
    }
    else {
      valide = true
    }
    valide
  }

  def serveClient(machine: Machine): Boolean = {


    do {
      print("Veuillez sélectionner votre boisson :\n" +
        "1) Expresso - CHF 2.00\n" +
        "2) Cappuccino - CHF 2.50\n" +
        "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" +
        ">")
      cafe = readInt()
      valide = validationentree(cafe, 3)
    } while (!valide)

    if (cafe == 1) {
      //Expresso
      boisson = "Expresso"
      varcafe = 8
      prixcafe = 2
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
    }
    else if (cafe == 2) {
      boisson = "Cappuccino"
      varcafe = 6
      varlait = 100
      prixcafe = 2.5
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
      do {
        print("Souhaitez-vous ajouter du lait en supplément ?\n" +
          "(Disponible uniquement pour Cappuccino et Latte)\n" +
          "1) Oui\n" +
          "2) Non\n" +
          ">")
        lait = readInt()
        valide = validationentree(lait, 2)
      } while (!valide)
      if (lait == 1) {
        do {
          print("Combien de doses ?\n" +
            ">")
          dose = readInt()
          valide = validationentree(dose, 3)
        } while (!valide)
        prixlait = 0.05 * dose
        varlait = varlait + (50 * dose)
        laitsupp = "Oui"
      }
      else {
        laitsupp = "Non"
      }
    }
    else if (cafe == 3) {
      do {
        boisson = "Latte"
        println("Quelle taille?\n" +
          "1) Petit\n" +
          "2) Moyen\n" +
          "3) Grand\n")
        taille = readInt()
        valide = validationentree(taille, 3)
      } while (!valide)
      if (taille == 1) {
        varcafe = 6
        varlait = 120
        prixcafe = 2.7
        chxtaille = " (Petit)"
      }
      else if (taille == 2) {
        varcafe = 8
        varlait = 150
        prixcafe = 3.2
        chxtaille = " (Moyen)"
      }
      else {
        varcafe = 12
        varlait = 200
        prixcafe = 3.7
        chxtaille = " (Grand)"
      }
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
      do {
        print("Souhaitez-vous ajouter du lait en supplément ?\n" +
          "(Disponible uniquement pour Cappuccino et Latte)\n" +
          "1) Oui\n" +
          "2) Non\n" +
          ">")
        lait = readInt()
        valide = validationentree(lait, 2)
      } while (!valide)
      if (lait == 1) {
        do {
          print("Combien de doses ?\n" +
            ">")
          dose = readInt()
          valide = validationentree(dose, 3)
        } while (!valide)
        prixlait = 0.05 * dose
        varlait = varlait + (50 * dose)
        laitsupp = "Oui"
      }
      else {
        laitsupp = "Non"
      }
    }

    // Vérification des stocks de chaque ingrédient dans la machine
    successucre = stocks(machine.sugar, varsucre)
     succescafe = stocks(machine.coffee, varcafe)
     succeslait = stocks(machine.milk, varlait)

    // Réussite
    if (successucre && succeslait && succescafe) {
      // Affichage du prix et demande de paiement
      if ((sucre > 1) && (lait == 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
          "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixlait, prixcafe + prixsucre + prixlait)
        println("\n")
      }
      else if ((sucre > 1) && (lait != 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille +"\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixcafe + prixsucre)
        println("\n")
      }
      else if ((sucre == 1) && (lait == 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
          "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixlait, prixcafe + prixlait)
        println("\n")
      }
      else if ((sucre == 1) && (lait != 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Prix total : CHF %.2f", prixcafe)
        println("\n")
      }

      // Interface de paiement
      for (_ <- 1 to 5) {
        twint += caracteres(Random.nextInt(caracteres.length))
      }
      println("Veuillez payer en utilisant Twint.\n" +
        "Votre code de paiement est : " + twint + "\n" +
        "(En attente de validation du paiement...)\n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.\n")

      // Mise à jour des stocks de la machine

      machine.removeIngredient("café", varcafe)
      machine.removeIngredient("sucre", varsucre)
      machine.removeIngredient("lait", varlait)


      println("Préparation de votre boisson...\n" +
        "[...]\n")
      Thread.sleep(3000)
      println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")

      // Remise à zéro des valeurs
      varsucre = 0
      varcafe = 0
      varlait = 0
      twint = ""
      prixcafe = 0
      prixlait = 0
      prixsucre = 0

      return true
    }
    else {
      varsucre = 0
      varcafe = 0
      varlait = 0
      twint = ""
      prixcafe = 0
      prixlait = 0
      prixsucre = 0
      if (!successucre && succeslait && succescafe) {
        println("Quantité de sucre insuffisante, choisissez moins de sucre.")
      }
      else if (successucre && !succeslait && succescafe) {
        println("Quantité de lait insuffisante, choisissez moins de lait, une autre boisson, ou une autre taille.")
      }
      else if (successucre && succeslait && !succescafe) {
        println("Quantité de café insuffisante, choisissez une autre boisson, ou une autre taille.")
      }
      else {println ("Ingrédients insuffisants. Réapprovisionnez les stocks dans le mode admin, ou choisissez une autre boisson.")}
    }

    false
  }
  def stocks(stock : Int, variation :Int): Boolean ={
    if (stock>=variation) {
      suffit = true
    }
    else {
      suffit = false
    }
    suffit
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var attempts = 3
    var isValid = false
    val machine = machines.find(_.id == nummachine).getOrElse {
      println("Machine avec ID " + machineId + " introuvable.")
      return false
    }

    while (attempts > 0 && !isValid) {
      print("Entrez le code PIN pour la machine " + {machine.id}+":\n> ")
      val inputPin = readLine()
      if (inputPin == machine.pincode) {
        println("Accès accordé à la machine " + {machine.id}+".")
        isValid = true
      } else {
        attempts -= 1
        if (attempts > 0) println("Code PIN incorrect. "+attempts+" tentative(s) restante(s).")
      }
    }

    if (!isValid) {
      println("Trop de tentatives échouées.")
    }

    isValid
  }
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la Machine "+ {machineId + 1}+".")
    var isValid = false
    while (!isValid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = readLine()
      if (newPin.length == 6) {
        val machine = machines.find(_.id == nummachine).getOrElse {
          println("Machine avec ID "+machineId+" introuvable.")
          return
        }
        machine.pincode = newPin
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
        Thread.sleep(2000)
        isValid = true
      } else {
        println("Le code PIN doit contenir exactement 6 chiffres.")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    val machines = loadcsv("machines.csv")
    machines.foreach(_.affichage())
    Thread.sleep(1000)
    if (machines.isEmpty) {
      fin = true
      error = true
    }


    while (!fin) {
      println({
        machines.length
      } + " machine(s) chargée(s) avec succès.")
      mode = 0
      do {
        print("        Nospresso Café\n" +
          "Veuillez sélectionner votre mode :\n" +
          "1) Client\n" +
          "2) Admin\n" +
          "3) Quitter\n" +
          ">")
        mode = readInt()
        validationentree(mode, 3)
      } while (!valide)

      if (mode == 1) {
        var machineId = 0
        do {
          println("Machine sélectionnée (1-5) : ")
          nummachine = readInt()
          machineId = nummachine - 1
          validationentree(nummachine, machines.length)
        } while (!valide)
        println("Machine sélectionnée: " + nummachine)
        serveClient(machines(machineId))
        savecsv("machines.csv", machines)

      }
      else if (mode == 2) {
        do {
          println("Machine sélectionnée (1-5) :")
          nummachine = readInt()
          machineId = nummachine - 1
          validationentree(nummachine, machines.length)
        } while (!valide)
        println("Machine sélectionnée: " + nummachine)
        if (validatePin(machineId, machines)) {
          println("1) Réapprovisionner  2) Mettre à jour le code PIN")
          val adminChoice = readInt()
          if (adminChoice == 1) {
            println("Avant ajout :")
            println("Stock de café: "+{machines(machineId).coffee}+"g")
            println("Stock de sucre: "+{machines(machineId).sugar}+"g")
            println("Stock de lait: "+{machines(machineId).milk .toDouble/1000.0 }+"L")
            println("Entrez les quantités à ajouter :")
            print("Poudre de café (en grammes) > ")
            val ajoutcafe = readInt()
            machines(machineId).addIngredient("café", ajoutcafe)
            print("Sucre (en grammes) > ")
            val ajoutsucre = readInt()
            machines(machineId).addIngredient("sucre", ajoutsucre)
            print("Lait (en litres) > ")
            val ajoutlaitL = readDouble()
            val ajoutlait = (ajoutlaitL * 1000).toInt
            machines(machineId).addIngredient("lait", ajoutlait)
            savecsv("machines.csv", machines)
          } else if (adminChoice == 2) {
            updatePin(machineId, machines)
            savecsv("machines.csv", machines)
          }
        } else {
          println("Fin du programme.")
          fin = true
        }
      }
      else {
        println("Fin du programme.")
        fin = true
        savecsv("machines.csv", machines)
      }
    }
    if (error) {
      println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")

    }
  }
}