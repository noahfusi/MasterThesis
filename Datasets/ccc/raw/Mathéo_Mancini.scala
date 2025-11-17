import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.io.Source
import scala.io.StdIn._
import java.io.{File, PrintWriter}

class Machine(val id: Int, var pincode: String, var lait: Int, var sucre: Int, var caffe: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "lait") {
      lait += amount
    } else if (ingredient == "sucre") {
      sucre += amount
    } else if (ingredient == "caffe") {
      caffe += amount
    } else {
      println("Ingrédient inconnu.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "lait" && lait >= amount) {
      lait -= amount
      true
    } else if (ingredient == "sucre" && sucre >= amount) {
      sucre -= amount
      true
    } else if (ingredient == "caffe" && caffe >= amount) {
      caffe -= amount
      true
    } else {
      println("Stock insuffisant pour " + ingredient + " (quantité disponible : " +
        (if (ingredient == "lait") lait else if (ingredient == "sucre") sucre else if (ingredient == "caffe") caffe else 0) + ").")
      false
    }
  }
}

object Nospresso {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def createDefaultFile(filename: String): Unit = {
    val file = new File(filename)
    if (!file.exists()) {
      println("Fichier " + filename + " introuvable. Création du fichier avec des données par défaut.")
      val writer = new PrintWriter(file)
      writer.println("PINCODE,LAIT,SUCRE,CAFFE")
      writer.println("434343,500,200,300")
      writer.println("123456,400,150,250")
      writer.close()
      println("Fichier " + filename + " créé avec succès.")
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"

    createDefaultFile(filename)

    machines = loadcsv(filename)

    var running = true
    while (running) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choix = readLine()
      if (choix == "1") {
        clientMode()
      } else if (choix == "2") {
        adminMode()
      } else if (choix == "3") {
        savecsv(filename, machines)
        println("Fichier " + filename + " sauvegardé avec succès.")
        running = false
      } else {
        println("Option invalide.")
      }
    }
  }

  def clientMode(): Unit = {
    println("Sélectionnez une machine (1-" + machines.size + "):")
    val machineId = readInt() - 1
    if (isValidMachine(machineId)) {
      val success = serveClient(machines(machineId))
      if (!success) println("Transaction échouée.")
    } else {
      println("Numéro de machine invalide.")
    }
  }

  def adminMode(): Unit = {
    println("Sélectionnez une machine (1-" + machines.size + "):")
    val machineId = readInt() - 1
    if (isValidMachine(machineId) && validatePin(machines(machineId))) {
      val machine = machines(machineId)

      println("Stocks actuels de la machine " + machine.id + ":")
      println("  Lait : " + machine.lait + " ml")
      println("  Sucre : " + machine.sucre + " g")
      println("  Café : " + machine.caffe + " g")

      println("1) Réapprovisionnement des stocks")
      println("2) Changer le code PIN")
      val adminChoix = readInt()
      if (adminChoix == 1) {
        restockMachine(machine)
      } else if (adminChoix == 2) {
        updatePin(machine)
      } else {
        println("Option invalide.")
      }
    } else {
      println("Accès refusé.")
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN :")
      val enteredPin = readLine()
      if (enteredPin == machine.pincode) return true
      attempts -= 1
      println("Code incorrect. Tentatives restantes : " + attempts)
    }
    println("Trop de tentatives échouées.")
    false
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN (6 chiffres) :")
    var newPin = ""
    var valide = false
    while (!valide) {
      newPin = readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machine.pincode = newPin
        println("Code PIN mis à jour avec succès.")
        valide = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres.")
      }
    }
  }

  def serveClient(machine: Machine): Boolean = {
    println("Choisissez votre boisson :")
    println("1) Expresso (8g café, 2.00 CHF)")
    println("2) Cappuccino (6g café, 100ml lait, 2.50 CHF)")
    println("3) Latte (Petit, Moyen, Grand)")

    val choix = readInt()
    var caffe = 0
    var lait = 0
    var prixBase = 0.0

    if (choix == 1) {
      caffe = 8
      lait = 0
      prixBase = 2.00
    } else if (choix == 2) {
      caffe = 6
      lait = 100
      prixBase = 2.50
    } else if (choix == 3) {
      println("Taille : 1) Petit (120ml lait, 2.70 CHF) 2) Moyen (150ml lait, 3.20 CHF) 3) Grand (200ml lait, 3.70 CHF)")
      val taille = readInt()
      if (taille == 1) {
        caffe = 6; lait = 120; prixBase = 2.70
      } else if (taille == 2) {
        caffe = 8; lait = 150; prixBase = 3.20
      } else if (taille == 3) {
        caffe = 12; lait = 200; prixBase = 3.70
      } else {
        println("Taille invalide.")
        return false
      }
    } else {
      println("Option invalide.")
      return false
    }

    val sucreQuantites = Array(0, 5, 10, 15)
    val sucrePrix = Array(0.0, 0.1, 0.2, 0.3)

    println("Choisissez la quantité de sucre :")
    println("1) Pas de sucre (0g)")
    println("2) Peu de sucre (5g, +0.10 CHF)")
    println("3) Moyen (10g, +0.20 CHF)")
    println("4) Beaucoup (15g, +0.30 CHF)")

    val sucreChoix = readInt()
    var sucreUtilise = 0
    var prixSucre = 0.0

    if (sucreChoix >= 1 && sucreChoix <= 4) {
      sucreUtilise = sucreQuantites(sucreChoix - 1)
      prixSucre = sucrePrix(sucreChoix - 1)
    } else {
      println("Choix invalide. Pas de sucre ajouté.")
    }

    var laitSupplement = 0
    var prixLaitSupplement = 0.0

    if (choix == 2 || choix == 3) {
      println("Souhaitez-vous ajouter du lait supplémentaire ? (1) Oui / (2) Non")
      val reponseLait = readInt()
      if (reponseLait == 1) {
        println("Combien de doses supplémentaires de lait ? (50ml/dose, 0.05 CHF par dose)")
        val doses = readInt()
        laitSupplement = doses * 50
        prixLaitSupplement = doses * 0.05
      }
    }

    val totalLait = lait + laitSupplement
    val prixTotal = prixBase + prixSucre + prixLaitSupplement

    if (machine.caffe < caffe || machine.lait < totalLait || machine.sucre < sucreUtilise) {
      println("Stock insuffisant pour la préparation de cette boisson.")
      return false
    }

    effectuerPaiement(prixTotal)

    machine.removeIngredient("caffe", caffe)
    machine.removeIngredient("lait", totalLait)
    machine.removeIngredient("sucre", sucreUtilise)

    println("Votre boisson est prête ! Bonne dégustation.")
    true
  }

  def effectuerPaiement(price: Double): Unit = {
    val code = Random.alphanumeric.take(5).mkString.toUpperCase
    println("Veuillez payer en utilisant Twint. Prix total : " + "%.2f".format(price) + " CHF")
    println("Votre code de paiement est : " + code)
    println("En attente de validation du paiement...")
    Thread.sleep(3000)
    println("Paiement validé.")
  }

  def restockMachine(machine: Machine): Unit = {
    println("Quantité à ajouter (café en g) :")
    machine.addIngredient("caffe", readInt())
    println("Quantité à ajouter (sucre en g) :")
    machine.addIngredient("sucre", readInt())
    println("Quantité à ajouter (lait en ml) :")
    machine.addIngredient("lait", readInt())
    println("Réapprovisionnement terminé.")
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis : " + filename)

    try {
      val file = Source.fromFile(filename)
      val lines = file.getLines().drop(1)

      var idx = 1
      for (line <- lines) {
        val data = line.split(",")
        val machine = new Machine(
          id = idx,
          pincode = data(0),
          lait = data(1).toInt,
          sucre = data(2).toInt,
          caffe = data(3).toInt
        )

        machines += machine

        println("Machine " + machine.id + " chargée :")
        println("  PINCODE : " + machine.pincode)
        println("  Lait : " + machine.lait + " ml")
        println("  Sucre : " + machine.sucre + " g")
        println("  Café : " + machine.caffe + " g")

        idx += 1
      }

      file.close()
      println("Chargement réussi de " + machines.size + " machine(s).")

    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        System.exit(1)
      case ex: Exception =>
        println("Échec de l’écriture dans machines : " + ex.getMessage)
        System.exit(1)
    }

    machines
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(filename)
      writer.println("PINCODE,LAIT,SUCRE,CAFFE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.lait + "," + machine.sucre + "," + machine.caffe)
      }
      writer.close()
      println("Sauvegarde réussie dans " + filename)
    } catch {
      case _: Exception =>
        println("Erreur : Échec de la sauvegarde des machines.")
        System.exit(1)
    }
  }


  def isValidMachine(machineId: Int): Boolean = machineId >= 0 && machineId < machines.size
}