import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import scala.io.StdIn.readLine

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") milk += amount
    else if (ingredient == "sugar") sugar += amount
    else if (ingredient == "coffee") coffee += amount
    else println("Ingrédient inconnu : " + ingredient)
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println("Stock insuffisant pour : " + ingredient)
      false
    }
  }
}

object Main {
  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    try {
      machines.appendAll(loadcsv(filename))
    } catch {
      case _: IOException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        System.exit(1)
    }

    if (machines.isEmpty) {
      println("Erreur : Aucune machine n'a été trouvée dans le fichier. Vérifiez le contenu de machines.csv.")
      System.exit(1)
    }

    machines.foreach { machine =>
      println("Machine " + machine.id + " chargée :")
      println(" ID: " + machine.id)
      println(" Code PIN: " + machine.pincode)
      println(" Lait: " + f"${machine.milk / 1000.0}%.3fL")
      println(" Sucre: " + machine.sugar + "g")
      println(" Café: " + machine.coffee + "g\n")
    }
    println(machines.length + " machine(s) chargée(s) avec succès.")

    var programmeActif = true
    while (programmeActif) {
      println("         Nospresso Café")
      println("Veuillez sélectionner une machine (1 à " + machines.length + ") :")
      var choixMachine = -1
      while (choixMachine < 1 || choixMachine > machines.length) {
        println("Entrez un nombre entre 1 et " + machines.length + " :")
        try {
          choixMachine = readLine("> ").toInt
          if (choixMachine < 1 || choixMachine > machines.length) {
            println("Choix invalide. Veuillez réessayer.")
          }
        } catch {
          case _: NumberFormatException =>
            println("Entrée invalide. Veuillez entrer un nombre valide.")
        }
      }
      val machine = machines(choixMachine - 1)

      println("Veuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      var mode = -1
      while (mode < 1 || mode > 3) {
        try {
          mode = readLine("> ").toInt
          if (mode < 1 || mode > 3) {
            println("Choix invalide. Veuillez entrer 1, 2 ou 3.")
          }
        } catch {
          case _: NumberFormatException =>
            println("Entrée invalide. Veuillez entrer un nombre valide.")
        }
      }

      if (mode == 1) {
        if (!serveClient(machine)) {
          println("Transaction échouée. Arrêt du programme.")
          programmeActif = false
        }
      } else if (mode == 2) {
        if (validatePin(machine)) {
          adminMenu(machine)
        } else {
          println("Accès refusé. Trop de tentatives échouées. Arrêt du programme.")
          programmeActif = false
        }
      } else if (mode == 3) {
        println("Merci de votre visite. À bientôt !")
        programmeActif = false
      }
    }

    try {
      savecsv(filename, machines)
    } catch {
      case _: IOException =>
        println("Erreur : Échec de l’écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule.")
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machinesBuffer = ArrayBuffer[Machine]()
    println("Chargement des machines depuis machines.csv...")
    val lines = Source.fromFile(filename).getLines().drop(1)
    lines.foreach { line =>
      val cols = line.split(",").map(_.trim)
      if (cols.length == 4) {
        val Array(pincode, milk, sugar, coffee) = cols
        val machine = new Machine(machinesBuffer.length + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machinesBuffer += machine
      }
    }
    machinesBuffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val writer = new BufferedWriter(new FileWriter(filename))
    writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
    machines.foreach { machine =>
      writer.write(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee + "\n")
    }
    writer.close()
    println("Fichier sauvegardé avec succès.")
  }

  def validatePin(machine: Machine): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN pour la machine " + machine.id + " :")
      val pin = readLine("> ")
      if (pin.trim == machine.pincode.trim) return true
      attempts -= 1
      println("Code PIN incorrect. Il vous reste " + attempts + " tentative(s).")
    }
    println("Code PIN incorrect. Aucune tentative restante.")
    false
  }

  def adminMenu(machine: Machine): Unit = {
    println("Accès administrateur accordé.")
    println("1) Réapprovisionner\n2) Mettre à jour le code PIN")
    var actionAdmin = -1
    while (actionAdmin < 1 || actionAdmin > 2) {
      println("Entrez 1 ou 2 :")
      try {
        actionAdmin = readLine("> ").toInt
        if (actionAdmin < 1 || actionAdmin > 2) {
          println("Choix invalide. Veuillez réessayer.")
        }
      } catch {
        case _: NumberFormatException =>
          println("Entrée invalide. Veuillez entrer un nombre valide.")
      }
    }

    if (actionAdmin == 1) {
      restockMachine(machine)
    } else if (actionAdmin == 2) {
      updatePin(machine)
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println("Réapprovisionnement des stocks :")
    println("Stock actuel de café : " + machine.coffee + " g. Entrez la quantité à ajouter (en g) :")
    var amount = -1
    while (amount < 0) {
      println("Veuillez entrer un nombre positif.")
      try {
        amount = readLine("> ").toInt
      } catch {
        case _: NumberFormatException =>
          println("Entrée invalide. Veuillez entrer un nombre valide.")
      }
    }
    machine.addIngredient("coffee", amount)

    println("Stock actuel de sucre : " + machine.sugar + " g. Entrez la quantité à ajouter (en g) :")
    amount = -1
    while (amount < 0) {
      println("Veuillez entrer un nombre positif.")
      try {
        amount = readLine("> ").toInt
      } catch {
        case _: NumberFormatException =>
          println("Entrée invalide. Veuillez entrer un nombre valide.")
      }
    }
    machine.addIngredient("sugar", amount)

    println("Stock actuel de lait : " + f"${machine.milk / 1000.0}%.3fL. Entrez la quantité à ajouter (en L) :")
    var litres = -1.0
    while (litres < 0) {
      println("Veuillez entrer un nombre positif (en litres).")
      try {
        litres = readLine("> ").toDouble
      } catch {
        case _: NumberFormatException =>
          println("Entrée invalide. Veuillez entrer un nombre valide.")
      }
    }
    machine.addIngredient("milk", (litres * 1000).toInt)
    println("Réapprovisionnement terminé.")
  }

  def updatePin(machine: Machine): Unit = {
    println("Mettre à jour le code PIN pour la machine " + machine.id + ".")
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var newPin = ""
    while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
      newPin = readLine("> ")
      if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        println("Code PIN invalide. Il doit contenir exactement 6 chiffres.")
      }
    }
    machine.pincode = newPin
    println("Code PIN mis à jour avec succès.")
  }

  def serveClient(machine: Machine): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - 3.20 CHF")

    var choix = readLine("> ").toInt
    while (choix < 1 || choix > 3) {
      println("Choix invalide. Veuillez entrer un nombre entre 1 et 3.")
      choix = readLine("> ").toInt
    }

    var prixBoisson = 0.0
    var cafeRequis = 0
    var laitRequis = 0
    var sucreRequis = 0
    var descriptionBoisson = ""

    if (choix == 1) {
      prixBoisson = 2.00
      cafeRequis = 8
      descriptionBoisson = "Expresso"
    } else if (choix == 2) {
      prixBoisson = 2.50
      cafeRequis = 6
      laitRequis = 100
      descriptionBoisson = "Cappuccino"
    } else if (choix == 3) {
      println("Veuillez choisir la taille de votre Latte :")
      println("1) Petit 2.70 CHF")
      println("2) Moyen 3.20 CHF")
      println("3) Grand 3.70 CHF")

      var taille = readLine("> ").toInt
      while (taille < 1 || taille > 3) {
        println("Choix invalide. Veuillez entrer un nombre entre 1 et 3.")
        taille = readLine("> ").toInt
      }

      if (taille == 1) {
        prixBoisson = 2.70
        cafeRequis = 6
        laitRequis = 120
        descriptionBoisson = "Latte (Petit)"
      } else if (taille == 2) {
        prixBoisson = 3.20
        cafeRequis = 8
        laitRequis = 150
        descriptionBoisson = "Latte (Moyen)"
      } else if (taille == 3) {
        prixBoisson = 3.70
        cafeRequis = 12
        laitRequis = 200
        descriptionBoisson = "Latte (Grand)"
      }
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10 CHF")
    println("3) Moyen (10g) - 0.20 CHF")
    println("4) Beaucoup (15g) - 0.30 CHF")

    var sucre = readLine("> ").toInt
    while (sucre < 1 || sucre > 4) {
      println("Choix invalide. Veuillez entrer un nombre entre 1 et 4.")
      sucre = readLine("> ").toInt
    }

    var prixSucre = 0.00
    var sucreRecap = "Sans sucre"
    if (sucre == 2) {
      prixSucre = 0.10
      sucreRequis = 5
      sucreRecap = "Peu (5g)"
    } else if (sucre == 3) {
      prixSucre = 0.20
      sucreRequis = 10
      sucreRecap = "Moyen (10g)"
    } else if (sucre == 4) {
      prixSucre = 0.30
      sucreRequis = 15
      sucreRecap = "Beaucoup (15g)"
    }

    var prixLaitSup = 0.0
    var laitSupplementaire = 0
    var laitsupRecap = "Non"
    if (choix == 2 || choix == 3){
    println("Souhaitez-vous ajouter du lait supplémentaire ? (1) Oui (2) Non")
    var laitSup = readLine("> ").toInt
    while (laitSup < 1 || laitSup > 2) {
      println("Choix invalide. Veuillez entrer 1 ou 2.")
      laitSup = readLine("> ").toInt
    }
    if (laitSup == 1) {
      println("Combien de doses supplémentaires ?\n 1) 50mL\n 2) 100mL\n 3) 150mL")
      var doses = readLine("> ").toInt
      while (doses < 1 || doses > 3) {
        println("Choix invalide. Veuillez entrer un nombre entre 1 et 3.")
        doses = readLine("> ").toInt
      }

      laitSupplementaire = doses * 50
      prixLaitSup = doses * 0.05
      if (machine.milk >= laitSupplementaire) {
        laitsupRecap = "Oui (" + doses + " dose(s))"
      } else {
        println("Stock de lait insuffisant pour ajouter cette dose.")
        laitSupplementaire = 0
        prixLaitSup = 0.00
      }
    }
    }

    println("\nRécapitulatif de la commande :")
    println("Boisson : " + descriptionBoisson)
    println("Niveau de sucre : " + sucreRecap)
    println("Lait supplémentaire : " + laitsupRecap)

    val prixTotal = prixBoisson + prixSucre + prixLaitSup
    println("Prix total : CHF " + prixBoisson + " + CHF " + prixSucre + " + CHF " + prixLaitSup + " = CHF " + prixTotal)

    if (!machine.removeIngredient("coffee", cafeRequis)) {
      println("\nErreur : Quantité de café insuffisante pour préparer votre boisson.")
      return false
    }
    if (laitRequis > 0 && !machine.removeIngredient("milk", laitRequis)) {
      println("\nErreur : Quantité de lait insuffisante pour préparer votre boisson.")
      return false
    }
    if (sucreRequis > 0 && !machine.removeIngredient("sugar", sucreRequis)) {
      println("\nErreur : Quantité de sucre insuffisante pour préparer votre boisson.")
      return false
    }

    println("\nVeuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + genererCodePaiement())
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("\nPaiement confirmé.")
    println("Préparation de votre boisson...")
    println("Votre " + descriptionBoisson + " est prêt ! Bonne dégustation !")
    Thread.sleep(5000)
    true
  }

  def genererCodePaiement(): String = {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to 6).map(_ => alphabet.charAt((Math.random() * alphabet.length).toInt)).mkString
  }
}
