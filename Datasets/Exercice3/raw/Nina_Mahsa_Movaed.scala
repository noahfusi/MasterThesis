import io.StdIn.readLine
import util.Random
import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Using}
import java.io.{File, PrintWriter}

object MachineCafe {

  // Constantes
  val nbMachines = 5

  // Tableaux pour les stocks de chaque machine
  var poudreCafe = Array.fill(nbMachines)(50)
  var sucre = Array.fill(nbMachines)(30)
  var lait = Array.fill(nbMachines)(500)

  // Codes PIN pour chaque machine
  val machinePins = Array.fill(nbMachines)("434343")

  // Collection dynamique des machines
  val machines = ArrayBuffer[Machine]()

  def main(args: Array[String]): Unit = {
    println("Nospresso Café")

    // Chargement des machines depuis un fichier CSV
    chargerMachines("machines.csv")

    menuPrincipal()

    // Sauvegarde des machines dans un fichier CSV à la sortie
    sauvegarderMachines("machines.csv")
  }

  def menuPrincipal(): Unit = {
    var continuer = true
    while (continuer) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println("4) Afficher les stocks de toutes les machines") // Nouvelle option

      var choix = ""
      var choixValide = false

      while (!choixValide) {
        choix = readLine("> ")
        if (choix == "1" || choix == "2" || choix == "3" || choix == "4") {
          choixValide = true
        } else {
          println("Choix invalide. Veuillez réessayer.")
        }
      }

      if (choix == "1") {
        modeClient()
      } else if (choix == "2") {
        modeAdmin()
      } else if (choix == "3") {
        println("Au revoir !")
        continuer = false
      } else if (choix == "4") {
        afficherStocksToutesMachines()
      }
    }
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner la machine (ID de la machine entre 0 et " + (nbMachines - 1) + "):")
    val machineId = readLine("> ").toInt

    if (machineId >= 0 && machineId < nbMachines) {
      serveClient(machineId, poudreCafe, sucre, lait)
    } else {
      println("ID de machine invalide. Retour au menu principal.")
    }
  }

  def modeAdmin(): Unit = {
    println("Veuillez sélectionner la machine (ID de la machine entre 0 et " + (nbMachines - 1) + "):")
    val machineId = readLine("> ").toInt

    if (machineId >= 0 && machineId < nbMachines) {
      if (validatePin(machineId, machinePins)) {
        println("Accès administrateur autorisé.")

        afficherStocksMachine(machineId)

        println("Souhaitez-vous réapprovisionner la machine ? (1 pour Oui, autre pour Non)")
        val reapprovisionnement = readLine("> ")

        if (reapprovisionnement == "1") {
          restockMachine(machineId, poudreCafe, sucre, lait)
        }

        println("Souhaitez-vous changer le code PIN ? (1 pour Oui, autre pour Non)")
        val changerPin = readLine("> ")

        if (changerPin == "1") {
          updatePin(machineId, machinePins)
        }
      } else {
        println("Code PIN incorrect. Retour au menu principal.")
      }
    } else {
      println("ID de machine invalide. Retour au menu principal.")
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 0
    var pinCorrect = false

    while (tentatives < 3 && !pinCorrect) {
      println("Entrez le code PIN de la machine :")
      val pinEntree = readLine("> ")

      if (pinEntree == machinePins(machineId)) {
        pinCorrect = true
      } else {
        tentatives += 1
        if (tentatives < 3) {
          println(s"Code PIN incorrect. Tentative ${tentatives}/3.")
        }
      }
    }

    pinCorrect
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var pinValide = false

    while (!pinValide) {
      println("Entrez le nouveau code PIN (6 chiffres) :")
      val nouveauPin = readLine("> ")

      if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
        machinePins(machineId) = nouveauPin
        pinValide = true
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Code PIN invalide. Veuillez entrer un code à 6 chiffres.")
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    val choixBoisson = readLine("> ")

    var prixBase = 0.0
    var laitRequis = 0
    var cafeRequis = 0
    var sucreRequis = 0
    var prixSucre = 0.0
    var laitSupplementaire = 0
    var prixLaitSupplementaire = 0.0

    if (choixBoisson == "1") {
      cafeRequis = 8
      prixBase = 2.0
    } else if (choixBoisson == "2") {
      cafeRequis = 6
      laitRequis = 100
      prixBase = 2.5
    } else if (choixBoisson == "3") {
      println("Choisissez la taille de votre Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      val tailleLatte = readLine("> ")

      if (tailleLatte == "1") {
        cafeRequis = 6
        laitRequis = 120
        prixBase = 2.7
      } else if (tailleLatte == "2") {
        cafeRequis = 8
        laitRequis = 150
        prixBase = 3.2
      } else if (tailleLatte == "3") {
        cafeRequis = 12
        laitRequis = 200
        prixBase = 3.7
      }
    }

    if (coffeeStocks(machineId) < cafeRequis || milkStocks(machineId) < laitRequis + laitSupplementaire) {
      println("Quantité d'ingrédients insuffisante pour préparer la boisson.")
      return false
    }

    println("Votre boisson est prête ! Bonne dégustation !")
    true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Quantité de poudre de café à ajouter :")
    coffeeStocks(machineId) += readLine("> ").toInt

    println("Quantité de sucre à ajouter :")
    sugarStocks(machineId) += readLine("> ").toInt

    println("Quantité de lait à ajouter (en ml) :")
    milkStocks(machineId) += readLine("> ").toInt

    println("Stocks mis à jour.")
  }

  // Nouvelle méthode pour charger les machines depuis un fichier CSV
  def chargerMachines(fichier: String): Unit = {
    Try {
      val lignes = Using(scala.io.Source.fromFile(fichier))(_.getLines().drop(1).toList).get
      lignes.foreach { ligne =>
        val Array(pincode, milk, sugar, coffee) = ligne.split(",")
        machines += Machine(machines.size + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
    }.recover {
      case ex => println(s"Erreur lors de la lecture du fichier : ${ex.getMessage}")
    }
  }

  // Nouvelle méthode pour sauvegarder les machines dans un fichier CSV
  def sauvegarderMachines(fichier: String): Unit = {
    Try {
      val writer = new PrintWriter(new File(fichier))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
    }.recover {
      case ex => println(s"Erreur lors de l'écriture dans le fichier : ${ex.getMessage}")
    }
  }

  def afficherStocksMachine(machineId: Int): Unit = {
    println(s"--- Stocks de la machine $machineId ---")
    println(s"Poudre de café : ${poudreCafe(machineId)} g")
    println(s"Sucre : ${sucre(machineId)} g")
    println(s"Lait : ${lait(machineId)} ml")
    println("--------------------------------------")
  }

  def afficherStocksToutesMachines(): Unit = {
    println("Affichage des stocks de toutes les machines :")
    for (i <- 0 until nbMachines) {
      afficherStocksMachine(i)
    }
  }
}

// Classe ajoutée pour la gestion dynamique
case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient inconnu : $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println(s"Stock insuffisant pour $ingredient."); false
    }
  }
}
