import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  val nbMachines = 5 // Nombre total de machines
  val pinAdminDefault = "434343" // PIN initial

  // Stocks initiaux pour chaque machine
  val cafeStocks = Array.fill(nbMachines)(50) // en grammes
  val sucreStocks = Array.fill(nbMachines)(30) // en grammes
  val laitStocks = Array.fill(nbMachines)(500) // en millilitres
  val machinePins = Array.fill(nbMachines)(pinAdminDefault)

  def main(args: Array[String]): Unit = {
    var continuer = true
    var tentatives = 3
    while (continuer && tentatives > 0) {
      println("\nBienvenue chez Nospresso Café - Multi-machines")
      println("Sélectionnez une machine (1 à 5) :")
      val machineId = readLine("> ").toInt

      if (machineId >= 1 && machineId <= nbMachines) {
        menuPrincipal(machineId - 1) // Les indices des machines commencent de 0 dans notre tableau
      } else {
        println("Numéro de machine invalide. Veuillez choisir entre 1 et 5.")
      }
    }
    if (tentatives <= 0) {
      println("Trop de tentatives échouées. Arrêt du programme.")
    } else {
      println("\nMerci d'avoir utilisé Nospresso Café. À bientôt!")
    }
  }

  def menuPrincipal(machineId: Int): Unit = {
    var redemarrer = true
    while (redemarrer) {
      println(s"\n--- Machine ${machineId + 1} ---")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Retour")

      val choix = readLine("> ").toInt
      if (choix == 1) {
        //Si la commande échoue(pas de stock), on retourne à la sélection de la machine sans continuer dans le mode client
        if (!serveClient(machineId, cafeStocks, sucreStocks, laitStocks)) {
          println("Retour au menu principal...")
          redemarrer = false
        }
      } else if (choix == 2) {
        if (!validatePin(machineId, machinePins)) {
          println("Trop de tentatives échouées pour le PIN. Fin du programme.")
          sys.exit(0)
        }
      } else if (choix == 3) {
        redemarrer = false
      } else {
        println("Option invalide.")
      }
    }
  }

  // Validation du code PIN
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val pin = readLine("> ")
      if (pin == machinePins(machineId)) {
        println("Accès autorisé.")
        menuAdmin(machineId)
        return true
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println(s"Code PIN incorrect. Tentatives restantes: $tentatives")
        } else {
          //println("Nombre maximun de tentatives atteint. Programme terminé.")
          //sys.exit(0) //On termine complètement le programme.
        }
      }
    }
    false
  }

  // Menu Administrateur avec réapprovisionnement et mise à jour du code PIN
  def menuAdmin(machineId: Int): Unit = {
    var continuer = true
    while (continuer) {
      println("\nMode Administrateur - Sélectionnez une option :")
      println("1) Réapprovisionner les stocks")
      println("2) Mettre à jour le code PIN")
      println("3) Afficher les stocks")
      println("4) Retour")

      val choix = readLine("> ").toInt
      if (choix == 1) {
        restockMachine(machineId, cafeStocks, sucreStocks, laitStocks)
      } else if (choix == 2) {
        updatePin(machineId, machinePins)
      } else if (choix == 3) {
        afficherStocks(machineId, cafeStocks, sucreStocks, laitStocks)
      } else if (choix == 4) {
        println("Retour au menu principal...")
        continuer = false
      } else {
        println("Option invalide. Veuillez réessayer.")
      }
    }
  }

  // Mise à jour du code PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var newPin = ""
    do {
      newPin = readLine("> ")
      if (newPin.matches("\\d{6}")) {
        machinePins(machineId) = newPin
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Format invalide. Le code PIN doit contenir exactement 6 chiffres.")
      }
    } while (!newPin.matches("\\d{6}"))
  }

  // Réapprovisionnement des stocks
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("\nRéapprovisionnement des stocks :")
    println("Ajoutez la quantité de café (en grammes) :")
    coffeeStocks(machineId) += readLine("> ").toInt
    println("Ajoutez la quantité de sucre (en grammes) :")
    sugarStocks(machineId) += readLine("> ").toInt
    println("Ajoutez la quantité de lait (en litres) :")
    val laitAjoute = readLine("> ").toDouble
    milkStocks(machineId) += (laitAjoute * 1000).toInt // Conversion de litres en millilitres

    println("Stocks mis à jour avec succès.")
    afficherStocks(machineId, coffeeStocks, sugarStocks, milkStocks)
  }

  // Affichage des stocks
  def afficherStocks(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"\nStocks de la machine ${machineId + 1} :")
    println(s"Café : ${coffeeStocks(machineId)}g")
    println(s"Sucre : ${sugarStocks(machineId)}g")
    println(s"Lait : ${milkStocks(machineId) / 1000.0}L")
  }

  // Service Client
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var recommencer = true
    while (recommencer) {
      println("\nMode Client - Sélectionnez une boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      println("4) Retour")

      val choixBoisson = readLine("> ").toInt
      if (choixBoisson == 1) {
        if (!commandeBoisson(machineId, "Expresso", 8, 0, 2.00, coffeeStocks, sugarStocks, milkStocks)) {
          return false
        }
      } else if (choixBoisson == 2) {
        if (!commandeBoisson(machineId, "Cappuccino", 6, 100, 2.50, coffeeStocks, sugarStocks, milkStocks, ajouterLaitSup = true)) {
          return false
        }
      } else if (choixBoisson == 3) {
        if (!tailleLatte(machineId, coffeeStocks, sugarStocks, milkStocks)) {
          return false
        }
      } else if (choixBoisson == 4) {
        recommencer = false
      } else {
        println("Option invalide.")
      }
    }
    true
  }

  def tailleLatte(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Choisissez la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")

    val taille = readLine("> ").toInt
    if (taille == 1) {
      if (!commandeBoisson(machineId, "Latte Petit", 6, 120, 2.70, coffeeStocks, sugarStocks, milkStocks, ajouterLaitSup = true)) {
        return false
      }
    } else if (taille == 2) {
      if (!commandeBoisson(machineId, "Latte Moyen", 8, 150, 3.20, coffeeStocks, sugarStocks, milkStocks, ajouterLaitSup = true)) {
        return false
      }
    } else if (taille == 3) {
      if (!commandeBoisson(machineId, "Latte Grand", 12, 200, 3.70, coffeeStocks, sugarStocks, milkStocks, ajouterLaitSup = true)) {
        return false
      }
    } else {
      println("Option invalide.")
    }
    true
  }

  // Commande pour chaque boisson
  def commandeBoisson(machineId: Int, nom: String, cafeReq: Int, laitReq: Int, prixBase: Double, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], ajouterLaitSup: Boolean = false): Boolean = {
    val sucreAjoute = ajouterSucre()
    val laitSup = if (ajouterLaitSup) ajouterLaitSupplementaire() else (0, 0.0)
    val prixFinal = prixBase + sucreAjoute._2 + laitSup._2

    if (coffeeStocks(machineId) >= cafeReq && sugarStocks(machineId) >= sucreAjoute._1 && milkStocks(machineId) >= (laitReq + laitSup._1)) {
      coffeeStocks(machineId) -= cafeReq
      sugarStocks(machineId) -= sucreAjoute._1
      milkStocks(machineId) -= (laitReq + laitSup._1)
      println(f"Prix total: CHF $prixFinal%.2f")
      println("Votre code de paiement Twint est : " + genererCodePaiement())
      Thread.sleep(3000)
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println(s"Votre $nom est prêt! Bonne dégustation!")
      true
    } else {
      println("Stocks insuffisants. Veuillez choisir une autre machine.")
      false //On retourne en false, si les stocks sont insuffisant
    }
  }

  def ajouterSucre(): (Int, Double) = {
    var choix = 0
    while (choix != 1 && choix != 2 && choix != 3 && choix != 4) {
      println("Ajoutez du sucre :")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")

      val input = readLine(">").toInt
      if (input == 1 || input == 2 || input == 3 || input == 4) {
        choix = input
      } else {
        println("Option invalide, veuillez choisir entre 1 et 4.")
      }
    }

    //var choix = readLine("> ").toInt
    if (choix == 2) {
      (5, 0.10)
    } else if (choix == 3) {
      (10, 0.20)
    } else if (choix == 4) {
      (15, 0.30)
    } else {
      (0, 0.0) // choix 1, sans sucre
    }
  }


  def ajouterLaitSupplementaire(): (Int, Double) = {
    var choix = 0
    while (choix != 1 && choix != 2 && choix != 3 && choix != 4) {
      println("Ajoutez du lait supplémentaire (en litres) :")
      println("1) Non")
      println("2) 1 dose (0.05L) - CHF 0.05")
      println("3) 2 doses (0.1L) - CHF 0.10")
      println("4) 3 doses (0.15L) - CHF 0.15")
      val input = readLine(">").toInt
      if (input == 1 || input == 2 || input == 3 || input == 4) {
        choix = input
      } else {
        println("Option invalide, veillez choisir entre 1 et 4.")
      }
    }

    //val choix = readLine("> ").toInt
    if (choix == 2) {
      (50, 0.05)
    } else if (choix == 3) {
      (100, 0.10)
    } else if (choix == 4) {
      (150, 0.15)
    } else {
      (0, 0.0) // Option sans lait supplémentaire
    }
  }

  def genererCodePaiement(): String = {
    val caracteres = ('A' to 'Z') ++ ('0' to '9')
    (1 to 5).map(_ => caracteres(Random.nextInt(caracteres.length))).mkString
  }

}