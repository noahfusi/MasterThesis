import io.StdIn.readLine
import util.Random

object MachineCafe {

  // Constantes
  val nbMachines = 5

  // Tableaux pour les stocks de chaque machine
  var poudreCafe = Array.fill(nbMachines)(50)
  var sucre = Array.fill(nbMachines)(30)
  var lait = Array.fill(nbMachines)(500)

  // Codes PIN pour chaque machine
  val machinePins = Array.fill(nbMachines)("434343")

  def main(args: Array[String]): Unit = {
    println("Nospresso Café")
    menuPrincipal()
  }

  def menuPrincipal(): Unit = {
    var continuer = true
    while (continuer) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      var choix = ""
      var choixValide = false

      while (!choixValide) { // Boucle pour vérifier la validité du choix
        choix = readLine("> ")
        if (choix == "1" || choix == "2" || choix == "3") {
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

        // Affichage des stocks après validation du code PIN
        println(s"\nStocks de la machine $machineId :")
        println(s"Poudre de café : ${poudreCafe(machineId)} g")
        println(s"Sucre : ${sucre(machineId)} g")
        println(s"Lait : ${lait(machineId)} ml")

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

    // Demander des doses supplémentaires de lait pour Cappuccino et Latte
    if (choixBoisson == "2" || choixBoisson == "3") {
      println("Souhaitez-vous ajouter du lait supplémentaire ? (jusqu'à 3 doses de 50ml chacune, 0.05 CHF par dose)")
      println("Entrez le nombre de doses supplémentaires que vous souhaitez (0, 1, 2 ou 3) :")
      val dosesSupplementaires = readLine("> ").toInt

      if (dosesSupplementaires >= 0 && dosesSupplementaires <= 3) {
        laitSupplementaire = dosesSupplementaires * 50
        prixLaitSupplementaire = dosesSupplementaires * 0.05
      } else {
        println("Nombre de doses invalide. Aucune dose supplémentaire ajoutée.")
      }
    }

    // Vérification des stocks
    if (coffeeStocks(machineId) < cafeRequis || milkStocks(machineId) < laitRequis + laitSupplementaire) {
      println("Quantité d'ingrédients insuffisante pour préparer la boisson.")
      return false
    }

    // Demander la quantité de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    val choixSucre = readLine("> ")

    if (choixSucre == "2") {
      sucreRequis = 5
      prixSucre = 0.1
    } else if (choixSucre == "3") {
      sucreRequis = 10
      prixSucre = 0.2
    } else if (choixSucre == "4") {
      sucreRequis = 15
      prixSucre = 0.3
    }

    // Vérification du sucre
    if (sugarStocks(machineId) < sucreRequis) {
      println("Quantité de sucre insuffisante.")
      return false
    }

    // Calcul du prix total et mise à jour des stocks
    val prixTotal = prixBase + prixSucre + prixLaitSupplementaire
    println(s"Prix total : CHF " + prixTotal)

    println("Veuillez payer en utilisant Twint.")
    val codePaiement = Random.alphanumeric.take(5).mkString("")
    println(s"Votre code de paiement est : " + codePaiement)
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")

    println("Préparation de votre boisson...")
    Thread.sleep(5000)

    coffeeStocks(machineId) -= cafeRequis
    sugarStocks(machineId) -= sucreRequis
    milkStocks(machineId) -= (laitRequis + laitSupplementaire)

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
}