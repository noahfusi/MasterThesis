import scala.io.StdIn._
import scala.util.Random

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var saisirPIN = readLine("Veuillez entrer le code PIN > ")
    var tentativesRestantes = 2
    while (saisirPIN != machinePins(machineId) && tentativesRestantes > 0) {
      if (saisirPIN != machinePins(machineId)) {
        saisirPIN = readLine("Code PIN incorrect. " + tentativesRestantes + " tentatives restantes. Veuillez entrer le code PIN > ")
        tentativesRestantes -= 1
      }
      if (saisirPIN != machinePins(machineId) && tentativesRestantes == 0) {
        println("Code PIN incorrect. " + tentativesRestantes + " tentatives restantes. \nTrop de tentatives échouées. Fin du programme.")
      }
    }
    return saisirPIN == machinePins(machineId)
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var nouveauCodePIN = readLine("Veuillez entrer un nouveau code PIN à 6 chiffres > ")
    while (nouveauCodePIN.length != 6 || !nouveauCodePIN.forall(x => x.isDigit)) {
      nouveauCodePIN = readLine("Format incorrect. Veuillez entrer un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(machineId) = nouveauCodePIN
    println("Le code PIN a été mis à jour avec succès.")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var poudreOk = true
    var sucreOk = true
    var laitOk = true
    var nomBoisson = "boisson"
    var nomSucre = "sucre"
    var choixBoisson = 0
    var taille = 0
    var choixSucre = 0
    var choixLait = 0
    var nbDoseLait = 0
    var prixSucre = 0.0 // CHF
    val prixDoseLait = 0.05 // CHF
    var prixLait = 0.0 // CHF
    var prixBase = 0.0 // CHF

    println()
    println("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    choixBoisson = readLine("> ").toInt
    while (!(choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3)) {
      choixBoisson = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
    }
    if (choixBoisson == 3) {
      println() // Taille du Latte
      println("Veuillez sélectionner la taille de votre Latte : \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70")
      taille = readLine("> ").toInt
      while (!(taille == 1 || taille == 2 || taille == 3)) {
        taille = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
      }
    }
    println() // Ajout du sucre
    println("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30")
    choixSucre = readLine("> ").toInt
    while (!(choixSucre == 1 || choixSucre == 2 || choixSucre == 3 || choixSucre == 4)) {
      choixSucre = readLine("Veuillez choisir entre 1, 2, 3 et 4 > ").toInt
    }
    // Ajout du lait
    if (choixBoisson == 2 || choixBoisson == 3) {
      println()
      println("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non")
      choixLait = readLine("> ").toInt
      while (!(choixLait == 1 || choixLait == 2)) {
        choixLait = readLine("Veuillez choisir entre 1 et 2 > ").toInt
      }
      if (choixLait == 1) {
        println("Combien de doses ? (Une dose contient 50 ml de lait. Vous ne pouvez pas ajouter plus de 3 doses.)")
        nbDoseLait = readLine("> ").toInt
        while (nbDoseLait > 3) {
          nbDoseLait = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
        }
      }
    }
    // Quantité de poudre de café + nom + prixBase
    if (choixBoisson == 1) { // Expresso
      nomBoisson = "Expresso"
      prixBase = 2.0
      if (coffeeStocks(machineId) < 8) {
        poudreOk = false
      } else {
        coffeeStocks(machineId) -= 8
      }
    } else if (choixBoisson == 2) { // Cappuccino
      nomBoisson = "Cappuccino"
      prixBase = 2.5
      if (coffeeStocks(machineId) < 6) {
        poudreOk = false
      } else {
        coffeeStocks(machineId) -= 6
      }
    } else if (choixBoisson == 3 && taille == 1) { // Latte (Petit)
      nomBoisson = "Latte (Petit)"
      prixBase = 2.7
      if (coffeeStocks(machineId) < 6) {
        poudreOk = false
      } else {
        coffeeStocks(machineId) -= 6
      }
    } else if (choixBoisson == 3 && taille == 2) { // Latte (Moyen)
      nomBoisson = "Latte (Moyen)"
      prixBase = 3.2
      if (coffeeStocks(machineId) < 8) {
        poudreOk = false
      } else {
        coffeeStocks(machineId) -= 8
      }
    } else if (choixBoisson == 3 && taille == 3) {
      nomBoisson = "Latte (Grand)"
      prixBase = 3.7
      if (coffeeStocks(machineId) < 12) {
        poudreOk = false
      } else {
        coffeeStocks(machineId) -= 12
      }
    }
    // Quantité de sucre
    if (choixSucre == 1) { // Sans
      nomSucre = "Sans sucre"
      prixSucre = 0.0
    } else if (choixSucre == 2) { // Peu
      nomSucre = "Peu (5g)"
      prixSucre = 0.1
      if (sugarStocks(machineId) < 5) {
        sucreOk = false
      } else {
        sugarStocks(machineId) -= 5
      }
    } else if (choixSucre == 3) { // Moyen
      nomSucre = "Moyen (10g)"
      prixSucre = 0.2
      if (sugarStocks(machineId) < 10) {
        sucreOk = false
      } else {
        sugarStocks(machineId) -= 10
      }
    } else if (choixSucre == 4) { // Beaucoup
      nomSucre = "Beaucoup (15g)"
      prixSucre = 0.3
      if (sugarStocks(machineId) < 15) {
        sucreOk = false
      } else {
        sugarStocks(machineId) -= 15
      }
    }
    // Quantité de lait
    if (choixBoisson == 2 && choixLait == 2) { // Cappuccino sans lait sup.
      if (milkStocks(machineId) < 100) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 100
      }
    } else if (choixBoisson == 2 && choixLait == 1) { // Cappuccino + lait sup.
      if (milkStocks(machineId) < 100 + 50 * nbDoseLait) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 100 + 50 * nbDoseLait
      }
    } else if (choixBoisson == 3 && taille == 1 && choixLait == 2) { // Latte (Petit) sans lait sup.
      if (milkStocks(machineId) < 120) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 120
      }
    } else if (choixBoisson == 3 && taille == 1 && choixLait == 1) { // Latte (Petit) + lait sup.
      if (milkStocks(machineId) < 120 + 50 * nbDoseLait) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 120 + 50 * nbDoseLait
      }
    } else if (choixBoisson == 3 && taille == 2 && choixLait == 2) { // Latte (Moyen) sans lait sup.
      if (milkStocks(machineId) < 150) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 150
      }
    } else if (choixBoisson == 3 && taille == 2 && choixLait == 1) { // Latte (Moyen) + lait sup.
      if (milkStocks(machineId) < 150 + 50 * nbDoseLait) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 150 + 50 * nbDoseLait
      }
    } else if (choixBoisson == 3 && taille == 3 && choixLait == 2) { // Latte (Grand) sans lait sup.
      if (milkStocks(machineId) < 200) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 200
      }
    } else if (choixBoisson == 3 && taille == 3 && choixLait == 1) { // Latte (Grand) + lait sup.
      if (milkStocks(machineId) < 200 + 50 * nbDoseLait) {
        laitOk = false
      } else {
        milkStocks(machineId) -= 200 + 50 * nbDoseLait
      }
    }
    // Prix du lait sup.
    if (choixLait == 1) {
      prixLait = prixDoseLait * nbDoseLait
    }
    // Aperçu de la commande
    if (choixBoisson == 1) {
      println()
      println("Boisson sélectionnée : " + nomBoisson)
      println("Niveau de sucre : " + nomSucre)
      println()
    } else {
      println()
      println("Boisson sélectionnée : " + nomBoisson)
      println("Niveau de sucre : " + nomSucre)
      if (choixLait == 1) {
        println("Lait en supplément: Oui")
      } else {
        println("Lait en supplément: Non")
      }
      println()
    }
    // Gestion des erreurs
    if (!poudreOk) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
    } else if (!sucreOk) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
    } else if (!laitOk) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
    }
    if (!poudreOk || !sucreOk || !laitOk) {
      println("         Veuillez sélectionner une autre machine.")
    } else {
      // Calcul du prix
      if (choixBoisson == 1) { // Expresso
        printf("Prix total : CHF %.2f + CHF %.2f (sucre) = CHF %.2f.\n", prixBase, prixSucre, prixBase + prixSucre)
      } else { // Cappuccino et Latte
        printf("Prix total : CHF %.2f + CHF %.2f (sucre) + CHF %.2f (lait sup.) = CHF %.2f.\n", prixBase, prixSucre, prixLait, prixBase + prixSucre + prixLait)
      }
      println()
      println("Veuillez payer en utilisant Twint.")
      val codeTwint = new String(Random.alphanumeric.take(5).toArray)
      println("Votre code de paiement est : " + codeTwint)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
      println()
      println("Paiement confirmé.")
      println("Préparation de votre boisson...")
      Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
      println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")
    }
    return poudreOk && sucreOk && laitOk
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Stocks actuels :")
    println("  Poudre de café : " + coffeeStocks(machineId) + "g")
    println("  Sucre : " + sugarStocks(machineId) + "g")
    printf("  Lait : %.2fL\n", milkStocks(machineId) / 1000.0) // conversion en L
    println()
    println("Veuillez entrer les quantités à ajouter :")
    var ajoutPoudre = readLine("  Poudre de café > ").toInt
    while (ajoutPoudre < 0) {
      ajoutPoudre = readLine("  Veuillez saisir une valeur positive. Poudre de café > ").toInt
    }
    var ajoutSucre = readLine("  Sucre > ").toInt
    while (ajoutSucre < 0) {
      ajoutSucre = readLine("  Veuillez saisir une valeur positive. Sucre > ").toInt
    }
    var ajoutLait = readLine("  Lait > ").toDouble // saisie en L
    while (ajoutLait < 0) {
      ajoutLait = readLine("  Veuillez saisir une valeur positive. Lait > ").toDouble
    }
    coffeeStocks(machineId) += ajoutPoudre
    sugarStocks(machineId) += ajoutSucre
    milkStocks(machineId) += (ajoutLait * 1000).toInt
    println("Les stocks ont été mis à jour avec succès.")
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val poudreStocks = Array.fill(nbMachines)(50)
    val sucreStocks = Array.fill(nbMachines)(30)
    val laitStocks = Array.fill(nbMachines)(500)
    val codesPIN = Array.fill(nbMachines)("434343")
    var choixMode = 0
    var choixMachine = 0
    var quitter = false

    while (!quitter) { // boucle Menu principal
      println("         Nospresso Café")
      println("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter")
      choixMode = readLine("> ").toInt
      while (!(choixMode == 1 || choixMode == 2 || choixMode == 3)) {
        choixMode = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
      }

      if (choixMode == 1) { // Mode Client
        var modeClient = true
        while (modeClient) {
          choixMachine = readLine("Machine sélectionnée (1-5) > ").toInt
          while (choixMachine < 1 || choixMachine > nbMachines) {
            choixMachine = readLine("Veuillez choisir entre 1, 2, 3, 4 et 5 > ").toInt
          }
          if (serveClient(choixMachine - 1, poudreStocks, sucreStocks, laitStocks)) {
            modeClient = false
          } else {
            modeClient = true
          }
        }

      } else if (choixMode == 2) { // Mode Admin
        choixMachine = readLine("Machine sélectionnée (1-5) > ").toInt
        while (choixMachine < 1 || choixMachine > nbMachines) {
          choixMachine = readLine("Veuillez choisir entre 1, 2, 3, 4 et 5 > ").toInt
        }
        if (validatePin(choixMachine - 1, codesPIN)) {
          println("Accès accordé à la Machine " + choixMachine)
          println()
          println("Veuillez choisir une opération : \n1) Gérer les stocks \n2) Changer le code PIN")
          var choixAction = readLine("> ").toInt
          while (!(choixAction == 1 || choixAction == 2)) {
            choixAction = readLine("Veuillez choisir entre 1 et 2 > ").toInt
          }
          if (choixAction == 1) {
            println()
            restockMachine(choixMachine - 1, poudreStocks, sucreStocks, laitStocks)
          } else {
            println()
            println("Mise à jour du code PIN pour la Machine " + choixMachine)
            updatePin(choixMachine - 1, codesPIN)
          }
          println("Retour au menu principal...")
        } else {
          quitter = true
        }
      } else { // Mode Quitter
        quitter = true
      }
      println("-------------------------------------")
    } // boucle Menu principal

  } // accolade fin (Unit)
} // accolade fin (objet Main)