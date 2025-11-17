import scala.io.StdIn._
import scala.util.Random
import collection.mutable.ArrayBuffer
import java.io.{FileWriter, PrintWriter}
import scala.io.Source

object Main {

  class Machine(id: Int = 0, pincode: String = "123456", milk: Int = 0, sugar: Int = 0, coffee: Int = 0) {
    val machineID = id
    var machinePin = pincode
    var machineLait = milk
    var machineSucre = sugar
    var machinePoudre = coffee

    def affichage(): Unit = {
      println("\nMachine " + machineID + " chargée :")
      println("ID : " + machineID)
      println("Code PIN : " + machinePin)
      printf("Lait : %.2fL\n", machineLait / 1000.0) // conversion en L
      println("Sucre : " + machineSucre + "g")
      println("Café : " + machinePoudre + "g")
    }

    def validatePin(): Boolean = {
      var saisirPIN = readLine("Veuillez entrer le code PIN > ")
      var tentativesRestantes = 2
      while (saisirPIN != machinePin && tentativesRestantes > 0) {
        if (saisirPIN != machinePin) {
          saisirPIN = readLine("Code PIN incorrect. " + tentativesRestantes + " tentatives restantes. Veuillez entrer le code PIN > ")
          tentativesRestantes -= 1
        }
        if (saisirPIN != machinePin && tentativesRestantes == 0) {
          println("Code PIN incorrect. " + tentativesRestantes + " tentatives restantes. \nTrop de tentatives échouées. Fin du programme.")
        }
      }
      return saisirPIN == machinePin
    }

    def updatePin(): Unit = {
      var nouveauCodePIN = readLine("Veuillez entrer un nouveau code PIN à 6 chiffres > ")
      while (nouveauCodePIN.length != 6 || !nouveauCodePIN.forall(x => x.isDigit)) {
        nouveauCodePIN = readLine("Format incorrect. Veuillez entrer un nouveau code PIN à 6 chiffres > ")
      }
      machinePin = nouveauCodePIN
      println("Le code PIN a été mis à jour avec succès.")
    }

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "Lait") {
        machineLait += amount
      } else if (ingredient == "Sucre") {
        machineSucre += amount
      } else if (ingredient == "Poudre") {
        machinePoudre += amount
      }
    }

    def restockMachine(): Unit = {
      println("Stocks actuels :")
      println("  Poudre de café : " + machinePoudre + "g")
      println("  Sucre : " + machineSucre + "g")
      printf("  Lait : %.2fL\n", machineLait / 1000.0) // conversion en L
      println("\nVeuillez entrer les quantités à ajouter :")
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
      addIngredient("Lait", (ajoutLait * 1000).toInt)
      addIngredient("Sucre", ajoutSucre)
      addIngredient("Poudre", ajoutPoudre)
      println("Les stocks ont été mis à jour avec succès.")
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      var removeOk = true
      if (ingredient == "Lait") {
        if (machineLait >= amount) {
          machineLait -= amount
        } else {
          removeOk = false
        }
      } else if (ingredient == "Sucre") {
        if (machineSucre >= amount) {
          machineSucre -= amount
        } else {
          removeOk = false
        }
      } else if (ingredient == "Poudre") {
        if (machinePoudre >= amount) {
          machinePoudre -= amount
        } else {
          removeOk = false
        }
      }
      return removeOk
    }

    def serveClient(): Boolean = {
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
        if (removeIngredient("Poudre", 8)) {
          poudreOk = true
        } else {
          poudreOk = false
        }
      } else if (choixBoisson == 2) { // Cappuccino
        nomBoisson = "Cappuccino"
        prixBase = 2.5
        if (removeIngredient("Poudre", 6)) {
          poudreOk = true
        } else {
          poudreOk = false
        }
      } else if (choixBoisson == 3 && taille == 1) { // Latte (Petit)
        nomBoisson = "Latte (Petit)"
        prixBase = 2.7
        if (removeIngredient("Poudre", 6)) {
          poudreOk = true
        } else {
          poudreOk = false
        }
      } else if (choixBoisson == 3 && taille == 2) { // Latte (Moyen)
        nomBoisson = "Latte (Moyen)"
        prixBase = 3.2
        if (removeIngredient("Poudre", 8)) {
          poudreOk = true
        } else {
          poudreOk = false
        }
      } else if (choixBoisson == 3 && taille == 3) {
        nomBoisson = "Latte (Grand)"
        prixBase = 3.7
        if (removeIngredient("Poudre", 12)) {
          poudreOk = true
        } else {
          poudreOk = false
        }
      }
      // Quantité de sucre
      if (choixSucre == 1) { // Sans
        nomSucre = "Sans sucre"
        prixSucre = 0.0
      } else if (choixSucre == 2) { // Peu
        nomSucre = "Peu (5g)"
        prixSucre = 0.1
        if (removeIngredient("Sucre", 5)) {
          sucreOk = true
        } else {
          sucreOk = false
        }
      } else if (choixSucre == 3) { // Moyen
        nomSucre = "Moyen (10g)"
        prixSucre = 0.2
        if (removeIngredient("Sucre", 10)) {
          sucreOk = true
        } else {
          sucreOk = false
        }
      } else if (choixSucre == 4) { // Beaucoup
        nomSucre = "Beaucoup (15g)"
        prixSucre = 0.3
        if (removeIngredient("Sucre", 15)) {
          sucreOk = true
        } else {
          sucreOk = false
        }
      }
      // Quantité de lait
      if (choixBoisson == 2 && choixLait == 2) { // Cappuccino sans lait sup.
        if (removeIngredient("Lait", 100)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 2 && choixLait == 1) { // Cappuccino + lait sup.
        if (removeIngredient("Lait", 100 + 50 * nbDoseLait)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 1 && choixLait == 2) { // Latte (Petit) sans lait sup.
        if (removeIngredient("Lait", 120)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 1 && choixLait == 1) { // Latte (Petit) + lait sup.
        if (removeIngredient("Lait", 120 + 50 * nbDoseLait)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 2 && choixLait == 2) { // Latte (Moyen) sans lait sup.
        if (removeIngredient("Lait", 150)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 2 && choixLait == 1) { // Latte (Moyen) + lait sup.
        if (removeIngredient("Lait", 150 + 50 * nbDoseLait)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 3 && choixLait == 2) { // Latte (Grand) sans lait sup.
        if (removeIngredient("Lait", 200)) {
          laitOk = true
        } else {
          laitOk = false
        }
      } else if (choixBoisson == 3 && taille == 3 && choixLait == 1) { // Latte (Grand) + lait sup.
        if (removeIngredient("Lait", 200 + 50 * nbDoseLait)) {
          laitOk = true
        } else {
          laitOk = false
        }
      }
      // Prix du lait sup.
      if (choixLait == 1) {
        prixLait = prixDoseLait * nbDoseLait
      }
      // Aperçu de la commande
      if (choixBoisson == 1) {
        println("\nBoisson sélectionnée : " + nomBoisson)
        println("Niveau de sucre : " + nomSucre)
      } else {
        println("\nBoisson sélectionnée : " + nomBoisson)
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

    def printData(): String = {
      return machinePin + "," + machineLait + "," + machineSucre + "," + machinePoudre
    }

  } // fin class Machine

  // Méthodes de gestion du CSV

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines.drop(1) // itérateur (à partir de la ligne 2: on saute le chapeau)
      val machines = new ArrayBuffer[Machine]()
      var i = 0
      while (lignefr.nonEmpty) { // lecture du fichier
        var ligne = lignefr.next
        var machine = ligne.split(",")
        machines += new Machine(i + 1, machine(0), machine(1).toInt, machine(2).toInt, machine(3).toInt)
        i += 1
      }
      return machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("\nErreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return null
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      Source.fromFile(filename).close()
      val printWriter = new PrintWriter(new FileWriter(filename, false)) // false => reécriture du fichier
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for (i <- machines.indices) {
        printWriter.println(machines(i).printData())
      }
      printWriter.close()
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case ex: java.io.FileNotFoundException => println("\nErreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println("\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    Thread.sleep(700)
    val machines = loadcsv("machines.csv")
    var nbrMachines = 0
    if (machines != null) {
      for (i <- machines.indices) {
        machines(i).affichage()
        nbrMachines += 1
      }
      if (nbrMachines > 0) {
        println(nbrMachines + " machine(s) chargée(s) avec succès.")
        var choixMode = 0
        var choixMachine = 0
        var quitter = false

        while (!quitter) { // boucle Menu principal
          println("\n         Nospresso Café")
          println("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter")
          choixMode = readLine("> ").toInt
          while (!(choixMode == 1 || choixMode == 2 || choixMode == 3)) {
            choixMode = readLine("Veuillez choisir entre 1, 2 et 3 > ").toInt
          }

          if (choixMode == 1) { // Mode Client
            var modeClient = true
            while (modeClient) {
              choixMachine = readLine("Machine sélectionnée (1-" + nbrMachines + ") > ").toInt
              while (choixMachine < 1 || choixMachine > nbrMachines) {
                choixMachine = readLine("Veuillez choisir entre 1 et " + nbrMachines + " > ").toInt
              }
              if (machines(choixMachine - 1).serveClient()) { // NB: cette méthode utilise la méthode removeIngredient
                modeClient = false
              } else {
                modeClient = true
              }
            }

          } else if (choixMode == 2) { // Mode Admin
            choixMachine = readLine("Machine sélectionnée (1-" + nbrMachines + ") > ").toInt
            while (choixMachine < 1 || choixMachine > nbrMachines) {
              choixMachine = readLine("Veuillez choisir entre 1 et " + nbrMachines + " > ").toInt
            }
            if (machines(choixMachine - 1).validatePin()) {
              println("Accès accordé à la Machine " + choixMachine)
              println("\nVeuillez choisir une opération : \n1) Gérer les stocks \n2) Changer le code PIN")
              var choixAction = readLine("> ").toInt
              while (!(choixAction == 1 || choixAction == 2)) {
                choixAction = readLine("Veuillez choisir entre 1 et 2 > ").toInt
              }
              if (choixAction == 1) {
                machines(choixMachine - 1).restockMachine() // NB: cette méthode utilise la méthode addIngredient
              } else {
                println("Mise à jour du code PIN pour la Machine " + choixMachine)
                machines(choixMachine - 1).updatePin()
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

        println("\nSauvegarde de " + nbrMachines + " machines dans machines.csv...")
        savecsv("machines.csv", machines) // impression dans le fichier
      } else {
        println("\nLe fichier contient 0 machines enregistrées.")
      }
    } else {
      println("\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  } // accolade fin (Unit)
} // accolade fin (objet Main)