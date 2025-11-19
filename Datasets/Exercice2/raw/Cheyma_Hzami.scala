import scala.io.StdIn._

object Nospresso {
  val nbMachines = 5 // Nombre total de machines
  val codesPinMachines = Array("434343", "434343", "434343", "434343", "434343") // Codes PIN des machines
  val stocksCafé = Array(30, 30, 30, 30, 30) // Stocks de café pour chaque machine
  val stocksSucre = Array(15, 15, 15, 15, 15) // Stocks de sucre pour chaque machine
  val stocksLait = Array(0.20, 0.20, 0.20, 0.20, 0.20) // Stocks de lait pour chaque machine (en litres)

  def main(args: Array[String]): Unit = {
    var enCours = true
    while (enCours) {
      println("\nBienvenue sur Nospresso !")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")

      val choix = readLine()

      if (choix == "1") {
        modeClient()
      } else if (choix == "2") {
        modeAdmin()
      } else if (choix == "3") {
        println("Merci d'avoir utilisé Nospresso ! À bientôt.")
        enCours = false
      } else {
        println("Option invalide. Veuillez réessayer.")
      }
    }
  }

  // Mode Client
  def modeClient(): Unit = {
    println("\nMachine sélectionnée (1-5) :")
    val idMachine = obtenirIdMachine() - 1

    if (idMachine >= 0 && idMachine < nbMachines) {
      // Afficher les stocks dès le choix de la machine
      afficherStocks(idMachine)

      println("\nMenu :")
      println("1) Expresso (CHF 2.0)")
      println("2) Cappuccino (CHF 2.50)")
      println("3) Latte (taille au choix : petit, moyen, grand)")
      print("> ")
      val choixBoisson = readLine()

      if (choixBoisson == "1") {
        servirClient(idMachine, "Expresso", 8, 0.0)
      } else if (choixBoisson == "2") {
        servirClient(idMachine, "Cappuccino", 6, 0.1)
      } else if (choixBoisson == "3") {
        choisirLatte(idMachine)
      } else {
        println("Option invalide.")
      }
    } else {
      println("ID de machine invalide.")
    }
  }

  // Mode Admin
  def modeAdmin(): Unit = {
    println("\nMachine sélectionnée (1-5) :")
    val idMachine = obtenirIdMachine() - 1

    if (idMachine >= 0 && idMachine < nbMachines) {
      if (validerPin(idMachine)) {
        println("\nAccès accordé à la machine " + (idMachine + 1) + ":")

        // Afficher directement les stocks de la machine dès la validation du PIN
        afficherStocks(idMachine)

        println("1) Réapprovisionner les stocks")
        println("2) Mettre à jour le code PIN")
        print("> ")
        val choixAdmin = readLine()

        if (choixAdmin == "1") {
          réapprovisionnerMachine(idMachine)
        } else if (choixAdmin == "2") {
          mettreAJourPin(idMachine)
        } else {
          println("Option invalide.")
        }
      } else {
        println("Code PIN incorrect. 0 tentatives restantes.")
        println("Trop de tentatives échouées. Fin du programme.")
      }
    } else {
      println("ID de machine invalide.")
    }
  }

  // ID de machine valide (1 à 5)
  def obtenirIdMachine(): Int = {
    var idMachine = -1
    while (idMachine < 1 || idMachine > nbMachines) {
      println("Entrez la machine (1 à 5) :")
      print("> ")
      try {
        idMachine = readInt()
      } catch {
        case _: Exception => println("Veuillez entrer un numéro valide.")
      }
    }
    idMachine
  }

  // valider le code PIN
  def validerPin(idMachine: Int): Boolean = {
    var tentatives = 0
    var valide = false
    while (tentatives < 3 && !valide) {
      println("Entrez le code PIN pour la machine " + (idMachine + 1) + ":")
      val pin = readLine()
      if (pin == codesPinMachines(idMachine)) {
        valide = true
      } else {
        tentatives += 1
        val tentativesRestantes = 3 - tentatives
        if (tentativesRestantes > 0) {
          println("Code PIN incorrect. Il vous reste " + tentativesRestantes + " tentative(s).")
        }
      }
    }
    valide
  }

  // Fonction pour mettre à jour le code PIN
  def mettreAJourPin(idMachine: Int): Unit = {
    var nouveauPin = ""

    // Demander un nouveau PIN tant que celui-ci n'est pas valide
    while (nouveauPin.length != 6) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      nouveauPin = readLine()

      if (nouveauPin.length != 6) {
        println("Le code PIN doit être composé de 6 chiffres. Veuillez réessayer.")
      }
    }

    // Mise à jour du code PIN
    codesPinMachines(idMachine) = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // servir une boisson
  def servirClient(idMachine: Int, nomBoisson: String, caféRequis: Int, laitRequis: Double): Unit = {
    if (stocksCafé(idMachine) >= caféRequis && stocksLait(idMachine) >= laitRequis) {
      stocksCafé(idMachine) -= caféRequis
      stocksLait(idMachine) -= laitRequis
      println("Votre " + nomBoisson + " est prêt !")
    } else {
      println("Stocks insuffisants pour le " + nomBoisson + ".")
    }
  }

  // Fonction pour choisir un Latte (avec différentes tailles)
  def choisirLatte(idMachine: Int): Unit = {
    println("\nChoisissez la taille du Latte :")
    println("1) Petit Latte (6g de café, 0.12L de lait)")
    println("2) Moyen Latte (8g de café, 0.15L de lait)")
    println("3) Grand Latte (12g de café, 0.2L de lait)")
    print("> ")

    val choixLatte = readLine()

    if (choixLatte == "1") {
      servirClient(idMachine, "Petit Latte", 6, 0.12)
    } else if (choixLatte == "2") {
      servirClient(idMachine, "Moyen Latte", 8, 0.15)
    } else if (choixLatte == "3") {
      servirClient(idMachine, "Grand Latte", 12, 0.2)
    } else {
      println("Option invalide.")
    }
  }

  // Fonction pour réapprovisionner les stocks
  def réapprovisionnerMachine(idMachine: Int): Unit = {
    println("Entrez la quantité de café à ajouter (grammes) :")
    val quantitéCafé = readInt()
    if (quantitéCafé > 0) {
      stocksCafé(idMachine) += quantitéCafé
    } else {
      println("Quantité invalide.")
    }

    println("Entrez la quantité de sucre à ajouter (grammes) :")
    val quantitéSucre = readInt()
    if (quantitéSucre > 0) {
      stocksSucre(idMachine) += quantitéSucre
    } else {
      println("Quantité invalide.")
    }

    println("Entrez la quantité de lait à ajouter (litres) :")
    val quantitéLait = readDouble() // Utilisation de readDouble pour lire un nombre décimal
    if (quantitéLait > 0) {
      stocksLait(idMachine) += quantitéLait
    } else {
      println("Quantité invalide.")
    }

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // Fonction pour afficher les stocks d'une machine
  def afficherStocks(idMachine: Int): Unit = {
    println("Stocks de la machine " + (idMachine + 1) + ":")
    println("Café : " + stocksCafé(idMachine) + "g")
    println("Sucre : " + stocksSucre(idMachine) + "g")
    println("Lait : " + stocksLait(idMachine) + "L")
  }
}