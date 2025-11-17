import scala.io.StdIn.readLine

object Main {
  val nbMachines = 5

  // Tableaux pour gérer les stocks et les codes PIN
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50) // en grammes
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30) // en grammes
  val milkStocks: Array[Int] = Array.fill(nbMachines)(500) // en millilitres
  val machinePins: Array[String] = Array.fill(nbMachines)("434343")

  // Méthode pour valider l'ID entre 0 et 4 de la machine
  def validateMachineId(): Int = {
    var valid = false
    var id = -1
    while (!valid) {
      println(s"Veuillez entrer un ID de machine (1 à 5) :")
      val input = readLine()
      if (input.forall(_.isDigit)) { // vérifie que l'entrée est un nombre
        val potentialId = input.toInt
        if (potentialId >= 1 && potentialId <= nbMachines) { // vérifie que l'ID est dans la plage valide
          id = potentialId - 1 // index 0 à 4
          valid = true
          println()
        } else {
          println("ID de machine invalide. Veuillez réessayer.")
        }
      } else {
        println("Entrée non valide. Veuillez entrer un numéro.")
      }
    }
    id
  }

  // Méthode pour valider le code PIN
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val pin = readLine()
      if (pin == machinePins(machineId)) { // compare le PIN saisi avec le PIN stocké
        println("Accès accordé.")
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. $attempts tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  // Méthode pour le changement de PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${machineId + 1}")
    var newPin = ""
    var valid = false

    while (!valid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      newPin = readLine()
      if (newPin.matches("\\d{6}")) { // pour contenir exactement 6 chiffres
        valid = true
      } else {
        println("Code PIN invalide. Veuillez entrer exactement 6 chiffres.")
      }
    }
    machinePins(machineId) = newPin // met à jour le tableau des PIN
    println("Le code PIN a été mis à jour avec succès.")
    println()
  }


  // Méthode du Mode Client
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println(s"Service client pour la machine ${machineId + 1}")
    var commandeValide = false

    while (!commandeValide) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

      val boisson = readLine("> ").toInt

      // Initialisation des quantités nécessaires et du prix
      var nomBoisson = ""
      var resumeBoisson = ""
      var cafeNecessaire = 0
      var laitNecessaire = 0
      var prixBoisson = 0.0

      if (boisson == 1) {
        nomBoisson = "Expresso"
        resumeBoisson = "Expresso"
        cafeNecessaire = 8
        laitNecessaire = 0
        prixBoisson = 2.00
      } else if (boisson == 2) {
        nomBoisson = "Cappuccino"
        resumeBoisson = "Cappuccino"
        cafeNecessaire = 6
        laitNecessaire = 100
        prixBoisson = 2.50
      } else if (boisson == 3) {
        println("Choisissez la taille du Latte :")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        val taille = readLine("> ").toInt
        if (taille == 1) {
          nomBoisson = "Latte Petit"
          resumeBoisson = "Latte (Petit)"
          cafeNecessaire = 6
          laitNecessaire = 120
          prixBoisson = 2.70
        } else if (taille == 2) {
          nomBoisson = "Latte Moyen"
          resumeBoisson = "Latte (Moyen)"
          cafeNecessaire = 8
          laitNecessaire = 150
          prixBoisson = 3.20
        } else if (taille == 3) {
          nomBoisson = "Latte Grand"
          resumeBoisson = "Latte (Grand)"
          cafeNecessaire = 12
          laitNecessaire = 200
          prixBoisson = 3.70
        } else {
          println("Taille invalide. Veuillez réessayer.")
        }
      }

      // Ajout de sucre
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      val sucreChoix = readLine("> ").toInt
      var sucreNecessaire = 0
      var prixSucre = 0.0

      if (sucreChoix == 2) {
        sucreNecessaire = 5
        prixSucre = 0.10
      } else if (sucreChoix == 3) {
        sucreNecessaire = 10
        prixSucre = 0.20
      } else if (sucreChoix == 4) {
        sucreNecessaire = 15
        prixSucre = 0.30
      }

      // Ajout de lait
      var dosesLait = 0
      if (boisson != 1) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Non")
        println("2) Oui")
        val choixdoses = readLine("> ").toInt

        if (choixdoses == 2) { // Si l'utilisateur veut ajouter du lait
          var choixValide = false
          while (!choixValide) {
            println("Combien de doses de lait supplémentaire (1-3) ? Chaque dose ajoute 50ml et coûte CHF 0.20 :")
            dosesLait = readLine("> ").toInt
            if (dosesLait >= 1 && dosesLait <= 3) choixValide = true
            else println("Entrée invalide. Veuillez entrer un nombre entre 1 et 3.")
          }
        } else if (choixdoses == 1) {
          println("Aucun lait supplémentaire ajouté.")
        } else {
          println("Entrée invalide. Aucun supplément ajouté par défaut.")
        }
      }

      val laitSupplementaire = dosesLait * 50
      val prixLaitSupplement = dosesLait * 0.20

      val totalLait = laitNecessaire + laitSupplementaire

      // vérification de la disponibilité des stocks
      if (coffeeStocks(machineId) < cafeNecessaire || milkStocks(machineId) < totalLait || sugarStocks(machineId) < sucreNecessaire) {
        println("Erreur : Stock insuffisant pour préparer votre boisson.")
        println("Veuillez sélectionner une autre machine.")
        return false
      } else {
        // Mise à jour des stocks
        coffeeStocks(machineId) -= cafeNecessaire
        milkStocks(machineId) -= totalLait
        sugarStocks(machineId) -= sucreNecessaire

        // Calcul du prix final
        val prixTotal = prixBoisson + prixSucre + prixLaitSupplement
        println(f"Prix total : CHF $prixTotal%.2f")


        // Générer un code alphanumérique de 5 caractères dont 1 chifre et 4 majuscules
        var codeTwint = ""
        val chiffres = ('0' + (math.random * 10).toInt).toChar

        val chiffrePosition = (math.random * 5).toInt // Position aléatoire pour placer le chiffre (entre 0 et 4 inclus)

        // Générer les 5 caractères
        for (i <- 0 until 5) {
          val char =
            if (i == chiffrePosition) {
              chiffres // Placer le chiffre à la position définie
            } else {
              ('A' + (math.random * 26).toInt).toChar // Générer une lettre majuscule
            }
          codeTwint += char // Ajouter le caractère au code
        }

        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + codeTwint)
        println("(En attente de paiement...)")
        println()
        Thread.sleep(3000)
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

        Thread.sleep(5000)
        println()
        println("Votre boisson est prête. Bonne dégustation !")
        println()

        commandeValide = true
      }
    }
    true
  }

  // Méthode pour le réapprovisionnement des stocks
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"Réapprovisionnement des stocks pour la machine ${machineId + 1}")

    println("Quantité de café à ajouter (g) : ")
    coffeeStocks(machineId) += readLine().toInt

    println("Quantité de sucre à ajouter (g) : ")
    sugarStocks(machineId) += readLine().toInt

    println("Quantité de lait à ajouter (ml) : ")
    milkStocks(machineId) += readLine().toInt

    Thread.sleep(3000)
    println()
    println("Stocks mis à jour avec succès.")
    println("Retour au menu Administrateur...")
    Thread.sleep(2000)
  }

  // Méthode mode Admin
  def adminMenu(machineId: Int): Unit = {
    var choix = 1
    while (choix != 4) {
      println()
      println("Menu Administrateur")
      println("1) Afficher les stocks\n2) Réapprovisionner\n3) Mettre à jour le code PIN\n4) Retour")
      choix = readLine("> ").toInt
      println()

      if (choix == 1) {
        // Affichage des stocks
        println(s"Stocks actuels pour la machine ${machineId + 1}")
        println(f"Café : ${coffeeStocks(machineId)}g\nSucre : ${sugarStocks(machineId)}g\nLait : ${milkStocks(machineId) / 1000.0}%.2f L")
      } else if (choix == 2) {
        // Réapprovisionnement
        restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
      } else if (choix == 3) {
        // Changement de PIN
        updatePin(machineId, machinePins)
      } else if (choix == 4) {
        println("Retour au menu principal...")
        Thread.sleep(3000)
        println()
      } else {
        println("Choix invalide.")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var continuer = true

    while (continuer) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val mode = readLine("> ").toInt
      println()

      if (mode == 1) {
        // Mode Client
          var machineServed = false
          while (!machineServed) {
            val machineId = validateMachineId()
            machineServed = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
      } else if (mode == 2) {
        // Mode Admin
        val machineId = validateMachineId()
        if (validatePin(machineId, machinePins)) adminMenu(machineId)
      } else if (mode == 3) {
        // Quitter
        println("Programme terminé.")
        continuer = false
      } else {
        println("Choix invalide.")
      }
    }
  }
}