import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import java.io.{File, FileWriter, PrintWriter}

// Définition de la classe Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Méthode pour ajouter des ingrédients
  def addIngredient(ingredient: String, amount: Int): Unit = {
    // Vérifie que la quantité est valide
    if (amount <= 0) {
      println("Quantité invalide. Veuillez entrer un nombre positif.")
      return
    }
    // Comparaison
    val lowerIngredient = ingredient.toLowerCase // Normalise la casse pour éviter les erreurs
    if (lowerIngredient == "milk") {
      milk += amount
    } else if (lowerIngredient == "sugar") {
      sugar += amount
    } else if (lowerIngredient == "coffee") {
      coffee += amount
    } else {
      println("Ingrédient invalide.") // Gère les cas où l'ingrédient n'est pas reconnu
      return
    }
    // Message de confirmation
    println(s"$amount unités de $ingredient ajoutées avec succès.")
  }

  // Méthode pour retirer des ingrédients
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    // Vérifie que la quantité demandée est positive
    if (amount <= 0) {
      println("Quantité invalide. Veuillez entrer un nombre positif.")
      return false
    }
    // Normalise la casse de l'ingrédient
    val lowerIngredient = ingredient.toLowerCase

    // Vérifie et met à jour le stock
    if (lowerIngredient == "milk") {
      if (milk >= amount) {
        milk -= amount
        println(s"$amount unités de lait retirées avec succès.")
        return true
      } else {
        println("Stock de lait insuffisant.")
        return false
      }
    } else if (lowerIngredient == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        println(s"$amount unités de sucre retirées avec succès.")
        return true
      } else {
        println("Stock de sucre insuffisant.")
        return false
      }
    } else if (lowerIngredient == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        println(s"$amount unités de café retirées avec succès.")
        return true
      } else {
        println("Stock de café insuffisant.")
        return false
      }
    } else {
      println("Ingrédient invalide.") // Gestion des cas non reconnus
      return false
    }
  }
}

object Main {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  // Charger les machines à partir d'un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    try {
      println(s"Chargement des machines depuis $filename...")
      val file = new File(filename)

      // Vérifie si le fichier existe
      if (!file.exists()) {
        throw new Exception(s"Fichier introuvable : $filename. Vérifiez le chemin d'accès et réessayez.")
      }

      // Lecture du fichier
      val source = Source.fromFile(filename)
      val lignefr = source.getLines() // Initialisation d'un itérateur sur les lignes
      lignefr.next() // Ignorer la première ligne (en-tête)

      var idx = 1 // Compteur pour l'ID des machines

      // Lecture ligne par ligne
      while (lignefr.hasNext) {
        val ligne = lignefr.next()
        val cols = ligne.split(",")

        // Vérifie si la ligne a 4 colonnes
        if (cols.length == 4) {
          try {
            buffer += new Machine(idx, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
          } catch {
            case e: NumberFormatException =>
              println(s"Ligne ignorée (valeur non numérique) : $ligne")
          }
        } else {
          println(s"Ligne ignorée (format invalide) : $ligne")
        }
        idx += 1 // Incrémentation de l'ID
      } // Fin de la boucle while

      // Affichage des données chargées
      println(s"${buffer.length} machine(s) chargée(s) avec succès.")
      println()
      for (machine <- buffer) {
        println(s"Machine ${machine.id} chargée :")
        println(s"ID: ${machine.id}")
        println(s"Code PIN: ${machine.pincode}")
        println(f"Lait: ${machine.milk / 1000.0}%.3f L") // Convertir ml en L
        println(s"Sucre: ${machine.sugar}g")
        println(s"Café: ${machine.coffee}g")
        println()
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(1)

      case e: java.io.IOException =>
        println(s"Erreur : Problème d'accès au fichier. Vérifiez les permissions.")
        System.exit(1)

      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}")
        System.exit(1)
    }
    buffer // Retourne les machines chargées
  }

  // Sauvegarder les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println(s"Sauvegarde des machines dans $filename...")
      val pw = new PrintWriter(new FileWriter(new File(filename), false))
      pw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        pw.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      pw.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: java.io.FileNotFoundException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(1)

      case e: java.io.IOException =>
        println(s"Erreur : Échec de l’écriture dans $filename. Le fichier peut être verrouillé ou en lecture seule.")
        System.exit(1)

      case e: Exception =>
        println(s"Erreur inattendue lors de la sauvegarde : ${e.getMessage}")
        System.exit(1)
    }
  }

  // Méthode pour valider l'ID entre 0 et 4 de la machine
  def validateMachineId(): Int = {
    var valid = false
    var id = -1

    if (machines.isEmpty) {
      println("Aucune machine disponible. Veuillez vérifier votre fichier CSV.")
      System.exit(1)
    }

    while (!valid) {
      println(s"Veuillez entrer un ID de machine (1 à 5) :")
      val input = readLine()
      if (input.forall(_.isDigit)) { // vérifie que l'entrée est un nombre
        val potentialId = input.toInt
        if (potentialId >= 1 && potentialId <= machines.length) { // vérifie que l'ID est dans la plage valide
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
  def validatePin(machineId: Int): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val pin = readLine()
      if (pin == machines(machineId).pincode) { // compare le PIN saisi avec le PIN stocké
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
  def updatePin(machineId: Int): Unit = {
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
    machines(machineId).pincode = newPin // met à jour le tableau des PIN
    println("Le code PIN a été mis à jour avec succès.")
    println()
    savecsv("machines.csv", machines)
  }


  // Méthode du Mode Client
  def serveClient(machineId: Int): Boolean = {
    println(s"Service client pour la machine ${machineId + 1}")
    var commandeValide = false

    while (!commandeValide) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

      var boisson = readLine("> ").toInt

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
        var taille = readLine("> ").toInt
        while (taille < 1 || taille > 3) {
          println("Taille invalide. Veuillez choisir 1, 2 ou 3.")
          taille = readLine("> ").toInt
        }
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
      if (machines(machineId).coffee < cafeNecessaire || machines(machineId).milk < totalLait || machines(machineId).sugar < sucreNecessaire) {
        println("Erreur : Stock insuffisant pour préparer votre boisson.")
        println("Veuillez sélectionner une autre machine.")
        return false
      } else {
        // Vérification de la disponibilité des stocks avant la mise à jour
        if (!machines(machineId).removeIngredient("coffee", cafeNecessaire) ||
          !machines(machineId).removeIngredient("milk", totalLait) ||
          !machines(machineId).removeIngredient("sugar", sucreNecessaire)) {
          println("Erreur : Stock insuffisant pour préparer votre boisson.")
          println("Veuillez sélectionner une autre machine.")
          return false
        }

        // Mise à jour confirmée
        println("Stock mis à jour pour préparer votre boisson.")


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

        Thread.sleep(2000)
        println()

        savecsv("machines.csv", machines)

        commandeValide = true
      }
    }

    return true
  }

  // Méthode pour le réapprovisionnement des stocks
  def restockMachine(machineId: Int): Unit = {
    println(s"Réapprovisionnement des stocks pour la machine ${machineId + 1}")

    println("Quantité de café à ajouter (g) : ")
    val cafeAjoute = readLine().toInt
    if (cafeAjoute >= 0) {
      machines(machineId).addIngredient("coffee", cafeAjoute)
    } else {
      println("Quantité invalide. Aucune mise à jour effectuée pour le café.")
    }

    println("Quantité de sucre à ajouter (g) : ")
    val sucreAjoute = readLine().toInt
    if (sucreAjoute >= 0) {
      machines(machineId).addIngredient("sugar", sucreAjoute)
    } else {
      println("Quantité invalide. Aucune mise à jour effectuée pour le sucre.")
    }

    println("Quantité de lait à ajouter (ml) : ")
    val laitAjoute = readLine().toInt
    if (laitAjoute >= 0) {
      machines(machineId).addIngredient("milk", laitAjoute)
    } else {
      println("Quantité invalide. Aucune mise à jour effectuée pour le lait.")
    }

    println()
    println("Stocks mis à jour avec succès.")
    println("Retour au menu Administrateur...")

    savecsv("machines.csv", machines)

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
        println(s"Lait : ${machines(machineId).milk}ml, Sucre : ${machines(machineId).sugar}g, Café : ${machines(machineId).coffee}g")
      } else if (choix == 2) {
        // Réapprovisionnement
        restockMachine(machineId)
      } else if (choix == 3) {
        // Changement de PIN
        updatePin(machineId)
      } else if (choix == 4) {
        println("Retour au menu principal...")
        Thread.sleep(3000)
        println()
        return
      } else {
        println("Choix invalide.Veuillez réessayer")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var continuer = true

    try {
      machines = loadcsv("machines.csv")

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
            machineServed = serveClient(machineId)
          }
        } else if (mode == 2) {
          // Mode Admin
          val machineId = validateMachineId()
          if (validatePin(machineId)) adminMenu(machineId)
        } else if (mode == 3) {
          // Quitter
          savecsv("machines.csv", machines)
          println("Programme terminé.")
          continuer = false
        } else {
          println("Choix invalide.")
        }
      }
    } catch {
      case e: Exception =>
        println(s"Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(1)
    }
  }
}