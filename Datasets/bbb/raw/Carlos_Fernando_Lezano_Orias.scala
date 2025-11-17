import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  // Nombre de machines
  val nbMachines = 5

  // Définition des stocks pour chaque machine
  var coffeeStocks = Array(50, 50, 50, 50, 50)    // en grammes
  var sugarStocks = Array(30, 30, 30, 30, 30)     // en grammes
  var milkStocks = Array(0.5, 0.5, 0.5, 0.5, 0.5) // en litres

  // Codes PIN pour chaque machine
  var machinePins = Array("434343", "434343", "434343", "434343", "434343")

  // fonction principal
  def main(args: Array[String]): Unit = {
    var Quitter = false
    while (!Quitter) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">")

      val mode = readLine().toInt

      if (mode == 1) {
        modeClient()  // Modo Cliente
      } else if (mode == 2) {
        modeAdmin()  //
      } else if (mode == 3) {
        println("Quitter le programme...")
        Quitter = true  // Terminar el programa
      } else {
        println("Option invalide.")
      }
    }
  }

  // Función para el Modo Cliente
  def modeClient(): Unit = {
    println("Sélectionnez une machine (1-5):")
    val machineId = readLine().toInt - 1

    if (machineId >= 0 && machineId < nbMachines) {
      serveClient(machineId)
    } else {
      println("Machine invalide.")    }
  }


  // Fonction pour le mode administrateur
  // Función para el Modo Administrador
  def modeAdmin(): Unit = {
    println("\nEntrez le code PIN de l'administrateur:")
    val pin = readLine()

    if (pin == "434343") {
      println("Accès autorisé")
      println("Options de l'administrateur :")
      println("1) Voir le stock d'ingrédients")
      println("2) Réapprovisionnement")
      println("3) Mettre à jour le code PIN de la machine")
      val choixAdmin = readLine().toInt

      println("Sélectionner la machine (1-5):")
      val machineId = readLine().toInt - 1

      if (machineId >= 0 && machineId < nbMachines) {
        if (validatePin(machineId)) {
          if (choixAdmin == 1) {
            voirStock(machineId)
          } else if (choixAdmin == 2) {
            restockMachine(machineId)
          } else if (choixAdmin == 3) {
            updatePin(machineId)
          } else {
            println("Option invalide.")          }
        }
      } else {
        println("Machine invalide.")      }
    } else {
      println("PIN incorrect. Accès refusé.")    }
  }


  // Valider le code PIN de la machine sélectionnée
  def validatePin(machineId: Int): Boolean = {
    println(s"Machine selectionnée (1-5) > ${machineId + 1}")
    println(s"Entrez le code PIN:")
    var attempts = 0

    while (attempts < 3) {
      val pin = readLine()
      if (pin == machinePins(machineId)) {
        return true
      } else {
        attempts += 1
        println(s"Code PIN incorrect.: ${3 - attempts} tentatives restantes")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  // Mettre à jour le code PIN de la machine sélectionnée
  def updatePin(machineId: Int): Unit = {
    println("Accès accordé ")
    println(s"Mis à jour pour le machine ${machineId + 1}.")
    var nouveauPin = readLine()

    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
      println("Entrez un nouveau code PIN à 6 chiffres >")
      nouveauPin = readLine()
    }

    machinePins(machineId) = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // Fonction pour générer un code Twint aléatoire
  def genererCodeTwint(): String = {
    val caracteres = "ABCDEFGHYJKLMNOPQRSTUVWXYZ123456789"
    val random = new Random()
    var codeTwint = ""

    for (_ <- 1 to 5) {
      val charAleatoire = caracteres(random.nextInt(caracteres.length))
      codeTwint += charAleatoire
    }
    codeTwint
  }
  // Fonction pour préparer un Expresso
  def preparerExpresso(machineId: Int): Boolean = {
    var sucre = 0
    while (sucre < 1 || sucre > 4){
      println("\nBoisson sélectionnée : Expresso ")
      println("¿Souhaitez-vous ajouter du sucre ?")
      println("1) San sucre")
      println("2) Peu (5 g) - CHF 0.10")
      println("3) Moyen (10 g) - CHF 0.20")
      println("4) Beaucoup (15 g) - CHF 0.30")
      print(">")
      sucre = readLine().toInt
      if (sucre < 1 || sucre > 4){
      println("Veuillez sélectionner une option valide (1-4).")
      }
    }

    var cafe = 8
    var sucreUtilise = 0
    var prixSucre = 0.0

    if (sucre == 1) {
      cafe = 8
      sucreUtilise = 0
      prixSucre = 0.0
    } else if (sucre == 2) {
      cafe = 8
      sucreUtilise = 5
      prixSucre = 0.10
    } else if (sucre == 3) {
      cafe = 8
      sucreUtilise = 10
      prixSucre = 0.20
    } else if (sucre == 4) {
      cafe = 8
      sucreUtilise = 15
      prixSucre = 0.30
    }

    // Calcul du prix total

    val prixBase = 2.0
    val prixTotal = prixBase + prixSucre

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, 0, machineId)) {
      println(s"Prix total: $prixBase CHF + $prixSucre CHF = ${"%.2f".format(prixTotal)} CHF\n")
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Votre code de paiement est : $codeTwint")
      println("(En attente de validation du paiement...)\n")
      Thread.sleep(3000)
      println("Paiement corfirmé...")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)

      // Mettre à jour le stock après avoir préparé la boisson
      viderStock(cafe, sucreUtilise, 0, machineId)

      println("Votre Expresso est prêt ! Bonne dégustation !\n")
      return true
    } else {
      println("Veuillez choisir une autre boisson ou choisir une untre machine")
      println("vérifier les stocks en mode Admin.")
      return false
    }
  }

  def preparerCappuccino(machineId: Int): Unit = {
    var sucre = 0
    while (sucre < 1 || sucre > 4){
      println("\nBoisson sélectionnée : Cappuccino")
      println("¿Souhaitez-vous ajouter du sucre ?")
      println("1) San sucre")
      println("2) Peu (5 g) - CHF 0.10")
      println("3) Moyen (10 g) - CHF 0.20")
      println("4) Beaucoup (15 g) - CHF 0.30")
      print(">")
      sucre = readLine().toInt
      if (sucre < 1 || sucre > 4){
        println("Veuillez sélectionner une option valide (1-4).")
        print(">")
      }
    }

    // Sélection de lait supplémentaire
    var laitSupplementaire = 0
    while (laitSupplementaire < 1 || laitSupplementaire > 2) {
      println("¿Souhaitez-vous ajouter du lait en supplément ?")
      println(("Disponible uniquement pour Cappuccino et Latte"))
      println("1) Oui")
      println("2) Non")
      print(">")
      laitSupplementaire = readLine().toInt
      if (laitSupplementaire < 1 || laitSupplementaire > 2) {
        println("Veuillez sélectionner une option valide (1-2).")
        print(">")
      }
    }
    var doseLait = 0.0
    if (laitSupplementaire == 1) {
      var dose = 0
      while (dose < 1 || dose > 3) {
        println("¿Combien de dose? (Le coût d'une dose de lait supplémentaire est de CHF 0.05)")
        print(">")
        dose = readLine().toInt
        if (dose < 1 || dose > 3) {
          println("Veuillez sélectionner une option valide (1-3).")
          print(">")
        }
      }
      doseLait = dose * 0.05
    }


    // Déterminer les quantités de café, de sucre et de lait selon la sélection
    var cafe = 6
    var lait = 0.1 + doseLait * 0.05
    var sucreUtilise = 0
    if (sucre == 1) {
      cafe = 6
      lait = 0.1 + doseLait * 0.05
      sucreUtilise = 0
    } else if (sucre == 2) {
      cafe = 6
      lait = 0.1 + doseLait * 0.05
      sucreUtilise = 5
    } else if (sucre == 3) {
      cafe = 6
      lait = 0.1 + doseLait * 0.05
      sucreUtilise = 10
    } else if (sucre == 4) {
      cafe = 6
      lait = 0.1 + doseLait * 0.05
      sucreUtilise = 15
    }

    // Calcul du prix
    val prixBase = 2.50
    var prixSucre = 0.0
    if (sucreUtilise == 0) {
      prixSucre = 0.0
    } else if (sucreUtilise == 5) {
      prixSucre = 0.10
    } else if (sucreUtilise == 10) {
      prixSucre = 0.20
    } else if (sucreUtilise == 15) {
      prixSucre = 0.30
    }


    val prixLaitSupplementaire = doseLait
    val prixTotal = prixBase + prixSucre + prixLaitSupplementaire

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, lait, machineId)) {
      println(s"Prix total: $prixBase CHF + $prixSucre CHF + ${"%.2f".format(prixLaitSupplementaire)} CHF = ${"%.2f".format(prixTotal)} CHF\n")
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Votre code de paiement est: $codeTwint")
      println("(En attente de validation du paiement...)\n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté...")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)
      // Mettre à jour le stock après avoir préparé la boisson
      viderStock(cafe, sucreUtilise, lait, machineId)
    } else {
      println("Veuillez choisir une autre boisson ou choisir une untre machine")
      println("vérifier les stocks en mode Admin.")    }
  }
  // Preparar café con leche con las opciones de azúcar, tamaño y leche extra
  def preparerLatte(machineId: Int): Boolean = {
    println("\nBoisson sélectionnée: Latte")

    var sucre = 0
    while (sucre < 1 || sucre > 4){
      println("\nBoisson sélectionnée : Latte ")
      println("¿Souhaitez-vous ajouter du sucre ?")
      println("1) San sucre")
      println("2) Peu (5 g) - CHF 0.10")
      println("3) Moyen (10 g) - CHF 0.20")
      println("4) Beaucoup (15 g) - CHF 0.30")
      print(">")
      sucre = readLine().toInt
      if (sucre < 1 || sucre > 4){
        println("Veuillez sélectionner une option valide (1-4).")
        print(">")
      }
    }

    // Sélection de taille
    var taille = 0
    while (taille < 1 || taille > 3){
      println("¿Sélectionnez une taille?")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")
      print(">")
      taille = readLine().toInt
      if (taille < 1 || taille > 3){
        println("Veuillez sélectionner une option valide (1-3).")
        print(">")
      }
    }

    // Sélection de lait supplémentaire
    var laitSupplementaire = -1
    while (laitSupplementaire < 1 || laitSupplementaire > 2) {
      println("¿Souhaitez-vous ajouter du lait en supplément ?")
      println(("Disponible uniquement pour Cappuccino et Latte"))
      println("1) Oui")
      println("2) Non")
      print(">")
      laitSupplementaire = readLine().toInt
      if (laitSupplementaire < 1 || laitSupplementaire > 2) {
        println("Veuillez sélectionner une option valide (1-2).")
        print(">")
      }
    }
    var doseLait = 0.0
    if (laitSupplementaire == 1){
      var dose = 0
      while (dose < 1 || dose > 3) {
        println("¿Combien de dose? (Le coût d'une dose de lait supplémentaire est de CHF 0.05)")
        print(">")
        dose = readLine().toInt
        if (dose < 1 || dose > 3){
          println("Veuillez sélectionner une option valide (1-3).")
          print(">")
        }
      }
      doseLait = dose * 0.05
    }
    // Déterminer les quantités de café, de sucre et de lait selon la sélection
    var cafe = 6
    var lait = 0.12 + doseLait
    var sucreUtilise = 0
    if (taille == 1) {
      cafe = 6
      lait = 0.12 + doseLait
    } else if (taille == 2) {
      cafe = 8
      lait = 0.15 + doseLait
    } else if (taille == 3) {
      cafe = 12
      lait = 0.2 + doseLait
    }

    if (sucre == 1) {
      sucreUtilise = 0
    } else if (sucre == 2) {
      sucreUtilise = 5
    } else if (sucre == 3) {
      sucreUtilise = 10
    } else if (sucre == 4) {
      sucreUtilise = 15
    }

    // Calcul du prix
    var prixBase = 0.0
    if (taille == 1) {
      prixBase = 2.70  // Prix petit
    } else if (taille == 2) {
      prixBase = 3.20  // Prix moyen
    } else if (taille == 3) {
      prixBase = 3.70 // Prix grand
    }


    val prixSucre = {
      if (sucreUtilise == 0) {
        0.0
      } else if (sucreUtilise == 5) {
        0.10
      } else if (sucreUtilise == 10) {
        0.20
      } else {
        0.30
      }
    }

    val prixLaitSupplementaire = doseLait
    val prixTotal = prixBase + prixSucre + prixLaitSupplementaire

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, lait, machineId)) {
      println(s"Prix total: $prixBase CHF + $prixSucre CHF + ${"%.2f".format(prixLaitSupplementaire)} CHF = ${"%.2f".format(prixTotal)} CHF\n")
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Su código es: $codeTwint")
      println("(En attente de validation du paiement....)\n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté....")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)
      // Mettre à jour le stock après avoir préparé la boisson
      viderStock(cafe, sucreUtilise, lait, machineId)
      return true
    } else {
      println("Veuillez choisir une autre boisson ou choisir une untre machine")
      println("vérifier les stocks en mode Admin.")
      return false
    }
  }

  def verifierStock(cafe: Int, sucre: Int, lait: Double, machineId: Int): Boolean = {
    // Verificar cada ingrediente por separado y mostrar el mensaje correspondiente
    if (coffeeStocks(machineId) < cafe) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      return false
    } else if (sugarStocks(machineId) < sucre) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      return false
    } else if (milkStocks(machineId) < lait) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      return false
    } else {
      println("Tous les ingrédients sont disponibles.")
      return true
    }
  }

  // Servir le client sur la machine sélectionnée
  def serveClient(machineId: Int): Unit = {
    var choixBoisson = 0
    while (choixBoisson < 1 || choixBoisson > 3){
      println("\n Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino – CHF 2.50")
      println("3) Latte: CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print(">")
      choixBoisson = readLine().toInt
      if (choixBoisson < 1 || choixBoisson > 3) {
        println("Veuillez sélectionner une option valide (1-3).")
        print(">")
      }
    }
    // Traite l'option sélectionnée
    if (choixBoisson == 1) {
      preparerExpresso(machineId)
    } else if (choixBoisson == 2) {
      preparerCappuccino(machineId)
    } else if (choixBoisson == 3) {
      preparerLatte(machineId)
    }
  }

  // Réapprovisionner les ingrédients de la machine sélectionnée
  def restockMachine(machineId: Int): Unit = {
    println(s"Réapprovisionner la machine ${machineId + 1}:")
    print("Entrez les quantités à ajouter :\n")
    print("Poudre de café > ")
    val cafe = readLine().toInt
    print("Sucre >")
    val sucre = readLine().toInt
    print("Lait >")
    val input = readLine()
    val lait = input.replace(",", ".").toDouble

    coffeeStocks(machineId) += cafe
    sugarStocks(machineId) += sucre
    milkStocks(machineId) += lait

    println("Les Stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  // Fonction de mise à jour du stock par machine
  def viderStock(cafe: Int, sucre: Int, lait: Double, machineId: Int): Unit = {
    coffeeStocks(machineId) -= cafe
    sugarStocks(machineId) -= sucre
    milkStocks(machineId) -= lait
  }

  // Afficher le stock d'ingrédients pour la machine sélectionnée
  def voirStock(machineId: Int): Unit = {
    println(s"\nNiveaux de stocks actuels :")
    println(s"Café: ${coffeeStocks(machineId)}g")
    println(s"Sucre: ${sugarStocks(machineId)}g")
    println(s"Lait: ${"%.2f".format(milkStocks(machineId))}L")
  }
}