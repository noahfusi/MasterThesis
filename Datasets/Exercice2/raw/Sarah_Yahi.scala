
import scala.io.StdIn._
import math._

object Main {

  // Constantes
  val nbMachines = 5 // Nombre de machines (0 a 4)
  val defaultPin = "434343" // Code PIN par défaut


  // Tableau pour les stocks et les codes PIN
  // Initialisation des tableau
  var coffeeStocks = new Array[Int](nbMachines)
  var sugarStocks = new Array[Int](nbMachines)
  var milkStocks = new Array[Int](nbMachines)
  var machinePins = new Array[String](nbMachines)

  // Remplissage des tableau
  var i = 0
  while (i < nbMachines) {
    coffeeStocks(i) = 50
    sugarStocks(i) = 30
    milkStocks(i) = 500
    machinePins(i) = defaultPin
    i += 1
  }


  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      // interface de présentation
      println("\nNospresso Café - Gestion Multi-Machines")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choix = lireEntreeNumerique()

      //choix client
      if (choix == 1) {
        val machineId = selectionnerMachine()
        modeClient(machineId)

        //choix admin
      } else if (choix == 2) {
        val machineId = selectionnerMachine()
        if (validatePin(machineId, machinePins)) {
          adminMenu(machineId)
        } else {
          println("Trop de tentatives échouées. Retour au menu principal.")//message d'erreur après 3 code pin erroné
        }

        //choix quitter
      } else if (choix == 3) {
        println("Au revoir !")
        continuer = false
      } else {

        //message d'erreur si choix invalide
        println("Choix invalide, veuillez réessayer.")
      }
    }
  }

//fonction pour selectionner une machine
  def selectionnerMachine(): Int = {
    var machineId = -1
    var machineValide = false

    while (machineValide == false) {
      println("Veuillez sélectionner une machine (0 à 4) :")
      machineId = lireEntreeNumerique()

      if (machineId >= 0 && machineId < nbMachines) {
        machineValide = true
      } else {
        //message d'erreur si choix invalide
        println("Numéro de machine invalide. Veuillez réessayer.")
      }
    }
    machineId
  }

  //fonction pour aficher les stoks (mode admin)
   def afficherStocks(machineId: Int): Unit = {
    println("Stocks actuels de la machine " + machineId + ":")
    println("- Café : " + coffeeStocks(machineId) + " g")
    println("- Sucre : " + sugarStocks(machineId) + " g")
    println("- Lait " + milkStocks(machineId) + " ml")
    println("-" * 30)
  }

  //fonction pour le code pin du mode admin
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN pour la machine " + machineId + " :")
      val entree = scala.io.StdIn.readLine()

      if (entree == machinePins(machineId)) {
        println("Code PIN correct.")
        return true
      } else {
        tentatives -= 1
        println("Code PIN incorrect. Tentative restantes :" + tentatives + ".")//3 tentative max
      }
    }
    //message d'erreur si trop de tentatives erroné
    println("Accès refusé après trop de tentatives.")
    false
  }

  //mode client
  def modeClient(machineId: Int): Unit = {
    println("Vous utilisez la machine " + machineId + ".")
    var choixValide = false
    var choixBoisson = 0

    while (choixValide == false) {
      //interface de présentqtion
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      choixBoisson = lireEntreeNumerique()

      if (choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3) {
        choixValide = true
      } else {
        //message d'erreur
        println("Choix invalide, veuillez réessayer.")
      }
    }

    var cafeNecessaire = 0
    var laitNecessaire = 0
    var prixBase = 0.0
    var prixtaille = 0.0

    if (choixBoisson == 1) { // Expresso
      cafeNecessaire = 8
      prixBase = 2.0
    } else if (choixBoisson == 2) { // Cappuccino
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.5
    } else if (choixBoisson == 3) { // Latte
      var tailleValide = false
      var taille = 0
      prixBase = 2.7

      // 3 tailles differrente seulement pour latte
      while (tailleValide == false) {
        println("Choisissez la taille :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")

        taille = lireEntreeNumerique()

        if (taille == 1 || taille == 2 || taille == 3) {
          tailleValide = true
        } else {
          //message d'erreur
          println("Choix invalide, veuillez réessayer.")
        }
      }

      if (taille == 1) {
        cafeNecessaire = 6
        laitNecessaire = 120
        prixtaille = 0
      } else if (taille == 2) {
        cafeNecessaire = 8
        laitNecessaire = 150
        prixtaille = 0.5
      } else if (taille == 3) {
        cafeNecessaire = 12
        laitNecessaire = 200
        prixtaille = 1
      }
    }

    //ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre - CHF 0.00")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    var sucreNecessaire = 0
    var prixSucre = 0.0
    var choixSucre = 0
    var choixSucreValide = false

   // sucre necessaire et prix selon le choix
    while (choixSucreValide == false) {
      choixSucre = lireEntreeNumerique()
      if (choixSucre == 1) {
        sucreNecessaire = 0
        prixSucre = 0.0
        choixSucreValide = true
      } else if (choixSucre == 2) {
        sucreNecessaire = 5
        prixSucre = 0.10
        choixSucreValide = true
      } else if (choixSucre == 3) {
        sucreNecessaire = 10
        prixSucre = 0.20
        choixSucreValide = true
      } else if (choixSucre == 4) {
        sucreNecessaire = 15
        prixSucre = 0.30
        choixSucreValide = true
      } else {
        println("Choix invalide, veuillez réessayer.") // Message d'erreur
      }
    }

    //ajout de lait
    var laitSupplementaire = 0
    var prixLait = 0.0

    // Validation pour le lait (uniquement pour Cappuccino et Latte)
    if (choixBoisson == 2 || choixBoisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")

      var choixLait = 0
      var choixLaitValide = false

      while (!choixLaitValide) {
        choixLait = lireEntreeNumerique()
        if (choixLait == 1) {
          println("Combien de doses ? (Maximum 3) - CHF 0.05 par dose")

          var doses = -1
          var dosesValides = false

          while (dosesValides == false) {
            doses = lireEntreeNumerique()
            if (doses >= 0 && doses <= 3) {
              dosesValides = true
            } else {
              println("Erreur : Veuillez entrer un nombre entre 0 et 3.") // Validation des doses
            }
          }

          laitSupplementaire = doses * 50 // Chaques dose correspond à 50 ml
          prixLait = doses * 0.05 // Prix de 0.05 CHF par dose
          choixLaitValide = true
        } else if (choixLait == 2) {
          laitSupplementaire = 0
          prixLait = 0.0
          choixLaitValide = true
        } else {
          println("Choix invalide, veuillez réessayer.") // Message d'erreur
        }
      }
    }

    // verification des stocks
    if (coffeeStocks(machineId) < cafeNecessaire ||
      milkStocks(machineId) < (laitNecessaire + laitSupplementaire) ||
      sugarStocks(machineId) < sucreNecessaire) {
      // message d'erreur si stocks inssufisant
      println("Stock insuffisant pour préparer la boisson.")
      return
    }

    coffeeStocks(machineId) -= cafeNecessaire
    milkStocks(machineId) -= (laitNecessaire + laitSupplementaire)
    sugarStocks(machineId) -= sucreNecessaire

    //calcul du prix final
    val prixFinal = prixBase + prixSucre + prixLait + prixtaille
    printf("Le prix total est : CHF %.2f\n", prixFinal)

    //payement avec twint
    println("Veuillez payer avec Twint. \nVotre code de paiement est : " + genererCodeTwint())
    println("En attente de validation du paiement...")
    Thread.sleep(3000) // pause de 3 seconde + realiste
    println("Paiement confirmé. Préparation de votre boisson...")

    println("Votre boisson est prête ! Bonne dégustation.")
  }

  // fonction pour génerer un code twint alphanumérique (pareil que l'ex 1)
  def genererCodeTwint(): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    //caractere 1
    val randomIndex1 = (math.random * caracteres.length).toInt
    val caractere1 = caracteres(randomIndex1)

    //caractere 2
    val randomIndex2 = (math.random * caracteres.length).toInt
    val caractere2 = caracteres(randomIndex2)

    //caractere 3
    val randomIndex3 = (math.random * caracteres.length).toInt
    val caractere3 = caracteres(randomIndex3)

    //caractere 4
    val randomIndex4 = (math.random * caracteres.length).toInt
    val caractere4 = caracteres(randomIndex4)

    //caractere 5
    val randomIndex5 = (math.random * caracteres.length).toInt
    val caractere5 = caracteres(randomIndex5)

    //additionner tout les caracteres pour cree le code
    caractere1.toString + caractere2.toString + caractere3.toString + caractere4.toString + caractere5.toString
  }

  // menu du mode admin
  def adminMenu(machineId: Int): Unit = {
    var continuerAdmin = true
    while (continuerAdmin) {

      // interface de presentation
      println("Menu administrateur :")
      println("1) Réapprovisionner les stocks")
      println("2) Mettre à jour le code PIN")
      println("3) Retour au menu principal")
      print("> ")

      val choix = lireEntreeNumerique()

      //choix gerer le stock
      if (choix == 1) {
        restockMachine(machineId)

        //choix changer le code pin
      } else if (choix == 2) {
        updatePin(machineId, machinePins)

        //choix retout au menu principale
      } else if (choix == 3) {
        continuerAdmin = false
      } else {
        //message d'erreur si choix invalide
        println("Choix invalide, veuillez réessayer.")
      }
    }
  }

  // gerer le stock de la machine selectionée
  def restockMachine(machineId: Int): Unit = {

    afficherStocks(machineId) // Affiche les stocks avant le réapprovisionnement

    println("Reapprovisionnement des stocks pour la machine " + machineId + " :")

    //ajout de café
    println("Quantité de café a ajouter (en g) :")
    val ajoutCafe = lireEntreeNumerique()
    coffeeStocks(machineId) += ajoutCafe

    //ajout de sucre
    println("Quantité de sucre a ajouter (en g) :")
    val ajoutSucre = lireEntreeNumerique()
    sugarStocks(machineId) += ajoutSucre

    //ajout de lait
    println("Quantité de lait a ajouter (en ml) :")
    val ajoutLait = lireEntreeNumerique()
    milkStocks(machineId) += ajoutLait

    afficherStocks(machineId) // Affiche les stocks après le réapprovisionnement

    println("Les stocks de la machine " + machineId + " ont été mis à jour avec succès.")
  }

  // changer le pin de la machine selectionné
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + machineId + ".")
    var valide = false
    while (valide == false) {
      println("Entrez un nouveau code PIN (exactement 6 chiffres) :")
      val nouveauPin = scala.io.StdIn.readLine()

      // Vérification
      var tousChiffres = true
      var longueur = 0

      for (caractere <- nouveauPin) {
        longueur += 1
        if (caractere < '0' || caractere > '9') {
          tousChiffres = false
        }
      }

      if (longueur == 6 && tousChiffres) {
        machinePins(machineId) = nouveauPin
        println("Le code PIN a été mis à jour avec succès.")
        valide = true
      } else {
        println("Erreur : le code PIN doit comporter exactement 6 chiffres.")
      }
    }
  }


  // Fonction pour lire une entrée numérique (pareil que ex 1)
  def lireEntreeNumerique(): Int = {
    var estValide = false // Indique si l'entrée est valide
    var nombre = 0 // Stock le nombre validé

    while (!estValide) {

      val entree = scala.io.StdIn.readLine() // Lecture de l'entrée utilisateur

      // Calcul de la longueur de l'entrée
      var longueur = 0
      for (nombre <- entree) {
        longueur += 1
      }

      // Vérifier si l'entrée est vide
      if (longueur > 0) {
        var tousChiffres = true // Indique si tous les caractère sont des chiffres
        var index = 0 // Index pour parcourir les caractère

        // Vérification caractère par caractère
        while (index < longueur && tousChiffres) {
          val caractere = entree(index)
          if (caractere < '0' || caractere > '9') {
            tousChiffres = false // Un caractère n'est pas un chiffre
          }
          index += 1 // Avancer au caractère suivant
        }

        //convertir l'entrée en entier
        if (tousChiffres) {
          var resultat = 0
          index = 0

          while (index < longueur) {
            val chiffre = entree(index) - '0' // Convertir le caractère en chiffre
            resultat = resultat * 10 + chiffre
            index += 1
          }
          nombre = resultat
          estValide = true // Entrée valide

          //si entrée invalide afficher message d'erreur
        } else {
          println("Erreur : Veuillez entrer uniquement des chiffres.") // Message d'erreur
        }
      } else {
        println("Erreur : L'entrée ne peut pas être vide.") // Message pour entrée vide
      }
    }
    nombre
  }
}
