// Début du programme : Nospresso Café
import scala.io.StdIn.readLine

object Main {

  // Déclaration des stocks au début :
  // Stock de base de café en grammes
  var stockCafe = 50
  // Stock de base de sucre en grammes
  var stockSucre = 30
  // Stock de base de lait en litres
  var stockLait = 0.5
  val pinAdmin = "434343" // Code PIN pour le mode admin

// fonction principales
  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      // affichage menue
      println("\n--- Nospresso Café ---")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")
      val choix = lireEntree()

        // mode client
      if (choix == "1") {
        modeClient()
          // mode administrateur
      } else if (choix == "2") {
        modeAdmin()
      } else if (choix == "3") {
        println("Merci d'avoir utilisé Nospresso !")
        continuer = false
      } else {
        println("Choix invalide, veuillez réessayer.")
      }
    }
  }

  // Gestion des commandes client
  def modeClient(): Unit = {
    println("\n--- Mode Client ---")
    println("1) Expresso (2.00 CHF)")
    println("2) Cappuccino (2.50 CHF)")
    println("3) Latte (Petit: 2.70 CHF, Moyen: 3.20 CHF, Grand: 3.70 CHF)")
    print("> ")
    val choixBoisson = lireEntree()

    val (cafeNecessaire, laitNecessaire, prixBase) = if (choixBoisson == "1") { // option expresso
      (8, 0.0, 2.0)
      // option capuccino
    } else if (choixBoisson == "2") {
      (6, 0.1, 2.5)
      // option latte
    } else if (choixBoisson == "3") {
      choisirTailleLatte()
    } else {
      println("Choix invalide, retour au menu.") // erreur ne fait pas partie des choix
      return
    }

    // Personnalisation avec le sucre
    println("Souhaitez-vous ajouter du sucre ? (1: Sans sucre, 2: Peu, 3: Moyen, 4: Beaucoup)")
    print("> ")
    val choixSucre = lireEntree()
    val (sucreNecessaire, prixSucre) = if (choixSucre == "1") { // sans sucre
      (0, 0.0)
      // option = peu de sucre
    } else if (choixSucre == "2") {
      (5, 0.1)
      // option = sucre moyen
    } else if (choixSucre == "3") {
      (10, 0.2)
      // option = beaucoup de sucre
    } else if (choixSucre == "4") {
      (15, 0.3)
    } else {
      println("Choix invalide, retour au menu.") // erreur pas valide
      return
    }

    // Ajout de lait supplémentaire qui est = a 0.05 CHF
    // Chaque dose = 50ml
    val laitSupplementaire = demanderDoseLait()
    val laitTotal = laitNecessaire + laitSupplementaire * 0.05

    // Vérification des stocks et calcul final
    if (verifierStock(cafeNecessaire, sucreNecessaire, laitTotal)) {
      val prixTotal = prixBase + prixSucre + laitSupplementaire * 0.05
      val prixArrondi = (prixTotal * 100).toInt / 100.0 // Arrondi
      val codeTwint = genererCodeTwint()

      println("Prix total : " + prixArrondi + " CHF")
      println("Code de paiement Twint : " + codeTwint)
      println("Préparation de votre boisson...")
      Thread.sleep(3000)
      println("Votre boisson est prête ! Bonne dégustation.")
      mettreAJourStock(cafeNecessaire, sucreNecessaire, laitTotal)
    } else {
      // message indiquant qu'il n'y a pas assez de stock donc remplir
      println("Désolé, les stocks sont insuffisants.")
    }
  }

  // Choix de la taille du Latte
  def choisirTailleLatte(): (Int, Double, Double) = {
    println("Choisissez la taille du Latte :")
    println("1) Petit (2.70 CHF)")
    println("2) Moyen (3.20 CHF)")
    println("3) Grand (3.70 CHF)")
    print("> ")
    val choixTaille = lireEntree()

    // Latte Petit
    if (choixTaille == "1") {
      (6, 0.12, 2.7)
      // Latte Moyen
    } else if (choixTaille == "2") {
      (8, 0.15, 3.2)
      // Latte Grand
    } else if (choixTaille == "3") {
      (12, 0.2, 3.7)
    } else {
      println("Choix invalide, retour au menu.")
      (0, 0.0, 0.0) // Valeurs par défaut en cas d'erreur
    }
  }

  // Demande pour ajouter des doses de lait
  def demanderDoseLait(): Int = {

    // Ajout de lait supplémentaire
    val laitSupplementaire = demanderDoseLait()
    val laitTotal = laitNecessaire + laitSupplementaire * 0.05 // Chaque dose = 50ml

    println("Souhaitez-vous ajouter du lait supplémentaire ? (Chaque dose = 50ml, coût : 0.05 CHF)")
    println("Combien de doses souhaitez-vous ajouter ? (0 à 3)")
    print("> ")
    val choixLait = lireEntree()

    if (choixLait == "0") {
      0
    } else if (choixLait == "1") {
      1
    } else if (choixLait == "2") {
      2
    } else if (choixLait == "3") {
      3
    } else {
      println("Choix invalide, aucune dose supplémentaire ajoutée.")
      0
    }
  }

  // Mode Admin pour gérer les stocks
  def modeAdmin(): Unit = {
    println("\n--- Mode Admin ---")
    print("Entrez le code PIN : ")
    val pin = lireEntree()

    if (pin == pinAdmin) {
      println("Stock actuel : Café: " + stockCafe + " g, Sucre: " + stockSucre + " g, Lait: " + stockLait + " L")
      // demande d'ajout de cafee
      println("Ajoutez du café (en grammes) :")
      val cafeAjoute = lireEntree().toInt
      // demande d'ajout de sucre
      println("Ajoutez du sucre (en grammes) :")
      val sucreAjoute = lireEntree().toInt
      // deamande d'ajout de lait
      println("Ajoutez du lait (en litres) :")
      val laitAjoute = lireEntree().toDouble

      stockCafe += cafeAjoute
      stockSucre += sucreAjoute
      stockLait += laitAjoute
      println("Nouveau stock : Café: " + stockCafe + " g, Sucre: " + stockSucre + " g, Lait: " + stockLait + " L")
    } else {
      println("Code PIN incorrect.")
    }
  }

  // Vérification des stocks
  def verifierStock(cafe: Int, sucre: Int, lait: Double): Boolean = {
    stockCafe >= cafe && stockSucre >= sucre && stockLait >= lait
  }

  // Mise à jour des stocks
  def mettreAJourStock(cafe: Int, sucre: Int, lait: Double): Unit = {
    stockCafe -= cafe
    stockSucre -= sucre
    stockLait -= lait
  }

  // Génération du code Twint
  def genererCodeTwint(): String = {
    "AB12X" // Code deja fixe
  }

  // Lecture des entrées utilisateur
  def lireEntree(): String = {
    readLine()
  }
}
