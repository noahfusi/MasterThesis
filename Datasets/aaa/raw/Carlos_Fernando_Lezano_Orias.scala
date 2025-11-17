import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  // stock initial
  var stockCafe = 50.0  // en grammes
  var stockSucre = 30.0 // en grammes
  var stockLait = 0.5 // en litres

  // PIN d'administrateur
  val pinAdmin = "434343"

  // Función principal para mostrar el menú
  def main(args: Array[String]): Unit = {
    var Quitter = false
    while (!Quitter) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")

      val mode = readLine().toInt

      if (mode == 1) {
        modeClient()  // Mode Client
      } else if (mode == 2) {
        modeAdmin()  // Mode Admin
      } else if (mode == 3) {
        println("Quitter le programme...")
        Quitter = true  // terminer le programme
      } else {
        println("Option invalide.")
      }
    }
  }

  // Fonction pour le mode Client
  def modeClient(): Unit = {
    println("\n Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino – CHF 2.50")
    println("3) Latte: CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    println(">")

    val choixBoisson = readLine().toInt
    if (choixBoisson == 1) {
      preparerExpresso()
    } else if (choixBoisson == 2) {
      preparerCappuccino()
    } else if (choixBoisson == 3) {
      preparerLatte()
    } else {
      println("Opción no válida.")
    }
  }

  // Fonction pour préparer un Expresso
  def preparerExpresso(): Unit = {
    println("\nBoisson sélectionnée : Expresso ")
    println("¿Souhaitez-vous ajouter du sucre ?")
    println("1) San sucre")
    println("2) Peu (5 g) - CHF 0.10")
    println("3) Moyen (10 g) - CHF 0.20")
    println("4) Beaucoup (15 g) - CHF 0.30")
    println(">\n")

    val sucre = readLine().toInt

    var cafe = 8
    var sucreUtilise = 0
    var prixSucre = 0.0

    if (sucre == 1) {
      cafe = 8
      sucreUtilise= 0
      prixSucre = 0.0
    } else if (sucre == 2) {
      cafe = 8
      sucreUtilise = 5
      prixSucre = 0.10 // Prix pour 5g de sucre
    } else if (sucre == 3) {
      cafe = 8
      sucreUtilise = 10
      prixSucre = 0.20 // Prix pour 10g de sucre
    } else if (sucre == 4) {
      cafe = 8
      sucreUtilise = 15
      prixSucre = 0.30 // Prix pour 15g de sucre
    } else {
      println("Option invalide. Il sera utilisé sans sucre par défaut.")
      cafe = 8
      sucreUtilise = 0
      prixSucre = 0.0
    }

    // Calcul du prix total

    val prixBase = 2.0
    val prixTotal = prixBase + prixSucre

    println(s"Prix total: $prixBase CHF + $prixSucre CHF = ${"%.2f".format(prixTotal)} CHF\n")

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, 0)) {
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Votre code de paiement est : $codeTwint")
      println("(En attente de validation du paiement...)\n")
      Thread.sleep(3000) // Esperar 3 segundos para simular el pago
      println("Paiement corfirmé...")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)

      // Mettre à jour le stock après avoir préparé la boisson
      effectuerPaiement(prixBase, cafe, sucreUtilise, 0)

      println("Votre Expresso est prêt ! Bonne dégustation !\n")
    } else {
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
  }

  // Fonction pour préparer un Cappuccino
  def preparerCappuccino(): Unit = {
    println("\nBoisson sélectionnée : Cappuccino")

    // Sélection de sucre
    println("¿Souhaitez-vous ajouter du sucre ?")
    println("1) San sucre")
    println("2) Peu (5 g) - CHF 0.10")
    println("3) Moyen (10 g) - CHF 0.20")
    println("4) Beaucoup (15 g) - CHF 0.30")
    println(">")
    val sucre = readLine().toInt

    // Sélection de lait supplémentaire
    println("¿Souhaitez-vous ajouter du lait en supplément ?")
    println("(Disponible uniquement pour Cappuccino et Latte)")
    println("1) Oui")
    println("2) Non")
    println(">")
    val laitSupplementaire = readLine().toInt
    var doseLait = 0
    if (laitSupplementaire == 1) {
      println("¿Combien de dose ? (Le coût d'une dose de lait supplémentaire est de CHF 0.05)")
      println(">")
      doseLait = readLine().toInt
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
    } else {
      println("Option invalide. Il sera utilisé sans sucre par défaut.")
      cafe = 6
      lait = 0.1 + doseLait * 0.05
      sucreUtilise = 0
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

    val prixLaitSupplementaire = doseLait * 0.05

    val prixTotal = prixBase + prixSucre + prixLaitSupplementaire

    println(s"Prix total: $prixBase CHF + $prixSucre CHF + $prixLaitSupplementaire CHF = ${"%.2f".format(prixTotal)} CHF\n")

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, lait)) {
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Votre code de paiement est: $codeTwint")
      println("(En attente de validation du paiement...)\n")
      Thread.sleep(3000)  // Attendez 3 secondes pour simuler le paiement
      println("Merci ! Votre paiement a été accepté...")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)

      // Mettre à jour le stock après avoir préparé la boisson
      effectuerPaiement(prixTotal, cafe, sucreUtilise, lait)

      println("¡Votre Cappuccino est prêt ! ¡ Bonne Déguistation !\n")
      Thread.sleep(5000)
    } else {
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
  }

  // Fonction pour préparer un café au lait
  def preparerLatte(): Unit = {
    println("\nBoisson sélectionnée: Latte")
    // Sélection de sucre
    println("¿Souhaitez-vous ajouter du sucre ?")
    println("1) San sucre")
    println("2) Peu (5 g) - CHF 0.10")
    println("3) Moyen (10 g) - CHF 0.20")
    println("4) Beaucoup (15 g) - CHF 0.30")
    println(">")
    val sucre = readLine().toInt
    // Sélection de taille
    println("¿Sélectionnez une taille?")
    println("1) Petit")
    println("2) Moyen")
    println("3) Grand")
    println(">")
    val taille = readLine().toInt

    // Sélection de lait supplémentaire
    println("¿Souhaitez-vous ajouter du lait en supplément ?")
    println("(Disponible uniquement pour Cappuccino et Latte)")
    println("1) Oui")
    println("2) Non")
    println(">")
    val laitSupplementaire = readLine().toInt

    var doseLait = 0.0
    if (laitSupplementaire == 1) {
      println("¿Combien de dose? (Le coût d'une dose de lait supplémentaire est de CHF 0.05)")
      println(">")
      doseLait = readLine().toInt * 0.05 // Chaque dose supplémentaire augmente le prix
    }

    // Déterminer les quantités de café, de sucre et de lait selon la sélection
    var cafe = 6
    var lait = 0.12 + doseLait * 0.05
    var sucreUtilise = 0
    if (taille == 1) {
      cafe = 6
      lait = 0.12 + doseLait * 0.05
    } else if (taille == 2) {
      cafe = 8
      lait = 0.15 + doseLait * 0.05
    } else if (taille == 3) {
      cafe = 12
      lait = 0.2 + doseLait * 0.05
    } else {
      println("Option invalide. La petite taille sera utilisée par défaut.")
      cafe = 6
      lait = 0.12 + doseLait * 0.05
    }

    if (sucre == 1) {
      sucreUtilise = 0
    } else if (sucre == 2) {
      sucreUtilise = 5
    } else if (sucre == 3) {
      sucreUtilise = 10
    } else if (sucre == 4) {
      sucreUtilise = 15
    } else {
      println("Option invalide. Il sera utilisé sans sucre par défaut.")
      sucreUtilise = 0
    }

    // Calcul du prix
    var prixBase = 0.0
    if (taille == 1) {
      prixBase = 2.70  // Prix petit
    } else if (taille == 2) {
      prixBase = 3.20  // Prix moyen
    } else if (taille == 3) {
      prixBase = 3.70  // Prix grand
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
    println(s"Prix total: $prixBase CHF + $prixSucre CHF + $prixLaitSupplementaire CHF = ${"%.2f".format(prixTotal)} CHF\n")

    // Vérification des stocks et paiement
    if (verifierStock(cafe, sucreUtilise, lait)) {
      println("Veuillez payer en utilisant  Twint.")
      val codeTwint = genererCodeTwint()
      println(s"Votre code de paiement est: $codeTwint")
      println("(En attente de validation du paiement....)\n")
      Thread.sleep(3000)  // Attendez 3 secondes pour simuler le paiement
      println("Merci ! Votre paiement a été accepté....")
      println("Préparation de votre boisson ....")
      Thread.sleep(5000)

      // Mettre à jour le stock après avoir préparé la boisson
      effectuerPaiement(prixTotal, cafe, sucreUtilise, lait)

      println("¡Votre Latte est prêt ! Bonne dégustation!\n")
      Thread.sleep(5000)
    } else {
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
  }


  def verifierStock(cafe: Int, sucre: Int, lait: Double): Boolean = {
    // Verificar cada ingrediente por separado y mostrar el mensaje correspondiente
    if (stockCafe < cafe) {
      println("Erreur : Quantité de poudre de café insuffisante pour prèparer la boisson sélectionnée .")
      false
    } else if (stockSucre < sucre) {
      println("Erreur : Quantité du sucre  insuffisante pour prèparer la boisson sélectionnée.")
      false
    } else if (stockLait < lait) {
      println("Erreur : Quantité du lait insuffisante pour prèparer la boisson sélectionnée.")
      false
    } else {
      println("Tous les ingrédients sont disponibles.")
      true
    }
  }


  // Fonction pour effectuer le paiement
  def effectuerPaiement(prix : Double, cafe: Int, sucre: Int, lait: Double): Unit = {
    stockCafe -= cafe
    stockSucre -= sucre
    stockLait -= lait
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


  // Fonction pour le mode Admin
  def modeAdmin(): Unit = {
    println("\nEntrez le code Pin: ******")
    val pin = readLine()

    if (pin == pinAdmin) {
      println("Accès autorisé")
      println("Options d'administrateur:")
      println("1) Voir stock d'ingrédients")
      println("2) Réapprovisionnement des stocks....")
      val choixAdmin = readLine().toInt

      if (choixAdmin == 1) {
        voirStock()
      } else if (choixAdmin == 2) {
        ajouterStock()
      } else {
        println("Option invalide.")
      }
    } else {
      println("PIN incorrect. Accès refusé.")
    }
  }

  // Fonctionnalité pour afficher le stock
  def voirStock(): Unit = {
    println(s"\nStock actuel:")
    println(s"Café: $stockCafe grammes")
    println(s"Sucre: $stockSucre grammes")
    println(s"Lait: $stockLait litres\n")
  }

  // Fonction de réapprovisionnement des stocks
  def ajouterStock(): Unit = {
    println("Réapprovisionnement des stocks . . .")
    println("Ajout")
    println("\tPoudre de café: ")
    val cafe = readLine().toInt
    println("\tsucre: ")
    val sucre = readLine().toInt
    println("\tlait:")
    val lait = readLine().toInt

    mettreajourStocks(cafe, sucre, lait)
    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }

  def mettreajourStocks(cafe: Int, sucre: Int, lait: Float): Unit = {
    stockCafe += cafe
    stockSucre += sucre
    stockLait += lait
    println(s"Stocks mis à jour: Café=$stockCafe g, sucre=$stockSucre g, lait=$stockLait ml.")
  }
}