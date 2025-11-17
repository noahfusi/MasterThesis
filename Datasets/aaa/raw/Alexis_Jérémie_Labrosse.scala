import scala.io.StdIn.readLine
import scala.util.Random

object Nospresso {
  var stockCafe: Double = 50.0 // en grammes
  var stockSucre: Double = 30.0 // en grammes
  var stockLait: Double = 500.0 // en millilitres
  val pinAdmin = "434343" // code pin

  def main(args: Array[String]): Unit = {
    mainMenu()
  }

  // Menu principal
  def mainMenu(): Unit = {
    var continue = true
    while (continue) {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val selection = readLine().toIntOption.getOrElse(-1)
      if (selection == 1) {
        clientMode()
      } else if (selection == 2) {
        adminMode()
      } else if (selection == 3) {
        println("Merci d'avoir utilisé Nospresso. À bientôt !")
        continue = false
      } else {
        println("Sélection invalide, veuillez réessayer.")
      }
    }
  }

  // Mode Client
  def clientMode(): Unit = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val choixBoisson = readLine().toIntOption.getOrElse(-1)
    var price = 0.0

    if (choixBoisson == 1) {
      if (verifierStock(8, 0, 0)) {
        price = 2.0
      } else {
        returnToClientMenu()
      }
    } else if (choixBoisson == 2) {
      if (verifierStock(6, 0, 100)) {
        price = 2.5
      } else {
        returnToClientMenu()
      }
    } else if (choixBoisson == 3) {
      price = choisirTailleLatte()
      if (price == 0.0) {
        returnToClientMenu()
      }
    } else {
      println("Boisson invalide.")
      returnToClientMenu()
    }

    val prixSucre = choisirSucre()
    val prixLait = if (choixBoisson == 2 || choixBoisson == 3) choisirLaitSupplementaire() else 0.0
    val prixFinal = price + prixSucre + prixLait

    println(f"Prix total : CHF $prixFinal%.2f")
    traiterPaiement()
    preparerBoisson(choixBoisson)
  }

  // Sélection de la taille pour le Latte
  def choisirTailleLatte(): Double = {
    println("Sélectionnez la taille du Latte :")
    println("1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
    print("> ")

    val taille = readLine().toIntOption.getOrElse(-1)
    if (taille == 1 && verifierStock(6, 0, 120)) {
      2.7
    } else if (taille == 2 && verifierStock(8, 0, 150)) {
      3.2
    } else if (taille == 3 && verifierStock(12, 0, 200)) {
      3.7
    } else {
      if (taille != 1 && taille != 2 && taille != 3) {
        println("Taille invalide.")
      }
      returnToClientMenu()
      0.0
    }
  }

  // Sélection du niveau de sucre
  def choisirSucre(): Double = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val sucre = readLine().toIntOption.getOrElse(0)
    if (sucre == 2 && stockSucre >= 5) {
      stockSucre -= 5
      0.1
    } else if (sucre == 3 && stockSucre >= 10) {
      stockSucre -= 10
      0.2
    } else if (sucre == 4 && stockSucre >= 15) {
      stockSucre -= 15
      0.3
    } else if (sucre == 1) {
      println("Pas de sucre ajouté.")
      0.0
    } else {
      println("Sucre insuffisant ou choix invalide.")
      returnToClientMenu()
      0.0
    }
  }

  // Option pour ajouter du lait
  def choisirLaitSupplementaire(): Double = {
    println("Souhaitez-vous ajouter du lait en supplément (0.05 CHF par dose, max 3 doses) ?")
    println("1) Oui\n2) Non")
    print("> ")

    val choix = readLine().toIntOption.getOrElse(0)
    if (choix == 1) {
      println("Combien de doses ? (max 3)")
      val doses = readLine().toIntOption.getOrElse(0)
      val quantiteLait = doses * 50

      if (doses <= 3 && stockLait >= quantiteLait) {
        stockLait -= quantiteLait
        doses * 0.05
      } else {
        println("Quantité de lait insuffisante.")
        returnToClientMenu()
        0.0
      }
    } else {
      0.0
    }
  }

  // Vérification du stock
  def verifierStock(cafe: Double, sucre: Double, lait: Double): Boolean = {
    val stockOk = stockCafe >= cafe && stockSucre >= sucre && stockLait >= lait
    if (stockOk) {
      stockCafe -= cafe
      stockSucre -= sucre
      stockLait -= lait
    } else {
      if (stockCafe < cafe) println("Quantité de café insuffisante.")
      if (stockSucre < sucre) println("Quantité de sucre insuffisante.")
      if (stockLait < lait) println("Quantité de lait insuffisante.")
    }
    stockOk
  }

  // Traitement du paiement
  def traiterPaiement(): Unit = {
    val codePaiement = CodePaiement()
    println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $codePaiement")
    Thread.sleep(3000)
    println("Paiement accepté. Merci !")
  }

  // Code pour le paiement
  def CodePaiement(): String = {
    val allowedChars = ('A' to 'Z') ++ ('0' to '9')
    (1 to 5).map(_ => allowedChars(Random.nextInt(allowedChars.length))).mkString("")
  }

  // Préparation de la boisson choisie
  def preparerBoisson(choix: Int): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    if (choix == 1) println("Votre Expresso est prêt ! Bonne dégustation !")
    else if (choix == 2) println("Votre Cappuccino est prêt ! Bonne dégustation !")
    else if (choix == 3) println("Votre Latte est prêt ! Bonne dégustation !")
  }

  // Retour au menu utilisateur
  def returnToClientMenu(): Unit = {
    println("Retour au menu utilisateur...\n")
    clientMode()
  }

  // Mode Admin pour réapprovisionnement
  def adminMode(): Unit = {
    println("Entrez le code PIN : ")
    val pin = readLine()
    if (pin == pinAdmin) {
      println("Accès autorisé. Réapprovisionnement des stocks...")
      reapprovisionner()
    } else {
      println("PIN incorrect.")
    }
  }

  // Réapprovisionnement des stocks
  def reapprovisionner(): Unit = {
    println(s"Stock actuel : Café : $stockCafe g, Sucre : $stockSucre g, Lait : $stockLait ml")
    println("Quantité de café à ajouter (en grammes) : ")
    stockCafe += readLine().toDoubleOption.getOrElse(0.0)

    println("Quantité de sucre à ajouter (en grammes) : ")
    stockSucre += readLine().toDoubleOption.getOrElse(0.0)

    println("Quantité de lait à ajouter (en millilitres) : ")
    stockLait += readLine().toDoubleOption.getOrElse(0.0)

    println(s"Nouveaux stocks : Café : $stockCafe g, Sucre : $stockSucre g, Lait : $stockLait ml")
  }
}
