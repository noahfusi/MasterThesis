import scala.io.StdIn.readLine
import scala.util.Random


object Main {
  def main(args: Array[String]): Unit = {
    var continuer = true

    while(continuer) {
      println("Sélectionnez le mode:")
      println("1. Mode client")
      println("2. Mode Admin")
      println("3. Quitter")

      val choix = readLine("Entrez votre choix (1-3):").toInt

      if(choix == 1) {
        modeClient() // Mode client
      } else if (choix == 2) {
        modeAdmin() // Mode admin
      } else if (choix == 3) {
        println("Au revoir!")
        continuer = false
      } else {
        println("Choix invalide, veuillez réessayer.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Choisissez votre boisson:")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (petit), CHF 3.20 (moyen), CHF 3.70 (grand)")

    val choixBoisson = readLine().toInt
    var nomBoisson = ""

    if (choixBoisson == 1) {
      nomBoisson = "Expresso"
    } else if (choixBoisson == 2) {
      nomBoisson = "Cappuccino"
    } else if (choixBoisson == 3) {
      nomBoisson = "Latte"
    }

    println("Vous avez choisi : " + nomBoisson)

    // Choix de la quantité de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Pas de sucre")
    println("2) Peu de sucre (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup de sucre (15g) - CHF 0.30")

    val choixSucre = readLine().toInt
    var prixSucre = 0.0

    if (choixSucre == 1) {
      prixSucre = 0.0
    } else if (choixSucre == 2) {
      prixSucre = 0.10
    } else if (choixSucre == 3) {
      prixSucre = 0.20
    } else if (choixSucre == 4) {
      prixSucre = 0.30
    }

    var prixLait = 0.0
    if (nomBoisson == "Latte" || nomBoisson == "Cappuccino") {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      val choixLait = readLine().toInt

      if (choixLait == 1) {
        println("Combien de doses de lait voulez-vous ajouter ? (MAX 3 doses)")
        val doseDeLait = readLine().toInt

        if (doseDeLait < 1 || doseDeLait > 3) {
          println("La dose de lait doit être comprise entre 1 et 3.")
        } else {
          prixLait = doseDeLait * 0.05
        }
      }
    }

    // Calcul du prix final
    var prixBoisson = 0.0
    if (nomBoisson == "Expresso") {
      prixBoisson = 2.00
    } else if (nomBoisson == "Cappuccino") {
      prixBoisson = 2.50
    } else if (nomBoisson == "Latte") {
      println("Choisissez la taille de votre Latte:")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")

      val tailleLatte = readLine().toInt
      if (tailleLatte == 1) {
        prixBoisson = 2.70
      } else if (tailleLatte == 2) {
        prixBoisson = 3.20
      } else if (tailleLatte == 3) {
        prixBoisson = 3.70
      } else {
        println("Choix de taille invalide.")
      }
    }

    // Total à payer
    val total = prixBoisson + prixSucre + prixLait
    println("Le prix total à payer est de : " + total + " CHF")

    // Paiement via Twint
    println("Veuillez effectuer le paiement via Twint...")

    val codeTwint = generateCodeTwint()
    println("Votre code de paiement : " + codeTwint)

    // Confirmation du paiement
    println("Appuyez sur Entrée pour valider le paiement...")
    readLine()
    println("Merci! Votre paiement a été accepté.")
  }

  def generateCodeTwint(): String = {
    val characters = "abcdefghijklmnopqrstuvwxyz0123456789"
    (1 to 4).map(_ => characters(scala.util.Random.nextInt(characters.length))).mkString
  }

  def modeAdmin(): Unit = {
    // Gestion des stocks
    var stockCafe = 50.0 // en grammes
    var stockLait = 0.500 // en litres
    var stockSucre = 30.0 // en grammes

    // Demande code PIN
    val pinCode = "434343"
    val saisiePin = readLine("Entrez le code PIN: ")

    if (saisiePin == pinCode) {
      println("Affichage du niveau des stocks : Café - " + stockCafe + " g, Sucre - " + stockSucre + " g, Lait - " + stockLait + " L")

      // Ingrédients à ajouter
      println("Que souhaitez-vous ajouter ?")
      println("1) Café")
      println("2) Sucre")
      println("3) Lait")
      println("4) Quitter")

      val choixAjout = readLine().toInt

      if (choixAjout == 1) {
        println("Combien de grammes de café souhaitez-vous ajouter ?")
        val ajoutCafe = readLine().toDouble
        stockCafe += ajoutCafe
      } else if (choixAjout == 2) {
        println("Combien de grammes de sucre souhaitez-vous ajouter ?")
        val ajoutSucre = readLine().toDouble
        stockSucre += ajoutSucre
      } else if (choixAjout == 3) {
        println("Combien de litres de lait souhaitez-vous ajouter ?")
        val ajoutLait = readLine().toDouble
        stockLait += ajoutLait
      } else if (choixAjout == 4) {
        println("Retour au menu principal.")
      } else {
        println("Choix non valide.")
      }

      // Affichage des stocks après modification
      println("Stocks mis à jour : Café - " + stockCafe + " g, Sucre - " + stockSucre + " g, Lait - " + stockLait + " L")
    } else {
      println("PIN incorrect.")
    }
  }
}