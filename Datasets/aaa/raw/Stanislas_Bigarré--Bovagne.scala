
import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  def main(args: Array[String]): Unit = {
    // Initialisation des stocks
    var cafe: Double = 50.0 // en grammes
    var sucre: Double = 30.0 // en grammes
    var lait: Double = 0.5 // en litres

    // Fonction pour afficher le menu principal
    def afficherMenuPrincipal(): Unit = {
      println("Nospresso Cafe")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
    }

    // Fonction pour le mode client
    def modeClient(): Unit = {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      val choixBoisson = readInt()

      var prixBase = 0.0
      var typeCafe = ""

      // Sélection du type de boisson
      if (choixBoisson == 1) {
        prixBase = 2.00
        typeCafe = "Expresso"
      } else if (choixBoisson == 2) {
        prixBase = 2.50
        typeCafe = "Cappuccino"
      } else if (choixBoisson == 3) {
        prixBase = 2.70
        typeCafe = "Latte"
      } else {
        println("Choix invalide.")
        return
      }

      // Personnalisation de la boisson
      println(s"Vous avez choisi un $typeCafe.")
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      val choixSucre = readInt()
      var prixSucre = 0.0

      if (choixSucre == 1) {
        prixSucre = 0.00
      } else if (choixSucre == 2) {
        prixSucre = 0.10
      } else if (choixSucre == 3) {
        prixSucre = 0.20
      } else if (choixSucre == 4) {
        prixSucre = 0.30
      } else {
        println("Choix invalide.")
        return
      }

      // Ajouter du lait (disponible pour Cappuccino et Latte)
      var prixLait = 0.0
      if (choixBoisson == 2 || choixBoisson == 3) {
        println("Souhaitez-vous ajouter du lait en supplément ? (1) Oui (2) Non")
        print("> ")
        val choixLait = readInt()
        if (dchoixLait == 1) {
          println("Combien de doses de lait ? (1 dose = 50ml)")
          val dosesLait = readInt()
          prixLait = dosesLait * 0.05
        }
      }

      // Calcul du prix total
      val prixFinal = prixBase + prixSucre + prixLait
      println(s"Prix total : CHF $prixFinal")

      // Demander à l'utilisateur de payer
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + generateCodeTwint())
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")

      // Vérifier les stocks et préparer la boisson
      if (verifierStocks(choixBoisson, choixSucre, dosesLait)) {
        println(s"Préparation de votre $typeCafe...")
        Thread.sleep(5000)
        println(s"Votre $typeCafe est prêt ! Bonne dégustation !")
        mettreAJourStocks(choixBoisson, choixSucre, dosesLait)
      } else {
        println("Erreur : Stock insuffisant pour préparer la boisson.")
      }
    }

    // Fonction pour générer un code Twint aléatoire
    def generateCodeTwint(): String = {
      val code = java.util.UUID.randomUUID().toString.substring(0, 5).toUpperCase
      code
    }

    // Vérifier les stocks avant de préparer la boisson
    def verifierStocks(choixBoisson: Int, sucreChoisi: Int, dosesLait: Int): Boolean = {
      if (choixBoisson == 1 && cafe >= 8 && sucre >= sucreChoisi && lait >= 0) {
        true
      } else if (choixBoisson == 2 && cafe >= 6 && sucre >= sucreChoisi && lait >= 0.1 * dosesLait) {
        true
      } else if (choixBoisson == 3 && cafe >= 6 && sucre >= sucreChoisi && lait >= 0.12 * dosesLait) {
        true
      } else {
        false
      }
    }

    // Mettre à jour les stocks après la commande
    def mettreAJourStocks(choixBoisson: Int, sucreChoisi: Int, dosesLait: Int): Unit = {
      if (choixBoisson == 1) {
        cafe -= 8
      } else if (choixBoisson == 2) {
        cafe -= 6
        lait -= 0.1 * dosesLait
      } else if (choixBoisson == 3) {
        cafe -= 6
        lait -= 0.12 * dosesLait
      }
      sucre -= sucreChoisi
    }

    // Mode Admin pour réapprovisionner les stocks
    def modeAdmin(): Unit = {
      println("Entrez le code PIN : ")
      val pin = readInt()
      if (pin == 434343) {
        println("Accès autorisé.")
        println(s"Stocks actuels: Cafe: $cafe g, Sucre: $sucre g, Lait: $lait L")
        println("Réapprovisionner les stocks...")
        println("Combien de café ? (en g)")
        val cafeAjoute = readDouble()
        println("Combien de lait ? (en L)")
        val laitAjoute = readDouble()
        println("Combien de sucre ? (en g)")
        val sucreAjoute = readDouble()

        cafe += cafeAjoute
        lait += laitAjoute
        sucre += sucreAjoute
        println("Stocks mis à jour.")
      } else {
        println("Code PIN incorrect.")
      }
    }

    // Boucle principale pour gérer le menu
    var continue = true
    while (continue) {
      afficherMenuPrincipal()
      val choix = readInt()

      if (choix == 1) {
        modeClient()
      } else if (choix == 2) {
        modeAdmin()
      } else if (choix == 3) {
        println("Merci d'avoir utilisé Nospresso.")
        continue = false
      } else {
        println("Choix invalide, essayez à nouveau.")
      }
    }
  }
}
