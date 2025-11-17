import io.StdIn._
import scala.util.Random
import math._

object Main {
  // Mot de passe pour accéder en mode Admin
  val MotDePasse = "434343"

  // Stocks initiaux de poudre de café, sucre et lait
  var stockPoudreCafe: Int = 50 // grammes
  var stockSucre: Int = 30 // grammes
  var stockLait: Double = 0.5 // litres

  def main(args: Array[String]): Unit = { // Création d'une boucle
    var poursuivre = true

    // Message d'accueil lors de l'ouverture du programme
    while (poursuivre) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :") // 3 choix sont possibles, aller en mode client, mode admin ou quitter le programme
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur

      // Utilisation d'une instruction "case" associée à chaque mode
      readLine() match {
        case "1" => modeClient() // Permet d'accéder au mode Client
        case "2" => modeAdmin() // Permet d'accéder au mode Admin
        case "3" => // Permet de quitter le programme
          poursuivre = false // Arrête immédiatement la boucle
      }
    }
  }
  // Mode client, permettant à l'utilisateur de commander une boisson
  def modeClient(): Unit = {
    println("Veuillez sélectionner votre boisson :") // L'utilisateur a le choix parmi Expresso, Cappuccino et Latte
    println("1) Expresso = 2.00 CHF")
    println("2) Cappuccino = 2.50 CHF")
    println("3) Latte = 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur

    val cafe = readLine()
    val prixInitial = cafe match {
      case "1" => 2 // Prix en CHF d'un Expresso
      case "2" => 2.5 // Prix en CHF d'un Cappuccino
      case "3" =>
        println("Choisissez la taille du Latte :") // Choix de la taille du Latte
        println("1) Petit = 2.70 CHF")
        println("2) Moyen = 3.20 CHF")
        println("3) Grand = 3.70 CHF")
        print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
        readLine() match {
          case "1" => 2.7 // Prix en CHF pour un "petit" Latte
          case "2" => 3.2 // Prix en CHF pour un "moyen" Latte
          case "3" => 3.7 // Prix en CHF pour un "grand" Latte
        }
    }
    println("Souhaitez-vous ajouter du sucre ?") // Choix de la quantité de sucre
    println("1) Sans sucre")
    println("2) Peu (5g) = 0.10 CHF")
    println("3) Moyen (10g) = 0.20 CHF")
    println("4) Beaucoup (15g) = 0.30 CHF")
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
    val sucre = readLine() // Utilisation d'une instruction "case" associée à chaque prix de sucre
    val prixSucre = sucre match {
      case "1" => 0 // Prix en CHF sans sucre
      case "2" => 0.1 // Prix en CHF pour 5g de sucre
      case "3" => 0.2 // Prix en CHF pour 10g de sucre
      case "4" => 0.3 // Prix en CHF pour 15g de sucre
    }

    var doseLait = 0
    if (cafe == "2" || cafe == "3") { // Car pas de lait en supplément pour l'Expresso "1"
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
      // Utilisation d'une instruction "case" associée à chaque choix de lait en supplément
      readLine() match {
        case "1" => // Lait en supplément
          println("Combien de doses ? (maximum 3)")
          print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
          doseLait = readLine().toIntOption.getOrElse(0) // Permet de rajouter une valeur entière
          if (doseLait > 3) { // Si le nombre de dose de lait est supérieur à 3
            println("Erreur, vous pouvez avoir maximum 3 doses de lait en supplément.") // affichage du message d'erreur
            return          }
        case "2" => // Pas de lait en supplément
      }
    }
    // Calcul du prix total en CHF
    val prixTotal = prixInitial + prixSucre + doseLait * 0.05
    // Contient le prix initial en CHF + prix du sucre supplémentaire en CHF + prix du lait supplémentaire en CHF

    // Gestion des stocks de lait
    // Utilisation d'une instruction "case" associée à chaque quantité de lait en supplément pour le Cappuccino et le Latte
    val laitRequis = cafe match { // Pas de lait pour l'Expresso "1"
      case "2" => 0.1 + doseLait * 0.05 // litres
      case "3" => // Chaque type de Latte a une certaine quantité de lait
        if (prixInitial == 2.7) 0.12 else if (prixInitial == 3.2) 0.15 else 0.2 + doseLait * 0.05 // litres
      // Petit Latte (0.12L)                 // Moyen Latte (0.15L)      // Grand Latte (0.2L) + dose supplémentaire de lait
      case _ => 0
    }

    // Gestion des stocks de sucre
    // Utilisation d'une instruction "case" associée à chaque quantité de sucre
    val sucreRequis = sucre match {
      case "2" => 5 // grammes, pour "peu" de quantité
      case "3" => 10 // grammes, pour "moyen" de quantité
      case "4" => 15 // grammes, pour "beaucoup" de quantité
      case _ => 0
    }

    // Gestion des stocks de poudre de café
    // Utilisation d'une instruction "case" associée à chaque quantité de poudre de café
    val poudreCafeRequis = cafe match {
      case "1" => 6 // grammes, pour Expresso
      case "2" => 8 // grammes, pour Cappuccino
      case "3" => 12 // grammes, pour Latte
      case _ => 0
    }

    // Problèmes liés aux stocks de lait, sucre et poudre de café
    if (stockLait < laitRequis) {
      println("Erreur : quantité de lait insuffisante pour préparer la boisson sélectionnée.") // Affichage message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stockSucre < sucreRequis) {
      println("Erreur : quantité de sucre insuffisante pour préparer la boisson sélectionnée.") // Affichage message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stockPoudreCafe < poudreCafeRequis) {
      println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.") // Affichage message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return
    }

    // Paiement via Twint si quantité suffisante de lait, sucre ou poudre de café
    if (stockLait < laitRequis || stockSucre < sucreRequis || stockPoudreCafe < poudreCafeRequis) {
    } else {
      // Si quantité suffisante, affichage du prix total à payer
      println(s"Prix total : $prixTotal CHF.")
      val codeTwint = Random.alphanumeric.take(5).mkString // Permet de composer aléatoirement un code Twint de 5 caractères alphanumériques
      println("Veuillez payer en utilisant Twint.")
      println(s"Votre code de paiement est : $codeTwint") // Affichage du code précédemment composé aléatoirement de 5 caractères
      println("(En attente de paiement...)")
      Thread.sleep(3000) // Attend pendant 3 secondes
      println("Paiement confirmé.")
    }

    // une fois le paiement validé, la machine affiche :

    // Préparation de la commande si quantité suffisante
    if (stockLait < laitRequis || stockSucre < sucreRequis || stockPoudreCafe < poudreCafeRequis) {
    } else {
      // Si quantité suffisante et si le paiement Twint a été fait, préparation de la boisson
      println("Préparation de votre boisson...")
      Thread.sleep(5000) // Attend pendant 5 secondes
      println("Votre boisson est prête ! Bonne dégustation !")
    }

    // Mise à jour automatique des stocks à chaque fois qu'une quantité de poudre de café, lait ou sucre est utilisée
    stockPoudreCafe -= poudreCafeRequis
    stockLait -= laitRequis
    stockSucre -= sucreRequis
  }

  // Mode Admin, permettant à l'administrateur de réapprovisionner les stocks de la machine
  def modeAdmin(): Unit = {
    println("Entrez le code PIN :") // Saisir le code 434343
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
    if (readLine() == MotDePasse) { // Dans le cas où le code PIN a été saisi correctement
      println("Accès autorisé.")
      println("Stocks :") // Affiche les stocks actuels
      println(s"Poudre de café : $stockPoudreCafe grammes")
      println(s"Lait : $stockLait litres")
      println(s"Sucre : $stockSucre grammes")
      println("Réapprovisionnement des stocks...") // Possibilité de modifier les stocks
      println("Ajout :")
      println("Poudre de café (grammes) : ")
      stockPoudreCafe += readLine().toIntOption.getOrElse(0) // Permet de rajouter une valeur entière
      println("Lait (litres) : ")
      stockLait += readLine().toDoubleOption.getOrElse(0.0) // Permet de rajouter une valeur entière ou décimale
      println("Sucre (grammes) : ")
      stockSucre += readLine().toIntOption.getOrElse(0) // Permet de rajouter une valeur entière
      println("Niveau de stock mis à jour.")
      println("Retour au menu principal...")
    } else { // Dans le cas où le code PIN n'a pas été saisi correctement
      println("Code incorrect, vous avez été déconnecté.")
    }
  }
}