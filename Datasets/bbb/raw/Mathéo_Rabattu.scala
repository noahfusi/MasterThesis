import io.StdIn._
import scala.util.Random
import math._

// Exercice 2
object Main {
  // Nombre total de machines
  val Machines = 5

  // Code PIN initial pour chaque machine
  val MotDePasse: Array[String] = Array.fill(Machines)("434343")

  // Stocks initiaux de poudre de café, sucre et lait pour chaque machine
  var stockPoudreCafe: Array[Int] = Array.fill(Machines)(50) // en grammes
  var stockSucre: Array[Int] = Array.fill(Machines)(30) // en grammes
  var stockLait: Array[Double] = Array.fill(Machines)(0.5) // en litres

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
      
      // Utilisation d'une instruction "case" associée à chaque choix de l'utilisateur
      readLine() match {
        case "1" => // Lorsqu'on souhaite accéder au mode client
          println("Machine sélectionnée (1-5) :") // On sélectionne la machine qu'on souhaite utiliser, qui doit être comprise entre 1 et 5
          print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
          val NumeroMachine = readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rentrer une valeur entière
          if (NumeroMachine >= 0 && NumeroMachine <= Machines) { // Permet de vérifier que la machine souhaitée se trouve bien entre 1 et 5 (on exclut 0)
            modeClient(NumeroMachine, stockPoudreCafe, stockSucre, stockLait) // On dirige l'utilisateur dans le mode Client de la machine sélectionnée, en utilisant les stocks associés
          } else {
            println("Le numéro de la machine n'existe pas.") // Dans le cas où la machine sélectionnée n'est pas comprise entre 1 et 5
          }
        case "2" => // Lorsqu'on souhaite accéder au mode admin
          println("Machine sélectionnée (1-5) :") // On sélectionne la machine qu'on souhaite utiliser, qui doit être comprise entre 1 et 5
          print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
          val NumeroMachine = readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rentrer une valeur entière
          if (NumeroMachine >= 0 && NumeroMachine <= Machines) { // Permet de vérifier que la machine souhaitée se trouve bien entre 1 et 5 (on exclut 0)
            if (validatePin(NumeroMachine, MotDePasse)) { // Si le mot de passe associé à la machine est juste, on accède au mode admin
              modeAdmin(NumeroMachine) // On dirige l'utilisateur dans le mode admin de la machine sélectionnée
            }
          } else {
            println("Le numéro de la machine n'existe pas.") // Dans le cas où la machine sélectionnée n'est pas comprise entre 1 et 5
          }
        case "3" => // Permet de quitter le programme
          poursuivre = false // Arrête immédiatement la boucle
      }
    }
  }
  // Validation du mot de passe associé à la machine
  def validatePin(NumeroMachine: Int, MotDePasse: Array[String]): Boolean = {
    var tentatives = 3 // Correspond au nombre de tentatives maximum pour saisir le bon mot de passe
    while (tentatives > 0) { // Le nombre de tentatives possible se trouve entre 1 et 3 (donc on exclut 0)
      println("Entrez le code PIN :") // L'utilisateur est amené à saisir le mot de passe pour la machine correspondante
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
      val MotDePasseSaisi = readLine() // On nomme la valeur "MotDePasseSaisi" le mot de passe qu'on souhaite valider
      if (MotDePasseSaisi == MotDePasse(NumeroMachine - 1)) { // Dans le cas où l'utilisateur saisi correctement le mot de passe qui correspond à celui associé à la machine
        println(s"Accès accordé à la machine ${NumeroMachine}.") // Message indiquant à l'utilisateur qu'il a accès à la machine sélectionnée
        return true
      } else { // Dans le cas où l'utilisateur saisi le mauvais mot de passe
        tentatives -= 1 // A chaque mauvais mot de passe, il y a une tentative de moins, jusqu'à avoir essayé les 3 tentatives possibles
        println(s"Code PIN incorrect. ${tentatives} tentatives restantes.") // Message indiquant à l'utilisateur que le mot de passe est incorrect ainsi que le nombre de tentatives restantes
      }
    } // Dans le cas où les 3 tentatives sont incorrectes
    println("Trop de tentatives échouées. Fin du programme.") // Message d'erreur indiquant la fin du programme après 3 tentatives échouées
    false
  }
  // Changement du mot de passe associé à la machine
  def updatePin(NumeroMachine: Int, MotDePasse: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${NumeroMachine}.") // Message indiquant le numéro de la machine dont on souhaite changer le mot de passe
    println("Entrez un nouveau code PIN à 6 chiffres :") // L'utilisateur est amené à saisir un nouveau mot de passe pour la machine correspondante
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
    val NouveauMotDePasse = readLine() // On nomme la valeur "NouveauMotDePasse" le nouveau mot de passe qu'on souhaite saisir à la place de l'ancien
    if (NouveauMotDePasse.matches("\\d{6}")) { // On vérifie si le nouveau mot de passe saisi contient bien 6 chiffres
      MotDePasse(NumeroMachine - 1) = NouveauMotDePasse // Changement du mot de passe dans le système
      println("Le code PIN a été mis à jour avec succès.") // Message indiquant à l'utilisateur que le mot de passe a correctement pu être modifié
    } else { // Dans le cas où le mot de passe saisi ne contient pas 6 chiffres
      println("Veuillez saisir un code PIN à 6 chiffres.") // Message d'erreur invitant l'utilisateur à resaisir un code à 6 chiffres
    }
  }
  // Mode client, permettant à l'utilisateur de commander une boisson sur une machine
  def modeClient(NumeroMachine: Int, stockPoudreCafe: Array[Int], stockSucre: Array[Int], stockLait: Array[Double]): Unit = {
    // Choix de la boisson
    println("Veuillez sélectionner votre boisson :") // L'utilisateur a le choix parmi Expresso, Cappuccino et Latte
    println("1) Expresso = 2.00 CHF")
    println("2) Cappuccino = 2.50 CHF")
    println("3) Latte = 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur

    // Utilisation d'une instruction "case" associée à chaque choix de boisson
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
          case _ =>
            println("Choix invalide.")
            return
        }
    }
    // Ajout du sucre
    println("Souhaitez-vous ajouter du sucre ?") // Choix de la quantité de sucre
    println("1) Sans sucre")
    println("2) Peu (5g) = 0.10 CHF")
    println("3) Moyen (10g) = 0.20 CHF")
    println("4) Beaucoup (15g) = 0.30 CHF")
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
    val sucre = readLine() // Utilisation d'une instruction "case" associée à chaque prix de sucre
    val prixSucre = sucre match {
      // Utilisation d'une instruction "case" associée à chaque choix de quantité de sucre
      case "1" => 0 // Prix en CHF sans sucre
      case "2" => 0.1 // Prix en CHF pour 5g de sucre
      case "3" => 0.2 // Prix en CHF pour 10g de sucre
      case "4" => 0.3 // Prix en CHF pour 15g de sucre
    }
    // Ajout du lait
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
          if (doseLait > 3) { // Si le nombre de dose de lait souhaité est supérieur à 3
            println("Erreur, vous pouvez avoir maximum 3 doses de lait en supplément.") // Affichage du message d'erreur si le nombre de doses de lait souhaité est supérieur à 3
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
    if (stockLait(NumeroMachine) < laitRequis) {
      println("Erreur : quantité de lait insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stockSucre(NumeroMachine) < sucreRequis) {
      println("Erreur : quantité de sucre insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stockPoudreCafe(NumeroMachine) < poudreCafeRequis) {
      println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    // Paiement via Twint si quantité suffisante de lait, sucre et poudre de café
    if (stockLait(NumeroMachine) < laitRequis || stockSucre(NumeroMachine) < sucreRequis || stockPoudreCafe(NumeroMachine) < poudreCafeRequis) {
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

    // Préparation de la commande si quantité suffisante de lait, sucre et poudre de café
    if (stockLait(NumeroMachine) < laitRequis || stockSucre(NumeroMachine) < sucreRequis || stockPoudreCafe(NumeroMachine) < poudreCafeRequis) {
    } else {
      // Si quantité suffisante et si le paiement Twint a été fait, préparation de la boisson
      println("Préparation de votre boisson...")
      Thread.sleep(5000) // Attend pendant 5 secondes
      println("Votre boisson est prête ! Bonne dégustation !")
    }
    // Mise à jour automatique des stocks à chaque fois qu'une quantité de poudre de café, lait ou sucre est utilisée par une machine
    stockPoudreCafe(NumeroMachine) -= poudreCafeRequis
    stockLait(NumeroMachine) -= laitRequis
    stockSucre(NumeroMachine) -= sucreRequis
  }
  // Donne les stocks actuels ainsi que la possibilité pour l'administrateur de réapprovisionner les stocks d'une machine
  def restockMachine(NumeroMachine: Int, stockPoudreCafe: Array[Int], stockSucre: Array[Int], stockLait: Array[Double]): Unit = { // Permet de gérer le réapprovisionnement des stocks de la machine sélectionnée
    println("Niveau de stock actuel :") // Affiche les stocks actuels de poudre de café, lait et sucre de la machine sélectionnée
    println(s"Poudre de café : ${stockPoudreCafe(NumeroMachine)} grammes") // Stock poudre de café en grammes
    println(s"Lait : ${stockLait(NumeroMachine)} litres") // Stock lait en litres
    println(s"Sucre : ${stockSucre(NumeroMachine)} grammes") // Stock sucre en grammes
    println("Entrez les quantités à ajouter :") // Possibilité de modifier les stocks de poudre de café, lait et sucre
    println("Poudre de café (grammes) : ")
    stockPoudreCafe(NumeroMachine) += readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière
    println("Lait (litres) : ")
    stockLait(NumeroMachine) += readLine().toDoubleOption.getOrElse(0.0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière ou décimale
    println("Sucre (grammes) : ")
    stockSucre(NumeroMachine) += readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière
    println("Les stocks ont été mis à jour avec succès.") // Message indiquant que les stocks ont correctement été mis à jour
  }
  // Mode admin, permettant aux administrateurs de réapprovisionner les ingrédients et de mettre à jour les codes PIN des machines
  def modeAdmin(NumeroMachine: Int): Unit = { // Création d'une boucle
    var poursuivre = true
    while (poursuivre) {
      // Message d'accueil du mode admin
      println("Mode Admin :") // Après avoir saisi le bon code PIN, on accède au mode admin
      println("1) Réapprovisionnement des stocks") // Donne le stock actuel de la machine ainsi que la possibilité pour l'administrateur de réapprovisionner les stocks
      println("2) Changement de code PIN") // Permet de changer le code PIN associé à la machine
      println("3) Retour au menu principal") // Retour au menu principal
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
      // Utilisation d'une instruction "case" associée à chaque choix
      readLine() match {
        case "1" => restockMachine(NumeroMachine, stockPoudreCafe, stockSucre, stockLait) // Réapprovisionnement des stocks
        case "2" => updatePin(NumeroMachine, MotDePasse) // Changement de code PIN
        case "3" => poursuivre = false // Retour au menu principal
      }
    }
  }
}