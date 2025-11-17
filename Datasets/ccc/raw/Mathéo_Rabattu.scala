import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.io.Source
import io.StdIn._
import math._

// Exercice 3

// Création de la classe "Machine" avec 4 variables (Mot de passe, stock lait, stock sucre et stock poudre de café)
case class Machine(val id: Int, var MotDePasse: String, var Lait: Double, var Sucre: Int, var PoudreCafe: Int) // Format donné dans l'énoncé

object gestionMachine { // Permet de gérer les machines présentes dans le fichier excel "machines.csv"
  val machines: ArrayBuffer[Machine] = ArrayBuffer() // On commence par initialiser l'ArrayBuffer en regardant le nombre de machines présentes dans le fichier
  // Le nombre de machines dépend du nombre de lignes présent dans le fichier
  // Permet de lire et de charger les données à partir du fichier excel "machines.csv"
  def loadcsv(nomFichier: String): ArrayBuffer[Machine] = { // Format donné dans l'énoncé
    val file = new File(nomFichier) // Permet de créer un fichier à partir du nom indiqué
    if (!file.exists()) { // Vérification dans le cas où le fichier n'existe pas avec le nom indiqué
      println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.") // Affichage d'un message indiquant à l'utilisateur que le nom du fichier n'existe pas
      return machines // Retour au menu principal
    }
    val sourceFichier = Source.fromFile(nomFichier) // Permet de récupérer les lignes du fichier excel "machines.csv"
    try {
      val lines = sourceFichier.getLines()
      if(!lines.isEmpty)lines.next() // Permet d'ignorer la première ligne qui représente l'en-tête
      var id = 1 // On commence par l'index 1 qui représente la première machine
      for (line <- lines) {
        val colonne = line.split(",") // Permet de diviser chaque ligne en plusieurs colonnes
        val MotDePasse = colonne(0) // On attribue la première colonne comme correspondante à la valeur du mot de passe
        val stockLait = colonne(1).toDouble // On attribue la deuxième colonne comme correspondante au stock du lait
        val stockSucre = colonne(2).toInt // On attribue la troisième colonne comme correspondante au stock du sucre
        val stockPoudreCafe = colonne(3).toInt // On attribue la quatrième colonne comme correspondante au stock de poudre de café
        machines += Machine(id, MotDePasse, stockLait, stockSucre, stockPoudreCafe) // On ajoute à chaque fois une nouvelle machine après avoir saisi les données de chaque ligne du tableau excel
        id += 1 // On continue le même processus jusqu'au nombre total de machines
      }
      sourceFichier.close() // Fermeture du fichier excel après avoir récupéré les données nécessaires
    }
    machines // On obtient désormais la liste complète des informations concernant les machines provenant du fichier excel
  }

  // Permet de sauvegarder les modifications apportées aux machines dans le fichier excel "machines.csv"
  def savecsv(nomFichier: String, machines: ArrayBuffer[Machine]): Unit = { // Format donné dans l'énoncé
    var poursuivre = true
    val file = new File(nomFichier) // Permet de créer un fichier à partir du nom initial
    val bw = new BufferedWriter(new FileWriter(file)) // Permet de gérer l'écriture
    try { // On définit ce qu'on souhaite écrire dans le fichier excel
      bw.write("PINCODE,MILK,SUGAR,COFFEE") // On écrit l'en-tête sur la première ligne (comme écrit dans l'énoncé)
      bw.newLine() // Ajout d'un saut de ligne
      for (machine <- machines) { // Permet de parcourir chaque machine
        bw.write(s"${machine.MotDePasse},${machine.Lait},${machine.Sucre},${machine.PoudreCafe}") // Permet d'inscrire les valeurs de chaque machine
        // On récupère les informations du mot de passe, du stock de lait, du stock de sucre et du stock de poudre à café
        bw.newLine() // Ajout d'un saut de ligne après chaque machine
      }
      bw.close() // Permet de fermer l'écriture du fichier lorsque les données ont été correctement saisi
    } catch { // Gère les exceptions
      case _: java.io.IOException => // Dans le cas où il y a une erreur lors de la sauvegarde du fichier excel "machines.csv"
        println("Erreur : Échec de l’écriture dans machines.csv") // Message d'erreur indiquant l'échec de l'écriture dans le fichier
        println("Le fichier peut être verrouillé ou en lecture seule.") // Donne les possibles raisons de l'erreur de la sauvegarde du fichier
        poursuivre = false // Arrête immédiatement le programme
    }
  }
}
object Main {
  def main(args: Array[String]): Unit = { // Création d'une boucle
    println("Chargement des machines depuis machines.csv...") // Affichage du texte indiquant que le programme charge les données du fichier excel "machines.csv"
    gestionMachine.loadcsv("machines.csv") // On charge les données présentes dans le fichier excel "machines.csv"
    Thread.sleep(2000) // Attend pendant 2 secondes
    // Affichage des données pour chaque machine comme demandé dans l'énoncé
    for ((machine, index) <- gestionMachine.machines.zipWithIndex) { // Permet d'afficher les informations de l'ensemble des machines présentes dans le fichier excel
      println(s"\nMachine ${index + 1} chargée :") // On va à la ligne après chaque nouvelle machine
      println(s" ID: ${machine.id}") // Indique le numéro de la machine
      println(s" Code PIN: ${machine.MotDePasse}") // Indique le mot de passe de la machine
      println(s" Lait: ${machine.Lait / 1000} litres") // Indique le stock de lait de la machine en litre (on divise par 1000 pour transformer les millilitres en litres)
      println(s" Sucre: ${machine.Sucre} grammes") // Indique le stock de sucre de la machine en gramme
      println(s" Café: ${machine.PoudreCafe} grammes") // Indique le stock de poudre de café de la machine en gramme
    }
    println(s"\n${gestionMachine.machines.length} machine(s) chargée(s) avec succès.") // Affichage du nombre de machines chargées avec succès dans le programme

    var poursuivre = true
    // Après avoir chargé les données dans le fichier excel, on arrive à la page d'accueil du programme
    while (poursuivre) {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode :") // 3 choix sont possibles, aller en mode client, mode admin ou quitter le programme
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur

      // Utilisation d'une instruction "case" associée à chaque choix de l'utilisateur
      readLine() match {
        case "1" => // Lorsqu'on souhaite accéder au mode client
          println("Machine sélectionnée :") // On sélectionne la machine qu'on souhaite utiliser
          print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
          val numeroMachine = readLine().toIntOption.getOrElse(0)  // Utilisation de la méthode getOrElse qui permet de rentrer une valeur entière
          if (numeroMachine > 0 && numeroMachine <= gestionMachine.machines.length) { // Permet de vérifier que la machine souhaitée existe
            val machine = gestionMachine.machines(numeroMachine - 1)
            modeClient(machine) // On dirige l'utilisateur dans le mode Client de la machine sélectionnée, en utilisant les stocks associés
          } else {
            println("Le numéro de la machine n'existe pas.") // Message d'erreur indiquant que le numéro de la machine sélectionnée n'existe pas
          }
        case "2" => // Lorsqu'on souhaite accéder au mode admin
          println("Machine sélectionnée :") // On sélectionne la machine qu'on souhaite utiliser
          print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
          val numeroMachine = readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rentrer une valeur entière
          if (numeroMachine > 0 && numeroMachine <= gestionMachine.machines.length) { // Permet de vérifier que la machine souhaitée existe
            val machine = gestionMachine.machines(numeroMachine - 1)
            if (validatePin(machine)) { // Si le mot de passe associé à la machine est juste, on accède au mode admin
              modeAdmin(machine) // On dirige l'utilisateur dans le mode admin de la machine sélectionnée
            }
          } else {
            println("Le numéro de la machine n'existe pas.") /// Dans le cas où le numéro de la machine sélectionnée n'existe pas
          }
        case "3" => // Permet de quitter le programme en sauvegardant les modifications apportées aux machines dans le fichier excel "machines.csv"
          println(s"Sauvegarde de ${gestionMachine.machines.length} machines dans machines.csv...") // Message indiquant la sauvegarde des machines dans le fichier excel
          gestionMachine.savecsv("machines.csv", gestionMachine.machines) // Enregistrement du fichier excel "machines.csv"
          Thread.sleep(2000) // Attend pendant 2 secondes
          println("Fichier sauvegardé avec succès.") // Message indiquant que les données ont bien été sauvegardées
          poursuivre = false // Arrête immédiatement la boucle
      }
    }
  }

  // Validation du mot de passe associé à la machine
  def validatePin(machine: Machine): Boolean = {
    var tentatives = 3 // Correspond au nombre de tentatives maximum pour saisir le bon mot de passe
    while (tentatives > 0) { // Le nombre de tentatives possible se trouve entre 1 et 3 (donc on exclut 0)
      println("Entrez le code PIN :") // L'utilisateur est amené à saisir le mot de passe pour la machine correspondante
      print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
      val MotDePasseSaisi = readLine()
      if (MotDePasseSaisi == machine.MotDePasse) { // Dans le cas où l'utilisateur saisi correctement le mot de passe qui correspond à celui associé à la machine
        println(s"Accès accordé à la machine ${machine}.") // Message indiquant à l'utilisateur qu'il a accès à la machine sélectionnée
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
  def updatePin(machine: Machine): Unit = {
    println(s"Mise à jour du code PIN pour la machine ${machine}.") // Message indiquant le numéro de la machine dont on souhaite changer le mot de passe
    println("Entrez un nouveau code PIN à 6 chiffres :") // L'utilisateur est amené à saisir un nouveau mot de passe pour la machine correspondante
    print(">") // Le caractère > représente ici l’entrée clavier de l’utilisateur
    val NouveauMotDePasse = readLine()
    if (NouveauMotDePasse.matches("\\d{6}")) { // Le mot de passe doit comporter 6 chiffres
      machine.MotDePasse = NouveauMotDePasse // Changement du mot de passe dans le système
      println("Le code PIN a été mis à jour avec succès.") // Message indiquant à l'utilisateur que le mot de passe a correctement pu être modifié
    } else { // Dans le cas où le mot de passe saisi ne contient pas 6 chiffres
      println("Veuillez saisir un code PIN à 6 chiffres.") // Message d'erreur invitant l'utilisateur à ressaisir un code à 6 chiffres
    }
  }

  // Mode client, permettant à l'utilisateur de commander une boisson sur une machine
  def modeClient(machine: Machine): Unit = {
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
            return
          }
        case "2" => // Pas de lait en supplément
      }
    }
    // Calcul du prix total en CHF
    val prixTotal = prixInitial + prixSucre + doseLait * 0.05
    // Contient le prix initial en CHF + prix du sucre supplémentaire en CHF + prix du lait supplémentaire en CHF

    // Gestion des stocks de lait
    // Utilisation d'une instruction "case" associée à chaque quantité de lait en supplément pour le Cappuccino et le Latte
    val laitRequis = cafe match { // Pas de lait pour l'Expresso "1"
      case "2" => 0.1 + doseLait * 0.05 // litre
      case "3" => // Chaque type de Latte a une certaine quantité de lait
        if (prixInitial == 2.7) 0.12 else if (prixInitial == 3.2) 0.15 else 0.2 + doseLait * 0.05 // litre
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
    if (machine.Lait < laitRequis) {
      println("Erreur : quantité de lait insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (machine.Sucre < sucreRequis) {
      println("Erreur : quantité de sucre insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (machine.PoudreCafe < poudreCafeRequis) {
      println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.") // Affichage du message d'erreur
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    // Paiement via Twint si quantité suffisante de lait, sucre et poudre de café
    if (machine.Lait < laitRequis || machine.Sucre < sucreRequis || machine.PoudreCafe < poudreCafeRequis) {
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
    if (machine.Lait < laitRequis || machine.Sucre < sucreRequis || machine.PoudreCafe < poudreCafeRequis) {
    } else {
      // Si quantité suffisante et si le paiement Twint a été fait, préparation de la boisson
      println("Préparation de votre boisson...")
      Thread.sleep(5000) // Attend pendant 5 secondes
      println("Votre boisson est prête ! Bonne dégustation !")
    }
    // Mise à jour automatique des stocks à chaque fois qu'une quantité de poudre de café, lait ou sucre est utilisée par une machine
    machine.PoudreCafe -= poudreCafeRequis
    machine.Lait -= laitRequis
    machine.Sucre -= sucreRequis
  }

  // Donne les stocks actuels ainsi que la possibilité pour l'administrateur de réapprovisionner les stocks d'une machine
  def restockMachine(machine: Machine): Unit = { // Permet de gérer le réapprovisionnement des stocks de la machine sélectionnée
    println("Niveau de stock actuel :") // Affiche les stocks actuels de poudre de café, lait et sucre de la machine sélectionnée
    println(s"Poudre de café : ${machine.PoudreCafe} grammes") // Stock poudre de café en gramme
    println(s"Lait : ${machine.Lait} litres") // Stock lait en litre
    println(s"Sucre : ${machine.Sucre} grammes") // Stock sucre en gramme
    println("Entrez les quantités à ajouter :") // Possibilité de modifier les stocks de poudre de café, lait et sucre
    println("Poudre de café (grammes) : ")
    machine.PoudreCafe += readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière
    println("Lait (litres) : ")
    machine.Lait += readLine().toDoubleOption.getOrElse(0.0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière ou décimale
    println("Sucre (grammes) : ")
    machine.Sucre += readLine().toIntOption.getOrElse(0) // Utilisation de la méthode getOrElse qui permet de rajouter une valeur entière
    println("Les stocks ont été mis à jour avec succès.") // Message indiquant que les stocks ont correctement été mis à jour
  }

  // Mode admin, permettant aux administrateurs de réapprovisionner les ingrédients et de mettre à jour les codes PIN des machines
  def modeAdmin(machine: Machine): Unit = { // Création d'une boucle
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
        case "1" => restockMachine(machine) // Réapprovisionnement des stocks
        case "2" => updatePin(machine) // Changement de code PIN
        case "3" => poursuivre = false // Retour au menu principal
      }
    }
  }
}