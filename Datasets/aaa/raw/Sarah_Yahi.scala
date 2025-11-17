import io.StdIn._
import math._

object Main{

  // Variables pour les stocks
  var poudreCafe: Int = 50 // Stock initial de café en gramme
  var sucre: Int = 30 // Stock initial de sucre en gramme
  var lait: Double = 0.5 // Stock initial de lait en litre

  // Code PIN pour l'accès au mode Admin
  val pinAdmin = 434343

  // Programme principal
  def main(args: Array[String]): Unit = {
    var continuer = true // Variable pour maintenir la boucle principale

    while (continuer) {
      // Affichage du menu principal
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")



      // Lecture des choix
      val choix = lireEntreeNumerique()

      // Navigation entre les modes
      if (choix == 1) {
        modeClient() // Accès au mode client
      } else if (choix == 2) {
        modeAdmin() // Accès au mode admin
      } else if (choix == 3) {
        println("Au revoir !") // Message de sortie
        continuer = false // Quitter la boucle principale
      } else {
        println("Choix invalide, veuillez réessayer.") // Gestion des entrée invalides
      }
    }
  }

  // Fonction pour lire une entrée numérique
  def lireEntreeNumerique(): Int = {
    var estValide = false // Indique si l'entrée est valide
    println(">")
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
    nombre //le nombre validé
  }

  // Mode Client
  def modeClient(): Unit = {
    var choixValide = false
    var choixBoisson = 0

    // Demande de choix de la boisson
    while (!choixValide) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")


      choixBoisson = lireEntreeNumerique()

      // verfifier si la demande es valide
      if (choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3) {
        choixValide = true
      } else {
        println("Choix invalide, veuillez réessayer.")
      }
    }

    var cafeNecessaire = 0
    var laitNecessaire = 0
    var prixBase = 0.0
    var prixtaille = 0.0


    // Définir les quantités selon le choix

    //Expresso
    if (choixBoisson == 1) {
      cafeNecessaire = 8
      prixBase = 2.0

      // Capuccino
    } else if (choixBoisson == 2) {
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.5

      //Latte
    } else if (choixBoisson == 3) {
      var tailleValide = false
      var taille = 0
      prixBase = 2.7

      //taille pour le latte
      while (!tailleValide) {
        println("Choisissez la taille :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")


        taille = lireEntreeNumerique()

        // verfie que la taille selectionner est valide
        if (taille == 1 || taille == 2 || taille == 3) {
          tailleValide = true
        } else {
          println("Choix invalide, veuillez réessayer.")
        }
      }

      // cafe et lait necessaire pour differente taille de latte + leur prix

      //petit
      if (taille == 1) {
        cafeNecessaire = 6
        laitNecessaire = 120
        prixtaille = 0 // ajout au prix de base de 2.7 CHF

       //moyen
      } else if (taille == 2) {
        cafeNecessaire = 8
        laitNecessaire = 150
        prixtaille = 0.5 // + 2.7 CHF

        //grand
      } else if (taille == 3) {
        cafeNecessaire = 12
        laitNecessaire = 200
        prixtaille = 1 // + 2.7 CHF
      }
    }

    // Ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre - CHF 0.00")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")


    var sucreNecessaire = 0
    var prixSucre = 0.0
    var choixSucre = lireEntreeNumerique()

    // peu de sucre
    if (choixSucre == 2) {
      sucreNecessaire = 5
      prixSucre = 0.10

      // moyen
    } else if (choixSucre == 3) {
      sucreNecessaire = 10
      prixSucre = 0.20

      // beaucoup
    } else if (choixSucre == 4) {
      sucreNecessaire = 15
      prixSucre = 0.30
    }

    // Ajouter de doses de lait (uniquement pour Cappuccino et Latte)
    var laitSupplementaire = 0
    var prixLait = 0.0

    // les boissons 2 et 3 correspondent au Cappucino et Latte
    if (choixBoisson == 2 || choixBoisson == 3) {

      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")


      var choixLait = 0
      var laitValide = false

      while (!laitValide) {
        choixLait = lireEntreeNumerique()
        if (choixLait == 1 || choixLait == 2) {
          laitValide = true
        } else {
          println("Choix invalide, veuillez réessayer.")
        }
      }

      // 1) l'utilisateur souhaite du lait en suplément
      if (choixLait == 1) {
        println("Combien de doses ? (Maximum 3) - CHF 0.05 par doses")


        var doses = 0
        var dosesValides = false

        // verifier si les doses sont valide (un entier entre 0 et 3)
        while (!dosesValides) {
          doses = lireEntreeNumerique()
          if (doses >= 0 && doses <= 3) {
            dosesValides = true
          } else {
            println("Erreur : Veuillez entrer un nombre entre 0 et 3.")
          }
        }

        laitSupplementaire = doses * 50 // chaque doses correspond à 50 ml
        prixLait = doses * 0.05 // prix de 0.05 CHF par doses

        //si l'utilisateur ne veut pas de lait supplementaire (2) on lui affiche directement le prix final

      }
    }

    // Vérification des stocks
    if (poudreCafe < cafeNecessaire || lait < (laitNecessaire + laitSupplementaire) / 1000.0 || sucre < sucreNecessaire) {
      println("Stock insuffisant pour préparer la boisson.")

      // si les stocks sont suffisant continuer ...
    } else {
      // Mise à jour des stocks
      poudreCafe -= cafeNecessaire
      lait -= (laitNecessaire + laitSupplementaire) / 1000.0
      sucre -= sucreNecessaire

      // Calcul prix final
      val prixFinal = prixSucre + prixLait + prixBase + prixtaille

      // Affichage du prix
      printf("Le prix total est : CHF %.2f\n", prixFinal)

      // Affichage code Twint avec pause réaliste
      println("Veuillez payer avec Twint. \n votre cpde de payement est " + genererCodeTwint())
      println("En attente de validation du paiement...")
      Thread.sleep(3000) // Pause de 3 secondes pour simuler le paiement
      println("Paiement confirmé.\nPréparation de votre boisson... \n [...]")

      println("Votre Boisson est prête ! Bonne dégustation.")
    }
  }

    // Génération d'un code Twint alpanumérique pour payement
  def genererCodeTwint(): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    //1er caractere
    val randomIndex1 = (math.random * caracteres.length).toInt
    val caractere1 = caracteres(randomIndex1)

    //2eme caractere
    val randomIndex2 = (math.random * caracteres.length).toInt
    val caractere2 = caracteres(randomIndex2)

    //3eme caractere
    val randomIndex3 = (math.random * caracteres.length).toInt
    val caractere3 = caracteres(randomIndex3)

    //4eme caractere
    val randomIndex4 = (math.random * caracteres.length).toInt
    val caractere4 = caracteres(randomIndex4)

    //5eme caractere
    val randomIndex5 = (math.random * caracteres.length).toInt
    val caractere5 = caracteres(randomIndex5)

    // Assemblage de tout les caracteres alphanumérique
    val code = caractere1.toString + caractere2.toString + caractere3.toString + caractere4.toString + caractere5.toString

    code
  }


  // Mode Admin
  def modeAdmin(): Unit = {
    println("Entrez le code PIN :") //434343

    val pin = lireEntreeNumerique()
    if (pin == pinAdmin) {

      println("Accès autorisé.")

      // Affichage du stock actuel
      println("Stocks actuels :")
      println("1) Café : " + poudreCafe + " g")
      println("2) Sucre : " + sucre + " g")
      println("3) Lait : " + lait + " L")

      //l'administrateur reaprovise les stocks manuellement
      println("Réapprovisionnement des stocks...")
      println("Ajout de café (en g) :")
      poudreCafe += lireEntreeNumerique()

      println("Ajout de sucre (en g) :")
      sucre += lireEntreeNumerique()

      println("Ajout de lait (en ml) :")
      lait += lireEntreeNumerique() / 1000.0 //convertir le lait en litre

      // affichage du stock mis a jour
      println(s"Stocks mis à jour - Café : $poudreCafe g, Sucre : $sucre g, Lait : $lait L")

    } else {
      // si le code est incorrect
      println("Code PIN incorrect.")
    }
  }
}