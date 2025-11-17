import scala.io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Fonction pour le code alphanumérique aléatoire
    object OneTimeCode {
      def apply(length: Int = 6) = {
        Random.alphanumeric.take(length).mkString("")
      }
    }

    var choixMode: Int = 0; // 1 = mode client ; 2 = mode admin
    var choixBoisson: Int = 0; // 1 = Espresso ; 2 = Cappuccino ; 3 = Latte
    var doseSucre: Int = 0; // 0 = 0g ; 1 = 5g; 2 = 10g ; 3 = 15g
    var choixLait: Int = 0; // 1 = oui ; 2 = non
    var doseLait: Int = 0; // 1 ; 2 ; 3 (1 dose = 50 ml de lait)
    var tailleLatte: Int = 0; // 1 = petit ; 2 = moyen ; 3 = grand

    var stockCafe: Double = 50; // en grammes
    var stockLait: Double = 500; // en litres
    var stockSucre: Double = 30; // en grammes

    val PRIX_BASE_ESPRESSO: Double = 2.00;
    val PRIX_BASE_CAPPUCINO: Double = 2.50;
    val PRIX_BASE_LATTE_PETIT: Double = 2.70;
    val PRIX_BASE_LATTE_MOYEN: Double = 3.20;
    val PRIX_BASE_LATTE_GRAND: Double = 3.70;

    val PRIX_DOSE_SUCRE: Double = 0.1;
    val POIDS_DOSE_SUCRE: Double = 5; // en grammes
    val PRIX_DOSE_LAIT: Double = 0.05;
    val POIDS_DOSE_LAIT: Double = 50; // en ml

    while (true) {

      // écran de sélection de mode
      println("     Nospresso Café     ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")

      choixMode = readInt();

      // Condition pour le  choix de mode entre client (1), admin (2) et quitter (3)

      // Mode client
      if (choixMode == 1) {

        // Demande du choix de boisson à l'utilisateur
        println("Veuillez sélectionner votre boisson :")
        println("1) Espresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen); CHF 3.70 (Grand)")
        println(">")

        choixBoisson = readInt();

        if (choixBoisson == 3) {

          // Demande de la taille du Latte
          println("Quelle taille de Latte souhaiteriez-vous ?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          println(">")

          tailleLatte = readInt();

        }

        if (choixBoisson == 2 || choixBoisson == 3) {

          // Demande du choix d'une dose supplémentaire de lait
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          println(">")

          choixLait = readInt();

          if (choixLait == 1) {
            // Demande de la quantité de doses de lait ajoutées
            println("Combien de doses ?")
            println(">")

            doseLait = readInt();
          }
        }


        // Demande du choix de sucre et de la dose de sucre ajoutée
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        println(">")

        doseSucre = readInt();


        var cafeNecessaire: Double = 0
        var laitNecessaire: Double  = 0
        var sucreNecessaire = doseSucre * POIDS_DOSE_SUCRE

        if (choixBoisson == 1) {
          cafeNecessaire = 8
        } else if (choixBoisson == 2) {
          cafeNecessaire = 6;
          laitNecessaire = 100;
        } else if (choixBoisson == 3 && tailleLatte == 1) {
          cafeNecessaire = 6;
          laitNecessaire = 120;
        } else if (choixBoisson == 3 && tailleLatte == 2) {
          cafeNecessaire = 8;
          laitNecessaire = 150;
        } else if (choixBoisson == 3 && tailleLatte == 3) {
          cafeNecessaire = 12;
          laitNecessaire = 200;
        }

        var cafeSuffisant = cafeNecessaire <= stockCafe;
        var laitSuffisant = laitNecessaire <= stockLait;
        var sucreSuffisant = sucreNecessaire <= stockSucre;


        if (choixBoisson == 1) {
          println("Boisson sélectionnée : Espresso")
        } else if (choixBoisson == 2) {
          println("Boisson sélectionnée : Cappuccino")
        } else if (choixBoisson == 3 && tailleLatte == 1) {
          println("Boisson sélectionnée : Latte (Petit)")
        } else if (choixBoisson == 3 && tailleLatte == 2) {
          println("Boisson sélectionnée : Latte (Moyen)")
        } else if (choixBoisson == 3 && tailleLatte == 3) {
          println("Boisson sélectionnée : Latte (Grand)")
        }

        if (doseSucre == 1) {
          println("Niveau de sucre : Sans sucre")
        } else if (doseSucre == 2) {
          println("Niveau de sucre : Peu (5g)")
        } else if (doseSucre == 3) {
          println("Niveau de sucre : Moyen (10g)")
        } else if (doseSucre == 3) {
          println("Niveau de sucre : Beaucoup (15g)")
        }

        if (choixLait == 2) {
          println("Lait supplémentaire : Non")
        } else if (choixLait == 1 && doseLait == 1) {
          println("Lait supplémentaire : Oui (1 dose)")
        } else if (choixLait == 1 && doseLait == 2) {
          println("Lait supplémentaire : Oui (2 doses)")
        } else if (choixLait == 1 && doseLait == 3) {
          println("Lait supplémentaire : Oui (3 doses)")
        }

        var prixBase: Double = 0.0

        if (choixBoisson == 1) {
          prixBase = PRIX_BASE_ESPRESSO
        } else if (choixBoisson == 2) {
          prixBase = PRIX_BASE_CAPPUCINO
        } else if (choixBoisson == 3 && tailleLatte == 1) {
          prixBase = PRIX_BASE_LATTE_PETIT
        } else if (choixBoisson == 3 && tailleLatte == 2) {
          prixBase = PRIX_BASE_LATTE_MOYEN
        } else if (choixBoisson == 3 && tailleLatte == 3) {
          prixBase = PRIX_BASE_LATTE_GRAND
        }

        var prixLait = PRIX_DOSE_LAIT * doseLait
        var prixSucre = PRIX_DOSE_SUCRE * doseSucre

        var prixTotal = prixLait + prixSucre + prixBase

        println("Prix total : CHF " + prixBase + " + CHF " + prixSucre + " + CHF " + prixLait + " = " + " CHF " + prixTotal)
        println("")

        if(cafeSuffisant && sucreSuffisant && laitSuffisant){
          println("Veuillez payer en utilisant Twint")
          println("Votre code de paiement est : " + (OneTimeCode.apply(5)))
          println("En attente validation de paiement...")

          Thread.sleep(3000)
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")
          if (choixBoisson == 1) {
            println("Votre Espresso est prêt ! Bonne dégustation !")
          } else if (choixBoisson == 2) {
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          } else if (choixBoisson == 3) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }

          stockCafe = stockCafe - cafeNecessaire
          stockLait = stockLait - laitNecessaire
          stockSucre = stockSucre - sucreNecessaire


        } else if(!cafeSuffisant){
          println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        } else if(!sucreSuffisant){
          println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        } else if(!laitSuffisant){
          println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        println("")
      } // Mode admin
      else if (choixMode == 2) {
        println("Entrez le code PIN : ******")
        var codeAdmin = readInt();
        if(codeAdmin == 434343){
          println("Accès autorisé.")
          println("")
          println("Stocks : ")


          println("Poudre de café : " + stockCafe + "g")
          println("Lait : " + stockLait/1000 + "L")
          println("Sucre : " + stockSucre + "g")
          println("")
          println("Réapprovisionnement des stocks...")
          println("Ajout : ")

          stockCafe = 50
          stockLait = 500
          stockSucre = 30

          println("Poudre de café : " + stockCafe + "g")
          println("Lait : " + stockLait/1000 + "L")
          println("Sucre : " + stockSucre + "g")
          println("Niveaux de stock mis a jour.")
          println("Retour au menu principal...")
          println("")
          Thread.sleep(3000)

        }

      } else if (choixMode == 3) {
      }
    }
  }
}