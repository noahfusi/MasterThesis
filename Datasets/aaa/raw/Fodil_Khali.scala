import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var choice = 0
    var boisson = 0
    var taillelait = 0
    var doselait = 0
    var dosesucre = 0
    var prixboisson = 0.0000
    var prixsucre = 0.0000
    var prixdoselait: Double = 0.0000
    var choixdoselait: Int = 0
    var cafe = 50
    var sucre = 30
    var lait = 0.500
    var prixtotal: Double = 0.0
    var pinentrer = 0
    var ajoutsucre = 0
    var ajoutlait = 0.0
    var ajoutcafe = 0


    while (choice != 1 && choice != 2 && choice != 3 || choice == 1 || choice == 2 || choice == 3) {
      println("\n \nVeuillez sélectionner votre mode : \n \n1) Client \n2) Admin \n3) Quitter ")
      choice = readInt()
      if (choice == 1) {
        print("Vous avez choisi le mode Client.\n")
        println("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        boisson = readInt()
        if (boisson == 1) {
          prixboisson = 2.00
          cafe = cafe - 8
        }
        if (boisson == 2) {
          prixboisson = 2.50
          cafe = cafe - 6
          lait = lait - 0.1
        }
        if (boisson == 3) {
          println(" Veuillez sélectionner la taille de votre lait : \n1) Petit \n2) Moyen \n3) Grand")
          taillelait = readInt()
        }
        if (taillelait == 1) {
          prixboisson = 2.70
          lait = lait - 0.120
          cafe = cafe - 6
        }
        if (taillelait == 2) {
          prixboisson = 3.20
          lait = lait - 0.150
          cafe = cafe - 8
        }
        if (taillelait == 3) {
          prixboisson = 3.70
          lait = lait - 0.200
          cafe = cafe - 8
        }
        println("Souhaitez-vous ajouter du sucre ? : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30")
        dosesucre = readInt()
        if (dosesucre == 2) {
          prixsucre = 0.10
          sucre = sucre - 5
        }
        if (dosesucre == 3) {
          prixsucre = 0.20
          sucre = sucre - 10
        }
        if (dosesucre == 4) {
          prixsucre = 0.30
          sucre = sucre - 15
        }

        if (boisson != 1) {
          println("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui\n2) Non")
          choixdoselait = readInt()
        }
        if (choixdoselait == 1) {
          println("Combien de dose ?")
          doselait = readInt()
          if (doselait == 1) {
            prixdoselait = 0.05
            lait = lait - 0.050
          }
          if (doselait == 2) {
            prixdoselait = 0.10
            lait = lait - 0.100
          }
          if (doselait == 3) {
            prixdoselait = 0.15
            lait = lait - 0.150
          }

        }
        prixtotal = prixboisson + prixsucre + prixdoselait
        if (boisson == 1 && dosesucre == 1) {
          print("\nBoisson séléctionnée : Expresso \nNiveau de sucre : Aucun")
        }
        if (boisson == 1 && dosesucre == 2) {
          print("\nBoisson séléctionnée : Expresso \nNiveau de sucre : Peu (5g)")
        }
        if (boisson == 1 && dosesucre == 3) {
          print("\nBoisson séléctionnée : Expresso \nNiveau de sucre : Moyen (10g)")
        }
        if (boisson == 1 && dosesucre == 4) {
          print("\nBoisson séléctionnée : Expresso \nNiveau de sucre : Beaucoup (15g)")
        }
        if (boisson == 2 && dosesucre == 1 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Aucun \nLait en supplément : Non")
        }
        if (boisson == 2 && dosesucre == 2 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Peu (5g) \nLait en supplément : Non")
        }
        if (boisson == 2 && dosesucre == 3 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Moyen (10g)\nLait en supplément : Non")
        }
        if (boisson == 2 && dosesucre == 4 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Beaucoup (15g) \nLait en supplément : Non")
        }
        if (boisson == 2 && dosesucre == 1 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Aucun \nLait en supplément : Oui")
        }
        if (boisson == 2 && dosesucre == 2 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Peu (5g) \nLait en supplément : Oui")
        }
        if (boisson == 2 && dosesucre == 3 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Moyen (10g) \nLait en supplément : Oui")
        }
        if (boisson == 2 && dosesucre == 4 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Cappuccino \nNiveau de sucre : Beaucoup (15g) \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 1 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Aucun \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 2 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Peu (5g) \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 3 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Moyen (10g) \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 4 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Beaucoup (15g) \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 1 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Aucun \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 2 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Peu (5g) \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 3 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Moyen (10g) \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 1 && dosesucre == 4 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Petit) \nNiveau de sucre : Beaucoup (15g) \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 1 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Aucun  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 2 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Peu (5g)  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 3 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Moyen (10g)  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 4 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Beaucoup (15g)  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 1 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Aucun  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 2 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Peu (5g)  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 3 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Moyen (10g)  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 2 && dosesucre == 4 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Moyen) \nNiveau de sucre : Beaucoup (15g)  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 1 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Aucun  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 2 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Peu (5g)  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 3 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Moyen (10g)  \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 4 && choixdoselait == 2) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Beaucoup (15g) \nLait en supplément : Non")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 1 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Aucun  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 2 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Peu (5g)  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 3 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Moyen (10g)  \nLait en supplément : Oui")
        }
        if (boisson == 3 && taillelait == 3 && dosesucre == 4 && choixdoselait == 1) {
          print("\nBoisson séléctionnée : Latte (Grand) \nNiveau de sucre : Beaucoup (15g)  \nLait en supplément : Oui")
        }
        // paiement confirmé
        if (boisson == 1 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 2 ||boisson == 1 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 3 ||boisson == 1 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 4 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixsucre + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString +  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 1 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 1) {
          printf("\nPrix total : CHF " + prixboisson +" = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString + "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}


        if (boisson == 2 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 1 && choixdoselait != 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 2 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre > 1 && choixdoselait != 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixsucre + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 2 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 1 && doselait >= 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixdoselait + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 2 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre > 1 && doselait >= 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixsucre + " + CHF " + prixdoselait + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}



        if (boisson == 3 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 1 && choixdoselait != 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 3 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre > 1 && choixdoselait != 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixsucre + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+ "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 3 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre == 1 && doselait >= 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixdoselait + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+ "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}

        if (boisson == 3 && cafe >= 0 && sucre >= 0 && lait >= 0.0 && dosesucre > 1 && doselait >= 1 ) {
          printf("\nPrix total : CHF " + prixboisson + " + CHF " + prixsucre + " + CHF " + prixdoselait + " = CHF %.2f \n",prixtotal)
          println("\nVeuillez payer en utilisant : Twint\nVotre code de paiement est : " +  Random.alphanumeric.filter(c => c.isDigit || c.isUpper).take(5).mkString+  "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre Latte est prêt ! Bonne Dégustation!")}




        if (boisson == 1 && cafe < 0 && sucre >= 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe >= 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe < 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe >= 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe >= 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe < 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 1 && cafe < 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }


        if (boisson == 2 && cafe < 0 && sucre >= 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe >= 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe < 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe >= 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe >= 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe < 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 2 && cafe < 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }


        if (boisson == 3 && cafe < 0 && sucre >= 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && cafe >= 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && cafe < 0 && sucre < 0 && lait >= 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && taillelait==1 && cafe >= 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && taillelait==2  && cafe >= 0 && sucre >= 0 && lait < 0.0 || boisson == 3 && taillelait==3  && cafe >= 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && cafe >= 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de lait et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && cafe < 0 && sucre >= 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if (boisson == 3 && cafe < 0 && sucre < 0 && lait < 0.0) {
          print("\n\nErreur : Quantité de poudre de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }

      }
      if (choice == 2){
        {println("Vous avez choisi le mode Admin \nEntrez le code PIN: ****** ")
          pinentrer = readInt()}
        if (pinentrer == 434343) {{
          print("Accès autorisé.\nStocks:\n   Poudre de café: " + cafe + "g\n   Lait          : " + lait + "L" + "\n   Sucre         : " + sucre + "g\n" )
          println("Réapprovisionnement des stocks...\nAjout :")
          print("   Poudre de café: ")
          ajoutcafe = readInt()
          cafe = cafe + ajoutcafe
          print("   Lait          : ")
          ajoutlait = readDouble()
          lait = lait + ajoutlait
          print("   Sucre         : ")
          ajoutsucre = readInt()
          sucre = sucre + ajoutsucre
          println("Niveaux de stock mis à jour.\nRetour au menu principal...")
          if (pinentrer != 434343) {print("\nCe n'est pas le bon code Pin.\n")}}}}

      if (choice == 3){print("Aurevoire et à bientôt.")}
      if (choice != 1 && choice != 2 && choice != 3) {print("Veuillez réessayer. \n")}
    }}}
