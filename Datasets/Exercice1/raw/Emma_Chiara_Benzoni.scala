import scala.io.StdIn._
import scala.util.Random

    object Main {
      def main(args: Array[String]): Unit = {
        var fin = false
        val pinAdmin = 434343
        var stockCafe = 50
        var stockSucre = 30
        var stockLait = 0.5

        while (!fin) {
          var choixMenu = readLine("\n      Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
          while(choixMenu != 1 && choixMenu != 2 && choixMenu != 3){
            println("Choix invalide. Veuillez réessayer.")
            choixMenu = readLine("\n      Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
          }

          if (choixMenu == 1) { // Mode Client
            var choixBoisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
            while(choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3){
              println("Choix invalide. Veuillez réessayer.")
              choixBoisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
            }

            var cafeUtilise = 0
            var laitUtilise = 0.0
            var sucreUtilise = 0

            var prixBase = 0.0
            var prixSucre = 0.0
            var prixLait = 0.0

            var nomBoisson = "<Inconnu>"
            var niveauSucre = "Sans sucre"
            var supplementLait = "Non"

            // Choix de la boisson
            if (choixBoisson == 1) {
              cafeUtilise = 8
              prixBase = 2.0
              nomBoisson = "Expresso"
            } else if (choixBoisson == 2) {
              cafeUtilise = 6
              laitUtilise = 0.1
              prixBase = 2.5
              nomBoisson = "Cappuccino"
            } else if (choixBoisson == 3) {
              var tailleLatte = readLine("Choisissez la taille du Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
              while(tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3){
                println("Choix invalide. Veuillez réessayer.")
                tailleLatte = readLine("Choisissez la taille du Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
              }
              if (tailleLatte == 1) {
                cafeUtilise = 6
                laitUtilise = 0.12
                prixBase = 2.7
                nomBoisson = "Latte (Petit)"
              } else if (tailleLatte == 2) {
                cafeUtilise = 8
                laitUtilise = 0.15
                prixBase = 3.2
                nomBoisson = "Latte (Moyen)"
              } else if (tailleLatte == 3) {
                cafeUtilise = 12
                laitUtilise = 0.2
                prixBase = 3.7
                nomBoisson = "Latte (Grand)"
              }
            }

            // Choix du sucre
            var choixSucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
            while(choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4){
              println("Choix invalide. Veuillez réessayer.")
              choixSucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
            }
            if (choixSucre == 2) {
              sucreUtilise = 5
              prixSucre = 0.1
              niveauSucre = "Peu (5g)"
            } else if (choixSucre == 3) {
              sucreUtilise = 10
              prixSucre = 0.2
              niveauSucre = "Moyen (10g)"
            } else if (choixSucre == 4) {
              sucreUtilise = 15
              prixSucre = 0.3
              niveauSucre = "Beaucoup (15g)"
            }

            // Choix du lait
            if(choixBoisson == 2 || choixBoisson == 3){
              var supplementLaitChoix = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n> ").toInt
              while(supplementLaitChoix != 1 && supplementLaitChoix != 2){
                println("Choix invalide. Veuillez réessayer.")
                supplementLaitChoix = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n> ").toInt
              }
              if (supplementLaitChoix == 1) {
                var supplementLaitDose = readLine("\nCombien de dose ? \n> ").toInt
                while(supplementLaitDose < 1 || supplementLaitDose > 3){
                  println("Choix invalide. Maximum 3 doses. Veuillez réessayer.")
                  supplementLaitDose = readLine("\nCombien de dose ? \n> ").toInt
                }
                laitUtilise += supplementLaitDose * 0.05
                prixLait = supplementLaitDose * 0.05
                supplementLait = f"$supplementLaitDose dose(s)"
              }
            }

            // Vériﬁcation des stocks
            if(stockCafe < cafeUtilise) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")

            if(stockLait < laitUtilise) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")

            if(stockSucre < sucreUtilise) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")

            if(stockCafe >= cafeUtilise && stockLait >= laitUtilise && stockSucre >= sucreUtilise){

              println(f"Boisson sélectionée : $nomBoisson \nNiveau de sucree : $niveauSucre \nLait supplémentaire : $supplementLait\n")
              println(f"Prix total : CHF $prixBase%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF ${prixBase + prixSucre + prixLait}%.2f")

              println("\nVeuillez payer en utilisant Twint.")
              println(f"Votre code de paiement est : ${Random.alphanumeric.take(5).mkString}")
              println("(En attente de paiement...)")

              Thread.sleep(5000)

              println("\nPaiement confirmé. \nPréparation de votre boisson...")

              stockCafe -= cafeUtilise
              stockLait -= laitUtilise
              stockSucre -= sucreUtilise

              Thread.sleep(5000)

              println(f"Votre $nomBoisson est prêt ! Bonne dégustation.")

            }

          } else if (choixMenu == 2) { // Mode Admin
            println("Mode Admin")
            val pin = readLine("Entrez le code PIN : ").toInt

            if (pin == pinAdmin) {
              println(f"\nAccès autorisé.\nStocks : \n   Café: ${stockCafe}g \n   Lait: ${stockLait}%.2fL \n   Sucre: ${stockSucre}g")

              println("\nRéapprovisionnement des stocks...\nAjout:")
              val ajoutCafe = readLine("   Poudre de café : ").toInt
              val ajoutLait = readLine("   Lait : ").toDouble
              val ajoutSucre = readLine("   Sucre : ").toInt

              stockCafe += ajoutCafe
              stockLait += ajoutLait
              stockSucre += ajoutSucre

              println("Niveaux de stock mis à jour. \nRetour au menu principal...")

            } else {
              println("Code PIN incorrect.")
            }

          } else if (choixMenu == 3) { // Quitter
            println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
            fin = true

          } else {
            println("Choix invalide. Veuillez réessayer.")
          }
        }
      }
}