import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    //On met cette variable a true pour pouvoir entrer dans la boucle while une premiere fois
    var reaffiche = true

    //Stocks sont utilisé dans les deux modes donc variab le globale
    // initialise les stocks
    var stockCafe = 50 // en g
    var stockSucre = 30 // en g
    var stockLait = 0.5 // en L

    // On a initialiser une variable reafficher pour determiner si on reaffiche  le menu de selection des modes
    while (reaffiche) {
      reaffiche = false
      // On affiche la page de sélection de mode
      var mode = 0
      while ((mode < 1) || (mode > 3)) {
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
      }

      //Cette condition s'execute si l'utilisateur a bien choisit 1
      if (mode == 1) {
        // On a initialiser cette variable booleen a faux pour pouvoir rentrer une premiere fois dans la boucle while
        var sansErreur = false
        while (!sansErreur) {
          sansErreur = true

          // On creer des variables pour avoir des compteurs de quantite d'ingredients
          var cafeNecess = 0
          var sucreNecess = 0
          var laitNecess = 0.0

          // Afficher la page de choix de boissons
          var boisson = 0
          while ((boisson < 1) || (boisson > 3)) {
            println("Veuillez sélectionner votre boisson :")
            println("1) Expresso - CHF 2.00")
            println("2) Cappuccino - CHF 2.50")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20(Moyen), CHF 3.70 (Grand)")
            boisson = readLine(">").toInt
          }

          // Deux variable pour stocker le nom et le prix de boisson
          var boissonNom = ""
          var boissonPrix = 0.00

          // Sachant le choix de boisson de l'utilisateur on affecte au variable les donnees necessaire
          if (boisson == 1) {
            boissonNom = "Expresso"
            boissonPrix = 2.00
            cafeNecess = 8
          }
          else if (boisson == 2) {
            boissonNom = "Cappuccino"
            boissonPrix = 2.50
            cafeNecess = 6
            laitNecess = 0.100
          }
          else if (boisson == 3) {
            // Puisque le latte est le seule a avoir des tailles on lui creer une page de selection de taille
            // Ainsi on lui creer une variable pour memoriser la taille
            var latteTaille = 0
            while ((latteTaille < 1) || (latteTaille > 3)) {
              println("Sélectionnez la taille de votre latte:")
              println("1) CHF 2.70 (Petit)")
              println("2) CHF 3.20 (Moyen)")
              println("3) CHF 3.70 (Grand)")
              latteTaille = readLine(">").toInt
            }

            // Sachant la taille du Latte entree par l'utilisateur, on affecte au variable tout les donnees necessaire
            if (latteTaille == 1) {
              boissonNom = "Latte (Small)"
              boissonPrix = 2.70
              cafeNecess = 6
              laitNecess = 0.120
            }
            else if (latteTaille == 2) {
              boissonNom = "Latte (Medium)"
              boissonPrix = 3.20
              cafeNecess = 8
              laitNecess = 0.150
            }
            else if (latteTaille == 3) {
              boissonNom = "Latte (Large)"
              boissonPrix = 3.70
              cafeNecess = 12
              laitNecess = 0.200
            }
          }
          // Page pour le choix de niveaux sucre
          var niveauSucre = 0
          while ((niveauSucre < 1) || (niveauSucre > 4)) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            niveauSucre = readLine(">").toInt
          }
          // pareil qu'avant grace au deux nouveaux variable on pourras stocker les donnes voulu concernant le niveaux de sucre
          var sucreNom = ""
          var sucrePrix = 0.00
          //Sachant le niveaux de sucre choisi, on affecte les donnes necessaire
          if (niveauSucre == 1) {
            sucreNom = "Sans sucre"
            sucrePrix = 0
            sucreNecess = 0
          }
          else if (niveauSucre == 2) {
            sucreNom = "Peu (5g)"
            sucrePrix = 0.10
            sucreNecess = 5
          }
          else if (niveauSucre == 3) {
            sucreNom = "Moyen (10g)"
            sucrePrix = 0.20
            sucreNecess = 10
          }
          else if (niveauSucre == 4) {
            sucreNom = "Beaucoup (15g)"
            sucrePrix = 0.30
            sucreNecess = 15
          }

          var laitSupp = 0
          var dose = 0

          // Grace aux deux nouveaux variables on pourra stocker les donnees voulu concernant l'ajout de lait sup
          var laitNom = ""
          var laitPrix = 0.0
          // Cette condition verifie que l'ajout supp de lait est que possible pour le cappuccino et le latte
          if ((boisson == 2) || (boisson == 3)) {

            while ((laitSupp < 1) || (laitSupp > 2)) {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("(Disponible uniquement pour Cappuccino et Latte)")
              println("1) Oui")
              println("2) Non")
              laitSupp = readLine(">").toInt
            }
            // Si on choisit d'ajouter deu lait
            if (laitSupp == 1) {
              laitNom = "Oui"
              // cette condition s'assure que l'utilisateur ajoute au moins 1 dose et 3 doses maximale par boisson sinon boucle
              // (J'utilise la negation de ce propos pour pouvoir ecrire une condition moins longue)
              while ((dose <= 0) || (dose > 3)) {
                println("Combien de dose ?")
                dose = readLine(">").toInt
              }
              laitPrix = dose * 0.05
              laitNecess = laitNecess + (dose * 0.05)
            }
            else if (laitSupp == 2) {
              laitNom = "Non"
            }
          }

          // Gestions des erreurs possibles pour les boissons 1 et 2
          //(On verifie les memes trucs meme si l'expresso utilise que de la poudre de cafe)
          if ((boisson == 1) || (boisson == 2)) {
            if (laitNecess > stockLait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              sansErreur = false
            }
            if (cafeNecess > stockCafe) {
              println(" Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              sansErreur = false
            }
            if (sucreNecess > stockSucre) {
              println(" Erreur : Quantité de sucre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              sansErreur = false
            }
          }
          //Gestion d'erreur pour la boisson 3
          else if (boisson == 3) {
            if (laitNecess > stockLait) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              sansErreur = false
            }
            else if (cafeNecess > stockCafe) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              sansErreur = false
            }
            else if (sucreNecess > stockSucre) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              sansErreur = false
            }
          }

          // Condition permettant de continuer a payer si il n'ya pas d'erreur
          if (sansErreur) {
            val final_price = boissonPrix + sucrePrix + laitPrix
            if (laitSupp == 1) {
              println("Boisson sélectionnée : " + boissonNom)
              println("Niveau de sucre : " + sucreNom)
              println("Lait en supplément : " + laitNom)
              println("Dose : " + dose)
              println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " + CHF " + laitPrix + " = CHF " + final_price)
            }
            else if (laitSupp == 2) {
              println("Boisson sélectionnée : " + boissonNom)
              println("Niveau de sucre : " + sucreNom)
              println("Lait en supplément : " + laitNom)
              println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " = CHF " + final_price)
            }

            println("Veuillez payer en utilisant Twint.")
            // Un string qui est aleatoirement generer et est composer de 5 caracteres alphanumerique
            val CharPossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            var codeTwint =""
            for (i <- 0 to 4) {
              codeTwint = codeTwint + CharPossible((math.random() * CharPossible.length).toInt)
            }
            println("Votre code de paiement est : " + codeTwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (31 secondes)
            println("\nMerci ! Votre paiement a été accepté.")

            println("Préparation de votre boisson...")
            println("[...]")
            println("Votre " + boissonNom + " est prêt ! Bonne dégustation !")

            stockCafe = stockCafe - cafeNecess
            stockLait = stockLait - laitNecess
            stockSucre = stockSucre - sucreNecess

            //reinitialise toute les valeur de la boisson personaliser pour pouvoir retourner et refaire une boisson
            reaffiche = true
            mode = 0
            boisson = 0
            niveauSucre = 0
            laitSupp = 0
            dose = 0
          }
        }
      }
      //Mode admin
      else if (mode == 2) {

        // Demande au admin d'ecrire le pin si faut il redemande
        val PIN = "434343" //default Pin code
        var pinUtil = ""
        while (pinUtil != PIN) {
          println("Mode Admin")
          pinUtil = readLine("Enter PIN: ")
        }
        // apres avoir saisie le bon code il continue
        println("Accès autorisé.")
        //montre les stocks maintenant
        println("\nStocks:")
        println("  Poudre de café: " + stockCafe + "g")
        println("  Lait          : " + stockLait + "L")
        println("  Sucre         : " + stockSucre + "g")

        println("\nRéapprovisionnement des stocks...")
        //permet d'ajouter des quantites specifique de chaque
        println("Ajout:")
        val coffee_add = readLine("  Poudre de café: ").toInt
        stockCafe = stockCafe + coffee_add
        val milk_add = readLine("  Lait          : ").toDouble
        stockLait = stockLait + milk_add
        val sugar_add = readLine("  Sucre         : ").toInt
        stockSucre = stockSucre + sugar_add

        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal...")

        //reinitialise les valeurs necessaire pour retourner a la page de slection de mode
        reaffiche = true
        mode = 0
      }
    }
  }
}