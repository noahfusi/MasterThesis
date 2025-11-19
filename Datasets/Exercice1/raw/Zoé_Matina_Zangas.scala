import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Déclaration des variables (les mesures sont en gramme pour le café et le sucre, en litre pour le lait)

    // variables pour les choix et la personalisation des boissons
    var choixmode = 0
    var choixboisson = 0.0
    var choixsucre = 0
    var choixlait = 0.0
    var choixdoselait = 0.0
    var boissonselectionnee1 = "" // vairable pour le récapitulatif (pour le latte ajout entre parenthèses de la taille)
    var boissonselectionnee = "" // variable pour la phrase après paiement (pour le latte, pas de précision de la taille)
    var sucreselectionne = ""
    var laitselectionne = ""

    // variables pour les prix
    var prixboisson = 0.00
    var prixtotal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00

    // variables pour la gestion des stock
    var stockcafe = 50
    var stocksucre = 30
    var stocklait = 0.500
    var caferequis = 0
    var sucrerequis = 0
    var laitrequis = 0.0
    var stocksuffisant = 1

    // variables pour les codes
    val longeurcodeTwint = 5
    val alphanumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    var codeTwint = ""
    var codePINadmin = 0



    // Etape 1: sélection du mode
    // Sélection du mode, arrêt du programme lors du choix "quitter" (3) et retour au menu principal seulement lorsque la boisson est réalisée
    while (choixmode != 3) {
      if (choixmode == 0) {
        println()
        println("       Nospresso Café    ")
        println("Veuillez sélectionner votre mode")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        println(">")
        choixmode = readInt()
      }

      // verification que l'entrée est valide (pour le choix du mode)
      while (choixmode != 1 && choixmode != 2 && choixmode != 3) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        choixmode = readInt()
      }

      // Etape 2: mode utitilasteur
      // Réinitialisation des prix (à 0) pour chaque transaction
      if (choixmode == 1) {
        prixboisson = 0.0
        prixlait = 0.0
        prixsucre = 0.0
        stocksuffisant = 1

        // choix de la boisson
        println("Veuillez sélectionner votre boisson:")
        println("1) Expresso - CHF 2.00")
        println("2) Capuccino - CHF 2.50")
        println("Latte:  3.1) Petit - CHF 2.70    3.2) Moyen - CHF 3.20    3.3) Grand - CHF 3.70")
        println(">")
        choixboisson = readDouble()

        // Vérification que l'entrée est valide (pour le choix de la boisson)
        while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3.1 && choixboisson != 3.2 && choixboisson != 3.3) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          choixboisson = readDouble()
        }

        // changement des variables caferequis et laitrequis pour faciliter le calcul des stock
        // changement des variables boissonselectionnee et prixboisson pour faciliter l'affichage du recapitulatif
        // changement de la variable prixboisson pour faciliter le calcul du prix
        if (choixboisson == 1) {
          caferequis = 8
          laitrequis = 0.000
          boissonselectionnee1 = "Expresso"
          boissonselectionnee = boissonselectionnee1
          prixboisson = 2.00
        }
        else if (choixboisson == 2) {
          caferequis = 6
          laitrequis = 0.100
          boissonselectionnee1 = "Capuccino"
          boissonselectionnee = boissonselectionnee1
          prixboisson = 2.50
        }
        else if (choixboisson == 3.1) {
          caferequis = 6
          laitrequis = 0.120
          boissonselectionnee1 = "Latte (Petit)"
          boissonselectionnee = "Latte"
          prixboisson = 2.70
        }
        else if (choixboisson == 3.2) {
          caferequis = 8
          laitrequis = 0.150
          boissonselectionnee1 = "Latte (Moyen)"
          boissonselectionnee = "Latte"
          prixboisson = 3.20
        }
        else {
          caferequis = 12
          laitrequis = 0.200
          boissonselectionnee1 = "Latte (Grand)"
          boissonselectionnee = "Latte"
          prixboisson = 3.70
        }

        // Personalisation de la boisson: choix du sucre
        println("Souhaitez vous ajouter du sucre?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        println(">")
        choixsucre = readInt()

        // vérification que l'entrée est valide (pour le choix du sucre)
        while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          choixsucre = readInt()
        }

        // changement de la variable sucrerequis pour faciliter le calcul des stock
        // changement des variables sucreselectionne pour faciliter l'affichage du recapitulatif
        // changement de la variable prix pour faciliter le calcul du prix

        sucrerequis = 5 * (choixsucre - 1) // formule génlérale qui englobe les 3 cas possibles (permet de simplifier et reduire les instructions)
        prixsucre = 0.10 * (choixsucre - 1)

        if (choixsucre == 1) {
          sucreselectionne = "Sans sucre"
        }
        else if (choixsucre == 2) {
          sucreselectionne = "Peu (5g)"
        }
        else if (choixsucre == 3) {
          sucreselectionne = "Moyen (10g)"
        }
        else {
          sucreselectionne = "Beaucoup (15g)"
        }

        // personalisation de la boisson : choix du lait (que pour le cappuccino et latte)
        if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
          println("Souhaitez vous ajouter du lait en supplément ?")
          println("1) oui")
          println("2) non")
          println(">")
          choixlait = readDouble()
          prixtotal = prixboisson

          // vérfication que l'entrée est valide (pour le choix du lait)
          while (choixlait != 1 && choixlait != 2) {
            println("Veuillez sélectionner une entrée valide")
            println(">")
            choixlait = readDouble()
          }

          // Personalisation de la boisson: choix du nombre de dose de lait (seulement si l'option 1 (Oui) est selectionée)
          // Changement de variables laitselectionne et prixlait pour faciliter l'affichage du recapitulatif et le calcul du prix
          if (choixlait == 1) {
            laitselectionne = "Oui"
            println("Combien de dose souhaitez vous ? Vous pouvez ajouter jusqu'à 3 dose maximum. 50 ml de lait et - CHF 0.05 par dose")
            println(">")
            choixdoselait = readDouble()

            // vérification que l'entrée est valide (pour le choix du nombre de dose de lait)
            while (choixdoselait != 1 && choixdoselait != 2 && choixdoselait != 3) {
              println("Veuillez sélectionner une entrée valide")
              println(">")
              choixdoselait = readDouble()
            }
            // changement de la variable laiterequis pour faciliter le calcul des stock
            // changement de la variable prixlait pour faciliter l'affichage du recapitulatif et le calcul du prix
            laitrequis += choixdoselait * 0.050 // formule générale qui englobe les trois cas possibles (permet de simplifier et réduire les instructions)

            prixlait = choixdoselait * 0.050
          }

          if (choixlait == 2) {
            laitselectionne = "Non"
            prixlait = 0.0
            choixdoselait = 0
          }
        }

        // Vérification des stock: affichage du récapitulatif de la boisson sans les prix et messages d'erreurs
        if ((stockcafe < caferequis) || (stocksucre < sucrerequis) || (stocklait < laitrequis)) {
          if (choixboisson == 1) {
            println("Boisson sélectionnée : " + boissonselectionnee1)
            println("Niveau de sucre : " + sucreselectionne)
            println()
          }
          if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
            println("Boisson sélectionnée : " + boissonselectionnee1)
            println("Niveau de sucre : " + sucreselectionne)
            println("Lait supplémentaire : " + laitselectionne)
            println()
          }
          if (stockcafe < caferequis) {
            if ((choixboisson == 1) || (choixboisson == 2) || (choixboisson == 3.1)) {
              println("Erreur : quantité de poudre de café insuiffisante pour préparer la boisson séléctionnée.")
              println("Veuillez choisir une autre boisson ou verifier les stocks en mode Admin.")
              println()
            }
            else {
              println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
              println("Veuillez choisir une taille plus petite, essayer une autre boisson ou verifier les stocks en mode Admin.")
              println()
            }
            stocksuffisant = 2 //changement de la variable stocksuffisant pour revenir au menu client si les stocks sont insuffisants
          }

          if (stocksucre < sucrerequis) {
            println("Erreur : quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
            println("Veuillez choisir une quantité de sucre plus petite ou verifier les stocks en mode Admin.")
            println()

            stocksuffisant = 2
          }

          if (stocklait < laitrequis) {
            if ((choixboisson == 2) || (choixboisson == 3.1)) {
              println("Erreur : quantité de lait insuffisante pour préprarer la boisson séléctionnée.")
              println("Veuillez choisir une autre boisson ou verifier les stocks en mode Admin.")
              println()
            }
            if ((choixboisson == 3.2) || (choixboisson == 3.3)) {
              println("Erreur : quantité de lait insuffisante pour préparer la boisson séléctionnée.")
              println("Veuillez choisir une taille plus petite, essayer une autre boisson ou vérifier les stocks en mode Admin.")
              println()
            }
            stocksuffisant = 2
          }
        }

        // Affichage du recapitulatif avec les prix (une fois que les stocs sont validés)
        // Comparaison avec la variable stocksuffisant pour permetre de continuer seulement si les stocks suffisent
        if (stocksuffisant == 1) {
          prixtotal = prixboisson + prixsucre + prixlait

          if (choixboisson == 1) {
            println("Boisson sélectionnée : " + boissonselectionnee1)
            println("Niveau de sucre : " + sucreselectionne)
            if (choixsucre == 1) {
              printf("Prix total : CHF %.2f", prixtotal) // Affichage du prix à deux décimales pour assurer un prix aux centimes (cohérent)
            }
            else {
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
            }
          }
          else {
            println("Boisson sélectionnée : " + boissonselectionnee1)
            println("Niveau de sucre : " + sucreselectionne)
            println("lait supplémentaire : " + laitselectionne)

            if (choixsucre == 1 && choixlait == 2)
              printf("Prix total: CHF %.2f", prixtotal)
            if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 2))
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
            if (choixsucre == 1 && choixlait == 1)
              printf("Prix total :  CHF %.2f + CHF %.2f  =  CHF %.2f", prixboisson, prixlait, prixtotal)
            if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 1))
              printf("Prix total :  CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixlait, prixtotal)
          }

          // Paiement avec un code Twint aléatoire: utilisation d'une boucle for pour additionner 5 caractères
          codeTwint = "" // initialisation du code
          for (_ <- 1 to longeurcodeTwint) {
            var alphanumericaleatoire = alphanumeric(Random.nextInt(alphanumeric.length))
            codeTwint += alphanumericaleatoire
          }
          println()
          println()
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + codeTwint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000) // attente de 3 secondes (3000 millisecondes)
          println("          ")
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          println("[...]")
          println("Votre " + boissonselectionnee + " est prêt ! Bonne dégustation !")
          println()

          // Déduction des stock après la transaction
          stockcafe -= caferequis
          stocklait -= laitrequis
          stocksucre -= sucrerequis
        }
      }

      // Etape 3 : mode Admin
      // Entrée du code PIN (retour a la demande du code PIN si il est faux)
      if (choixmode == 2) {
        codePINadmin = 0
        while (codePINadmin != 434343) {
          println("Mode Admin")
          println("Entrez le code PIN : ******")
          codePINadmin = readInt()
        }

        // Affichage des stocks actuels (seulement lorsque le code PIN est validé)
        println("Accès autorisé.")
        println()
        println("Stock : ")
        println("Poudre de café : " + stockcafe + "g")
        printf("Lait :  %.3fL", stocklait) // affichage du lait à 3 décimales pour la cohérence des mesures
        println()
        println("Sucre : " + stocksucre + "g")
        println()

        // Réaprovisionnement des stocks et changement des variables stockcafe, stocksucre et stocklait pour enregistrer les ajouts
        println("Réapprovisionnement des stocks...")
        println("Ajout : ")
        println("Poudre de café : ")
        stockcafe += readInt()
        println("Lait : ")
        stocklait += readDouble()
        println("Sucre : ")
        stocksucre += readInt()
        println("Niveaux des stock mis à jour.")
        println("Retour au menu principal...")
        println()
      }
      // retour au menu principal seulement si les stock sont suffisants, si non, retour au menu client.
      if (stocksuffisant != 2 && choixmode != 3) {
        choixmode = 0
      }
    }
  }
}















