
import io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val pincode = 434343
    var stockCafe = 50.0
    var stockSucre = 30.0
    var stockLait = 0.500
    var consommationCafe = 0.0
    var consommationLait = 0.000
    var consommationSucre = 0.0
    var prixBase = 0.00
    var prixLaitSupp = 0.00
    var prixSucre = 0.00
    var prixSupp = 0.00
    var prixFinal = 0.0
    var boisson = ""
    var taille = ""
    var niveausucre = ""
    var laitSupp = ""


    var lancement = true
    while (lancement) {
      println()
      println("Nospresso Café")
      println()
      println("Veuillez s ́electionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var mode = readLine(">").toInt

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Erreur. La mode sélectionné n'est pas valide. Veuillez saisir 1, 2 ou 3.")
        mode = readLine(">").toInt
      }

      if (mode == 1) {
        var modeClient = true

        while (modeClient) {
          //lancement = false
          println()
          println("Mode Client")
          println()
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          var choixCafe = readLine(">").toInt

          while (choixCafe != 1 && choixCafe != 2 && choixCafe != 3) {
            println("Entrée invalide. Veuillez saisir 1, 2 ou 3.")
            choixCafe = readLine(">").toInt
          }

          if (choixCafe == 1) {
            boisson = "Expresso"
            taille = " "
            prixBase = 2.00
            prixSupp = 0.00
            prixLaitSupp = 0.00
            consommationCafe = 8.0
            consommationLait = 0.0
            laitSupp = "Non"
          }
          else if (choixCafe == 2) {
            boisson = "Cappuccino"
            taille = " "
            prixBase = 2.50
            consommationCafe = 6.0
            consommationLait = 0.100
          }

          else if (choixCafe == 3) {
            println("Veuillez sélectionner la taille:")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            var choixTaille = readLine(">").toInt

            while (choixTaille != 1 && choixTaille != 2 && choixTaille != 3) {
              println("Entrée invalide. Veuillez saisir 1, 2 ou 3.")
              choixTaille = readLine(">").toInt
            }

            if (choixTaille == 1) {
              boisson = "Latte"
              taille = " (Petit) "
              prixBase = 2.70
              consommationCafe = 6.0
              consommationLait = 0.120
            }
            else if (choixTaille == 2) {
              boisson = "Latte"
              taille = " (Moyen) "
              prixBase = 3.20
              consommationCafe = 8.0
              consommationLait = 0.150
            }
            else if (choixTaille == 3) {
              boisson = "Latte"
              taille = " (Grand) "
              prixBase = 3.70
              consommationCafe = 12.0
              consommationLait = 0.200
            }
          }

          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          var choixSucre = readLine(">").toInt

          while (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4) {
            println("Entrée invalide. Veuillez saisir 1, 2, 3 ou 4.")
            choixSucre = readLine(">").toInt
          }

          if (choixSucre == 1) {
            prixSucre = 0.00
            niveausucre = "Sans sucre"
            consommationSucre = 0.0
          }
          else if (choixSucre == 2) {
            prixSucre = 0.10
            niveausucre = "Peu (5g)"
            consommationSucre = 5.0
          }
          else if (choixSucre == 3) {
            prixSucre = 0.20
            niveausucre = "Moyen (10g)"
            consommationSucre = 10.0
          }
          else if (choixSucre == 4) {
            prixSucre = 0.30
            niveausucre = "Beaucoup (15g)"
            consommationSucre = 15.0
          }

          if (choixCafe == 2 || choixCafe == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            var choixLaitSupp = readLine(">").toInt

            if (choixLaitSupp == 1) {
              println("Combien de dose ? ")
              var doseLaitSupp = readLine(">").toDouble
              if (doseLaitSupp > 3){
                println("Erreur : 3 doses maximale par boisson. Veuillez entrer une quantité comprise entre 1 et 3.")
                doseLaitSupp = readLine(">").toDouble
              }
              prixLaitSupp = doseLaitSupp * 0.05
              consommationLait += (doseLaitSupp * 0.05)
              laitSupp = "Oui"
            }
            else if (choixLaitSupp == 2) {
              laitSupp = "Non"
              consommationLait = 0.0
              prixLaitSupp = 0.00
            }

            while (choixLaitSupp != 1 && choixLaitSupp != 2 ) {
              println("Entrée invalide. Veuillez saisir 1 ou 2.")
              choixLaitSupp = readLine(">").toInt
            }
          }

          if (stockCafe >= consommationCafe && stockLait >= consommationLait && stockSucre >= consommationSucre) {
            prixSupp = prixLaitSupp + prixSucre
            prixFinal = prixBase + prixSupp
            println()
            println("Boisson sélectionnée : " + boisson + taille)
            println("Niveau de sucre : " + niveausucre)
            println("Lait en supplément : " + laitSupp)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixSupp, prixFinal)
            println()

            val codeTwint = Random.alphanumeric.take(5).mkString
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été́ accepté.")

            println()

            println("Préparation de votre boisson...")
            println("[...]")
            Thread.sleep(5000)
            println("Votre " + boisson + " est prêt ! Bonne dégustation !")

            stockCafe -= consommationCafe
            stockLait -= consommationLait
            stockSucre -= consommationSucre

            //println("stock cafe : " + stockCafe + " stock lait: " + stockLait + " stock sucre: " + stockSucre)

            modeClient = false
          }

          else {
            if (stockLait < consommationLait) {
              println("stock cafe : " + stockCafe + " stock lait: " + stockLait + " stock sucre: " + stockSucre)
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sé́lectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              println()
            }
            else if (stockCafe < consommationCafe) {
              println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println()
            }
            else if (stockSucre < consommationSucre) {
              println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println()
            }
          }
        }
      }

      else if (mode == 2) {
        var modeAdmin = true
        while (modeAdmin) {
          println()
          println("Mode Admin")
          var pinEntree = readLine("Entrez le code PIN :").toInt
          while (pinEntree != pincode) {
            println("Code PIN invalide. Veuillez réesseyer.")
            pinEntree = readLine(">").toInt
          }
          if (pinEntree == pincode){
            println("Accès autorisé.")
            println("Stocs : ")
            println("Poudre à café : " + stockCafe + "g")
            println("Lait : " + stockLait + "L")
            println("Sucre : " + stockSucre + "g")
            println()

            println("Réapprovisionnement des stocks...")
            println("Ajout :")
            var ajoutStockCafe = readLine("Poudre de café : ").toDouble
            var ajoutStockLait = readLine("Lait : ").toDouble
            var ajoutStockSucre = readLine("Sucre : ").toDouble

            stockCafe += ajoutStockCafe
            stockLait += ajoutStockLait
            stockSucre += ajoutStockSucre

            println("Niveaux de stock mis à jour.")
            println("Retour au menu principal...")

            modeAdmin = false
          }
        }
      }

      else if (mode == 3) {
        println("Fin du programme. Merci pour votre utilisation.")
        lancement = false
      }
    }


  }
}
