import scala.io.StdIn._
object Main {
  // Variables de la gestion des stocks
  var stockCafe: Int = 50 // la quantité de café (en grammes)
  var stockSucre: Int = 30 // la quantité de sucre (en grammes)
  var stockLait: Double = 0.5 // la quantité de lait (en litres)

  val adminPin = 434343 // le code pin que l'administrateur doit entrer pour deverouiller le mode admin
  def main(args: Array[String]): Unit = {


    var machineActive = true // La machine reste active jusqu'à ce que l'utilisateur décide de la quitter

    // boucle principale
    var continue = true
    do {
      while (machineActive) {




        // choix de l'utilisateur
        println("\nNospresso Café")
        println("\nVeuillez selectionner votre mode")
        println("1) Mode Client")
        println("2) Mode Admin")
        println("3) Quitter")


        var validInput = false
        var userChoice = 0
        while (!validInput) {
          print("Votre choix: ")
          userChoice = readInt()
          if (userChoice >= 1 && userChoice <= 3) {
            validInput = true
          } else {
            println("Erreur : veuillez entrez uniquement un nombre entre 1 et 3.")
          }
        }

        // l'utilisateur est un client et va choisir sa boisson
        // Lancer le mode client

        if (userChoice == 1) {


          var choixBoisson = 0
          var choixBoissonValide = false
          while (!choixBoissonValide) {
            println("\nVeuillez sélectionner votre boisson :")
            println("1) Expresso")
            println("2) Cappuccino")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            choixBoisson = readInt()
            if (choixBoisson >= 1 && choixBoisson <= 3) {
              choixBoissonValide = true
            } else {
              println("Erreur : Veuillez entrer 1, 2 ou 3.")
            }
          }

          var prix = 0.0
          var cafe = 0
          var lait = 0.0

          //quantité de poudre de café et le prix pour un expresso
          if (choixBoisson == 1) {
            cafe = 8
            prix = 2.0
          }

          // quantité de poudre de café, de lait et le prix pour cappuccino
          else if (choixBoisson == 2) {
            cafe = 6
            lait = 0.100
            prix = 2.5
          }

          //pour un latte, le client doit choisir entre 3 tailles
          else if (choixBoisson == 3) {
            var taille = 0
            var choixTailleValide = false
            while (!choixTailleValide) {
              println("Choisissez la taille de votre Latte :")
              println("1) Petit")
              println("2) Moyen")
              println("3) Grand")
              taille = readInt()
              if (taille >= 1 && taille <= 3) {
                choixTailleValide = true
              } else {
                println("Erreur : Veuillez entrer 1, 2 ou 3.")
              }
            }

            //quantité de lait, de poudre de café et le prix selon le choix de taille du latte
            if (taille == 1) {
              cafe = 6
              lait = 0.120
              prix = 2.7
            }

            else if (taille == 2) {
              cafe = 8
              lait = 0.150
              prix = 3.2
            }

            else if (taille == 3) {
              cafe = 12
              lait = 0.200
              prix = 3.7
            }

          }




          // demander si le client veut du sucre
          var sucre = 0
          var choixSucreValide = false
          while (!choixSucreValide) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - GHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            sucre = readInt()
            if (sucre >= 1 && sucre <= 4) {
              choixSucreValide = true
            } else {
              println("Erreur : Veuillez entrer 1, 2, 3 ou 4.")
            }
          }


          var sucreQantite = 0

          // la quantité de sucre et le prix en fonction du choix de l'utilisateur
          if (sucre == 2) {
            sucreQantite = 5
            prix += 0.10
          }

          else if (sucre == 3) {
            sucreQantite = 10
            prix += 0.20
          }

          else if (sucre == 4) {
            sucreQantite = 15
            prix += 0.30
          }






          // option de lait supplémentaire pour cappuccino et latte
          if (choixBoisson == 2 || choixBoisson == 3) {
            println("Souhaitez-vous ajouter du lait supplémentaire ? (1 = Oui, 0 = Non)")

            val laitSupplementaire = readInt()
            if (laitSupplementaire == 1) {
              println("Combien de doses ? (1 dose = 50ml, max 3 doses)")
              val doses = readInt()
              if (doses > 0 && doses <= 3) {
                val laitSupplementaireQuantite = doses * 0.050
                if (stockLait >= lait + laitSupplementaireQuantite) {
                  lait += laitSupplementaireQuantite
                  prix += doses * 0.05 // 0.05 CHF par dose de 50ml
                } else {
                  println("Désolé, stock de lait insuffisant pour l'ajout demandé.")
                }
              } else {
                println("Nombre de doses invalide. Aucun lait supplémentaire ajouté.")
              }
            }
          }




          // vérification des stocks. Si les stock ne sont pas suffisant un message d'erreur est affiché

          if (stockLait < lait) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            continue = false
            return


          } else if (stockCafe < cafe) {
            println("Erreur : Quantite de poudre de café inssufisante pour préparer la boisson selectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
            continue = false
            return


          } else if (stockSucre < sucre) {
            println("Désolé, stock de sucre insuffisant pour préparer cette boisson.")
            continue = false
            return

          }


          // affichage de ce que l'utilisateur a choisi
          println(s"Vous avez choisi un café avec $cafe grammes de café et ${lait * 1000} ml de lait.")



          // Affichage du prix final
          println(s"Prix total de votre boisson : CHF $prix")

          // Paiement (interface de paiement)
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : AB12X")
          println("(En attente de validation du paiement...)")
          println("Merci ! Votre paiement a été accepté.")

          // Affichage de la confirmation de la commande et et de la preparation de la boisson
          println("Merci pour votre commande ! Votre boisson est en préparation...")
          println("Votre boisson est prête. Bonne dégustation !")




          // si l'utilisateur est l'administrateur
        } else if (userChoice == 2) {
          println("Entrez le code PIN administrateur :") // demande d'acces avec le code
          val pinSaisi = readInt()

          if (pinSaisi == adminPin) {
            println("Accès autorisé.")
            // Affichage des stocks actuels
            println(s"Stocks actuels : Café = $stockCafe g, Sucre = $stockSucre g, Lait = $stockLait L")

            // Réapprovisionnement en café
            println("Quantité de café à ajouter (en g) :")
            val ajoutCafe = readInt()
            stockCafe += ajoutCafe


            // Réapprovisionnement en sucre
            println("Quantité de sucre à ajouter (en g) :")
            val ajoutSucre = readInt()
            stockSucre += ajoutSucre


            // Réapprovisionnement en lait
            println("Quantité de lait à ajouter (en ml) :")
            val ajoutLait = readInt()
            stockLait += ajoutLait / 1000.0 // conversion en littre


            // Affichage de tous les ajouts effectué
            println("Vous avez ajouté:")
            println(s"$ajoutCafe g de café.")
            println(s"$ajoutSucre g de sucre.")
            println(s"${ajoutLait / 1000.0} L de lait.")





            // Affichage des stocks restants
            println(s"Il reste $stockCafe g de café, $stockSucre g de sucre et $stockLait L de lait.")
            println("niveau de stock mis à jour.")
            println("Retour au menu principal")

            // Le code n'est pas le bon
          } else {
            println("Code PIN incorrect. Accès refusé.")
          }



          // l'utilisateur decide de quitter
        } else if (userChoice == 3) {
          println("Merci d'avoir utilisé Nespresso.")
          machineActive = false
        } else {
          println("Choix invalide. Essayez encore.")


        }
      }

    }while (continue)


  }
}
