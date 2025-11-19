import io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val adminPIN = "434343" // Mot de passe administrateur par défaut

    var saisiPIN = "" // Mot de passe saisi par l'utilisateur

    var mode = 0 // Choix de l'utilisateur entre Client/Admin/Quitter
    var boissonId = 0 // Boisson sélectionnée par le client
    var boissonTailleId = 0 // Taille du latte sélectionnée par le client
    var sucreSupp = 0 // Quantité de sucre supplémentaire sélectionnée par le client
    var laitSupp = 0 // Quantité de lait supplémentaire sélectionnée par le client
    var boissonSelect = "" // Nom de la boisson sélectionnée affiché à l'écran
    var boissonTailleSelect = "" // Taille de la boisson sélectionnée affichée à l'écran
    var sucreSelect = ""   // Quantité de sucre supplémentaire sélectionnée affichée à l'écran
    var laitSelect = ""    // Quantité de lait supplémentaire sélectionnée affichée à l'écran

    var prixBoisson = 0.0 // Prix de base de la boisson sélectionnée par le client

    var poudreCafeConsomme = 0 // Quantité de café en poudre consommée lors de la préparation de la boisson sélectionnée
    var laitConsomme = 0.0 // Quantité de lait consommée lors de la préparation de la boisson sélectionnée
    var sucreConsomme = 0 // Quantité de sucre consommée lors de la préparation de la boisson sélectionnée

    var stockPoudreCafe = 50 // Quantité actuelle en stock de café en poudre
    var stockLait = 0.5 // Quantité actuelle en stock de lait
    var stockSucre = 30 // Quantité actuelle en stock de sucre

    var ajoutStockPoudreCafe = 0 // Quantité de café en poudre ajoutée au stock par l'administrateur
    var ajoutStockSucre = 0 // Quantité de sucre ajoutée au stock par l'administrateur
    var ajoutStockLait = 0.0 // Quantité de lait ajoutée au stock par l'administrateur

    var stockDispo = true // Disponibilité des stocks pour la boisson sélectionnée

    // Message de notification pour une sélection incorrecte
    val invalidChoiceMessage = "\nErreur: Vous avez fait un mauvais choix. Veuillez choisir une des options proposées." +
      "\nVeuillez choisir une des options proposées.\n"

    var exit = false // Quitter le programme

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //Le programme démarre
    println("         Nospresso Café")

    //Sauf si le mode de Quitter est sélectionné;
    while(!exit) {
      //L'utilisateur sélectionne le mode
      while (mode != 1 && mode != 2 && mode != 3) {
        println("Veuillez sélectionner votre mode: ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt

        //Si le mode Client est sélectionné;
        if(mode == 1) {
          //La sélection de boissons
          boissonId = 0
          while (boissonId != 1 && boissonId != 2 && boissonId != 3) {
            println()
            println("Veuillez sélectionner votre boisson : ")
            println("1) Expresso - CHF 2.00")
            println("2) Cappucino - CHF 2.50")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            boissonId = readLine(">").toInt

            //Si la boisson a été mal choisie;
            if(boissonId != 1 && boissonId != 2 && boissonId != 3){
              println(invalidChoiceMessage)
              boissonId = 0
            }

            //Si la boisson n'a pas été mal choisie;
            else{
              //Si expresso est sélectionné;
              if(boissonId == 1){
                boissonSelect = "Expresso"
                poudreCafeConsomme = 8
                laitConsomme = 0.0
                prixBoisson = 2.00
              }
              //Si cappuccino est sélectionné;
              else if(boissonId == 2){
                boissonSelect = "Cappuccino"
                poudreCafeConsomme = 6
                laitConsomme = 0.1
                prixBoisson = 2.50
              }
              //Si late est sélectionné;
              else{
                //Choisir la taille du Latte
                boissonSelect = "Latte"
                boissonTailleId = 0
                while (boissonTailleId != 1 && boissonTailleId != 2 && boissonTailleId != 3) {
                  println()
                  println("Veuillez sélectionner la taille de votre boisson : ")
                  println("1) Petit")
                  println("2) Moyen")
                  println("3) Grand")
                  boissonTailleId = readLine(">").toInt

                  //Latte (Petit)
                  if(boissonTailleId == 1){
                    boissonTailleSelect = "(Petit)"
                    poudreCafeConsomme = 6
                    laitConsomme = 0.12
                    prixBoisson = 2.70
                  }

                  //Latte (Moyen)
                  else if(boissonTailleId == 2){
                    boissonTailleSelect = "(Moyen)"
                    poudreCafeConsomme = 8
                    laitConsomme = 0.15
                    prixBoisson = 3.20
                  }

                  //Latte (Grand)
                  else if(boissonTailleId == 3){
                    boissonTailleSelect = "(Grand)"
                    poudreCafeConsomme = 12
                    laitConsomme = 0.2
                    prixBoisson = 3.70
                  }

                  //Si la taille de latte est mal choisie;
                  else {
                    println(invalidChoiceMessage)
                    boissonTailleId = 0
                  }
                }
              }

              //Personnalisation de la boisson
              //Ajouter du sucre supplémentaire
              while(sucreSupp < 1 || sucreSupp > 4){
                println()
                println("Souhaitez-vous ajouter du sucre ? ")
                println("1) Sans sucre")
                println("2) Peu (5g) - CHF 0.10")
                println("3) Moyen (10g) - CHF 0.20")
                println("4) Beaucoup (15g) - CHF 0.30")
                sucreSupp = readLine(">").toInt
                //Sans sucre
                if(sucreSupp == 1){
                  sucreSelect = "Sans sucre"
                  sucreConsomme = 0
                }
                //Peu
                else if(sucreSupp == 2){
                  sucreSelect = "Peu (5g)"
                  sucreConsomme = 5
                }
                //Moyen
                else if(sucreSupp == 3){
                  sucreSelect = "Moyen (10g)"
                  sucreConsomme = 10
                }
                //Beaucoup
                else if(sucreSupp == 4){
                  sucreSelect = "Beaucoup (15g)"
                  sucreConsomme = 15
                }
                //Si le sucre supplémentaire est mal choisie;
                else {
                  println(invalidChoiceMessage)
                  sucreSupp = 0
                  sucreConsomme = 0
                }
              }

              //Ajouter du lait supplémentaire
              var choix = 0
              while(choix != 1 && choix != 2 && boissonId != 1){
                println()
                println("Souhaitez-vous ajouter du lait en supplément ? ")
                println("(Disponible uniquement pour Cappuccino et Latte)")
                println("1) Oui")
                println("2) Non")
                choix = readLine(">").toInt

                if(choix == 1){
                  while(laitSupp < 1 || laitSupp > 3){
                    println()
                    println("Combien de dose ? \n1) 1 dose : 50mL\n2) 2 doses : 100mL\n3) 3 doses : 150mL")
                    laitSupp = readLine(">").toInt
                    // 1 dose
                    if(laitSupp == 1){
                      laitSelect = "1 dose"
                    }
                    //1 doses
                    else if(laitSupp == 2){
                      laitSelect = "2 doses"
                    }
                    // 3 doses
                    else if (laitSupp == 3){
                      laitSelect = "3 doses"
                    }
                    //Si le dose de lait supplémentaire est mal choisie;
                    else{
                      println(invalidChoiceMessage)
                      laitSupp = 0
                    }
                  }
                }
                //Si une dose de lait supplémentaire n'est pas sélectionnée;
                else if(choix == 2){
                  laitSupp = 0
                  laitSelect = "Non"
                }
                //Si la sélection de lait supplémentaire est incorrecte;
                else{
                  println(invalidChoiceMessage)
                  choix = 0
                }
              }

              //Les informations sur la boisson et les ajouts sélectionnés sont imprimées à l'écran:
              println()
              println("-------------------------------------------------------------------------------------")
              println("Boisson sélectionnée : " + boissonSelect + " " + boissonTailleSelect)
              println("Niveau de sucre : " + sucreSelect)
              if(boissonId != 1)
                println("Lait en supplément : " + laitSelect)

              //Des contrôles de suffisance des stocks sont effectués
              stockDispo = true
              //Contrôle de qualification du stock de café en poudre :
              if(poudreCafeConsomme > stockPoudreCafe){
                stockDispo = false
                println()
                println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println("-------------------------------------------------------------------------------------")
                println()
              }
              //Contrôle de l'adéquation du stock de sucre:
              if(sucreConsomme > stockSucre){
                stockDispo = false
                println()
                println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez réduire la quantité de sucre ou choisir une autre boisson.")
                println("-------------------------------------------------------------------------------------")
                println()
              }
              //Contrôle de l'adéquation du stock de lait:
              if((laitConsomme + (laitSupp * 0.05)) > stockLait){
                stockDispo = false
                println()
                println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                println("-------------------------------------------------------------------------------------")
                println()
              }

              //Si les stocks sont suffisants pour la boisson sélectionnée;
              if(stockDispo){
                //Le stock est mis à jour en réduisant les quantités consommées du stock.
                stockPoudreCafe -= poudreCafeConsomme
                stockLait -= (laitConsomme + (laitSupp * 0.05))
                stockSucre -= sucreConsomme

                //Le prix est calculé pour la boisson sélectionnée et les produits ajoutés.
                if(sucreSupp <= 1){

                  if(laitSupp < 1){
                    //L'ajout de sucre et l'ajout de lait ne sont pas sélectionnés :
                    printf("Prix total : CHF %.2f\n", prixBoisson)
                  }
                  else{
                    //L'ajout du lait sélectionné:
                    printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, (laitSupp * 0.05), (prixBoisson  + (laitSupp * 0.05)))
                  }
                }
                else{
                  if(laitSupp < 1){
                    //L'ajout du sucre sélectionné:
                    printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, ((sucreSupp -1) * 0.10), (prixBoisson + ((sucreSupp -1) * 0.10)))
                  }
                  else{
                    //L'ajout de sucre et l'ajout de lait ont été retenus :
                    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, ((sucreSupp -1) * 0.10), (laitSupp * 0.05), (prixBoisson + ((sucreSupp -1) * 0.10) + (laitSupp * 0.05)))
                  }
                }

                //Processus de paiement:
                println()
                println("Veuillez payer en utilisant Twint.")
                println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString)
                println("(En attente de paiement...)")
                Thread.sleep(3000)

                //Paiement confirmé, boisson en préparation
                println()
                println("Paiement confirmé.")
                println("Préparation de votre boisson...")
                println("Votre " + boissonSelect + " est prêt ! Bonne dégustation !")
                println("-------------------------------------------------------------------------------------")
                println()

                //Les variables sont réinitialisées pour la nouvelle sélection
                // et vous êtes dirigé vers l'écran de sélection de mode.
                mode = 0
                sucreSupp = 0
                laitSupp = 0
              }
              //Si les stocks ne sont pas suffisants pour la boisson sélectionnée,
              //celle-ci revient au menu de sélection des boissons ;
              else{
                boissonId = 0
                sucreSupp = 0
                laitSupp = 0
              }
            }
          }
        }

        //Si le mode Admin est sélectionné;
        else if(mode == 2) {
          //Code PIN administrateur requis;
          println("Mode Admin")
          saisiPIN = readLine("Entrez le code PIN : ")

          //Si le code PIN administrateur saisi est incorrect;
          while(adminPIN != saisiPIN){
            println()
            println("Le code PIN que vous avez entré est incorrect. Veuillez réessayer.")
            saisiPIN = readLine("Entrez le code PIN : ")
          }

          //Si le code PIN administrateur saisi est correct;
          if(adminPIN == saisiPIN){
            println("Accès autorisé.")
            println()

            //Les quantités en stock actuelles sont affichées:
            println("Stock: ")
            println("    Poudre de café : " + stockPoudreCafe + "g")
            printf("    Lait           : %.2fL\n", stockLait)
            println("    Sucre          : " + stockSucre + "g")

            //Ajout de café en poudre au stock:
            println()
            println("Quelle quantité de café de poudre souhaitez-vous ajouter ? (g)")
            print(">")
            ajoutStockPoudreCafe = readInt()
            //Si le quantité a été mal saisi;
            while(ajoutStockPoudreCafe < 0){
              println()
              println("Erreur: Vous avez saisi une quantité incorrecte, veuillez réessayer.")
              println("Quelle quantité de café de poudre souhaitez-vous ajouter ? (g)")
              print(">")
              ajoutStockPoudreCafe = readInt()
            }

            //Ajout de lait au stock:
            println()
            println("Quelle quantité de lait souhaitez-vous ajouter ? (L)")
            print(">")
            ajoutStockLait = readDouble()
            //Si le quantité a été mal saisi;
            while(ajoutStockLait < 0){
              println()
              println("Erreur: Vous avez saisi une quantité incorrecte, veuillez réessayer.")
              println("Quelle quantité de lait souhaitez-vous ajouter ? (L)")
              print(">")
              ajoutStockLait = readDouble()
            }

            //Ajout de sucre au stock:
            println()
            println("Quelle quantité de sucre souhaitez-vous ajouter ? (g)")
            print(">")
            ajoutStockSucre = readInt()
            //Si le quantité a été mal saisi;
            while(ajoutStockSucre < 0){
              println()
              println("Erreur: Vous avez saisi une quantité incorrecte, veuillez réessayer.")
              println("Quelle quantité de sucre souhaitez-vous ajouter ? (g)")
              print(">")
              ajoutStockSucre = readInt()
            }

            //Les nouveaux montants à ajouter sont indiqués:
            println()
            println("Réapprovisionnement des stocks...")
            println("Ajout : ")
            println("    Poudre de café : " + ajoutStockPoudreCafe)
            printf("    Lait           : %.2f\n", ajoutStockLait)
            println("    Sucre          : " + ajoutStockSucre)
            println("Niveaux de stock mis à jour.")
            println("Retour au menu principal...")
            println()
            println()

            //Les stocks sont mis à jour avec de nouvelles quantités
            stockPoudreCafe = stockPoudreCafe + ajoutStockPoudreCafe
            stockLait = stockLait + ajoutStockLait
            stockSucre = stockSucre + ajoutStockSucre

            //Les variables sont réinitialisées pour la nouvelle sélection
            // et vous êtes dirigé vers l'écran de sélection de mode.
            ajoutStockPoudreCafe = 0
            ajoutStockLait = 0.0
            ajoutStockSucre = 0
            mode = 0
            saisiPIN = ""
          }
        }

        //Si le mode Quitter est sélectionné;
        else if(mode == 3) {
          println("Le programme a été terminé.")
          exit = true
        }

        //Si une mauvaise sélection de mode est effectuée;
        else{
          println(invalidChoiceMessage)
        }
      }
    }
  }
}