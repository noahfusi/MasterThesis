



import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.util.Random
import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var argent = 0.0 // comme Double
    var continuer = true

    // Définition des stocks initiaux
    var stockCafe = 50.0 // en grammes
    var stockSucre = 30.0 // en grammes
    var stockLait = 0.5 // en litres (500 ml)

    // Coût d'une dose de lait supplémentaire
    val coutLaitSupplementaire = 0.05

    // Boucle principale pour afficher à nouveau le menu après chaque action
    var continuerClient = true
    while (continuerClient == true) {
      println("Nospressocafé \nVeuillez selectionner votre mode: \n1)Client \n2)Admin \n3)Quitter \n>")
      var choix = readInt()


      while (choix == 1) { // Mode Client
        var pin = ""
        val car ="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        for (i<- 1 to 5) {// Génère un nombre aléatoire de 6 chiffres
          var twint = (Math.random() * 38).toInt
          pin += car(twint)
        }

          println("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>")

          var boissonDesiree = readInt()
        while (boissonDesiree != 1 && boissonDesiree != 2 && boissonDesiree != 3 ) {
          println("Entrée invalide, veuillez entrer '1', '2' ou '3'")
          println("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>")
          boissonDesiree = readInt()
        }


        // Variables pour la personnalisation de la boisson
          var sucre = 0
          var lait = 0.0
          var prix = 0.0

          // Variables pour vérifier si les ingrédients sont suffisants
          var suffisanceCafe = true
          var suffisanceSucre = true
          var suffisanceLait = true

          // Vérification de la boisson choisie
          if (boissonDesiree == 1) { // Expresso
            continuerClient = true
            prix = 2.00
            println("Vous avez choisi un Expresso")

            // Vérification des ingrédients nécessaires pour faire un expresso
            if (stockCafe >= 8) {
              suffisanceCafe = true
            } else {
              println("Il manque du café")
              suffisanceCafe = false
              continuerClient = false
            }
            if (stockSucre >= 5) {
              suffisanceSucre = true
            } else {
              println("Il manque du sucre")
              suffisanceSucre = false
              continuerClient = false
            }

            // Si les ingrédients sont suffisants
            if (suffisanceCafe && suffisanceSucre) {
              println("Souhaitez-vous ajouter du sucre? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n >")
              var sucreChoisiExpresso = readInt()
               while (sucreChoisiExpresso != 1 && sucreChoisiExpresso != 2 && sucreChoisiExpresso != 3 && sucreChoisiExpresso != 4) {
                 println("Entrée invalide, veuillez entrer '1', '2' ou '3' ou '4'")
                 println("Souhaitez-vous ajouter du sucre? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n >")
                 sucreChoisiExpresso = readInt()
        }
              if (sucreChoisiExpresso == 1) {
                sucre = 0
                prix = 2.00 // Expresso sans sucre
              } else if (sucreChoisiExpresso == 2) {
                sucre = 5
                prix = 2.10 // Expresso avec peu de sucre
              } else if (sucreChoisiExpresso == 3) {
                sucre = 10
                prix = 2.20 // Expresso avec moyen de sucre
              } else if (sucreChoisiExpresso == 4) {
                sucre = 15
                prix = 2.30 // Expresso avec beaucoup de sucre
              } else {
                println("Choix du sucre invalide.")
                prix = 2.00
              }

              // Calcul du prix en fonction de la quantité de sucre
              println("Vous avez ajouté " + sucre + " grammes de sucre à votre Expresso.")
              printf("Le prix de votre Expresso avec " + sucre + " grammes de sucre est de %.2f CHF.", prix)
              choix = 0


              // Mise à jour des stocks
              stockCafe -= 8
              stockSucre -= sucre
            } else {
              // Si les ingrédients sont manquants
              println("Désolé, il manque ces ingrédients pour préparer votre Expresso.")
            }
            boissonDesiree = 0 // Réinitialiser le choix de boisson

          }


          if (boissonDesiree == 2) { // Cappuccino
            continuerClient = true
            println("Vous avez choisi un Cappuccino")

            // Vérification nécessaire des ingrédients pour faire un cappuccino
            if (stockCafe >= 6) {
              suffisanceCafe = true
            } else {
              println("Il manque du café")
              suffisanceCafe = false
              continuerClient=false
            }
            if (stockSucre >= 5) {
              suffisanceSucre = true
            } else {
              println("Il manque du sucre")
              suffisanceSucre = false
              continuerClient=false
            }
            if (stockLait >= 0.1) {
              suffisanceLait = true
            } else {
              println("Il manque du lait")
              suffisanceLait = false
              continuerClient=false

            }

            // Vérification de la disponibilité des ingrédients et personnalisation
            if (suffisanceCafe==true && suffisanceSucre == true && suffisanceLait == true) {
              println("Souhaitez-vous ajouter du sucre? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n >")
              var sucreChoisiCappuccino = readInt()
              while (sucreChoisiCappuccino != 1 && sucreChoisiCappuccino != 2 &&  sucreChoisiCappuccino != 3 && sucreChoisiCappuccino != 4) {
                 println("Entrée invalide, veuillez entrer '1', '2' ou '3' ou '4'")
                 println("Souhaitez-vous ajouter du sucre? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n >")
                 sucreChoisiCappuccino = readInt()}

              if (sucreChoisiCappuccino == 1) {
                sucre = 0
                prix = 2.50 // Cappuccino sans sucre
              } else if (sucreChoisiCappuccino == 2) {
                sucre = 5
                prix = 2.60 // Capuccino avec un peu de sucre
              } else if (sucreChoisiCappuccino == 3) {
                sucre = 10
                prix = 2.70 // Cappuccino avec moyen de sucre
              } else if (sucreChoisiCappuccino == 4) {
                sucre = 15
                prix = 2.80 // Cappuccino avec beaucoup de sucre
              } else {
                println("Choix du sucre invalide")
                prix = 2.50
              }
              println("Souhaitez-vous ajouter du lait en supplément? \n1) Oui \n2) Non \n>")
              var choixLait = readInt()
                while (choixLait != 1 && choixLait != 2 ) {
                  println("Entrée invalide, veuillez entrer '1' ou '2'")
                  println ("Souhaitez-vous ajouter du lait en supplément? \n1) Oui \n2) Non \n>")
                  choixLait = readInt()
                  }

              if (choixLait == 1) {
                println("Combien de doses de lait ?")
                var laitChoisi = readInt()
                while (laitChoisi != 1 && laitChoisi != 2 && laitChoisi != 3 ) {
                  println("Maximum 3 doses permises")
                println ("Combien de doses de lait ? \n >")
                  laitChoisi = readInt()}


                if (laitChoisi == 0) {
                  lait = 0.0
                } else if (laitChoisi == 1) {
                  lait = 0.05
                } else if (laitChoisi == 2) {
                  lait = 0.1
                } else if (laitChoisi == 3) {
                  lait = 0.15
                } else {



                }
                prix += lait
                stockLait -= laitChoisi * 0.050
                }

              println("Vous avez ajouté " + sucre + " grammes de sucre à votre Cappuccino.")
              printf("Le prix de votre Cappuccino avec " + sucre + " grammes de sucre est de %.2f CHF.", prix)


                println("Vous avez ajouté " + lait + " ml de lait à votre Cappuccino.")
              choix = 0


              // Mise à jour des stocks
              stockCafe -= 6
              stockLait -= 0.1
              stockSucre -= sucre
            } else {
              // Si les ingrédients sont manquants
              println("Désolé, il manque ces ingrédients pour préparer votre Cappuccino.")
            }
            boissonDesiree = 0 // Réinitialiser le choix de boisson
          }

        if (boissonDesiree == 3) { // Latte
          continuerClient = true
          println("Vous avez choisi un Latte")

          // Demander quelle taille de Latte
          println("Quelle taille de Latte voulez-vous? \n1) Petit (6g de café et 120 ml de lait) \n2) Moyen (8g de café et 150 ml de lait) \n3) Grand (12 g de café et 200 ml de lait) \n> ")
          var tailleLatte = readInt()
          while ( tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Entrée invalide, veuillez entrer '1', '2' ou '3'")
            println("Quelle taille de Latte voulez-vous? \n1) Petit (6g de café et 120 ml de lait) \n2) Moyen (8g de café et 150 ml de lait) \n3) Grand (12 g de café et 200 ml de lait) \n> ")
            tailleLatte = readInt()
          }



          // Vérifier si la quantité d'ingrédients est suffisante pour chaque taille de Latte
          if (tailleLatte == 1) { // Petit Latte
            if (stockCafe >= 6 && stockLait >= 0.12) {
              sucre = 5
              lait = 0.12
              prix = 2.70 // Petit Latte sans sucre
            } else {
              if (stockCafe < 6) {
                println("Il manque du café pour le Petit Latte.")
                suffisanceCafe = false
              }
              if (stockLait < 0.12) {
                println("Il manque du lait pour le Petit Latte.")
                suffisanceLait = false
              }
              continuerClient = false
            }
          } else if (tailleLatte == 2) { // Moyen Latte
            if (stockCafe >= 8 && stockLait >= 0.15) {
              sucre = 10
              lait = 0.15
              prix = 3.20 // Moyen Latte sans sucre
            } else {
              if (stockCafe < 8) {
                println("Il manque du café pour le Moyen Latte.")
                suffisanceCafe = false
              }
              if (stockLait < 0.15) {
                println("Il manque du lait pour le Moyen Latte.")
                suffisanceLait = false
              }
              continuerClient = false
            }
          } else if (tailleLatte == 3) { // Grand Latte
            if (stockCafe >= 12 && stockLait >= 0.2 && stockSucre >= 15) {
              sucre = 15
              lait = 0.2
              prix = 3.70 // Grand Latte sans sucre
            } else {
              if (stockCafe < 12) {
                println("Il manque du café pour le Grand Latte.")
                suffisanceCafe = false
              }
              if (stockLait < 0.2) {
                println("Il manque du lait pour le Grand Latte.")
                suffisanceLait = false
              }
              if (stockSucre < 15) {
                println("Il manque du sucre pour le Grand Latte.")
                suffisanceSucre = false
              }
              continuerClient = false
            }
          } else {
            println("Choix Latte invalide")
            continuerClient = false
          }

          // Calcul du prix avec sucre si les ingrédients sont suffisants
          if (continuerClient) {
            println("Quel quantité de sucre voulez-vous? \n1) Pas de sucre \n2) Peu de sucre (5g) \n3) Moyen (10g) \n4) Beaucoup (15g) \n> ")
            var sucreChoisiLatte = readInt()
             while ( sucreChoisiLatte != 1 && sucreChoisiLatte != 2 && sucreChoisiLatte != 3 && sucreChoisiLatte != 4) {
               println("Entrée invalide, veuillez entrer '1', '2' , '3' ou '4'")
               println("Quel quantité de sucre voulez-vous? \n1) Pas de sucre \n2) Peu de sucre (5g) \n3) Moyen (10g) \n4) Beaucoup (15g) \n> ")
               sucreChoisiLatte = readInt()
          }
            if (sucreChoisiLatte == 1) {
              sucre = 0
            } else if (sucreChoisiLatte == 2) {
              sucre = 5
              prix += 0.10
            } else if (sucreChoisiLatte == 3) {
              sucre = 10
              prix += 0.20
            } else if (sucreChoisiLatte == 4) {
              sucre = 15
              prix += 0.30
            } else {
              println("Choix du sucre invalide.")
            }

            println("Souhaitez-vous ajouter du lait en supplément? \n1) Oui \n2) Non \n>")
              var choixLait = readInt()
                while (choixLait != 1 && choixLait != 2 ) {
                  println("Entrée invalide, veuillez entrer '1' ou '2'")
                  println ("Souhaitez-vous ajouter du lait en supplément? \n1) Oui \n2) Non \n>")
                  choixLait = readInt()
                  }

              if (choixLait == 1) {
                println("Combien de doses de lait ?")
                var laitChoisi = readInt()
                while (laitChoisi != 1 && laitChoisi != 2 && laitChoisi != 3 ) {
                  println("Maximum 3 doses permises")
                println ("Combien de doses de lait ? \n >")
                  laitChoisi = readInt()}


                if (laitChoisi == 0) {
                  lait = 0.0
                } else if (laitChoisi == 1) {
                  lait = 0.05
                } else if (laitChoisi == 2) {
                  lait = 0.1
                } else if (laitChoisi == 3) {
                  lait = 0.15
                } else {



                }
                prix += lait
                stockLait -= laitChoisi * 0.050
                }


            println("Vous avez choisi un Latte de taille "+ tailleLatte + " avec " + sucre + " grammes de sucre et " + lait + " litre de lait.")
            printf("Le prix de votre Latte avec " +sucre+ " grammes de sucre est de %.2f CHF.", prix)


            // Mise à jour des stocks
            if (tailleLatte == 1) {
              stockCafe -= 6
              stockLait -= 0.12
              stockSucre -= sucre
            } else if (tailleLatte == 2) {
              stockCafe -= 8
              stockLait -= 0.15
              stockSucre -= sucre
            } else if (tailleLatte == 3) {
              stockCafe -= 12
              stockLait -= 0.2
              stockSucre -= sucre
            }
          }
        }



        // Vérification de la disponibilité des ingrédients
          if (suffisanceCafe == true  && suffisanceLait == true && suffisanceSucre == true) {
            println("Veuillez payer en utilisant Twint. \nVotre code de paiement est : " + pin + " \nEn attente de validation du paiement...")

            // Simulation de validation de paiement
            Thread.sleep(5000) // Attendre 5 secondes
            println("Merci ! Votre paiement a été accepté")
            println("Préparation de la boisson en cours... \n[...]")




            boissonDesiree = 0 // Réinitialisation
            // Préparation de la boisson
            println("Boisson préparée avec succès!")
            choix = 0

            continuerClient = true
          } else{
        continuerClient = true
          }

      }
      if (choix == 2) { // Mode Admin
        println("Entrez le code Pin pour accéder à l'administration.")
        val pin = readLine()

        // Vérification du code pin
        if (pin == "434343") {
          println("Le code pin est correct. Accès à l'admin accepté.")
          println("Stocks :")
          println("Poudre de café : " +stockCafe + "g")
          printf("Lait : %.3f L", stockLait )
          println("\nSucre : " + stockSucre + "g")



          // Demander quelle action doit être effectuée parmi la gestion des stocks
          println("Que souhaitez-vous faire ? \n1) Réapprovisionner le café \n2) Réapprovisionner le sucre \n3) Réapprovisionner le lait \n4) Retour au menu principal \n>")
          var actionAdmin = readInt()
          while ( actionAdmin != 1 && actionAdmin != 2 && actionAdmin != 3 && actionAdmin != 4) {
               println("Entrée invalide, veuillez entrer '1', '2' , '3' ou '4'")
                println("Que souhaitez-vous faire ? \n1) Réapprovisionner le café \n2) Réapprovisionner le sucre \n3) Réapprovisionner le lait \n4) Retour au menu principal \n>")
               actionAdmin = readInt()
          }


          // Réapprovisionnement des stocks
          if (actionAdmin == 1) {
            println("Entrez la quantité de café à rajouter dans le stock (en grammes) :\n>")
            val quantiteCafe = readDouble()
            stockCafe += quantiteCafe
            println("Stock de café réapprovisionné. Il reste désormais " + stockCafe + " grammes de café disponibles.")
          } else if (actionAdmin == 2) {
            println("Entrez la quantité de sucre à rajouter dans le stock (en grammes) :\n>")
            val quantiteSucre = readDouble()
            stockSucre += quantiteSucre
            println("Stock de sucre réapprovisionné. Il reste désormais " + stockSucre + " grammes de sucre disponibles.")
          } else if (actionAdmin == 3) {
            println("Entrez la quantité de lait à rajouter dans le stock (en litres) :\n>")
            val quantiteLait = readDouble()
            stockLait += quantiteLait
            println("Stock de lait réapprovisionné. Il reste désormais " + stockLait + " litres de lait disponibles.")
          } else if (actionAdmin == 4) {
            println("Retour au menu principal")
          } else {
            println("Choix invalide, retour au menu principal.")
          }
        } else {
          println("Code pin incorrect. Accès à l'admin refusé.")
        }

      } else if (choix == 3) { // Mode Quitter
        println("Merci et à bientôt!")
        continuerClient = false

      }
    }

 }}

