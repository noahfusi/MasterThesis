import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Variables
    var demarrer = true
    var choix_sucre = 0
    var taille_latte = 0
    var supplement_lait = 0
    var dose_lait_supp = 0

    // Stocks initiaux
    var poudre_cafe = 50
    var sucre = 30
    var lait = 500

    // Qunatités nécessaires
    var poudre_necessaire = 0
    var lait_necessaire = 0
    var sucre_necessaire = 0

    // Prix boissons
    val expresso = 2.00
    val cappuccino = 2.50
    val latte_petit = 2.70
    val latte_moyen = 3.20
    val latte_grand = 3.70

    // Prix Sucre :
    val peu_sucre = 0.10
    val moyen_sucre = 0.200
    val bcp_sucre = 0.30
    var prix_total = 0.00

    // Boucle séléction mode
    while (demarrer){
      println(" Nospresso Café ")
      println(" Veuillez sélectionner votre mode : ")
      println(" 1) Client ")
      println(" 2) Admin ")
      println(" 3) Quitter ")
      print(" > ")

      val mode = readLine().toInt
      println(mode + " mode choisi")

      // mode invalide
      if (mode != 1 && mode != 2 && mode != 3) {
        println("Veuillez sélectionner un mode : 1) Client, 2) Admin ou 3) Quitter ")
      }

      // mode client
      else if (mode == 1){
        var demarrer_client = true

        while (demarrer_client) {
          println(" Veuillez sélectionner votre boisson : ")
          println(" 1) Expresso - CHF 2.00 ")
          println(" 2) Cappuccino - CHF 2.50 ")
          println(" 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
          print(" > ")

          val boisson = readLine().toInt
          //println(boisson + " boisson choisi ")

          // choix boisson invalide
          if (boisson != 1 && boisson != 2 && boisson != 3) {
            println(" Veuillez sélectionner une boisson valide. ")
          }

          //choix boisson
          else if (boisson == 1 || boisson == 2 || boisson == 3) {

            // Ingredients nécessaires
            // expresso
            if (boisson == 1) {
              poudre_necessaire = 8
              lait_necessaire = 0
              //println(poudre_necessaire + " poudre necessaire pour expresso")
              poudre_cafe -= poudre_necessaire
              //println(poudre_cafe + " poudre restant ")
            }
            // cappuccino
            else if (boisson == 2) {
              poudre_necessaire = 6
              lait_necessaire = 100
              //println(poudre_necessaire + " poudre necessaire et " + lait_necessaire + " lait necessaire pour cappucino")
              poudre_cafe -= poudre_necessaire
              lait -= lait_necessaire
              //println(poudre_cafe + " poudre " + lait + " restants ")
            }
            // latte
            else if (boisson == 3) {
              println(" Veuillez sélectionner la taille : ")
              println(" 1) Petit ")
              println(" 2) Moyen ")
              println(" 3) Grand")
              print(" > ")

              // taille latte
              taille_latte = readInt()
              if (taille_latte == 1) {
                poudre_necessaire = 6
                lait_necessaire = 120
                //println(poudre_necessaire + " poudre necessaire et " + lait_necessaire + " lait necessaire pour latte petit")
                poudre_cafe -= poudre_necessaire
                lait -= lait_necessaire
                //println(poudre_cafe + " poudre " + lait + " restants ")
              } else if (taille_latte == 2) {
                poudre_necessaire = 8
                lait_necessaire = 150
                //println(poudre_necessaire + " poudre necessaire et " + lait_necessaire + " lait necessaire pour latte moyen")
                poudre_cafe -= poudre_necessaire
                lait -= lait_necessaire
                //println(poudre_cafe + " poudre " + lait + " restants ")
              } else if (taille_latte == 3) {
                poudre_necessaire = 12
                lait_necessaire = 200
                //println(poudre_necessaire + " poudre necessaire et " + lait_necessaire + " lait necessaire pour latte grand")
                poudre_cafe -= poudre_necessaire
                lait -= lait_necessaire
                //println(poudre_cafe + " poudre " + lait + " restants ")
              }
            }

            // Sélection sucre :
            println(" Souhaitez-vous ajouter du sucre ? ")
            println(" 1) Sans sucre ")
            println(" 2) Peu (5g) - CHF 0.10 ")
            println(" 3) Moyen (10g) - CHF 0.20 ")
            println(" 4) Beaucoup (15g) - CHF 0.30 ")
            print(" > ")
            choix_sucre = readInt()
            //println(choix_sucre + " choix sucre")

            if (choix_sucre == 2) {
              sucre_necessaire = 5
              //println(sucre_necessaire + " sucre necessaire ")
              sucre -= sucre_necessaire
              //println(sucre + " sucre restant ")
            } else if (choix_sucre == 3) {
              sucre_necessaire = 10
              //println(sucre_necessaire + " sucre necessaire ")
              sucre -= sucre_necessaire
              //println(sucre + " sucre restant ")
            } else if (choix_sucre == 4) {
              sucre_necessaire = 15
              //println(sucre_necessaire + " sucre necessaire ")
              sucre -= sucre_necessaire
              //println(sucre + " sucre restant ")
            }

            // Sélection lait supplementaire
            if (boisson == 2 || boisson == 3 || taille_latte == 1 || taille_latte == 2 || taille_latte == 3) {
              println(" Souhaitez-vous ajouter du lait en supplément ? ")
              println(" 1) Oui ")
              println(" 2) Non ")
              print(" > ")
              supplement_lait = readInt()

              // Supplement lait OUI :
              if (supplement_lait == 1) {
                do {
                  println(" Combien de dose(s) ? (3 doses maximales par boisson) ")
                  println(" > ")
                  dose_lait_supp = readInt()
                  if (dose_lait_supp >= 1 && dose_lait_supp <= 3) {
                    println((dose_lait_supp * 50) + " ml supplementaires")
                    supplement_lait = dose_lait_supp
                  } else {
                    println(" Erreur : 3 doses maximales par boisson ! ")
                  }
                } while (dose_lait_supp < 1 || dose_lait_supp > 3)
                lait_necessaire += dose_lait_supp * 50
                // Supplement lait NON :
              } else if (supplement_lait == 2) {
                //println(" Pas de dose de lait supplementaire ")
              }
            }

            // Vérification stock
            if (poudre_cafe >= poudre_necessaire && lait >= lait_necessaire && sucre >= sucre_necessaire) {
              poudre_cafe -= poudre_necessaire
              lait -= lait_necessaire
              sucre -= sucre_necessaire
              //preparation = true
              //println(poudre_cafe + " poudre " + lait + " lait " + sucre + " sucre restants ")

              // Print choix boisson :
              if (boisson == 1) {
                println(" Boisson sélectionnée : Expresso ")
              } else if (boisson == 2) {
                println(" Boisson sélectionnée : Cappuccino ")
              } else if (boisson == 3) {
                if (taille_latte == 1) {
                  println(" Boisson sélectionnée : Latte (Petit) ")
                } else if (taille_latte == 2) {
                  println(" Boisson sélectionnée : Latte (Moyen) ")
                } else if (taille_latte == 3) {
                  println(" Boisson sélectionnée : Latte (Grand) ")
                }
              }

              // Print choix sucre :
              if (choix_sucre == 1) {
                println(" Niveau de sucre : Sans sucre ")
              } else if (choix_sucre == 2) {
                println(" Niveau de sucre : Peu (5g) ")
              } else if (choix_sucre == 3) {
                println(" Niveau de sucre : Moyen (10g) ")
              } else if (choix_sucre == 4) {
                println(" Niveau de sucre : Beaucoup (15g) ")
              }

              // Print supplement lait :
              if (supplement_lait == 1) {
                println(" Lait supplémentaire : Oui")
                println(dose_lait_supp + " dose(s) supplémentaire(s) ")
              } else if (supplement_lait == 2) {
                println(" Lait supplémentaire : Non ")
              }

              // Pri total expresso
              if (boisson == 1 && choix_sucre == 1) {
                printf(" Prix total : CHF %.2f \n ", expresso)
              } else if (boisson == 1 && choix_sucre == 2) {
                prix_total = expresso + peu_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", expresso, peu_sucre, prix_total)
              } else if (boisson == 1 && choix_sucre == 3) {
                prix_total = expresso + moyen_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, moyen_sucre, prix_total)
              } else if (boisson == 1 && choix_sucre == 4) {
                prix_total = expresso + bcp_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, bcp_sucre, prix_total)
              }

              // Prix total cappuccino
              if (boisson == 2 && choix_sucre == 1) {
                printf(" Prix total : CHF %.2f \n", cappuccino)
              } else if (boisson == 2 && choix_sucre == 2) {
                prix_total = cappuccino + peu_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", cappuccino, peu_sucre, prix_total)
              } else if (boisson == 2 && choix_sucre == 3) {
                prix_total = cappuccino + moyen_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, moyen_sucre, prix_total)
              } else if (boisson == 2 && choix_sucre == 4) {
                prix_total = cappuccino + bcp_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", cappuccino, bcp_sucre, prix_total)
              }

              // Prix total latte
              // Latte Petit
              if (boisson == 3 && taille_latte == 1 && choix_sucre == 1) {
                printf(" Prix total : CHF %.2f \n", latte_petit)
              } else if (boisson == 3 && taille_latte == 1 && choix_sucre == 2) {
                prix_total = latte_petit + peu_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_petit, peu_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 1 && choix_sucre == 3) {
                prix_total = latte_petit + moyen_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_petit, moyen_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 1 && choix_sucre == 4) {
                prix_total = latte_petit + bcp_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_petit, bcp_sucre, prix_total)
              }
              // Latte Moyen
              if (boisson == 3 && taille_latte == 2 && choix_sucre == 1) {
                printf(" Prix total : CHF %.2f \n", latte_moyen)
              } else if (boisson == 3 && taille_latte == 2 && choix_sucre == 2) {
                prix_total = latte_moyen + peu_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_moyen, peu_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 2 && choix_sucre == 3) {
                prix_total = latte_moyen + moyen_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_moyen, moyen_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 2 && choix_sucre == 4) {
                prix_total = latte_moyen + bcp_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_moyen, bcp_sucre, prix_total)
              }

              // Latte Grand
              if (boisson == 3 && taille_latte == 3 && choix_sucre == 1) {
                printf(" Prix total : CHF %.2f \n", latte_grand)
              } else if (boisson == 3 && taille_latte == 3 && choix_sucre == 2) {
                prix_total = latte_grand + peu_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_grand, peu_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 3 && choix_sucre == 3) {
                prix_total = latte_grand + moyen_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_grand, moyen_sucre, prix_total)
              } else if (boisson == 3 && taille_latte == 3 && choix_sucre == 4) {
                prix_total = latte_grand + bcp_sucre
                printf(" Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latte_grand, bcp_sucre, prix_total)
              }

              // Interface paiement
              var code = Random.alphanumeric.take(5).mkString.toUpperCase
              Thread.sleep(1000)
              println(" Veuillez payer en utilisant Twint. ")
              println(" Votre code de paiement est : " + code)
              println(" (En attente de paiement ...) ")
              Thread.sleep(3000)
              println(" Paiement confirmé")

              // Préparation boisson
              println(" Pré́paration de votre boisson...")

              if (boisson == 1) {
                println(" Votre Expresso est prêt ! Bonne dégustation ! \n ")
                demarrer_client = false
              } else if (boisson == 2) {
                println(" Votre Cappuccino est prêt ! Bonne dégustation ! \n ")
                demarrer_client = false
              } else if (boisson == 3) {
                println(" Votre Latte est prêt ! Bonne dégustation ! \n ")
                demarrer_client = false
              }
            } else if (poudre_cafe < poudre_necessaire) {
              println(" Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
              println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            } else if (lait < lait_necessaire) {
              println(" Erreur : Quantité de poudre de lait insuffisante pour préparer la boisson sélectionnée. ")
              println(" Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            } else if (sucre < sucre_necessaire) {
              println(" Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. ")
              println("  Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          }
        }
      }
      // mode admin
      else if (mode == 2){
        var demarrer_admin = true
        while (demarrer_admin){
          val PIN = 434343
          var saisie_pin = 0

          println(" Mode Admin ")
          println(" Entrez le code PIN : ")

          saisie_pin = readInt()

          // PIN invalide
          if (saisie_pin != PIN) {
            println(" Pin incorrect. ")
            while (saisie_pin != PIN) {
              println(" Code Pin incorrect. Accès non autorisé. ")
              println(" Entrez le code PIN : ")
              saisie_pin = readInt()
            }
          }
          // PIN valide
          else if (saisie_pin == PIN) {
            // variables
            var ajout_cafe = 0
            var ajout_lait = 0
            var ajout_sucre = 0

            println(" Accès autorisé ")
            println(" ")

            // Print stocks
            println(" Stocks : ")
            println(" Poudre de café : " + poudre_cafe + "g")
            println(" Lait : " + lait + "ml")
            println(" Sucre : " + sucre + "g")


            // Ajout des quantités
            println(" Combien de g de café vouliez-vous ajouter ? ")
            ajout_cafe = readInt()
            println(" Combien de ml de lait vouliez-vous ajouter ? ")
            ajout_lait = readInt()
            println(" Combien de g de sucre vouliez-vous ajouter ? ")
            ajout_sucre = readInt()

            // Reapprovisionnement
            println(" Réapprovisionnement des stocks...")
            poudre_cafe += ajout_cafe
            lait += ajout_lait
            sucre += ajout_sucre
            println(" Ajout : ")
            println(" Poudre de café : " + poudre_cafe + "g ")
            println(" Lait : " + lait + "mg ")
            println(" Sucre : " + sucre + "g ")
            println(" ")

            println(" Niveaux de stock mis à jour.")
            println(" Retour au menu principal... \n ")
            demarrer_admin = false
          }
        }
      }
      // mode quitter
      else if (mode == 3) {
        println(" Fin du programme. ")
        demarrer = false
      }
    }
  }
}