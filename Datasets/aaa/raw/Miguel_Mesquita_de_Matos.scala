import io.StdIn._



object Main {
  def main(args: Array[String]): Unit = {

    println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n > ")
    var mode = readInt()
    while ( mode < 1 || mode > 3 ){
      println(" vous devez choisir un mode entre 1 et 3 ")
      println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n >")

      mode = readInt()
    }


    var lait = 0
    var stock_poudredecafe: Int = 50 // en gramme
    var stock_sucre: Int = 30 // en gramme
    var stock_lait: Int = 50 // en décilitre

    while (mode != 3) {
      var prix_final: Double = 0.0
      var prix_boisson: Double = 0.0
      var taille: Int = 0
      var sucre: Double = 0.0
      var prix_sucre: Double = 0.00
      var lait_suplementaire: Int = 0
      var cout_lait_suplementaire: Double = 0.00
      val code: Int = 434343


      if (mode == 1) {
        var commande_reussie = false
        while (!commande_reussie) {

          println(" veuillez choisir la boisson que vous désirez  \n 1) pour expresso (2CHF)      \n 2) pour capuccino  (2.5CHF)   \n 3) pour latte, petit (2.7CHF), moyen ( 3.2CHF), grand( 3.7CHF) \n > ")
          var client = readInt()
          while (client < 1 || client > 3) {
            println(" vous devez choisir un nombre entre 1 et 3 ")
            client = readInt()
          }
          sucre = readLine(" souhaitez vous ajouter du sucre  \n 1) pour non   \n 2) pour  peu (0.1 chf)  \n 3) pour moyen ( 0.2 chf) \n 4) pour beaucoup  ( 0.3 chf) \n > ").toInt
          while (sucre < 1 || sucre > 4) {
            println(" vous devez choisir un nombre entre 1 et 4 ")
            sucre = readInt()
          }
          if (client == 1) {
            println(" vous ne pouvez pas ajouter de lait dans votre boisson ")
          } else if (client == 2 || client == 3) {
            lait = readLine("souhaitez-vous ajouter du lait \n 1) non, \n 2) oui\n > ").toInt
            while (lait < 1 || lait > 2) {
              println(" vous devez choisir un nombre entre 1 et 2 ")
              lait = readInt()
            }
          }


          if (client == 1 && stock_poudredecafe < 8) {
            println("Erreur: stock de poudre de café insuffisant, veuiller choisir une autre boisson ou vérifier les stocks dans l'admin  ")
            commande_reussie = false
            lait = 0
            prix_final = 0.0
            prix_boisson = 0.0
            taille = 0
            sucre = 0.0
            prix_sucre = 0.00
            lait_suplementaire = 0
            cout_lait_suplementaire = 0.00
          } else if (client == 1 && stock_poudredecafe >= 8) {
            println(" vous avez choisi l'expresso ")
            stock_poudredecafe = stock_poudredecafe - 8
            commande_reussie = true
            prix_boisson = 2.00

          }
          if (client == 2) {
            if (stock_poudredecafe < 6) {
              println("Erreur : quantité insuffisante de poudres de cafés, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin ")
              commande_reussie = false
              lait = 0
              prix_final = 0.0
              prix_boisson = 0.0
              taille = 0
              sucre = 0.0
              prix_sucre = 0.00
              lait_suplementaire = 0
              cout_lait_suplementaire = 0.00
            } else if (stock_lait < 10) {
              println(" Erreur : quantité insuffisante de lait, veuilez choisir une autre boisson ou vérifier les stocks dans l'admin ")
              commande_reussie = false
              lait = 0
              prix_final = 0.0
              prix_boisson = 0.0
              taille = 0
              sucre = 0.0
              prix_sucre = 0.00
              lait_suplementaire = 0
              cout_lait_suplementaire = 0.00
            } else  if ( stock_poudredecafe >= 6 && stock_lait >= 10 )  {
              stock_poudredecafe = stock_poudredecafe - 6
              stock_lait = stock_lait - 10
              println(" vous avez choisi le cappuccino ")
              commande_reussie = true
              prix_boisson = 2.50
            }
          } else if (client == 3) {
            taille = readLine(" choisissez la taille de votre latte  \n 1) pour petit (2.7 CHF )   \n 2) pour moyen  (3.2CHF)  \n 3) pour grand ( 3.7CHF) ) \n > ").toInt
            while (taille < 1 || taille > 3) {
              println(" vous devez choisir un nombre entre 1 et 3 ")
              taille = readInt()
            }


            if (taille == 1) {
              if (stock_poudredecafe < 6) {
                println("Erreur : quantité insuffisante de poudres de cafés, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00
              } else if (stock_lait < 12) {
                println("Erreur : quantité insuffisante de lait, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin.")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00
              } else {
                stock_poudredecafe = stock_poudredecafe - 6
                stock_lait = stock_lait - 12
                println("Vous avez choisi le latte petit.")
                commande_reussie = true
                prix_boisson = 2.70
              }
            } else if (taille == 2) {
              if (stock_poudredecafe < 8) {
                println("Erreur : quantité insuffisante de poudres de cafés, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin.")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00
              } else if (stock_lait < 15) {
                println("Erreur : quantité insuffisante de lait, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00
              } else {
                stock_poudredecafe = stock_poudredecafe - 8
                stock_lait = stock_lait - 15
                println("Vous avez choisi le latte moyen.")
                commande_reussie = true
                prix_boisson = 3.20
              }
            } else if (taille == 3) {
              if (stock_poudredecafe < 12) {
                println("Erreur : quantité insuffisante de poudres de cafés, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00

              } else if (stock_lait < 20) {
                println("Erreur : quantité insuffisante de lait, veuillez choisir une autre boisson ou vérifier les stocks dans l'admin")
                commande_reussie = false
                lait = 0
                prix_final = 0.0
                prix_boisson = 0.0
                taille = 0
                sucre = 0.0
                prix_sucre = 0.00
                lait_suplementaire = 0
                cout_lait_suplementaire = 0.00
              } else {
                stock_poudredecafe = stock_poudredecafe - 12
                stock_lait = stock_lait - 20
                println("Vous avez choisi le latte grand.")
                commande_reussie = true
                prix_boisson = 3.70
              }
            } else {
              println("Erreur : taille non valide. Veuillez sélectionner une taille correcte.")
              commande_reussie = false
              lait = 0
              prix_final = 0.0
              prix_boisson = 0.0
              taille = 0
              sucre = 0.0
              prix_sucre = 0.00
              lait_suplementaire = 0
              cout_lait_suplementaire = 0.00

            }
          }


          if (sucre == 1) {
            println(" sucre = pas de sucre ")
            commande_reussie = true
          }
          if (sucre == 2 && stock_sucre >= 5) {
            println(" sucre = peu de sucre ")
            stock_sucre = stock_sucre - 5
            prix_sucre = 0.10
            commande_reussie = true

          } else if (sucre == 2 && stock_sucre < 5) {
            println(" Erreur: quantité de sucre insuffisante ")
            commande_reussie = false
            lait = 0
            prix_final = 0.0
            prix_boisson = 0.0
            taille = 0
            sucre = 0.0
            prix_sucre = 0.00
            lait_suplementaire = 0
            cout_lait_suplementaire = 0.00

          } else if (sucre == 3 && stock_sucre >= 10) {
            println(" sucre = moyen  de sucre ")
            stock_sucre = stock_sucre - 10
            prix_sucre = 0.20
            commande_reussie = true

          } else if (sucre == 3 && stock_sucre < 10) {
            println("Erreur: quantité de sucre insuffisante ")
            commande_reussie = false
            lait = 0
            prix_final = 0.0
            prix_boisson = 0.0
            taille = 0
            sucre = 0.0
            prix_sucre = 0.00
            lait_suplementaire = 0
            cout_lait_suplementaire = 0.00
          } else if (sucre == 4 && stock_sucre >= 15) {
            println(" sucre = beaucoup de sucre ")
            stock_sucre = stock_sucre - 15
            prix_sucre = 0.30
            commande_reussie = true
          } else if (sucre == 4 && stock_sucre < 15) {
            println("Erreur: quantité de sucre insuffisante ")
            commande_reussie = false
            lait = 0
            prix_final = 0.0
            prix_boisson = 0.0
            taille = 0
            sucre = 0.0
            prix_sucre = 0.00
            lait_suplementaire = 0
            cout_lait_suplementaire = 0.00
          }


          if (lait == 2 && (client == 2 || client == 3)) {
            lait_suplementaire = readLine(" combien de doses de lait souhaitez-vous (max 3, 0.05 CHF/dose)? \n > ").toInt
            while (lait_suplementaire > 3 || lait_suplementaire < 1) {
              println(" vous devez choisir entre 1 a 3 doses maximums")
              lait_suplementaire = readInt()
            }

            if ((stock_lait >= 5 && lait_suplementaire == 1) || (stock_lait >= 10 && lait_suplementaire == 2) || (stock_lait >= 15 && lait_suplementaire == 3)) {
              if (lait_suplementaire == 1) {
                stock_lait = stock_lait - 5
                cout_lait_suplementaire = 0.05
                println("1 dose de lait ajoutée.")
                commande_reussie = true
              } else if (lait_suplementaire == 2) {
                stock_lait = stock_lait - 10
                cout_lait_suplementaire = 0.10
                commande_reussie = true

                println("2 doses de lait ajoutées.")
              } else if (lait_suplementaire == 3) {
                stock_lait = stock_lait - 15
                cout_lait_suplementaire = 0.15
                println("3 doses de lait ajoutées.")
                commande_reussie = true
              }
            } else {
              println("Erreur : stock insuffisant pour ajouter des dose(s) de lait.")
              commande_reussie = false
              lait = 0
              prix_final = 0.0
              prix_boisson = 0.0
              taille = 0
              sucre = 0.0
              prix_sucre = 0.00
              lait_suplementaire = 0
              cout_lait_suplementaire = 0.00

            }
          }
          if (commande_reussie == false) {

            lait = 0
            prix_final = 0.0
            prix_boisson = 0.0
            taille = 0
            sucre = 0.0
            prix_sucre = 0.00
            lait_suplementaire = 0
            cout_lait_suplementaire = 0.00
          }

          if (prix_boisson >= 2.00 || cout_lait_suplementaire > 0.00 || prix_sucre > 0.00) {
            prix_final = prix_boisson + cout_lait_suplementaire + prix_sucre

          if (prix_boisson >= 2.00 && cout_lait_suplementaire > 0.00 && prix_sucre > 0.00) {
            printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson) + %.2f CHF (sucre) + %.2f CHF ( lait)\n  ", prix_final, prix_boisson, prix_sucre, cout_lait_suplementaire)
          }
          if (prix_boisson >= 2.00 && cout_lait_suplementaire == 0 && prix_sucre > 0) {
            printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson) + %.2f CHF (sucre) \n ", prix_final, prix_boisson, prix_sucre)
          }
          if (prix_boisson >= 2.00 && cout_lait_suplementaire > 0 && prix_sucre == 0) {
            printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson) + %.2f CHF (lait) \n ", prix_final, prix_boisson, cout_lait_suplementaire)
          }
          if (prix_boisson >= 2.00 && cout_lait_suplementaire == 0 && prix_sucre == 0) {
            printf(" le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  \n ", prix_final, prix_boisson)
          }
          println("\n veuillez procédez au paiement par twint ")
          import scala.util.Random

          val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var code_twint = ""

          for (i <- 1 to 5) {
            val index_aléatoire = Random.nextInt(chars.length)
            code_twint = code_twint + chars(index_aléatoire)
          }


          println("Votre code twint  est : " + code_twint)
          println("(en attente de la confirmation de votre paiement) \n  Merci ! votre paiement est confirmé ")
          println(" préparation de votre boisson...")
          Thread.sleep(3000)// Attend pendant 3 seconde
          if (client == 1) {
            println(" votre expresso est prêt")
          } else if (client == 2) {
            println(" votre capucinno est prêt")
          } else if (client == 3)
            println(" votre Latte est prêt")


          println("Merci d'avoir utilisé la machine Nospresso. À bientôt !")
        }





        }

        println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n > ")
        mode = readInt()
        while ( mode > 3 || mode <1 ){
          println(" vous devez choisir un mode entre 1 et 3 ")
          println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n >")
          mode = readInt()
        }
      } else if (mode == 2) {

        var pin = readLine(" inscrivez votre code d'admin ").toInt
        if (code == pin) {
          println(" votre stock de lait est égal " + stock_lait)
          println(" votre stock de poudre de café est égal " + stock_poudredecafe)
          println(" votre stock de sucre est égal " + stock_sucre)
          println(" combien de lait souhaitez-vous ajouter au stock \n > ")
          var ajout_lait = readInt()
          while (ajout_lait < 0) {
            println(" vous pouvez mettre des stocks positifs ")
            ajout_lait = readInt()
          }
          stock_lait = stock_lait + ajout_lait
          println(" stock lait mis à jour est égal à " + stock_lait)
          println(" combien de poudre de café souhaitez-vous ajouter au stock \n >  ")
          var ajout_poudredecafé = readInt()
          while (ajout_poudredecafé  < 0) {
            println(" vous pouvez mettre des stocks positifs ")
            ajout_poudredecafé = readInt()
          }
          stock_poudredecafe = stock_poudredecafe + ajout_poudredecafé
          println(" stock poudre de café mis à jour est égal à " + stock_poudredecafe)
          println(" combien de sucre souhaitez-vous ajouter au stock \n >  ")
          var ajout_sucre = readInt()
          while (ajout_sucre < 0) {
            println(" vous pouvez mettre des stocks positifs ")
            ajout_sucre = readInt()
          }
          stock_sucre =  stock_sucre + ajout_sucre
          println(" stock sucre mis à jour est égal à " + stock_sucre)
        } else {
          println(" code incorrect, retour au menu principal ")
        }
        println("\n veuillez choisir votre mode \n 1) client \n 2) admin \n 3) quitter \n > ")
        mode = readInt()
        while ( mode > 3 || mode < 1 ){
          println (" vous devez choisir un mode entre 1 et 3 ")
          mode = readInt()
        }
      }

    }

  }
}




