import io.StdIn._
import math._
import util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var assez_lait = false
    var assez_sucre = false
    //Stock
    var stock_cafe = 50 //g
    var stock_sucre = 30 //g
    var stock_lait = 0.5 //L
    //Conso
    val conso_cafe_expresso = 8
    val conso_cafe_cappuccino = 6
    val conso_cafe_latte_p = 6
    val conso_cafe_latte_m = 8
    val conso_cafe_latte_g = 12
    val conso_lait_cappuccino = 0.10
    val conso_lait_latte_petit = 0.12
    val conso_lait_latte_moyen = 0.15
    val conso_lait_latte_grand = 0.2
    val conso_sucre_p = 5
    val conso_sucre_m = 10
    val conso_sucre_b = 15
    // prix
    val prix_expresso = 2.00
    val prix_cappuccino = 2.50
    val prix_latte_petit = 2.70
    val prix_latte_moyen = 3.20
    val prix_latte_grand = 3.70
    val prix_sucre_p = 0.10
    val prix_sucre_m = 0.20
    val prix_sucre_b = 0.30
    var quitter = false // mettre à true dans la boucle pour arreter le programme

    while (!quitter) {
      //Menu d'accueil
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choix = readLine("> ").toInt

      //vérif des données valides
      while (!(choix == 1 || choix == 2 || choix == 3)) {
        print("Votre sélection n'est pas correcte, choisissez entre 1) Client 2) Admin 3) Quitter ")
        choix = readLine("> ").toInt
      }

      var recap_prix_cafe = 0.0
      var recap_prix_sucre = 0.0
      var recap_prix_lait = 0.0
      var erreur =true
      // mode client séléctionné
      if (choix == 1) {
        while (erreur) {
          var prix_total = 0.0

          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          var choix_client = readLine("> ").toInt

          //vérif des données valides
          while (!(choix_client == 1 || choix_client == 2 || choix_client == 3)) {
            print("Votre sélection n'est pas correcte, choisissez entre 1) Expresso - CHF 2.00  2) Cappuccino - CHF 2.50  3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
            choix_client = readLine("> ").toInt
          }
          //faire 3 variables recap: café, sucre et lait sans string pour pouvoir utiliser %.2f
          // Verif stocks, prix et choix de la boisson

          var choix_latte_taille = 0
          var boisson_reussie = false

          if (choix_client == 1) {
            if (stock_cafe >= conso_cafe_expresso) {
              boisson_reussie = true
              assez_lait = true
              erreur=false
            }
            else {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println()
              erreur = true
            }

          } else if (choix_client == 2) {
            if ((stock_cafe >= conso_cafe_cappuccino) && (stock_lait >= conso_lait_cappuccino)) {
              boisson_reussie = true
              erreur=false
            }
            else if (stock_cafe < conso_cafe_cappuccino) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println()
              erreur = true
            }
            else if (stock_lait < conso_lait_cappuccino) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println()
              erreur = true
            }
          }
          else if (choix_client == 3) {
            println("Quelle taille de latté souhaitez-vous ? ")
            println("1) Petit - CHF 2.70 ")
            println("2) Moyen - CHF 3.20 ")
            println("3) Grand - CHF 3.70 ")
            choix_latte_taille = readLine("> ").toInt
            //vérif des données valides
            while (!(choix_latte_taille == 1 || choix_latte_taille == 2 || choix_latte_taille == 3)) {
              print("Votre sélection n'est pas correcte, choisissez entre 1) Petit - CHF 2.70  2) Moyen - CHF 3.20  3) Grand - CHF 3.70")
              choix_latte_taille = readLine("> ").toInt
            }
            if (choix_latte_taille == 1) {
              if ((stock_cafe >= conso_cafe_latte_p) && (stock_lait >= conso_lait_latte_petit)) {
                boisson_reussie = true
                erreur=false
              }

              else if ((stock_cafe < conso_cafe_latte_p) && (stock_lait >= conso_lait_latte_petit)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe >= conso_cafe_latte_p) && (stock_lait < conso_lait_latte_petit)) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe < conso_cafe_latte_p) && (stock_lait < conso_lait_latte_petit)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
            }

            else if (choix_latte_taille == 2) {
              if ((stock_cafe >= conso_cafe_latte_m) && (stock_lait >= conso_lait_latte_moyen)) {
                boisson_reussie = true
                erreur=false

              }
              else if ((stock_cafe < conso_cafe_latte_m) && (stock_lait >= conso_lait_latte_moyen)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe >= conso_cafe_latte_m) && (stock_lait < conso_lait_latte_moyen)) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre taille, une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe < conso_cafe_latte_m) && (stock_lait < conso_lait_latte_moyen)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre taille, une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
            }

            else if (choix_latte_taille == 3) {
              if ((stock_cafe >= conso_cafe_latte_g) && (stock_lait >= conso_lait_latte_grand)) {
                boisson_reussie = true
                erreur=false
              }
              else if ((stock_cafe < conso_cafe_latte_g) && (stock_lait >= conso_lait_latte_grand)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe >= conso_cafe_latte_g) && (stock_lait < conso_lait_latte_grand)) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre taille, une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
              else if ((stock_cafe < conso_cafe_latte_g) && (stock_lait < conso_lait_latte_grand)) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir une autre taille, une autre boisson ou vérifier les stocks en mode Admin.")
                println()
                erreur = true
              }
            }
          }

          // sucre?
          if (boisson_reussie) {
            println("Souhaitez-vous du sucre ? ")
            println("1) Sans sucre ")
            println("2) Peu (5g) - CHF 0.10 ")
            println("3) Moyen (10g) - CHF 0.20 ")
            println("4) Beaucoup (15g) - CHF 0.30")
            var choix_client_sucre = readLine("> ").toInt

            //vérif des données valides
            while (!(choix_client_sucre == 1 || choix_client_sucre == 2 || choix_client_sucre == 3 || choix_client_sucre == 4)) {
              print("Votre sélection n'est pas correcte, choisissez entre 1)  Sans sucre  2) Peu (5g) - CHF 0.10  3) Moyen (10g) - CHF 0.20  4) Beaucoup (15g) - CHF 0.30 ")
              choix_client_sucre = readLine("> ").toInt
            }

            // vérif stock sucre
            if (choix_client_sucre == 1) {
              assez_sucre = true
            }
            //peu
            if (choix_client_sucre == 2) {
              if (stock_sucre < conso_sucre_p) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir sans sucre ou vérifier les stocks en mode Admin.")
                println()
                assez_sucre = false
                erreur = true
              }
              else {
                assez_sucre = true
                erreur=false
              }
            }
            //moyen
            else if (choix_client_sucre == 3) {
              if (stock_sucre < conso_sucre_m) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir moins de sucre, sans ou vérifier les stocks en mode Admin.")
                println()
                assez_sucre = false
                erreur = true
              }
              else {
                assez_sucre = true
                erreur=false
              }
            }
            //beaucoup
            else if (choix_client_sucre == 4) {
              if (stock_sucre < conso_sucre_b) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. ")
                println("Veuillez choisir moins de sucre, sans ou vérifier les stocks en mode Admin.")
                println()
                assez_sucre = false
                erreur = true
              }
              else {
                assez_sucre = true
                erreur=false
              }
            }

            var choix_client_lait = 0

            //lait supp?
            if (boisson_reussie && assez_sucre) {
              if ((choix_client == 2) || (choix_client == 3)) {
                println("Souhaitez-vous ajouter du lait en supplément ? ")
                println("1) Oui ")
                println("2) Non ")
                choix_client_lait = readLine("> ").toInt

                //vérif des données valides
                while (!(choix_client_lait == 1 || choix_client_lait == 2)) {
                  print("Votre sélection n'est pas correcte, choisissez entre 1) Oui  2) Non ")
                  choix_client_lait = readLine("> ").toInt
                }


                //cbn doses
                if (choix_client_lait == 1) {
                  println("Combien de dose(s) ? ")
                  var dose_lait_choix = readLine(" > ").toInt
                  //verif données valides
                  while (!(dose_lait_choix == 1 || dose_lait_choix == 2 || dose_lait_choix == 3)) {
                    print("Votre sélection n'est pas correcte, choisissez entre 1, 2 ou 3 doses. ")
                    dose_lait_choix = readLine(" > ").toInt
                  }

                  if (dose_lait_choix == 1) {
                    if (stock_lait < 0.05) {
                      println("Erreur : Quantité de lait insuffisante pour préparer 'la boisson sélectionnée. ")
                      println("Veuillez choisir sans lait supplémentaire ou vérifier les stocks en mode Admin.")
                      println()
                      assez_lait = false
                      erreur = true
                    }
                    else {
                      stock_lait -= 0.05
                      prix_total += 0.05
                      recap_prix_lait += 0.05
                      assez_lait = true
                      erreur=false
                    }
                  }
                  if (dose_lait_choix == 2) {
                    if (stock_lait < 0.1) {
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                      println("Veuillez choisir moins de doses, sans supplément ou vérifier les stocks en mode Admin.")
                      println()
                      assez_lait = false
                      erreur = true
                    }
                    else {
                      stock_lait -= 0.1
                      prix_total += 0.1
                      recap_prix_lait += 0.1
                      assez_lait = true
                      erreur=false
                    }

                  }
                  if (dose_lait_choix == 3) {
                    if (stock_lait < 0.15) {
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                      println("Veuillez choisir moins de doses, sans supplément ou vérifier les stocks en mode Admin.")
                      println()
                      assez_lait = false
                      erreur = true
                    }
                    else {
                      stock_lait -= 0.15
                      prix_total += 0.15
                      recap_prix_lait += 0.15
                      assez_lait = true
                      erreur=false

                    }
                  }
                }
                else {
                  assez_lait = true
                }
              }
            }
            if (boisson_reussie && assez_sucre && assez_lait) {
              erreur = false

              //expresso
              if (choix_client == 1) {
                stock_cafe -= conso_cafe_expresso
                prix_total += prix_expresso
                recap_prix_cafe += prix_expresso
                println("Boisson sélectionnée : Expresso ")
              } //cappuccino
              else if (choix_client == 2) {
                stock_cafe -= conso_cafe_cappuccino
                stock_lait -= conso_lait_cappuccino
                prix_total += prix_cappuccino
                recap_prix_cafe += prix_cappuccino
                println("Boisson sélectionnée : Cappuccino ")
              }
              //latte
              else {
                if (choix_latte_taille==1) {
                  stock_cafe -= conso_cafe_latte_p
                  stock_lait -= conso_lait_latte_petit
                  prix_total += prix_latte_petit
                  recap_prix_cafe += prix_latte_petit
                  println("Boisson sélectionnée : Latté (Petit)")
                }
                else if (choix_latte_taille==2) {
                  stock_cafe -= conso_cafe_latte_m
                  stock_lait -= conso_lait_latte_moyen
                  prix_total += prix_latte_moyen
                  recap_prix_cafe += prix_latte_moyen
                  println("Boisson sélectionnée : Latté (Moyen)")
                }
                else if (choix_latte_taille==3){
                  stock_cafe -= conso_cafe_latte_g
                  stock_lait -= conso_lait_latte_grand
                  prix_total += prix_latte_grand
                  recap_prix_cafe += prix_latte_grand
                  println("Boisson sélectionnée : Latté (Grand)")
                }
              }

              //sucre
              if (choix_client_sucre == 1) {
                println("Niveau de sucre : Sans sucre ")
              }
              else if (choix_client_sucre == 2) {
                stock_sucre -= conso_sucre_p
                prix_total += prix_sucre_p
                recap_prix_sucre += prix_sucre_p
                println("Niveau de sucre : Peu (5g) ")
              }
              else if (choix_client_sucre == 3) {
                stock_sucre -= conso_sucre_m
                prix_total += prix_sucre_m
                recap_prix_sucre += prix_sucre_m
                println("Niveau de sucre : Moyen (10g) ")
              }
              else {
                stock_sucre -= conso_sucre_b
                prix_total += prix_sucre_b
                recap_prix_sucre += prix_sucre_b
                println("Niveau de sucre : Beacoup (15g) ")
              }

              //lait
              if (choix_client==2 || choix_client==3) {
                if (choix_client_lait==1){
                  println("Lait supplémentaire: Oui ")
                }
                else {
                  println("Lait supplémentaire: Non ")
                }
              }

              if ((recap_prix_sucre > 0) && (recap_prix_lait > 0)) {
                printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, recap_prix_lait, prix_total)
              }
              //sucre
              else if (recap_prix_sucre > 0) {
                printf("Prix total: CHF %.2f + CHF %.2f  = CHF %.2f\n ", recap_prix_cafe, recap_prix_sucre, prix_total)
              }
              //lait
              else if (recap_prix_lait > 0) {
                printf("Prix total: CHF %.2f + CHF %.2f = CHF %.2f\n ", recap_prix_cafe, recap_prix_lait, prix_total)
              }
              // ni lait ni sucre
              else if ((recap_prix_sucre == 0) && (recap_prix_lait == 0)) {
                printf("Prix total: = CHF %.2f\n ",  prix_total)
              }

              //Paiement
              val alphanum = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              var code_twint = ""
              val hazard1 = alphanum((math.random() * alphanum.length()).toInt).toString
              val hazard2 = alphanum((math.random() * alphanum.length()).toInt).toString
              val hazard3 = alphanum((math.random() * alphanum.length()).toInt).toString
              val hazard4 = alphanum((math.random() * alphanum.length()).toInt).toString
              val hazard5 = alphanum((math.random() * alphanum.length()).toInt).toString

              code_twint = hazard1 + hazard2 + hazard3 + hazard4 + hazard5

              println()
              println("Veuillez payer en utilisant Twint.")
              println("Votre code de paiement est : " + code_twint)
              println("En attente de paiement...")
              println()
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté.")
              println()
              // Préparation de la boisson
              println("Préparation de votre boisson...")
              println("[...]")
              println("Votre Cappuccino est prêt ! Bonne dégustation !")
              println()
            }
          }
        }
      }
      if (choix == 2) {
        println("Mode Admin")
        println("Entrez le code PIN")
        var code_pin = readLine(">").toInt
        while (code_pin != 434343) {
          println("Code erroné, veuillez rentrer le bon code PIN.")
          code_pin = readLine("> ").toInt
        }
        println("Accès autorisé. ")
        println()
        println("Stocks: ")
        println("    Poudre de cafe: " + stock_cafe + "g")
        printf("    Lait          :%.2fL",stock_lait)
        println()
        println("    Sucre         : " + stock_sucre + "g")

        println("Souhaitez-vous ajouter les stocks ? ")
        println("1) Oui")
        println("2) Non")

        var choix_admin = readLine("> ").toInt
        //verif données valides
        while (!(choix_admin == 1 || choix_admin == 2)) {
          print("Votre sélection n'est pas correcte, choisissez entre 1 ou 2. ")
          choix_admin = readLine("> ").toInt
        }
        var ajout_cafe = 0
        var ajout_lait = 0.0
        var ajout_sucre = 0
        var quitter2 = false
        while (!quitter2) {
          if (choix_admin == 1) {
            println("Combien de grammes de poudre de café souhaitez-vous ajouter ?")
            ajout_cafe = readLine(">").toInt
            stock_cafe += ajout_cafe

            println("Combien de grammes de sucre souhaitez-vous ajouter ?")
            ajout_sucre = readLine(">").toInt
            stock_sucre += ajout_sucre

            println("Combien de litres de lait souhaitez-vous ajouter ?")
            ajout_lait = readLine(">").toDouble
            stock_lait += ajout_lait
            println("Réapprovisionnement des stocks...")

            println("Ajout: ")
            println("    Poudre de cafe : " + ajout_cafe + "g")
            println("    Lait : " + ajout_lait + "L")
            println("    Sucre : " + ajout_sucre + "g")
            println("Niveaux de stocks mis à jour.")
            println("Retour au menu principal...")
            println()
            quitter2 = true
          }
          else {
            println("Retour au menu principal...")
            quitter2 = true

          }
        }
      }

      if (choix == 3){
        println("Vous avez quitté le programme.")
        quitter = true
      }
    }

  }
}