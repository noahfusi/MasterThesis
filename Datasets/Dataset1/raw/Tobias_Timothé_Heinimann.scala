import scala.io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var programme = 0
    //Variable pour l'affichage
    var prix = 0.00
    var prix_e = 2.00
    var prix_c = 2.50
    var prix_l_p = 2.70
    var prix_l_m = 3.20
    var prix_l_g = 3.70
    var prix_lait= 0.00
    var prix_sucre = 0.00
    var a_lait = "test"
    var a_sucre = "test"
    var d_lait = 0.00

    //Variable du paiement + TWINT
    var alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code_twint = ""

    var pin = 434343
    var panne = 0
    //Variable des sotcks de base
    var pdc = 50
    var sucre = 30
    var lait = 0.500

    //Variable de personalisation
    var p_sucre = 0
    var p_lait = 0
    var q_lait = 0.00
    var q_sucre = 0 //quantité sucre
    //Début du programme--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    while(programme == 0) {
      print("\nNospresso Café")
      println
      var choix = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin\n3) Quitter\n >").toInt
      code_twint = ""
      for(i <- 1 to 5){
        var numero = (math.random()*alphanumerique.length).toInt
        code_twint = code_twint + alphanumerique(numero)
      }
      //CLIENT--------------------------------------------------------------------------------------------------------------------------------------------------------
      panne = 0
      if (choix == 1){
        while(panne == 0){
          var choix_boisson = readLine("Quelle boisson souhaitez vous commander ?\n1) Espresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
          //Expresso--------------------------------------------------------------------------------------------------------------------------------------------------------
          if(choix_boisson == 1){
            prix = 2.00
            var  p_sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n>").toInt

            if((pdc-8)>= 0){
              if((p_sucre == 1)){
                q_sucre = 0
                prix_sucre = 0.00
                a_sucre = "Sans sucre"
              }else if(p_sucre == 2){
                q_sucre = 5
                prix_sucre = 0.10
                prix = prix + prix_sucre
                a_sucre = "Peu (5g)"
              }else if(p_sucre == 3){
                q_sucre = 10
                prix_sucre = 0.20
                prix = prix + prix_sucre
                a_sucre = "Moyen (10g)"
              }else if(p_sucre == 4){
                q_sucre = 15
                prix_sucre = 0.30
                prix = prix + prix_sucre
                a_sucre = "Beaucoup (15g)"
              }
              if((q_sucre <= sucre)&&((pdc-8) >= 0)){
                pdc = pdc - 8
                panne = 1
                sucre = sucre - q_sucre
                //Resumer Commande
                println("\nBoisson selectionnée : Espresso\nNiveau de sucre :" + a_sucre)
                if(p_sucre == 1){
                  printf("Prix total : CHF %.2f \n", prix)
                }else{
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", prix_e, prix_sucre, prix)
                }
                //Paiement
                println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté.\n")
                //Apres paiement
                println("Préparation de votre boisson...\n[...]\nVotre Espresso est prêt ! Bonne dégustation !")
              }else if (q_sucre > sucre){
                println("\nErreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
              }
            }else if((pdc-8) < 0){
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
            //Choix Cappucino--------------------------------------------------------------------------------------------------------------------------------------------------
          }else if(choix_boisson == 2){
            prix = 2.50
            var p_sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
            p_lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n>").toInt
            if(p_lait == 1){
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toDouble
              if(d_lait <= 3){
                q_lait = 0.1 + (d_lait * 0.05)
                prix = prix + (d_lait * 0.05)
                if(d_lait == 1){
                  a_lait = "1 dose (0.05L)"
                  prix_lait = 0.05
                }else if(d_lait == 2){
                  a_lait = "2 doses (0.1L)"
                  prix_lait = 0.10
                }else{
                  a_lait = "3 doses (0.15L)"
                  prix_lait = 0.15
                }
              }else{
                println("Erreur: Le nombre de doses demandés est trop élevés.")
                d_lait = readLine("Combien de dose ? (3 doses maximum)\n>").toDouble
              }

            }else if(p_lait == 2){
              q_lait = 0.100
              a_lait = "Non"
            }
            if ((pdc - 6) >= 0) {
              if (lait - q_lait >= 0) {
                if((p_sucre == 1)){
                  q_sucre = 0
                  a_sucre = "Sans sucre"
                  prix_sucre = 0.00
                }else if(p_sucre == 2){
                  q_sucre = 5
                  prix_sucre = 0.10
                  prix = prix + prix_sucre
                  a_sucre = "Peu (5g)"
                }else if(p_sucre == 3){
                  q_sucre = 10
                  prix_sucre = 0.20
                  prix = prix + prix_sucre
                  a_sucre = "Moyen (10g)"
                }else if(p_sucre == 4){
                  q_sucre = 15
                  prix_sucre = 0.30
                  prix = prix + prix_sucre
                  a_sucre = "Beaucoup (15g)"
                }
                if((q_sucre <= sucre)&&(lait-q_lait >= 0)&&((pdc-6)>=0)){
                  sucre = sucre - q_sucre
                  lait = lait - q_lait
                  pdc = pdc - 6
                  panne = 1
                  //Resumer Commande
                  if((p_lait ==1)&&(p_sucre !=1)){
                    println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_c, prix_sucre, prix_lait, prix)
                  }else if((p_lait == 2)&&(p_sucre != 1)){
                    println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                    printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_c, prix_sucre, prix)
                  }else if((p_lait == 1)&&(p_sucre == 1)){
                    println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                    printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_c, prix_lait, prix)
                  }else{
                    println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                    printf("Prix total : CHF %.2f \n", prix)
                  }

                  //Paiement
                  println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
                  println("(En attente de validation du paiement...)")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté.\n")
                  //Apres paiement
                  println("Préparation de votre boisson...\n[...]\nVotre Cappuccino est prêt ! Bonne dégustation !")
                }else if (q_sucre > sucre) {
                  println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                }

              } else if (lait - q_lait < 0) {
                println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
              }
            } else if ((pdc - 6) < 0) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
            //Choix Latte--------------------------------------------------------------------------------------------------------------------------------------------------
          }else if(choix_boisson == 3){
            var taille = readLine("Quelle taille souahitez vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
            var p_sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
            p_lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
            // Taille PETIT ------------------------------------------------------------------------------------------------------------------------------------------
            if(taille == 1){
              prix = 2.70
              if(p_lait == 1){
                d_lait = readLine("Combien de dose ?\n> ").toDouble
                if(d_lait <= 3){
                  q_lait = 0.120 + (d_lait * 0.05)
                  prix = prix + (d_lait * 0.05)
                  if(d_lait == 1){
                    a_lait = "1 dose (0.05L)"
                    prix_lait = 0.05
                  }else if(d_lait == 2){
                    a_lait = "2 doses (0.1L)"
                    prix_lait = 0.10
                  }else{
                    a_lait = "3 doses (0.15L)"
                    prix_lait = 0.15
                  }
                }else{
                  println("Erreur: Le nombre de doses demandés est trop élevés.")
                  d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toDouble
                }
              }else if(p_lait == 2){
                q_lait = 0.120
                a_lait = "Non"
              }
              if ((pdc - 6) >= 0) {
                if (lait - q_lait >= 0) {
                  if((p_sucre == 1)){
                    q_sucre = 0
                    a_sucre = "Sans sucre"
                  }else if(p_sucre == 2){
                    q_sucre = 5
                    prix_sucre = 0.10
                    prix = prix + prix_sucre
                    a_sucre = "Peu (5g)"
                  }else if(p_sucre == 3){
                    q_sucre = 10
                    prix_sucre = 0.20
                    prix = prix + prix_sucre
                    a_sucre = "Moyen (10g)"
                  }else if(p_sucre == 4){
                    q_sucre = 15
                    prix_sucre = 0.30
                    prix = prix + prix_sucre
                    a_sucre = "Beaucoup (15g)"
                  }
                  if((q_sucre <= sucre)&&(lait - q_lait >= 0)&&((pdc - 6) >= 0)){
                    sucre = sucre - q_sucre
                    lait = lait - q_lait
                    pdc = pdc - 6
                    panne = 1
                    //Resumer Commande
                    if((p_lait ==1)&&(p_sucre !=1)){
                      println("\nBoisson selectionnée : Latte (Petit)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_l_p, prix_sucre, prix_lait, prix)
                    }else if((p_lait == 2)&&(p_sucre != 1)){
                      println("\nBoisson selectionnée : Latte (Petit)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_p, prix_sucre, prix)
                    }else if((p_lait == 1)&&(p_sucre == 1)){
                      println("\nBoisson selectionnée : Latte (Petit)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_p, prix_lait, prix)
                    }else{
                      println("\nBoisson selectionnée : Latte (Petit)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f \n", prix)
                    }
                    //Paiement
                    println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
                    println("(En attente de validation du paiement...)")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté.\n")
                    //Apres paiement
                    println("Préparation de votre boisson...\n[...]\nVotre Latte est prêt ! Bonne dégustation !")
                  }else if (q_sucre > sucre) {
                    println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                  }
                } else if (lait - q_lait < 0) {
                  println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                }
              } else if ((pdc - 6) < 0) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              //Taille MOYEN --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
            }else if(taille == 2){
              prix = 3.20
              if(p_lait == 1){
                d_lait = readLine("Combien de dose ?\n> ").toDouble
                if(d_lait <= 3){
                  q_lait = 0.150 + (d_lait * 0.05)
                  prix = prix + (d_lait * 0.05)
                  if(d_lait == 1){
                    a_lait = "1 dose (0.05L)"
                    prix_lait = 0.05
                  }else if(d_lait == 2){
                    a_lait = "2 doses (0.1L)"
                    prix_lait = 0.10
                  }else{
                    a_lait = "3 doses (0.15L)"
                    prix_lait = 0.15
                  }
                }else{
                  println("Erreur: Le nombre de doses demandés est trop élevés.")
                  d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toDouble
                }
              }else if(p_lait == 2){
                q_lait = 0.150
                a_lait = "Non"
              }
              if ((pdc - 8) >= 0) {
                if (lait - q_lait >= 0) {
                  if((p_sucre == 1)){
                    q_sucre = 0
                    a_sucre = "Sans sucre"
                  }else if(p_sucre == 2){
                    q_sucre = 5
                    prix_sucre = 0.10
                    prix = prix + prix_sucre
                    a_sucre = "Peu (5g)"
                  }else if(p_sucre == 3){
                    q_sucre = 10
                    prix_sucre = 0.20
                    prix = prix + prix_sucre
                    a_sucre = "Moyen (10g)"
                  }else if(p_sucre == 4){
                    q_sucre = 15
                    prix_sucre = 0.30
                    prix = prix + prix_sucre
                    a_sucre = "Beaucoup (15g)"
                  }
                  if((q_sucre <= sucre)&&(lait - q_lait >= 0)&&((pdc - 8) >= 0)){
                    sucre = sucre - q_sucre
                    pdc = pdc - 8
                    lait = lait - q_lait
                    panne = 1
                    //Resumer Commande
                    if((p_lait ==1)&&(p_sucre !=1)){
                      println("\nBoisson selectionnée : Latte (Moyen)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_l_m, prix_sucre, prix_lait, prix)
                    }else if((p_lait == 2)&&(p_sucre != 1)){
                      println("\nBoisson selectionnée : Latte (Moyen)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_m, prix_sucre, prix)
                    }else if((p_lait == 1)&&(p_sucre == 1)){
                      println("\nBoisson selectionnée : Latte (Moyen)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_m, prix_lait, prix)
                    }else{
                      println("\nBoisson selectionnée : Latte (Moyen)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f \n", prix)
                    }
                    //Paiement
                    println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
                    println("(En attente de validation du paiement...)")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté.\n")
                    //Apres paiement
                    println("Préparation de votre boisson...\n[...]\nVotre Latte est prêt ! Bonne dégustation !")
                  }else if (q_sucre > sucre) {
                    println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                  }
                } else if (lait - q_lait < 0) {
                  println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                }
              } else if ((pdc - 8) < 0) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              //Taille GRAND-----------------------------------------------------------------------------------------------------------------------------------------------------------------
            }else if(taille == 3){
              prix = 3.70
              if(p_lait == 1){
                d_lait = readLine("Combien de dose ?\n> ").toDouble
                if(d_lait <= 3){
                  q_lait = 0.200 + (d_lait * 0.05)
                  prix = prix + (d_lait * 0.05)
                  if(d_lait == 1){
                    a_lait = "1 dose (0.05L)"
                    prix_lait = 0.05
                  }else if(d_lait == 2){
                    a_lait = "2 doses (0.1L)"
                    prix_lait = 0.10
                  }else{
                    a_lait = "3 doses (0.15L)"
                    prix_lait = 0.15
                  }
                }else{
                  println("Erreur: Le nombre de doses demandés est trop élevés.")
                  d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toDouble
                }
              }else if(p_lait == 2){
                q_lait = 0.200
                a_lait = "Non"
              }
              if ((pdc - 12) >= 0) {
                if (lait - q_lait >= 0) {
                  if((p_sucre == 1)){
                    q_sucre = 0
                    a_sucre = "Sans sucre"
                  }else if(p_sucre == 2){
                    q_sucre = 5
                    prix_sucre = 0.10
                    prix = prix + prix_sucre
                    a_sucre = "Peu (5g)"
                  }else if(p_sucre == 3){
                    q_sucre = 10
                    prix_sucre = 0.20
                    prix = prix + prix_sucre
                    a_sucre = "Moyen (10g)"
                  }else if(p_sucre == 4){
                    q_sucre = 15
                    prix_sucre = 0.30
                    prix = prix + prix_sucre
                    a_sucre = "Beaucoup (15g)"
                  }
                  if((q_sucre <= sucre)&&(lait - q_lait >= 0)&&((pdc - 12) >= 0)){
                    sucre = sucre - q_sucre
                    lait = lait - q_lait
                    pdc = pdc - 12
                    panne = 1
                    //Resumer Commande
                    if((p_lait ==1)&&(p_sucre !=1)){
                      println("\nBoisson selectionnée : Latte (Grand)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_l_g, prix_sucre, prix_lait, prix)
                    }else if((p_lait == 2)&&(p_sucre != 1)){
                      println("\nBoisson selectionnée : Latte (Grand)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_g, prix_sucre, prix)
                    }else if((p_lait == 1)&&(p_sucre == 1)){
                      println("\nBoisson selectionnée : Latte (Grand)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_l_g, prix_lait, prix)
                    }else{
                      println("\nBoisson selectionnée : Latte (Grand)\nNiveau de sucre : "+a_sucre+"\nLait en supplément : "+ a_lait)
                      printf("Prix total : CHF %.2f \n", prix)
                    }
                    //Paiement
                    println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
                    println("(En attente de validation du paiement...)")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté.\n")
                    //Apres paiement
                    println("Préparation de votre boisson...\n[...]\nVotre Latte est prêt ! Bonne dégustation !")
                  }else if (q_sucre > sucre) {
                    println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                  }
                } else if (lait - q_lait < 0) {
                  println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez essayer avec une autre boisson.\n")
                }
              } else if ((pdc - 12) < 0) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
            }

          }
        }
      }


      if(choix == 2){
        var code = readLine("Entrez le code PIN : ").toInt
        if(pin == code){
          println("Accès autorisé.")
          println("\nStocks:\nPoudre de café: "+ pdc+"g\nLait: "+ lait +"L\nSucre: "+ sucre +"g")
          println("Réapprovisionnement des stocks...\n Ajout :")
          var pdc_a = readLine("Poudre de café : ").toInt
          var lait_a = readLine("Lait :").toDouble
          var sucre_a = readLine("Sucre : ").toInt
          sucre = sucre + sucre_a
          pdc = pdc + pdc_a
          lait = lait + lait_a
          println("Niveaux des stocks mis à jour.\nRetour au menu principal...")
        }else if(pin != code){
          println("Erreur: Code PIN erroné.\nRetour au menu principal...")
        }
      }else if(choix == 3){ //fin choix 2
        programme = 1
      }else{
        // println("Erreur: L'entrée saisie n'est pas prise en compte par la machine.\nVeuillez réessayer.")
      }
    }
  }
}