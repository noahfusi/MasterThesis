import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {

  // Déclaration de variables:

    // Stock avec les quantitées initiaux
      var poudrecafe = 50
      var stocksucre = 30
      var stocklait = 0.500

    // Menu
      var mode = 0

    //Personalisation des boissons
      var boisson = 0
      var tailleLatte = 0
      var sucre = 0
      var lait = 0
      var doselait = 5
      var QtotalLait = 0.0

      var boissonSelectionnee = "jsp"
      var niveauSucre = "jsp"
      var laitSupplementaire = 0

    // Suffisance des Stocks
      var cafeSuffisant = false
      var sucreSuffisant = false
      var laitSuffisant = false
      var SuffisanceStocks = false
      var MessErreur = "jsp"

    // Prix
      var prixCafe = 0.00
      var prixSucre = 0.00
      var prixLait = 0.00
      var PrixTotal = 0.00

    //Code Twint
      var typecaractere = 0
      var a = "/"
      var b = "/"
      var c = "/"
      var d = "/"
      var e = "/"
      var CodeTwint = "_____"

    // Le code PIN -> Mode Admin
      val CodePINadmin = 434343
      var PINsaisit = 0
      var ajouterstock = 0
      var ajoutcafe = 0
      var ajoutsucre = 0
      var ajoutlait = 0.000

    // Mode réussit/complété
      var ModeReussie = false



  // Recommencer d'ici, lorsque le mode a été réussit/complété
    do {

      // Réinitialiser valeurs des variables fonctionnelles

        // Menu
          mode = 0

        // Personnalisation des boissons
          boisson = 0
          tailleLatte = 0
          sucre = 0
          lait = 0
          doselait = 5
          QtotalLait = 0.0

          boissonSelectionnee = "jsp"
          niveauSucre = "jsp"
          laitSupplementaire = 0

        // Suffisance des Stocks
          cafeSuffisant = false
          sucreSuffisant = false
          laitSuffisant = false
          SuffisanceStocks = false
          MessErreur = "jsp"

        // Prix
          prixCafe = 0.00
          prixSucre = 0.00
          prixLait = 0.00
          PrixTotal = 0.00

        // Mode Réussit/complété
          ModeReussie = false



      //Affichage du menu
        mode = readLine('\t' + "Nospresso Café " + '\n' +
                          "Veuillez sélectionner votre mode:" + '\n' +
                          "1) Client" + '\n' +
                          "2) Admin" + '\n' +
                          "3) Quitter" + '\n' +
                          ">  ").toInt

      // Répéter jusqu'à ce que l'utilisateur a entré une valeur autorisée
        while ((mode < 1) || (mode > 3)) {
          mode = readLine('\t' + "Nospresso Café " + '\n' +
                            "Veuillez sélectionner votre mode:" + '\n' +
                            "1) Client" + '\n' +
                            "2) Admin" + '\n' +
                            "3) Quitter" + '\n' +
                            ">  ").toInt
        }



      //Mode Client
        if (mode == 1) {

          // Faire tant que les stocks sont insuffisant
            do {
              //Choix de la boisson
                boisson = readLine("Veuillez sélectionner votre boisson :" + '\n' +
                                      "1) Expresso - CHF 2.00 " + '\n' +
                                      "2) Cappuccino - CHF 2.50" + '\n' +
                                      "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" + '\n' +
                                      ">  ").toInt

              // Recommencer valeur "boisson" jusqu'à valeur autorisée
                while ((boisson < 1) || (boisson > 3)) {
                  boisson = readLine("Veuillez sélectionner votre boisson :" + '\n' +
                                        "1) Expresso - CHF 2.00 " + '\n' +
                                        "2) Cappuccino - CHF 2.50" + '\n' +
                                        "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" + '\n' +
                                        ">  ").toInt
                }

              //Si a choisi un Latte, quelle taille?
                if (boisson == 3) {
                  tailleLatte = readLine("Veuillez sélectionner votre taille de Latte: " + '\n' +
                                            "1) Petit - CHF 2.70" + '\n' +
                                            "2) Moyen - CHF 3.20" + '\n' +
                                            "3) Grand - CHF 3.70" + '\n' +
                                            ">  ").toInt

              // Répetter jusqu'à que "tailleLatte" a une valeur autorisée
                while ((tailleLatte < 1) || (tailleLatte > 3)) {
                  tailleLatte = readLine("Veuillez sélectionner votre taille de Latte: " + '\n' +
                                            "1) Petit - CHF 2.70" + '\n' +
                                            "2) Moyen - CHF 3.20" + '\n' +
                                            "3) Grand - CHF 3.70" + '\n' +
                                            ">  ").toInt
                }
                }

              //Personalisation de la boisson
                //Sucre?
                  sucre = readLine("Souhaitez-vous ajouter du sucre?" + '\n' +
                                      "1) Sans sucre" + '\n' +
                                      "2) Peu (5g) - CHF 0.10" + '\n' +
                                      "3) Moyen (10g) - CHF 0.20" + '\n' +
                                      "4) Grand (15g) - CHF 0.30" + '\n' +
                                      ">  ").toInt

                // Répetter jusqu'à que "sucre" a une valeur autorisée
                  while ((sucre < 1) || (sucre > 4)) {
                    sucre = readLine("Souhaitez-vous ajouter du sucre?" + '\n' +
                                        "1) Sans sucre" + '\n' +
                                        "2) Peu (5g) - CHF 0.10" + '\n' +
                                        "3) Moyen (10g) - CHF 0.20" + '\n' +
                                        "4) Grand (15g) - CHF 0.30" + '\n' +
                                        ">  ").toInt
                  }



                //Lait? (Attention -> uniquement si a choisi Cappuccino ou Latte!!)
                  if ((boisson == 2) || (boisson == 3)) {
                    lait = readLine("Souhaitez-vous ajouter du lait en supplément ?" + '\n' +
                                      "1) Oui" + '\n' +
                                      "2) Non" + '\n' +
                                      ">  ").toInt

                  // Répetter jusqu'à que "lait" a une valeur autorisée
                    while ((lait < 1) || (lait > 2)) {
                      lait = readLine("Souhaitez-vous ajouter du lait en supplément ?" + '\n' +
                                        "1) Oui" + '\n' +
                                        "2) Non" + '\n' +
                                        ">  ").toInt
                    }


                  //Combien de dose de lait
                    if (lait == 1) {
                      doselait = readLine("Combien de dose? (Max.3)" + '\n' +
                                            ">  ").toInt

                    // max 3 dose de lait
                      while ((doselait < 0) || (3 < doselait)) {
                        doselait = readLine("Combien de dose? (Max.3)" + '\n' +
                                              ">  ").toInt
                      }
                    } else if (lait == 2) {
                      doselait = 0
                    }
                  }



              //Suffisance des stocks?

                //Suffisance de café
                  if (boisson == 1) {
                    if (poudrecafe >= 8) {
                      cafeSuffisant = true
                    }
                  } else if (boisson == 2) {
                    if (poudrecafe >= 6) {
                      cafeSuffisant = true
                    }
                  } else { // (donc c'est la boisson 3 --> latte mais quelle taille?)
                    if (tailleLatte == 1) {
                      if (poudrecafe >= 6) {
                        cafeSuffisant = true
                      }
                    } else if (tailleLatte == 2) {
                      if (poudrecafe >= 8) {
                        cafeSuffisant = true
                      }
                    } else { // (donc c'est la Latte de grande taille (3))
                      if (poudrecafe >= 12) {
                        cafeSuffisant = true
                      }
                    }
                  }

                // Suffisance de sucre?
                  if (sucre == 1) {
                    sucreSuffisant = true
                  } else if (sucre == 2) {
                    if (stocksucre >= 5) {
                      sucreSuffisant = true
                    }
                  } else if (sucre == 3) {
                    if (stocksucre >= 10) {
                      sucreSuffisant = true
                    }
                  } else { // (Donc c'est 15g de sucre (sucre =4))
                    if (stocksucre >= 15) {
                      sucreSuffisant = true
                    }
                  }

                // Quantité total de lait dans la boisson + Suffisance de lait?
                  if (boisson == 1) {
                    QtotalLait = 0
                    laitSuffisant = true
                  } else if (boisson == 2) {
                    QtotalLait = (0.05 * doselait) + 0.1
                  } else if ((boisson == 3) && (tailleLatte == 1)) {
                    QtotalLait = (0.05 * doselait) + 0.12
                  } else if ((boisson == 3) && (tailleLatte == 2)) {
                    QtotalLait = (0.05 * doselait) + 0.15
                  } else if ((boisson == 3) && (tailleLatte == 3)) {
                    QtotalLait = (0.05 * doselait) + 0.2
                  }

                  if((stocklait >= QtotalLait) || (boisson == 1)){
                    laitSuffisant = true
                  }else{
                    laitSuffisant = false
                  }

                // Suffisance générale(totale) des stocks
                  if ((cafeSuffisant == true) && (sucreSuffisant == true) && (laitSuffisant == true)) {
                    SuffisanceStocks = true
                  }



              //Nommer ce qui a été sélectionnée
                if (boisson == 1) {
                  boissonSelectionnee = "Expresso"
                } else if (boisson == 2) {
                  boissonSelectionnee = "Cappuccino"
                } else if ((boisson == 3) && (tailleLatte == 1)) {
                  boissonSelectionnee = "Latte (Petit)"
                } else if ((boisson == 3) && (tailleLatte == 2)) {
                  boissonSelectionnee = "Latte (Moyen)"
                } else if ((boisson == 3) && (tailleLatte == 3)) {
                  boissonSelectionnee = "Latte (Grand)"
                }

                if (sucre == 1) {
                  niveauSucre = "Sans sucre"
                } else if (sucre == 2) {
                  niveauSucre = "Peu (5g)"
                } else if (sucre == 3) {
                  niveauSucre = "Moyen (10g)"
                } else if (sucre == 4) {
                  niveauSucre = "Beaucoup (15g)"
                }

                if (lait == 1) {
                  laitSupplementaire = doselait
                } else if (lait == 2) {
                  laitSupplementaire = 0
                }


              // Message d'erreur
                if (cafeSuffisant == false) {
                  MessErreur = "Quantité de poudre de café insuffisante pour" + '\n' +
                                  "préparer la boisson sélectionnée." + '\n' +
                                "Veuillez choisir une autre boisson ou vérifier les" + '\n' +
                                  "stocks en mode Admin."
                } else if (sucreSuffisant == false) {
                  MessErreur = "Quantité de sucre insuffisante pour préparer" + '\n' +
                                  "la boisson sélectionnée." + '\n' +
                                "Veuillez choisir une plus petite quantitée de sucre."
                } else if (laitSuffisant == false) {
                  MessErreur = "Quantité de lait insuffisante pour préparer" + '\n' +
                                  "la boisson sélectionnée." + '\n' +
                                "Veuillez choisir une taille plus petite ou essayer" + '\n' +
                                  "une autre boisson (ou choisir une plus petite dose de lait supplémentaire)."
                }


              // Affichage (Attention, l'affichage change selon si c'est un cafe, cappuccino ou latte)
                if (SuffisanceStocks == false) {
                  if (boisson == 1) {
                    println('\n' + "Boisson sélectionnée : " + boissonSelectionnee + '\n' +
                                  "Niveau de sucre : " + niveauSucre + '\n' +
                                  '\n' +
                                  "Erreur : " + MessErreur + '\n' + '\n' + '\n')
                  } else if ((boisson == 2) || (boisson == 3)) {
                    println('\n' + "Boisson sélectionnée : " + boissonSelectionnee + '\n' +
                                  "Niveau de sucre : " + niveauSucre + '\n' +
                                  "Lait en supplément : " + laitSupplementaire + '\n' +
                                  '\n' +
                                  "Erreur : " + MessErreur + '\n' + '\n' + '\n')
                  }
                }

            } // Fin du "do while" stock insuffisant
            while (SuffisanceStocks == false)



            if (SuffisanceStocks == true) {
              // Prix de la commande
                // Prix Café
                  if (boisson == 1) {
                    prixCafe = 2.00
                  } else if (boisson == 2) {
                    prixCafe = 2.50
                  } else if ((boisson == 3) && (tailleLatte == 1)) {
                    prixCafe = 2.70
                  } else if ((boisson == 3) && (tailleLatte == 2)) {
                    prixCafe = 3.20
                  } else if ((boisson == 3) && (tailleLatte == 3)) {
                    prixCafe = 3.70
                  }

                // Prix Sucre
                  if (sucre == 1) {
                    prixSucre = 0.00
                  } else if (sucre == 2) {
                    prixSucre = 0.10
                  } else if (sucre == 3) {
                    prixSucre = 0.20
                  } else {
                    prixSucre = 0.30
                  }

                // Prix Lait
                  prixLait = laitSupplementaire * 0.05

                // Prix total de la commande
                  PrixTotal = prixCafe + prixSucre + prixLait


              // Générer le code du paiement twint
                val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                val chiffres = "0123456789"
                typecaractere = 0
                a = "/"
                b = "/"
                c = "/"
                d = "/"
                e = "/"
                CodeTwint = "_____"

                typecaractere = (math.random()*2).toInt
                if(typecaractere == 0){
                  a = alphabet((math.random() * alphabet.length - 1).toInt).toString
                }else{
                  a = chiffres((math.random() * chiffres.length - 1).toInt).toString
                }

                typecaractere = (math.random()*2).toInt
                if(typecaractere == 0) {
                  b = alphabet((math.random() * alphabet.length - 1).toInt).toString
                }else{
                  b = chiffres((math.random() * chiffres.length - 1).toInt).toString
                }

                typecaractere = (math.random()*2).toInt
                if(typecaractere == 0) {
                  c = alphabet((math.random() * alphabet.length - 1).toInt).toString
                }else{
                  c = chiffres((math.random() * chiffres.length - 1).toInt).toString
                }

                typecaractere = (math.random()*2).toInt
                if(typecaractere == 0) {
                  d = alphabet((math.random() * alphabet.length - 1).toInt).toString
                }else{
                  d = chiffres((math.random() * chiffres.length - 1).toInt).toString
                }

                typecaractere = (math.random()*2).toInt
                if(typecaractere == 0) {
                  e = alphabet((math.random() * alphabet.length - 1).toInt).toString
                }else{
                  e = chiffres((math.random() * chiffres.length - 1).toInt).toString
                }

                CodeTwint = a + b + c + d + e



              // Affichage de la commande + préparation + Twint
                if (boisson == 1) {
                  println('\n' + "Boisson sélectionnée : " + boissonSelectionnee + '\n' +
                                "Niveau de sucre : " + niveauSucre)
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixCafe, prixSucre, PrixTotal)
                  println('\n')
                  println("Veuillez payer en utilisant Twint." + '\n' +
                          "Votre code de paiement est : " + CodeTwint + '\n' +
                          "(En attente de paiement)" + '\n')
                  // Attendre 5000 millisecondes (5 secondes)
                  Thread.sleep(5000)
                  println("Paiement confirmé." + '\n' +
                          "Préparation de votre boisson ..." + '\n' +
                          "Votre Expresso est prêt ! Bonne dégustation !" + '\n' + '\n' + '\n')
                } else if (boisson == 2) {
                  println('\n' + "Boisson sélectionnée : " + boissonSelectionnee + '\n' +
                                "Niveau de sucre : " + niveauSucre + '\n' +
                                "Lait supplémentaire : " + laitSupplementaire)
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixCafe, prixSucre, prixLait, PrixTotal)
                  println('\n')
                  println("Veuillez payer en utilisant Twint." + '\n' +
                          "Votre code de paiement est : " + CodeTwint + '\n' +
                          "(En attente de paiement)" + '\n')
                  // Attendre 5000 millisecondes (5 secondes)
                  Thread.sleep(5000)
                  println("Paiement confirmé." + '\n' +
                          "Préparation de votre boisson ..." + '\n' +
                          "Votre Cappuccino est prêt ! Bonne dégustation !" + '\n' + '\n' + '\n')
                } else {
                  println('\n' + "Boisson sélectionnée : " + boissonSelectionnee + '\n' +
                                "Niveau de sucre : " + niveauSucre + '\n' +
                                "Lait supplémentaire : " + laitSupplementaire)
                  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixCafe, prixSucre, prixLait, PrixTotal)
                  println('\n')
                  println("Veuillez payer en utilisant Twint." + '\n' +
                          "Votre code de paiement est : " + CodeTwint + '\n' +
                          "(En attente de paiement)" + '\n')
                  // Attendre 5000 millisecondes (5 secondes)
                  Thread.sleep(5000)
                  println("Paiement confirmé." + '\n' +
                          "Préparation de votre boisson ..." + '\n' +
                          "Votre Latte est prêt ! Bonne dégustation !" + '\n' + '\n' + '\n')
                }


              //Retirer du stock
                // retirer de la poudre de café
                  if (boisson == 1) {
                    poudrecafe = poudrecafe - 8
                  } else if (boisson == 2) {
                    poudrecafe = poudrecafe - 6
                  } else if ((boisson == 3) && (tailleLatte == 1)) {
                    poudrecafe = poudrecafe - 6
                  } else if ((boisson == 3) && (tailleLatte == 2)) {
                    poudrecafe = poudrecafe - 8
                  } else if ((boisson == 3) && (tailleLatte == 3)) {
                    poudrecafe = poudrecafe - 12
                  }

                // retirer du sucre du stock
                  if (sucre == 2) {
                    stocksucre = stocksucre - 5
                  } else if (sucre == 3) {
                    stocksucre = stocksucre - 10
                  } else if (sucre == 4) {
                    stocksucre = stocksucre - 15
                  }

                // retirer du lait des stocks
                  stocklait = stocklait - QtotalLait

              // Boisson réussi - servie et stocks retirés
                ModeReussie = true

            } // Fin de if -> suffisance des stock true

        } //Fin de if -> Mode Client (touche = 1)






      // Mode Admin
        if (mode == 2) {

          // Réinitialiser valeurs des variables fonctionnelles
            PINsaisit = 0
            ajouterstock = 0
            ajoutcafe = 0
            ajoutsucre = 0
            ajoutlait = 0.000

          // Entrez le code PIN
            PINsaisit = readLine("\nMode Admin \nEntrez le code PIN : ******  > ").toInt

          //Répéter jusqu'à ce que le code PIN soit juste
            while (PINsaisit != CodePINadmin) {
              PINsaisit = readLine("Entrez le code PIN : ******  > ").toInt
            }

          // Affichage - Ajout de stocks
            if (PINsaisit == CodePINadmin) {
              println("Accès autorisé." + '\n' + '\n' +
                      "Stocks: " + '\n' +
                      '\t' + "Poudre de café: " + poudrecafe + "g")
              printf('\t' + "Lait          : %.3f L \n",stocklait)
              println('\t' + "Sucre         : " + stocksucre + "g" + '\n')

              ajouterstock = readLine('\n' + "Réapprovisionnement des stocks ..." + '\n' +
                                      "Ajout :" + '\n' +
                                      '\t' + "1) Poudre de café" + '\n' +
                                      '\t' + "2) Lait" + '\n' +
                                      '\t' + "3) Sucre" + '\n' +
                                      "> ").toInt
              while ((ajouterstock < 1) || (3 < ajouterstock)) {
                ajouterstock = readLine("Réapprovisionnement des stocks ..." + '\n' +
                                        "Ajout :" + '\n' +
                                        '\t' + "1) Poudre de café" + '\n' +
                                        '\t' + "2) Lait" + '\n' +
                                        '\t' + "3) Sucre" + '\n' +
                                        "> ").toInt
              }

           // Ajout de stocks
              if (ajouterstock == 1) {
                ajoutcafe = readLine("Combien de grammes (g) de poudre de café ajoutez-vous?" + '\n' +
                                      "> ").toInt
                while (ajoutcafe < 0) {
                  ajoutcafe = readLine("Combien de grammes (g) de poudre de café ajoutez-vous?" + '\n' +
                                        "> ").toInt
                }
                poudrecafe += ajoutcafe
              } else if (ajouterstock == 2) {
                ajoutlait = readLine("Combien de litres (L) de lait ajoutez-vous?" + '\n' +
                                      "> ").toDouble
                while (ajoutlait < 0) {
                  ajoutlait = readLine("Combien de litres (L) de lait ajoutez-vous?" + '\n' +
                                        "> ").toDouble
                }
                stocklait += ajoutlait
              } else {
                ajoutsucre = readLine("Combien de grammes (g) de sucre ajoutez-vous?" + '\n' +
                                      "> ").toInt
                while (ajoutsucre < 0) {
                  ajoutsucre = readLine("Combien de grammes (g) de sucre ajoutez-vous?" + '\n' +
                                        "> ").toInt
                }
                stocksucre += ajoutsucre
              }

          // Mise à jour des stocks
              println("\nNiveaux de stock mis à jour." + '\n' +
                      "Voici les stocks à jours : " + '\n' +
                      '\t' + "Poudre de café: " + poudrecafe + "g")
              printf( '\t' + "Lait          : %.3f L \n",stocklait)
              println('\t' + "Sucre         : " + stocksucre + "g" + '\n' +
                      "\nRetour au menu principal..." + '\n' + '\n' + '\n')

              Thread.sleep(5000)

              ModeReussie = true

            } // Fin du if code PIN juste
        } // Fin du if Mode Admin



      // Mode Quitter
        if (mode == 3) {
          println('\n' + "Vous avez quitté le programme Nospresso ...")
        }

    } // Fin du "do while" mode réussit/complété
    while (ModeReussie == true)

  } // Fin
  }


