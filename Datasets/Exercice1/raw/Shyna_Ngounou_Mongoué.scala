import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var mode = 0

    // Quantité pour Boisson + Suppléments
    var TypeBoisson = 0
    var Boisson_Selectionnee = ""
    var Niveau_Sucre = ""
    var Lait_Supp = ""
    var TailleLatte = 0
    var Sucre = 0
    var DoseSucre = 5.0
    var Quantite_Sucre_Add = 0.0
    var lait = 0.0
    var DoseLait = 0.0
    var Quantite_Lait_Cappuccino = 0.0
    var Quantite_Lait_Petit_Latte = 0.0
    var Quantite_Lait_Moyen_Latte = 0.0
    var Quantite_Lait_Grand_Latte = 0.0

    // Prix
    var PrixTotal = 0.0
    var PrixBaseExpresso = 2.00
    var PrixSucre = 0.10
    var PrixBaseCappuccino = 2.50
    var PrixPetitLatte = 2.70
    var PrixMoyenLatte = 3.20
    var PrixGrandLatte = 3.70

    //Stock initial des ingrédients
    var PoudreCafe = 30.0
    var StockSucre = 30.0
    var StockLait = 0.500

    // Choix du mode
    var continuer = true
    while (continuer) {
      mode = 0

      TypeBoisson = 0

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Nospresso Café \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        mode = readInt()
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez choisir un nombre valide")
        }


        // Choix de la boisson

        while (mode == 1) {
          TypeBoisson = 0



        while (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
            println(" Veuillez sélectionner votre boisson : \n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit) , CHF 3.20 (Moyen) , CHF 3.70 (Grand) \n >")
            TypeBoisson = readInt()
            if (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
              println("Veuillez choisir un nombre valide")
            }
          }
          // Gestion des boissons Latte
          if (TypeBoisson == 3) {
            println("Quelle taille de Latte voulez-vous ? \n 1) Petit \n 2) Moyen \n 3)Grand\n > ")
            TailleLatte = readInt()
          }

          // Gestion du sucre
          println(" Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n >")
          Sucre = readInt()
          if (Sucre == 1) {
            Niveau_Sucre = "Sans Sucre"
          } else if (Sucre == 2) {
            Niveau_Sucre = "Peu (5g)"
          } else if (Sucre == 3) {
            Niveau_Sucre = " Moyen (10g)"
          } else if (Sucre == 4) {
            Niveau_Sucre = "Beaucoup (15g)"
          }

          // Gestion quantité sucre ajouté
          if (Sucre == 1) { // Si l'utilisateur chosit "Sans sucre", aucun sucre n'est ajouté . Quantité_Sucre_add = 0
            Quantite_Sucre_Add = 0
            StockSucre -= Quantite_Sucre_Add
          }
          else {
            Quantite_Sucre_Add = (Sucre - 1) * DoseSucre
            StockSucre -= Quantite_Sucre_Add
          }

          //Gestion des boissons Expresso
          if (TypeBoisson == 1) {

            Boisson_Selectionnee = "Expresso"
            if (PoudreCafe >= 8 && (StockSucre >= Quantite_Sucre_Add)) {
              PoudreCafe -= 8
              println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre)
              PrixSucre *= (Sucre - 1)
              PrixTotal = PrixBaseExpresso + PrixSucre
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseExpresso, PrixSucre, PrixTotal)
              // Interface paiement
              println("Veuillez payer en utilsant Twint.")
              val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
              val charsSize = chars.length
              var pin = ""
              for (i <- 1 to 5) {
                val twint = (Math.random() * charsSize).toInt
                pin += chars(twint)
              }
              println("Votre code de paiement est :" + pin)
              println("(En attente de validation du paiement...)")
              Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
              println("Merci ! Votre paiement a été accepté.")

              // Préparation de la boisson
              println("Préparation de votre boisson ... \n [...] \n Votre" + TypeBoisson + " est prêt ! Bonne dégustation ! ")
              mode = 0
            }
            else {
              if (PoudreCafe < 8) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              else if (StockSucre < Quantite_Sucre_Add) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionée.\n Veuillez choisir une autre boisson ou vérifier les stocks en Mode Admin.")
              }
            }
          }

          // Cas si boisson est un cappuccino ou un latte : Deamnde supp lait
          if (TypeBoisson == 2 || TypeBoisson == 3) {
            mode = 0
            println(" Souhaitez-vous ajouter du lait en supplément ? \n ( Disponible uniquemnt pour Cappucino et Latte)\n 1) Oui \n 2) Non\n >")
            lait = readInt()
          }
          if (lait == 1) {
            while (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {
              println("Combien de dose ? \n >")
              DoseLait = readInt()
              if (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {
                println("Veuillez choisir une dose de lait supplémentaire valide")

              }
            }
            // Gestion niveau de Lait utilisé
            if (lait == 1) {
              Lait_Supp = " Oui "
            } else if (lait == 2) {
              Lait_Supp = " Non "
            }
            // Gestion quantité lait ajouté
            if (DoseLait == 1 && TypeBoisson == 2) {
              Quantite_Lait_Cappuccino = 0.100 + 0.05
              StockLait -= Quantite_Lait_Cappuccino
            }
            else if (DoseLait == 2) {
              Quantite_Lait_Cappuccino = 0.100 + (0.05 * 2)
              StockLait -= Quantite_Lait_Cappuccino
            }
            else if (DoseLait == 3) {
              Quantite_Lait_Cappuccino = 0.100 + (0.05 * 3)
              StockLait -= Quantite_Lait_Cappuccino
            }
            else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 1) {
              Quantite_Lait_Petit_Latte = 0.120 + 0.05
              StockLait -= Quantite_Lait_Petit_Latte
            }
            else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 2) {
              Quantite_Lait_Moyen_Latte = 0.150 + 0.05
              StockLait -= Quantite_Lait_Moyen_Latte
            }
            else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 3) {
              Quantite_Lait_Grand_Latte = 0.200 + 0.05
              StockLait -= Quantite_Lait_Grand_Latte
            }
            else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 1) {
              Quantite_Lait_Grand_Latte = 0.120 + (0.05 * 2)
              StockLait -= Quantite_Lait_Petit_Latte
            }
            else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 2) {
              Quantite_Lait_Moyen_Latte = 0.150 + (0.05 * 2)
              StockLait -= Quantite_Lait_Moyen_Latte
            }
            else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 3) {
              Quantite_Lait_Grand_Latte = 0.200 + (0.05 * 2)
              StockLait -= Quantite_Lait_Grand_Latte
            }
            else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 1) {
              Quantite_Lait_Grand_Latte = 0.120 + (0.05 * 3)
              StockLait -= Quantite_Lait_Petit_Latte
            }
            else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 2) {
              Quantite_Lait_Moyen_Latte = 0.150 + (0.05 * 3)
              StockLait -= Quantite_Lait_Moyen_Latte
            }
            else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 3) {
              Quantite_Lait_Grand_Latte = 0.200 + (0.05 * 3)
              StockLait -= Quantite_Lait_Grand_Latte
            }
          }



          // Gestion des boissons Cappuccino
          if (TypeBoisson == 2) {
            Boisson_Selectionnee = "Cappuccino"
            if (PoudreCafe >= 6 && (StockSucre >= Quantite_Sucre_Add) && (StockLait >= Quantite_Lait_Cappuccino)) {
              PoudreCafe -= 6
              println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
              PrixSucre *= (Sucre - 1)
              PrixTotal = PrixBaseCappuccino + PrixSucre
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseCappuccino, PrixSucre, PrixTotal)
              // Interface paiement
              println("Veuillez payer en utilsant Twint.")
              val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
              val charsSize = chars.length
              var pin = ""
              for (i <- 1 to 5) {
                val twint = (Math.random() * charsSize).toInt
                pin += chars(twint)
              }
              println("Votre code de paiement est :" + pin)
              println("(En attente de validation du paiement...)")
              Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
              println("Merci ! Votre paiement a été accepté.")

              // Préparation de la boisson
              println("Préparation de votre boisson ... \n [...] \n Votre" + TypeBoisson + " est prêt ! Bonne dégustation ! ")
              mode = 0

            }

            else {
              if (PoudreCafe < 6) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              else if (StockSucre < Quantite_Sucre_Add) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              else if (StockLait < Quantite_Lait_Cappuccino) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
            }

          }


          // Gestion commande petit Latte
          if (TailleLatte == 1 && TypeBoisson == 3) {
            Boisson_Selectionnee = "Latte (petit)"
            if (PoudreCafe >= 6 && (StockSucre >= Quantite_Sucre_Add) && (StockLait >= Quantite_Lait_Petit_Latte)) {
              PoudreCafe -= 6
              println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
              PrixSucre *= (Sucre - 1)
              PrixTotal = PrixPetitLatte + PrixSucre
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixPetitLatte, PrixSucre, PrixTotal)
              // Interface paiement
              println("Veuillez payer en utilsant Twint.")
              val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
              val charsSize = chars.length
              var pin = ""
              for (i <- 1 to 5) {
                val twint = (Math.random() * charsSize).toInt
                pin += chars(twint)
              }
              println("Votre code de paiement est :" + pin)
              println("(En attente de validation du paiement...)")
              Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
              println("Merci ! Votre paiement a été accepté.")

              // Préparation de la boisson
              println("Préparation de votre boisson ... \n [...] \n Votre" + TypeBoisson + " est prêt ! Bonne dégustation ! ")
              mode = 0

            }

            else {
              if (PoudreCafe < 6) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              else if (StockSucre < Quantite_Sucre_Add) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              else if (StockLait < Quantite_Lait_Petit_Latte) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }

              // Gestion commande moyen Latte
              if (TailleLatte == 2 && TypeBoisson == 3) {
                Boisson_Selectionnee = "Latte (moyen)"
                if (PoudreCafe >= 8 && (StockSucre >= Quantite_Sucre_Add) && (StockLait >= Quantite_Lait_Moyen_Latte)) {
                  PoudreCafe -= 8
                  println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                  PrixSucre *= (Sucre - 1)
                  PrixTotal = PrixMoyenLatte + PrixSucre
                  printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixMoyenLatte, PrixSucre, PrixTotal)
                  // Interface paiement
                  println("Veuillez payer en utilsant Twint.")
                  val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                  val charsSize = chars.length
                  var pin = ""
                  for (i <- 1 to 5) {
                    val twint = (Math.random() * charsSize).toInt
                    pin += chars(twint)
                  }
                  println("Votre code de paiement est :" + pin)
                  println("(En attente de validation du paiement...)")
                  Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                  println("Merci ! Votre paiement a été accepté.")

                  // Préparation de la boisson
                  println("Préparation de votre boisson ... \n [...] \n Votre" + TypeBoisson + " est prêt ! Bonne dégustation ! ")
                  mode = 0

                }

                else {
                  if (PoudreCafe < 8) {
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                  else if (StockSucre < Quantite_Sucre_Add) {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                  else if (StockLait < Quantite_Lait_Petit_Latte) {
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin.")
                  }

                  // Gestion commande Grand Latte
                  if (TailleLatte == 3 && TypeBoisson == 3) {
                    Boisson_Selectionnee = "Latte (moyen)"
                    if (PoudreCafe >= 12 && (StockSucre >= Quantite_Sucre_Add) && (StockLait >= Quantite_Lait_Grand_Latte)) {
                      PoudreCafe -= 12
                      println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                      PrixSucre *= (Sucre - 1)
                      PrixTotal = PrixMoyenLatte + PrixSucre
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixMoyenLatte, PrixSucre, PrixTotal)
                      // Interface paiement
                      println("Veuillez payer en utilsant Twint.")
                      val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                      val charsSize = chars.length
                      var pin = ""
                      for (i <- 1 to 5) {
                        val twint = (Math.random() * charsSize).toInt
                        pin += chars(twint)
                      }
                      println("Votre code de paiement est :" + pin)
                      println("(En attente de validation du paiement...)")
                      Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                      println("Merci ! Votre paiement a été accepté.")

                      // Préparation de la boisson
                      println("Préparation de votre boisson ... \n [...] \n Votre" + TypeBoisson + " est prêt ! Bonne dégustation ! ")
                      mode = 0

                    }

                    else {
                      if (PoudreCafe < 8) {
                        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                      else if (StockSucre < Quantite_Sucre_Add) {
                        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                      else if (StockLait < Quantite_Lait_Petit_Latte) {
                        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                    }
                  }


                // Mode Admin
                while (mode == 2) {
                  mode = 0
                  println("Mode Admin")
                  val PIN_Defaut = "434343"
                  var Statut_acces = false
                  println("Entrer le code PIN : ******")
                  val PIN_entre = readLine()
                  if (PIN_entre == PIN_Defaut) {
                    Statut_acces = true
                    println("Accès autorisé.")
                    println("\n")
                    println("Stocks :")
                    println("Poudre de café:" + PoudreCafe + "g")
                    println("Lait:" + StockLait + "L")
                    println("Sucre:" + StockSucre + "g")
                    println("\n")
                    println("Réapprovisionnement des stocks....\n Ajout :")
                    println("Poudre de café:" + (50 - PoudreCafe))
                    println("Lait:" + (0.5 - StockLait))
                    println("Sucre:" + (30 - StockSucre))
                    println("Niveaux de stock mis à jour." + "Retour au menu principal...")

                    mode = 0
}
                }


                // Mode Quitter
                if (mode == 3) {
                  TypeBoisson = 0
                  println(" Merci d'avoir utilisé Nospresso Café !")

                }
              }
            }
          }
        }

      }
    }
            continuer = false
          }



    }






}
