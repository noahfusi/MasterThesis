import io.StdIn._
import math._

object Main {
  def main(args: Array[String]): Unit = {

    var machine = 0
    var choixcafe = 0
    var affichage = 1
    var mode = 0

    var boisson = "café"
    var ajsucre = "avec"
    var ajlait = "combien de doses"

    var boissonprix = 0.00
    var prixfinal = 0.00
    var ajsucreprix = 0.00
    var ajlaitprix = 0.00
    var choixsucre = 0
    var choixlait = 0
    var quantitesucre = 0
    var doselait = 0
    var quantitelait = 0.00
    var client = 0

    //Stocks initiaux
    var poudrecafe = 50 //en grammes
    var sucre = 30 //en grammes
    var lait = 0.500 //en litres

    //Affichage d'acceuil de la machine
    while(machine == 0) {
      println("\nNospresso café")
      var mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n >").toInt

      //Mode client pour commander le café
      if (mode == 1) {
        while (client == 0) {
          choixcafe = readLine("Veuillez sélectionné votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n > ").toInt

          //Si client choisi esspresso
          if (choixcafe == 1) {
            var PCesspresso = 8
            boissonprix = 2.00
            prixfinal = boissonprix
            boisson = "Expresso"
            choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt

            //Si poudre de café (PC) en stock est >= à PC esspresso
            if (poudrecafe >= PCesspresso) {
              if (choixsucre == 1) {
                quantitesucre = 0
                ajsucre = "Sans sucre"

              } else if (choixsucre == 2) {
                quantitesucre = 5
                ajsucreprix = 0.10
                ajsucre = "Peu (5g)"
                prixfinal += ajsucreprix

              } else if (choixsucre == 3) {
                quantitesucre = 10
                ajsucreprix = 0.20
                ajsucre = "Moyen (10g)"
                prixfinal += ajsucreprix

              } else if (choixsucre == 4) {
                quantitesucre = 15
                ajsucreprix = 0.30
                ajsucre = "Beaucoup (15g)"
                prixfinal += ajsucreprix
              }

              //Si pas assez de sucre mais assez de poudre à café
              if ((quantitesucre <= sucre) && (poudrecafe >= PCesspresso)) {
                sucre -= quantitesucre
                poudrecafe -= PCesspresso
                client = 1

              } else if (quantitesucre > sucre) {
                affichage = 0
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
              }

            } else if (poudrecafe < PCesspresso) {
              affichage = 0
              println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
            }

            //Si client choisi Capuccino
          } else if (choixcafe == 2) {
            var PCcapuccino = 6
            var laitcapuccino = 0.100
            var boissonprix = 2.50
            prixfinal = boissonprix
            boisson = "Capuccino"
            choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
            choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt

            //Choix du lait à ajouter (même chose uniquement Cappucino et Latte)
            if (choixlait == 1) {
              doselait = readLine("Combien de dose ?\n > ").toInt

              if (doselait == 1) {
                quantitelait = 0.1 + 0.05
                ajlait = "1 dose de lait (0.05 L)"
                ajlaitprix = 0.05 //CHF
                prixfinal += ajlaitprix

              } else if (doselait == 2) {
                quantitelait = 0.1 + 0.10
                ajlait = "2 dose de lait (0.1 L)"
                ajlaitprix = 0.10 //CHF
                prixfinal += ajlaitprix

              } else if (doselait == 3) {
                quantitelait = 0.1 + 0.15
                ajlait = "3 dose de lait (0.15 L)"
                ajlaitprix = 0.15 //CHF
                prixfinal += ajlaitprix

              } else {
                println("Erreur : La quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                doselait = readLine("Combien de dose ?\n > ").toInt
              }

            } else {
              quantitelait = 0.100
              ajlait = "Non"
            }

            //Choix de sucre
            if (poudrecafe >= PCcapuccino) {
              if (lait >= quantitelait) {
                if (choixsucre == 1) {
                  quantitesucre = 0
                  ajsucre = "Sans sucre"

                } else if (choixsucre == 2) {
                  quantitesucre = 5
                  ajsucreprix = 0.10
                  ajsucre = "Peu (5g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 3) {
                  quantitesucre = 10
                  ajsucreprix = 0.20
                  ajsucre = "Moyen (10g)"
                  prixfinal += ajsucreprix

                } else if (choixsucre == 4) {
                  quantitesucre = 15
                  ajsucreprix = 0.30
                  ajsucre = "Beaucoup (15g)"
                  prixfinal += ajsucreprix
                }
                //Si quantités sucre lait et poudre à café sont OK
                if ((sucre >= quantitesucre) && (lait >= quantitelait) && (poudrecafe >= PCcapuccino)) {
                  sucre -= quantitesucre
                  poudrecafe -= PCcapuccino
                  lait -= quantitesucre
                  client = 1
                } else {
                  affichage = 0
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                }
              } else {
                affichage = 0
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone.")
              }
            } else {
              println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
            }

            //Si client choisi Latte
          } else if (choixcafe == 3) {
            var taille = readLine("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70").toInt
            choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
            choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt

            //Choix des différentes tailles pour le Latte (petit, moyen ou grand)
            //Si taille du Latte --> Petit
            if (taille == 1) {
              var PCpetitlatte = 6
              var laitpetitlatte = 0.120
              boisson = "Latte (Petit)"
              boissonprix = 2.70
              prixfinal = boissonprix

              //Choix du lait à ajouter (même chose uniquement pour Capuccino et Latte)
              if (choixlait == 1) {
                doselait = readLine("Combien de dose ?\n > ").toInt
                if (doselait == 1) {
                  quantitelait = laitpetitlatte + 0.05
                  ajlait = "1 dose de lait (0.05 L)"
                  ajlaitprix = 0.05
                  prixfinal += ajlaitprix

                } else if (doselait == 2) {
                  quantitelait = laitpetitlatte + 0.10
                  ajlait = "2 dose de lait (0.1 L)"
                  ajlaitprix = 0.10
                  prixfinal += ajlaitprix

                } else if (doselait == 3) {
                  quantitelait = laitpetitlatte + 0.15
                  ajlait = "3 dose de lait (0.15 L)"
                  ajlaitprix = 0.15
                  prixfinal += ajlaitprix

                } else {
                  println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                  doselait = readLine("Combien de dose ?\n > ").toInt
                }

              } else {
                quantitelait = 0.100
                ajlait = "Non"
              }
              if (poudrecafe >= PCpetitlatte) {
                if (lait >= quantitelait) {
                  if (choixsucre == 1) {
                    quantitesucre = 0
                    ajsucre = "Sans sucre"

                  } else if (choixsucre == 2) {
                    quantitesucre = 5
                    ajsucreprix = 0.10
                    ajsucre = "Peu (5g)"
                    prixfinal += ajsucreprix

                  } else if (choixsucre == 3) {
                    quantitesucre = 10
                    ajsucreprix = 0.20
                    ajsucre = "Moyen (10g)"
                    prixfinal += ajsucreprix

                  } else if (choixsucre == 4) {
                    quantitesucre = 15
                    ajsucreprix = 0.30
                    ajsucre = "Beaucoup (15g)"
                    prixfinal += ajsucreprix
                  }
                  if ((sucre >= quantitesucre) && (poudrecafe >= PCpetitlatte) && (lait >= quantitelait)) {
                    sucre -= quantitesucre
                    poudrecafe -= PCpetitlatte
                    lait -= quantitelait
                    client = 1
                  } else {
                    affichage = 0
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                  }

                } else {
                  affichage = 0
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                }

              } else {
                println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
              }

              //Si taille du Latte --> Moyen (pareil que petit mais avec variable et prix différent
            } else if (taille == 2) {
              var PCmoyenlatte = 8
              var laitmoyenlatte = 0.150
              boisson = "Latte (Moyen)"
              boissonprix = 3.20
              prixfinal = boissonprix
              if (choixlait == 1) {
                doselait = readLine("Combien de dose ?\n > ").toInt
                if (doselait == 1) {
                  quantitelait = laitmoyenlatte + 0.05
                  ajlait = "1 dose de lait (0.05 L)"
                  ajlaitprix = 0.05
                  prixfinal += ajlaitprix

                } else if (doselait == 2) {
                  quantitelait = laitmoyenlatte + 0.10
                  ajlait = "2 dose de lait (0.1 L)"
                  ajlaitprix = 0.10
                  prixfinal += ajlaitprix

                } else if (doselait == 3) {
                  quantitelait = laitmoyenlatte + 0.15
                  ajlait = "3 dose de lait (0.15 L)"
                  ajlaitprix = 0.15
                  prixfinal += ajlaitprix

                } else {
                  println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                  doselait = readLine("Combien de dose ?\n > ").toInt
                }

              } else {
                quantitelait = 0.150
                ajlait = "Non"
              }
              if (poudrecafe >= PCmoyenlatte) {
                if (lait >= quantitelait) {
                  if (choixsucre == 1) {
                    quantitesucre = 0
                    ajsucre = "Sans sucre"

                  } else if (choixsucre == 2) {
                    quantitesucre = 5
                    ajsucreprix = 0.10
                    ajsucre = "Peu (5g)"
                    prixfinal += ajsucreprix

                  } else if (choixsucre == 3) {
                    quantitesucre = 10
                    ajsucreprix = 0.20
                    ajsucre = "Moyen (10g)"
                    prixfinal += ajsucreprix

                  } else if (choixsucre == 4) {
                    quantitesucre = 15
                    ajsucreprix = 0.30
                    ajsucre = "Beaucoup (15g)"
                    prixfinal += ajsucreprix
                  }

                  if ((sucre >= quantitesucre) && (poudrecafe >= PCmoyenlatte) && (lait >= quantitelait)) {
                    sucre -= quantitesucre
                    poudrecafe -= PCmoyenlatte
                    lait -= quantitelait
                    client = 1
                  } else {
                    affichage = 0
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                  }

                } else {
                  affichage = 0
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                }
              } else {
                println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
              }

              //Si taille du Latte --> Grand (même chose encore une fois)
            } else if (taille == 3) {
              var PCgrandlatte = 12
              var laitgrandlatte = 0.200
              boisson = "Latte (Grand)"
              boissonprix = 3.70
              prixfinal = boissonprix
              if (choixlait == 1) {
                doselait = readLine("Combien de dose ?\n > ").toInt

                if (doselait == 1) {
                  quantitelait = laitgrandlatte + 0.05
                  ajlait = "1 dose de lait (0.05 L)"
                  ajlaitprix = 0.05
                  prixfinal += ajlaitprix

                } else if (doselait == 2) {
                  quantitelait = laitgrandlatte + 0.10
                  ajlait = "2 dose de lait (0.1 L)"
                  ajlaitprix = 0.10
                  prixfinal += ajlaitprix

                } else if (doselait == 3) {
                  quantitelait = laitgrandlatte + 0.15
                  ajlait = "3 dose de lait (0.15 L)"
                  ajlaitprix = 0.15
                  prixfinal += ajlaitprix

                } else {
                  println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                  doselait = readLine("Combien de dose ?\n > ").toInt
                }
              } else {
                quantitelait = 0.100
                ajlait = "Non"
              }
              if (poudrecafe >= PCgrandlatte) {
                if (lait >= quantitelait) {
                  if (choixsucre == 1) {
                    quantitesucre = 0
                    ajsucre = "Sans sucre"
                  } else if (choixsucre == 2) {
                    quantitesucre = 5
                    ajsucreprix = 0.10
                    ajsucre = "Peu (5g)"
                    prixfinal += ajsucreprix
                  } else if (choixsucre == 3) {
                    quantitesucre = 10
                    ajsucreprix = 0.20
                    ajsucre = "Moyen (10g)"
                    prixfinal += ajsucreprix
                  } else if (choixsucre == 4) {
                    quantitesucre = 15
                    ajsucreprix = 0.30
                    ajsucre = "Beaucoup (15g)"
                    prixfinal += ajsucreprix
                  }
                  if ((sucre >= quantitesucre) && (poudrecafe >= PCgrandlatte) && (lait >= quantitelait)) {
                    sucre -= quantitesucre
                    poudrecafe -= PCgrandlatte
                    lait -= quantitelait
                    client = 1
                  } else {
                    affichage = 0
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                  }
                } else {
                  affichage = 0
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
                }
              } else {
                println(" Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boissone ou vérifier les stocks en mode Admin.")
              }
            }
          }
        }

        //Résumé de la commande pour chaque scénario de possible de commande
        //Si le choix du café est pour Capuccino ou Latte (tout sauf Esspresso)
        if (affichage == 1) {
          if (choixcafe != 1) {
            if ((choixlait == 1) && (choixsucre != 1)) {
              println("\nBoisson sélectionée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              println("Lait en supplément : " + ajlait)
              printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, ajlaitprix, prixfinal)
            } else if ((choixlait == 2) && (choixsucre != 1)) {
              println("\nBoisson sélectionée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              println("Lait en supplément : " + ajlait)
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
            } else if ((choixlait == 1) && (choixsucre == 1)) {
              println("\nBoisson sélectionée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              println("Lait en supplément : " + ajlait)
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajlaitprix, prixfinal)
            } else {
              println("\nBoisson sélectionée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              println("Lait en supplément : " + ajlait)
              printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
            }

            //Si choix du café est Esspresso
          } else if (choixcafe == 1) {
            if (choixsucre == 1) {
              println("\nBoisson sélectionnée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
            } else {
              println("\nBoisson sélectionnée : " + boisson)
              println("Niveau de sucre : " + ajsucre)
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
            }
          }

          //Paiement de la boisson avec Twint et génération du code alphanumérique
          var alphanumérique = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          var codetwint = ""
          for (i <- 1 to 5) {
            var index = (math.random() * 62).toInt
            codetwint += alphanumérique(index)
          }
          //Affichage correcte paiement Twint
          println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          println("Préparation de votre boisson...\n[...]\nVotre " + boisson + " est prêt ! Bonne dégustation !")
          client = 0
        }

        //Mode Admin avec vérification code PIN
      } else if (mode == 2) {
        var motdepasse = 434343
        println("Mode Admin")
        val pin = readLine("Entrez le code PIN : ").toInt
        if (motdepasse == pin) {
          println("Accès autorisé.")
          println("__________________________________________________________________")
          println("Stocks :")
          println("Poudre de café : " + poudrecafe)
          println("Lait : " + lait)
          println("Sucre : " + sucre)
          println("___________________________________________________________________")
          println("Réapprovisionnement des stocks...")
          println("Ajout :")
          poudrecafe += readLine("Poudre de café : ").toInt
          lait += readLine("Lait : ").toDouble
          sucre += readLine("Sucre : ").toInt
          println("Niveaux des stocks mis à jour.")
          println("__________________________________________________________________")
          println("Retour au menu principal...")
          Thread.sleep(1000)
        } else {
          println("Erreur : Code PIN incorrecte.\nRetour au menu principal...")
          Thread.sleep(1000)
        }

        //Mode Quitter
      } else if (mode == 3) {
        machine = 1
      }
      if((mode != 1) && (mode != 2) && (mode != 3)) {
        println("Erreur : Veuillez séléctionner parmi les 3 modes disponibles.")
        }

      }


    }
  }