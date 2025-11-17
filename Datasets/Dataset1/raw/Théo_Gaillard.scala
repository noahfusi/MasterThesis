import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //On définit des valeurs pour plus tard
    //var des stocks
    var stockcafe: Int = 50
    var stocksucre = 30
    var stocklait = 0.500
    //on as pas vraiment d'info sur le taux d'argent du client donc je met ici 5 chf
    var argent = 5.00
    //var choix servent pour sélectionner dans les menus plus tard et quantité
    var choixboisson = 0
    var choixtaille = 0
    var choixsucre = 0
    var choixlait = 0
    var quantitelait = 0
    //les 3 var prix ici servent à être additionner plus tard et à être afficher dans le résultat "Prix total"
    var prixboisson = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    //on met des quantité provisoires que on soustrait uniquement à la fin et qui permet de vérifier les quantités
    var dosecafe = 0
    var doselait = 0.00
    var dosesucre = 0
    //on crée des variables sur les rajouts de stocks
    var rajoutcafe = 0
    var rajoutlait = 0.000
    var rajoutsucre = 0
    //une crée une variable pour avoir une boucle sur le menu principal
    var lancement = true

    while (lancement) {
      //on remet la var du prix, comme ca lorsque le paiement se relance après une erreur le compte recommence à 0
      var prix = 0.00
      //la même avec le prix de chaque partie
      var prixboisson = 0.00
      var prixsucre = 0.00
      var prixlait = 0.00
      //on reset aussi les doses provisoires
      var dosecafe = 0
      var doselait = 0.00
      var dosesucre = 0

      //pour chaque boucle on aura un code twint pour le paiement
      //on crée un code Twint aléatoire alphanumérique de 5 caractères
      def alphanumeric(length: Int): String = {
        val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        (1 to length).map(_ => caracteres(Random.nextInt(caracteres.length))).mkString
      }

      val codetwint = alphanumeric(5)

      println("\n        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      //la première étape est de permettre à l'utilisateur de choisir un mode
      var choix = readLine(">").toInt
      //on doit refusser ce qui ne fais pas partie des trois choix
      while (!(choix == 1) && !(choix == 2) && !(choix == 3)) {
        println("Commande pas reconnu, veuillez ressayer SVP")
        choix = readLine(">").toInt
      }

      //Maintenant il faut lier la commande tapé aux trois options
      if (choix == 1 /*client*/ ) {

        //on peut faire comme pour le choix de mode mais avec les boissons
        println("\nVeuillez sélectionn votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappucino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
        var choixboisson = readLine(">").toInt
        //comment avant on limite les choix à ce qui est possible
        while (!(choixboisson == 1) && !(choixboisson == 2) && !(choixboisson == 3)) {
          println("Commande pas reconnu, veuillez ressayer SVP")
          choixboisson = readLine(">").toInt
        }

        //pour le latte on permet le choix de taille
        if (choixboisson == 3) /*latte*/ {
          //on pose la question uniquement pour le 3 car c'est le seul avec plusieurs taille
          println("\nQuelle taille ?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          var choixtaille = readLine(">").toInt
          while (!(choixtaille == 1) && !(choixtaille == 2) && !(choixtaille == 3)) {
            println("Commande pas reconnu, veuillez ressayer SVP")
            choixtaille = readLine(">").toInt
          }
        }

        println("\nSouhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        var choixsucre = readLine(">").toInt
        while (!(choixsucre == 1) && !(choixsucre == 2) && !(choixsucre == 3) && !(choixsucre == 4)) {
          println("Commande pas reconnu, veuillez ressayer SVP")
          choixsucre = readLine(">").toInt
        }

        //on permet aussi pour le lait, mais uniquement  pour le cappuccino et le latte
        if ((choixboisson == 2) || (choixboisson == 3)) {
          println("\nSouaitez-vous du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          choixlait = readLine(">").toInt
          //on limite sur deux choix
          while (!(choixlait == 1) && !(choixlait == 2)) {
            println("Choisissez entre 1 et 2")
            choixlait = readLine(">").toInt
          }
          //On continue que pour Oui
          if (choixlait == 1) {
            //on permet de mettre des doses de lait
            println("\nCombien de dose ?")
            quantitelait = readLine(">").toInt
            //on met des limites
            while (quantitelait < 0 || quantitelait > 3) {
              if (quantitelait < 0) {
                println("La quantité doit être positive")
                quantitelait = readLine(">").toInt
              } else if (quantitelait > 3) {
                println("Vous ne pouvez pas prendre plus de 3 doses")
                quantitelait = readLine(">").toInt
              }
            }
          }else{
            //on ne mets rien de plus
          }
        }
        //on affiche les choix finaux des boissons en premier et on compte la quantité de café et de lait
        if (choixboisson == 1) {
          println("\nBoisson sélectionnée : Expresso")
          prixboisson += 2.00
          dosecafe += 8
        } else if (choixboisson == 2) {
          println("\nBoisson sélectionnée : Cappucino")
          prixboisson += 2.50
          dosecafe += 6
          doselait += 0.100
        } else {
          //pour le latte la taille est aussi afficher donc on doit permettre cela
          if (choixtaille == 1) {
            println("\nBoisson sélectionnée : Latte (Petit)")
            prixboisson += 2.70
            dosecafe += 6
            doselait += 0.120
          } else if (choixtaille == 2) {
            println("\nBoisson sélectionnée : Latte (Moyen)")
            prixboisson += 3.20
            dosecafe += 8
            doselait += 0.150
          } else {
            println("\nBoisson sélectionnée : Latte (Grand)")
            prixboisson += 3.70
            dosecafe += 12
            doselait += 0.200
          }
        }
        //ensuite on affiche le niveau de sucre et cpmpte les doses aussi
        if (choixsucre == 1) {
          println("Niveau de sucre : Sans Sucre")
        } else if (choixsucre == 2) {
          println("Niveau de sucre : Peu (5g)")
          prixsucre = 0.10
          dosesucre = 5
        } else if (choixsucre == 3) {
          println("Niveau de sucre : Moyen (10g)")
          prixsucre = 0.20
          dosesucre = 10
        } else {
          println("Niveau de sucre : Beaucoup (15g)")
          prixsucre = 0.30
          dosesucre = 15
        }
        //Même choix avec le lait toujours que pour 2 et 3
        if ((choixboisson == 2) || (choixboisson == 3)) {
          if (choixlait == 1 && quantitelait > 0) {
            if (quantitelait == 1) {
              println("Lait supplémentaire: Oui (1 dose)")
              prixlait += 0.05
              doselait += 0.050
            } else if (quantitelait == 2) {
              println("Lait supplémentaire: Oui (2 dose)")
              prixlait += 0.10
              doselait += 0.100
            } else {
              println("Lait supplémentaire: Oui (3 dose)")
              prixlait += 0.15
              doselait += 0.150
            }
          } else{
            println("Lait supplémentaire: Non")
          }
        }

        //on affiche maintenant le prix et check les quantités
        if (choixboisson == 1 /*expresso*/ ) {
          //on doit d'abord check les quantités car si il n'y a pas assez cela ne propose pas le prix et quitte le programme
          if (stockcafe < dosecafe) {
            println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.")
            println("Veuillez choisir une autre boisson ou vérifier les \nstocks en mode Admin")
          } else if (stockcafe >= dosecafe) {
            //on regarde maintenant pour si il y du sucre ou non. On modifie le prix et l'affichage
            //si il n'y a pas assez de sucre on arrête le programme
            if (stocksucre < dosesucre) {
              println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.")
              println("Veuillez choisir moins de sucre.")
            } else {
              prix += prixboisson + prixsucre //on additonne le prix total
              if (choixsucre == 1) { //on doit faire de manière différente si il y a ou non du sucre en plus
                printf("Prix total : CHF %.2f", prix)
              } else {
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
              }
            }
          }
          //permet maintenant le paiment
          println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint)
          println("(En attente de validation du paiement...)")
          //on fait attendre 3 secondes (3000 millisecondes)
          Thread.sleep(3000)
          //on check si le paiment est accepté
          if (prix <= argent) {
            println("\nPaiement confirmé.\nPréparation de votre boisson...")
            //on attends 5 secondes cette fois pour que le café se fasse
            Thread.sleep(5000)
            println("Votre Expresso est prêt ! Bonne déustation !")

            //on enlève que maintenant les quantités
            stockcafe -= dosecafe
            stocklait -= doselait
            stocksucre -= dosesucre
          } else /*on ajoute le résultat si il n'y a pas assez d'argent*/ {
            println("\nPaiement refusé.\nRedémarrage de la machine.")
          }

        } else if (choixboisson == 2 /*cappuccino*/ ) {
          if (stockcafe < dosecafe) {
            println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.")
            println("Veuillez vérifier les stocks en mode Admin")
          } else {
            if (stocklait < doselait) { //ici on doit aussi vérifier pour le lait
              println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.")
              println("Veuillez essayer une autre boisson.")
            } else {
              if (stocksucre < dosesucre) { //on refait avec le sucre
                println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.")
                println("Veuillez choisir moins de sucre.")
              } else {
                prix += prixboisson + prixsucre + prixlait //on additonne le prix total avec le lait en plus
                if (choixsucre == 1) { //on doit faire de manière différente si il y a ou non du sucre en plus
                  if (choixlait == 1) { //cette fois-ci on doit faire le lait aussi, l'afficher ou non
                    printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                  } else {
                    printf("Prix total : CHF %.2f", prix)
                  }
                } else {
                  if (choixlait == 1) {
                    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                  } else {
                    printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                  }
                }
              }
            }
          }
          println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          if (prix <= argent) {
            println("\nPaiement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(5000)
            println("Votre Cappuccino est prêt ! Bonne déustation !")

            //on enlève que maintenant les quantités
            stockcafe -= dosecafe
            stocklait -= doselait
            stocksucre -= dosesucre
          } else /*on ajoute le résultat si il n'y a pas assez d'argent*/ {
            println("\nPaiement refusé.\nRedémarrage de la machine.")
          }

        } else /*latte*/ {
          if (choixtaille == 1) {
            if (stockcafe < dosecafe) {
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.")
              println("Veuillez vérifier les stocks en mode Admin")
            } else {
              if (stocklait < doselait) {
                println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.")
                println("Veuillez choisir une autre boisson.")
              } else {
                if (stocksucre < dosesucre) {
                  println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.")
                  println("Veuillez choisir moins de sucre.")
                } else {
                  prix += prixboisson + prixsucre + prixlait
                  if (choixsucre == 1) {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                    } else {
                      printf("Prix total : CHF %.2f", prix)
                    }
                  } else {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                    } else {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                    }
                  }
                }
              }
            }
          } else if (choixtaille == 2) {
            if (stockcafe < dosecafe) {
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.")
              println("Veuillez choisir une taille plus petite ou essayer \nune autre boisson.")
            } else {
              if (stocklait < doselait) {
                println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.")
                println("Veuillez choisir une taille plus petite ou essayer \nune autre boisson.")
              } else {
                if (stocksucre < dosesucre) {
                  println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.")
                  println("Veuillez choisir moins de sucre.")
                } else {
                  prix += prixboisson + prixsucre + prixlait
                  if (choixsucre == 1) {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                    } else {
                      printf("Prix total : CHF %.2f", prix)
                    }
                  } else {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                    } else {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                    }
                  }
                }
              }
            }
          } else {
            if (stockcafe < dosecafe) {
              println("\nErreur : Quantité de poudre de café insuffisante pour préparer \nla boisson sélectionmée.")
              println("Veuillez choisir une taille plus petite ou essayer \nune autre boisson.")
            } else {
              if (stocklait < doselait) {
                println("\nErreur : Quantité de lait insuffisante pour préparer \nla boisson sélectionmée.")
                println("Veuillez choisir une taille plus petite ou essayer \nune autre boisson.")
              } else {
                if (stocksucre < dosesucre) {
                  println("\nErreur : Quantité de sucre insuffisante pour préparer \nla boisson sélectionmée.")
                  println("Veuillez choisir moins de sucre.")
                } else {
                  prix += prixboisson + prixsucre + prixlait
                  if (choixsucre == 1) {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
                    } else {
                      printf("Prix total : CHF %.2f", prix)
                    }
                  } else {
                    if (choixlait == 1) {
                      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prixsucre, prix)
                    } else {
                      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
                    }
                  }
                }
              }
            }
            println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiment est : " + codetwint)
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            if (prix <= argent) {
              println("\nPaiement confirmé.\nPréparation de votre boisson...")
              Thread.sleep(5000)
              println("Votre Latte est prêt ! Bonne déustation !")

              //on enlève que maintenant les quantités par taille ici
              if (choixtaille == 1) {
                stockcafe -= dosecafe
                stocklait -= doselait
                stocksucre -= dosesucre
              } else if (choixtaille == 2) {
                stockcafe -= dosecafe
                stocklait -= doselait
                stocksucre -= dosesucre
              } else {
                stockcafe -= dosecafe
                stocklait -= doselait
                stocksucre -= dosesucre
              }
            } else {
              /*on ajoute le résultat si il n'y a pas assez d'argent*/
              println("\nPaiement refusé.\nRedémarrage de la machine.")
            }
          }
        }
      } else if (choix == 2 /*admin*/ ) {
        println("\nMode Admin")
        var code = readLine("Entrez le code PIN :") //pour être admin il faut entrer un code de 6 caractères (434343)
        while (!(code == "434343")) { //pas de limite d'essais donné donc on permet en boucle
          println("Accès refusé")
          var code = readLine("Entrez le code PIN :")
        }
        if (code == "434343") {
          println("Accès autorisé")
          //vu que le code est correct on peut permettre à l'admin d'ajouter des stocks
          println("\nStocks") //on commence par afficher l'état actuel des stocks
          println("   Poudre de café: " + stockcafe + "g")
          println("   Lait          : " + stocklait + "l")
          println("   Sucre         : " + stocksucre + "g")
          println("\nRéapprovisionnement des stocks...")
          println("Ajout :")
          //On laisse la sélection des quantités
          rajoutcafe = readLine("   Poudre de café: ").toInt
          rajoutlait = readLine("   Lait          : ").toDouble
          rajoutsucre = readLine("   Sucre         : ").toInt
          //On fait maintenant le calcul du total
          stockcafe += rajoutcafe
          stocklait += rajoutlait
          stockcafe += rajoutsucre
          //on remet les rajouts à 0
          rajoutcafe = 0
          rajoutlait = 0
          rajoutcafe = 0
          println("Niveaux de stock mis à jour")
          println("Retour au menu principal")
          //le programme prends donc fin
        }
      } else /*quitter*/ {
        lancement = false //on quitte la boucle pour eteindre
      }
    }
  }
 }
