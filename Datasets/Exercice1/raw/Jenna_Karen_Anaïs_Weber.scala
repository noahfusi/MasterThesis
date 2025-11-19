import scala.io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var boisson = 0
    var ajoutsucre = 0
    var ajoutlait = 0
    var choixboisson = "0"
    var niveausucre="0"
    var laitsup ="0"
    var prixtotal = 0.00
    var prix = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    var choix = 0
    var code = "0"
    var pin = 0
    var poudrecafe = 50
    var sucre = 30
    var lait = 0.500
    var quantitecafe = 0
    var quantitesucre = 0
    var quantitelait = 0.0
    var boucle = true
    var Quantitecafe = 0
    var Quantitesucre = 0
    var Quantitelait = 0.0

    while (boucle) {

    println("         Nospresso Café" )
    println(" Veuillez sélectionner votre mode : ")
    println(" 1) Client" )
    println(" 2) Admin" )
    println(" 3) Quitter" )
    println (" > ")

      choix = readLine().toInt


      while ((choix != 1) && (choix != 2) && (choix != 3)) {
        println("         Nospresso Café")
        println(" Veuillez sélectionner votre mode : ")
        println(" 1) Client")
        println(" 2) Admin")
        println(" 3) Quitter")
        println(" > ")
        choix = readLine().toInt

      }


      if (choix == 1) {

        println("Veuillez s'électionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        println("> ")
        boisson = readLine().toInt

        if (boisson == 3) {
          println("4) CHF 2.70 (Petit)")
          println("5) CHF 3.20 (Moyen)")
          println("6) CHF 3.70 (Grand) ")
          println("> ")
          boisson = readLine().toInt


          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          println("> ")
          ajoutsucre = readLine().toInt


          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          println("> ")
          ajoutlait = readLine().toInt
          if (ajoutlait == 1) {

            println("Combien de dose ? 1, 2 ou 3")
            println(">")

            ajoutlait = readLine().toInt

          } else {
            ajoutlait = 0
          }

          if (boisson == 1) {
            choixboisson = "Expresso"
          } else if (boisson == 2) {
            choixboisson = "Cappuccino"
          } else {
            choixboisson = "Latte"
          }

          if (ajoutsucre == 1) {
            niveausucre = "Sans sucre"
          } else if (ajoutsucre == 2) {
            niveausucre = "Peu (5g)"
          } else if (ajoutsucre == 3) {
            niveausucre = "Moyen (10g)"
          } else {
            niveausucre = "Beaucoup (15g)"
          }

          if (ajoutlait == 1) {
            laitsup = "Une dose"
          } else if (ajoutlait == 2) {
            laitsup = "Deux doses"
          } else if (ajoutlait == 3) {
            laitsup = "Trois doses"
          } else {
            laitsup = "Non"
          }

          if (boisson == 4) {
            //petit//

            Quantitecafe = (Quantitecafe - 6)

            if (ajoutsucre == 1) {
              Quantitesucre = (Quantitesucre - 0)
            } else if (ajoutsucre == 2) {
              Quantitesucre = (Quantitesucre - 5)
            } else if (ajoutsucre == 3) {
              Quantitesucre = (Quantitesucre - 10)
            } else {
              Quantitesucre = (Quantitesucre - 15)
            }

            if (ajoutlait == 1) {
              Quantitelait = (Quantitelait - 0.170)
            } else if (ajoutlait == 2) {
              Quantitelait = (Quantitelait - 0.220)
            } else if (ajoutlait == 3) {
              Quantitelait = (Quantitelait - 0.270)
            } else {
              Quantitelait = (Quantitelait - 0.120)
            }

          } else if (boisson == 5) {
            //moyen//

            Quantitecafe = (Quantitecafe - 8)

            if (ajoutsucre == 1) {
              Quantitesucre = (Quantitesucre - 0)
            } else if (ajoutsucre == 2) {
              Quantitesucre = (Quantitesucre - 5)
            } else if (ajoutsucre == 3) {
              Quantitesucre = (Quantitesucre - 10)
            } else {
              Quantitesucre = (Quantitesucre - 15)
            }

            if (ajoutlait == 1) {
              Quantitelait = (Quantitelait - 0.200)
            } else if (ajoutlait == 2) {
              Quantitelait = (Quantitelait - 0.250)
            } else if (ajoutlait == 3) {
              Quantitelait = (Quantitelait - 0.300)
            } else {
              Quantitelait = (Quantitelait - 0.150)
            }

          } else {
            //grand//

            Quantitecafe = (Quantitecafe - 12)

            if (ajoutsucre == 1 ) {  //sans sucre//
              Quantitesucre = (Quantitesucre - 0)
            } else if (ajoutsucre == 2) {
              Quantitesucre = (Quantitesucre - 5)
            } else if (ajoutsucre == 3) {
              Quantitesucre = (Quantitesucre - 10)
            } else {
              Quantitesucre = (Quantitesucre - 15)
            }

            if (ajoutlait == 1) {
              Quantitelait = (Quantitelait - 0.250)
            } else if (ajoutlait == 2) {
              Quantitelait = (Quantitelait - 0.300)
            } else if (ajoutlait == 3) {
              Quantitelait = (Quantitelait - 0.350)
            } else {
              Quantitelait = (Quantitelait - 0.200)
            }

          }

          if (-Quantitesucre > sucre) {
            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre + "\n")
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")


          } else if (-Quantitelait > lait) {
            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre)
            println("Lait supplémentaire: " + laitsup + "\n")
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")


          } else if (-Quantitecafe > poudrecafe) {

            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre + "\n")
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")

          } else {


            if (boisson == 4) {
              //petit//

              quantitecafe = (quantitecafe - 6)

              if (ajoutsucre == 1) {
                quantitesucre = (quantitesucre - 0)
              } else if (ajoutsucre == 2) {
                quantitesucre = (quantitesucre - 5)
              } else if (ajoutsucre == 3) {
                quantitesucre = (quantitesucre - 10)
              } else {
                quantitesucre = (quantitesucre - 15)
              }

              if (ajoutlait == 1) {
                quantitelait = (quantitelait - 0.170)
              } else if (ajoutlait == 2) {
                quantitelait = (quantitelait - 0.220)
              } else if (ajoutlait == 3) {
                quantitelait = (quantitelait - 0.270)
              } else {
                quantitelait = (quantitelait - 0.120)
              }

            } else if (boisson == 5) {
              //moyen//

              quantitecafe = (quantitecafe - 8)

              if (ajoutsucre == 1) {
                quantitesucre = (quantitesucre - 0)
              } else if (ajoutsucre == 2) {
                quantitesucre = (quantitesucre - 5)
              } else if (ajoutsucre == 3) {
                quantitesucre = (quantitesucre - 10)
              } else {
                quantitesucre = (quantitesucre - 15)
              }

              if (ajoutlait == 1) {
                quantitelait = (quantitelait - 0.200)
              } else if (ajoutlait == 2) {
                quantitelait = (quantitelait - 0.250)
              } else if (ajoutlait == 3) {
                quantitelait = (quantitelait - 0.300)
              } else {
                quantitelait = (quantitelait - 0.150)
              }

            } else {
              //grand//

              quantitecafe = (quantitecafe - 12)

              if (ajoutsucre == 1 ) {  //sans sucre//
                quantitesucre = (quantitesucre - 0)
              } else if (ajoutsucre == 2) {
                quantitesucre = (quantitesucre - 5)
              } else if (ajoutsucre == 3) {
                quantitesucre = (quantitesucre - 10)
              } else {
                quantitesucre = (quantitesucre - 15)
              }

              if (ajoutlait == 1) {
                quantitelait = (quantitelait - 0.250)
              } else if (ajoutlait == 2) {
                quantitelait = (quantitelait - 0.300)
              } else if (ajoutlait == 3) {
                quantitelait = (quantitelait - 0.350)
              } else {
                quantitelait = (quantitelait - 0.200)
              }

            }


            if (boisson == 4) {
              prix = 2.70
            } else if (boisson == 5) {
              prix = 3.20
            } else {
              prix = 3.70
            }

            if (ajoutsucre == 1) {
              prixsucre = 0.00
            } else if (ajoutsucre == 2) {
              prixsucre = 0.10
            } else if (ajoutsucre == 3) {
              prixsucre = 0.20
            } else {
              prixsucre = 0.30
            }

            if (ajoutlait == 1) {
              prixlait = 0.05
            } else if (ajoutlait == 2) {
              prixlait = 0.10
            } else if (ajoutlait == 3) {
              prixlait = 0.15
            } else {
              prixlait = 0.0
            }

            prixtotal = (prix + prixlait + prixsucre)


            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre)
            println("Lait supplémentaire: " + laitsup)
            printf("Prix total : CHF %.2f +  CHF %.2f + CHF %.2f = CHF %.2f \n", prix,prixsucre,prixlait,prixtotal)
            println()
            println("Veuillez payer en utilisant Twint")
            code = Random.alphanumeric.take(5).mkString("")
            println("Votre code de paiement est : " + code)
            println("(En attente de validation du paiement...)\n")
            Thread.sleep(3000)

            println("Merci ! Votre paiement a été accepté.")
            println("Préparation de votre boisson...")
            println("[...]")
            println("Votre " + choixboisson + " est prêt ! Bonne dégustation ! \n")

          }

        } else { //boisson 1 et 2 //

          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          println("> ")

          ajoutsucre = readLine().toInt

          if (boisson==2){

          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          println("> ")
          ajoutlait = readLine().toInt

          if (ajoutlait == 1) {

            println("Combien de dose ? 1, 2 ou 3")
            println(">")

            ajoutlait = readLine().toInt

          } else {
            ajoutlait = 0
          }
          }

          if (boisson == 1) {
            choixboisson = "Expresso"
          } else if (boisson == 2) {
            choixboisson = "Cappuccino"
          } else {
            choixboisson = "Latte"
          }

          if (ajoutsucre == 1) {
            niveausucre = "Sans sucre"
          } else if (ajoutsucre == 2) {
            niveausucre = "Peu (5g)"
          } else if (ajoutsucre == 3) {
            niveausucre = "Moyen (10g)"
          } else {
            niveausucre = "Beaucoup (15g)"
          }

          if (ajoutlait == 1) {
            laitsup = "Une dose"
          } else if (ajoutlait == 2) {
            laitsup = "Deux doses"
          } else if (ajoutlait == 3) {
            laitsup = "Trois doses"
          } else {
            laitsup = "Non"
          }


          if (boisson == 1) {

            Quantitecafe = (Quantitecafe - 8)

            if (ajoutsucre == 1) {
              Quantitesucre = (Quantitesucre - 0)
            } else if (ajoutsucre == 2) {
              Quantitesucre = (Quantitesucre - 5)
            } else if (ajoutsucre == 3) {
              Quantitesucre = (Quantitesucre - 10)
            } else {
              Quantitesucre = (Quantitesucre - 15)
            }

            if (ajoutlait == 1) {
              Quantitelait = (Quantitelait - 0.050)
            } else if (ajoutlait == 2) {
              Quantitelait = (Quantitelait - 0.100)
            } else if (ajoutlait == 3) {
              Quantitelait = (Quantitelait - 0.150)
            } else {
              Quantitelait = (Quantitelait - 0)
            }


          } else {

            Quantitecafe = (Quantitecafe - 6)

            if (ajoutsucre == 1) {
              Quantitesucre = (Quantitesucre - 0)
            } else if (ajoutsucre == 2) {
              Quantitesucre = (Quantitesucre - 5)
            } else if (ajoutsucre == 3) {
              Quantitesucre = (Quantitesucre - 10)
            } else {
              Quantitesucre = (Quantitesucre - 15)
            }

            if (ajoutlait == 1) {
              Quantitelait = (Quantitelait - 0.150)
            } else if (ajoutlait == 2) {
              Quantitelait = (Quantitelait - 0.200)
            } else if (ajoutlait == 3) {
              Quantitelait = (Quantitelait - 0.250)
            } else {
              Quantitelait = (Quantitelait - 0.100)
            }

          }


          if (-quantitesucre > sucre) {
            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre + "\n")
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")



          } else if (-quantitelait > lait) {
            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre)
            println("Lait supplémentaire: " + laitsup + "\n")
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")


          } else if (-quantitecafe > poudrecafe) {

            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre + "\n")
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")


          } else {



            if (boisson == 1) {

              quantitecafe = (quantitecafe - 8)

              if (ajoutsucre == 1) {
                quantitesucre = (quantitesucre - 0)
              } else if (ajoutsucre == 2) {
                quantitesucre = (quantitesucre - 5)
              } else if (ajoutsucre == 3) {
                quantitesucre = (quantitesucre - 10)
              } else {
                quantitesucre = (quantitesucre - 15)
              }

              if (ajoutlait == 1) {
                quantitelait = (quantitelait - 0.050)
              } else if (ajoutlait == 2) {
                quantitelait = (quantitelait - 0.100)
              } else if (ajoutlait == 3) {
                quantitelait = (quantitelait - 0.150)
              } else {
                quantitelait = (quantitelait - 0)
              }


            } else {

              quantitecafe = (quantitecafe - 6)

              if (ajoutsucre == 1) {
                quantitesucre = (quantitesucre - 0)
              } else if (ajoutsucre == 2) {
                quantitesucre = (quantitesucre - 5)
              } else if (ajoutsucre == 3) {
                quantitesucre = (quantitesucre - 10)
              } else {
                quantitesucre = (quantitesucre - 15)
              }

              if (ajoutlait == 1) {
                quantitelait = (quantitelait - 0.150)
              } else if (ajoutlait == 2) {
                quantitelait = (quantitelait - 0.200)
              } else if (ajoutlait == 3) {
                quantitelait = (quantitelait - 0.250)
              } else {
                quantitelait = (quantitelait - 0.100)
              }

            }


            if (boisson == 1) {
              prix = 2.00
              if (ajoutsucre == 1) {
                prixsucre = 0.00
              } else if (ajoutsucre == 2) {
                prixsucre = 0.10
              } else if (ajoutsucre == 3) {
                prixsucre = 0.20
              } else {
                prixsucre = 0.30
              }

            } else {
              prix = 2.50
              if (ajoutsucre == 1) {
                prixsucre = 0.00
              } else if (ajoutsucre == 2) {
                prixsucre = 0.10
              } else if (ajoutsucre == 3) {
                prixsucre = 0.20
              } else {
                prixsucre = 0.30
              }

            }

            if (ajoutlait == 1) {
              prixlait = 0.05
            } else if (ajoutlait == 2) {
              prixlait = 0.10
            } else if (ajoutlait == 3) {
              prixlait = 0.15
            } else {
              prixlait = 0.00
            }

            prixtotal = (prix + prixlait + prixsucre)

            println("Boisson s'électionnée : " + choixboisson)
            println("Niveau de sucre : " + niveausucre)
            println("Lait supplémentaire: " + laitsup)
            printf("Prix total : CHF %.2f +  CHF %.2f + CHF %.2f = CHF %.2f \n", prix,prixsucre,prixlait,prixtotal)
            println()
            println("Veuillez payer en utilisant Twint")
            code = Random.alphanumeric.take(5).mkString("")
            println("Votre code de paiement est : " + code)
            println("(En attente de validation du paiement...)\n")
            Thread.sleep(3000)

            println("Merci ! Votre paiement a été accepté.")
            println("Préparation de votre boisson...")
            println("[...]")
            println("Votre " + choixboisson + " est prêt ! Bonne dégustation ! \n")

          }

        }

      } else if (choix == 2) {
        println("Mode Admin")
        println("Entrez le code PIN : ****** ")
        pin = readLine().toInt
        while (pin != 434343) {
          println("Code erroné.")
          println("Entrez le code PIN : ****** ")
          pin = readLine().toInt
        }
        println("Accès autorisé.\n")

        poudrecafe = ( poudrecafe + quantitecafe )
        lait = (lait + quantitelait)
        sucre = (sucre+quantitesucre)


        println("Stocks :")
        println("Poudre de café : " + poudrecafe + "g")
        printf("Lait           : %.3f L \n",lait)
        println("Sucre          : " + sucre + "g \n")

        println("Réapprovisionnement des stocks...")
        println("Ajout :")
        println("Poudre de café : ")
        poudrecafe = readLine().toInt
        println("Lait           : ")
        lait = readLine().toDouble
        println("Sucre          : ")
        sucre = readLine().toInt
        println()
        println("Poudre de café : " + poudrecafe + "g")
        printf("Lait           : %.3f L \n",lait)
        println("Sucre          : " + sucre + "g \n")

        println("Niveaux de stock mis à jour.")
        println("Retour au menu principal... \n")

      } else {
        //option 3 (quitter)//
        boucle = false

      }



    }



  }
}



