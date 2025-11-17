import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var mode = 0
    var coffeepowder = 50
    var milk = 0.500
    var sugar = 30
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


    do {

      mode = readLine("    Nespresso Café \nVeuillez selectionner votre mode: \n1)Client \n2)Admin \n3)Quitter \n>").toInt
      if (mode == 1) {

        var ordervalid = false
        while (!ordervalid) {

          var boisson = 0.0
          var taillelatte = 0
          var prixboisson = 0.0
          var coffeepowdermass = 0
          var addsugar = 0
          var prixsugar = 0.0
          var sugarmass = 0
          var addmilkyn = 0
          var addmilk = 0
          var milkmass = 0.0
          var prixmilk = 0.0
          var prixtotal = 0.0


          do {
            boisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Capuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>").toInt
            if (boisson == 1) {
              prixboisson = 2.00
              coffeepowdermass = 8

              do {
                addsugar = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n>").toInt
                if (addsugar == 1) {
                  prixsugar = 0.0
                  sugarmass = 0
                }
                else if (addsugar == 2) {
                  prixsugar = 0.10
                  sugarmass = 5
                }
                else if (addsugar == 3) {
                  prixsugar = 0.20
                  sugarmass = 10
                }
                else if (addsugar == 4) {
                  prixsugar = 0.30
                  sugarmass = 15
                }
              } while ((addsugar != 1) && (addsugar != 2) && (addsugar != 3) && (addsugar != 4))


            } else if (boisson == 2) {
              prixboisson = 2.50
              coffeepowdermass = 6
              milkmass = 0.100

              do {
                addsugar = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n>").toInt
                if (addsugar == 1) {
                  prixsugar = 0.0
                  sugarmass = 0
                }
                else if (addsugar == 2) {
                  prixsugar = 0.10
                  sugarmass = 5
                }
                else if (addsugar == 3) {
                  prixsugar = 0.20
                  sugarmass = 10
                }
                else if (addsugar == 4) {
                  prixsugar = 0.30
                  sugarmass = 15
                }
              } while ((addsugar != 1) && (addsugar != 2) && (addsugar != 3) && (addsugar != 4))

              do {
                addmilkyn = readLine("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappucino et Latte) \n1) Oui \n2) Non \n>").toInt
              } while ((addmilkyn != 1) && (addmilkyn != 2))
              if (addmilkyn == 1) {

                do {
                  addmilk = readLine("Combien de dose ? \n>").toInt
                  if (addmilk == 1) {
                    milkmass = milkmass + 0.050
                    prixmilk = 0.05
                  }
                  else if (addmilk == 2) {
                    milkmass = milkmass + 0.10
                    prixmilk = 0.10
                  }
                  else if (addmilk == 3) {
                    milkmass = milkmass + 0.150
                    prixmilk = 0.15
                  }
                } while ((addmilk != 1) && (addmilk != 2) && (addmilk != 3))
              }


            } else if (boisson == 3) {

              do {
                taillelatte = readLine("Quelle taille de Latte désirez-vous ? \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n>").toInt
                if (taillelatte == 1) {
                  prixboisson = 2.70
                  coffeepowdermass = 6
                  milkmass = 0.120
                }
                else if (taillelatte == 2) {
                  prixboisson = 3.20
                  coffeepowdermass = 8
                  milkmass = 0.150
                }
                else if (taillelatte == 3) {
                  prixboisson = 3.70
                  coffeepowdermass = 12
                  milkmass = 0.200
                }
              } while ((taillelatte != 1) && (taillelatte != 2) && (taillelatte != 3))


              do {
                addsugar = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n>").toInt
                if (addsugar == 1) {
                  prixsugar = 0.0
                  sugarmass = 0
                }
                else if (addsugar == 2) {
                  prixsugar = 0.10
                  sugarmass = 5
                }
                else if (addsugar == 3) {
                  prixsugar = 0.20
                  sugarmass = 10
                }
                else if (addsugar == 4) {
                  prixsugar = 0.30
                  sugarmass = 15
                }
              } while ((addsugar != 1) && (addsugar != 2) && (addsugar != 3) && (addsugar != 4))

              do {
                addmilkyn = readLine("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappucino et Latte) \n1) Oui \n2) Non \n>").toInt
              } while ((addmilkyn != 1) && (addmilkyn != 2))
              if (addmilkyn == 1) {

                do {
                  addmilk = readLine("Combien de dose ? \n>").toInt
                  if (addmilk == 1) {
                    milkmass = milkmass + 0.050
                    prixmilk = 0.05
                  }
                  else if (addmilk == 2) {
                    milkmass = milkmass + 0.10
                    prixmilk = 0.10
                  }
                  else if (addmilk == 3) {
                    milkmass = milkmass + 0.150
                    prixmilk = 0.15
                  }
                } while ((addmilk != 1) && (addmilk != 2) && (addmilk != 3))
              }
            }
          } while ((boisson != 1) && (boisson != 2) && (boisson != 3))


          print("Boisson sélectionnée : ")
          if (boisson == 1) println("Expresso")
          else if (boisson == 2) println("Cappucino")
          else if (boisson == 3) println("Latte")

          print("Niveau de sucre : ")
          if (addsugar == 1) println("Sans sucre")
          else if (addsugar == 2) println("Peu (5g)")
          else if (addsugar == 3) println("Moyen (10g)")
          else if (addsugar == 4) println("Beaucoup (15g)")

          if (addmilkyn == 1) println("Lait supplémentaire : Oui")
          else if (addmilkyn == 2) println("Lait supplémentaire : Non")


          if (coffeepowder < coffeepowdermass) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
            coffeepowder = coffeepowder + coffeepowdermass
          }
          else if (milk < milkmass) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
            milk = milk + milkmass
          }
          else if (sugar < sugarmass) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
            sugar = sugar + sugarmass
          }
          else {
            ordervalid = true
            print(f"Prix total : CHF $prixboisson%.2f")
            if (addsugar != 1) {
              print(f" + CHF $prixsugar%.2f")
            }
            if (addmilkyn == 1) {
              print(f" + CHF $prixmilk%.2f")
            }
            prixtotal = prixboisson + prixsugar + prixmilk
            println(f" = CHF $prixtotal%.2f")

            coffeepowder = coffeepowder - coffeepowdermass
            milk = milk - milkmass
            sugar = sugar - sugarmass

            println()

            val codetwint = (1 to 5).map(_ => chars(Random.nextInt(chars.length))).mkString
            println("Veuillez payer en utlisant Twint. \nVotre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")

            println()

            print("Préparation de votre boisson... \n[...] \nVotre ")
            if (boisson == 1) print("Expresso")
            else if (boisson == 2) print("Cappucino")
            else if (boisson == 3) print("Latte")
            println(" est prêt ! Bonne dégustation !\n \n")
          }
        }



      } else if (mode == 2) {

        val pinv = 434343
        var pinadmin = 0.0
        var restockcoffepowder = 0
        var restockmilk = 0.0
        var restocksugar = 0

        do {

          pinadmin = readLine("Mode Admin \nEntrez le code Pin : \n>").toDouble
          if (pinadmin != pinv) println("Accès refusé. \nVeuillez fournir une entrée valide.\n")

        } while (pinadmin != pinv)

        println("Accès autorisé. \nStocks : \n   Poudre de café : " + coffeepowder + "g\n   Lait : " + milk + "L\n   Sucre : " + sugar + "g \n")

        restockcoffepowder = readLine("Quelle masse de poudre de café voulez-vous ajouter au stock ? \n>").toInt
        coffeepowder = coffeepowder + restockcoffepowder
        restocksugar = readLine("Quelle masse de sucre voulez-vous ajouter au stock ? \n>").toInt
        sugar = sugar + restocksugar
        restockmilk = readLine("Quel volume de lait voulez-vous ajouter au stock ? \n>").toInt
        milk = milk + restockmilk

        println("Réapprovissionnement des stocks...\nAjout : \n   Poudre de café : " + restockcoffepowder + "g\n   Lait : " + restockmilk + "L\n   Sucre : " + restocksugar + "g \nNiveaux de stocks mis à jour. \nRetour au menu principal...\n")



      } else if (mode == 3) {


      }
    } while ((mode != 1) || (mode != 2) || (mode != 3))
  }
}