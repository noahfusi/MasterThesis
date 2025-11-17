import io.StdIn.{readLine,readInt}
import util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var ProgrammeActif: Boolean = true

    //variables pour les stocks en grammes et litres pour le lait
    var stockpoudredecafe = 50.0
    var stocksucre = 30.0
    var stocklait = 0.500

    //PIN code pour le mode ADMIN
    val PINadmin = "434343"

    //Variables pour les prix et stocks (réaprovisionnement et vérification après choix de boisson)
    var prixBoisson = 0.0
    var cafeUtilise = 0.0
    var sucreUtilise = 0.0
    var laitUtilise = 0.0
    var prixSucre = 0.0
    var prixLait = 0.0

    var supplementLait = "Non"

    while (ProgrammeActif == true) {
      //Menu principal, choix du mode
      println("\nNospresso Café ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client\n2) Admin\n3) Quitter")
      //On indique > comme dans l'énoncé pour montrer que le programme attend une réponse

      val choixMode = readLine("> ").toInt

      if (choixMode == 1) {
        //Mode Client: choix de boisson (+ toute information concernant prix et stock)
        println("Veuillez sélectionner votre boisson : ")
        println("1) Expresso - CHF 2.00")
        println("2) Cappucino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

        val choixBoisson = readLine("> ").toInt

        if (choixBoisson == 1) {
          //Informations Expresso: 1 seule taille disponible
          cafeUtilise = 8.0
          prixBoisson = 2.0
          stockpoudredecafe -= cafeUtilise
          stocklait -= laitUtilise
          println("Expresso - CHF 2.00")

        } else if (choixBoisson == 2) {
          //Informations Cappucino: 1 seule taille disponible + lait en supplément
          cafeUtilise = 6.0
          laitUtilise = 0.100
          prixBoisson = 2.5
          stockpoudredecafe -= cafeUtilise
          stocklait -= laitUtilise
          println("Cappucino - CHF 2.50")

        } else if (choixBoisson == 3) {
          //Informations Latte: 3 tailles + lait en supplément
          println("Choisissez la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen -3.20\n3) Grand - 3.70")

          var tailleLatte = readLine("> ").toInt

          if (tailleLatte == 1) {
            //Petit latte
            cafeUtilise = 6.0
            laitUtilise = 0.120
            prixBoisson = 2.7
            stockpoudredecafe -= cafeUtilise
            stocklait -= laitUtilise
            println("Petit Latte - CHF 2.7")

          } else if (tailleLatte == 2) {
            //Latte moyen
            cafeUtilise = 8.0
            laitUtilise = 0.150
            prixBoisson = 3.2
            stockpoudredecafe -= cafeUtilise
            stocklait -= laitUtilise
            stocksucre -= sucreUtilise
            println("Latte moyen - CHF 3.2")

          } else if (tailleLatte == 3) {
            //Grand Latte
            cafeUtilise = 12.0
            laitUtilise = 0.200
            prixBoisson = 3.7
            stockpoudredecafe -= cafeUtilise
            stocklait -= laitUtilise
            println("Grand Latte - CHF 3.7")

          } else {
            println("Erreur de saisie.")
          }
        } else println("Erreur de saisie.")


        //Supplément sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) 5g (peu)- CHF 0.10")
        println("3) 10g (moyen) - CHF 0.20")
        println("4) 15g (beaucoup) - CHF 0.30")

        val quantiteSucre = readLine("> ").toInt

        if (quantiteSucre == 1) {
          //Sans sucre

        } else if (quantiteSucre == 2) {
          //Ajout de 5g de supplément de sucre
          sucreUtilise = 5.0
          stocksucre -= sucreUtilise
          prixSucre = 0.10


        } else if (quantiteSucre == 3) {
          //Ajout de 10g de supplément de sucre
          sucreUtilise = 10.0
          stocksucre -= sucreUtilise
          prixSucre = 0.2


        } else if (quantiteSucre == 4) {
          //Ajout de 15g de supplément de sucre
          sucreUtilise = 15.0
          stocksucre -= sucreUtilise
          prixSucre = 0.3

        }

        //Supplément lait, slm pour Cappucino et Latte
        var supplementLait = "Non"
        var dosesLait:Int = 0
        var prixLait = 0.0

        if (choixBoisson == 2 || choixBoisson == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ? \n (Disponible seulement pour Cappuccino et Latte)")
          println("1) Oui\n2) Non")

          var supplementLait = readLine("> ")

          if (supplementLait == "1") {
            //Si il veut du lait supplémentaire, combien de doses ?
            println("Combien de dose (1 dose = 50 mL) ?\n1) 1\n2) 2 \n3) 3 ")

            dosesLait = readLine("> ").toInt


          if ((dosesLait == 1) || (dosesLait == 2) || (dosesLait == 3)) {
              //calcul général pour calcul de la quantité de lait nécessaire, pour dédudction des stocks

              if (dosesLait ==1) {
                stocklait -= 0.05
                prixLait += 0.05

              } else if (dosesLait ==2) {
                stocklait -= 0.100
                prixLait += 0.10

              } else if (dosesLait ==3) {
                stocklait -= 0.150
                prixLait += 0.15
              }
            } else {
              println("Erreur de saisie. Aucun supplément de lait ajouté.")
            }
          }
        }

        // Vérifier les stock avant de passer à la préparation de la boisson.
        if (cafeUtilise > stockpoudredecafe || sucreUtilise > stocksucre || laitUtilise > stocklait) {
          if (cafeUtilise > stockpoudredecafe) {
            println("Stock insuffisant de café.")
          } else if (sucreUtilise > stocksucre) {
            println("Stock insuffisant de sucre.")
          } else if (laitUtilise > stocklait) {
            println("Stock insuffisant de lait.")
          }
          println("Veuillez choisir une autre boisson ou une autre quantitée!")

        } else {
          //Le stock est suffisant, on prépare la boisson

          // Affichage du résumé de la commande
          println("\nRésumé de votre commande :")
          println("Boisson sélectionnée : " + (if (choixBoisson == 1) "Expresso" else if (choixBoisson == 2) "Cappuccino" else "Latte"))
          println("Niveau de sucre : " + {
            if (quantiteSucre == 1) "Sans sucre"
            else if (quantiteSucre == 2) "Peu (5g)"
            else if (quantiteSucre == 3) "Moyen (10g)"
            else if (quantiteSucre == 4) "Beaucoup (15g)"
            else "Erreur"
          })

          println("Doses de lait : " + {
            if (dosesLait == 1) "Une dose de lait"
            else if (dosesLait == 2) "Deux doses de lait"
            else if (dosesLait == 3) "Moyen (10g)"
            else "Erreur"
          })

          if (dosesLait == 1) dosesLait * 5.0/100.0
          else if (dosesLait == 2) dosesLait * 5.0/100.0
          else if (dosesLait == 3) dosesLait * 5.0/100.0


          // Affichage du prix total
          val prixtotal = prixBoisson + prixSucre + prixLait
          printf("%.2f + %.2f + %.2f = %.2f \n", prixBoisson, prixSucre, prixLait, prixtotal)

          //Paiement par TWINT
          println("Vous allez être redirigé pour le paiement.")
          println("Veuillez payer en utilisant TWINT.")
          val codeTWINT = Random.alphanumeric.take(5).mkString
          println("Votre code de paiement est: " + codeTWINT)

          println("Validation du paiement ... ")
          Thread.sleep(5000) // Attend pendant 5000 millisecondes

          println("Paiement confirmé.")
          println("Préparation de la boisson ... ")
          println("MERCI!")

        }


      } else if (choixMode == 2 ) {
        //Deuxième mode :ADMIN avec le pin code
        println("Code PIN : ")

        val codePIN = readLine("> ")

        if (codePIN == PINadmin) {
          println("Code validé. Accès autorisé")
          println("Stock: Café = " + stockpoudredecafe)
          println("Stock: Sucre = " + stocksucre)
          println("Stock: Lait = " + stocklait)

          println("Vous-voulez ajouter des stocks ?\n1) Oui \n2) Non")

          val reponserestock = readLine ("> ").toInt

          if (reponserestock == 1) {
            println("Quantitée de poudre de café que vous ajouter?")
            stockpoudredecafe += readLine ("> ").toInt
            println("Quantitée de sucre que vous ajouter?")
            stocklait += readLine("> ").toInt
            println("Quantitée de lait que vous ajouter?")
            stocksucre += readLine(">").toInt
            println("Réapprovisionnement des stocks ... ")

              println("Stock mis à jour")
              println("Stock: Café = " + stockpoudredecafe)
              println("Stock: Sucre = " + stocksucre)
              println("Stock: Lait = " + stocklait)


          } else {
            println("Annulation de restockage.")
          }
        } else {
          println("Code PIN invalide")
        }
        println("Retour au menu principal")

      } else if (choixMode == 3) {
        ProgrammeActif = false
        println("Merci d'avoir utilisé Nospresso.")
      } else {
        println("Erreur de saisie.")
      }
    }
  }
}