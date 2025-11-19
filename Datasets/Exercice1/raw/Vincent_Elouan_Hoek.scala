import io.StdIn._
import math._
import scala.util.Random

object Machine_a_cafe_cobaye {
  def main(args: Array[String]): Unit = {

    var stock_poudre = 50
    var stock_sucre = 30
    var stock_lait = 500 // Unités en mL

    var ajout_cafe = 0
    var ajout_sucre = 0
    var ajout_lait = 0.0

    println("        Nospresso Café ")
    println("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter")           // Modes

    var mode = readLine("> ").toInt
    do {
      while (mode < 1 || mode > 3) {
        println("Sélectionner un mode valide : ")
        mode = readLine("> ").toInt
      }
                                                                                              // Mode Client
      if (mode == 1) {

        var choix_sucre = 0
        var nbr_doses = 0
        var prix_suppsucre = 0.10 * (choix_sucre - 1)
        var prix_supplait = 0.05 * nbr_doses
        var prix_cafe = 0.00
        var prix_final = 0.00
        var nom_cafe = ""
        var nom_sucre = ""
        var nom_lait = ""

        println("Veuillez sélectionner votre boisson : ")                       // Choix cafés
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

        var cafe = readLine("> ").toInt
        while (cafe < 1 || cafe > 3) {
          println("Sélectionner votre boisson : ")
          cafe = readLine("> ").toInt
        }
          if (cafe == 1) {                                            // Expresso
            prix_cafe = 2.00
            stock_poudre = stock_poudre - 8
            nom_cafe = "Expresso"
            if (stock_poudre < 0) {
              println("Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              while(cafe < 2 || cafe > 3){
                stock_poudre = stock_poudre + 8
                cafe = readLine("Veuillez choisir une autre boisson ou vérifiez les stocks en mode Admin.\nCafé > ").toInt}
              if(cafe == 2 || cafe == 3){
                println("Sélection...")
                Thread.sleep(1000)
              }
            }
          }

          if (cafe == 2) {                                            // Cappuccino
            prix_cafe = 2.50
            stock_poudre = stock_poudre - 6
            stock_lait = stock_lait - 100
            nom_cafe = "Cappuccino"
            if (stock_poudre < 0 || stock_lait < 0) {
              println("Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.")
              do{
                stock_poudre = stock_poudre + 6
                stock_lait = stock_lait + 100
                cafe = readLine("Veuillez choisir une autre boisson ou vérifiez les stocks en mode Admin.\nCafé > ").toInt}
              while(stock_poudre < 0 || stock_lait < 0)
            }
          }

          if (cafe == 3) {                                              //  Latte / Taille

            println("1) Petit \n2) Moyen \n3) Grand")

            var taille = readLine("Sélectionnez la taille de votre Latte : ").toInt
            while (taille < 1 || taille > 3) {
              println("Erreur. Sélectionnez à nouveau la taille de votre Latte : ")
              taille = readLine("> ").toInt
            }
            if(stock_poudre < 6 || stock_lait < 100){
              println("Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.")
              do{cafe = readLine("Choisissez une autre boisson ou vérifiez les stocks en mode Admin: \nCafé > ").toInt}
              while(cafe < 1 || cafe > 2)
            }
            if (taille == 1 && stock_poudre >= 6 && stock_lait >= 120) {
              prix_cafe = 2.70
              stock_poudre = stock_poudre - 6
              stock_lait = stock_lait - 120
              nom_cafe = "Latte (Petit)"
            }
            if (taille == 2 && stock_poudre >= 8 && stock_lait >= 150) {
              prix_cafe = 3.20
              stock_poudre = stock_poudre - 8
              stock_lait = stock_lait - 150
              nom_cafe = "Latte (Moyen)"
            }
            if (taille == 3 && stock_poudre >= 12 && stock_lait >= 200) {
              prix_cafe = 3.70
              stock_poudre = stock_poudre - 12
              stock_lait = stock_lait - 200
              nom_cafe = "Latte (Grand)"
            }
          }

            println("Souhaitez-vous ajouter du sucre ?")            // Option sucre
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            choix_sucre = readLine("> ").toInt

            while (choix_sucre < 1 || choix_sucre > 4) {
              println("Souhaitez-vous ajouter du sucre ?")
              choix_sucre = readLine("> ").toInt
            }
            if (choix_sucre == 2 || choix_sucre == 3 || choix_sucre == 4) {
              if (choix_sucre == 2) {
                stock_sucre = stock_sucre - 5
                nom_sucre = "Peu (5g)"
              }
              if (choix_sucre == 3) {
                stock_sucre = stock_sucre - 10
                nom_sucre = "Moyen (10g)"
              }
              if (choix_sucre == 4) {
                stock_sucre = stock_sucre - 15
                nom_sucre = "Beaucoup (15g)"
              }

              prix_final = prix_cafe + ((choix_sucre - 1) * 0.1)
              prix_suppsucre = 0.10 * (choix_sucre - 1)
            }
            if(choix_sucre == 1 || stock_sucre < 0){
              prix_final = prix_cafe
              prix_suppsucre = 0
              nom_sucre = "Sans sucre"
              if(stock_sucre < 0){println("Stock de sucre insuffisant.")}
            }


            if (cafe == 2 || cafe == 3) {

              var choix_lait = 0                    // Option lait pour Cappuccino et Latte

              println("Souhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non")
              choix_lait = readLine("> ").toInt

              while (choix_lait < 1 || choix_lait > 2) {
                println("Souhaitez-vous du lait ? \nSélectionnez \n1) Oui \n2) Non")
                choix_lait = readLine("> ").toInt
              }
              if (choix_lait == 1) {

                nom_lait = "Lait en supplément : Oui"
                var nbr_doses = 0
                println("Combien de doses souhaitez-vous (1 dose : 50 mL) ?")
                nbr_doses = readLine("> ").toInt

                while (nbr_doses < 0 || nbr_doses > 3) {
                  println("Combien de doses souhaitez-vous ?")
                  if (nbr_doses > 3) {
                    println("Le nombre de doses maximal est de 3.")
                    nbr_doses = readLine("> ").toInt
                  }
                  if (nbr_doses < 0) {
                    println("La valeur que vous venez d'entrer n'est pas valable. Entrez une autre valeur : ")
                    nbr_doses = readLine("> ").toInt
                  }
                }
                stock_lait = stock_lait - nbr_doses * 50
                if (stock_lait < 50) {
                  println("Il n'y a pas assez de lait.")
                  nbr_doses = 0
                  nom_lait = "Lait en supplément : Non"
                }
                prix_final += nbr_doses * 0.05
                prix_supplait = nbr_doses * 0.05
              }
              else {
                prix_final += 0
                prix_supplait = 0
                nom_lait = "Lait en supplément : Non"
              }
            }                                                        // Paiement

            println("Boisson sélectionnée : " + nom_cafe + "\nNiveau de sucre : " + nom_sucre + "\n" + nom_lait)

            printf("Prix total : CHF %.2f " + " + CHF %.2f (Sucre)" + " + CHF %.2f (Lait)" + " = CHF %.2f \n", prix_cafe, prix_suppsucre, prix_supplait, prix_final)
            println("\nVeuillez payer en utilisant Twint.")

            val caracteres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            var code = ""
            var test_code = ""

            for (i <- 1 to 5) {
              val i = Random.nextInt(caracteres.length)         // Méthode/package alphanum
              val chaine = caracteres(i)
              code = code + chaine
            }
            print("Le code est : " + code)
            test_code = readLine("\nEntrez le code : ")
            while (test_code != code) {
              println("Entrez à nouveau le code : ")
              test_code = readLine("> ")
            }
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
            Thread.sleep(1000)
            println("[...]")
            Thread.sleep(3000)
            println("Votre " + nom_cafe + " est prêt ! Bonne dégustation !")
            Thread.sleep(1000)

            println("\nRetour au menu principal...\n")
            Thread.sleep(2000)
            println("Veuillez sélectionner votre mode : \n 1) Client \n 2) Admin \n 3) Quitter")

            mode = readLine("> ").toInt
            while (mode < 1 || mode > 3) {
              println("Sélectionner un mode valide : ")
              mode = readLine("> ").toInt
            }
      }

                                                                        // Mode Admin
      if (mode == 2) {
        println("Mode Admin.")
        var PIN = readLine("Entrez le code PIN : ").toInt               // PIN
        val test_PIN = 434343
        while (PIN != test_PIN) {
          println("Entrez à nouveau le PIN : ")
          PIN = readLine("> ").toInt
        }
        if (PIN == test_PIN) {
          println("Accès autorisé.\n")
        }
        println("Stocks : \n")                                          // Stocks
        println("Poudre de café : " + stock_poudre + "g")
        println("Lait : " + (stock_lait / 1000.0) + "L")
        println("Sucre : " + stock_sucre + "g")

        var reap = 0                                                    // Réapprovisionnement
        println("Souhaitez-vous réapprovisionner les stocks ?")
        reap = readLine("1) Oui \n 2) Non \n > ").toInt
        while (reap < 1 || reap > 2) {
          println("Souhaitez-vous réaprovisionner les stocks ?")
          reap = readLine("1) Oui \n 2) Non \n > ").toInt
        }

        if (reap == 1) {
          println("Ajout : ")
          ajout_cafe = readLine("Poudre de café (g) : ").toInt
          ajout_sucre = readLine("Sucre (g) : ").toInt
          ajout_lait = readLine("Lait (en L) : ").toDouble

          stock_poudre = stock_poudre + ajout_cafe
          stock_sucre = stock_sucre + ajout_sucre
          stock_lait = (stock_lait + ajout_lait).toInt

          println("Niveau des stocks mis à jour.")
        }
        println("\nRetour au menu principal... \n")
        Thread.sleep(2000)
        println("Veuillez sélectionner votre mode : \n 1) Client \n 2) Admin \n 3) Quitter")

        mode = readLine("> ").toInt
        while (mode < 1 || mode > 3) {
          println("Sélectionner un mode valide : ")
          mode = readLine("> ").toInt
        }
      }
      else {}
    } while (mode != 3)
  }
}