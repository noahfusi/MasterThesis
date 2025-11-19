object Main {
  def main(args: Array[String]): Unit = {

    import scala.io.StdIn._
    import scala.util.Random

    var code = 0
    var boisson = 0
    var sucre = 0
    var prix: Double = 0
    var poudre = 0
    var lait: Double = 0.0
    var dose = 0
    var taille = 0

    var modif = 0

    var Spoudre = 50
    var Ssucre = 30
    var Slait: Double = 0.5

    var choix = 0
    var psucre = 0
    var llait = 0

    var exit = false

    while (!exit) {
      val pass = Random.alphanumeric.take(5).mkString
      while (choix != 1 && choix != 2 && choix != 3) {
        choix = readLine("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      }

      if (choix == 1) {
        println("Vous avez choisi le mode Client")
        while (boisson != 1 && boisson != 2 && boisson != 3) {
          boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
        }

        if (boisson == 1) {
          println("Vous avez choisi un expresso")
          while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4) {
            psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
          }
          if (psucre == 1) {
            prix = 2.00
            poudre += 8
          }
          if (psucre == 2) {
            prix = 2.10
            poudre += 8
            sucre += 5
          }
          if (psucre == 3) {
            prix = 2.20
            poudre += 8
            sucre += 10
          }
          if (psucre == 4) {
            prix = 2.30
            poudre += 8
            sucre += 15
          }
        }


        if (boisson == 2) {
          println("Vous avez choisi un capuccino")
          while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4 && llait != 1 && llait != 2) {
            psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
            llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
          }
          if (llait == 2 && psucre == 1) {
            prix = 2.50
            poudre += 6
            lait += 0.10
          }
          if (llait == 2 && psucre == 2) {
            prix = 2.60
            poudre += 6
            lait += 0.10
            sucre += 5
          }
          if (llait == 2 && psucre == 3) {
            prix = 2.70
            poudre += 6
            lait += 0.10
            sucre += 10
          }
          if (llait == 2 && psucre == 4) {
            prix = 2.80
            poudre += 6
            lait += 0.10
            sucre += 15
          }
          if (llait == 1) {
            while (dose != 1 && dose != 2 && dose != 3) {
              dose = readLine("Combien de dose ?\n>").toInt
            }
            if (dose == 1 && psucre == 1) {
              prix = 2.55
              poudre += 6
              lait += 0.15
            }
            if (dose == 1 && psucre == 2) {
              prix = 2.65
              poudre += 6
              lait += 0.15
              sucre += 5
            }
            if (dose == 1 && psucre == 3) {
              prix = 2.75
              poudre += 6
              lait += 0.15
              sucre += 10
            }
            if (dose == 1 && psucre == 4) {
              prix = 2.85
              poudre += 6
              lait += 0.15
              sucre += 15
            }
            if (dose == 2 && psucre == 1) {
              prix = 2.60
              poudre += 6
              lait += 0.20
            }
            if (dose == 2 && psucre == 2) {
              prix = 2.70
              poudre += 6
              lait += 0.20
              sucre += 5
            }
            if (dose == 2 && psucre == 3) {
              prix = 2.80
              poudre += 6
              lait += 0.20
              sucre += 10
            }
            if (dose == 2 && psucre == 4) {
              prix = 2.90
              poudre += 6
              lait += 0.20
              sucre += 15
            }
            if (dose == 3 && psucre == 1) {
              prix = 2.65
              poudre += 6
              lait += 0.25
            }
            if (dose == 3 && psucre == 2) {
              prix = 2.75
              poudre += 6
              lait += 0.25
              sucre += 5
            }
            if (dose == 3 && psucre == 3) {
              prix = 2.85
              poudre += 6
              lait += 0.25
              sucre += 10
            }
            if (dose == 3 && psucre == 4) {
              prix = 2.95
              poudre += 6
              lait += 0.25
              sucre += 15
            }
          }
        }

        if (boisson == 3) {
          println("Vous avez choisi un latte")
          while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4 && llait != 1 && llait != 2 && taille != 1 && taille != 2 && taille != 3) {
            psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
            taille = readLine("Quelle taille souaitez-vous ?\n1) Petit \n2) Moyen \n3) Grand \n>").toInt
            llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
          }
          if (llait == 2 && psucre == 1 && taille == 1) {
            prix = 2.70
            poudre += 6
            lait += 0.12
          }
          if (llait == 2 && psucre == 2 && taille == 1) {
            prix = 2.80
            poudre += 6
            lait += 0.12
            sucre += 5
          }
          if (llait == 2 && psucre == 3 && taille == 1) {
            prix = 2.90
            poudre += 6
            lait += 0.12
            sucre += 10
          }
          if (llait == 2 && psucre == 4 && taille == 1) {
            prix = 3.00
            poudre += 6
            lait += 0.12
            sucre += 15
          }
          if (llait == 2 && psucre == 1 && taille == 2) {
            prix = 3.20
            poudre += 8
            lait += 0.15
          }
          if (llait == 2 && psucre == 2 && taille == 2) {
            prix = 3.30
            poudre += 8
            lait += 0.15
            sucre += 5
          }
          if (llait == 2 && psucre == 3 && taille == 2) {
            prix = 3.40
            poudre += 8
            lait += 0.15
            sucre += 10
          }
          if (llait == 2 && psucre == 4 && taille == 2) {
            prix = 3.50
            poudre += 8
            lait += 0.15
            sucre += 15
          }
          if (llait == 2 && psucre == 1 && taille == 3) {
            prix = 3.70
            poudre += 12
            lait += 0.20
          }
          if (llait == 2 && psucre == 2 && taille == 3) {
            prix = 3.80
            poudre += 12
            lait += 0.20
            sucre += 5
          }
          if (llait == 2 && psucre == 3 && taille == 3) {
            prix = 3.90
            poudre += 12
            lait += 0.20
            sucre += 10
          }
          if (llait == 2 && psucre == 4 && taille == 3) {
            prix = 4.00
            poudre += 12
            lait += 0.20
            sucre += 15

          }
          if (llait == 1) {
            while (dose != 1 && dose != 2 && dose != 3) {
              dose = readLine("Combien de dose ?\n>").toInt
            }
            if (dose == 1 && taille == 1 && psucre == 1) {
              prix = 2.75
              Spoudre += 6
              Slait += 0.17
            }
            if (dose == 1 && taille == 1 && psucre == 2) {
              prix = 2.85
              poudre += 6
              lait += 0.17
              sucre += 5
            }
            if (dose == 1 && taille == 1 && psucre == 3) {
              prix = 2.95
              poudre += 6
              lait += 0.17
              sucre += 10
            }
            if (dose == 1 && taille == 1 && psucre == 4) {
              prix = 3.05
              poudre += 6
              lait += 0.17
              sucre += 15
            }
            if (dose == 2 && taille == 1 && psucre == 1) {
              prix = 2.80
              poudre += 6
              lait += 0.22
            }
            if (dose == 2 && taille == 1 && psucre == 2) {
              prix = 2.90
              poudre += 6
              lait += 0.22
              sucre += 5
            }
            if (dose == 2 && taille == 1 && psucre == 3) {
              prix = 3.00
              poudre += 6
              lait += 0.22
              sucre += 10
            }
            if (dose == 2 && taille == 1 && psucre == 4) {
              prix = 3.10
              poudre += 6
              lait += 0.22
              sucre += 15
            }
            if (dose == 3 && taille == 1 && psucre == 1) {
              prix = 2.85
              poudre += 6
              lait += 0.27
            }
            if (dose == 3 && taille == 1 && psucre == 2) {
              prix = 2.95
              poudre += 6
              lait += 0.27
              sucre += 5
            }
            if (dose == 3 && taille == 1 && psucre == 3) {
              prix = 3.05
              poudre += 6
              lait += 0.27
              sucre += 10
            }
            if (dose == 3 && taille == 1 && psucre == 4) {
              prix = 3.15
              poudre += 6
              lait += 0.27
              sucre += 15
            }
            if (dose == 1 && taille == 2 && psucre == 1) {
              prix = 3.25
              poudre += 8
              lait += 0.20
            }
            if (dose == 1 && taille == 2 && psucre == 2) {
              prix = 3.35
              poudre += 8
              lait += 0.20
              sucre += 5
            }
            if (dose == 1 && taille == 2 && psucre == 3) {
              prix = 3.45
              poudre += 8
              lait += 0.20
              sucre += 10
            }
            if (dose == 1 && taille == 2 && psucre == 4) {
              prix = 3.55
              poudre += 8
              lait += 0.20
              sucre += 15
            }
            if (dose == 2 && taille == 2 && psucre == 1) {
              prix = 3.30
              poudre += 8
              lait += 0.25
            }
            if (dose == 2 && taille == 2 && psucre == 2) {
              prix = 3.40
              poudre += 8
              lait += 0.25
              sucre += 5
            }
            if (dose == 2 && taille == 2 && psucre == 3) {
              prix = 3.50
              poudre += 8
              lait += 0.25
              sucre += 10
            }
            if (dose == 2 && taille == 2 && psucre == 4) {
              prix = 3.60
              poudre += 8
              lait += 0.25
              sucre += 15
            }
            if (dose == 3 && taille == 2 && psucre == 1) {
              prix = 3.35
              poudre += 8
              lait += 0.30
            }
            if (dose == 3 && taille == 2 && psucre == 2) {
              prix = 3.45
              poudre += 8
              lait += 0.30
              sucre += 5
            }
            if (dose == 3 && taille == 2 && psucre == 3) {
              prix = 3.55
              poudre += 8
              lait += 0.30
              sucre += 10
            }
            if (dose == 3 && taille == 2 && psucre == 4) {
              prix = 3.65
              poudre += 8
              lait += 0.30
              sucre += 15
            }
            if (dose == 1 && taille == 3 && psucre == 1) {
              prix = 3.75
              poudre += 12
              lait += 0.25
            }
            if (dose == 1 && taille == 3 && psucre == 2) {
              prix = 3.85
              poudre += 12
              lait += 0.25
              sucre += 5
            }
            if (dose == 1 && taille == 3 && psucre == 3) {
              prix = 3.95
              poudre += 12
              lait += 0.25
              sucre += 10
            }
            if (dose == 1 && taille == 3 && psucre == 4) {
              prix = 4.05
              poudre += 12
              lait += 0.25
              sucre += 15
            }
            if (dose == 2 && taille == 3 && psucre == 1) {
              prix = 3.80
              poudre -= 12
              lait -= 0.30
            }
            if (dose == 2 && taille == 3 && psucre == 2) {
              prix = 3.90
              poudre += 12
              lait += 0.30
              sucre += 5
            }
            if (dose == 2 && taille == 3 && psucre == 3) {
              prix = 4.00
              poudre += 12
              lait += 0.30
              sucre += 10
            }
            if (dose == 2 && taille == 3 && psucre == 4) {
              prix = 4.10
              poudre += 12
              lait += 0.30
              sucre += 15
            }
            if (dose == 3 && taille == 3 && psucre == 1) {
              prix = 3.85
              poudre += 12
              lait += 0.35
            }
            if (dose == 3 && taille == 3 && psucre == 2) {
              prix = 3.95
              poudre += 12
              lait += 0.35
              sucre += 5
            }
            if (dose == 3 && taille == 3 && psucre == 3) {
              prix = 4.05
              poudre += 12
              lait += 0.35
              sucre += 10
            }
            if (dose == 3 && taille == 3 && psucre == 4) {
              prix = 4.15
              poudre += 12
              lait += 0.35
              sucre += 15
            }
          }
        }
        if (Spoudre >= poudre && Ssucre >= sucre && Slait >= lait) {
          Spoudre -= poudre
          Slait -= lait
          Ssucre -= sucre
          poudre -= poudre
          lait -= lait
          sucre -= sucre
          println("Le prix est de " + f"$prix%.2f" + " CHF")
          Thread.sleep(1000)
          println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + pass)
          Thread.sleep(1000)
          println("(En attente de validation du paiement...)")
          Thread.sleep(2000)
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(3000)
          println("Votre expresso est prêt! Bonne dégustation !")
        }
        if (poudre>Spoudre || sucre>Ssucre || lait>Slait) {
          if (poudre > Spoudre) {
          println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
        }
          if (sucre>Ssucre){
           println("Erreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          }
          if(lait>Slait){
            println("Erreur : Quantité de lait insuffisante pour \npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          }
        }
      }

      else if (choix == 2) {
        println("Vous avez choisi le mode Admin")
        code = readLine("Veuillez saisir le mot de passe: ").toInt
        if (code != 434343) {
          println("Le code est faux \nRetour au menu principal")
        } else {
          println("Le code est correct")
          Thread.sleep(1000)
          println("Stocks:\nPoudre de café: " + Spoudre + "g" + "\nLait : " + Slait + "l" + "\nSucre : " + Ssucre + "g")
          Thread.sleep(1000)
          while (modif != 1 && modif != 2) {
            modif = readLine("Souhaitez-vous modifiez les stocks ? \n1) Oui \n2) Non \n>").toInt
          }
          if (modif == 1) {
            Spoudre += readLine("Combien de poudre de café souhaitez vous ajoutez ? ").toInt
            Slait += readLine("Combien de lait souhaitez vous ajoutez ? ").toInt
            Ssucre += readLine("Combien de sucre souhaitez vous ajoutez ? ").toInt
            println("Stocks mis à jour:\nPoudre de café: " + Spoudre + "g" + "\nLait : " + Slait + "l" + "\nSucre : " + Ssucre + "g")
            Thread.sleep(1000)
            println("Retour au menu principal ")
          }
          if (modif == 2) {
            println("Retour au menu principal ")
          }
        }
      }
      else {
        exit=true
        println("Vous avez quitté le programme")
      }
      code = 0
      boisson = 0
      sucre = 0
      prix = 0.0
      lait = 0
      dose = 0
      taille = 0
      choix = 0
      modif = 0
      psucre = 0
      llait = 0
    }
  }
}

