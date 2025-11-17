import scala.io.StdIn._
import math._
import scala.util.Random


object Main {
  def main(args: Array[String]): Unit = {
    // Stock initiaux
    var StockLait = BigDecimal(0.5).setScale(2,BigDecimal.RoundingMode.HALF_UP) // en L
    var StockSucre = 30.0 // en gr
    var StockPoudreCafe = 50.0 // en gr
    // boucle infinie
    var Nospresso = true
    while (Nospresso) {
      //Type supplement/boisson
      var sucre = ""
      var lait = ""
      var boisson = ""
      // Prix boissons
      var prixboisson = 0.0
      val Expresso = 2.00
      val Cappuccino = 2.50
      val LattePetit = 2.70
      val LatteMoyen = 3.20
      val LatteGrand = 3.70
      // Prix
      var prix = 0.0
      var prixsucre = 0.0
      var prixlait = 0.0
      // Quantité consommée
      var qsucre = 0.0
      var qlait = BigDecimal(0.0).setScale(2,BigDecimal.RoundingMode.HALF_UP)
      var qcafe = 0.0
      // Choix du mode
      var choixMode = readLine("Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ").toInt
      while ((choixMode != 1) && (choixMode != 2) && (choixMode != 3)) {
        if ((choixMode != 1) && (choixMode != 2) && (choixMode != 3)) println("Selection invalide")
        choixMode = readLine("Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ").toInt
      }

      // mode Client
      if (choixMode == 1) {
        var choixBoisson = readLine("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
        while ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) {
          if ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) println("Selction invalide")
          choixBoisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
        }

        // expresso
        if (choixBoisson == 1) {
          prixboisson = Expresso
          prix = Expresso
          boisson = "Expresso"
          lait = "Non"
          qcafe = 8.0
          qlait = 0
          //Choix quantité de sucre
          var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
            if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
            Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          }
          if (Qsucre == 1) {
            sucre = "sans sucre"
            qsucre = 0.0
          } else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            prixsucre = 0.10
            qsucre = 5.0
            prix = Expresso + 0.10
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            prixsucre = 0.20
            qsucre = 10.0
            prix = Expresso + 0.20
          } else {
            sucre = "Beaucoup (15g)"
            prixsucre = 0.30
            qsucre = 15.0
            prix = Expresso + 0.30
          }
          // Cappuccino
        } else if (choixBoisson == 2) {
          //Diminution dans les stock
          prixboisson = Cappuccino
          boisson = "Cappuccino"
          qcafe = 6
          qlait = 0.1
          prix = Cappuccino
          //Choix quantité de sucre
          var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
            if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
            Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          }
          if (Qsucre == 1) {
            sucre = "sans sucre"
            qsucre = 0
          } else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            prixsucre = 0.1
            qsucre = 5
            prix = prix + 0.10
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            prixsucre = 0.2
            qsucre = 10
            prix = prix + 0.20
          } else {
            sucre = "Beaucoup (15g)"
            prixsucre = 0.3
            qsucre = 15
            prix = prix + 0.30
          }
          // Choix Quantité de lait
          var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
          while ((Qlait != 1) && (Qlait != 2)) {
            if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
            Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
          }
          if (Qlait == 1) {
            lait = "Oui"
            var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
              if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
              DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            }
            if (DoseLaitSupp == 1) {
              prixlait = 0.05
              qlait = qlait + 0.05
              prix = prix + 0.05
            } else if (DoseLaitSupp == 2) {
              qlait = qlait + 0.1
              prixlait = 0.10
              prix = prix + 0.10
            } else {
              qlait = qlait + 0.15
              prixlait = 0.15
              prix = prix + 0.15
            }
          } else {
            lait = "Non"
            qlait = qlait + 0
          }
          // Latte
        } else {
          var Latte = readLine("\nVeuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
          while ((Latte != 1) && (Latte != 2) && (Latte != 3)) {
            if ((Latte != 1) && (Latte != 2) && (Latte != 3)) println("Selection invalide")
            Latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
          }

          // Petit latte
          if (Latte == 1) {
            prixboisson = LattePetit
            boisson = "Petit Latte"
            qlait = 0.12
            qcafe = 6
            prix = LattePetit
            // Quantité de sucre
            var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
              if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
              Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            }
            if (Qsucre == 1) {
              sucre = "sans sucre"
            } else if (Qsucre == 2) {
              sucre = "Peu (5g)"
              prixsucre = 0.1
              qsucre = 5.0
              prix = LattePetit + 0.10
            } else if (Qsucre == 3) {
              sucre = "Moyen (10g)"
              qsucre = 10.0
              prix = LattePetit + 0.20
              prixsucre = 0.2
            } else {
              sucre = "Beaucoup (15g)"
              qsucre = 15.0
              prix = LattePetit + 0.30
              prixsucre = 0.3
            }
            // Dose de lait
            var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
            while ((Qlait != 1) && (Qlait != 2)) {
              if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
              Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
            }
            if (Qlait == 1) {
              lait = "Oui"
              var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
                if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
                DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              }
              if (DoseLaitSupp == 1) {
                qlait = qlait + 0.05
                prixlait = 0.05
                prix = prix + 0.05
              } else if (DoseLaitSupp == 2) {
                qlait = qlait + 0.1
                prixlait = 0.10
                prix = prix + 0.10
              } else {
                qlait = qlait + 0.15
                prixlait = 0.15
                prix = prix + 0.15
              }
            } else  {
              lait = "Non"
              qlait = qlait + 0
            }

            // Latte moyen
          } else if (Latte == 2) {
            prixboisson = LatteMoyen
            boisson = "Moyen Latte"
            qlait = 0.15
            qcafe = 8.0
            prix = LatteMoyen
            var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
              if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
              Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            }
            if (Qsucre == 1) {
              sucre = "sans sucre"
              qsucre = 0.0
            } else if (Qsucre == 2) {
              sucre = "Peu (5g)"
              qsucre = 5.0
              prix = LattePetit + 0.10
              prixsucre = 0.1
            } else if (Qsucre == 3) {
              sucre = "Moyen (10g)"
              qsucre = 10.0
              prix = LattePetit + 0.20
              prixsucre = 0.2
            } else {
              sucre = "Beaucoup (15g)"
              qsucre = 15.0
              prix = LattePetit + 0.30
              prixsucre = 0.3
            }
            // Dose de lait
            var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
            while ((Qlait != 1) && (Qlait != 2)) {
              if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
              Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
            }
            if (Qlait == 1) {
              lait = "Oui"
              var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
                if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
                DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              }
              if (DoseLaitSupp == 1) {
                qlait = qlait + 0.05
                prixlait = 0.05
                prix = prix + 0.05
              } else if (DoseLaitSupp == 2) {
                qlait = qlait + 0.1
                prixlait = 0.10
                prix = prix + 0.10
              } else {
                qlait = qlait + 0.15
                prixlait = 0.15
                prix = prix + 0.15
              }
            } else {
              lait = "Non"
              qlait = qlait + 0.0
            }

            // Grand Latte
          } else {
            prixboisson = LatteGrand
            boisson = "Grand Latte"
            qlait = 0.2
            qcafe = 12.0
            prix = LatteGrand
            var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
              if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
              Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
            }
            if (Qsucre == 1) {
              sucre = "sans sucre"
              qsucre = 0.0
            } else if (Qsucre == 2) {
              sucre = "Peu (5g)"
              qsucre = 5.0
              prix = LatteGrand + 0.10
              prixsucre = 0.1
            } else if (Qsucre == 3) {
              sucre = "Moyen (10g)"
              qsucre = 10.0
              prix = LatteGrand + 0.20
              prixsucre = 0.2
            } else {
              sucre = "Beaucoup (15g)"
              qsucre = 15.0
              prix = LatteGrand + 0.30
              prixsucre = 0.3
            }
            // Dose de lait
            var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
            while ((Qlait != 1) && (Qlait != 2)) {
              if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
              Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
            }
            if (Qlait == 1) {
              lait = "Oui"
              var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
                if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
                DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
              }
              if (DoseLaitSupp == 1) {
                qlait = qlait + 0.05
                prixlait = 0.05
                prix = prix + 0.05
              } else if (DoseLaitSupp == 2) {
                qlait = qlait + 0.1
                prixlait = 0.10
                prix = prix + 0.10
              } else {
                qlait = qlait + 0.15
                prixlait = 0.15
                prix = prix + 0.15
              }
            } else {
              lait = "Non"
              qlait = qlait + 0.0
            }
          }
        }
        // Recapitulatif de la commande
        if (boisson == "Expresso") {
          println ("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre)
        } else {
          println ("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre + "\nLait supplémentaire : " + lait)
        }
        //cas où stock insufisant
        if (qcafe > StockPoudreCafe) {
          println("\nErreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
          StockPoudreCafe = StockPoudreCafe + qcafe
          StockLait = StockLait + qlait
          StockSucre = StockSucre + qsucre
        } else if (qlait > StockLait) {
          println ("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
          StockLait = StockLait + qlait
          StockPoudreCafe = StockPoudreCafe + qcafe
          StockSucre = StockSucre + qsucre
        } else if (qsucre > StockSucre) {
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir un autre dosage de sucre ou vérifier les stocks en mode Admin.\n")
          StockSucre = StockSucre + qsucre
          StockLait = StockLait + qlait
          StockPoudreCafe = StockPoudreCafe + qcafe


        } else {
          // supp sucre
          if ((prixsucre > 0) && (prixlait == 0)) {
            printf ("Prix total : CHF %.2f + CHF %.2f = CHF %.2f",prixboisson,prixsucre,prix)
          }
          // supp sucre et lait
          else if ((prixsucre > 0) && (prixlait > 0)) {
            printf ("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f",prixboisson,prixsucre,prixlait,prix)
          }
          // supp lait
          else if ((prixsucre == 0) && (prixlait > 0)){
            printf ("Prix total : CHF %.2f + CHF %.2f = CHF %.2f",prixboisson,prixlait,prix)
          }
          // pas supp
          else {
            printf ("Prix total : CHF %.2f",prixboisson)
          }
          // Paiement
          val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          val random = new Random()
          var codeTwint = ""
          for (_ <- 1 to 5) {
            val charAleatoire = caracteres(random.nextInt(caracteres.length))
            codeTwint += charAleatoire
          }
          println (("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : ") + codeTwint + ("\nEn attente de validation du paiement...\n"))
          Thread.sleep(3000)
          println ("Merci ! Votre paiement a été accepté.")

          println ("Préparation de votre boisson...")
          println ("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
        }
        // Diminution des stocks
        StockLait = StockLait - qlait
        StockSucre = StockSucre - qsucre
        StockPoudreCafe = StockPoudreCafe - qcafe

        // mode Admin
      } else if (choixMode == 2) {
        println("\nMode admin")
        var codePin = readLine("Entrez le code PIN : ")
        while (codePin != "434343") {
          if (codePin != "434343") println("Code erronné, veuillez réssayer svp")
          codePin = readLine("Entrez le code PIN : ")
        }
        println ("Accès autorisé.\n\n")
        println ("Sotck : \nPoudre à café : " + StockPoudreCafe + " g")
          printf("Lait : %.2f L",StockLait)
          println("\nSucre : " + StockSucre + " g\n")
        println("Réapprovisionnent des stocks...\nAjout :")
        StockPoudreCafe = readLine("Poudre à café (en g) : ").toDouble + StockPoudreCafe
        StockLait = readLine("Lait (en L) : ").toDouble + StockLait
        StockSucre = readLine("Sucre (en g) : ").toDouble + StockSucre
        println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")

        // mode Quitter
      } else {
          Nospresso = false
          println("Quitter")
        }
      }
      }
      }