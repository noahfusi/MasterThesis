import scala.io.StdIn._
import scala.util.Random

object Main {
  val nbMachines = 5
  var StockPoudreCafe = Array(50, 50, 50, 50, 50)
  var StockSucre = Array(30, 30, 30, 30, 30)
  var StockLait = Array(500, 500, 500, 500, 500)
  val machinePins = Array.fill(nbMachines)("434343")

  def main(args: Array[String]): Unit = {
    var mode = 0
    var loop = true
    while (loop) {
      println("\nMachine sélectionnée (1-5) > ")
      var machineId = readLine().toInt
      if (machineId >= 1 && machineId < nbMachines + 1) {
        machineId -= 1
        println("1) Client\n2) Admin\n3) Exit")
        mode = readLine().toInt
        if (mode == 1) {
          modeclient(machineId, StockPoudreCafe, StockSucre, StockLait)
        }
        if (mode == 2) {
          if (validatePin(machineId, machinePins)) {
            println("1) Réapprovisionnement machine\n2) Mise à jour du code PIN")
            val choixadmin = readLine().toInt
            if (choixadmin == 1) {
              restockMachine(machineId, StockPoudreCafe, StockSucre, StockLait)
            }
            else if (choixadmin == 2) {
              updatePin(machineId, machinePins)
            }
            else {
              println("Option invalide. Retour au menu principal")
            }
          }
        }
        if (mode == 3) {
          println("Merci à bientot!")
          loop = false
        }
      }
    }
  }

  def modeclient(machineId: Int, StockPoudreCafe: Array[Int], StockSucre: Array[Int], StockLait: Array[Int]): Boolean = {

    {
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
      var qsucre = 0
      var qlait = 0
      var qcafe = 0
      // Choix boisson
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
        qcafe = 8
        qlait = 0
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
          prixsucre = 0.10
          qsucre = 5
          prix = Expresso + 0.10
        } else if (Qsucre == 3) {
          sucre = "Moyen (10g)"
          prixsucre = 0.20
          qsucre = 10
          prix = Expresso + 0.20
        } else {
          sucre = "Beaucoup (15g)"
          prixsucre = 0.30
          qsucre = 15
          prix = Expresso + 0.30
        }
        // Cappuccino
      }
      else if (choixBoisson == 2) {
        //Diminution dans les stock
        prixboisson = Cappuccino
        boisson = "Cappuccino"
        qcafe = 6
        qlait = 100
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
            qlait = qlait + 50
            prix = prix + 0.05
          } else if (DoseLaitSupp == 2) {
            qlait = qlait + 100
            prixlait = 0.10
            prix = prix + 0.10
          } else {
            qlait = qlait + 150
            prixlait = 0.15
            prix = prix + 0.15
          }
        } else {
          lait = "Non"
          qlait = qlait + 0
        }
        // Latte
      }
      else {
        var Latte = readLine("\nVeuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
        while ((Latte != 1) && (Latte != 2) && (Latte != 3)) {
          if ((Latte != 1) && (Latte != 2) && (Latte != 3)) println("Selection invalide")
          Latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
        }

        // Petit latte
        if (Latte == 1) {
          prixboisson = LattePetit
          boisson = "Petit Latte"
          qlait = 120
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
          }
          else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            prixsucre = 0.1
            qsucre = 5
            prix = LattePetit + 0.10
          }
          else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LattePetit + 0.20
            prixsucre = 0.2
          }
          else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
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
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            }
            else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            }
            else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          }
          else {
            lait = "Non"
            qlait = qlait + 0
          }

          // Latte moyen
        }
        else if (Latte == 2) {
          prixboisson = LatteMoyen
          boisson = "Moyen Latte"
          qlait = 150
          qcafe = 8
          prix = LatteMoyen
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
            qsucre = 5
            prix = LatteMoyen + 0.10
            prixsucre = 0.1
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LatteMoyen + 0.20
            prixsucre = 0.2
          } else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
            prix = LatteMoyen + 0.30
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
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            } else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            } else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          } else {
            lait = "Non"
          }

          // Grand Latte
        }
        else {
          prixboisson = LatteGrand
          boisson = "Grand Latte"
          qlait = 200
          qcafe = 12
          prix = LatteGrand
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
            qsucre = 5
            prix = LatteGrand + 0.10
            prixsucre = 0.1
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LatteGrand + 0.20
            prixsucre = 0.2
          } else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
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
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            } else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            } else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          } else {
            lait = "Non"
          }
        }
      }
      // Recapitulatif de la commande
      if (boisson == "Expresso") {
        println("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre)
      }
      else {
        println("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre + "\nLait supplémentaire : " + lait)
      }
      //cas où stock insufisant
      if (qcafe > StockPoudreCafe(machineId)) {
        println("\nErreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        StockPoudreCafe(machineId) = StockPoudreCafe(machineId) + qcafe
        StockLait(machineId) = StockLait(machineId) + qlait
        StockSucre(machineId) = StockSucre(machineId) + qsucre
      }
      else if (qlait > StockLait(machineId)) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        StockLait(machineId) = StockLait(machineId) + qlait
        StockPoudreCafe(machineId) = StockPoudreCafe(machineId) + qcafe
        StockSucre(machineId) = StockSucre(machineId) + qsucre
      }
      else if (qsucre > StockSucre(machineId)) {
        println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir un autre dosage de sucre ou vérifier les stocks en mode Admin.\n")
        StockSucre(machineId) = StockSucre(machineId) + qsucre
        StockLait(machineId) = StockLait(machineId) + qlait
        StockPoudreCafe(machineId) = StockPoudreCafe(machineId) + qcafe
      }
      else {
        // supp sucre
        if ((prixsucre > 0) && (prixlait == 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
        }
        // supp sucre et lait
        else if ((prixsucre > 0) && (prixlait > 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixlait, prix)
        }
        // supp lait
        else if ((prixsucre == 0) && (prixlait > 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
        }
        // pas supp
        else {
          printf("Prix total : CHF %.2f", prixboisson)
        }
        val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        val random = new Random()
        var codeTwint = ""
        for (_ <- 1 to 5) {
          val charAleatoire = caracteres(random.nextInt(caracteres.length))
          codeTwint += charAleatoire
        }
        println(("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : ") + codeTwint + ("\nEn attente de validation du paiement...\n"))
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")

        println("Préparation de votre boisson...")
        println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
      }
      // Diminution des stocks
      StockLait(machineId) = StockLait(machineId) - qlait
      StockSucre(machineId) = StockSucre(machineId) - qsucre
      StockPoudreCafe(machineId) = StockPoudreCafe(machineId) - qcafe
    }
    true
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var essaies = 2
    var codePin = readLine("Entrez le code PIN : ")
    while (codePin != machinePins(machineId)) {
      println("Code PIN incorrrect. " + essaies + " tentatives restantes")
      if (essaies <= 0) {
        println("\nTrop de tentatives échouées. Fin du programme.")
        System.exit(0)
      }
      codePin = readLine("Entrez le code PIN : ")
      essaies -= 1
    }
    println("Accès accordé à la Machine " + (machineId + 1) + ".\n")
    true
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    var codeValide = false
    while (!codeValide) {
      println("Entrez un nouveau code PIN à 6 chiffres > ")
      val pinAsString = readLine()

      if (pinAsString.length == 6 && pinAsString.forall(_.isDigit)) {
        machinePins(machineId) = pinAsString
        println("Le code PIN a été mis à jour avec succes.\nRetour au menu principal...\n")
        codeValide = true
      } else {
        println("Le code Pin n'a pas été mis à jour.\nRetour au menu principal...\n")
        return false
      }
    }
  }


  def restockMachine(machineId: Int, StockPoudreCafe: Array[Int], StockSucre: Array[Int], StockLait: Array[Int]): Unit = {

    println("Niveau de stock actuels : \nPoudre à café : " + StockPoudreCafe(machineId) + " g")
    println("Lait : " + StockLait(machineId) / 1000.0)
    println("Sucre : " + StockSucre(machineId) + " g\n")
    println("Entrez les quantité à ajouter :\n")
    StockPoudreCafe(machineId) = readLine("Poudre à café (en g) : ").toInt + StockPoudreCafe(machineId)
    var LaitAjoute = 0.0
    LaitAjoute = readLine("Lait (en L) : ").toDouble
    StockLait(machineId) = (LaitAjoute * 1000.0).toInt + StockLait(machineId)
    StockSucre(machineId) = readLine("Sucre (en g) : ").toInt + StockSucre(machineId)
    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")

  }
}
