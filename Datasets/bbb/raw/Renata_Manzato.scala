import scala.io.StdIn._

object Nospresso2 {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var codedepaiement = ""
    for (_ <- 1 to 5) {
      val randomChar = alphanumerique((math.random * alphanumerique.length).toInt)
      codedepaiement += randomChar
    }
    val machinePins = Array.fill(nbMachines)("434343")
    val Poudre_de_cafe = Array(50, 50, 50, 50, 50)
    val lait = Array(500, 500, 500, 500, 500)
    val sucre = Array(30, 30, 30, 30, 30)
    var reiteration1 = true

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var tentatives = 3
      while (tentatives > 0) {
        print("Entrez le code PIN : \n> ")
        val PIN_saisi = readLine()
        if (PIN_saisi == machinePins(machineId)) {
          println("Accès accordé. \n")
          return true
        } else {
          tentatives -= 1
          if (tentatives > 0) {
            println("PIN incorrect, " + tentatives + " tentatives restantes.")
          } else {
            println("Trop de tentatives échouées. Fin du programme.")
            return false
          }
        }
      }
      false
    }
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Réapprovisionnement des stocks.")

      println("Niveaux de stock actuels : ")
      println("Poudre de café : " + coffeeStocks(machineId) + "g")
      println("Sucre : " + sugarStocks(machineId) + "g")
      println("Lait : " + milkStocks(machineId) + " ml")

      print("Entrez la quantité de poudre de café à ajouter (g) : ")
      val ajoutCafe = readInt()
      print("Entrez la quantité de sucre à ajouter (g) : ")
      val ajoutSucre = readInt()
      print("Entrez la quantité de lait à ajouter (L) : ")
      val ajoutLait = readDouble()

      coffeeStocks(machineId) += ajoutCafe
      sugarStocks(machineId) += ajoutSucre
      milkStocks(machineId) = milkStocks(machineId) + (ajoutLait * 1000).toInt

      println("Les stocks ont été mis à jour avec succès.")
      println("Poudre de café : " + coffeeStocks(machineId) + "g")
      println("Sucre : " + sugarStocks(machineId) + "g")
      println("Lait : " + milkStocks(machineId) + " ml")
    }
    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

      var nouveauPIN = ""
      var estvalide = false

      while (!estvalide) {
        println("Entrez un nouveau code PIN à 6 chiffres >")
        nouveauPIN = readLine()

        if (nouveauPIN.length == 6) {
          estvalide = true
        }
      }
      machinePins(machineId) = nouveauPIN
      println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")

    }
    def serveClient(machineId: Int, Poudre_de_cafe: Array[Int], sucre: Array[Int], lait: Array[Int]): Boolean = {
      var prixtotal = 0.00

      var choixboisson = 0
      while (choixboisson < 1 || choixboisson > 3) {
        print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
        choixboisson = readInt()
        if (choixboisson < 1 || choixboisson > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
        }
      }
      if (choixboisson == 1) {
        if (Poudre_de_cafe(machineId) < 8) {
          return false
        } else {
          prixtotal += 2.00
          Poudre_de_cafe(machineId) -= 8
        }
        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre(machineId) < 5 || choixsucre == 3 && sucre(machineId) < 10 || choixsucre == 4 && sucre(machineId) < 15) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            print("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.\n")
          }
          if (choixsucre == 2 && sucre(machineId) < 5) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 3 && sucre(machineId) < 10) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 4 && sucre(machineId) < 15) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 2) {
            prixtotal += 0.10
            sucre(machineId) -= 5
          }
          if (choixsucre == 3) {
            prixtotal += 0.20
            sucre(machineId) -= 10
          }
          if (choixsucre == 4) {
            prixtotal += 0.30
            sucre(machineId) -= 15
          }
        }
      }
      if (choixboisson == 2){
        if (Poudre_de_cafe(machineId) < 6 || lait(machineId) < 100) {
        println("Stock insuffisant.")
        return false
      } else {
        prixtotal += 2.50
        Poudre_de_cafe(machineId) -= 6
        lait(machineId) -= 100
      }
        var choixsucre = 0
        while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre(machineId) < 5 || choixsucre == 3 && sucre(machineId) < 10 || choixsucre == 4 && sucre(machineId) < 15) {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          choixsucre = readInt()
          if (choixsucre < 1 || choixsucre > 4) {
            print("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.\n")
          }
          if (choixsucre == 2 && sucre(machineId) < 5) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 3 && sucre(machineId) < 10) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 4 && sucre(machineId) < 15) {
            println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
          }
          if (choixsucre == 2) {
            prixtotal += 0.10
            sucre(machineId) -= 5
          }
          if (choixsucre == 3) {
            prixtotal += 0.20
            sucre(machineId) -= 10
          }
          if (choixsucre == 4) {
            prixtotal += 0.30
            sucre(machineId) -= 15
          }
        }
        var choixlait = 0
        while (choixlait < 1 || choixlait > 2) {
          print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pout Cappucino et Latte)\n1) Oui\n2) Non\n>")
          choixlait = readInt()
          if (choixlait < 1 || choixlait > 2) {
            println("Choix invalide. Veuillez sélectionner 1 ou 2.")
          }
        }
        if (choixlait == 1) {
          var choixdose = 0
          while (choixdose < 1 || choixdose > 3 || choixdose == 1 && lait(machineId) < 50 || choixdose == 2 && lait(machineId) < 100 || choixdose == 3 && lait(machineId) < 150) {
            print("Combien de doses ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) Trois doses (150ml) - CHF 0.15\n>")
            choixdose = readInt()
            if (choixdose < 1 || choixdose > 3) {
              println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
            }
            if (choixdose == 1 && lait(machineId) < 50) {
              println("Stock insuffisant.\n")
              return false
            }
            if (choixdose == 2 && lait(machineId) < 100) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
            if (choixdose == 3 && lait(machineId) < 150) {
              println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
            }
          }
          if (choixdose == 1) {
            prixtotal += 0.05
            lait(machineId) -= 50
          }
          if (choixdose == 2) {
            prixtotal += 0.10
            lait(machineId) -= 100
          }
          if (choixdose == 3) {
            prixtotal += 0.15
            lait(machineId) -= 150
          }
        }

      }
      if (choixboisson == 3){
        var choixtaille = 0
        while (choixtaille < 1 || choixtaille > 3) {
          print("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>")
          choixtaille = readInt()
          if (choixtaille == 1 && (Poudre_de_cafe(machineId) < 6 || lait(machineId) < 120)) {
            println("Stock insuffisant.")
            return false
          } else if (choixtaille == 2 && (Poudre_de_cafe(machineId) < 8 || lait(machineId) < 150)) {
            println("Stock insuffisant.")
            return false
          } else if (choixtaille == 3 && (Poudre_de_cafe(machineId) < 12 || lait(machineId) < 200)) {
            println("Stock insuffisant.")
            return false
          } else if (choixtaille == 1) {
            prixtotal += 2.70
            Poudre_de_cafe(machineId) -= 6
            lait(machineId) -= 120
          }
          else if (choixtaille == 2) {
            prixtotal += 3.20
            Poudre_de_cafe(machineId) -= 8
            lait(machineId) -= 150
          }
          else if (choixtaille == 3) {
            prixtotal += 3.70
            Poudre_de_cafe(machineId) -= 12
            lait(machineId) -= 200
          }
          var choixsucre = 0
          while (choixsucre < 1 || choixsucre > 4 || choixsucre == 2 && sucre(machineId) < 5 || choixsucre == 3 && sucre(machineId) < 10 || choixsucre == 4 && sucre(machineId) < 15) {
            print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
            choixsucre = readInt()
            if (choixsucre < 1 || choixsucre > 4) {
              print("Choix invalide. Veuillez sélectionner 1, 2, 3 ou 4.\n")
            }
            if (choixsucre == 2 && sucre(machineId) < 5) {
              println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
            }
            if (choixsucre == 3 && sucre(machineId) < 10) {
              println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
            }
            if (choixsucre == 4 && sucre(machineId) < 15) {
              println("Erreur : quantité de sucre insuffisante.\nVeuillez choisir une quantité de sucre inferieure.\n")
            }
            if (choixsucre == 2) {
              prixtotal += 0.10
              sucre(machineId) -= 5
            }
            if (choixsucre == 3) {
              prixtotal += 0.20
              sucre(machineId) -= 10
            }
            if (choixsucre == 4) {
              prixtotal += 0.30
              sucre(machineId) -= 15
            }
          }
          var choixlait = 0
          while (choixlait < 1 || choixlait > 2) {
            print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pout Cappucino et Latte)\n1) Oui\n2) Non\n>")
            choixlait = readInt()
            if (choixlait < 1 || choixlait > 2) {
              println("Choix invalide. Veuillez sélectionner 1 ou 2.")
            }
          }
          if (choixlait == 1) {
            var choixdose = 0
            while (choixdose < 1 || choixdose > 3 || choixdose == 1 && lait(machineId) < 50 || choixdose == 2 && lait(machineId) < 100 || choixdose == 3 && lait(machineId) < 150) {
              print("Combien de doses ?\n1) Une dose (50ml) - CHF 0.05\n2) Deux doses (100ml) - CHF 0.10\n3) Trois doses (150ml) - CHF 0.15\n>")
              choixdose = readInt()
              if (choixdose < 1 || choixdose > 3) {
                println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
              }
              if (choixdose == 1 && lait(machineId) < 50) {
                println("Stock insuffisant.\n")
                return false
              }
              if (choixdose == 2 && lait(machineId) < 100) {
                println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
              }
              if (choixdose == 3 && lait(machineId) < 150) {
                println("Erreur : quantité de lait insuffisante.\nVeuillez choisir une dose inferieure.\n")
              }
            }
            if (choixdose == 1) {
              prixtotal += 0.05
              lait(machineId) -= 50
            }
            if (choixdose == 2) {
              prixtotal += 0.10
              lait(machineId) -= 100
            }
            if (choixdose == 3) {
              prixtotal += 0.15
              lait(machineId) -= 150
            }
          }

        }
      }
      println(f"Prix total : $prixtotal%.2f CHF")
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + codedepaiement + "\n(En attente de validation du paiement...)")
      Thread.sleep(2000)
      println("Merci, votre paiement a été accepté.\n")
      println("Préparation de votre boisson...\n[...]\n")
      Thread.sleep(3000)
      println("Votre boisson est prête ! Bonne dégustation !\n")

      true
    }


    while (reiteration1) {
      var choixmode = 0
      while (choixmode < 1 || choixmode > 3) {
        print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
        choixmode = readInt()
        if (choixmode < 1 || choixmode > 3) {
          println("Choix invalide. Veuillez sélectionner 1, 2 ou 3.")
        }

        if (choixmode == 1 ) {
          var machineselectionne = false
          while (!machineselectionne) {
            var choixId = -1
            while (choixId < 1 || choixId > 5) {
              println("Veuillez sélectionner la machine (1-5) >")
              choixId = readInt()
              if (choixId < 1 || choixId > 5) {
                println("Choix invalide. Veuillez sélectionner 1, 2, 3, 4 ou 5")
              }
            }
            if (choixId >= 1 && choixId <= nbMachines) {
              val index = choixId - 1
              println("Vous avez sélectionné la machine " + choixId + ".")
              if (!validatePin(index, machinePins)) {
                reiteration1 = false
                machineselectionne = true
              }
              else {
                val stocksuffisant = serveClient(index, Poudre_de_cafe, sucre, lait)
                if (stocksuffisant) {
                  machineselectionne = true
                } else {
                  println("Stock insuffisant.")
                }
              }
            }
          }
        }

        if (choixmode == 2 ){
          var choixId = -1
          while(choixId < 1 || choixId > 5){
          println("Veuillez sélectionner la machine (1-5) >")
          choixId = readInt()
            if(choixId < 1 || choixId > 5){
              println("Choix invalide. Veuillez sélectionner 1, 2, 3, 4 ou 5")
            }
          }
          if (choixId >= 1 && choixId <= nbMachines) {
            val index = choixId - 1
            println("Vous avez sélectionné la machine " + choixId + ".")
            if (!validatePin(index, machinePins)){
              reiteration1 = false
            }
            else {
              println("1) Réapprovisionnement\n2) Mise à jour du code PIN")
              val choixAdmin = readInt()
              if (choixAdmin == 1) {
                restockMachine(index, Poudre_de_cafe, sucre, lait)
              }
              if (choixAdmin == 2) {
                updatePin(index,machinePins)
              }
            }
          }
        }

        if (choixmode == 3) {
          reiteration1 = false
          println("\t\tBye bye!")
        }
      }
    }
  }
}
