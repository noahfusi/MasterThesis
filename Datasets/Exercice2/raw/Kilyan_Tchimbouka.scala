import scala.io.StdIn._

object Main {



  def main(args: Array[String]): Unit = {
    var prog = 1
    var machine = 0
    var poudre = Array(50, 50, 50, 50, 50)
    var sucre = Array(30, 30, 30, 30, 30)
    var lait = Array(500, 500, 500, 500, 500)
    val machinePins = Array("434343", "434343", "434343", "434343", "434343")
    val nbMachines = 5
    var machineId = 0

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var tentativereste = 3
      println("Veuillez entrer le mot de passe pour accéder aux paramètres.")
      var mdpsaisie = readLine()

      if (mdpsaisie == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      }
      while (mdpsaisie != machinePins(machineId) && tentativereste > 1) {
        tentativereste -= 1
        println("Mot de passe incorrect. Il vous reste " + tentativereste + " tentative(s).")
        mdpsaisie = readLine()
      }

      if (mdpsaisie != machinePins(machineId)) {
        println("Nombre de tentatives dépassé. Accès refusé.")
        return false

      } else {
        println("Accès autorisé.")
        return true
      }
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
      println("Entrez un nouveau code PIN à 6 chiffres :")
      var newMachinePins = readLine()

      while (newMachinePins.length != 6 || !numberPin(newMachinePins)) {
        println("Entrée incorrecte. Votre mot de passe doit contenir exactement 6 chiffres.")
        println("Entrez un nouveau code PIN à 6 chiffres :")
        newMachinePins = readLine()
      }
      machinePins(machineId) = newMachinePins

      println("Le code PIN pour la Machine " + (machineId + 1) + " a été mis à jour avec succès.")
    }

    def numberPin(pin: String): Boolean = {
      pin.forall(c => c >= '0' && c <= '9')
    }


    while (prog == 1) {
      def serveClient(numeromachine: Int, stock1: Array[Int], stock2: Array[Int], stock3: Array[Int]): Boolean = {

        var prixdose = 0.0
        var extraLait = 0
        println("Sélectionnez votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70")
        print("> ")
        var boisson = readInt()

        while (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70")
          print("> ")
          boisson = readInt()


        }

        var boissonnom = ""
        var prixboisson = 0.0
        var besoinPoudre = 0
        var besoinLait = 0

        if (boisson == 1) {
          boissonnom = "Expresso"
          prixboisson = 2.00
          besoinPoudre = 8
          besoinLait = 0
        } else if (boisson == 2) {
          boissonnom = "Cappuccino"
          prixboisson = 2.50
          besoinPoudre = 6
          besoinLait = 100
        } else if (boisson == 3) {
          boissonnom = "Latte"
          println("Veuillez sélectionner la taille :")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          var taille = readInt()
          while (taille != 1 && taille != 2 && taille != 3) {
            println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
            println("Veuillez sélectionner la taille :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            print("> ")
            taille = readInt()
          }
          if (taille == 1) {
            prixboisson = 2.70
            besoinPoudre = 6
            besoinLait = 120
          } else if (taille == 2) {
            prixboisson = 3.20
            besoinPoudre = 8
            besoinLait = 150
          } else if (taille == 3) {
            prixboisson = 3.70
            besoinPoudre = 12
            besoinLait = 200
          }
        }
        println("Choisissez le niveau de sucre :")
        println("1) Sans sucre (0g)")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        var sucreclient = readInt()
        while (sucreclient != 1 && sucreclient != 2 && sucreclient != 3 && sucreclient != 4) {
          println("Entrée invalide, veuillez saisir '1', '2' , '3' ou '4'")
          println("Choisissez le niveau de sucre :")
          println("1) Sans sucre (0g)")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          sucreclient = readInt()
        }

        var sucrePrix = 0.0
        var besoinSucre = 0

        if (sucreclient == 1) {
          sucrePrix = 0.0
          besoinSucre = 0
        } else if (sucreclient == 2) {
          sucrePrix = 0.10
          besoinSucre = 5
        } else if (sucreclient == 3) {
          sucrePrix = 0.20
          besoinSucre = 10
        } else if (sucreclient == 4) {
          sucrePrix = 0.30
          besoinSucre = 15
        }
        if (boisson == 2 || boisson == 3) {
          println("Ajout de lait supplémentaire :")
          println("1) Non")
          println("2) 1 dose (50ml) - CHF 0.05")
          println("3) 2 doses (100ml) - CHF 0.10")
          println("4) 3 doses (150ml) - CHF 0.15")
          print("> ")
          var choixlait = readInt()
          while (choixlait != 1 && choixlait != 2 && choixlait != 3 && choixlait != 4) {
            println("Entrée invalide, veuillez saisir '1', '2' , '3' ou '4'")
            println("Ajout de lait supplémentaire :")
            println("1) Non")
            println("2) 1 dose (50ml) - CHF 0.05")
            println("3) 2 doses (100ml) - CHF 0.10")
            println("4) 3 doses (150ml) - CHF 0.15")
            print("> ")
            choixlait = readInt()
          }
          if (choixlait == 1) {
            prixdose = 0
            extraLait = 0
          } else if (choixlait == 2) {
            prixdose = 0.05
            extraLait = 50
          } else if (choixlait == 3) {
            prixdose = 0.10
            extraLait = 100
          } else if (choixlait == 4) {
            prixdose = 0.15
            extraLait = 150
          }
        }
        var prixTotal = prixboisson + sucrePrix + prixdose
        if (stock1(machineId) < besoinPoudre) {
          println("Erreur : Poudre insuffisante pour préparer votre boisson.")
          return false
        }
        if (stock2(machineId) < besoinSucre) {
          println("Erreur : Sucre insuffisant pour préparer votre boisson.")
          return false
        }
        def TwintCode(): String = {
          val caractères = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var twint = ""
          for (i <- 1 to 5) {
            val choixcaractere = (Math.random() * 36).toInt
            twint += caractères(choixcaractere)
          }
          return twint

        }

        var twint = TwintCode()

        def Prepboisson(machineId: Int, poudreReq: Int, sucreReq: Int, laitReq: Int, nom: String): Unit = {
          poudre(machineId) -= poudreReq
          sucre(machineId) -= sucreReq
          lait(machineId) -= laitReq
          println("Votre " + nom + " est prêt ! Bonne dégustation !")
        }
        if (stock3(machineId) < besoinLait) {
          println("Erreur : Lait insuffisant pour préparer votre boisson.")
          return false
        } else {
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixboisson, prixdose, sucrePrix, prixTotal)
          println("Code Twint : " + twint)
          println("En attente de paiement...\n")
          Thread.sleep(3000)
          println("Paiement confirmé.")
          Prepboisson(machineId, besoinPoudre, besoinSucre, besoinLait + extraLait, boissonnom)
          return true
        }
      }
      println("Veuillez sélectionner une machine de 1 à 5 \n> ")
      machineId = readInt() - 1
      while (machineId < 0 || machineId > 4) {
        println("Entrée invalide. Veuillez choisir une machine entre 1 et 5 :")
        print("> ")
        machineId = readInt() - 1
      }
      var machineaffiche = machineId + 1
      println("La machine sélectionnée est la " + machineaffiche)

      println("\n        Nospresso Café        ")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      var mode = readInt()

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Entrée invalide, veuillez saisir '1', '2', ou '3'")
        println("        Nospresso Café        ")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        mode = readInt()
      }

      if (mode == 1) {
        var boissonPrete = false
        while (!boissonPrete) {
          boissonPrete = serveClient(machineId, poudre, sucre, lait)
        }
      } else if (mode == 2) {
        adminMode(machineId)
      } else {
        prog = 0
      }

      def adminMode(machineId: Int): Unit = {
        println("Mode Admin")
        val accesautorise = validatePin(machineId, machinePins)

        if (accesautorise) {
          println("Que souhaitez-vous faire ? \n 1) Modifier le mot de passe de la machine \n 2) Réapprovisionner les stocks de la machine")
          var adminChoix = readInt()
          while (adminChoix != 1 && adminChoix != 2) {
            println("Entrée incorrecte, veuillez saisir 1 ou 2")
            println("Que souhaitez-vous faire ? \n 1) Modifier le mot de passe de la machine \n 2) Réapprovisionner les stocks de la machine")
            adminChoix = readInt()
          }
          if (adminChoix == 1) {
            updatePin(machineId, machinePins)
          } else if (adminChoix == 2) {
            restockMachine(machineId, poudre, sucre, lait)
          }
        } else {
          println("Accès refusé. Programme terminé.")
          prog = 0
        }
      }


      def restockMachine(machineId: Int, stock1: Array[Int], stock2: Array[Int], stock3: Array[Int]): Unit = {
        println("Accès autorisé.")
        println("Stocks Actuels :\n")
        println("Poudre de café : " + poudre(machineId) + "g")
        println("Lait : " + lait(machineId) + " ml")
        println("Sucre : " + sucre(machineId) + "g")

        println("Ajout de poudre (g) :")
        print("> ")
        var ajoutpoudre = readInt()
        while (ajoutpoudre < 0) {
          println("Valeur invalide. Réessayez.")
          print("> ")
          ajoutpoudre = readInt()
        }
        poudre(machineId) += ajoutpoudre

        println("Ajout de lait (ml) :")
        print("> ")
        var ajoutlait = readInt()
        while (ajoutlait < 0) {
          println("Valeur invalide. Réessayez.")
          print("> ")
          ajoutlait = readInt()
        }
        lait(machineId) += ajoutlait

        println("Ajout de sucre (g) :")
        print("> ")
        var ajoutsucre = readInt()
        while (ajoutsucre < 0) {
          println("Valeur invalide. Réessayez.")
          print("> ")
          ajoutsucre = readInt()
        }
        sucre(machineId) += ajoutsucre

        println("Stocks mis à jour.")
      }
    }
  }
}
