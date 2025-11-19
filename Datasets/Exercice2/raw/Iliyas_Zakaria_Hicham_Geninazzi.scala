import scala.io.StdIn
import scala.util.Random

object Main {

  val nbMachines = 6
  val CodePinPardefaut = "434343"
  // Stocks et codes PIN
  val stockDePoudreDeCafe = Array.fill(nbMachines)(50.0) // en grammes
  val stockDeSucre = Array.fill(nbMachines)(30.0) // en grammes
  val stockDeLait = Array.fill(nbMachines)(0.5) // en litres
  val machinePins = Array.fill(nbMachines)(CodePinPardefaut)


    // valeur du sucre
    val sansSucre: Double = 0.00
    val peuDeSucre: Double = 0.10
    val moyenDeSucre: Double = 0.20
    val beaucoupDeSucre: Double = 0.30
    // prix des éléments
    val prixBaseExpresso = 2.00
    val prixBaseCappuccino = 2.50
    var prixDuSucre = 0.0
    var prixDuLait = 0.0
    var prixTaille = 0.0
    // quantité des éléments
    var quantiteSucre = 0.0
    var quantiteDeCafe = 0.0
    var quantiteDeLait = 0.0
    // choix des boucles
    var choixMenu = 0 // déclarer la variable à l'extérieur de la boucle do-while "choixMenu"
    var choixBoisson = 0 // déclarer la variable à l'extérieur de la boucle do-while "choixBoisson"
    var choixDuSucre = 0 // déclarer la variable à l'extérieur de la boucle do-while "choixDuSucre"

    val codeTwint = Random.alphanumeric.take(5).mkString // génère aléatoirement un string avec 5 caractère


    def main(args: Array[String]): Unit = {
      do {

         println("Nospresso Café")
         println("Veuillez sélectionner votre mode :")
         println("1) Client")
         println("2) Admin")
         println("3) Quitter")
         print("> ")


         choixMenu = StdIn.readInt() // choix de l'utilisateur du mode qu'il veut

         val machineId = selectMachine()

          if (choixMenu == 1) { // mode client
            serveClient(machineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)

          } else if (choixMenu == 2) { // mode admin
            if (validatePin(machineId, machinePins)) {
              ModeAdmin(machineId: Int)
            } else {
              return
            }

          } else if (choixMenu == 3) { // Quitter
            println("Merci et à bientôt !")
            return

          } else {
            println("Option invalide. Veuillez sélectionner un choix valide.")
          }
         } while (choixMenu != 3) // le programme redemande à l'utilisateur jusqu'à ce qu'il quitte le programme
      }

    def selectMachine(): Int = {

      var machineId = -1
      while (machineId <= 0 || machineId >= nbMachines) {
        println("Veuillez sélectionner une machine 1 à " + {
          nbMachines - 1
        })
        machineId = StdIn.readInt()
        if (machineId <= 0 || machineId >= nbMachines) {
          println("Machine invalide. Veuillez réessayer.")
        }
      }
      machineId
    }


    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      println("Machine  sélectionnée (1-5) > " + machineId)
      println("Mode admin")

      val tentativesMax = 3 // Nombre maximum de tentatives autorisées
      var tentative = 0 // Compteur de tentatives
      var PinEntree = ""
      var accessAutorise = false


      do {
        println("Veuillez entrer votre code PIN \n(tentative " + {tentative + 1}+ ("/3)") )
        PinEntree = StdIn.readLine() // Lire l'entrée de l'utilisateur

        // Vérifiez si le code PIN est correct
        if (PinEntree == machinePins(machineId)) {
          accessAutorise = true // Retourner immédiatement la validation réussie
          println("Accès autorisé. Bienvenue !")
          return true
        } else {
          tentative += 1
          if (tentative < tentativesMax) {
            println("Code PIN incorrect. Veuillez réessayer.")
          }
        }
      } while (!accessAutorise && tentative < tentativesMax)

      // Si le maximum de tentatives est atteint
      if (!accessAutorise) {
        println("Nombre maximum de tentatives dépassé. Accès refusé.")
        return false // Retour immédiat de l'échec
      }
      accessAutorise
    }


    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Machine  sélectionnée (1-5) > " + machineId)
      var newPin = ""
      do {
        print("Entrez un nouveau code PIN à 6 chiffres : ")
        newPin = StdIn.readLine()
        if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
          println("Le code PIN doit comporter exactement 6 chiffres.")
        }
      } while (newPin.length != 6 || !newPin.forall(_.isDigit))

      machinePins(machineId) = newPin
      println("Le code PIN a été mis à jour avec succès.")
    }


    def restockMachine(machineId: Int, stockDePoudreDeCafe: Array[Double], stockDeLait: Array[Double], stockDeSucre: Array[Double]): Unit = {
      println("Machine  sélectionnée (1-5) > " + machineId)
      println("Mode réapprovisionnement.\n")
      println("Niveaux de stock actuels:")
      println("Poudre de café : " + stockDePoudreDeCafe(machineId) + "g") // affichage du stock de café
      println("Lait : " + stockDeLait(machineId) + "L") // affichage du stock de lait
      println("Sucre : " + stockDeSucre(machineId) + "g") // affichage du stock de sucre


      println("\nRéapprovisionnement des stocks...\nEntrez les quantités à ajouter : ")

      println("Poudre de café >")
      stockDePoudreDeCafe(machineId) += StdIn.readLine().toDouble // ravitaillement de café
      println("Lait >")
      stockDeLait(machineId) += StdIn.readLine().toDouble // ravitaillement de lait
      println("Sucre >")
      stockDeSucre(machineId) += StdIn.readLine().toDouble // ravitaillement de suc

      println("Niveaux de stocks mis à jour.\nRetour au menu principal...")

    }

    def serveClient(machineId: Int, stockDePoudreDeCafe: Array[Double], stockDeSucre: Array[Double], stockDeLait: Array[Double]): Unit = {
      println("Machine  sélectionnée (1-5) > " + machineId)
      var currentMachineId = machineId

      do {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")

        choixBoisson = StdIn.readInt() // choix de l'utilisateur de la boisson qu'il veut


        if (choixBoisson == 1) { // choix de l'expresso
          do {

            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            print("> ")

            choixDuSucre = StdIn.readInt() // choix du sucre ou non


            if (choixDuSucre == 1) { // pas de sucre
              quantiteSucre = 0
              prixDuSucre = sansSucre
            } else if (choixDuSucre == 2) { // peu de sucre
              quantiteSucre = 5
              prixDuSucre = peuDeSucre
            } else if (choixDuSucre == 3) { // moyen de sucre
              quantiteSucre = 10
              prixDuSucre = moyenDeSucre
            } else if (choixDuSucre == 4) { // beaucoup de sucre
              quantiteSucre = 15
              prixDuSucre = beaucoupDeSucre
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (choixDuSucre != 1 && choixDuSucre != 2 && choixDuSucre != 3 && choixDuSucre != 4)
          // Boucle jusqu'à ce que l'on choisisse une option qui est disponible


          if (stockDeSucre(machineId) >= quantiteSucre) {
            stockDeSucre(machineId) = stockDeSucre(machineId) - quantiteSucre //déduire du stock de sucre
          } else {
            println("Stock insuffisant de sucre.")
            stockDeSucre(machineId) -= 0
          }
          if (stockDePoudreDeCafe(machineId) >= 8) {
            stockDePoudreDeCafe(machineId) -= 8 //déduire du stock de café
            val prixTotal = prixBaseExpresso + prixDuSucre // calculer et afficher le prix total de l'expresso
            printf("prix : CHF %.2f + CHF %.2f = CHf%.2f", prixBaseExpresso, prixDuSucre, prixTotal)
            println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint)
            println("(En attente de paiement...)")

            Thread.sleep(3000)
            println(s"\nPaiment confirmé.\nPéparation de votre boisson...")
            println("\nVotre Expresso est prêt ! Bonne dégustation !")

          } else println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          currentMachineId = selectMachine()
          serveClient(currentMachineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)

        } else if (choixBoisson == 2) { // choisir la boisson cappucinno
          do {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            print("> ")
            // choisir si l'on veut du sucre ou non

            choixDuSucre = StdIn.readInt()


            if (choixDuSucre == 1) { // pas de sucre
              quantiteSucre = 0
              prixDuSucre = sansSucre
            } else if (choixDuSucre == 2) { // peu de sucre
              quantiteSucre = 5
              prixDuSucre = peuDeSucre
            } else if (choixDuSucre == 3) { // moyen de sucre
              quantiteSucre = 10
              prixDuSucre = moyenDeSucre
            } else if (choixDuSucre == 4) { // beaucoup de sucre
              quantiteSucre = 15
              prixDuSucre = beaucoupDeSucre
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (choixDuSucre != 1 && choixDuSucre != 2 && choixDuSucre != 3 && choixDuSucre != 4)
          // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide

          if (stockDeSucre(machineId) >= quantiteSucre) {
            stockDeSucre(machineId) -= quantiteSucre //déduire du stock de sucre
          } else {
            println("Stock insuffisant de sucre.")
            stockDeSucre(machineId) -= 0
          }

          var choixDeLait = 0 // la valeur pour chosir si l'on veut du lait ou non
          do {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            choixDeLait = StdIn.readInt()

            if (choixDeLait == 1) { // avec du lait

              var dosesDeLait = 0 // déclare la variable à l'éxterieur de le boucle
              do {
                println("Combien de dose (1 dose = 50ml, maximum 3 doses) ?")
                print("> ")
                dosesDeLait = StdIn.readInt()

                if (dosesDeLait > 0 && dosesDeLait <= 3) { // nombres de doses comprisent entre 1 et 3
                } else
                  println("Option invalide. Veuillez sélectionner un choix valide.")
              } while (dosesDeLait != 0 && dosesDeLait != 1 && dosesDeLait != 2 && dosesDeLait != 3)

              val doses = dosesDeLait
              if (doses > 0 && doses <= 3 && stockDeLait(machineId) >= doses * 0.05) {
                stockDeLait(machineId) -= doses * 0.05 // le stock du lait sera soustraire de 0.05L
                prixDuLait = doses * 0.05 // le prix du lait égale 0.05chf par doses
              } else {
                println("Option invalide ou stock de lait insuffisant.")
              }
            } else if (choixDeLait == 2) { // sans lait
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (choixDeLait != 1 && choixDeLait != 2) // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide


          if (stockDePoudreDeCafe(machineId) >= 6 && stockDeLait(machineId) >= 0.1) {
            stockDePoudreDeCafe(machineId) -= 6 //déduire du stock de café
            stockDeLait(machineId) -= 0.1 //déduire du stock de lait
            var prixTotal = prixBaseCappuccino + prixDuSucre + prixDuLait // calculer et afficher le prix total du cappuccino
            printf("prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixBaseCappuccino, prixDuSucre, prixDuLait, prixTotal)
            println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint)
            println("(En attente de paiement...)")

            Thread.sleep(3000)
            println(s"\nPaiment confirmé.\nPéparation de votre boisson...")
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          } else if (stockDePoudreDeCafe(machineId) >= 6 || stockDeLait(machineId) <= 0.0) {
            println("Erreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.") // erreur pour la quantité de lait
            currentMachineId = selectMachine()
            serveClient(currentMachineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)}
          else {
            println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.") // erreur pour la quantité de café
            currentMachineId = selectMachine()
            serveClient(currentMachineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait) }
        } else if (choixBoisson == 3) { // choisir la boisson latte


          var tailleDeLaBoisson = 0

          do {
            println("Sélectionnez la taille :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            print("> ")
            tailleDeLaBoisson = StdIn.readInt()


            if (tailleDeLaBoisson == 1) { // prix et quantité nécessaire pour le petit latte
              prixTaille = 2.70
              quantiteDeCafe = 6
              quantiteDeLait = 0.12
            } else if (tailleDeLaBoisson == 2) { // prix et quantité nécessaire pour le moyen latte
              prixTaille = 3.20
              quantiteDeCafe = 8
              quantiteDeLait = 0.15
            } else if (tailleDeLaBoisson == 3) { // prix et quantité nécessaire pour le grand latte
              prixTaille = 3.70
              quantiteDeCafe = 12
              quantiteDeLait = 0.2
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (tailleDeLaBoisson != 1 && tailleDeLaBoisson != 2 && tailleDeLaBoisson != 3)
          println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")


          do {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            print("> ")
            // choisir si l'on veut du sucre ou non

            choixDuSucre = StdIn.readInt()


            if (choixDuSucre == 1) { // pas de sucre
              quantiteSucre = 0
              prixDuSucre = sansSucre
            } else if (choixDuSucre == 2) { // peu de sucre
              quantiteSucre = 5
              prixDuSucre = peuDeSucre
            } else if (choixDuSucre == 3) { // moyen de sucre
              quantiteSucre = 10
              prixDuSucre = moyenDeSucre
            } else if (choixDuSucre == 4) { // beaucoup de sucre
              quantiteSucre = 15
              prixDuSucre = beaucoupDeSucre
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (choixDuSucre != 1 && choixDuSucre != 2 && choixDuSucre != 3 && choixDuSucre != 4)
          // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide

          var choixDeLait = 0 // la valeur pour chosir si l'on veut du lait ou non

          do {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            choixDeLait = StdIn.readInt()

            if (choixDeLait == 1) { // avec du lait

              var dosesDeLait = 0 // déclare la variable à l'éxterieur de le boucle
              do {
                println("Combien de dose (1 dose = 50ml, maximum 3 doses) ?")
                print("> ")
                dosesDeLait = StdIn.readInt()

                if (dosesDeLait > 0 && dosesDeLait <= 3) { // nombres de doses comprisent entre 1 et 3
                } else
                  println("Option invalide. Veuillez sélectionner un choix valide.")
              } while (dosesDeLait != 0 && dosesDeLait != 1 && dosesDeLait != 2 && dosesDeLait != 3)

              val doses = dosesDeLait
              if (doses > 0 && doses <= 3 && stockDeLait(machineId) >= doses * 0.05) {
                stockDeLait(machineId) -= doses * 0.05 // le stock du lait sera soustraire de 0.05L
                prixDuLait = doses * 0.05 // le prix du lait égale 0.05chf par doses
              } else {
                println("Option invalide ou stock de lait insuffisant.")
              }
            } else if (choixDeLait == 2) { // sans lait
            } else {
              println("Option invalide. Veuillez sélectionner un choix valide.")
            }
          } while (choixDeLait != 1 && choixDeLait != 2) // le programme redemande à l'utilisateur jusqu'à ce qu'il donne une réponse valide


          if (stockDePoudreDeCafe(machineId) >= quantiteDeCafe && stockDeLait(machineId) >= quantiteDeLait) {
            stockDePoudreDeCafe(machineId) -= quantiteDeCafe //déduire du stock de café
            stockDeLait(machineId) -= quantiteDeLait //déduire du stock de lait

            val prixTotal = prixTaille + prixDuSucre + prixDuLait // calculer et afficher le prix total du Latte
            printf("prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixTaille, prixDuSucre, prixDuLait, prixTotal)
            println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint)
            println("(En attente de paiement...)")

            Thread.sleep(3000)
            println(s"\nPaiment confirmé.\nPéparation de votre boisson...")
            println("Votre Latte est prêt ! Bonne dégustation !")
          } else if (stockDePoudreDeCafe(machineId) >= 6 || stockDeLait(machineId) <= 0.0) {
            println("Erreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.") // erreur pour la quantité de lait
            currentMachineId = selectMachine()
            serveClient(currentMachineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)
          } else {
            println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.") // erreur pour la quantité de café
            currentMachineId = selectMachine()
            serveClient(currentMachineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)
          }
        } else {
          println("Option invalide. Veuillez sélectionner un choix valide.")
        }
      } while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)
    }


    def ModeAdmin(machineId: Int): Unit = {
      println("Machine  sélectionnée (1-5) > " + machineId)
      println("1) Reapprovisionner")
      println("2) Mettre à jour le code PIN")
      print("> ")

        val ChoixDeAdmin = StdIn.readInt()

        if (ChoixDeAdmin == 1) {
          restockMachine(machineId, stockDePoudreDeCafe, stockDeSucre, stockDeLait)
        } else if (ChoixDeAdmin == 2) {
          updatePin(machineId, machinePins)
        } else {
          println("Option invalide, retour au menu principal.")
        }
      }

}
