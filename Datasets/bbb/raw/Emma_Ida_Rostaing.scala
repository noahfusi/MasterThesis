

import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

   val nbMachines = 5

    // stock de chaque machine
    var coffeeStocks = Array.fill(nbMachines)(50) // grammes
    var sugarStocks = Array.fill(nbMachines)(30) // grammes
    var milkStocks = Array.fill(nbMachines)(500) // litres
    var machinePinAdmin = Array.fill(nbMachines)("434343")


    var dosesucre = 5 // g / dose

    var retour_erreur = false

    var continuer = true
    while (continuer) {
      var mode: Int = 0

      println("   Nospresso Cafe   \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter")

      do {
        mode = readLine("> ").toInt
      } while (mode < 1 || mode > 3)

      if (mode == 1) {
        var machine = readLine("Choisissez une machine (1-5) >").toInt - 1

        while (machine < 0 || machine >= nbMachines) {
          machine = readLine("Votre séléction n'est pas correcte, veuillez sélectionner une machine entre 1 et 5 \n >").toInt - 1
        }

        println (s"Vous avez sélectionné la machine ${machine + 1}")

        if (machine >=0 && machine < nbMachines) {
          serveClient(machine, coffeeStocks, sugarStocks, milkStocks)
        }
        
      } else if (mode == 2) {
        var action = readLine("Que souhaitez-vous sélectionner ? \n 1) Réaprovisionner le stock \n 2) Mettre à jour le code PIN \n > ").toInt

        while (action != 1 && action != 2) {
          action = readLine("Votre sélection n'est pas correcte, veuillez choisir une machine entre 1 et 5 \n >").toInt
        }
            var machine = readLine ("Choisissez une machine (1-5) >").toInt - 1
            while (machine < 0 || machine >= nbMachines) {
              machine = readLine("Votre séléction n'est pas correcte, veuillez sélectionner une machine entre 1 et 5 \n >").toInt - 1 
            }
        if (machine >= 0 && machine < nbMachines) {
          if (validatePin(machine, machinePinAdmin)){
             println ("Accès accordé à la machine " + (machine + 1))

              if (action  == 1 ){
                restockMachine(machine, coffeeStocks,sugarStocks, milkStocks )

              } else if (action == 2) {
                updatePin(machine, machinePinAdmin)
              }
          } else {
            println ("Trop de tentative échouées. Fin du programme. \n")
          }
        }
      } else if (mode == 3) {
        continuer = false
      }
    }

    def validatePin(machineId: Int, machinePinAdmin: Array[String]): Boolean = {
      // valider codes PIN d'une des machines
      var tentative = 0
      val max_tentative = 3

      while (tentative < max_tentative){
        val entrerPIN = readLine (s"Veuillez entrer le code PIN correspondant à la machine sélectionnée : " + (machineId + 1) + " \n >")

        if (entrerPIN == machinePinAdmin(machineId)) {
          println("Code PIN correct, Accès accordé.")
          return true
        } else {
          tentative += 1
          println(s"Code PIN incorrect. Tentative ${tentative} sur ${max_tentative}.")
        }
      }
      false
    }


    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var continuer = true
      while (continuer) {
        println("Veuillez entrer le nouveau code PIN (6 chiffres) : ")
        var nouveauPin = readLine(">")
        if (nouveauPin.length == 6){
          continuer = false
          machinePins(machineId) = nouveauPin
          println("Le code Pin a été mis à jour avec succès.")
        }
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array [Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

          // mettre toutes les valeurs
          var prixdosesucre = 0.10
          var prixdoselait = 0.0
          var prixexpresso = 2.00
          var prixcappuccino = 2.50
          var prixlattepetit = 2.70
          var prixlattemoyen = 3.20
          var prixlattegrand = 3.70

          var boisson: Int = 0
          var taillelatte: Int = 0
          var prixboisson = 0.0
          var besoincafe: Double = 0
          var besoinsucre: Double = 0
          var besoinlait= 0
          var nomboisson: String = ""

          var nomsucre = ""
          var sucre: Int = 0
          var prixsucre = 0.0
          var sans_sucre = 0
          var peu_sucre = 5
          var moyen_sucre = 10
          var beaucoup_sucre = 15

          println("Veuillez sélectionner votre boisson : \n1) Expresso – CHF 2.00 \n2) Cappuccino – CHF 2.50 \n3) Latte – CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

          do {
            boisson = readLine("> ").toInt
            if (boisson < 1 || boisson > 3) {
              println("Votre sélection n'est pas correcte, veuillez choisir entre:\n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            }
          } while (boisson < 1 || boisson > 3)


          if (boisson == 1) {
            nomboisson = "Expresso"
            prixboisson = prixexpresso
            besoincafe = 8
            besoinsucre = (sucre - 1) * 5
            println("Boisson sélectionnée : Expresso")
          } else if (boisson == 2) {
            nomboisson = "Cappuccino"
            prixboisson = prixcappuccino
            besoincafe = 6
            besoinsucre = (sucre - 1) * 5
            besoinlait = 100
            println("Boisson sélectionnée : Cappuccino")
          } else {
            var taillelatte = 0
            do {
              println("Choisissez la taille de votre Latte : \n 1) Petit - CHF 2.70 \n 2) Moyen - CHF 3.20 \n 3) Grand - CHF 3.70)")
              taillelatte = readLine("> ").toInt
              if (taillelatte < 1 || taillelatte > 3) {
                println("Votre sélection n'est pas correcte, veuillez choisir entre : \n 1) Latte Petit \n 2) Latte Moyen \n 3) Latte Grand ")
              }
            } while (taillelatte < 1 || taillelatte > 3)

            if (taillelatte == 1) {
              nomboisson = "Latte Petit"
              prixboisson = prixlattepetit
              besoincafe = 6
              besoinsucre = (sucre - 1) * 5
              besoinlait = 120
              println("Boissons sélectionnée : Latte (Petit)")
            } else if (taillelatte == 2) {
              nomboisson = "Latte Moyen"
              prixboisson = prixlattemoyen
              besoincafe = 8
              besoinsucre = (sucre - 1) * 5
              besoinlait = 150
              println("Boisson sélectionnée : Latte (Moyen)")
            } else {
              nomboisson = "Latte Grand"
              prixboisson = prixlattegrand
              besoincafe = 12
              besoinsucre = (sucre - 1) * 5
              besoinlait = 200
              println("Boisson sélectionnée : Latte (Grand)")
            }
          }

          println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30")

          do {
            sucre = readLine("> ").toInt
            if (sucre < 1 || sucre > 4) {
              println("Votre sélection n'est pas correcte. Veuillez choisir entre : \n1) Sans sucre\n2) Peu\n3) Moyen\n4) Beaucoup ")
            }
          } while (sucre < 1 || sucre > 4)

          if (sucre == 1) {
            nomsucre = "Sans sucre"
            besoinsucre = sans_sucre
            prixsucre = 0.0
          } else if (sucre == 2) {
            nomsucre = "Peu"
            besoinsucre = peu_sucre
            prixsucre = prixdosesucre
          } else if (sucre == 3) {
            nomsucre = "Moyen"
            besoinsucre = moyen_sucre
            prixsucre = prixdosesucre * 2
          } else if (sucre == 4) {
            nomsucre = "Beaucoup"
            besoinsucre = beaucoup_sucre
            prixsucre = prixdosesucre * 3
          }

          //vérification des stocks
          if (coffeeStocks(machineId) < besoincafe) {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            retour_erreur = true
          } else if (milkStocks(machineId) < besoinlait) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            retour_erreur = true
          } else if (sugarStocks(machineId) < besoinsucre) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            retour_erreur = true
          } else {


            var nomchoixlaitsupp: String = ""
            var lait = 0
            var doselait = 0
            var prixlait = 0.0
            var laitsupplement = 0
            var prixlaitsupplement: Double = 0


            if (boisson == 2 || boisson == 3) {
              println("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui\n2) Non")
              do {
                lait = readLine("> ").toInt
              } while (lait < 1 || lait > 2)
              if (lait < 1 || lait > 2) println("Erreur. Veuillez sélectionner votre choix (1 ou 2).")


              if (lait == 1) {
                nomchoixlaitsupp = "Oui"
                println("Combien de dose ? max 3 doses, CHF 0.05 une dose de lait supplémentaire.")
                do {
                  doselait = readLine("> ").toInt
                } while (doselait < 1 || doselait > 3)

                if (doselait < 1 || doselait > 3) {
                  println("Erreur. Veuillez sélectionner une valeur entre 1 et 3.")
                }

                laitsupplement = doselait * 50
                prixlaitsupplement = doselait * 0.05

                if (doselait == 1) {
                  laitsupplement = 50
                  prixlaitsupplement = 0.05
                } else if (doselait == 2) {
                  laitsupplement = 100
                  prixlaitsupplement = 0.10
                } else if (doselait == 3) {
                  laitsupplement = 150
                  prixlaitsupplement = 0.15
                }

              } else if (lait == 2) {
                nomchoixlaitsupp = "Non"
                laitsupplement = 0
                prixlaitsupplement = 0
              }
            }
            if (sugarStocks(machineId) < besoinsucre) {
              println("Erreur : Quantité de sucre insuffisante pour le niveau de sucre sélectionné. \nVeuillez choisir un autre niveau de sucre.")
              return false
            }
            if (milkStocks(machineId) < besoinlait + laitsupplement) {
              println("Erreur : Quantité de lait insuffisante pour la dose de lait supplémentaire sélectionnée. \nVeuillez choisir un autre niveau de lait.")
              return false
            }

            var prixfinalboisson = prixboisson + prixsucre + prixlaitsupplement

            println("Boisson sélectionnée : " + nomboisson + "\nNiveau de sucre : " + nomsucre + "(" + besoinsucre + "g)")
            if (boisson == 2 || boisson == 3) {
              println("Lait en supplément : " + nomchoixlaitsupp)
            }

            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prixboisson, prixsucre, prixlaitsupplement, prixfinalboisson)

            //Payement
            val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est :" + codeTwint)
            println("\n")
            println("(En attente de paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté..")
            println("Préparation de votre boisson...")
            println("Votre " + nomboisson + " est prêt ! Bonne dégustation !")

            coffeeStocks(machineId) -= besoincafe.toInt
            sugarStocks(machineId) -= besoinsucre.toInt
            milkStocks(machineId) -= besoinlait + laitsupplement

            return true
          }
    }
          def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
            println("Stocks:")

            printf("Poudre de café : %.2f g\n",  coffeeStocks (machineId).toDouble)
            printf("Lait : %.2f L\n", milkStocks (machineId)/1000.0)
            printf("Sucre: %.2f g\n", sugarStocks (machineId).toDouble)

              val coffee = readLine("Quantité de café à ajouter : ").toInt
              val sugar = readLine("Quantité de sucre à ajouter : ").toInt
              println("Lait >")
              val laitenlitres = readLine("Quantité de lait à ajouter : ").toDouble
              milkStocks(machineId) += (laitenlitres*1000).toInt


                coffeeStocks(machineId) += coffee
                sugarStocks(machineId) += sugar


          }

    }
}




