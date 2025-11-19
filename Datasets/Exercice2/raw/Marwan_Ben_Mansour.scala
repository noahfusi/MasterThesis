import scala.io.StdIn.readLine
import scala.util.Random


object Nospresso2 {

  // Define initial variables
  private val nbMachines: Int = 5
  private val chars: String = "ABCDEFGHIJKLMNOPQRATUVWXYZ0123456789"
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50) // in grams
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30) // in grams
  val milkStocks: Array[Int] = Array.fill(nbMachines)(500) // in milliliters
  var machinePins: Array[String] = Array.fill(nbMachines)("434343") // Pin initialized at 434343


  // Check if the pin is valid or not
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var machinetryPins = 0
    val machinemaxPins = 3
    while (machinetryPins < machinemaxPins) {
      val machinecustomerPins = readLine(s"Entrez le code PIN pour la machine ${machineId + 1} (tentative ${machinetryPins + 1}/$machinemaxPins): \n> ")

      if (machinecustomerPins == machinePins(machineId)) {
        println(s"Code PIN correct. \nAccès accordé à la machine ${machineId + 1}. \n")
        return true
      } else {
        println("Code PIN incorrect.")
        machinetryPins += 1
      }
    }
    println("Nombre de tentatives dépassées. Le programme se termine.")
    sys.exit(0)
  }


  // Update the pin of one machine
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    var newPin = ""

    do {
      newPin = readLine(s"Entrez le nouveau code PIN pour la machine ${machineId + 1} (6 Chiffres) : \n>")
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machinePins(machineId) = newPin
        println(s"Le code PIN de la machine ${machineId + 1} a été mis à jour avec succès. \n Retour au menu principal... \n")
      } else {
        println("Le code PIN doit comportement exactement 6 chiffres.")
      }
    } while (!(newPin.length == 6 && newPin.forall(_.isDigit)))
  }

//Client Mode in the method serveClient
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var ordervalid = false
    while (!ordervalid) {

      var boisson = 0
      var taillelatte = 0
      var drinkprice = 0.0
      var coffeepowdermass = 0
      var addsugar = 0
      var prixsugar = 0.0
      var sugarmass = 0
      var addmilkyn = 0
      var addmilk = 0
      var milkmass = 0
      var prixmilk = 0.0
      var prixtotal = 0.0


      do {
        boisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Capuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n>").toInt
        if (boisson == 1) {
          drinkprice = 2.00
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
          drinkprice = 2.50
          coffeepowdermass = 6
          milkmass = 100
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
                milkmass = milkmass + 50
                prixmilk = 0.05
              }
              else if (addmilk == 2) {
                milkmass = milkmass + 100
                prixmilk = 0.10
              }
              else if (addmilk == 3) {
                milkmass = milkmass + 150
                prixmilk = 0.15
              }
            } while ((addmilk != 1) && (addmilk != 2) && (addmilk != 3))
          }


        } else if (boisson == 3) {

          do {
            taillelatte = readLine("Quelle taille de Latte désirez-vous ? \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n>").toInt
            if (taillelatte == 1) {
              drinkprice = 2.70
              coffeepowdermass = 6
              milkmass = 120
            }
            else if (taillelatte == 2) {
              drinkprice = 3.20
              coffeepowdermass = 8
              milkmass = 150
            }
            else if (taillelatte == 3) {
              drinkprice = 3.70
              coffeepowdermass = 12
              milkmass = 200
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
                milkmass = milkmass + 50
                prixmilk = 0.05
              }
              else if (addmilk == 2) {
                milkmass = milkmass + 100
                prixmilk = 0.10
              }
              else if (addmilk == 3) {
                milkmass = milkmass + 150
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


      if (coffeeStocks(machineId) < coffeepowdermass) {
        ordervalid = true
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre machine ou vérifier les stocks en mode Admin. \n")
      }

      else if (milkStocks(machineId) < milkmass) {
        ordervalid = true
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre machine ou vérifier les stocks en mode Admin. \n")
      }

      else if (sugarStocks(machineId) < sugarmass) {
        ordervalid = true
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une autre machine ou vérifier les stocks en mode Admin. \n")
      }

      else {
        ordervalid = true
        print(f"Prix total : CHF $drinkprice%.2f")
        if (addsugar != 1) {
          print(f" + CHF $prixsugar%.2f")
        }
        if (addmilkyn == 1) {
          print(f" + CHF $prixmilk%.2f")
        }
        prixtotal = drinkprice + prixsugar + prixmilk
        println(f" = CHF $prixtotal%.2f")

        coffeeStocks(machineId) = coffeeStocks(machineId) - coffeepowdermass
        milkStocks(machineId) = milkStocks(machineId) - milkmass
        sugarStocks(machineId) = sugarStocks(machineId) - sugarmass

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
    true
  }


  // Restock in restockMachine method
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    var restockcoffepowder = 0
    var restockmilkL = 0.0
    var restockmilkmL = 0
    var milkStocksL = 0.0
    var restocksugar = 0

    milkStocksL = milkStocks(machineId).toDouble
    milkStocksL = milkStocksL/1000
    println("Stocks : \n   Poudre de café : " + coffeeStocks(machineId) + "g\n   Lait : " + milkStocksL + "L\n   Sucre : " + sugarStocks(machineId) + "g \n")

    restockcoffepowder = readLine("Quelle masse de poudre de café voulez-vous ajouter au stock ? (grammes) \n> ").toInt
    coffeeStocks(machineId) = coffeeStocks(machineId) + restockcoffepowder

    restocksugar = readLine("Quelle masse de sucre voulez-vous ajouter au stock ? (grammes) \n> ").toInt
    sugarStocks(machineId) = sugarStocks(machineId) + restocksugar

    restockmilkL = readLine("Quel volume de lait voulez-vous ajouter au stock ? (litres) \n> ").toDouble
    restockmilkmL = (restockmilkL * 1000).toInt
    milkStocks(machineId) = milkStocks(machineId) + restockmilkmL

    println(s"Réapprovissionnement des stocks...\nAjout : \n   Poudre de café : " + restockcoffepowder + "g\n   Lait : " + restockmilkL + "L\n   Sucre : " + restocksugar + s"g \nNiveaux de stocks de la machine ${machineId + 1} mis à jour. \nRetour au menu principal...\n")
  }
}



object Main {
  def main(args: Array[String]): Unit = {
    import Nospresso2._
    var machineId = 0
    var mode = 0
    var mode2choice = 0


    do {
      machineId = readLine("Veuillez sélectionner votre machine. (1-5) \n> ").toInt - 1
      if ((machineId < 0) || (machineId > 4)) {
        println("Entrée incorrecte. Veuillez saisir une valeur valide.")
      } else {
        do {
          mode = readLine("    Nespresso Café \nVeuillez selectionner votre mode: \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
          if (mode == 1) {
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          }

          else if (mode == 2) {
            validatePin(machineId, machinePins)
            do {
              mode2choice = readLine(s"Voulez vous changer le PIN de la machine ${machineId + 1} ou recharger les stocks ? \n 1) PIN \n 2) Stock \n> ").toInt
              if (mode2choice == 1)
                updatePin(machineId, machinePins)
              else if (mode2choice == 2) {
                restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
              }
            } while ((mode2choice != 1) && (mode2choice != 2))

          }
          else if (mode == 3) {
            sys.exit(0)
          }
        } while ((mode < 1) || (mode > 3))
      }
    } while ((machineId != 0) || (machineId != 1) || (machineId != 2) || (machineId != 3) || (machineId != 4))
  }
}