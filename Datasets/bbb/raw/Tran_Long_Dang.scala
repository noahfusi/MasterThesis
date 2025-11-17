import io.StdIn._
import scala.util.Random


object Main {
  val nbMachines = 5
  var machinePins = Array(434343, 434343, 434343, 434343, 434343)
  var accesValide = false

  var sugarStocks = Array(15, 20, 30, 15, 50)
  var coffeeStocks = Array(50, 20, 35, 40, 25)
  var milkStocks = Array(0.5, 1.0, 1.6, 2.0, 3.0)

  var prixBoisson = 0.0
  var prixSucre = 0.0
  var prixLait = 0.0
  var prixTotal = prixBoisson + prixSucre + prixLait
  var boucleModeAdmin = true



  def validatePin(machineId: Int, machinePins: Array[Int]): Boolean = {
    var essai = 0
    var SaisiePIN = readLine("Entrez le code PIN: ").toInt
    while (essai != 3) {
      if (SaisiePIN == machinePins(machineId)) {
        println("Accès accordé à la machine " + (machineId + 1))
        boucleModeAdmin = false
        return true
      }
      else {
        essai += 1
        if (essai == 3) {
          println("Code PIN incorrect.  " + (3 - essai) + " tentatives restantes. ")
          println("Trop de tentatives échouées. Fin du programme.")


        }
        else{
          println("Accès refusé à la machine " + (machineId + 1))
          println("Code PIN incorrect.  " + (3 - essai) + " tentatives restantes. ")
          SaisiePIN = readLine(">").toInt
          accesValide = true
        }

      }
    }
    false
  }


  def updatePin(machineId: Int, machinePins: Array[Int]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId + 1))
    var boucleNewPIN = true
    while (boucleNewPIN) {
      var newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if (newPin.length != 6) {
         newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      else {
        boucleNewPIN = false
        machinePins(machineId) = newPin.toInt
        println("Le code PIN a été mis à jour avec succèes.")
        println("Retour au menu principal...")

      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
    var boisson = readLine("Veuillez sélectionner votre boisson : \n" + "1) Expresso - CHF 2.00\n" + "2) Cappuchino - CHF 2.50\n" + "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" + ">").toInt

    if (boisson == 1) {
      if (coffeeStocks(machineId) >= 8) {
        prixBoisson += 2.00
        coffeeStocks(machineId) -= 8

      }
      else {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false

      }
    }

    if (boisson == 2) {
      if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 0.1) {
        prixBoisson += 2.50
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 0.1
      }
      else {
        println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false

      }
    }

    if (boisson == 3) {
      var taille = readLine("Quelle taille désirez-vous ? :  \n" + "1) Petit \n" + "2) Moyen \n" + "3) Grand\n" + ">").toInt

      if (taille == 1) {
        if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 0.120) {
          prixBoisson += 2.70
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 0.120
        }

        else {
          println(
            "Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false

        }
      }
      else if (taille == 2) {
        if (coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 0.150) {
          prixBoisson += 3.20
          coffeeStocks(machineId) -= 8
          milkStocks(machineId) -= 0.150
        }
        else {
          println("Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }
      else if (taille == 3) {
        if (coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 0.200) {
          prixBoisson += 3.70
          coffeeStocks(machineId) -= 12
          milkStocks(machineId) -= 0.200
        }
        else {
          println("Erreur : Quantité de poudre de café ou de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }
    }
    if (boisson == 1 || boisson == 2 || boisson == 3) {
      var sucre = readLine("Souhaitez-vous ajouter du sucre ? \n" + "1) Sans sucre\n" + "2) Peu (5g) - CHF 0.10\n" + "3) Moyen (10g) - CHF 0.20\n" + "4) Beaucoup (15g) - CHF 0.30\n" + ">").toInt

      if (sucre == 2 && sugarStocks(machineId) >= 5) {
        sugarStocks(machineId) -= 5
        prixSucre += 0.10
      }
      else if (sucre == 3 && sugarStocks(machineId) >= 10) {
        sugarStocks(machineId) -= 10
        prixSucre += 0.20
      }
      else if (sucre == 4 && sugarStocks(machineId) >= 15) {
        sugarStocks(machineId) -= 15
        prixSucre += 0.30
      }

      else if (sucre ==2 || sucre ==3 || sucre==4) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }

    }

    if (boisson == 2 || boisson == 3) {

      var laitSupplement = readLine("Souhaitez-vous ajouter du lait en supplément ?\n" + "(Disponible uniquement pour Cappuccino et Latte)\n" + "1) Oui\n" + "2) Non\n" + ">").toInt

      if (laitSupplement == 1) {
        var nbdose = readLine("Combien de doses ?\n>").toInt
        while (nbdose < 1 || nbdose > 3) {
          println("Erreur : Les doses doivent être comprises entre 1 et 3")
          nbdose = readLine("Combien de doses ?\n>").toInt
        }
        if (milkStocks(machineId) >= nbdose * 0.05) {
          milkStocks(machineId) -= nbdose * 0.05
          prixLait += nbdose * 0.05
        }

        else {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          return false
        }
      }

    }


    prixTotal = prixBoisson + prixSucre + prixLait
    printf("Prix total: %.2f CHF ", prixTotal)

    val characters = ('A' to 'Z') ++ ('0' to '9')
    val length = 5
    var randomString = ""

    for (_ <- 1 to length) {
      randomString += Random.shuffle(characters).head
    }
    println("Veuillez payer en utilisant Twint." + "\n" + "Votre code de paiement est :" + randomString)
    println("(En attente de validation du paiement...)")
    println("\n" + "Merci ! Votre paiement a été accepté.")

    println("Préparation de votre boisson...")
    println("[...]")
    Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)

    if (boisson == 1) {
      println("Votre expresso est prêt ! Bonne dégustation ! ")
    }

    if (boisson == 2) {
      println("Votre cappuchino est prêt ! Bonne dégustation ! ")
    }

    if (boisson == 3) {
      println("Votre Latte est prêt ! Bonne dégustation ! ")
    }
    true
  }


    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit={
      println("\n" + "Stocks: " + "\n" + "    Poudre à café : " + coffeeStocks(machineId) + "g" + "\n" + "    Lait: " + milkStocks(machineId) + "L" + "\n" + "    Sucre : " + sugarStocks(machineId) + "g")
      var SucreAjoute = readLine("Sucre : ").toInt
      var LaitAjoute = readLine("Lait : ").toDouble
      var PoudreCafeAjoute = readLine("Poudre à café : ").toInt
      println("Réapprovisionnement des stocks... " + "\n" + "Ajout : ")
      printf("   Pourdre à café : " + PoudreCafeAjoute + "\n" + "   Lait :  %.2f\n" + "   Sucre : " + SucreAjoute, LaitAjoute)
      println("\nNiveaux de stocks mis à jour. ")
      sugarStocks(machineId) += SucreAjoute
      milkStocks(machineId) += LaitAjoute
      coffeeStocks(machineId) += PoudreCafeAjoute
    }






  def main(args: Array[String]): Unit = {


      while (!accesValide) {
        var machineIdInput = readLine("Machine sélectionnée(1-5): > ").toInt
        if (machineIdInput > 5 || machineIdInput < 0) {
          println("Erreur: Veuillez sélectionner une machine valable")

        }
        else {
          val machineId = machineIdInput - 1
          val choix = readLine("        Nospresso Café\n" + "Veuillez sélectionner votre mode :\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter\n" + ">").toInt
          if (choix == 1) {
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          }


          else if (choix == 2) {


            if (validatePin(machineId, machinePins)) {
              var choixAdmin = readLine("Que souhaitez-vous faire? " + "\n" + "1. Modifier les stocks" + "\n" + "2. Changer le code PIN de la machine" + "\n" + ">" ).toInt

              if (choixAdmin == 1) {
                restockMachine(machineId,coffeeStocks, sugarStocks, milkStocks)


              }

              else if (choixAdmin == 2) {
                updatePin(machineId, machinePins)

              }


            }
          }
          else if (choix == 3) {
            println("Vous quittez le programme...")
            accesValide = true
          }


        }
      }


    }
  }


