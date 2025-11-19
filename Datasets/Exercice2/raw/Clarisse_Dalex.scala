import io.StdIn._
import math._
import scala.util.Random
import io.StdIn.readLine
import scala.reflect.internal.util.TriState.{False, True}



object Main {

  //MÉTHODE
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {

    //Faire en sorte que ce soit 434343 puis si ca doit chqnger ce sera l'updatePin qui sera le nouveau PIN.
    var maxessaie = 3
    for (i <- 1 to maxessaie) {
      println("\nEntrez le code PIN:")
      print(">")
      var codePIN = readLine()
      if (codePIN == machinePins(machineId)) {
        println("\nAccès accordé à la machine " + (machineId + 1))
        return true
        //Code Pin correct
      } else {
        val essaierestant = maxessaie - i
        if (essaierestant > 0) {
          println("Code PIN incorrect. " + essaierestant + " tentatives restantes.")
        } else {
          println("Trop de tentatives échouées. Fin du programme. ")
          return false
        }
      }
    }
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    var valid = false
    while (!valid) {
      print("\n\nEntrez un nouveau code PIN à 6 chiffres >")
      newPin = readLine()

      //Vérifiions si les conditions sont réspectées soit: longueur 6 chiffres et Que des chiffres

      if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
        valid = true //Pin Valide
      } else if (newPin.isEmpty) {
        println("Erreur : Le code PIN ne peux pas être vide.")
        print(">")
        newPin = readLine()
        if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
          valid = true //Pin Valide
        }
      } else {
        println("Erreur : Le code PIN doit contenir 6 CHIFFRES ")
        print(">")
        newPin = readLine()
        if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
          valid = true //Pin Valide
        }
      }

      //Mise à jour du tableau avec le nouveau PIN


    }
    machinePins(machineId) = newPin

    println("Le nouveau code PIN a été mis a jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(5000)

  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    val expresso: Double = 2.00
    val Cappuccino: Double = 2.50
    val lattepetit: Double = 2.70
    val lattemoyen: Double = 3.20
    val lattegrand: Double = 3.70

    //tableau des stocks initiaux


    //variable different sucre prix
    var sanssucre: Double = 0.0
    var peusucre: Double = 0.10
    var moyensucre: Double = 0.20
    var beaucoupsucre: Double = 0.30

    //variable grammage sucre

    var sisucrepeu = 5
    var sisucremoyen = 10
    var sisucrebeaucoup = 15


    //Consommation des ingrédients par type de boisson: en gramme
    var consoexpresso = 8
    var consocappuccinocafe = 6
    var consocappuccinolait = 100 //en millilitre
    var consolattepetitcafe = 6
    var consolattepetitlait = 100 // en millilitre
    var consolattemoyencafe = 8
    var consolattemoyenlait = 150 //en millilitre
    var consolattegrandcafe = 12
    var consolattegrandlait = 200 // en millilitre
    var sucre = 0
    var encore = True

    //Pour avoir présentation à la fin correcte
    var boissonnom = ""
    var sucrenom = ""
    var laitnom = ""

    println("\n\nVeuillez sélectionner votre boisson:")
    println("1) Expresso - " + expresso + " CHF")
    println("2) Cappuccino -" + Cappuccino + "CHF")
    println("3) Latte - " + " Latte petit " + lattepetit + " CHF ")
    println("4) Latte - " + " Latte Moyen " + lattemoyen + " CHF ")
    println("5) Latte -" + " Latte Grand " + lattegrand + " CHF ")
    print(">")

    //variable choix boisson
    var boisson = readInt()
    var prixtotal = 0.0
    var prixboisson = 0.0
    var prixsucre = 0.0
    var laitconso = 0
    var sucreconso = 0
    var cafeconso = 0
    var prixdoselait = 0.0

    while (boisson != 1 && boisson != 2 && boisson != 3 && boisson != 4 && boisson != 5) {
      println("Boisson inexistante, veuillez réessayer: ")
      boisson = readInt()

    }

    if (boisson == 1) {
      boissonnom = "Expresso"
      println("\n\nBoisson séléctionnée: " + boissonnom)
      prixboisson = expresso
      cafeconso += consoexpresso

    }
    else if (boisson == 2) {
      boissonnom = "Cappuccino"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = Cappuccino
      cafeconso += consocappuccinocafe
      laitconso += consocappuccinolait

    }
    else if (boisson == 3) {
      boissonnom = "Latte petit"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattepetit
      cafeconso += consolattepetitcafe
      laitconso += consolattepetitlait

    }
    else if (boisson == 4) {
      boissonnom = "Latte moyen"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattemoyen
      cafeconso += consolattemoyencafe
      laitconso += consolattemoyenlait

    }
    else if (boisson == 5) {
      boissonnom = "Latte grand"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattegrand
      cafeconso += consolattegrandcafe
      laitconso += consolattegrandlait

    } else {
      println("Boisson n'existe pas, réessayer : ")
      boisson = readInt()
    }


    //pour rajouter du sucre
    if (boisson >= 1 && boisson <= 5) {
      println("\n\nSouhaitez-vous ajouter du sucre ?")
      println("1)Sans sucre")
      println("2)Peu (5g) -" + peusucre + "CHF")
      println("3)Moyen (10g) -" + moyensucre + "CHF")
      println("4)Beaucoup (15g)-" + beaucoupsucre + "CHF")
      print(">")
      var sucre = readInt()

      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        println("Veuillez choisir entre 1 à 4 ")
        sucre = readInt()

      }

      if (sucre == 1) {
        sucrenom = "Sans sucre"
        println("Niveau de sucre : " + sucrenom)
        prixsucre = sanssucre
      } else if (sucre == 2) {
        sucrenom = "Peusucre (5g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucrepeu
        prixsucre = peusucre
      } else if (sucre == 3) {
        sucrenom = "Moyen sucre (10g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucremoyen
        prixsucre = moyensucre


      } else if (sucre == 4) {
        sucrenom = "Beaucoup sucre (15g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucrebeaucoup
        prixsucre = beaucoupsucre

      }


      var choixlaitsupp = 0
      var dose = 4
      //rajouter du lait ou non dans cappucino et latte
      if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5) {
        println("\n\nSouhaitez-vous ajouter du lait en supplément ?")

        println("1) Oui")
        println("2) Non")
        print(">")
        choixlaitsupp = readInt()

        while (choixlaitsupp != 1 && choixlaitsupp != 2) {
          println("Merci d'entrer un choix valide : ")
          print(">")
          choixlaitsupp = readInt()
        }
        if (choixlaitsupp == 1) {
          laitnom = "Oui"
          println("Lait supplémentaire: " + laitnom)

          while (dose > 3) {
            println("Combien de dose ? (Maximum 3 doses)")
            dose = readInt()
            if (dose == 1 || dose == 2 || dose == 3) {
              println("Le nombre de dose choisi est : " + dose)
              laitconso += dose * 50 //COMMENT FAIRE
              prixdoselait = dose * 0.05 //COMMENT FAIRE
            }
          }
        }
        else {
          laitnom = "Non"
          println("\n\nLait supplémentaire : " + laitnom)
        }

      }


      //Résumé de commande
      println("\n\nBoisson Séléctionnée : " + boissonnom)
      println("Niveau de sucre : " + sucrenom)

      if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5) {
        println("Lait en supplément : " + laitnom)
      }
      //faisons les stocks

      if ((sugarStocks(machineId) < sucreconso) || (milkStocks(machineId) < laitconso) || (coffeeStocks(machineId) < cafeconso)) {
        if (sugarStocks(machineId) < sucreconso) {
          println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre machine.")



        }
        if (milkStocks(machineId) < laitconso) {
          println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre machine.")


        }
        if (coffeeStocks(machineId) < cafeconso) {
          println("Quantité de cafe insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou essayer une autre machine.")


        }
        println("Séléctionner une autre machine :")
        var nouveauchoixmachine = readInt()
        var machineIdnouveau = (nouveauchoixmachine - 1)
        serveClient(machineIdnouveau, coffeeStocks, sugarStocks, milkStocks)


      } else {
        sugarStocks(machineId) = sugarStocks(machineId) - sucreconso
        milkStocks(machineId) = milkStocks(machineId) - laitconso
        coffeeStocks(machineId) = coffeeStocks(machineId) - cafeconso

        //Paiement

        prixtotal = prixboisson + prixsucre + prixdoselait
        //println("Prix Total :  CHF " + prixboisson+ " + CHF " + prixsucre + "= CHF " + prixtotal)
        printf("\n\nPrix Total :  CHF  %.2f + CHF %.2f + CHF %.2f  = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("\nVeuillez payer en utilisant Twint.")
        val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        var code = ""
        for (i <- 1 to 5) {
          val randomChar = chars(Random.nextInt(chars.length))
          code += randomChar
        }

        println("Votre code de paiement est : " + code)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("\n\nMerci ! Votre paiement a été accepté")

        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre " + boissonnom + " est prêt ! Bonne dégustation !")
        //faisons les stocks
        Thread.sleep(5000)
      }
    }
    return false //PAS SURE


  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {



    //montrer les stocks avant réapprovisionnement
    println("\n\nNiveaux de stocks actuels : ")
    println("Poudre de café: " + coffeeStocks(machineId) + "g")
    // val laitenlitres= milkStocks(machineId)/1000             //de mL en L
    println("Lait          : " + (milkStocks(machineId) / 1000.0) + "L") //Pas sur
    println("Sucre         :" + sugarStocks(machineId) + "g")

    println("\n\nEntrez les quantité à ajouter : ")

    var acafe = -1
    var alait = -1
    var asucre = -1

    while (acafe < 0 || alait < 0 || asucre < 0) {

      println(f"Poudre de café: ")
      acafe = readInt
      println(f"Lait          : ")
      alait = (readDouble() * 1000).toInt //Voir comment faire pour lait
      println(f"Sucre          : ")
      asucre = readInt

      if (acafe < 0 || alait < 0 || asucre < 0) {
        println("Veuillez mettre des valeurs positives.")
      }


    }

    println("Les stocks ont été mis à jour avec succès...")
    coffeeStocks(machineId) += acafe
    milkStocks(machineId) += alait
    sugarStocks(machineId) += asucre
    println("Retour au menu principal...")
    Thread.sleep(5000)

  }


  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    //TABLEAUX STOCKS
    var coffeeStocks = Array(50, 50, 50, 50, 50) //en grammes
    var sugarStocks = Array(30, 30, 30, 30, 30) //en grammes
    var milkStocks = Array(500, 500, 500, 500, 500) //en millilitres (mL)
    var machinesPins = Array.fill(nbMachines)("434343")
    var choixmachine = 1
    var machineId = choixmachine - 1
    var nouveauchoixmachine=1
    var nouveaumachineId = (nouveauchoixmachine - 1)


    //prix boisson
    val expresso: Double = 2.00
    val Cappuccino: Double = 2.50
    val lattepetit: Double = 2.70
    val lattemoyen: Double = 3.20
    val lattegrand: Double = 3.70


    var running = true
    while (running == true) {
      println("\n\nNospresso Café")
      println()
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">")

      val choix = readInt()

      if (choix == 1) {
        println("Mode Client")

        if (choixmachine == 1 || nouveauchoixmachine==1) {
          println("\nSélectionner la machine: (1 à 5)")
          print(">")
          choixmachine = readInt()
          machineId = choixmachine - 1
        }


        while (machineId < 0 || machineId >= nbMachines) {
          println("Erreur : Numéro de machine invalide.")
          println ("\n\nVeuillez rentrer un numéro de machine valide :")
          print(">")
          choixmachine = readInt()
          machineId = choixmachine - 1

        }

        if (machineId > 0 || machineId <= nbMachines) {
          println("\n\nVous avez sélectionné la machine : " + choixmachine)
        }


        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)



      }
      if (choix == 2) {

        println("Mode Admin")
        println("\nSélectionner la machine: (1 à 5)")
        print(">")
        choixmachine = readInt()
        machineId = choixmachine - 1

        while (machineId < 0 || machineId >= nbMachines) {
          println("Erreur : Numéro de machine invalide.")
          println ("\n\nVeuillez rentrer un numéro de machine valide :")
          print(">")
          choixmachine = readInt()
          machineId = choixmachine - 1

        }

        if (machineId > 0 || machineId <= nbMachines) {
          println("\n\nVous avez sélectionné la machine : " + choixmachine)
        }



        if (validatePin(machineId, machinesPins)) {


          println("\n\nQue Souhaitez-vous faire ? \n\n 1) Réapprovisonner les stocks \n\n 2) Faire une mise à jour du code PIN")
          print(">")
          var choixadmin = readInt()

          if (choixadmin == 1) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)


          } else if (choixadmin == 2) {
            println("Mise à jour du code PIN pour la Machine " + choixmachine)
            updatePin(machineId, machinesPins)



          } else {
            println("Choix invalide")
          }

        }
        else
          return


      }
      if (choix == 3) {
        running = false
      }
      println("Vous avez quitté, Merci d'avoir utilisé Nospresso")


    }


  }


}




