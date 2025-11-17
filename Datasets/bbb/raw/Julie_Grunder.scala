import io.StdIn._
import math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val coffeeStocks = Array(50, 50, 50, 50, 50) //en grammes
    val sugarStocks = Array(30,30,30,30, 30) //en grammes
    val milkStocks = Array(500, 500, 500, 500, 500) //en millilitres
    val machinePins = Array("434343", "434343", "434343", "434343", "434343")

    //Choix du mode  et de la machine :
    var mode = 0
    var structureProgramme = true
    while (structureProgramme) { // sturctureProgramnme commence vraie donc à la fin des modes 1 et 2, ça recommence, mais comme elle devient faux pour le mode 3, ça quitte cette boucle et il se passe rien après donc ça quitte
      do {
        mode = readLine("\tNospresso Café\nVeuillez sélectionnez votre mode : \n1) Client\n2) Admin\n3) Quitter\n> ").toInt
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (mode != 1 && mode != 2 && mode != 3)
      if (mode == 1 || mode == 2) {
        //choix machine :
        var machineId = readLine("Machine sélectionnée (1-5) > ").toInt -1
        do {
          if (machineId != 0 && machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4) {
            println("Entrée invalide. Veuillez svp réessayer avec un numéro de machine valide.")
          }
        } while (machineId != 0 && machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4)
        if (mode == 1) {
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } else if (mode ==2) {
          if (validatePin(machineId, machinePins)) {
            modeAdmin(machineId, coffeeStocks, sugarStocks, milkStocks, machinePins)
          } else {
            println("Trop de tentatives échouées. Fin du programme.")
            structureProgramme = false
          }
        }
      } else {
        structureProgramme = false
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val tentativesMax = 3
    var tentativesRestantes = tentativesMax
    println("Mode Admin")
    while (tentativesRestantes > 0) {
      var pinUtilisateur = readLine("Entrez le code PIN : \n> ")
      if (pinUtilisateur == machinePins(machineId)) {
        println("Accès accordé.")
        return true
      } else {
        tentativesRestantes -= 1
        println("Code PIN incorrect. " + tentativesRestantes + " tentative(s) restante(s).")
      }
    }
    false
  }

  def modeAdmin (machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int], machinePins: Array[String]): Unit = {
    var choixActionAdmin = 0
    do {
      choixActionAdmin = readLine("\nVeuillez svp choisir une action\n1) Gérer les stocks\n2) Modier le code PIN\n> ").toInt
      if (choixActionAdmin != 1 && choixActionAdmin != 2) {
        println("Veuillez svp entrer une valeur valable.")
      }
    } while (choixActionAdmin != 1 && choixActionAdmin != 2)
    if (choixActionAdmin == 1) {
      restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
    } else {
      updatePin(machineId, machinePins)
    }
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("\nMise à jour du code PIN pour la Machine " + machineId + ".")
    var nouveauPIN = " "
    var nbChiffrePIN = true
    do {
      nouveauPIN = readLine("Entrez un nouveau code PIN à 6 chiffres > ").toString
      if (nouveauPIN.length != 6) {
      nbChiffrePIN = false
      }
    } while (nouveauPIN.length != 6)
    machinePins(machineId) = nouveauPIN
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println() //ligne d'espace
    printf("Niveaux de stock actuels :\n" +
      "\tPoudre de café: " + coffeeStocks(machineId) + " g\n" +
      "\tSucre :" + sugarStocks(machineId) + "g\n" +
      "\tLait : %.2f L", milkStocks(machineId) / 1000.0)
    var stockCafeAjoute = 0
    var stockSucreAjoute = 0
    var stockLaitAjoute = 0
    do {
      println() //ligne d'espace
      println("\nEntrez les quantités à ajouter :")
      stockCafeAjoute = readLine("\tPoudre de café (en g) > ").toInt
      stockSucreAjoute = readLine("\tSucre (en g) > ").toInt
      println("\tLait (en L) > ")
      stockLaitAjoute = (readDouble() * 1000).toInt //conversion en ml

      if (stockCafeAjoute < 0 || stockCafeAjoute < 0 || stockLaitAjoute < 0) {
        println("Les quantités doivent être positives.")
      }
    } while (stockCafeAjoute < 0 || stockSucreAjoute < 0 || stockLaitAjoute < 0)
    print("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var commande = true
    while (commande) {
      val prixExpresso = 2.00 //en CHF
      val prixCappuccino = 2.50
      val prixLattePetit = 2.70
      val prixLatteMoyen = 3.20
      val prixLatteGrand = 3.70
      val prixSucrePeu = 0.10
      val prixSucreMoyen = 0.20
      val prixSucreBeaucoup = 0.30
      val prixDoseLait = 0.05
      var prixTotal = 0.0
      // val stockCafeInitial = 50 //en g -> pas besoin car dans tableau
      val doseCafeExpresso = 8
      val doseCafeCappucino = 6
      val doseCafeLattePetit = 6
      val doseCafeLatteMoyen = 8
      val doseCafeLatteGrand = 12
      var stockCafeActuel = coffeeStocks
      var stockCafeNecessaire = 0
      var stockCafeAjoute = 0
      // val stockLaitInitial = 500 //en millilitres -> pas besoin car dans tableau
      val doseLaitCappucino = 100
      val doseLaitLattePetit = 120
      val doseLaitLatteMoyen = 150
      val doseLaitLatteGrand = 200
      val doseLaitSupp = 50
      var stockLaitActuel = sugarStocks
      var stockLaitNecessaire = 0
      var stockLaitAjoute = 0
      // val stockSucreInitial = 30 //en g -> pas besoin car dans tableau
      val doseSucrePeu = 5
      val doseSucreMoyen = 10
      val doseSucreBeaucoup = 15
      var stockSucreActuel = milkStocks
      var stockSucreNecessaire = 0
      var stockSucreAjoute = 0
      var stockTotalNecessaire = true //par défaut le stockTotalNecessaire commence vrai, car le programme commence avec assez de stock pour effectuer toutes les possibilités de café
      var boissonNom = "boisson"
      var niveauSucre = "Sans sucre"
      var prixBoisson = 0.0
      var prixSucre = 0.0
      var prixSuppLait = 0.0
      // choix de la boisson :
      var boisson = 0
      do {
        boisson = readLine("Veuillez sélectionner votre boisson :" +
          "\n1) Expresso - CHF 2.00" +
          "\n2) Cappuccino - CHF 2.50" +
          "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
        if (boisson != 1 && boisson != 2 && boisson != 3) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (boisson != 1 && boisson != 2 && boisson != 3)
      //adaptation prix total :
      if (boisson == 1) {
        prixTotal = prixExpresso
        stockCafeNecessaire = doseCafeExpresso
        boissonNom = "Expresso"
        prixBoisson = prixExpresso
      }
      else if (boisson == 2) {
        prixTotal = prixCappuccino
        stockCafeNecessaire = doseCafeCappucino
        stockLaitNecessaire = doseLaitCappucino
        boissonNom = "Cappuccino"
        prixBoisson = prixCappuccino
      }
      else {
        var tailleLatte = 0
        boissonNom = "Latte"
        do {
          tailleLatte = readLine("Veuillez précisez la taille de votre latte : \n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Veuillez svp entrer une valeur valide.")
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
        if (tailleLatte == 1) {
          prixTotal = prixLattePetit
          stockCafeNecessaire = doseCafeLattePetit
          stockLaitNecessaire = doseLaitLattePetit
          prixBoisson = prixLattePetit
        } else if (tailleLatte == 2) {
          prixTotal = prixLatteMoyen
          stockCafeNecessaire = doseCafeLatteMoyen
          stockLaitNecessaire = doseLaitLatteMoyen
          prixBoisson = prixLatteMoyen
        } else {
          prixTotal = prixLatteGrand
          stockCafeNecessaire = doseCafeLatteGrand
          stockLaitNecessaire = doseLaitLatteGrand
          prixBoisson = prixLatteGrand
        }
      }
      //personnalisation :
      //sucre
      var sucre = 0
      do {
        sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez svp entrer une valeur valide.")
        }
      } while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4)
      //adaptation prix et des stocks :
      if (sucre == 1) {
        stockSucreNecessaire = 0
        niveauSucre = "Sans sucre"
        prixSucre = 0.0
      } else if (sucre == 2) {
        prixTotal += prixSucrePeu
        stockSucreNecessaire = doseSucrePeu
        niveauSucre = "Peu (5g)"
        prixSucre = prixSucrePeu
      } else if (sucre == 3) {
        prixTotal += prixSucreMoyen
        stockSucreNecessaire = doseSucreMoyen
        niveauSucre = "Moyen (10g)"
        prixSucre = prixSucreMoyen
      } else { // (sucre ==4)
        prixTotal += prixSucreBeaucoup
        stockSucreNecessaire = doseSucreBeaucoup
        niveauSucre = "Beaucoup (15g)"
        prixSucre = prixSucreBeaucoup
      }
      //lait :
      var lait = 0
      var nbDoseLait = 0
      if (boisson == 2 || boisson == 3) {
        do {
          lait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          if (lait != 1 && lait != 2) {
            println("Veuillez svp entrer une valeur valide.")
          }
        } while (lait != 1 && lait != 2)
        if (lait == 1) {
          do {
            nbDoseLait = readLine("Combien de dose ?\n> ").toInt
            if (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3) {
              if (nbDoseLait == 0) {
                println("Veuillez entrez une valeur svp.")
              } else if (nbDoseLait < 0) {
                println("Le nombre de dose de lait ne peut pas être négatif.")
              } else { // (nbDoseLait > 3)
                println("Il n'est pas possible d'ajouter plus de 3 doses de lait en supplément.")
              }
            }
          } while (nbDoseLait != 1 && nbDoseLait != 2 && nbDoseLait != 3)
          prixTotal += (nbDoseLait * prixDoseLait)
          stockLaitNecessaire += (nbDoseLait * doseLaitSupp)
          prixSuppLait = nbDoseLait * prixDoseLait
        }
      }
      //Gestion des stocks :
      if (coffeeStocks(machineId) < stockCafeNecessaire) {
        println("Stock de café insuffisant.")
        return false
      } else if (sugarStocks(machineId) < stockSucreNecessaire) {
        println("Stock de sucre insuffisant.")
        return false
      } else if (milkStocks(machineId) < stockLaitNecessaire) {
        println("Stock de lait insuffisant.")
        return false
      } else {
        // Mise à jour des stocks
        coffeeStocks(machineId) -= stockCafeNecessaire
        sugarStocks(machineId) -= stockSucreNecessaire
        milkStocks(machineId) -= stockLaitNecessaire
        // résumé commande et affichange prix
        print("Boisson sélectionnée : " + boissonNom + "\nNiveau de sucre : " + niveauSucre + "\nLait supplémentaire : ")
        if (nbDoseLait == 0) {
          print("Non\n")
        } else {
          print(nbDoseLait + " dose(s)\n")
        }
        //+ nbDoseLait + " dose(s)")
        printf("Prix total : CHF %.2f", prixBoisson)
        if (sucre != 1) {
          printf(" + CHF %.2f", prixSucre)
        }
        if (lait == 1) {
          printf(" + CHF %.2f", prixSuppLait)
        }
        printf(" = CHF %.2f", prixTotal)
        println() //ligne vide// résumé commande et affichange prix
        print("Boisson sélectionnée : " + boissonNom + "\nNiveau de sucre : " + niveauSucre + "\nLait supplémentaire : ")
        if (nbDoseLait == 0) {
          print("Non\n")
        } else {
          print(nbDoseLait + " dose(s)\n")
        }
        //+ nbDoseLait + " dose(s)")
        printf("Prix total : CHF %.2f", prixBoisson)
        if (sucre != 1) {
          printf(" + CHF %.2f", prixSucre)
        }
        if (lait == 1) {
          printf(" + CHF %.2f", prixSuppLait)
        }
        printf(" = CHF %.2f", prixTotal)
        println() //ligne vide
        //payement
        val codeTwint = Random.alphanumeric.take(5).mkString
        println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :" + codeTwint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println() //ligne vide
        println("Merci ! Votre paiement a été accepté.")
        //Préparation de la boisson
        println("Préparation de votre boisson...\n[...]\nVotre " + boissonNom + " est prêt ! Bonne dégustation !\n")
        return true
      }
    }
    false
  }
}