import scala.io.StdIn._
object Main {
  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50)
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(500)

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
        println("Nospresso Café !")
        println("1) Mode Client")
        println("2) Mode Admin")
        println("3) Quitter")
        print("> ")
        var mode = readInt()

        while (!(mode == 1 || mode == 2 || mode == 3)) {
          println("Le mode sélectionné n'est pas valide, Veuillez resélectionner votre mode : ")
          println("1 ) Client")
          println("2 ) Admin")
          println("3 ) Quitter")
          print(">")
          mode = readInt()
        }
      if (mode == 1) {
        var selectionmachine = true
        while (selectionmachine) {
          println("Veuillez choisir une machine (1-5)")
          print("> ")
          var machineId = readInt()
          while (!(machineId == 1 || machineId == 2 || machineId == 3 || machineId == 4 || machineId == 5)) {
            println("Veuillez choisir une machine (1-5)")
            print("> ")
            machineId = readInt()
          }
          println("Accès accordé à la Machine " + machineId)
          if (serveClient(machineId - 1, coffeeStocks, sugarStocks, milkStocks)) {
            selectionmachine = false
          } else {
            print("> ")
          }
        }
      }

      else if (mode == 2) {
        println("Veuillez choisir une machine (1-5)")
        print("> ")
        var machineId = readInt()
        while (!(machineId==1 || machineId==2 ||machineId==3 ||machineId==4 ||machineId == 5 )) {
          println("Veuillez choisir une machine (1-5)")
          print("> ")
          machineId =  readInt()
        }
        if ((machineId==1 || machineId==2 ||machineId==3 ||machineId==4 ||machineId == 5 )&& (validatePin(machineId - 1, machinePins))) {
          var retouraumenu = true
          while (retouraumenu) {
            println("Quel option desirez vous ? ")
            println("")
            println("1) Réapprovisionner")
            println("2) Mettre à jour le code PIN")
            print("> ")
            var adminChoice = readInt()
            while (!(adminChoice==1 || adminChoice == 2)) {
              println("Veuillez selectionnez une option valide : ")
              println("")
              println("1) Réapprovisionner")
              println("2) Mettre à jour le code PIN")
              print("> ")
              adminChoice = readInt()
            }
            if (adminChoice == 1) {
              restockMachine(machineId -1, coffeeStocks, sugarStocks, milkStocks)
              retouraumenu = false
            } else if (adminChoice == 2) {
              updatePin(machineId - 1, machinePins)
              retouraumenu = false
            }
          }
        }else {
          running = false  // ici je dois stoppe boucle principale si trop tentatives dans validatepin, ok
        }

      } else if (mode == 3) {
        println("Merci de votre visite ! Au revoir ! ")
        running = false
      }
    }
  }


  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var nmbressaies = 3

    while (nmbressaies > 0) {
      println("Entrez le code PIN :")
      print("> ")
      val codePIN = readLine()

      if (codePIN == machinePins(machineId)) {
        println("Accès accordé à la Machine " + (machineId+1))
        return true
      } else {
        nmbressaies = nmbressaies - 1
        println("Code PIN incorrect "+ nmbressaies + " tentatives restantes")
      }
    }
    while (nmbressaies == 0) {
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }
    return false
  }



  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    var codePINvalid = false

    while (!codePINvalid) {
      println("Mise à jour du code PIN pour la Machine " + (machineId+1))
      println("Entrez un nouveau code PIN à 6 chiffres >")
      var nouveaucodePIN = readLine()

      if (nouveaucodePIN.length == 6 && nouveaucodePIN.forall(_.isDigit)) {
        machinePins(machineId) = nouveaucodePIN
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal ...")
        codePINvalid = true
        // retour menu principale
      } else {
        println("")
      }
    }
  }


  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var prixtotal = 0.0
    var prixboisson = 0.0
    var prixsucre = 0.0
    var prixdoselait = 0.0
    val codetwint = scala.util.Random.alphanumeric.take(5).mkString

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print(">")
    var boisson = readInt()
    while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
      println("ERREUR : La boisson sélectionnée n'est pas valide, veuillez réessayer.")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print(">")
      boisson = readInt()
    }

    if (boisson == 3) {
      println("------------------------------------------------------------------")
      println("Note : la taille du Latte désirée par l'utilisateur sera demandée après.")
      println("-------------------------------------------------------------------")
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print(">")
    var sucreoption = readInt()
    while (!(sucreoption == 1 || sucreoption == 2 || sucreoption == 3 || sucreoption == 4)) {
      println("ERREUR : Option invalide, Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print(">")
      sucreoption = readInt()
    }

    if (sucreoption == 2 && sugarStocks(machineId) >= 5) {
      prixsucre += 0.10
      prixtotal += 0.10
      sugarStocks(machineId) -= 5
    } else if (sucreoption == 3 && sugarStocks(machineId) >= 10) {
      prixsucre += 0.20
      prixtotal += 0.20
      sugarStocks(machineId) -= 10
    } else if (sucreoption == 4 && sugarStocks(machineId) >= 15) {
      prixsucre += 0.30
      prixtotal += 0.30
      sugarStocks(machineId) -= 15
    } else if (sucreoption != 1) {
      println("ERREUR : Quantité insuffisante de sucre. Veuillez choisir une autre option, machine ou vérifier les stocks en mode admin.")
      return false
    }

    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print(">")
      var laitsupplement = readInt()
      while (!(laitsupplement == 1 || laitsupplement == 2)) {
        println("ERREUR : Choix invalide, Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")
        laitsupplement = readInt()
      }

      if (laitsupplement == 1) {
        println("Combien de doses souhaitez-vous ? (3 max)")
        println("1) 1 dose -- CHF 0.05")
        println("2) 2 doses -- CHF 0.10")
        println("3) 3 doses -- CHF 0.15")
        print(">")
        var doselait = readInt()
        while (!(doselait == 1 || doselait == 2 || doselait == 3)) {
          println("ERREUR : Choix invalide, Combien de doses souhaitez-vous ?")
          println("1) 1 dose -- CHF 0.05")
          println("2) 2 doses -- CHF 0.10")
          println("3) 3 doses -- CHF 0.15")
          print(">")
          doselait = readInt()
        }

        val laitnecessaire = doselait * 50
        if (milkStocks(machineId) >= laitnecessaire) {
          prixdoselait += doselait * 0.05
          prixtotal += doselait * 0.05
          milkStocks(machineId) -= laitnecessaire
        } else {
          println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre option, machine ou vérifier les stocks en mode admin.")
          return false
        }
      }
    }

    if (boisson == 1) {
      if (coffeeStocks(machineId) >= 8) {
        coffeeStocks(machineId) -= 8
        prixboisson = 2.00
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else {
        println("ERREUR : Quantité insuffisante de café. Veuillez choisir une autre boisson, machine ou vérifier les stocks en mode admin.")
        return false
      }
    } else if (boisson == 2) {
      if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 100
        prixboisson = 2.50
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (coffeeStocks(machineId) < 6){
        println("ERREUR : Quantité insuffisante de café. Veuillez choisir une autre boisson ou machine ")
        return false
      } else if (milkStocks(machineId) < 100){
        println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre boisson ou machine ")
        return false
      }
    } else if (boisson == 3) {
      println("Veuillez sélectionner la taille :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print(">")
      var taille = readInt()
      while (!(taille == 1 || taille == 2 || taille == 3)) {
        println("ERREUR : Taille invalide, veuillez réessayer.")
        print(">")
        taille = readInt()
      }

      if (taille == 1 && coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120) {
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 120
        prixboisson = 2.70
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (taille == 2 && coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150) {
        coffeeStocks(machineId) -= 8
        milkStocks(machineId) -= 150
        prixboisson = 3.20
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if (taille == 3 && coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200) {
        coffeeStocks(machineId) -= 12
        milkStocks(machineId) -= 200
        prixboisson = 3.70
        prixtotal += prixboisson
        printf("Prix : CHF %.2f (Boisson) + CHF %.2f (Sucre) + CHF %.2f (Lait) = CHF %.2f\n", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codetwint)
        println("En attente de paiement...")
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Votre boisson est prête ! Bonne dégustation !")
      } else if ((taille == 1 && coffeeStocks(machineId) < 6) || (taille == 2 && coffeeStocks(machineId) < 8) || (taille == 3 && coffeeStocks(machineId) < 12)){
        println("ERREUR : Quantité insuffisante de café . Veuillez choisir une autre option ou machine.")
        return false
      } else if ((taille == 1 && milkStocks(machineId) < 120) || (taille == 2 && milkStocks(machineId) < 150) || (taille == 3 && milkStocks(machineId) < 200)){
        println("ERREUR : Quantité insuffisante de lait. Veuillez choisir une autre option ou machine.")
        return false
      }
    }
    return true
  }

  // ok !
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels pour la Machine " + machineId + " :")
    println("Poudre De Café : " + coffeeStocks(machineId) + "g")
    println("Sucre : " + sugarStocks(machineId) + "g" )
    printf ("Lait : %.3f litres\n",(milkStocks(machineId) / 1000.0 ))
    println("Entrez les quantités à ajouter :")

    print("Poudre de café (gr) > ")
    var poudredecafeaajouter = readInt()
    while (!(poudredecafeaajouter >= 0)) {
      println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
      print("Poudre de café (gr) > ")
      poudredecafeaajouter = readInt()
    }
    if (poudredecafeaajouter >= 0) {
      coffeeStocks(machineId) += poudredecafeaajouter}

    print("Sucre (gr) > ")
    var sucreaajouter = readInt()
    while (!(sucreaajouter >= 0)) {
      println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
      print(" Sucre (g) > ")
      sucreaajouter = readInt()
    }
    if (sucreaajouter >= 0) {
      sugarStocks(machineId) += sucreaajouter}

    print("Lait (A saisir en ml) > ")
    var laitaajouter = readInt()
    while (!(laitaajouter >= 0)) {
      println("les quantites ajouter sont incorrecte, Veuillez resaisir une valeur : ")
      print("Lait (a saisir en ml) > ")
      laitaajouter = readInt()
    }
    if (laitaajouter >= 0) {
      milkStocks(machineId) += laitaajouter}

    println("Les stocks ont été mis à jour avec succès")

    println("Nouveaux niveaux de stock :")

    println("Café : " + coffeeStocks(machineId) + "g")
    println ("Sucre : " + sugarStocks(machineId) + "g")
    printf ("Lait : %.3f litres\n",(milkStocks(machineId) / 1000.0 ))

    println("Retour au menu principal ...")
  }
}