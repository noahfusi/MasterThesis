import io.StdIn._
import scala.util.Random
object Main {
  // LES METHODES
  //pins
  val machinePins = Array.fill(5)("434343")
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 0
    var codePinClient = " "
    codePinClient = readLine("Entrez le code PIN : \n >")
    while (tentatives < 2 && codePinClient != machinePins(machineId)) {
      tentatives += 1
      codePinClient = readLine("Code PIN incorrect. " + (3 - tentatives) + " tentatives restantes. \n>")
    }
    if (codePinClient == machinePins(machineId)) {
      println("Accès accordé.")
      return true
    } else {
      println("\nTrop de tentatives échouées. Fin du programme. ")
      return false
    }
  }

  // update pins
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var CodePINModifie = " "
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
    CodePINModifie =  readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
    var codeincorrect = (CodePINModifie.length != 6 || !CodePINModifie.forall(_.isDigit))
    while (codeincorrect == true){
      CodePINModifie =  readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
      if (CodePINModifie.length == 6 && CodePINModifie.forall(_.isDigit)) {
        //println("Code PIN actuel " + codeadmin.mkString)
        codeincorrect = false

      }
    }
    println("\nLe code PIN a été mis à jour avec succès. \nRetour au menu principal...\n")
    machinePins(machineId) = CodePINModifie
    // print(machinePins.mkString(" "))
  }

  //gestion stock
  var StockSucre = Array.fill(5)(30)
  var StockCafe = Array.fill(5)(50)
  var StockLait = Array.fill(5)(500)
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var ajout = readLine("\nSouhaitez-vous réapprovisionner : \n1) Sucre \n2) Poudre de café \n3) Lait \n>").toInt
    while(ajout > 3 && ajout < 1){
      ajout = readLine("Vous ne pouvez qu'ajouter : \n1) Sucre \n2) Poudre de café \n3) Lait \n>").toInt
    }
    println("\nRéapprovisionnement des stocks ... \nAjout : \n")
    if (ajout == 2) {
      var ajoutcafe = readLine("Quelle quantité de café souhaitez-vous rajouter ?").toInt
      while (ajoutcafe < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutcafe = readInt()
      }
      coffeeStocks(machineId) = coffeeStocks(machineId) + ajoutcafe
      println("\t Poudre de café : " + coffeeStocks(machineId) + "g")
      println("\t Lait : " + milkStocks(machineId) * 0.001 + "L")
      println("\t Sucre : " + sugarStocks(machineId) + "g")
    } ; else if (ajout == 3) {
      var ajoutlait = readLine("Quelle quantité de lait souhaitez-vous rajouter ?").toInt
      while (ajoutlait < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutlait = readInt()
      } ;
      milkStocks(machineId) = milkStocks(machineId) + ajoutlait
      println("\t Lait : " + milkStocks(machineId)*0.001 + "L")
      println("\t Café : " + coffeeStocks(machineId) + "g")
      println("\t Sucre : " + sugarStocks(machineId) + "g")
    }
    else {
      var ajoutsucre = readLine("Quelle quantité de sucre souhaitez-vous rajouter ?").toInt
      while (ajoutsucre < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutsucre = readInt()
      } ;
      sugarStocks(machineId) = sugarStocks(machineId) + ajoutsucre
      println("\t Sucre: " + sugarStocks(machineId) + "g")
      println("\t Lait : " + milkStocks(machineId)*0.001 + "L")
      println("\t Poudre: " + coffeeStocks(machineId) + "g")
    }
    println("Niveaux de stocks mis à jour.")
    println("Retour au menu principal...\n")
  }

  // définition des doses à soustraire
  var DoseCafeExpresso = 8 //grammes
  var DoseCafeCappuccino = 6 //grammes
  var DoseCafeLattePetit = 6 //grammes
  var DoseCafeLatteMoyen = 8 //grammes
  var DoseCafeLatteGrand = 12
  var DoseSucre = 5 // grammes
  var DoseLait = 50

  // définition des prix de base
  var PrixTotal = 0.0
  var PrixBaseExresso = 2.00 //CHF
  var PrixBaseCappuccino = 2.50 // CHF
  var PrixBaseLattePetit = 2.70 // CHF
  var PrixBaseLatteMoyen = 3.20 // CHF
  var PrixBaseLatteGrand = 3.70 // CHF

  // définition des prix des suppléments
  var SucrePrix = 0.10 // CHF par 5g
  var LaitPrix = 0.05 // CHF par 0.05L

  // définition qt suppléments
  val LimiteDoseLait = 3 // dose limite de lait supplémentaire
  var NbDoseLait = 0 // nombre de doses
  var TypeBoisson = 0

  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    TypeBoisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
    while(TypeBoisson > 3 || TypeBoisson < 1 ){
      TypeBoisson = readLine("Veuillez sélectionner une des boissons proposées : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
    }
    if (coffeeStocks(machineId) < DoseCafeExpresso || coffeeStocks(machineId) < DoseCafeCappuccino || coffeeStocks(machineId) < DoseCafeLattePetit || coffeeStocks(machineId) < DoseCafeLatteMoyen || coffeeStocks(machineId) < DoseCafeLatteGrand || milkStocks(machineId) < DoseLait || sugarStocks(machineId) < DoseSucre) {
      if (coffeeStocks(machineId) < DoseCafeExpresso || coffeeStocks(machineId) < DoseCafeCappuccino || coffeeStocks(machineId) < DoseCafeLattePetit || coffeeStocks(machineId) < DoseCafeLatteMoyen || coffeeStocks(machineId) < DoseCafeLatteGrand) {
        println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      } else if (milkStocks(machineId) < DoseLait) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      } else if (sugarStocks(machineId) < DoseSucre) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      }
      return false
    }
    //Expresso
    else {
      if (TypeBoisson == 1) {
        var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
        while (sucre > 4 || sucre < 1) {
          println("Veuillez choisir une des quantités de sucre proposée")
          sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
        }
        if (sucre == 2) {
          PrixTotal = PrixBaseExresso + SucrePrix
          LaitPrix = 0
          sugarStocks(machineId) = sugarStocks(machineId) - DoseSucre
          milkStocks(machineId) = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeExpresso
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, SucrePrix, PrixTotal)
        } // sucre 2
        else if (sucre == 3) {
          PrixTotal = PrixBaseExresso + (SucrePrix * 2)
          LaitPrix = 0
          sugarStocks(machineId) = sugarStocks(machineId) - (2 * DoseSucre)
          milkStocks(machineId) = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeExpresso
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 2 * SucrePrix, PrixTotal)
        } // sucre 3
        else if (sucre == 4) {
          PrixTotal = PrixBaseExresso + (SucrePrix * 3)
          LaitPrix = 0
          sugarStocks(machineId) = sugarStocks(machineId) - (DoseSucre * 3)
          milkStocks(machineId) = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeExpresso
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 3 * SucrePrix, PrixTotal)
        } // sucre 4
        else {
          PrixTotal = PrixBaseExresso
          LaitPrix = 0
          sugarStocks(machineId) = sugarStocks(machineId)
          milkStocks(machineId) = milkStocks(machineId)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeExpresso
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 0 * SucrePrix, PrixTotal)
        } // sucre 1
      } // if Expresso


      // cappuccino
      else if (TypeBoisson == 2) {
        var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
        while (sucre > 4 || sucre < 1) {
          println("Veuillez choisir une des quantités de sucre proposée")
          sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
        }
        var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
        while(lait > 2 || lait < 1){
          lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
        }
        if (sucre == 2) {
          PrixTotal = PrixBaseCappuccino + SucrePrix
          sugarStocks(machineId) = sugarStocks(machineId) - DoseSucre
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeCappuccino
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>1").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = DoseLait * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("le stock de lait " + milkStocks)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, LaitPrix, PrixTotal)
          } // lait oui
          else {
            LaitPrix = DoseLait * NbDoseLait
            PrixTotal = PrixTotal
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("le stock de lait " + milkStocks)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, 0 * LaitPrix, PrixTotal)
          } //lait 2
        } // sucre 2


        else if (sucre == 3) {
          PrixTotal = PrixBaseCappuccino + 2 * SucrePrix
          sugarStocks(machineId) = sugarStocks(machineId) - (2 * DoseSucre)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeCappuccino
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1)   {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, LaitPrix, PrixTotal)
          }
          else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } // sucre 3


        else if (sucre == 4) {
          PrixTotal = PrixBaseCappuccino + 3 * SucrePrix
          sugarStocks(machineId) = sugarStocks(machineId) - (3 * DoseSucre)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeCappuccino
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, LaitPrix, PrixTotal)
          }
          else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } //sucre 4


        else {
          PrixTotal = PrixBaseCappuccino + 0 * SucrePrix
          sugarStocks(machineId) = sugarStocks(machineId) - (0 * DoseSucre)
          coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeCappuccino
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1)  {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, LaitPrix, PrixTotal)
          }
          else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            milkStocks(machineId) = milkStocks(machineId) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } //sucre 1
      } // Cappuccino


      //Latte
      else {
        var TailleLatte = readLine("\nVeuillez sélectionner la taille de votre Latte : \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n> ").toInt
        while(TailleLatte > 3 || TailleLatte < 1) {
          TailleLatte = readLine("Veuillez choisir une taille de latte valable.").toInt
        }
        if (TailleLatte == 1) {
          var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          while (sucre > 4 || sucre < 1) {
            println("Veuillez choisir une des quantités de sucre proposée")
            sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          }
          var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          while(lait > 2 || lait < 1){
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLattePetit + SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - DoseSucre
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLattePetit
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2


          else if (sucre == 3) {
            PrixTotal = PrixBaseLattePetit + 2 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (2 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLattePetit
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3


          else if (sucre == 2) {
            PrixTotal = PrixBaseLattePetit + 3 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (3 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLattePetit
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4


          else {
            PrixTotal = PrixBaseLattePetit + 0 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (0 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLattePetit
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else {
              LaitPrix = LaitPrix* NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          }
        } //petit latte


        else if (TailleLatte == 2) {
          var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          while (sucre > 4 || sucre < 1) {
            println("Veuillez choisir une des quantités de sucre proposée")
            sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          }
          var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          while(lait > 2 || lait < 1){
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLatteMoyen + SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - DoseSucre
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteMoyen
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2


          else if (sucre == 3) {
            PrixTotal = PrixBaseLatteMoyen + 2 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (2 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteMoyen
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3


          else if (sucre == 4) {
            PrixTotal = PrixBaseLatteMoyen + 3 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (3 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteMoyen
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Beacoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4


          else if (sucre == 1) {
            PrixTotal = PrixBaseLatteMoyen + 0 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (0 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteMoyen
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 1


        } //moyen latte
        else if (TailleLatte == 3) {
          var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          while (sucre > 4 || sucre < 1) {
            println("Veuillez choisir une des quantités de sucre proposée")
            sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          }
          var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          while(lait > 2 || lait < 1){
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLatteGrand + SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - DoseSucre
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteGrand
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2


          else if (sucre == 3) {
            PrixTotal = PrixBaseLatteGrand + 2 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (2 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteGrand
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3


          else if (sucre == 4) {
            PrixTotal = PrixBaseLatteGrand + 3 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (3 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteGrand
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4


          else if (sucre == 1) {
            PrixTotal = PrixBaseLatteGrand + 0 * SucrePrix
            sugarStocks(machineId) = sugarStocks(machineId) - (0 * DoseSucre)
            coffeeStocks(machineId) = coffeeStocks(machineId) - DoseCafeLatteGrand
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              milkStocks(machineId) = milkStocks(machineId) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
            } // lait 2
          } // sucre 1
        } // grand latte
      }
    }
    return true
  }// Latte


  def main(args: Array[String]): Unit = {
    var mode = 0
    var sortir = mode == 3
    println("\t\nNospresso Café")
    while (sortir == false) {
      mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      while(mode > 3 || mode < 1){
        mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      }
      if (mode == 1) {
        var sortirModeClient = false
        while (sortirModeClient == false) {
          var machineID = readLine("Choississez votre machine (1-5). \n> ").toInt - 1
          while(machineID > 4 || machineID < 0){
           println ("Choisissez une machine valable!")
            machineID = readLine("Choississez votre machine (1-5). \n> ").toInt - 1
          }
          var paiementreussi = serveClient(machineID, StockCafe, StockSucre, StockLait)
          if (paiementreussi == false) {
            sortirModeClient = false
            // (sortirModeClient = false) car arrive pas le paiement
          } else {
            println('\n')
            val caracteres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            val random = new Random()
            var codetwint = " "
            val length = 5
            for (_ <- 1 to length) {
              val caracteresAleatoire = caracteres(random.nextInt(caracteres.length))
              codetwint += caracteresAleatoire
            }
            println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)\n")
            Thread.sleep(3000)
            println("Paiement confirmé.\nPréparation de votre boisson...")
            if (TypeBoisson == 1) {
              println("Votre Expresso est prêt ! Bonne dégustation !\n")
            } else if (TypeBoisson == 2) {
              println("Votre Cappuccino est prêt ! Bonne dégustation !\n")
            } else {
              println("Votre Latte est prêt ! Bonne dégustation !\n")
            }
            sortirModeClient = true
          }
        }
      } // mode 1
      else if (mode == 2) {
        println("\nMode Admin")
        var machineID = readLine("Choississez votre machine (1-5). \n> ").toInt - 1 // choix machine
        while(machineID > 4 || machineID < 0){
          machineID = readLine("Choisissez une machine valable ! ").toInt - 1
        }
        var codevalide = validatePin(machineID, machinePins)
        if (codevalide == false) {
          sortir = true
        } else {
          var quefaire = readLine("Souhaitez vous gérer les stocks (1) ou modifier le code PIN (2) ?").toInt
          while(quefaire > 2 || quefaire < 1){
            quefaire = readLine("Vous ne pouvez que gérer les stocks (1) et modifier le code PIN (2).").toInt
          }
          if (quefaire == 1) {
            restockMachine(machineID, StockCafe, StockSucre, StockLait)
          } else {
            updatePin(machineID, machinePins)
          }
        }
      } else {
        sortir = true
      }
    } //Do du début
  } // Array
}// main

