import java.io.{FileWriter, PrintWriter}
import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random


object Main {
  val machines = ArrayBuffer[Machine]()

  class Machine(var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) { // machineID = Id

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "café") {
        machines(id).coffee += amount
      } else if (ingredient == "lait") {
        machines(id).milk += amount
      } else if (ingredient == "sucre") {
        machines(id).sugar += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      //var amount = true
      if (ingredient == "café") {
        if (coffee >= amount) {
          machines(id).coffee -= amount
          return true
        } else {
          println("Erreur, la quantité de café n'est pas suffisante.")
          return false
        }
      } else if (ingredient == "lait") {
        if (milk >= amount) {
          machines(id).milk -= amount
          return true
        } else {
          println("Erreur, la quantité de lait n'est pas suffisante.")
          return false
        }
      } else if (ingredient == "sucre") {
        if (sugar >= amount) {
          machines(id).sugar -= amount
          return true
        } else {
          println("Erreur, la quantité de sucre n'est pas suffisante.")
          return false
        }
      }
      false
    }
  }

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
      true
    } else {
      println("\nTrop de tentatives échouées. Fin du programme. ")
      false
    }
  }

  // update pins
  def updatePin(machineId: Int): Unit = {
    var CodePINModifie = " "
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
    CodePINModifie = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
    var codeincorrect = CodePINModifie.length != 6 || !CodePINModifie.forall(_.isDigit)
    while (codeincorrect == true) {
      CodePINModifie = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
      if (CodePINModifie.length == 6 && CodePINModifie.forall(_.isDigit)) {
        //println("Code PIN actuel " + codeadmin.mkString)
        codeincorrect = false
      }
    }
    println("\nLe code PIN a été mis à jour avec succès. \nRetour au menu principal...\n")
    machines(machineId).pincode = CodePINModifie
    // print(machinePins.mkString(" "))
  }

  //gestion stock
  /* var StockSucre = Array.fill(5)(30)
     var StockCafe = Array.fill(5)(50)
     var StockLait = Array.fill(5)(500)*/

  def restockMachine(machineId: Int): Unit = {
    var ajout = readLine("\nSouhaitez-vous réapprovisionner : \n1) Sucre \n2) Poudre de café \n3) Lait \n>").toInt
    while (ajout > 3 && ajout < 1) {
      ajout = readLine("Vous ne pouvez qu'ajouter : \n1) Sucre \n2) Poudre de café \n3) Lait \n>").toInt
    }
    println("\nRéapprovisionnement des stocks ... \nAjout : \n")
    if (ajout == 2) {
      var ajoutcafe = readLine("Quelle quantité de café souhaitez-vous rajouter ?").toInt
      while (ajoutcafe < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutcafe = readInt()
      }
      machines(machineId).addIngredient("café", ajoutcafe)
      println("\t Café : " + machines(machineId).coffee + "g")
      println("\t Lait : " + machines(machineId).milk * 0.001 + "L")
      println("\t Sucre : " + machines(machineId).sugar + "g")
    } else if (ajout == 3) {
      var ajoutlait = readLine("Quelle quantité de lait souhaitez-vous rajouter ?").toInt
      while (ajoutlait < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutlait = readInt()
      }
      machines(machineId).addIngredient("lait", ajoutlait)
      println("\t Lait : " + machines(machineId).milk * 0.001 + "L")
      println("\t Café : " + machines(machineId).coffee + "g")
      println("\t Sucre : " + machines(machineId).sugar + "g")
    }
    else {
      var ajoutsucre = readLine("Quelle quantité de sucre souhaitez-vous rajouter ?").toInt
      while (ajoutsucre < 0) {
        println("Vous ne pouvez pas ajouter de quantités négatives")
        ajoutsucre = readInt()
      }
      machines(machineId).addIngredient("sucre", ajoutsucre)
      println("\t Sucre: " + machines(machineId).sugar + "g")
      println("\t Lait : " + machines(machineId).milk * 0.001 + "L")
      println("\t Café: " + machines(machineId).coffee + "g")
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
  var sucre = 0


  def serveClient(machineId: Int): Boolean = {
    TypeBoisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
    while (TypeBoisson > 3 || TypeBoisson < 1) {
      TypeBoisson = readLine("Veuillez sélectionner une des boissons proposées : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
    }
    if (machines(machineId).coffee < DoseCafeExpresso || machines(machineId).coffee < DoseCafeCappuccino || machines(machineId).coffee < DoseCafeLattePetit || machines(machineId).coffee < DoseCafeLatteMoyen || machines(machineId).coffee < DoseCafeLatteGrand || machines(machineId).milk < DoseLait || machines(machineId).sugar < DoseSucre) {
      if (machines(machineId).coffee < DoseCafeExpresso || machines(machineId).coffee < DoseCafeCappuccino || machines(machineId).coffee < DoseCafeLattePetit || machines(machineId).coffee < DoseCafeLatteMoyen || machines(machineId).coffee < DoseCafeLatteGrand) {
        println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      } else if (machines(machineId).milk < DoseLait) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      } else if (machines(machineId).sugar < DoseSucre) {
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
          /*machines(id).removeIngredient("sugar") = machines(id).removeIngredient(sugar) - DoseSucre
            machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeExpresso*/
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, SucrePrix, PrixTotal)
        } // sucre 2
        else if (sucre == 3) {
          PrixTotal = PrixBaseExresso + (SucrePrix * 2)
          LaitPrix = 0
          /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (2 * DoseSucre)
            machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeExpresso*/
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 2 * SucrePrix, PrixTotal)
        } // sucre 3
        else if (sucre == 4) {
          PrixTotal = PrixBaseExresso + (SucrePrix * 3)
          LaitPrix = 0
          /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (DoseSucre * 3)
            machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeExpresso*/
          println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
          printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 3 * SucrePrix, PrixTotal)
        } // sucre 4
        else {
          PrixTotal = PrixBaseExresso
          LaitPrix = 0
          //machines(machineId).removeIngredient("sugar") = machines(machineId).removeIngredient("sugar")
           // machines(machineId).removeIngredient("milk") = machines(machineId).removeIngredient("milk")
            //machines(machineId).removeIngredient("coffee") = machines(machineId).removeIngredient("coffee") - DoseCafeExpresso
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
        while (lait > 2 || lait < 1) {
          lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
        }
        if (sucre == 2) {
          PrixTotal = PrixBaseCappuccino + SucrePrix
          /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - DoseSucre
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeCappuccino*/
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>1").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = DoseLait * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            /* machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("le stock de lait " + machines(machineId).milk)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, LaitPrix, PrixTotal)
          } // lait oui
          else {
            LaitPrix = DoseLait * NbDoseLait
            PrixTotal = PrixTotal
            /* machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("le stock de lait " + machines(machineId).milk)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, 0 * LaitPrix, PrixTotal)
          } //lait 2
        } // sucre 2
        else if (sucre == 3) {
          PrixTotal = PrixBaseCappuccino + 2 * SucrePrix
          /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (2 * DoseSucre)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeCappuccino*/
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            /*machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, LaitPrix, PrixTotal)
          } else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            /* machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } // sucre 3
        else if (sucre == 4) {
          PrixTotal = PrixBaseCappuccino + 3 * SucrePrix
          /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (3 * DoseSucre)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeCappuccino*/
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            /* machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, LaitPrix, PrixTotal)
          } else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            /* machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)*/
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } //sucre 4
        else {
          PrixTotal = PrixBaseCappuccino + 0 * SucrePrix
          /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (0 * DoseSucre)
            machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeCappuccino*/
          if (lait == 1) {
            var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
              println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
              NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
            }
            println("Vous avez choisi " + NbDoseLait + " doses.")
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal + LaitPrix
            // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, LaitPrix, PrixTotal)
          }
          else {
            LaitPrix = LaitPrix * NbDoseLait
            PrixTotal = PrixTotal
            // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 100 - (NbDoseLait * DoseLait)
            println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
            printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
          }
        } //sucre 1
      } // Cappuccino
      //Latte
      else {
        var TailleLatte = readLine("\nVeuillez sélectionner la taille de votre Latte : \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n> ").toInt
        while (TailleLatte > 3 || TailleLatte < 1) {
          TailleLatte = readLine("Veuillez choisir une taille de latte valable.").toInt
        }
        if (TailleLatte == 1) {
          var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          while (sucre > 4 || sucre < 1) {
            println("Veuillez choisir une des quantités de sucre proposée")
            sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
          }
          var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          while (lait > 2 || lait < 1) {
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLattePetit + SucrePrix
            /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - DoseSucre
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLattePetit*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2
          else if (sucre == 3) {
            PrixTotal = PrixBaseLattePetit + 2 * SucrePrix
            /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (2 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLattePetit*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3
          else if (sucre == 2) {
            PrixTotal = PrixBaseLattePetit + 3 * SucrePrix
            /*machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (3 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLattePetit*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4
          else {
            PrixTotal = PrixBaseLattePetit + 0 * SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (0 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLattePetit*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              //  machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 120 - (NbDoseLait * DoseLait)
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
          while (lait > 2 || lait < 1) {
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLatteMoyen + SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - DoseSucre
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteMoyen*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2
          else if (sucre == 3) {
            PrixTotal = PrixBaseLatteMoyen + 2 * SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (2 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteMoyen*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3
          else if (sucre == 4) {
            PrixTotal = PrixBaseLatteMoyen + 3 * SucrePrix
            /*  machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (3 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteMoyen*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Beacoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4
          else if (sucre == 1) {
            PrixTotal = PrixBaseLatteMoyen + 0 * SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (0 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteMoyen*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 150 - (NbDoseLait * DoseLait)
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
          while (lait > 2 || lait < 1) {
            lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt
          }
          if (sucre == 2) {
            PrixTotal = PrixBaseLatteGrand + SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - DoseSucre
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteGrand*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, LaitPrix, PrixTotal)
            } // lait oui
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, 0 * LaitPrix, PrixTotal)
            } //lait 2
          } //sucre 2
          else if (sucre == 3) {
            PrixTotal = PrixBaseLatteGrand + 2 * SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (2 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteGrand*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              //machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } // sucre 3
          else if (sucre == 4) {
            PrixTotal = PrixBaseLatteGrand + 3 * SucrePrix
            /* machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (3 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteGrand*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
            }
          } //sucre 4
          else if (sucre == 1) {
            PrixTotal = PrixBaseLatteGrand + 0 * SucrePrix
            /*  machines(id).removeIngredient(sugar) = machines(id).removeIngredient(sugar) - (0 * DoseSucre)
              machines(id).removeIngredient(coffee) = machines(id).removeIngredient(coffee) - DoseCafeLatteGrand*/
            if (lait == 1) {
              var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              while (NbDoseLait > LimiteDoseLait || NbDoseLait < 1) {
                println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
              }
              println("Vous avez choisi " + NbDoseLait + " doses.")
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal + LaitPrix
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, LaitPrix, PrixTotal)
            }
            else if (lait == 2) {
              LaitPrix = LaitPrix * NbDoseLait
              PrixTotal = PrixTotal
              // machines(id).removeIngredient(milk) = machines(id).removeIngredient(milk) - 200 - (NbDoseLait * DoseLait)
              println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
              printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
            } // lait 2
          } // sucre 1
        } // grand latte
      }// Latte
    }
    true
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    //val machines = ArrayBuffer[Machine]()
    try {
      val tableaumachine = Source.fromFile(filename)
      val lignetableaumachine = tableaumachine.getLines().drop(1)
      var id = 0
      for (line <- lignetableaumachine) {
        val uneMachine = line.split(",")
        machines += new Machine(id, uneMachine(0), uneMachine(1).toInt, uneMachine(2).toInt, uneMachine(3).toInt)
        id += 1
      }
      tableaumachine.close()
    } catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifier le chemin d'accès et réessayez.")

      case ex: java.io.IOException => println("Erreur : Echec de l'écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")

      case ex: Exception => println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")

    }
      machines
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val pW = new PrintWriter(new FileWriter(filename), true)
      pW.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        pW.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      pW.close()
    } catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifier le chemin d'accès et réessayez.")
      case ex: java.io.IOException => println("Erreur : Echec de l'écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
      case ex: java.nio.file.AccessDeniedException => println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }
  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis.csv...")
    loadcsv("src/machines.csv")
    if (!machines.isEmpty) {
      for (machine <- machines) {
        println("Machine ID: " + machine.id)
        println("Code PIN: " + machine.pincode)
        println("Lait: " + machine.milk + " L")
        println("Sucre: " + machine.sugar + " g")
        println("Café: " + machine.coffee + " g")
        println("Machine " + machine.pincode + " - Lait: " + machine.milk + ", Sucre: " + machine.sugar + ", Café: " + machine.coffee)
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")
    } else {
      println("Aucune machine n'a été trouvée.")
    }
    var mode = 0
    var sortir = mode == 3
    println("\t\nNospresso Café")
    while (!sortir) {
      mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      while (mode > 3 || mode < 1) {
        mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      }
      if (mode == 1) {
        var sortirModeClient = false
        while (!sortirModeClient) {
          var machineID = readLine("Choississez votre machine (1-" + machines.length + "). \n> ").toInt- 1
          while (machineID > 4 || machineID < 0) {
            println("Choisissez une machine valable!")
            machineID = readLine("Choississez votre machine (1-" + machines.length + "). \n> ").toInt- 1
          }
          val paiementreussi = serveClient(machineID)
          if(!paiementreussi){
            sortirModeClient = false
            // (sortirModeClient = false) car arrive pas le paiement
          } else {
            println('\n')
            val caracteres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            val random = new Random()
            var codetwint = ""
            val length = 5
            for (_ <- 1 to length) {
              val caracteresAleatoire = caracteres(random.nextInt(caracteres.length))
              codetwint += caracteresAleatoire
            }
            println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)\n")
            Thread.sleep(3000)
            println("Paiement confirmé.\nPréparation de votre boisson...")
            //val TypeBoisson = serveClient(machineID)
            if (TypeBoisson == 1) {
              machines(machineID).removeIngredient("sugar", DoseSucre * sucre)
              machines(machineID).removeIngredient("coffee", DoseCafeExpresso)
              machines(machineID).removeIngredient("milk", DoseLait * NbDoseLait)
              println("Votre Expresso est prêt ! Bonne dégustation !\n")
            } else if (TypeBoisson == 2) {
              machines(machineID).removeIngredient("sugar", DoseSucre * sucre)
              machines(machineID).removeIngredient("coffee", DoseCafeCappuccino)
              machines(machineID).removeIngredient("milk", DoseLait * NbDoseLait)
              println("Votre Cappuccino est prêt ! Bonne dégustation !\n")
            } else {
              machines(machineID).removeIngredient("sugar", DoseSucre * sucre)
              machines(machineID).removeIngredient("coffee", DoseCafeLattePetit)
              machines(machineID).removeIngredient("coffee", DoseCafeLatteMoyen)
              machines(machineID).removeIngredient("coffee", DoseCafeLatteGrand)
              machines(machineID).removeIngredient("milk", DoseLait * NbDoseLait)
              println("Votre Latte est prêt ! Bonne dégustation !\n")
            }
            savecsv("src/machines.csv", machines)
            sortirModeClient = true
          }
        }
      } // mode 1
      else if (mode == 2) {
        println("\nMode Admin")
        var machineID = readLine("Choississez votre machine (1-" + machines.length + "). \n> ").toInt - 1
        while (machineID > 4 || machineID < 0) {
          machineID = readLine("Choisissez une machine valable ! ").toInt - 1
        }
        println("Veuillez entrer 3 fois le même code svp. ")
        var codeadmin = readLine("Entrez le code PIN : ").toInt
        val codevalide = machines(machineID).pincode
        var tentatives = 0
        //var accesaccorde = false
        while (codeadmin != codevalide && tentatives < 2){

          var codeadmin = readLine("Entrez le code PIN : ").toInt
          tentatives += 1
        }
        if  (codeadmin != codevalide && tentatives == 3){
          sortir = true
          //accesaccorde = true
        } else {
          //tentatives = 3
          println("Accès accordé")
          var quefaire = readLine("Souhaitez vous gérer les stocks (1) ou modifier le code PIN (2) ?").toInt
          while (quefaire > 2 || quefaire < 1) {
            quefaire = readLine("Vous ne pouvez que gérer les stocks (1) et modifier le code PIN (2).").toInt
          }
          if (quefaire == 1) {
            restockMachine(machineID)
          } else {
            updatePin(machineID)
          }
        }
      } else {
        savecsv("src/machines.csv", machines)
        println("Sauvegarde de " + machines.length + " machines dans machines.csv")
        println("Fichier sauvegardé avec succès.")
        sortir = true
      }
    } //Do du début
  }
}
// main Array