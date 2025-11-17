import scala.io.StdIn._
import scala.io.Source
import scala.util.Random
import java.io.{FileWriter, PrintWriter}
import collection.mutable.ArrayBuffer


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  // méthode pour ajouter du stock
  def addIngredient(ingredient: String, amount: Int): Unit = {
    val ingredients = ingredient.toLowerCase  // me permet de matcher avec les ingrédients de mon Array Buffer
    val estPositif = amount >= 0  // les valeurs ajoutées doivent être positives
    val Message = "Opération terminée. Les ingrédients ont été ajoutés"

    if (ingredients == "milk") { // me permet de rajouter du lait, si je demande à rajouter du lait
      val Stock = milk
      if (estPositif) {
        milk = Stock + amount
        val laitEnLitre = milk / 1000.0
        println((amount / 1000.0) + "L de lait ont été ajoutés. Nouveau stock : " + laitEnLitre + "L.")
      } else {
        println("La quantité doit être positive. Aucun lait n'a été ajouté.")
      }
    } else if (ingredients == "sugar") { // me permet de rajouter du sucre, si je demande à rajouter du sucre
      val Stock = sugar
      if (estPositif) {
        sugar = Stock + amount
        println(amount + "g de sucre ont été ajoutés. Stock mis à jour : " + sugar + "g.")
      } else {
        println("La quantité doit être positive. Aucun sucre n'a été ajouté.")
      }
    } else if (ingredients == "coffee") { // me permet de rajouter du café, si je demande à rajouter du café
      val Stock = coffee
      if (estPositif) {
        coffee = Stock + amount
        println(amount + "g de café ont été ajoutés. Stock mis à jour : " + coffee + "g.")
      } else {
        println("La quantité doit être positive. Aucun café n'a été ajouté.")
      }
    } else {
      val IngredientInconnuMessage = "Cet ingrédient n'est pas inclus."
      println(IngredientInconnuMessage + " Rien n'a été ajouté.")
      val errorStatus = false
    }
    println(Message)
  }
  // méthode pour soustraire du stock lors de la fabrication de café
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    val ingredients = ingredient.toLowerCase // me permet de matcher avec les ingrédients de mon Array Buffer
    val isValid = ingredients.nonEmpty
    val quantitePositif = amount > 0

    if (ingredients == "milk") {
      val Stock = milk
      if (Stock >= amount && quantitePositif) { // je vérifie que les stocks sont disponibles et que la quantité est positif
        milk = Stock - amount
        val laitEnLitre = milk / 1000.0
        println((amount / 1000.0) + "L de lait ont été retirés. Nouveau stock : " + laitEnLitre + "L.")
        true
      } else {
        println("Stock insuffisant. Aucun lait n'a été retiré.")
        false
      }
    } else if (ingredients == "sugar") {
      val Stock = sugar
      if (Stock >= amount && quantitePositif) { // je vérifie que les stocks sont disponibles et que la quantité est positif
        sugar = Stock - amount
        println(amount + "g de sucre ont été retirés. Stock mis à jour : " + sugar + "g.")
        true
      } else {
        println("Stock insuffisant. Aucun sucre n'a été retiré.")
        false
      }
    } else if (ingredients == "coffee") {
      val Stock = coffee
      if (Stock >= amount && quantitePositif) { // je vérifie que les stocks sont disponibles et que la quantité est positif
        coffee = Stock - amount
        println(amount + "g de café ont été retirés. Stock mis à jour : " + coffee + "g.")
        true
      } else {
        println("Stock insuffisant. Aucun café n'a été retiré.")
        false
      }
    } else {
      val IngredientInconnuMessage = "Cet ingrédient n'est pas inclus."
      println(IngredientInconnuMessage + " Rien n'a été retiré.")
      val IngredientErreur = true
      false
    }
  }
}

object Main {
  val machines = ArrayBuffer[Machine]() // les machines disponibles
  val listeMachines = "machines.csv" // fichier csv

  def main(args: Array[String]): Unit = {

    machines  ++= LectureMachines(listeMachines)

    var mode = 0
    var loop = true
    while (loop) {
      var machine = choixMachine()
      if (machine != null) {
        println("\nNospresso Café")
        println("Veuillez séltionner votre mode :")
        println("1) Client\n2) Admin\n3) Quitter\n> ")
        mode = readLine().toInt
        if (mode == 1) {
          modeclient(machine)
        }
        if (mode == 2) {
          if (validatePin(machine)) {
            println("1) Réapprovisionnement machine\n2) Mise à jour du code PIN\n3) Quitter\n>")
            val choixadmin = readLine().toInt
            if (choixadmin == 1) {
              restockMachine(machine)
            }
            else if (choixadmin == 2) {
              updatePin(machine)
            } else if (choixadmin == 3) {
              println("Retour au menu principal.")
            }
            else {
              println("Option invalide. Retour au menu principal.")
            }
          }
        }
        if (mode == 3) {
          SauvegardeMachines(listeMachines,machines)
          loop = false
        }
      }
    }
  }

  def modeclient(machine: Machine): Boolean = {
    {
      //Type supplement/boisson
      var sucre = ""
      var lait = ""
      var boisson = ""
      // Prix boissons
      var prixboisson = 0.0
      val Expresso = 2.00
      val Cappuccino = 2.50
      val LattePetit = 2.70
      val LatteMoyen = 3.20
      val LatteGrand = 3.70
      // Prix
      var prix = 0.0
      var prixsucre = 0.0
      var prixlait = 0.0
      // Quantité consommée
      var qsucre = 0
      var qlait = 0
      var qcafe = 0
      // Choix boisson
      var choixBoisson = readLine("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
      while ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) {
        if ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) println("Selction invalide")
        choixBoisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
      }

      // expresso
      if (choixBoisson == 1) {
        prixboisson = Expresso
        prix = Expresso
        boisson = "Expresso"
        lait = "Non"
        qcafe = 8
        qlait = 0
        //Choix quantité de sucre
        var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
          if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
          Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        if (Qsucre == 1) {
          sucre = "sans sucre"
          qsucre = 0
        } else if (Qsucre == 2) {
          sucre = "Peu (5g)"
          prixsucre = 0.10
          qsucre = 5
          prix = Expresso + 0.10
        } else if (Qsucre == 3) {
          sucre = "Moyen (10g)"
          prixsucre = 0.20
          qsucre = 10
          prix = Expresso + 0.20
        } else {
          sucre = "Beaucoup (15g)"
          prixsucre = 0.30
          qsucre = 15
          prix = Expresso + 0.30
        }
        // Cappuccino
      }
      else if (choixBoisson == 2) {
        //Diminution dans les stock
        prixboisson = Cappuccino
        boisson = "Cappuccino"
        qcafe = 6
        qlait = 100
        prix = Cappuccino
        //Choix quantité de sucre
        var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
          if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
          Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        if (Qsucre == 1) {
          sucre = "sans sucre"
          qsucre = 0
        } else if (Qsucre == 2) {
          sucre = "Peu (5g)"
          prixsucre = 0.1
          qsucre = 5
          prix = prix + 0.10
        } else if (Qsucre == 3) {
          sucre = "Moyen (10g)"
          prixsucre = 0.2
          qsucre = 10
          prix = prix + 0.20
        } else {
          sucre = "Beaucoup (15g)"
          prixsucre = 0.3
          qsucre = 15
          prix = prix + 0.30
        }
        // Choix Quantité de lait
        var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
        while ((Qlait != 1) && (Qlait != 2)) {
          if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
          Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ").toInt
        }
        if (Qlait == 1) {
          lait = "Oui"
          var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
          while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
            if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
            DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
          }
          if (DoseLaitSupp == 1) {
            prixlait = 0.05
            qlait = qlait + 50
            prix = prix + 0.05
          } else if (DoseLaitSupp == 2) {
            qlait = qlait + 100
            prixlait = 0.10
            prix = prix + 0.10
          } else {
            qlait = qlait + 150
            prixlait = 0.15
            prix = prix + 0.15
          }
        } else {
          lait = "Non"
          qlait = qlait + 0
        }
        // Latte
      }
      else {
        var Latte = readLine("\nVeuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
        while ((Latte != 1) && (Latte != 2) && (Latte != 3)) {
          if ((Latte != 1) && (Latte != 2) && (Latte != 3)) println("Selection invalide")
          Latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Petit - CHF 2.70\n2) Moyen - 3.20\n3) Grand - 3.70\n> ").toInt
        }

        // Petit latte
        if (Latte == 1) {
          prixboisson = LattePetit
          boisson = "Petit Latte"
          qlait = 120
          qcafe = 6
          prix = LattePetit
          // Quantité de sucre
          var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
            if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
            Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          }
          if (Qsucre == 1) {
            sucre = "sans sucre"
          }
          else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            prixsucre = 0.1
            qsucre = 5
            prix = LattePetit + 0.10
          }
          else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LattePetit + 0.20
            prixsucre = 0.2
          }
          else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
            prix = LattePetit + 0.30
            prixsucre = 0.3
          }
          // Dose de lait
          var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          while ((Qlait != 1) && (Qlait != 2)) {
            if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
            Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          }
          if (Qlait == 1) {
            lait = "Oui"
            var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
              if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
              DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            }
            if (DoseLaitSupp == 1) {
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            }
            else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            }
            else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          }
          else {
            lait = "Non"
            qlait = qlait + 0
          }

          // Latte moyen
        }
        else if (Latte == 2) {
          prixboisson = LatteMoyen
          boisson = "Moyen Latte"
          qlait = 150
          qcafe = 8
          prix = LatteMoyen
          var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
            if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
            Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          }
          if (Qsucre == 1) {
            sucre = "sans sucre"
            qsucre = 0
          } else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            qsucre = 5
            prix = LatteMoyen + 0.10
            prixsucre = 0.1
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LatteMoyen + 0.20
            prixsucre = 0.2
          } else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
            prix = LatteMoyen + 0.30
            prixsucre = 0.3
          }
          // Dose de lait
          var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          while ((Qlait != 1) && (Qlait != 2)) {
            if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
            Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          }
          if (Qlait == 1) {
            lait = "Oui"
            var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
              if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
              DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            }
            if (DoseLaitSupp == 1) {
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            } else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            } else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          } else {
            lait = "Non"
          }

          // Grand Latte
        }
        else {
          prixboisson = LatteGrand
          boisson = "Grand Latte"
          qlait = 200
          qcafe = 12
          prix = LatteGrand
          var Qsucre = readLine("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          while ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) {
            if ((Qsucre != 1) && (Qsucre != 2) && (Qsucre != 3) && (Qsucre != 4)) println("Selection invalide")
            Qsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
          }
          if (Qsucre == 1) {
            sucre = "sans sucre"
            qsucre = 0
          } else if (Qsucre == 2) {
            sucre = "Peu (5g)"
            qsucre = 5
            prix = LatteGrand + 0.10
            prixsucre = 0.1
          } else if (Qsucre == 3) {
            sucre = "Moyen (10g)"
            qsucre = 10
            prix = LatteGrand + 0.20
            prixsucre = 0.2
          } else {
            sucre = "Beaucoup (15g)"
            qsucre = 15
            prix = LatteGrand + 0.30
            prixsucre = 0.3
          }
          // Dose de lait
          var Qlait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
          while ((Qlait != 1) && (Qlait != 2)) {
            if ((Qlait != 1) && (Qlait != 2)) println("Selection invalide")
            Qlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
          }
          if (Qlait == 1) {
            lait = "Oui"
            var DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            while ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) {
              if ((DoseLaitSupp != 1) && (DoseLaitSupp != 2) && (DoseLaitSupp != 3)) println("Selection invalide")
              DoseLaitSupp = readLine("\nCombien de dose ? (maximum 3 doses)\n> ").toInt
            }
            if (DoseLaitSupp == 1) {
              qlait = qlait + 50
              prixlait = 0.05
              prix = prix + 0.05
            } else if (DoseLaitSupp == 2) {
              qlait = qlait + 10
              prixlait = 0.10
              prix = prix + 0.10
            } else {
              qlait = qlait + 150
              prixlait = 0.15
              prix = prix + 0.15
            }
          } else {
            lait = "Non"
          }
        }
      }
      // Recapitulatif de la commande
      if (boisson == "Expresso") {
        println("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre)
      }
      else {
        println("\nBoisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucre + "\nLait supplémentaire : " + lait)
      }
      //cas où stock insufisant
      if (qcafe > machine.coffee) {
        println("\nErreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionnée\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        machine.addIngredient("coffee",qcafe)
        machine.addIngredient("milk",qlait)
        machine.addIngredient("sugar",qsucre)
      }
      else if (qlait > machine.milk) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        machine.addIngredient("milk",qlait)
        machine.addIngredient("coffee",qcafe)
        machine.addIngredient("sugar",qsucre)
      }
      else if (qsucre > machine.sugar) {
        println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir un autre dosage de sucre ou vérifier les stocks en mode Admin.\n")
        machine.addIngredient("sugar",qsucre)
        machine.addIngredient("milk",qlait)
        machine.addIngredient("coffee",qcafe)
      }
      else {
        // supp sucre
        if ((prixsucre > 0) && (prixlait == 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prix)
        }
        // supp sucre et lait
        else if ((prixsucre > 0) && (prixlait > 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixlait, prix)
        }
        // supp lait
        else if ((prixsucre == 0) && (prixlait > 0)) {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixlait, prix)
        }
        // pas supp
        else {
          printf("Prix total : CHF %.2f", prixboisson)
        }
        val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        val random = new Random()
        var codeTwint = ""
        for (_ <- 1 to 5) {
          val charAleatoire = caracteres(random.nextInt(caracteres.length))
          codeTwint += charAleatoire
        }
        println(("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : ") + codeTwint + ("\nEn attente de validation du paiement...\n"))
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.")

        println("Préparation de votre boisson...")
        println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
      }
      // Diminution des stocks
      machine.removeIngredient("milk",qlait)
      machine.removeIngredient("sugar",qsucre)
      machine.removeIngredient("coffee",qcafe)
    }
    true
  }

  def validatePin(machine: Machine): Boolean = {
    var essaies = 2
    var codePin = readLine("Entrez le code PIN : ")
    while (codePin != machine.pincode) {
      println("Code PIN incorrrect. " + essaies + " tentatives restantes")
      if (essaies <= 0) {
        println("\nTrop de tentatives échouées. Fin du programme.")
        System.exit(0)
      }
      codePin = readLine("Entrez le code PIN : ")
      essaies -= 1
    }
    println("Accès accordé à la Machine " + machine.id + ".\n")
    true
  }

  def updatePin(machine: Machine): Unit = {
    println("Mise à jour du code PIN pour la Machine " + machine.id + ".")
    var codeValide = false
    while (!codeValide) {
      println("Entrez un nouveau code PIN à 6 chiffres > ")
      val pinAsString = readLine()

      if (pinAsString.length == 6 && pinAsString.forall(_.isDigit)) {
        machine.pincode = pinAsString
        println("Le code PIN a été mis à jour avec succes.\nRetour au menu principal...\n")
        codeValide = true
      } else {
        println("Le code Pin n'a pas été mis à jour.\nRetour au menu principal...\n")
        return false
      }
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println("Niveau de stock actuels : \nPoudre à café : " + machine.coffee + " g")
    println("Lait : " + machine.milk/ 1000.0)
    println("Sucre : " + machine.sugar + " g\n")
    println("Entrez les quantité à ajouter :\n")
    val StockPoudreCafe = readLine("Poudre à café (en g) : ").toInt
    machine.addIngredient("coffee", StockPoudreCafe)
    var LaitAjoute = 0.0
    LaitAjoute = readLine("Lait (en L) : ").toDouble
    val StockLait = (LaitAjoute * 1000.0).toInt
    machine.addIngredient("milk", StockLait)
    val StockSucre = readLine("Sucre (en g) : ").toInt
    machine.addIngredient("sugar", StockSucre)
    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
  }

  def LectureMachines(filePath: String): ArrayBuffer[Machine] = {
    machines.clear()
    val machinesObtenues = ArrayBuffer[Machine]()
    println("Démarrage du processus de chargement...")
    val cheminFichier = "machines.csv"
    println("Chargement des machines depuis le fichier " + cheminFichier + "...")
    try {
      val machines_dispo = Source.fromFile(cheminFichier).getLines().drop(1).toList
      val lignesTotales = machines_dispo.size
      println("\n--- Informations sur les machines ---")
      for (lignes <- machines_dispo) {
        val donnéesMachine = lignes.split(",")
        if (donnéesMachine.length == 4) {
          val machineId = machinesObtenues.size + 1
          val machine = new Machine(
            id = machineId,
            pincode = donnéesMachine(0),
            milk = donnéesMachine(1).toInt,
            sugar = donnéesMachine(2).toInt,
            coffee = donnéesMachine(3).toInt
          )
          machinesObtenues += machine
          println("")
          println("La machine " + machine.id + " a été chargée avec succès :")
          println("    ID : " + machine.id)
          println("    Code PIN : " + machine.pincode)
          println("    Lait : " + (machine.milk / 1000.0) + "L")
          println("    Sucre : " + machine.sugar + "g")
          println("    Café : " + machine.coffee + "g")
        }
      }
      println("\nTotal des machines chargées : " + machinesObtenues.size + ".")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    machinesObtenues
  }

  def SauvegardeMachines(filePath: String, machines: ArrayBuffer[Machine]): Unit = {
    println("\nDémarrage de la sauvegarde des machines...")
    val cheminFichier = "machines.csv"
    println("Enregistrement des machines dans le fichier : " + cheminFichier + "...")
    try {
      val nouveauxStocks = new PrintWriter(cheminFichier)
      nouveauxStocks.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        val stockMachine = machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee
        nouveauxStocks.println(stockMachine)
      }
      nouveauxStocks.close()
      println("\nSauvegarde réussie : " + machines.size + " machine(s) enregistrée(s) dans " + cheminFichier + ".")
    } catch {
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans " + cheminFichier + ".")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de l'enregistrement des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    println("Merci à bientot!")
  }

  def choixMachine(): Machine = {
    println("\nSélection de la machine à utiliser...")
    val totalMachines = machines.size
    val identifiants = machines.map(_.id)
    println("Nombre total de machines disponibles : " + totalMachines)
    println("\nVeuillez choisir une machine disponible entre 1 et " +totalMachines + ".")
    var machineDispo: Machine = null
    var machineAccepté= false
    while (!machineAccepté) {
      println("\nEntrez le numéro de la machine que vous voulez sélectionner : ")
      try {
        val choix = readInt()
        if (choix >= 1 && choix <= totalMachines) {
          machineDispo = machines(choix - 1)
          machineAccepté = true
        } else {
          println("Le choix " + choix + " est hors des limites valides (1-" + totalMachines + ").")
        }
      } catch {
        case _: NumberFormatException =>
          println("Entrée invalide : veuillez entrer un numéro.")
      }
    }
    println("\nLa machine sélectionnée est : " + machineDispo.id)
    machineDispo
  }
}