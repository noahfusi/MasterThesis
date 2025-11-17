import io.StdIn._
import scala.util.Random
object Main {

  // METHODES OBLIGATOIRES :
  // Méthode validatePin
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentative = 0
    val nbTentatives = 3
    var pinOk = false

    while (tentative < nbTentatives && !pinOk) {
      println("Entrez le code PIN : ")
      print("> ")
      val pinDemarrage = readLine()

      if (pinDemarrage == machinePins(machineId)) {
        println("Accès accordé à la machine " + (machineId + 1)+".")
        pinOk = true
      } else {
        tentative += 1
        println("Code PIN incorrect. " + (nbTentatives-tentative) + " tentatives restantes.")
      }
    }
    return pinOk
  }

  // Méthode updatePin
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var pin = false

    //while (!pin) {
    println("Entrez un nouveau code PIN à 6 chiffres > ")
    val nouveauPin = readLine()

    // Entrée d'un nouveau PIN validé
    if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)){
      machinePins(machineId) = nouveauPin
      println("Le code PIN a été mis à jour avec succès.")
      println("Le nouveau PIN est : " + machinePins(machineId))
      println("Retour au menu principal ... ")
      println()
      pin = true
    }
    // Entrée d'un nouveau PIN non validé
    else {
      println("Le code PIN n'a pas été mis à jour.")
      println("Retour au menu principal ...")
      println()
      return
    }
  }

  // Méthode serveClient
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    // Variables
    var coffee = 0
    var sugar = 0
    var milk = 0
    var choixBoisson = 0
    var tailleLatte = 0
    var choixSugar = 0
    var supplementMilk = 0
    var doseMilk = 0

    do {
      println("Veuillez sélectionner votre boisson : ")
      println("1) Expresso - CHF 2.00 ")
      println("2) Cappuccino - CHF 2.50 ")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
      print("> ")

      choixBoisson = readLine().toInt
      if (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3) {
        println("Choix du boisson incorrect.")
      }
    } while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)

    // Quantités nécessaires pour préparer les boissons
    if (choixBoisson == 1 ) {
      // Expresso
      coffee = 8
      sugar = 0
      milk = 0
    } else if (choixBoisson == 2) {
      // Cappuccino
      coffee = 6
      sugar = 0
      milk = 100
    } else if (choixBoisson == 3) {
      // Latte
      do {
        println("Veuillez sélectionner la taille : ")
        println("1) Petit ")
        println("2) Moyen ")
        println("3) Grand")
        print("> ")

        tailleLatte = readLine().toInt
        if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
          println("Choix de la taille incorrect.")
        }
      } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)

      // tailles du latte
      if (tailleLatte == 1) {
        // Petit
        coffee = 6
        sugar = 0
        milk = 120
      } else if (tailleLatte == 2) {
        // Moyen
        coffee = 8
        sugar = 0
        milk = 150
      } else if (tailleLatte == 3) {
        // Grand
        coffee = 12
        sugar = 0
        milk = 200
      }
    }

    // Sélection sucre invalide
    do {
      println("Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10 ")
      println("3) Moyen (10g) - CHF 0.20 ")
      println("4) Beaucoup (15g) - CHF 0.30 ")
      print("> ")

      choixSugar = readInt()
      if (choixSugar != 1 && choixSugar != 2 && choixSugar != 3 && choixSugar != 4) {
        println("Choix du sucre incorrect.")
      }
    } while (choixSugar != 1 && choixSugar != 2 && choixSugar != 3 && choixSugar != 4)
    // Sélection sucre valide
    if (choixSugar == 2) {
      sugar = 5
    } else if (choixSugar == 3) {
      sugar = 10
    } else if (choixSugar == 4) {
      sugar = 15
    }

    // Sélection lait supplementaire
    if (choixBoisson == 2 || choixBoisson == 3 || tailleLatte == 1 || tailleLatte == 2 || tailleLatte == 3) {
      var choixValide = false
      // sélection lait suppl. invalide
      do {
        println("Souhaitez-vous ajouter du lait en supplément ? ")
        println("1) Oui ")
        println("2) Non ")
        print("> ")

        supplementMilk = readInt()
        // Supplement lait OUI
        if (supplementMilk == 1) {
          do {
            println("Combien de dose(s) ? (3 doses maximales par boisson)")
            print("> ")
            doseMilk = readInt()
            if (doseMilk >= 1 && doseMilk <= 3) {
              milk += (doseMilk * 50)
              choixValide = true
            } else {
              println("Erreur : 3 doses maximales par boisson ! ")
            }
          } while (doseMilk < 1 || doseMilk > 3)
        }
        // Supplement lait NON
        else if (supplementMilk == 2) {
          println("Pas de dose de lait supplementaire ")
          choixValide = true
        } else {
          println("Choix du lait supplémentaire incorrect")
        }
      } while (!choixValide)
    }

    // Appel METHODE verifierStocks
    if(!verifierStocks(machineId, coffee, sugar, milk, coffeeStocks, sugarStocks, milkStocks)) {
      // stocks pas suffisants
      return false
    }

    // Stocks suffisants + mise à jour des stocks
    coffeeStocks(machineId) -= coffee
    sugarStocks(machineId) -= sugar
    milkStocks(machineId) -= milk

    // Afficher les choix
    if (choixBoisson == 1) {
      println("Boisson sélectionnée : Expresso ")
    } else if (choixBoisson == 2) {
      println("Boisson sélectionnée : Cappuccino ")
    } else if (choixBoisson == 3) {
      if (tailleLatte == 1) {
        println("Boisson sélectionnée : Latte Petit ")
      } else if(tailleLatte == 2) {
        println("Boisson sélectionnée : Latte Moyen ")
      } else if (tailleLatte == 3) {
        println("Boisson sélectionnée : Latte Grand ")
      }
    }
    // Print choix sucre
    if (choixSugar == 1) {
      println("Niveau de sucre : Sans sucre ")
    } else if (choixSugar == 2) {
      println("Niveau de sucre : Peu (5g) ")
    } else if (choixSugar == 3) {
      println("Niveau de sucre : Moyen (10g) ")
    } else if (choixSugar == 4) {
      println("Niveau de sucre : Beaucoup (15g) ")
    }

    // Variables prix
    val expresso = 2.00
    val cappuccino = 2.50
    val lattePetit = 2.70
    val latteMoyen = 3.20
    val latteGrand = 3.70

    // Prix Sucre
    val peuSucre = 0.10
    val moyenSucre = 0.200
    val bcpSucre = 0.30
    var prixTotal = 0.00

    // Prix total expresso
    if (choixBoisson == 1 && choixSugar == 1) {
      printf("Prix total : CHF %.2f \n ", expresso)
    } else if (choixBoisson == 1 && choixSugar == 2) {
      prixTotal = expresso + peuSucre
      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", expresso, peuSucre, prixTotal)
    } else if (choixBoisson == 1 && choixSugar == 3) {
      prixTotal = expresso + moyenSucre
      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, moyenSucre, prixTotal)
    } else if (choixBoisson == 1 && choixSugar == 4) {
      prixTotal = expresso + bcpSucre
      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, bcpSucre, prixTotal)
    }

    // Prix total cappuccino, sans sucre
    if (choixBoisson == 2 && choixSugar == 1) {
      // pas de lait supplementaire
      if ( supplementMilk == 2) {
        printf("Prix total : CHF %.2f \n", cappuccino)
      } // lait supplementaire
      else if (supplementMilk == 1) {
        prixTotal = cappuccino + (doseMilk * 0.05)
        printf("Prix total : CHF %.2f + CHF\n", cappuccino, prixTotal)
      }
    }
    // peu sucre
    else if (choixBoisson == 2 && choixSugar == 2) {
      if (supplementMilk == 2) {
        prixTotal = cappuccino + peuSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", cappuccino, peuSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = cappuccino + peuSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", cappuccino, peuSucre, prixMilk, prixTotal)
      }
    }
    // moyen sucre
    else if (choixBoisson == 2 && choixSugar == 3) {
      if (supplementMilk == 2) {
        prixTotal = cappuccino + moyenSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", expresso, moyenSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = cappuccino + moyenSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", cappuccino, moyenSucre, prixMilk, prixTotal)
      }
    }
    // beaucoup sucre
    else if (choixBoisson == 2 && choixSugar == 4) {
      if (supplementMilk == 2) {
        prixTotal = cappuccino + bcpSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", cappuccino, bcpSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = cappuccino + bcpSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", cappuccino, bcpSucre, prixMilk, prixTotal)
      }
    }

    // Prix total latte
    // Latte Petit
    if (choixBoisson == 3 && tailleLatte == 1 && choixSugar == 1) {
      if (supplementMilk == 2) {
        printf("Prix total : CHF %.2f \n", lattePetit)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = lattePetit + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", lattePetit, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 1 && choixSugar == 2) {
      if (supplementMilk == 2) {
        prixTotal = lattePetit + peuSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", lattePetit, peuSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = lattePetit + peuSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", lattePetit, peuSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 1 && choixSugar == 3) {
      if (supplementMilk == 2) {
        prixTotal = lattePetit + moyenSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", lattePetit, moyenSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = lattePetit + moyenSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", lattePetit, moyenSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 1 && choixSugar == 4) {
      if (supplementMilk == 2) {
        prixTotal = lattePetit + bcpSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", lattePetit, bcpSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = lattePetit + bcpSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", lattePetit, bcpSucre, prixMilk, prixTotal)
      }
    }
    // Latte Moyen
    if (choixBoisson == 3 && tailleLatte == 2 && choixSugar == 1) {
      if (supplementMilk == 2) {
        printf("Prix total : CHF %.2f \n", latteMoyen)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteMoyen + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", latteMoyen, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 2 && choixSugar == 2) {
      if (supplementMilk == 2) {
        prixTotal = latteMoyen + peuSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteMoyen, peuSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteMoyen + peuSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteMoyen, peuSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 2 && choixSugar == 3) {
      if (supplementMilk == 2) {
        prixTotal = latteMoyen + moyenSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteMoyen, moyenSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteMoyen + moyenSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteMoyen, moyenSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 2 && choixSugar == 4) {
      if (supplementMilk == 2) {
        prixTotal = latteMoyen + bcpSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteMoyen, bcpSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteMoyen + bcpSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteMoyen, bcpSucre, prixMilk, prixTotal)
      }
    }

    // Latte Grand
    if (choixBoisson == 3 && tailleLatte == 3 && choixSugar == 1) {
      if (supplementMilk == 2) {
        printf("Prix total : CHF %.2f \n", latteGrand)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteGrand + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", latteGrand, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 3 && choixSugar == 2) {
      if (supplementMilk == 2) {
        prixTotal = latteGrand + peuSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteGrand, peuSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteGrand + peuSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteGrand, peuSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 3 && choixSugar == 3) {
      if (supplementMilk == 2) {
        prixTotal = latteGrand + moyenSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteGrand, moyenSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteGrand + moyenSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteGrand, moyenSucre, prixMilk, prixTotal)
      }
    } else if (choixBoisson == 3 && tailleLatte == 3 && choixSugar == 4) {
      if (supplementMilk == 2) {
        prixTotal = latteGrand + bcpSucre
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f%n \n", latteGrand, bcpSucre, prixTotal)
      } else if (supplementMilk == 1) {
        val prixMilk = doseMilk * 0.05
        prixTotal = latteGrand + bcpSucre + prixMilk
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", latteGrand, bcpSucre, prixMilk, prixTotal)
      }
    }

    interfacePaiement()

    return true
  }

  // Méthode resctockMachine
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels : ")
    afficherTableaux(machineId, coffeeStocks, sugarStocks, milkStocks)
    println("Entrez les quantités à ajouter : ")
    print("Poudre de café > ")
    var ajoutCoffee = readInt()
    print("Sucre > ")
    var ajoutSugar = readInt()
    print("Lait > ")
    var ajoutMilk = readInt()

    // Restock machines, mise à jour
    coffeeStocks(machineId) += ajoutCoffee
    sugarStocks(machineId) += ajoutSugar
    milkStocks(machineId) += ajoutMilk

    println("Les stocks ont été mis à jour avec succès. ")
    println("Retour au menu principal ...")
    println()
  }

  // AUTRES METHODES :
  // methode pour afficher les tableaux
  def afficherTableaux(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Stock de la machine : " + (machineId + 1))
    println("Café : " + coffeeStocks(machineId) + " g")
    println("Sucre : " + sugarStocks(machineId) + " g")
    println("Lait : " + (milkStocks(machineId)/1000.0) + " L" )
  }

  // méthode pour vérifier les stocks
  def verifierStocks(machineId: Int, coffee: Int, sugar: Int, milk: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    if (coffeeStocks(machineId) >= coffee && sugarStocks(machineId) >= sugar && milkStocks(machineId) >= milk) {
      true
    } else {
      if (coffeeStocks(machineId) < coffee) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        println()
      } else if (sugarStocks(machineId) < sugar) {
        println("Erreur : Quantité de poudre de sucre insuffisante pour préparer la boisson sélectionnée. ")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        println()
      } else if (milkStocks(machineId) < milk) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        println()
      }
      false
    }
  }

  // méthode pour l'interface de paiement
  def interfacePaiement(): Unit = {
    var code = Random.alphanumeric.take(5).mkString.toUpperCase
    Thread.sleep(1000)
    println("Veuillez payer en utilisant Twint. ")
    println("Votre code de paiement est : " + code)
    println("(En attente de paiement ...) ")
    Thread.sleep(3000)
    println("Paiement confirmé")
    println()
  }

  def main(args: Array[String]): Unit = {

    // Variables
    val nbMachines = 5
    var machineId = 0 // = Aucune machine sélectionnée

    // Tableau codes PIN machines
    val machinePins = Array("434343", "434343", "434343", "434343", "434343")

    // Tableaus stocks initiaux machines
    var coffeeStocks = Array(50, 50, 50, 50, 50)
    var sugarStocks = Array(30, 30, 30, 30, 30)
    var milkStocks = Array(500, 500, 500, 500, 500)

    // Début boucle
    var continuer = true
    while (continuer) {
      println("Nospresso Café ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client ")
      println("2) Admin ")
      println("3) Quitter ")
      print("> ")

      val choix = readLine().toInt

      // Entrée choix invalide
      if (choix != 1 && choix != 2 && choix != 3) {
        println("Veuillez sélectionner un mode : 1) Client, 2) Admin ou 3) Quitter ")
      }

      // Entrée choix valide
      else if (choix == 1 || choix == 2) {

        // Choix Machine :
        do{
          println("Veuillez sélectionner une machine (1-5) : ")
          print("> ")
          machineId = readLine().toInt - 1
          if (machineId < 0 || machineId > nbMachines){
            println("Choix de machine incorrect. ")
          }
        } while (machineId < 0 || machineId >= nbMachines)

        // Appel METHODE validatePin
        if (machineId >= 0 && machineId < nbMachines) {
          if (validatePin(machineId, machinePins)) {
            var machineSelectionne = true
          } else {
            println("Trop de tentatives échouées. Fin du programme.")
            continuer = false
          }
        }

        // Choix Client
        if (choix == 1) {
          /*afficherTableaux(machineId, coffeeStocks, sugarStocks, milkStocks)*/// pour contrôler les stocks
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        }

        // Choix Admin
        else if (choix == 2) {
          var choixAdmin = 0
          do {
            println("1) Mettre à jour le code PIN ")
            println("2) Réapprovisionner les ingrédients ")
            print("> ")

            choixAdmin = readLine().toInt

            if (choixAdmin < 1 || choixAdmin > 2) {
              println("Veuillez sélectionner : 1) Mettre à jour le code PIN ou 2) Réapprovisionner les ingrédients.")
              println("> ")
            }
          } while (choixAdmin < 1 || choixAdmin > 2)

          // Choix mise à jour PIN
          if (choixAdmin == 1) {
            updatePin(machineId, machinePins)
          }
          // Choix réapprovisionnement
          else {
            if (choixAdmin == 2) {
              // Appel Methode restockMachine
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
              /*// Appel Methode afficherTableaux
              afficherTableaux(machineId, coffeeStocks, sugarStocks, milkStocks)*/ // pour contrôler que le stock est ajouté correctement
            }
          }
        }
      }

      // Choix Quitter
      if (choix == 3) {
        continuer = false
        println("Fin du programme.")
      }
    }

  }
}