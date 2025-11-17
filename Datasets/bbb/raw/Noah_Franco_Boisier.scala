import scala.io.StdIn.readLine
import scala.util.Random

object NospressoApp extends App {

// Déclaration des variables
  var cafeStocks = Array(50, 50, 50, 50, 50)
  var sucreStocks = Array(30, 30, 30, 30, 30)
  var laitStocks = Array(500, 500, 500, 500, 500)
  val nbMachines = 5
  var machineId = Array(1, 2, 3, 4, 5)
  var machinePins = Array("434343", "434343", "434343", "434343", "434343")
  var choixMachine = -1


  // Méthode validatePin
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      var Pin = readLine("Entrez le code PIN : \n> ").toInt
      if (Pin == machinePins(choixMachine).toInt) {
        println("Accès accordé à la Machine " + (choixMachine + 1) + ".")
        return true
      } else {
        tentatives -= 1
        if (tentatives > 0)
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
      }
      if (tentatives == 0) {
        println("Code PIN incorrect. 0 tentatives restantes.\n\nTrop de tentatives échouées. Fin du programme.")
        return false
      }
    }
    false
  }

  // Méthode updatePin
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    if (machinePins(choixMachine) == machinePins(choixMachine)) {
      println("Mise à jour du code PIN pour la machine " + machineId)
      machinePins(choixMachine) = readLine("Entrez un nouveau code Pin à 6 chiffres > ")
      while (machinePins(choixMachine).length != 6) {
        machinePins(choixMachine) = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...\n\n")
    } else {

    }
  }

  // Méthode serveClient
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    val expresso = 2.00
    val cappuccino = 2.50
    val latteP = 2.70
    val latteM = 3.20
    val latteG = 3.70
    var dose = 0
    var prix : Double = 0



    // Sélection de boissons

    println("Veuillez selectionner votre boisson : ")
    println("1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var choixBoisson = readLine("> ").toInt
    while (choixBoisson < 1 || choixBoisson > 3) choixBoisson = readLine("Veuillez selectionner 1,2 ou 3 : ").toInt


    // Choix du sucre

    println("Souhaitez-vous ajouer du sucre ?")
    println("1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    var sucre = readLine("> ").toInt
    while (sucre < 1 || sucre > 4) sucre = readLine("Veuillez selectionner 1, 2, 3 ou 4 : ").toInt
    if (sucre == 2) {
      sucreStocks(choixMachine) -= 5
      prix += 0.10
    }
    if (sucre == 3) {
      sucreStocks(choixMachine) -= 10
      prix += 0.20
    }
    if (sucre == 4) {
      sucreStocks(choixMachine) -= 15
      prix += 0.30
    }


    // Choix du lait

    if (choixBoisson == 2 || choixBoisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui \n2) Non")
      var lait = readLine("> ").toInt
      while (lait < 1 || lait > 2) lait = readLine("Veuillez selectionner 1 ou 2 : ").toInt
      if (lait == 1) {
        println("1) 1 dose (50ml) - CHF 0.05 \n2) 2 doses (100ml) - CHF 0.10 \n3) 3 doses (150ml) - CHF 0.20")
        dose = readLine("> ").toInt
        while (dose < 1 || dose > 3) dose = readLine("Veuillez selectionner 1, 2, ou 3 : ").toInt
        if (dose == 1) {
          laitStocks(choixMachine) -= 50
          prix += 0.05
        }
        if (dose == 2) {
          laitStocks(choixMachine) -= 100
          prix += 0.10
        }
        if (dose == 3) {
          laitStocks(choixMachine) -= 150
          prix += 0.15
        }
      }
    }


    // Expresso

    if (choixBoisson == 1) {
      if (cafeStocks(choixMachine) < 8) {
        println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
        if (sucre == 2) {
          sucreStocks(choixMachine) += 5
        }
        if (sucre == 3) {
          sucreStocks(choixMachine) += 10
        }
        if (sucre == 4) {
          sucreStocks(choixMachine) += 15
        }
        return false
      } else {
        prix += expresso
        cafeStocks(choixMachine) -= 8
      }
    }


    // Cappuccino

    if (choixBoisson == 2) {
      if (cafeStocks(choixMachine) < 6) {
        println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
        if (dose == 1) {
          laitStocks(choixMachine) += 50
        }
        if (dose == 2) {
          laitStocks(choixMachine) += 100
        }
        if (dose == 3) {
          laitStocks(choixMachine) += 150
        }
        if (sucre == 2) {
          sucreStocks(choixMachine) += 5
        }
        if (sucre == 3) {
          sucreStocks(choixMachine) += 10
        }
        if (sucre == 4) {
          sucreStocks(choixMachine) += 15
        }
        return false
      } else if (laitStocks(choixMachine) < 100) {
        println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
        if (dose == 1) {
          laitStocks(choixMachine) += 50
        }
        if (dose == 2) {
          laitStocks(choixMachine) += 100
        }
        if (dose == 3) {
          laitStocks(choixMachine) += 150
        }
        if (sucre == 2) {
          sucreStocks(choixMachine) += 5
        }
        if (sucre == 3) {
          sucreStocks(choixMachine) += 10
        }
        if (sucre == 4) {
          sucreStocks(choixMachine) += 15
        }
        return false
      } else {
        prix += cappuccino
        laitStocks(choixMachine) -= 100
        cafeStocks(choixMachine) -= 6
      }
    }


    // Latte

    if (choixBoisson == 3) {
      println("Choissisez la taille de votre boisson : ")
      println("1) Petit \n2) Moyen \n3) Grand")
      var taille = readLine("Choisir 1, 2 ou 3 : ").toInt
      while (taille < 1 || taille > 3) taille = readLine("Veuillez selectionner 1, 2, ou 3 : \n> ").toInt
      if (taille == 1) {
        if (cafeStocks(choixMachine) < 6) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          return false
        } else if (laitStocks(choixMachine) < 120) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          return false
        } else {
          cafeStocks(choixMachine) -= 6
          laitStocks(choixMachine) -= 120
          prix += latteP
        }
      }
      if (taille == 2) {
        if (cafeStocks(choixMachine) < 8) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          return false
        } else if (laitStocks(choixMachine) < 150) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          return false
        } else {
          cafeStocks(choixMachine) -= 8
          laitStocks(choixMachine) -= 150
          prix += latteM
        }
      }
      if (taille == 3) {
        if (cafeStocks(choixMachine) < 12) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          return false
        } else if (laitStocks(choixMachine) < 200) {
          println("Erreur : Stocks insuffisant. Veuillez sélectionner une autre Machine.")
          if (sucre == 2) {
            sucreStocks(choixMachine) += 5
          }
          if (sucre == 3) {
            sucreStocks(choixMachine) += 10
          }
          if (sucre == 4) {
            sucreStocks(choixMachine) += 15
          }
          if (dose == 1) {
            laitStocks(choixMachine) += 50
          }
          if (dose == 2) {
            laitStocks(choixMachine) += 100
          }
          if (dose == 3) {
            laitStocks(choixMachine) += 150
          }
          return false
        } else {
          cafeStocks(choixMachine) -= 12
          laitStocks(choixMachine) -= 200
          prix += latteG
        }
      }
    }



    // Paiement

    printf("Le montant de votre commande est de : %.2f CHF\n\n", prix
)
    println("Veuillez payer en utilisant Twint.")
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    val longueurcode = 5
    for (_ <- 1 to longueurcode) {
      code += caracteres(Random.nextInt(caracteres.length))
    }
    println("Votre code de paiement est : " + code)
    println("(En attente de validation du paiement)\n")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
    Thread.sleep(1500)
    println("Préparation de votre boisson...")
    println("[...]")
    if (choixBoisson == 1) {
      println("Votre Expresso est prêt ! Bonne dégustation !\n\n")
    }
    if (choixBoisson == 2) {
      println("Votre Cappuccino est prêt ! Bonne dégustation !\n\n")
    }
    if (choixBoisson == 3) {
      println("Votre Latte est prêt ! Bonne dégustation !\n\n")
    }
    Thread.sleep(1500)
    return true
  }

  // Méthode restockMachine
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stocks actuels :")
    println("Poudre de Café  : " + cafeStocks(choixMachine) + "g")
    println("Lait            : " + laitStocks(choixMachine) + "mL")
    println("Sucre           : " + sucreStocks(choixMachine) + "g")

    println("Entrez les quantités à ajouter : ")
    var ajoutcafe = readLine("Poudre de café > ").toInt
    var ajoutlait = readLine("Lait > ").toInt
    var ajoutsucre = readLine("Sucre > ").toInt
    cafeStocks(choixMachine) += ajoutcafe
    laitStocks(choixMachine) += ajoutlait
    sucreStocks(choixMachine) += ajoutsucre
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal... \n")
    Thread.sleep (1500)
  }

  // Début du programme
  var boucle = true
  while (boucle) {

    // Sélection du mode
    println("       Nospresso café              ")

    println("Veuiller choisir votre mode : ")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")
    var choix = readLine("> ").toInt
    while (choix < 1 || choix > 3) choix = readLine("Veuillez selectionner 1,2 ou 3 : \n> ").toInt

    // Mode client
    if (choix == 1) {
      choixMachine = readLine("Veuillez sélectionner une machine (1-5) : \n> ").toInt - 1
      while (choixMachine < 0 || choixMachine > 4) {
        choixMachine = readLine("Veuillez sélectionner un numéro valide : \n> ").toInt - 1
      }
      println("Vous avez sélectionné la Machine " + (choixMachine + 1))
      serveClient(machineId(choixMachine), cafeStocks, sucreStocks, laitStocks)
    }

    // Mode Admin
    if (choix == 2) {
      choixMachine = readLine("Veuillez sélectionner une machine (1-5) : \n> ").toInt - 1
      while (choixMachine < 0 || choixMachine > 4) {
        choixMachine = readLine("Veuillez sélectionner un numéro valide : \n> ").toInt - 1
      }
      if (!validatePin(machineId(choixMachine), machinePins)){
        boucle = false
      } else {
        println("1) Réapprovisionnement des stocks")
        println("2) Changement de code Pin")
        var choixadmin = readLine("Veuillez selectionner 1 ou 2 : \n> ").toInt
        while (choixadmin < 1 || choixadmin > 2) {
          choixadmin = readLine("Veuillez sélectionner un numéro valide : \n> ").toInt
        }
        // Restock de Machine
        if (choixadmin == 1) {
          restockMachine(machineId(choixMachine), cafeStocks, sucreStocks, laitStocks)
        }
        // Changement du Code PIN
        if (choixadmin == 2) {
          updatePin(machineId(choixMachine), machinePins)
        }
      }
    }

    // Quitter le programme
    if (choix == 3){
      println("Merci d'avoir utilisé Nospresso. À bientôt !")
      boucle = false
    }
  }
}