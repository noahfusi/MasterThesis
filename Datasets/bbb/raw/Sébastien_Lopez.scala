import scala.util.Random
import io.StdIn._

object Main {

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    if (coffeeStocks(machineId) < 0) {
      println
      println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
      println
    }
    /*else*/ if (milkStocks(machineId) < 0) {
      println
      println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      println
    }
    /*else*/ if (sugarStocks(machineId) < 0) {
      println
      println(" Erreur = Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une autre boisson, une quantité plus faible de sucre ou alors vérifier les stocks en mode Admin.")
      println
    }


    if ((coffeeStocks(machineId) < 0) || (sugarStocks(machineId) < 0) || (milkStocks(machineId) < 0)) {
      return false
    } else {
      return true
    }
    // déduit les quantités dans coffee(machineId), sugar et milk pareil
    // si stock insuffisant alors pas de transaction et programme demande de changer de machine
    // TRUE SI TRANSACTION + mise à jour des tableaux
    // FAlSE SI TRANSACTION échoue
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var nbTentatives: Int = 1
    var tentatives_restantes: Int = 2
    val code_Pin = machinePins(machineId)
    var code = readLine(" >")
    var Pin: Boolean = false
    while ((code != code_Pin) && (nbTentatives < 3)) {
      println(" Code Pin incorrect. " + tentatives_restantes + " tentatives restantes.")
      code = readLine(" >")
      nbTentatives += 1
      tentatives_restantes -= 1
      if (tentatives_restantes == 0) {
        Pin = false
      }
    }
    if (code == code_Pin) {
      Pin = true
    }
    return Pin
    // invite à saisir code Pin de la machine donnée
    // validation selon le code stocké dans MACHINEPINS
    // TRUE SI BON CODE
    // FALSE SI PAS BON CODE
  }


  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var NouveaucodePin = readLine(" >")

    while ((NouveaucodePin.length != 6) || (!NouveaucodePin.forall(_.isDigit))) {
      println(" Entrez un nouveau code Pin à 6 chiffres :")
      NouveaucodePin = readLine(" >")
    }

    machinePins(machineId) = NouveaucodePin

    // machine Id = machine du code qui doit être changé et machinePins = tableau contenant les codes
    // nouveau code si validé alors va CHANGER LE TABLEAU
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    var Ajout_Stock_PoudredeCafe = readLine(" Poudre de café > ").toInt
    while (Ajout_Stock_PoudredeCafe < 0) {
      Ajout_Stock_PoudredeCafe = readLine(" Poudre de café > ").toInt
    }

    var Ajout_Stock_Lait = (readLine(" Lait > ").toDouble * 1000).toInt
    while (Ajout_Stock_Lait < 0) {
      Ajout_Stock_Lait = (readLine(" Lait > ").toDouble * 1000).toInt
    }

    var Ajout_Stock_Sucre = readLine(" Sucre > ").toInt
    while (Ajout_Stock_Sucre < 0) {
      Ajout_Stock_Sucre = readLine(" Sucre > ").toInt
    }

    coffeeStocks(machineId) = Ajout_Stock_PoudredeCafe + coffeeStocks(machineId)
    milkStocks(machineId) = Ajout_Stock_Lait + milkStocks(machineId)
    sugarStocks(machineId) = Ajout_Stock_Sucre + sugarStocks(machineId)

    // modifie directement le tableau des valeurs
    // VERIFIER LES ENTREES TJRS AVANT VALIDATION
  }


  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val machinePins: Array[String] = Array.fill(nbMachines)("434343")
    val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50)
    val sugarStocks: Array[Int] = Array.fill(nbMachines)(30)
    val milkStocks: Array[Int] = Array.fill(nbMachines)(500)
    var machineId: Int = 0

    var Machine: Int = 0
    var mode: Int = 0
    var boisson: Int = 0
    var sucre: Int = 0
    var Latte: Int = 0
    var lait: Int = 0
    var dose: Int = 0

    var prix_lait: Double = 0.00
    var prix_sucre: Double = 0.00
    var prix_Latte: Double = 0.00
    var prix_Expresso: Double = 2.00
    var prix_Cappuccino: Double = 2.50

    var boisson_selectionnee: String = "."
    var niveaudesucre: String = "."
    var laitsupp: String = "."

    var deductionStock_lait: Int = 0
    var deductionStock_sucre: Int = 0
    var deductionStock_laitsupp: Int = 0
    var deductionStock_cafe: Int = 0

    var Stock_valide : Boolean = true
    val s = 17



    do {
      println("         Nospresso Café")
      println(" Veuilez sélectionner votre mode :")
      println(" 1) Client")
      println(" 2) Admin")
      println(" 3) Quitter")
      mode = readLine(" >").toInt
      println
      while ((mode < 1) || (mode > 3)) {
        if ((mode < 1) || (mode > 3)) {
          println(" Veuilez sélectionner votre mode :")
          println(" 1) Client")
          println(" 2) Admin")
          println(" 3) Quitter")
          mode = readLine(" >").toInt
          println
        }
      }

      do {
        if ((mode == 1) || (mode == 2)) {
          println(" Veuillez sélectionner la machine :")
          println(" 1) Machine n°1")
          println(" 2) Machine n°2")
          println(" 3) Machine n°3")
          println(" 4) Machine n°4")
          println(" 5) Machine n°5")
          Machine = readLine(" Machine sélectionnée (1-5) >").toInt
          println

          while ((Machine < 1) || (Machine > 5)) {
            if ((Machine < 1) || (Machine > 5)) {
              println(" Veuillez sélectionner la machine :")
              println(" 1) Machine n°1")
              println(" 2) Machine n°2")
              println(" 3) Machine n°3")
              println(" 4) Machine n°4")
              println(" 5) Machine n°5")
              Machine = readLine(" Machine sélectionnée (1-5) >").toInt
              println
            }
          }
        }

        machineId = Machine - 1

        if (mode == 1) {
          println(" Veuillez sélectionner votre boissson :")
          println(" 1)Expresso - CHF 2.00")
          println(" 2)Cappuccino - CHF 2.50")
          println(" 3)Latte - CHF 2.70(Petit), CHF 3.20(Moyen), CHF 3.70(Grand)")
          boisson = readLine(" >").toInt
          println

          while ((boisson < 1) || (boisson > 3)) {
            if ((boisson < 1) || (boisson > 3)) {
              println(" Veuillez sélectionner votre boissson :")
              println(" 1)Expresso - CHF 2.00")
              println(" 2)Cappuccino - CHF 2.50")
              println(" 3)Latte - CHF 2.70(Petit), CHF 3.20(Moyen), CHF 3.70(Grand)")
              boisson = readLine(" >").toInt
              println
            }
          }


          if (boisson == 3) {
            println("Veuillez sélectionner votre Latte :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            Latte = readLine(" >").toInt
            println

            while ((Latte < 1) || (Latte > 3)) {
              println("Veuillez sélectionner votre Latte :")
              println("1) Petit - CHF 2.70")
              println("2) Moyen - CHF 3.20")
              println("3) Grand - CHF 3.70")
              Latte = readLine(" >").toInt
              println
            }
            if (Latte == 1) {
              prix_Latte = 2.70
            }
            if (Latte == 2) {
              prix_Latte = 3.20
            }
            if (Latte == 3) {
              prix_Latte = 3.70
            }
          }


          println(" Souhaitez-vous ajouter du sucre ?")
          println(" 1) Sans sucre")
          println(" 2) Peu (5g) - CHF 0.10")
          println(" 3) Moyen (10g) - CHF 0.20")
          println(" 4) Beaucoup (15g) - CHF 0.30")
          sucre = readLine(" >").toInt
          println

          while ((sucre < 1) || (sucre > 4)) {
            println(" Souhaitez-vous ajouter du sucre ?")
            println(" 1) Sans sucre")
            println(" 2) Peu (5g) - CHF 0.10")
            println(" 3) Moyen (10g) - CHF 0.20")
            println(" 4) Beaucoup (15g) - CHF 0.30")
            sucre = readLine(" >").toInt
            println
          }
          if (sucre == 1) {
            prix_sucre = 0.00
          }
          if (sucre == 2) {
            prix_sucre = 0.10
          }
          if (sucre == 3) {
            prix_sucre = 0.20
          }
          if (sucre == 4) {
            prix_sucre = 0.30
          }


          if ((boisson == 2) || (boisson == 3)) {

            println(" Souhaitez-vous ajouter du lait en supplément ?")
            println(" (Disponible uniquement pour Cappuccino et Latte)")
            println(" 1) Oui")
            println(" 2) Non")
            lait = readLine(" >").toInt
            println

            while ((lait < 1) || (lait > 2)) {
              println(" Souhaitez-vous ajouter du lait en supplément ?")
              println(" (Disponible uniquement pour Cappuccino et Latte)")
              println(" 1) Oui")
              println(" 2) Non")
              lait = readLine(" >").toInt
              println
            }


            if (lait == 1) {
              println(" Combien de dose ? (entre 1 et 3)")
              dose = readLine(" >").toInt
              println

              while ((dose < 1) || (dose > 3)) {
                println(" Combien de dose ? (entre 1 et 3)")
                dose = readLine(" >").toInt
                println
              }
              if (dose == 1) {
                prix_lait = 0.05
              }
              if (dose == 2) {
                prix_lait = 0.10
              }
              if (dose == 3) {
                prix_lait = 0.15
              }
            }

            if (lait == 2) {
              dose = 0
              if (dose == 0) {
                prix_lait = 0.00
              }
            }
          }


          if (boisson == 1) {
            boisson_selectionnee = "Expresso"
          }
          if (boisson == 2) {
            boisson_selectionnee = "Cappuccino"
          }
          if (boisson == 3) {
            if (Latte == 1) {
              boisson_selectionnee = "Latte (Petit)"
            }
            if (Latte == 2) {
              boisson_selectionnee = "Latte (Moyen)"
            }
            if (Latte == 3) {
              boisson_selectionnee = "Latte (Grand)"
            }
          }

          if (sucre == 1) {
            niveaudesucre = "Sans sucre"
          }
          if (sucre == 2) {
            niveaudesucre = "Peu (5g)"
          }
          if (sucre == 3) {
            niveaudesucre = "Moyen (10g)"
          }
          if (sucre == 4) {
            niveaudesucre = "Beaucoup (15g)"
          }

          if (lait == 1) {
            laitsupp = "Oui"
          }
          if (lait == 2) {
            laitsupp = "Non"
          }


          println(" Boisson sélectionnée : " + boisson_selectionnee)
          println(" Niveau de sucre : " + niveaudesucre)
          if ((boisson == 2) || (boisson == 3)) {
            println(" Lait supplémentaire : " + laitsupp)
          }

          if (boisson == 1) {
            deductionStock_cafe = 8
            deductionStock_lait = 0
          }

          if (boisson == 2) {
            deductionStock_cafe = 6
            deductionStock_lait = 100
          }

          if (boisson == 3) {
            if (Latte == 1) {
              deductionStock_cafe = 6
              deductionStock_lait = 120
            }
            if (Latte == 2) {
              deductionStock_cafe = 8
              deductionStock_lait = 150

            }
            if (Latte == 3) {
              deductionStock_cafe = 12
              deductionStock_lait = 200
            }
          }

          if ((boisson == 1) || (boisson == 2) || (boisson == 3)) {
            if (niveaudesucre == "Sans sucre") {
              deductionStock_sucre = 0
            }
            if (niveaudesucre == "Peu (5g)") {
              deductionStock_sucre = 5
            }
            if (niveaudesucre == "Moyen (10g)") {
              deductionStock_sucre = 10
            }
            if (niveaudesucre == "Beaucoup (15g)") {
              deductionStock_sucre = 15
            }
          }

          if (boisson == 1) {
            deductionStock_laitsupp = 0
          }
          if ((boisson == 2) || (boisson == 3)) {
            if (lait == 2) {
              deductionStock_laitsupp = 0
            }
            if (lait == 1) {
              if (dose == 1) {
                deductionStock_laitsupp = 50
              }
              if (dose == 2) {
                deductionStock_laitsupp = 100
              }
              if (dose == 3) {
                deductionStock_laitsupp = 150
              }
            }
          }

          coffeeStocks(machineId) = coffeeStocks(machineId) - deductionStock_cafe
          milkStocks(machineId) = milkStocks(machineId) - deductionStock_lait - deductionStock_laitsupp
          sugarStocks(machineId) = sugarStocks(machineId) - deductionStock_sucre


          Stock_valide = serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int])

          if (Stock_valide == true) {
            (coffeeStocks(machineId) >= 0) || (milkStocks(machineId) >= 0) || (sugarStocks(machineId) >= 0)
          } else {
            (coffeeStocks(machineId) < 0) || (milkStocks(machineId) < 0) || (sugarStocks(machineId) < 0)
            coffeeStocks(machineId) = coffeeStocks(machineId) + deductionStock_cafe
            milkStocks(machineId) = milkStocks(machineId) + deductionStock_lait + deductionStock_laitsupp
            sugarStocks(machineId) = sugarStocks(machineId) + deductionStock_sucre
          }


          if (Stock_valide == true) {

            if (boisson == 1) {
              prix_Expresso = prix_Expresso + prix_sucre
            }
            if (boisson == 2) {
              prix_Cappuccino = prix_Cappuccino + prix_sucre + prix_lait
            }
            if (boisson == 3) {
              prix_Latte = prix_Latte + prix_sucre + prix_lait
            }


            if (boisson == 1) {
              if (sucre == 1) {
                printf(" Prix total : CHF 2.00 + CHF 0.00 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 2) {
                printf(" Prix total : CHF 2.00 + CHF 0.10 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 3) {
                printf(" Prix total : CHF 2.00 + CHF 0.20 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 4) {
                printf(" Prix total : CHF 2.00 + CHF 0.30 = CHF %.2f \n", prix_Expresso)
              }
            }
            if (boisson == 2) {
              if ((sucre == 1) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
            }
            if (boisson == 3) {
              if (Latte == 1) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
              if (Latte == 2) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }

              }
              if (Latte == 3) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
            }

            println

            if (boisson == 1) {
              prix_Expresso = prix_Expresso - prix_sucre
            }
            if (boisson == 2) {
              prix_Cappuccino = prix_Cappuccino - prix_sucre
              prix_Cappuccino = prix_Cappuccino - prix_lait
            }
            if (boisson == 3) {
              prix_Latte = prix_Latte - prix_sucre
              prix_Latte = prix_Latte - prix_lait
            }


            val code_paiement = Random.alphanumeric.take(5).mkString
            println(" Veuillez payer en utilisant Twint.")
            println(" Votre code de paiement est : " + code_paiement)
            println(" (En attente de validation du paiement...)")
            Thread.sleep(3000)
            println
            println(" Merci! Votre paiement a été accepté.")
            println(" Préparation de votre boisson... ")
            println(" Votre " + boisson_selectionnee + " est prêt ! Bonne dégustation !")
            println
            println

          }
        }


        if (mode == 2) {
          println(" Mode Admin")
          println(" Entrez le code Pin :")

          val OuvertureAdmin = validatePin(machineId, machinePins)
          if (OuvertureAdmin == true) {

            println(" Accès accordé à la Machine " + Machine + ".")
            println
            println(" Choississez une de ces deux option : ")
            println(" Option n°1 = Changez le code administrateur de la machine ")
            println(" Option n°2 = Visualiser les stocks + possibilité de les modifier en rajoutant des ingrédients")
            var choix = readLine(" >").toInt

            while ((choix < 1) || (choix > 2)) {
              println(" Choississez une de ces deux option : ")
              println(" Option n°1 = Changez le code administrateur de la machine ")
              println(" Option n°2 = Visualiser les stocks + possibilité de les modifier en rajoutant des ingrédients")
              choix = readLine(" >").toInt
            }

            if (choix == 1) {
              println
              println(" Mise à jour du code Pin pour la machine " + Machine)
              println(" Entrez un nouveau code Pin à 6 chiffres :")
              updatePin(machineId, machinePins)
              println
              println(" Le code PIN a été mis à jour avec succès.")
              println(" Retour au menu principal...")
              println
            }

            if (choix == 2) {
              println(" Niveaux de stocks actuels :")
              println("    Poudre de café : " + coffeeStocks(machineId) + "g")
              printf("    Lait : %.2fL \n", (milkStocks(machineId) / 1000.0))
              println("    Sucre : " + sugarStocks(machineId) + "g")
              println
              println(" Entrez les quantités à ajouter :")
              restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int])
              println
              println(" Les stock ont été mis à jour avec succès.")
              println(" Retour au menu principal...")
              println
            }

          } else {
            println(" Code Pin incorrect. 0 tentatives restantes.")
            println
            println(" Trop de tentatives échouées. Fin du programme.")
            println(" Retour au menu principal...")
            println
          }
        }

        if (mode == 3) {
          println(" Vous avez quitté le programme Nospresso Café.")
        }

      } while (Stock_valide == false)

    } while ((s != 6) && (mode != 3))

  }
}
