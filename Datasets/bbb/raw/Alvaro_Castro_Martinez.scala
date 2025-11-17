import io.StdIn._
import scala.util.Random
object Main {
  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50) // en grammes
  val sugarStocks = Array.fill(nbMachines)(30) // en grammes
  val milkStocks = Array.fill(nbMachines)(500) // en millilitres


  def main(args: Array[String]): Unit = {

    var continuer = true

    while (continuer) {
      println("Bienvenue chez Nospresso!")
      println("Veuillez sélectionner une machine (1 à 5)")
      val machineId = demanderOptionValide((1 until nbMachines + 1).map(_.toString).toSet).toInt - 1

      var modeContinuer = true

        while (modeContinuer) {
          println("")
          println("")
          println("Nospresso Café")
          println("veuillez sélectioner votre mode: ")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter le programme")
          val choix = demanderOptionValide(Set("1", "2", "3"))

          if (choix == "3") {
            continuer = false
            modeContinuer = false
            println("vous quittez le programme.")
          }

          if (choix == "1") {
            if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
              println("Transaction échouée pour la machine." + machineId)
            }
            modeContinuer = false
          }
          if (choix == "2") {
            if (validatePin(machineId, machinePins)) {
              println("Accès Administrateur validé.")
              println("1) Réapprovisionner la machine")
              println("2) Mettre à jour le code PIN")
              val action = demanderOptionValide(Set("1", "2"))

              if (action == "1") {
                afficherStocks(machineId, coffeeStocks, sugarStocks, milkStocks)
                restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
              } else if (action == "2") {
                updatePin(machineId, machinePins)

              }
            } else {
              println("Code PIN invalide après 3 tentatives. Le programme se termine.")
            }

              modeContinuer = false


            }
          }
        }
      }


    def afficherStocks(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Stock actuel pour la machine" + machineId + ":")
      println("- Café :" + {
        coffeeStocks(machineId)
      } + "g")
      println("- Sucre :" + {
        sugarStocks(machineId)
      } + "g")
      println("- Lait :" + {
        milkStocks(machineId)
      } + "ml")
    }

    def demanderOptionValide(optionsValides: Set[String]): String = {
      var saisie = ""
      do {
        saisie = readLine(">")
        if (!optionsValides.contains(saisie)) {
          println("Option invalide. Veuillez réessayer.")
        }
      } while (!optionsValides.contains(saisie))
      saisie
    }

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var tentatives = 3
      while (tentatives > 0) {
        println("Veuillez entrer le code PIN pour la machine " + machineId + " (il vous reste " + tentatives + " tentative(s)) :")
        val pin = readLine()
        if (pin == machinePins(machineId)) {
          return true
        }
        tentatives -= 1
      }
      false
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var valide = false
      while (!valide) {
        println("Entrez le nouveau code PIN (6 chiffres) :")
        val nouveauPin = readLine()
        if (nouveauPin.matches("\\d{6}")) {
          machinePins(machineId) = nouveauPin
          println("Le code PIN pour la machine " + machineId + " a été mis à jour.")
          valide = true
        } else {
          println("Code PIN invalide. Il doit comporter exactement 6 chiffres.")
        }
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      println("Veuillez sélectionner une boisson :")
      println("Veuillez sélectionner une boisson : ")
      println("1) Expresso - CHF 2.00")
      println("2) Capuccino - CHF 2.50")
      println("3) Latte Petit - CHF 2.70")
      println("4) Latte Moyen - CHF 3.20")
      println("5) Latte Grand - CHF 3.70")
      val boisson = demanderOptionValide(Set("1", "2", "3", "4", "5"))
      var cafeNec = 0
      var laitNec = 0
      var prix = 0.0

      if (boisson == "1") {
        cafeNec = 8
        laitNec = 0
        prix = 2.0
      } else if (boisson == "2") {
        cafeNec = 6
        laitNec = 100
        prix = 2.5
      } else if (boisson == "3") {
        cafeNec = 6
        laitNec = 120
        prix = 2.7
      } else if (boisson == "4") {
        cafeNec = 8
        laitNec = 150
        prix = 3.2
      } else if (boisson == "5") {
        cafeNec = 12
        laitNec = 200
        prix = 3.7

      }
      if (boisson == "1")
        println("")
      if (boisson == "2" || boisson == "3" || boisson == "4" || boisson == "5") {
        println("Souhaitez-vous ajouter du lait en supplément ? ")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")

        val laitSup = demanderOptionValide(Set("1", "2"))
        if (laitSup == "1") {
          println("Combien de dose ? ")
          println("1) 1 dose (50ml)")
          println("2) 2 doses(100ml)")
          println("3) 3 doses (150ml)")
          val lait = demanderOptionValide(Set("1", "2", "3"))

          if (lait == "1") {
            laitNec += 50
            prix += 0.05
          } else if (lait == "2") {
            laitNec += 100
            prix += 0.1
          } else if (lait == "3") {
            laitNec += 150
            prix += 0.15
          }
        }
      }

      println("Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      val sucre = demanderOptionValide(Set("1", "2", "3", "4"))

      var sucreNec = 0
      var prixSucre = 0.0

      if (sucre == "1") {
        sucreNec = 0
        prixSucre = 0.0
      } else if (sucre == "2") {
        sucreNec = 5
        prixSucre = 0.1
      } else if (sucre == "3") {
        sucreNec = 10
        prixSucre = 0.2
      } else if (sucre == "4") {
        sucreNec = 15
        prixSucre = 0.3
      }

      val total = prix + prixSucre

      if (coffeeStocks(machineId) < cafeNec) {
        println("Quantité de café insuffisante.")
        return false
      } else if (sugarStocks(machineId) < sucreNec) {
        println("Quantité de sucre insuffisante.")
        return false
      } else if (milkStocks(machineId) < laitNec) {
        println("Quantité de lait insuffisante.")
        return false
      }

      coffeeStocks(machineId) -= cafeNec
      sugarStocks(machineId) -= sucreNec
      milkStocks(machineId) -= laitNec

      println("Veuillez payer CHF." + total + " via Twint.")
      val codeTwint = Random.alphanumeric.take(5).mkString
      println("Code de paiement : " + codeTwint + " (Attente de validation...)")
      Thread.sleep(2000)
      println("Paiement accepté. Préparation de votre boisson...")
      Thread.sleep(2000)
      println("Votre boisson est prête. Bonne dégustation!")
      true
    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Réapprovisionnement pour la machine " + machineId + "")
      println("Ajout de café (grammes) :")
      coffeeStocks(machineId) += readLine().toInt
      println("Ajout de sucre (grammes) :")
      sugarStocks(machineId) += readLine().toInt
      println("Ajout de lait (millilitres) :")
      milkStocks(machineId) += readLine().toInt
      println("Réapprovisionnement terminé.")
    }
  }




