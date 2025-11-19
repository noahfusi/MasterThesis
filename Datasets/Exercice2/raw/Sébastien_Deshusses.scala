import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit ={
    val nbMachines = 5

    // Code PIN mode Admin toutes les machines
    var machinePins = Array.fill(nbMachines)("434343")

    // Stocks initiaux ingrédients toutes les machines
    var coffeeStocks = Array.fill(nbMachines)(30.0) // exprimé en grammes
    var sugarStocks = Array.fill(nbMachines)(15.0) // exprimé en grammes
    var milkStocks = Array.fill(nbMachines)(0.200) // exprimé en litres

    var continue = true

    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var attempts = 0
      val maxAttempts = 3
      while (attempts < maxAttempts) {
        println("Entrez le code PIN: ")
        val pin = readLine("> ")
        if (pin == machinePins(machineId)) {
          println("Accès accordé à la Machine " + (machineId + 1) + ".")
          return true
        } else {
          attempts += 1
          val remainingAttempts = maxAttempts - attempts
          println("Code PIN incorrect. " + remainingAttempts + " tentatives restantes.")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      false
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var pinUpdated = false
      var attempts = 0
      val maxAttempts = 3
      while (!pinUpdated && attempts < maxAttempts) {
        println("Entrez un nouveau code PIN à 6 chiffres: ")
        val newPin = readLine("> ")
        if (newPin.length == 6 && newPin.forall(_.isDigit)) {
          machinePins(machineId) = newPin
          println("Le code PIN a été mis à jour avec succès. Retour au menu principal...")
          pinUpdated = true
        } else {
          attempts += 1
          val remainingAttempts = maxAttempts - attempts
          println("Erreur: le code PIN doit comporter exactement 6 chiffres. " + remainingAttempts + " tentatives restantes.")
        }
      }
      if (!pinUpdated) {
        println("Trop de tentatives échouées. Fin du programme.")
        continue = false
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Boolean = {
      var newChoixBoisson = true

      while (newChoixBoisson) {
        println("Veuillez sélectionner votre boisson: ")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte petit - CHF 2.70")
        println("4) Latte moyen - CHF 3.20")
        println("5) Latte grand - CHF 3.70")

        val boissonCommandee = readLine("> Indiquez le numéro correspondant à votre boisson (1-5): ").toInt

        var prixBoisson = 0.0
        var nomBoisson = ""
        var cafeBase = 0.0
        var laitBase = 0.0
        var sucreBase = 0.0

        if (boissonCommandee == 1) {
          prixBoisson = 2.00
          nomBoisson = "Expresso"
          cafeBase = 8.0
        } else if (boissonCommandee == 2) {
          prixBoisson = 2.50
          nomBoisson = "Cappuccino"
          cafeBase = 6.0
          laitBase = 0.1
        } else if (boissonCommandee == 3) {
          prixBoisson = 2.70
          nomBoisson = "Latte petit"
          cafeBase = 6.0
          laitBase = 0.12
        } else if (boissonCommandee == 4) {
          prixBoisson = 3.20
          nomBoisson = "Latte moyen"
          cafeBase = 8.0
          laitBase = 0.15
        } else if (boissonCommandee == 5) {
          prixBoisson = 3.70
          nomBoisson = "Latte grand"
          cafeBase = 12.0
          laitBase = 0.20
        } else {
          println("Erreur: choix de boisson invalide.")
          newChoixBoisson = true
          return false
        }

        if (coffeeStocks(machineId) < cafeBase) {
          println("Erreur: quantité de café insuffisante.")
          newChoixBoisson = true
        } else if (milkStocks(machineId) < laitBase) {
          println("Erreur: quantité de lait insuffisante.")
          newChoixBoisson = true
        } else {
          newChoixBoisson = false
          println("Vous avez choisi un " + nomBoisson + " - CHF " + prixBoisson)

          var choixSucreValide = false
          var choixSucre = 0
          while (!choixSucreValide) {
            println("Souhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")

            choixSucre = readLine("> Indiquez votre choix de sucre avec 1, 2, 3, 4: ").toInt

            if (choixSucre >= 1 && choixSucre <= 4) {
              choixSucreValide = true
            } else {
              println("Erreur: veuillez entrer une valeur comprise entre 1 et 4.")
            }
          }

          if (choixSucre == 1) {
            sucreBase = 0.0
          } else if (choixSucre == 2) {
            sucreBase = 5.0
            prixBoisson += 0.10
          } else if (choixSucre == 3) {
            sucreBase = 10.0
            prixBoisson += 0.20
          } else if (choixSucre == 4) {
            sucreBase = 15.0
            prixBoisson += 0.30
          }

          if (sugarStocks(machineId) >= sucreBase) {
            if (nomBoisson.contains("Latte") || nomBoisson.contains("Cappuccino")) {
              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("1) Oui")
              println("2) Non")

              val choixLait = readLine("> Indiquez si vous souhaitez ajouter du lait avec 1 ou 2: ").toInt
              if (choixLait == 1) {
                var saisieUser = false
                var quantiteLait = 0
                while (!saisieUser) {
                  println("Vous pouvez ajouter 1, 2 ou 3 doses de lait supplémentaire. 1 dose coûte CHF 0.05.")
                  quantiteLait = readLine("> Indiquez combien de doses supplémentaires vous souhaitez ajouter (1-3): ").toInt
                  if (quantiteLait >= 1 && quantiteLait <= 3) {
                    saisieUser = true
                  } else {
                    println("Erreur: veuillez entrer une valeur valide (1, 2 ou 3).")
                  }
                }
                val doseLait = quantiteLait * 0.05
                prixBoisson += doseLait

                val laitBaseplusDose = laitBase + doseLait
                if (milkStocks(machineId) >= laitBaseplusDose) {
                  milkStocks(machineId) -= laitBaseplusDose
                } else {
                  println("Erreur: quantité de lait insuffisante pour ajouter la dose supplémentaire.")
                  newChoixBoisson = true
                }
              } else {
                milkStocks(machineId) -= laitBase
              }
            } else {
              milkStocks(machineId) -= laitBase
            }

            if (!newChoixBoisson) {
              println("Le prix total de votre " + nomBoisson + " est : CHF " + prixBoisson)
              println("Veuillez payer le montant de CHF " + prixBoisson + " avec Twint en saisissant le code de paiement suivant: ")
              val codeTwint = Random.alphanumeric.filter(_.isLetterOrDigit).take(5).mkString.toUpperCase
              println(codeTwint)

              println("En attente de validation du paiement...")
              Thread.sleep(3000)

              println("Merci ! Votre paiement a été accepté.")
              println("Préparation de votre boisson...")
              Thread.sleep(5000)
              println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

              coffeeStocks(machineId) -= cafeBase
              sugarStocks(machineId) -= sucreBase
              return true
            }
          } else {
            println("Erreur: quantité de sucre insuffisante.")
            newChoixBoisson = true
          }
        }
      }
      false
    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Unit = {
      println("Stocks actuels de la Machine " + (machineId + 1) + ":")
      println("Stock de poudre de café: " + coffeeStocks(machineId) + " gm")
      println("Stock de sucre: " + sugarStocks(machineId) + " gm")
      println("Stock de lait: " + milkStocks(machineId) + " litre")

      println("Voulez-vous réapprovisionner les stocks ?")
      println("1) Oui")
      println("2) Non")
      val choixMajStock = readLine("> Votre choix: ").toInt

      if (choixMajStock == 1) {
        var saisieUser = false
        while (!saisieUser) {
          println("Ajout poudre de café en gramme: ")
          val majCafe = readLine("> ").toDouble
          if (majCafe >= 0) {
            coffeeStocks(machineId) += majCafe
            saisieUser = true
          } else {
            println("Erreur: saisir une quantité positive.")
          }
        }

        saisieUser = false
        while (!saisieUser) {
          println("Ajout sucre en gramme: ")
          val majSucre = readLine("> ").toDouble
          if (majSucre >= 0) {
            sugarStocks(machineId) += majSucre
            saisieUser = true
          } else {
            println("Erreur: saisir une quantité positive.")
          }
        }
        saisieUser = false
        while (!saisieUser) {
          println("Ajout lait en litre: ")
          val majLait = readLine("> ").toDouble
          if (majLait >= 0) {
            milkStocks(machineId) += majLait
            saisieUser = true
          } else {
            println("Erreur: saisir une quantité positive.")
          }
        }
        println("Les stocks ont été mis à jour avec succès. ")
        println("Retour au menu principal...")
      }
    }

    println("Nospresso Café")
    while (continue) {
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val mode = readLine("> Votre mode: ")

      if (mode == "1") {
        println("Veuillez sélectionner votre machine (1-5): ")
        var machineId = -1
        while (machineId < 0 || machineId >= nbMachines) {
          machineId = readLine("> Machine sélectionnée (1-5): ").toInt - 1
          if (machineId < 0 || machineId >= nbMachines) {
            println("Erreur: numéro de machine non valide. Veuillez saisir une nouvelle valeur.")
          }
        }
        if (serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
          println("Transaction réussie.")
        } else {
          println("Transaction échouée.")
        }

      } else if (mode == "2") {
        println("Veuillez sélectionner votre machine (1-5): ")
        var machineId = -1
        while (machineId < 0 || machineId >= nbMachines) {
          machineId = readLine("> Machine sélectionnée (1-5): ").toInt - 1
          if (machineId < 0 || machineId >= nbMachines) {
            println("Erreur: numéro de machine non valide. Veuillez saisir une nouvelle valeur.")
          }
        }
        if (validatePin(machineId, machinePins)) {
          println("Choisissez l'action à effectuer: ")
          println("1) Réapprovisionner les stocks")
          println("2) Mettre à jour le code PIN")

          val action = readLine("> Votre choix: ").toInt

          if (action == 1) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else if (action == 2) {
            updatePin(machineId, machinePins)
          } else {
            println("Action non valide.")
          }
        } else {
          continue = false // Arrêter le programme après trois essais de PIN échoués
        }

      } else if (mode == "3") {
        println("Vous avez sélectionné quitter. À bientôt !")
        continue = false

      } else {
        println("Mode non valide. Veuillez réessayer.")
      }
    }
  }
}