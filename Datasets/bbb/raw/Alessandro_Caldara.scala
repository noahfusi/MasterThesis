import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {


    // modalités
    val client = 1
    val admin = 2
    val quitter = 3
    var mode = 0

    //gestion stocks machines
    val nbMachines = 5
    var coffeeStocks = Array.fill(nbMachines)(50)
    var sugarStocks = Array.fill(nbMachines)(30)
    var milkStocks = Array.fill(nbMachines)(500)
    var machinePins = Array.fill(nbMachines)("434343")
    var machineId = 0

    //fonction pour choisir la machine
    def choixMachine(): Int = {
      var machineId = 0
      do {
        print("\nMachine sélectionnée (1-" + nbMachines + ") > ")
        machineId = readInt()
      } while (machineId < 1 || machineId > nbMachines)
      return (machineId - 1)
    }


    // itération principale
    do {
      // bienvenue
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      mode = readInt()

      if (mode != client && mode != admin && mode != quitter) {
        println("Le mode sélectionné n'est pas valable.")
        println("Veuillez entrer un nombre valable: " + client + ", " + admin + " ou " + quitter + ".")
      }
      if (mode == client) {
        machineId = choixMachine()
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks) //if mode est client --> serveClient
      }
      if (mode == admin) {
          adminMode( machinePins, coffeeStocks, sugarStocks, milkStocks)
      }
    } while (mode != quitter)

    def checkStock(stock: Int, quantity: Int, typeStock: String): Boolean = {
      if (stock > quantity) {
        return (true)
      } else {
        println("Erreur : Quantité de " + typeStock + " insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int],
                    sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

      //selection des boissons
      val expresso = 1
      val cappuccino = 2
      val latte = 3
      var boisson = 0
      var sucreSup = 0
      var tailleLatte = 0
      var returnCode = false
      //sélection du boisson
      do {
        println("=====Mode Client=====")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        boisson = readInt()
        if (boisson != 1 && boisson != 2 && boisson != 3) {
          println("La boisson sélectionnée n'est pas valable.")
          println("Veuillez entrer une boisson valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
        }
      } while (boisson != 1 && boisson != 2 && boisson != 3)

      if (boisson == latte) {
        do {
          println("Quelle taille voulez-vous?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          print("> ")
          tailleLatte = readInt()
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("La taille sélectionnée n'est pas valable.")
            println("Veuillez entrer une taille valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
      }

      if (boisson == expresso || boisson == cappuccino || boisson == latte) {
        do {
          println("Souhaitez-vous ajouter du sucre?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          sucreSup = readInt()
          if (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4) {
            println("La quantité sélectionnée n'est pas valable.")
            println("Veuillez entrer une quantité valable: " + 1 + ", " + 2 + ", " + 3 + " ou " + 4 + ".")
          }
        } while (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4)
      }

      var laitSup = 0
      var doseLaitSup = 0
      if (boisson == cappuccino || boisson == latte) {
        do {
          println("Souhaitez-vous ajouter du lait en supplément? (Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          print("> ")
          laitSup = readInt()
          if (laitSup != 1 && laitSup != 2) {
            println("La touche sélectionnée n'est pas valable.")
            println("Veuillez entrer une touche valable: " + 1 + " ou " + 2 + ".")
          }
        } while (laitSup != 1 && laitSup != 2)
      }

      if (laitSup == 1) {
        println("Combien de doses?")
        print("> ")
        doseLaitSup = readInt()
        while (doseLaitSup < 1 || doseLaitSup > 3) {
          println("La dose sélectionnée n'est pas valable.")
          println("Veuillez entrer une dose valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
          doseLaitSup = readInt()
        }
      }

      var stockSuffisantCafe = true
      var stockSuffisantLait = true
      var stockSuffisantSucre = true

      //éléments nécessaires
      var poudreCafeEx = 8
      var poudreCafeCap = 6
      var laitCap = 100
      var poudreCafeLat1 = 6
      var laitLat1 = 120
      var poudreCafeLat2 = 8
      var laitLat2 = 150
      var poudreCafeLat3 = 12
      var laitLat3 = 200

      // vérification stock
      if (boisson == 1) {
        stockSuffisantCafe = checkStock(coffeeStocks(machineId), poudreCafeEx, "poudre de café")
        stockSuffisantSucre = checkStock(sugarStocks(machineId), ((sucreSup - 1) * 5), "sucre")
      } else if (boisson == 2) {
        stockSuffisantCafe = checkStock(coffeeStocks(machineId), poudreCafeCap, "poudre de café")
        stockSuffisantLait = checkStock(milkStocks(machineId), ((laitCap) + (doseLaitSup * 50)), "lait")
        stockSuffisantSucre = checkStock(sugarStocks(machineId), ((sucreSup - 1) * 5), "sucre")
      } else if (boisson == 3) {
        if (tailleLatte == 1) {
          stockSuffisantCafe = checkStock(coffeeStocks(machineId), poudreCafeLat1, "poudre de café")
          stockSuffisantLait = checkStock(milkStocks(machineId), ((laitLat1) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(sugarStocks(machineId), ((sucreSup - 1) * 5), "sucre")
        } else if (tailleLatte == 2) {
          stockSuffisantCafe = checkStock(coffeeStocks(machineId), poudreCafeLat2, "poudre de café")
          stockSuffisantLait = checkStock(milkStocks(machineId), ((laitLat2) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(sugarStocks(machineId), ((sucreSup - 1) * 5), "sucre")
        } else if (tailleLatte == 3) {
          stockSuffisantCafe = checkStock(coffeeStocks(machineId), poudreCafeLat3, "poudre de café")
          stockSuffisantLait = checkStock(milkStocks(machineId), ((laitLat3) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(sugarStocks(machineId), ((sucreSup - 1) * 5), "sucre")
        }
      }

      // paiement
      if (stockSuffisantCafe && stockSuffisantLait && stockSuffisantSucre) {
        var prix = 0.0
        if (boisson == expresso) {
          prix = 2
        }
        else if (boisson == cappuccino) {
          prix = 2.50
        }
        else if (tailleLatte == 1) {
          prix = 2.70
        }
        else if (tailleLatte == 2) {
          prix = 3.20
        }
        else if (tailleLatte == 3) {
          prix = 3.70
        }

        val prixLait = (doseLaitSup) * 0.05
        val prixSucre = (sucreSup - 1) * 0.10
        val prixTotal = prix + prixSucre + prixLait

        println("Prix total: " + "CHF " + prix.toFloat + " +" + " CHF " + prixSucre.toFloat + " +" + " CHF " + prixLait.toFloat + " = CHF " + prixTotal.toFloat + ".")
        val twintCode = Random.alphanumeric.take(5).mkString.toUpperCase() //méthode trouvé dans la librairie Scala
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + twintCode + ".")
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")

        // deductionm stock
        if (boisson == expresso) {
          coffeeStocks(machineId) -= poudreCafeEx
        }
        if (boisson == cappuccino) {
          coffeeStocks(machineId) -= poudreCafeCap
          milkStocks(machineId) -= laitCap
        }
        if (boisson == latte && tailleLatte == 1) {
          coffeeStocks(machineId) -= poudreCafeLat1
          milkStocks(machineId) -= laitLat1
        }
        if (boisson == latte && tailleLatte == 2) {
          coffeeStocks(machineId) -= poudreCafeLat2
          milkStocks(machineId) -= laitLat2
        }
        if (boisson == latte && tailleLatte == 3) {
          coffeeStocks(machineId) -= poudreCafeLat3
          milkStocks(machineId) -= laitLat3
        }
        sugarStocks(machineId) -= ((sucreSup - 1) * 5)
        milkStocks(machineId) -= (doseLaitSup * 50)

        //preparation boisson
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        returnCode = true
        if (boisson == 1) {
          println("Votre Expresso est prêt! Bonne dégustation!")
        }
        else if (boisson == 2) {
          println("Votre Cappuccino est prêt! Bonne dégustation!")
        }
        else if (boisson == 3) {
          println("Votre Latte est prêt! Bonne dégustation!")
        }
      }
      return (returnCode)
    }

    // fonction pour valider le PIN
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var correctPin: String = machinePins(machineId)
      var tentatives = 3
      var found = false
      do {
        var tentativePin = readLine("Entrez le code PIN: ")
        tentatives -= 1
        if (correctPin == tentativePin) {
          println("Accès accordé à la Machine " + machineId + " .")
          found = true
        } else if (tentatives == 1) {
          println("Code PIN incorrect. " + tentatives + " tentative restante.")
        } else {
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
        }
      } while (tentatives > 0 && !found)

      return found
    }

    // fonction mode Admin
    def adminMode(machinePins: Array[String], coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

      println("==========Mode Admin==========")
      var operationAdmin = 0
      do {
        println("Quelle opération voulez vous faire? ")
        println("1) Restock")
        println("2) Changement du PIN")
        print("> ")
        operationAdmin = readInt()
      } while (operationAdmin != 1 && operationAdmin != 2)

      machineId = choixMachine()
      if (!validatePin(machineId, machinePins)) {
        println("Trop de tentatives échouées. Fin du programme.")
        return
      }

      if (operationAdmin == 1) {
        restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
      } else if (operationAdmin == 2) {
        updatePin(machineId, machinePins)
      }
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
      var newPin = 0.toString
      do {
        print("Entrez un nouveau code PIN à 6 chiffres > ")
        newPin = readLine()
      } while (newPin.size != 6 || ! newPin.toIntOption.isDefined)

        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")

    }

    def restockMachine(machineId: Int, coffeeStocks: Array[Int],
    sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

      println("Niveaux de stock actuels:")
      println("Poudre de café : " + coffeeStocks(machineId))
      println("Lait : " + milkStocks(machineId))
      println("Sucre : " + sugarStocks(machineId))

      println("Entrez les quantités à ajouter:")
      print("Poudre de café: ")
      coffeeStocks(machineId) += readInt()
      print("Lait: ")
      milkStocks(machineId) += readInt()
      print("Sucre: ")
      sugarStocks(machineId) += readInt()

      println("Les stocks ont été mis à jour avec succès.")
      println("Retour au menu principal...")
      Thread.sleep(1000)

    }


  }
}