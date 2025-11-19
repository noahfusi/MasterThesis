import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    var machineId = Array.fill(nbMachines)(0)
    //Chaque machine posse`de un identifiant uniquecompris entre0etnbMachines−1
    //tableau code machine
    var machinePins = Array("434343", "434343", "434343", "434343", "434343")
    val coffeeStocks = Array(50.0, 50.0, 50.0, 50.0, 50.0)
    val sugarStocks = Array(30.0, 30.0, 30.0, 30.0, 30.0)
    val milkStocks = Array(0.500, 0.500, 0.500, 0.500, 0.500)

    var NomBoisson = ""
    var NiveauSucre = ""
    var NiveauLait = ""

    //STOCK POUR DIFFERENT CAFé
    val CafeEspresso = 8.0 //g
    val CafeCappuccino = 6.0 //g
    val LaitCappuccino = 0.100 //L
    val CaffeLattePetit = 6.0
    val LaitLattePetit = 0.120
    val CaffeLatteMoyen = 8.0
    val LaitLatteMoyen = 0.150
    val CaffeLatteGrand = 12.0
    val LaitLatteGrand = 0.200

    //stock sucre x-tra
    val SucrePeu = 5.0
    val SucreMoyen = 10.0
    val SucreBeaucoup = 15.0

    //stock lait x-tra
    val LaitSup1 = 0.050
    val LaitSup2 = 0.100
    val LaitSup3 = 0.150

    //PRIX
    // Lait
    val PrixLait1 = 0.05
    val PrixLait2 = 0.10
    val PrixLait3 = 0.15

    //sucre
    val PrixSucrePeu = 0.10
    val PrixSucreMoyen = 0.20
    val PrixSucreBeaucoup = 0.30

    //prix type de café
    //Expresso
    val PrixEspresso = 2.0

    //Cappuccino
    val PrixCappuccino = 2.5

    //Latte
    val PrixLattePetit = 2.7
    val PrixLatteMoyen = 3.2
    val PrixLatteGrand = 3.7


    var continuerProgram = true

    while (continuerProgram) {
      println("Bienvenue à Nospresso! Veuillez sélectionner une de nos machines (1-5) >")
      val machineId = readInt() - 1
      if (machineId >= 0) {
        println("Nospresso Café\nVeuillez s'electionner votre mode :\n1) Client\n2) Admin (changer le PIN)\n 3) Admin (ajouter du stock)\n4) Quitter\n>")
        val mode = readInt()
        if (mode == 1) {
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } else if (mode == 2) {
          if (validatePin(machineId, machinePins)) {
            updatePin(machineId, machinePins)
          } else {
            println("Accès refusé.")
          }
        } else if (mode == 3) {
          if (validatePin(machineId, machinePins)) {
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
        } else if (mode == 4) {
          continuerProgram = false
          println("Erreur.")
        }
      }
    }


    //validatePin
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      //fix so that the stocks are correlated with the new Arrays and so that the reflect the machine ID (eg. stockCoffee(machineID))
      println("Veuillez entrer le code PIN (6 chiffres) :")
      var essaie = 0
      while (essaie < 3) {
        val motDePasse = readLine
        if (motDePasse == machinePins(machineId)) {
          println("Accès autorisé.")
          return true
        } else {
          essaie += 1
          var tentativesRestantes = 3 - essaie
          println("Code PIN incorrect. " + tentativesRestantes + "tentatives restantes. >")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }




    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + "\n Entrez un nouveau code PIN à 6 chiffres >")
      var nouveauPIN = ""
      while(nouveauPIN.length < 6) {
        println("Entrez un nouveau code PIN à 6 chiffres >")
        nouveauPIN = readLine()
      }
      machinePins(machineId) = nouveauPIN
      println("Le code PIN a  été mis à jour avec succès.\nRetour au menu principal...")
    }


    def serveClient(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Unit = {
      var PrixClient = 0.00
      var PrixCalcul = ""
      var stockSuffisant = true //pour vérifier que le stock de produit = suffisant

      println("Veuillez s´electionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
      var selectionboisson = readInt()

      if (selectionboisson == 1) {
        if (coffeeStocks(machineId) >= CafeEspresso) {
          coffeeStocks(machineId) -= CafeEspresso
          PrixClient += PrixEspresso
          NomBoisson = "Espresso"
          PrixCalcul += "CHF 2.00 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (selectionboisson == 2) {
        if (coffeeStocks(machineId) >= CafeCappuccino && milkStocks(machineId) >= LaitCappuccino) {
          coffeeStocks(machineId) -= CafeCappuccino
          milkStocks(machineId) -= LaitCappuccino
          PrixClient += PrixCappuccino
          NomBoisson = "Cappuccino"
          PrixCalcul += "CHF 2.50 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      }
      if (selectionboisson == 3) {
        println("Veuillez choisir la taille du latte :\n1) Petit - CHF 2.70\n2)Moyen - CHF 3.20\n3) Grand - CHF 3.70")
        var taillelatte = readInt()
        if (taillelatte == 1) {
          if (coffeeStocks(machineId) >= CaffeLattePetit && milkStocks(machineId) >= LaitLattePetit) {
            coffeeStocks(machineId) -= CaffeLattePetit
            milkStocks(machineId) -= LaitLattePetit
            PrixClient += PrixLattePetit
            NomBoisson = "Latte Petit"
            PrixCalcul += "CHF 2.70 "
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        } else if (taillelatte == 2) {
          if (coffeeStocks(machineId) >= CaffeLatteMoyen && milkStocks(machineId) >= LaitLatteMoyen) {
            coffeeStocks(machineId) -= CaffeLatteMoyen
            milkStocks(machineId) -= LaitLatteMoyen
            PrixClient += PrixLatteMoyen
            NomBoisson = "Latte Moyen"
            PrixCalcul += "CHF 3.20 "
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        } else if (taillelatte == 3) {
          if (coffeeStocks(machineId) >= CaffeLatteGrand && milkStocks(machineId) >= LaitLatteGrand) {
            coffeeStocks(machineId) -= CaffeLatteGrand
            milkStocks(machineId) -= LaitLatteGrand
            PrixClient += PrixLatteGrand
            NomBoisson = "Latte Grand"
            PrixCalcul += "CHF 3.70"
            stockSuffisant = true
          } else {
            stockSuffisant = false
          }
        }
      }
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
      var Qsucre = readInt()
      if (Qsucre == 2) {
        if (sugarStocks(machineId) >= SucrePeu) {
          sugarStocks(machineId) -= SucrePeu
          PrixClient += PrixSucrePeu
          NiveauSucre = "Peu (5g)"
          PrixCalcul += " + CHF 0.10 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 3) {
        if (sugarStocks(machineId) >= SucreMoyen) {
          sugarStocks(machineId) -= SucreMoyen
          PrixClient += PrixSucreMoyen
          NiveauSucre = "Moyen (10g)"
          PrixCalcul += " + CHF 0.20 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 4) {
        if (sugarStocks(machineId) >= SucreBeaucoup) {
          sugarStocks(machineId) -= SucreBeaucoup
          PrixClient += PrixSucreBeaucoup
          NiveauSucre = "Beaucoup (15g)"
          PrixCalcul += " + CHF 0.30 "
          stockSuffisant = true
        } else {
          stockSuffisant = false
        }
      } else if (Qsucre == 1) {
        NiveauSucre = "Non"
      }

      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>")
      var choixLait = readInt()

      if (choixLait == 1) {
        println("Combien de dose ?\n>")
        var QLait = readInt()

        while (QLait > 3) {
          if (QLait == 1 && milkStocks(machineId) >= LaitSup1) {
            milkStocks(machineId) -= LaitSup1
            PrixClient += PrixLait1
            NiveauLait = "Une dose (50ml)"
            PrixCalcul += " + CHF 0.05 "
            stockSuffisant = true

          } else if (QLait == 2 && milkStocks(machineId) >= LaitSup2) {
            milkStocks(machineId) -= LaitSup2
            PrixClient += PrixLait2
            NiveauLait = "Deux doses (100ml)"
            PrixCalcul += " + CHF 0.10 "
            stockSuffisant = true

          } else if (QLait == 3 && milkStocks(machineId) >= LaitSup3) {
            milkStocks(machineId) -= LaitSup3
            PrixClient += PrixLait3
            NiveauLait = "Trois doses(150ml)"
            PrixCalcul += " + CHF 0.15 "
            stockSuffisant = true

          } else if (QLait > 3) {
            println("Choisisez une quantité de lait plus bas (3 doses ou moin)\n>")
            var QLait = readInt()
          } else if (choixLait == 2) {
            NiveauLait = "Non"
          }
        }
        //Annonce de la commande (selon si stock = suffisant ou pas)
        if (stockSuffisant == true) {
          printf("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire: " + NiveauLait + "\n Prix total: " + PrixCalcul + " = %.2f", PrixClient)
        } else if (!stockSuffisant) {
          print("Boisson sélectionnée: " + NomBoisson + "\n Niveau de sucre:" + NiveauSucre + "\n Lait supplémentaire:" + NiveauLait + "\n Erreur : Stock insuffisante pour préparer le boisson sélectionnée. \nVeuillez choisir une tailleplus petite ou essayer une autre boisson.")
        }
      }


      println(" Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + Random.alphanumeric.take(5).mkString + "\n (En attente de validation du paiement...)")

      Thread.sleep(5000)
      println("Merci ! Votre paiement a été accepté. \n ")
      println("Préparation de votre boisson... \n[...]")
      println("votre " + NomBoisson + " est prêt ! Bonne dégustation !")
    }


    def restockMachine(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Unit = {
      println("Stock actuels: \n Poudre de café: " + coffeeStocks(machineId-1) + "g \n Lait: " + milkStocks(machineId-1) + "L \n Sucre: " + sugarStocks(machineId-1) + "g")
      println("Entrez les quantités à ajouter: ")
      println("Poudre de café > ")
      val AjoutCafe = readDouble()
      println("Sucre >")
      val AjoutSucre = readDouble()
      println("Lait >")
      val AjoutLait = readDouble()
      //Réapprovisonnement des stocks interne
      coffeeStocks(machineId-1) += AjoutCafe
      sugarStocks(machineId-1) += AjoutSucre
      milkStocks(machineId-1) += AjoutLait
    }
  }
}





