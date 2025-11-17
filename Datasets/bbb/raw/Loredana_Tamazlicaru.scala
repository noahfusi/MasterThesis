import io.StdIn.{readLine,readInt}
import util.Random

object Main {
  var machineId = -1
  def main(args: Array[String]): Unit = {
    var ProgrammeActif: Boolean = true
    val nbMachines = 5
    var machinePins = Array.fill(nbMachines)("434343")

    var coffeeStocks = Array.fill(nbMachines)(50.0)
    var sugarStocks = Array.fill(nbMachines)(30.0)
    var milkStocks = Array.fill(nbMachines)(0.500)

    // Function to select the id of the machine wanted
    def selectMachineId(): Int = {
      machineId = -1
      while (machineId < 1  || machineId > nbMachines){
        machineId = readLine("Machine sélectionnéee (1-" + (nbMachines).toString + ") >").toInt
        if(machineId < 1 || machineId > nbMachines){
          println("Erreur de saisie. (Mauvaise identifiant.)")
        }
      }
      machineId = machineId - 1
      validatePin(machineId,machinePins)
      return machineId
    }

    def validatePin(machineId : Int, machinePins : Array[String]) : Boolean = {
      var nbTentatives = 3
      var isPinOk = false
      println("Code PIN: ")
      while (nbTentatives > 0) {
        val codePIN = readLine("> ")

        if (codePIN == machinePins(machineId)) {
          println("Accès accordé à la machine " + (machineId+1).toString)
          return true
        } else {
          println("Code PIN invalide. " + (nbTentatives-1).toString + " tentatives restantes.")
          nbTentatives = nbTentatives - 1
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      System.exit(0)
      return isPinOk
    }

    def updatePin(machineId : Int, machinePins : Array[String]) : Boolean = {
      var inputPin = ""

      println("Mise à jour du code PIN pour la Machine " + machineId.toString + ".")
      while(inputPin.length != 6){
        inputPin = readLine("Entrez un nouveau code PIN à 6 chiffres >")
        if(inputPin.length != 6){
          println("Erreur de saisie.")
        }
      }
      machinePins(machineId) = inputPin
      println("Le code PIN à été mis à jour avec succès.")
      println("Retour au menu principal...")

      for (x <- machinePins) println(x)
      return true
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Double],   sugarStocks: Array[Double], milkStocks: Array[Double]): Boolean = {
      //Variables pour les prix et stocks (réaprovisionnement et vérification après choix de boisson)
      var prixBoisson = 0.0
      var cafeUtilise = 0.0
      var sucreUtilise = 0.0
      var laitUtilise = 0.0
      var prixSucre = 0.0
      var prixLait = 0.0

      var dosesLait = 0

      var choixBoisson = 0
      var quantiteSucre = 0
      var supplementLait = "Non"

      var serveSucessfull = false

      //Mode Client: choix de boisson (+ toute information concernant prix et stock)
      println("Veuillez sélectionner votre boisson : ")
      println("1) Expresso - CHF 2.00")
      println("2) Cappucino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")



      while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3){
        choixBoisson = readLine("> ").toInt
        if (choixBoisson == 1) {
          //Informations Expresso: 1 seule taille disponible
          cafeUtilise = 8.0
          prixBoisson = 2.0


          coffeeStocks(machineId) = (coffeeStocks(machineId) - cafeUtilise).toDouble
          milkStocks(machineId) = (milkStocks(machineId) - laitUtilise).toDouble
          println("Expresso - CHF 2.00")
          serveSucessfull = true
        } else if (choixBoisson == 2) {
          //Informations Cappucino: 1 seule taille disponible + lait en supplément
          cafeUtilise = 6.0
          laitUtilise = 0.100
          prixBoisson = 2.5
          coffeeStocks(machineId) = (coffeeStocks(machineId) - cafeUtilise).toDouble
          milkStocks(machineId) = (milkStocks(machineId) - laitUtilise).toDouble
          println("Cappucino - CHF 2.50")
          serveSucessfull = true

        } else if (choixBoisson == 3) {
          //Informations Latte: 3 tailles + lait en supplément
          println("Choisissez la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen -3.20\n3) Grand - 3.70")

          var tailleLatte = readLine("> ").toInt
          if (tailleLatte == 1) {
            //Petit latte
            cafeUtilise = 6.0
            laitUtilise = 0.120
            prixBoisson = 2.7

            coffeeStocks(machineId) = coffeeStocks(machineId) - cafeUtilise
            milkStocks(machineId) = milkStocks(machineId) - laitUtilise
            println("Petit Latte - CHF 2.7")
            serveSucessfull = true

          } else if (tailleLatte == 2) {
            //Latte moyen
            cafeUtilise = 8.0
            laitUtilise = 0.150
            prixBoisson = 3.2

            coffeeStocks(machineId) = coffeeStocks(machineId) - cafeUtilise
            milkStocks(machineId) = milkStocks(machineId) - laitUtilise
            sugarStocks(machineId) = sugarStocks(machineId) - sucreUtilise
            println("Latte moyen - CHF 3.2")
            serveSucessfull = true

          } else if (tailleLatte == 3) {
            //Grand Latte
            cafeUtilise = 12.0
            laitUtilise = 0.200
            prixBoisson = 3.7
            coffeeStocks(machineId) = coffeeStocks(machineId) - cafeUtilise
            milkStocks(machineId) = milkStocks(machineId) - laitUtilise
            println("Grand Latte - CHF 3.7")
            serveSucessfull = true

          } else {
            println("Erreur de saisie.")
          }
        } else println("Erreur de saisie.")
      }


      //Supplément sucre
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) 5g (peu)- CHF 0.10")
      println("3) 10g (moyen) - CHF 0.20")
      println("4) 15g (beaucoup) - CHF 0.30")



      while (quantiteSucre != 1 && quantiteSucre != 2 && quantiteSucre != 3 && quantiteSucre != 4){
        quantiteSucre = readLine("> ").toInt
        if (quantiteSucre == 1) {
          //Sans sucre
          serveSucessfull = true
        } else if (quantiteSucre == 2) {
          //Ajout de 5g de supplément de sucre
          sucreUtilise = 5.0
          sugarStocks(machineId) = sugarStocks(machineId) - sucreUtilise
          prixSucre = 0.10
          serveSucessfull = true
        } else if (quantiteSucre == 3) {
          //Ajout de 10g de supplément de sucre
          sucreUtilise = 10.0
          sugarStocks(machineId) = sugarStocks(machineId) - sucreUtilise
          prixSucre = 0.2
          serveSucessfull = true
        } else if (quantiteSucre == 4) {
          //Ajout de 15g de supplément de sucre
          sucreUtilise = 15.0
          sugarStocks(machineId) = sugarStocks(machineId) - sucreUtilise
          prixSucre = 0.3
          serveSucessfull = true

        }else println("Erreur de saisie.")

      }


      if (choixBoisson == 2 || choixBoisson == 3) {
        println("Souhaitez-vous ajouter du lait en supplément ? \n (Disponible seulement pour Cappuccino et Latte)")
        println("1) Oui\n2) Non")

        while (supplementLait != "1" && supplementLait != "2"){
          supplementLait = readLine("> ")
          if (supplementLait == "1") {
            //Si il veut du lait supplémentaire, combien de doses ?
            println("Combien de dose (1 dose = 50 mL) ?\n1) 1\n2) 2 \n3) 3 ")
            while (dosesLait != 1 && dosesLait != 2 && dosesLait != 3){
              dosesLait = readLine("> ").toInt
              if ((dosesLait == 1) || (dosesLait == 2) || (dosesLait == 3)) {
                //calcul général pour calcul de la quantité de lait nécessaire, pour dédudction des stocks
                if (dosesLait ==1) {
                  milkStocks(machineId) = milkStocks(machineId) - 0.05
                  prixLait += 0.05
                  serveSucessfull = true
                } else if (dosesLait ==2) {
                  milkStocks(machineId) = milkStocks(machineId) - 0.100
                  prixLait += 0.10
                  serveSucessfull = true

                } else if (dosesLait ==3) {
                  milkStocks(machineId) = milkStocks(machineId) - 0.150
                  prixLait += 0.15
                  serveSucessfull = true
                }
              } else println("Erreur de saisie.")
            }
          } else println("Erreur de saisie.")
        }
      }

      // Vérifier les stock avant de passer à la préparation de la boisson.
      if (cafeUtilise > coffeeStocks(machineId) || sucreUtilise > sugarStocks(machineId) || laitUtilise > milkStocks(machineId)) {
        if (cafeUtilise > coffeeStocks(machineId)) {
          println("Stock insuffisant de café.")
          serveSucessfull = false
        } else if (sucreUtilise > sugarStocks(machineId)) {
          println("Stock insuffisant de sucre.")
          serveSucessfull = false
        } else if (laitUtilise > milkStocks(machineId)) {
          println("Stock insuffisant de lait.")
          serveSucessfull = false
        }
        println("Veuillez choisir une autre boisson ou une autre quantitée!")

      } else {
        //Le stock est suffisant, on prépare la boisson

        // Affichage du résumé de la commande
        println("\nRésumé de votre commande :")
        println("Boisson sélectionnée : " + (if (choixBoisson == 1) "Expresso" else if (choixBoisson == 2) "Cappuccino" else "Latte"))
        println("Niveau de sucre : " + {
          if (quantiteSucre == 1) "Sans sucre"
          else if (quantiteSucre == 2) "Peu (5g)"
          else if (quantiteSucre == 3) "Moyen (10g)"
          else if (quantiteSucre == 4) "Beaucoup (15g)"
          else "Erreur"
        })

        println("Doses de lait : " + {
          if (dosesLait == 1) "Une dose de lait"
          else if (dosesLait == 2) "Deux doses de lait"
          else if (dosesLait == 3) "Moyen (10g)"
          else "Erreur"
        })

        if (dosesLait == 1) dosesLait * 5.0/100.0
        else if (dosesLait == 2) dosesLait * 5.0/100.0
        else if (dosesLait == 3) dosesLait * 5.0/100.0


        // Affichage du prix total
        val prixtotal = prixBoisson + prixSucre + prixLait
        printf("%.2f + %.2f + %.2f = %.2f \n", prixBoisson, prixSucre, prixLait, prixtotal)

        //Paiement par TWINT
        println("Vous allez être redirigé pour le paiement.")
        println("Veuillez payer en utilisant TWINT.")
        val codeTWINT = Random.alphanumeric.take(5).mkString
        println("Votre code de paiement est: " + codeTWINT)

        println("Validation du paiement ... ")
        Thread.sleep(5000) // Attend pendant 5000 millisecondes

        println("Paiement confirmé.")
        println("Préparation de la boisson ... ")
        println("MERCI!")

        choixBoisson = 0

      }
      return serveSucessfull
    }


    def restockMachine(machineId: Int, coffeeStocks: Array[Double],sugarStocks: Array[Double], milkStocks: Array[Double]): Unit = {
      println("Stock: Café = " + coffeeStocks(machineId))
      println("Stock: Sucre = " + sugarStocks(machineId))
      println("Stock: Lait = " + milkStocks(machineId))

      println("Vous-voulez ajouter des stocks ?\n1) Oui \n2) Non")

      val reponserestock = readLine ("> ").toInt
      if (reponserestock == 1) {
        println("Quantitée de poudre de café que vous ajouter?")
        coffeeStocks(machineId) =  coffeeStocks(machineId) + readLine ("> ").toDouble
        println("Quantitée de sucre que vous ajouter?")
        sugarStocks(machineId) =  sugarStocks(machineId) + readLine("> ").toDouble
        println("Quantitée de lait que vous ajouter?")
        milkStocks(machineId) =  milkStocks(machineId) + readLine(">").toDouble
        println("Réapprovisionnement des stocks ... ")

        println("Stock mis à jour")
        println("Stock: Café = " + coffeeStocks(machineId))
        println("Stock: Sucre = " + sugarStocks(machineId))
        println("Stock: Lait = " + milkStocks(machineId))
      } else {
        println("Annulation de restockage.")
      }
    }

    while (ProgrammeActif == true) {
      //Menu principal, choix du mode
      println("\nNospresso Café ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client\n2) Admin \n3) Quitter")
      var choixMode = readLine("> ").toInt
      if (choixMode == 1) {
        machineId = selectMachineId()

        if(serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)){
          println("Service OK")
        }else{
          println("Service NOT OK")
        }
      } else if (choixMode == 2 ) {
        var choixAdmin = -1
        println("1) Update PIN\n2) Restock")

        while (choixAdmin < 0 || choixAdmin > nbMachines - 1){
          choixAdmin = readLine("> ").toInt
          if(choixAdmin < 0 || choixAdmin > nbMachines - 1){
            println("Erreur de saisie.")
          }
        }

        if(choixAdmin == 1){
          machineId = selectMachineId()
          if(updatePin(machineId, machinePins)){
            println("Update PIN OK")
          }else{
            println("Update PIN NOT OK")
          }
        }else if(choixAdmin == 2) {
          machineId = selectMachineId()
          restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        }
      }else if (choixMode == 3) {
        ProgrammeActif = false
        println("Merci d'avoir utilisé Nospresso.")
      } else {
        println("Erreur de saisie.")
      }
    }
  }
}