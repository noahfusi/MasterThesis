import scala.io.StdIn.readLine
import scala.util.Random

//////// --Methods
object Methodefonction {
  //Methode ValidatePin
  def validatePin(machineId: Int, machinePins: Array[String], codePinSaisie: String): Boolean = {
    val codePintableau = machinePins(machineId - 1)
    if (codePintableau.toString == codePinSaisie)
      return true

    return false
  }

  //Methode UpdatePin
  def updatePin(machineId: Int, machinePins: Array[String], nouveauPin: String): Unit = {
    machinePins(machineId - 1) = nouveauPin
  }

  //Méthode restockMachine
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int], flagChoixAdmin: Int): Unit = {
    //Initialisation des variables relatives aux quantités de stock ajoutées par l'admin
    var laitAjouteParlAdmin = 0
    var sucreAjouteParlAdmin = 0
    var poudredeCafeAjouteeParlAdmin = 0

    //flagMiseAJourrOk permet d'identifier que la mise à jour des stocks a bien été effectuée.
    var flagMiseAJourOk = false

    //Début de la boucle while pour modifier le stock de poudre de café, de lait et de sucre
    while (flagMiseAJourOk == false && flagChoixAdmin == 1) {
      println("Quelle quantité de poudre de café (en g) voulez-vous ajouter?>")
      val poudredeCafeAjouteeParlAdmin = readLine().toInt
      if (poudredeCafeAjouteeParlAdmin >= 0) {
        println("Quantité de poudre de café OK")
        println("Quelle quantité de lait (en mL) voulez-vous ajouter?>")
        val laitAjouteParlAdmin = readLine().toInt
        if (laitAjouteParlAdmin >= 0) {
          println("Quantité de lait OK")
          println("Quelle quantité de sucre (en g) voulez-vous ajouter?>")
          val sucreAjouteParlAdmin = readLine().toInt
          if (sucreAjouteParlAdmin >= 0) {
            println("Quantité de sucre OK")
            flagMiseAJourOk = true
            sugarStocks(machineId - 1) = sugarStocks(machineId - 1) + sucreAjouteParlAdmin
            milkStocks(machineId - 1) = milkStocks(machineId - 1) + laitAjouteParlAdmin
            coffeeStocks(machineId - 1) = coffeeStocks(machineId - 1) + poudredeCafeAjouteeParlAdmin

            println("Stock Poudre de Café : " + coffeeStocks(machineId - 1) + "g")
            println("Stock Lait :" + milkStocks(machineId - 1) + "mL")
            println("Stock Sucre :" + sugarStocks(machineId - 1) + "g")
          }
        }
      }
    } //Fin de la boucle while pour modifier le stock de poudre de café, de lait et de sucre

    //Si l'admin a saisi un nombre négatif pour les quantités:
    if (flagMiseAJourOk == false && flagChoixAdmin == 1) {
      println("Saisie erronée. Veuillez saisir un nombre positif pour les quantités. ")
    }
  }

  //Methode ServeClient
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var flagBoissons = true; //permet de vérifier que l'utilisateur a bien commandé une boisson
    var flagExpresso = false //permet d'identifier que l'utilisateur a commandé un expresso
    var flagCappuccino = false //permet d'identifier que l'utilisateur a commandé un cappuccino
    var flagLatte = false //permet d'identifier que l'utilisateur a commandé un latte
    var flagTypeCafe = 0; //1 = Expresso, 2 = Cappuccino, 3 = Latte
    var stockSucre = 0
    var stockLait = 0
    var stockLaitRequis = 0
    var stockPoudredeCafeRequis = 0
    var stockSucreRequis = 0
    var stockLaitSupplementRequis = 0
    var prixSucreAAjouter = 0.00
    var prixLaitAAjouter = 0.00
    var prixCafe = 0.00
    var flagLattePetit = false
    var flagLatteMoyen = false
    var flagLatteGrand = false

    //Début de la boucle while pour sélectionner les boissons
    while (flagBoissons == true) {

      println("Veuillez sélectionner votre boisson:")
      println("1) Expresso - CHF 2.00 ")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      println(">")

      val commandeBoissons = readLine().toInt; //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

      //Début de la boucle if else pour sélectionner les boissons
      if (commandeBoissons == 1) {
        println("Expresso")
        flagBoissons = false
        flagExpresso = true
        flagTypeCafe = 1
        stockPoudredeCafeRequis = 8
      }
      else if (commandeBoissons == 2) {
        println("Cappuccino")
        flagBoissons = false
        flagCappuccino = true
        flagTypeCafe = 2
        stockPoudredeCafeRequis = 6
        stockLaitRequis = 100
      }
      else if (commandeBoissons == 3) {
        println("Latte")
        flagBoissons = false
        flagLatte = true
        flagTypeCafe = 3
      }
      else {
        println("Saisie erronée. Veuillez sélectionner le nombre 1, 2 ou 3.")
      }
      //Fin de la boucle if else pour sélectionner les boissons

      //Boucle if else pour sélectionner la taille du latte
      if (flagTypeCafe == 3) {
        println("Quelle taille de latte vous faudrait-il? ")
        println("Saisissez 1 pour petit")
        println("Saisissez 2 pour moyen")
        println("Saisissez 3 pour grand")

        val commandeLatte = readLine().toInt //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

        var flagcommandeLatte = true //flagcommandeLatte est un Boolean. Si on lui attribue la valeur true
        //qui est la valeur initiale, on reste dans le programme. Si
        //on lui attribue la valeur false, on sort du programme.

        //Début de la boucle while pour sélectionner la taille du latte
        while (flagcommandeLatte == true) {
          if (commandeLatte == 1) {
            println("Vous avez sélectionné un petit latte.")
            flagcommandeLatte = false
            stockPoudredeCafeRequis = 6
            stockLaitRequis = 120
            flagLattePetit = true
          }
          else if (commandeLatte == 2) {
            println("Vous avez sélectionné un moyen latte.")
            flagcommandeLatte = false
            stockPoudredeCafeRequis = 8
            stockLaitRequis = 150
            flagLatteMoyen = true
          }
          else if (commandeLatte == 3) {
            println("Vous avez sélectionné un grand latte.")
            flagcommandeLatte = false
            stockPoudredeCafeRequis = 12
            stockLaitRequis = 200
            flagLatteGrand = true
          }
          else {
            println("Saisie erronée. Veuillez sélectionner le nombre 1, 2 ou 3.")
          }
        } //Fin de boucle while pour sélectionner la taille du latte
      } //Fin de la boucle if else pour sélectionner la taille du latte

      //Début de la boucle if else pour ajouter du sucre à la boisson
      if (flagTypeCafe > 0 && flagTypeCafe <= 3) {
        //if (flagExpresso == true || flagLatte == true || flagCappuccino == true){
        println("Souhaitez-vous ajouter du sucre ?")
        println("1)Sans sucre")
        println("2)Peu (5g) - CHF 0.10")
        println("3)Moyen (10g) - CHF 0.20")
        println("4)Beaucoup (15g) - CHF 0.30")
        println(">")

        val commandeSucre = readLine().toInt //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

        var flagSucre = true //flagSucre permet d'identifier si l'utilisateur a commandé du sucre et si oui en
        //quelle quantité.

        //Début de la boucle while pour ajouter du sucre à la boisson
        while (flagSucre == true) {
          if (commandeSucre == 1) {
            println("sans sucre")
            flagSucre = false
          }
          else if (commandeSucre == 2) {
            println("peu")
            flagSucre = false
            stockSucreRequis = 5
            prixSucreAAjouter = 0.10
          }
          else if (commandeSucre == 3) {
            println("moyen")
            flagSucre = false
            stockSucreRequis = 10
            prixSucreAAjouter = 0.20
          }
          else if (commandeSucre == 4) {
            println("beaucoup")
            flagSucre = false
            stockSucreRequis = 15
            prixSucreAAjouter = 0.30
          }
          else {
            println("Saisie erronée. Veuillez sélectionner le nombre 1, 2, 3 ou 4.")
          }
        } //Fin de la boucle while pour ajouter du sucre à la boisson
      } //Fin de la boucle if else pour ajouter du sucre à la boisson

      //flagCommandeSupplement permet d'identifier si l'utilisateur a
      //commandé du lait et si oui en quelle quantité.
      var flagCommandeSupplement = false

      //Début de la boucle pour ajouter ou non du lait à la boisson
      if (flagTypeCafe == 2 || flagTypeCafe == 3) {
        //if (flagCappuccino == true || flagLatte == true) {

        println("Souhaitez-vous ajouter du lait?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        println(">")

        val commandelait = readLine().toInt //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

        //flaglait permet d'identifier si l'utilisateur a commandé du lait ou non.
        var flaglait = true

        //Début de la boucle while pour ajouter ou non du lait à la boisson
        while (flaglait == true) {
          if (commandelait == 1) {
            println("Vous avez commandé du lait.")
            flaglait = false
            flagCommandeSupplement = true
          }
          else if (commandelait == 2) {
            println("Vous n'avez pas commandé de lait.")
            flaglait = false
          }
          else {
            println("Saisie erronée. Veuillez sélectionner le nombre 1 ou 2.")
          } //Fin de la boucle if else pour ajouter ou non du lait à la boisson
        } //Fin de la boucle while pour ajouter ou non du lait à la boisson

        //Début de la boucle if else pour sélectionner le nombre de doses de lait à ajouter
        if (flagCommandeSupplement == true) {

          //flagSupplement permet d'identifier la sélection pour les doses de lait.
          var flagSupplement = true

          println("Combien de dose(s)?>")
          println("Vous pouvez commander 1, 2 ou 3 doses")

          val commandeSupplement = readLine().toInt //L'utilisateur peut saisir une valeur grâce à la fonction
          //readLine.

          //Début de la boucle while pour sélectionner le nombre de doses de lait à ajouter
          while (flagSupplement == true) {
            if (commandeSupplement == 1) {
              println("Vous avez commandé 1 dose de lait.")
              flagSupplement = false
              stockLaitSupplementRequis = 50
              prixLaitAAjouter = 0.05
            }
            else if (commandeSupplement == 2) {
              println("Vous avez commandé 2 doses de lait.")
              flagSupplement = false
              stockLaitSupplementRequis = 100
              prixLaitAAjouter = 0.10
            }
            else if (commandeSupplement == 3) {
              println("Vous avez commandé 3 doses de lait.")
              flagSupplement = false
              stockLaitSupplementRequis = 150
              prixLaitAAjouter = 0.15
            }
            else {
              println("Saisie erronée. Veuillez sélectionner le nombre 1, 2 ou 3.")
            }
          } //Fin de la boucle while pour sélectionner le nombre de doses de lait à ajouter

        } //Fin de la boucle if else pour sélectionner le nombre de doses de lait à ajouter
      }
    }
    //Génération d'un code aléatoire et composé de 5 caractères alphanumériques
    var codePaiement: String = "";
    codePaiement = Random.alphanumeric.take(5).mkString;

    //Calcul du prix du café
    if (flagTypeCafe == 1) {
      prixCafe = 2
    } else if (flagTypeCafe == 2) {
      prixCafe = 2.50
    }
    else if (flagTypeCafe == 3) {
      if (flagLattePetit == true)
        prixCafe = 2.70
      else if (flagLatteMoyen == true)
        prixCafe = 3.20
      else if (flagLatteGrand == true)
        prixCafe = 3.70
    }
    //Prix final du café
    prixCafe = prixCafe + prixLaitAAjouter + prixSucreAAjouter

    //On vérifie si le stock est suffisant.
    //Si le stock est suffisant, on peut préparer le café.
    //Mise à jour des stocks pour la poudre de café, le lait et le sucre
    if (stockPoudredeCafeRequis <= coffeeStocks(machineId - 1)
      && (stockLaitSupplementRequis + stockLaitRequis) <= milkStocks(machineId - 1)
      && stockSucreRequis <= sugarStocks(machineId - 1)) {
      coffeeStocks(machineId - 1) = coffeeStocks(machineId - 1) - stockPoudredeCafeRequis
      milkStocks(machineId - 1) = milkStocks(machineId - 1) - (stockLaitSupplementRequis + stockLaitRequis)
      sugarStocks(machineId - 1) = sugarStocks(machineId - 1) - stockSucreRequis

      println("Le prix de votre café est : " + prixCafe + "CHF");
      println("Veuillez payer en utilisant Twint. ");
      println("Votre code de paiement est : " + codePaiement);
      println("En attente de validation du paiement...");
      // 3000 millisecondes = 3 secondes
      //permet de mettre l’execution le code en pause pendant 3 secondes
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté");
      return true
    }
    else {
      //Si le stock n'est pas suffisant
      println("Stock insuffisant. Impossible de préparer le café.");
      return false
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    //Définition de la variable nbMachines pour définir le nombre total de machines
    var nbMachines = 5
    var flagAutreMachine = false
    var choixMachine = 0
    val codePinInitial = 434343

    //Création d'un tableau vide
    val tableauCodePin = new Array[String](nbMachines)

    //Initialisation de chaque élément du tableau avec une boucle for
    for (i <- 0 until nbMachines) {
      tableauCodePin(i) = codePinInitial.toString
    }

    //Début de la gestion des stocks : valeurs initiales pour les stocks de poudre de café, de lait et de sucre
    val tableauCoffeeStocks = new Array[Int](nbMachines)
    val tableauSugarStocks = new Array[Int](nbMachines)
    val tableauMilkStocks = new Array[Int](nbMachines)

    // Initialisation de chaque élément du tableau avec une boucle for
    for (i <- 0 until nbMachines) {
      tableauCoffeeStocks(i) = 50
      tableauSugarStocks(i) = 30
      tableauMilkStocks(i) = 500
    }

    //Ce flag permet d'identifier le type de café. Il a la même fonction que flagExpresso, flagCappuccino et flagLatte.
    //Mais il permet de simplifier l'implémentation des conditions.
    var flagMode: Boolean = true;
    var flagMachine = true
    while (flagMode == (true)) {
      flagMachine = false
      while (flagMachine == false) {
        if (flagAutreMachine == false) {
          println("Sélectionner votre machine en tapant 1, 2, 3, 4 ..." + nbMachines)
          choixMachine = readLine().toInt //L'utilisateur peut choisir sa machine grâce à la fonction readLine.
          if (choixMachine >= 1 && choixMachine <= nbMachines) {
            println("Vous avez sélectionné la machine n°" + choixMachine)
            flagMachine = true
          }
          else println("Saisie erronnée.")
        }
        else {
          flagAutreMachine = false
          println("Les quantités pour cette machine ne sont pas suffisantes.")
          println("Sélectionner une autre machine que la machine n° " + choixMachine)
          var choixAncienneMachine = choixMachine
          choixMachine = readLine().toInt //L'utilisateur peut choisir sa machine grâce à la fonction readLine.
          if (choixMachine >= 1 && choixMachine <= nbMachines && choixMachine != choixAncienneMachine) {
            println("Vous avez sélectionné la machine n°" + choixMachine)
            flagMachine = true
          }
          else {
            println("Vous ne pouvez pas sélectionner la même machine. Le programme va s'arrêter.")
            sys.exit(1)
          }
        }
      }

      println("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")

      val commandeMode = readLine().toInt; //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

      //Initialisation des variables relatives au stock
      var stockPoudredeCafeRequis = 0
      var stockSucreRequis = 0
      var stockLaitRequis = 0
      var stockLaitSupplementRequis = 0

      //Initialisation des variables relatives au prix
      var prixCafe = 0.00 //prix du café
      var prixLaitAAjouter = 0.00 //prix du lait ajouté par l'utilisateur
      var prixSucreAAjouter = 0.00 //prix du sucre ajouté par l'utilisateur

      //Ces flags permettent d'identifier si le latte est petit, moyen ou grand. Il aurait été possible d'utiliser un
      //seul flag pour les 3 mais on aurait du lui attribuer les valeurs 1, 2 ou 3.
      var flagLattePetit = false
      var flagLatteMoyen = false
      var flagLatteGrand = false

      //Début mode Client
      if (commandeMode == 1) {
        println("Mode Client");

        if (Methodefonction.serveClient(choixMachine, tableauCoffeeStocks,
          tableauSugarStocks, tableauMilkStocks) == false)
          flagAutreMachine = true

      } //Fin du mode Client

      //Début du mode Admin
      else if (commandeMode == (2)) {
        println("Mode Admin");
        //flagMode = false;

        //val codePinReference = "434343"
        var flagAdminAutorise = false //flagAdminAutorise permet d'identifier que l'utilisateur a tapé le bon code PIN.
        var CompteurCodePin = 0
        //Début de la boucle if else pour vérifier que le code PIN est correct

        while (CompteurCodePin < 3) {
          println("Entrez le code PIN")
          val codePinSaisie = readLine() //L'utilisateur peut saisir une valeur grâce à la fonction readLine.

          if (Methodefonction.validatePin(choixMachine, tableauCodePin, codePinSaisie) == true) {
            println("Accès autorisé")
            flagAdminAutorise = true
            CompteurCodePin = 3; //Sortie de la boucle
          }
          else {
            println("Code PIN incorrect. Pour réessayer, choisissez l'option 2 pour entrer dans le mode Admin.")
            CompteurCodePin = CompteurCodePin + 1
          }
        }
        if (flagAdminAutorise == false) {
          sys.exit(0)
        }
        //Fin de la boucle if else pour vérifier que le code PIN est correct

        if (flagAdminAutorise == true) {
          var flagChoixAdmin = 0
          while (flagChoixAdmin == 0) {
            //Etat des stocks
            println("Valeur du stock de poudre de Cafe: " + tableauCoffeeStocks(choixMachine - 1) + "g")
            println("Valeur du stock de lait: " + tableauMilkStocks(choixMachine - 1) + "mL")
            println("Valeur du stock de sucre: " + tableauSugarStocks(choixMachine - 1) + "g")
            println("Tapez 1 pour changer votre Code PIN")
            println("tapez 2 pour changer les quantités")
            println("Tapez autre chose pour revenir au menu principal.")

            val demandeDeChangementDeCodePin = readLine().toInt
            if (demandeDeChangementDeCodePin == 1) {
              var flagCodePinMisAJour = 0
              while (flagCodePinMisAJour == 0) {
                println("Entrez le nouveau code PIN. Il doit contenir 6 chiffres.")
                val nouveauCodePin = readLine()
                if (nouveauCodePin.length == 6 && nouveauCodePin.forall(c => c.isDigit)) {
                  Methodefonction.updatePin(choixMachine, tableauCodePin, nouveauCodePin)
                  println("Vous avez bien mis à jour votre code Pin.")
                  flagCodePinMisAJour = 1
                }
                else println("Votre code doit contenir exactement 6 chiffres. Réessayez.")
              }
            }
            else if (demandeDeChangementDeCodePin == 2) {
              flagChoixAdmin = 1
            }
            else {
              flagChoixAdmin = 2
            }
          }

          Methodefonction.restockMachine(choixMachine, tableauCoffeeStocks,
            tableauSugarStocks, tableauMilkStocks, flagChoixAdmin)
        }
      }

      //Fin du mode Admin

      //Début du mode Quitter
      else if (commandeMode == (3)) {
        println("Vous avez quitté le programme.");
        flagMode = false
      }
      //Fin du mode quitter

      // Si la saisie est erronée, on retourne au menu initial où il faut choisir entre le mode Admin,
      //le mode Client et le mode Quitter.
      else {
        println("Saisie erronée. Veuillez sélectionner le nombre 1, 2 ou 3.");
      }
    }
  }
}









