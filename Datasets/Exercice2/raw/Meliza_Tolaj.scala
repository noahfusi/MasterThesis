import scala.util.Random
import scala.io.StdIn
import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var continue = true //on déclare les variables de bases
    var sucrest = 30
    val pincode =  434343
    var cafest = 50
    var laitst = 500
    val nbMachines = 5
    var cafeStock = Array.fill(5)(50)
    var sucreStock = Array.fill(5)(30)
    var laitStock = Array.fill(5)(500)
    var machinePins = Array.fill(5)("434343")


    /* Valide le pin pour la machine selectionnée */
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      /*Initialise le nombre d'essais */
      var essais = 3
      /* Initalise une valeur booléenne afin de savoir quand le pin est valide*/
      var validPin = false
      while (essais > 0 && !validPin) {
        println(s"Entrez le code PIN pour la machine $machineId :")
        val enteredPin = StdIn.readLine()
        if (enteredPin == machinePins(machineId - 1)) {
          println("Accès accordé.")
          validPin = true
        } else {
          essais -= 1
          println(s"Code PIN incorrect. $essais tentative(s) restante(s).")
        }
      }
      if (!validPin) {
        println("Trop de tentatives échouées. Fin du programme.")
      }
      validPin
    }
    /* Met à jour le Pin de la machine selectiionnée */
    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      /* Initialise une valeur booléenne pour avoir un pin valide*/

      var validPin = false
      while (!validPin) {
        val nouveauPin = StdIn.readLine()

        /* La taille du pin doit être de 6 et doit être que des chiffres */
        if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
          machinePins(machineId -  1) = nouveauPin
          println(s"Le code PIN a été mis à jour pour la machine $machineId avec succès.")
          validPin = true
        } else {
          println("Le code PIN doit comporter exactement 6 chiffres. Réessayez :")
        }
      }
    }


    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      var choixmenu = 1
      var success = false // Indique si la transaction a réussi

      while (choixmenu == 1) { // Cas menu client
        // Initialisation des variables
        var quantiteSucre = 0
        var sucreLettre = ""
        var coutCafe = 0
        var coutLait = 0
        var coutSucre = 0
        var nomBoisson = ""
        var prixBoisson = 0.0
        var prixSucre = 0.0
        var prixLait = 0.0
        var prixTotal = 0.0
        var choixBoisson = 0

        // Sélection de la boisson
        while (!(choixBoisson == 1 || choixBoisson == 2 || choixBoisson == 3)) {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          choixBoisson = scala.io.StdIn.readInt()
        }

        // Configuration de la boisson
        if (choixBoisson == 1) { // Expresso
          prixBoisson = 2.00
          nomBoisson = "Expresso"
          coutCafe = 8
        } else if (choixBoisson == 2) { // Cappuccino
          prixBoisson = 2.5
          nomBoisson = "Cappuccino"
          coutCafe = 6
          coutLait = 100
        } else if (choixBoisson == 3) { // Latte
          println("Quelle taille souhaitez-vous pour votre Latte ?")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          print("> ")
          val tailleLatte = scala.io.StdIn.readInt()

          if (tailleLatte == 1) {
            prixBoisson = 2.7
            coutLait = 120
            coutCafe = 6
            nomBoisson = "Latte (Petit)"
          } else if (tailleLatte == 2) {
            prixBoisson = 3.2
            coutLait = 150
            coutCafe = 8
            nomBoisson = "Latte (Moyen)"
          } else if (tailleLatte == 3) {
            prixBoisson = 3.7
            coutCafe = 12
            coutLait = 200
            nomBoisson = "Latte (Grand)"
          }
        }
        if (choixBoisson == 2 || choixBoisson == 3) {
          var laitPlus = 0
          while (!(laitPlus == 1 || laitPlus == 2)) {
            println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui \n2) Non \n>")
            laitPlus = readInt()
          }
          if (laitPlus == 1) {
            var doseLait = 0
            while (!(doseLait == 1 || doseLait == 2 || doseLait == 3)) {
              print("Combien de doses ? (max 3) \n>")
              doseLait = readLine().toInt
            }
            coutLait += 50 * doseLait
            prixLait = doseLait * 0.05
          }
        }

        // Ajouter du sucre
        while (!(quantiteSucre == 1 || quantiteSucre == 2 || quantiteSucre == 3 || quantiteSucre == 4)) {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          quantiteSucre = scala.io.StdIn.readInt()
        }

        prixSucre = (quantiteSucre - 1) * 0.1
        if (quantiteSucre == 1) {
          sucreLettre = "Sans sucre"
          coutSucre = 0
        } else if (quantiteSucre == 2) {
          coutSucre = 5
          sucreLettre = "Peu (5g)"
        } else if (quantiteSucre == 3) {
          coutSucre = 10
          sucreLettre = "Moyen (10g)"
        } else if (quantiteSucre == 4) {
          coutSucre = 15
          sucreLettre = "Beaucoup (15g)"
        }

        if (coffeeStocks(machineId - 1) < coutCafe || milkStocks(machineId - 1) < coutLait || sugarStocks(machineId - 1) < coutSucre) {
          println(s"Erreur : Stock insuffisant pour cette machine (Café: ${coffeeStocks(machineId - 1)}g, Lait: ${milkStocks(machineId - 1)}ml, Sucre: ${sugarStocks(machineId - 1)}g). Souhaitez-vous essayer une autre machine ? (Oui = 1 / Non = 2)")
          val choix = scala.io.StdIn.readInt()
          if (choix == 1) {
            println(s"Entrez l'ID d'une autre machine (1 à $nbMachines) :")
            val newMachineId = scala.io.StdIn.readInt()
            if (newMachineId >= 1 && newMachineId <= nbMachines && newMachineId != machineId) {
              return serveClient(newMachineId, coffeeStocks, sugarStocks, milkStocks)
            } else {
              println("ID de machine invalide ou identique. Retour au menu principal.")
              return false
            }
          } else {
            println("Retour au menu principal.")
            return false
          }
        } else {
          // Calcul du prix total et paiement
          prixTotal = prixBoisson + prixSucre
          println(f"Prix total : CHF $prixBoisson%.2f + CHF $prixSucre%.2f = CHF $prixTotal%.2f")

          val alphabetNum = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          var codeTwint = ""
          for (_ <- 1 to 5) {
            val index = (Math.random() * alphabetNum.length).toInt
            codeTwint += alphabetNum(index)
          }
          println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $codeTwint")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println(s"Votre $nomBoisson avec $sucreLettre est prêt ! Bonne dégustation !")

          // Mise à jour des stocks
          coffeeStocks(machineId - 1) -= coutCafe
          sugarStocks(machineId - 1) -= coutSucre
          milkStocks(machineId - 1) -= coutLait
          success = true
        }

        // Demander si le client veut commander une autre boisson

        choixmenu = 0
      }

      success
    }


    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println(s"Réapprovisionnement de la machine $machineId.")
      println(s"Niveaux de stock actuels :\nCafé : ${coffeeStocks(machineId - 1)}g\nSucre : ${sugarStocks(machineId - 1)}g\nLait : ${milkStocks(machineId - 1)}L")
      /* Initialise les valeurs à ajouter dans la machine */
      var cafe_ajouter = -1
      var sucre_ajouter = -1
      var lait_ajouter = -1

      while (cafe_ajouter < 0) {
        println("Entrez la quantité de café à ajouter (en grammes, positif) :")
        cafe_ajouter = scala.io.StdIn.readInt()
        if (cafe_ajouter < 0) println("La quantité doit être positive. Réessayez.")
      }

      while (sucre_ajouter < 0) {
        println("Entrez la quantité de sucre à ajouter (en grammes, positif) :")
        sucre_ajouter = scala.io.StdIn.readInt()
        if (sucre_ajouter < 0) println("La quantité doit être positive. Réessayez.")
      }

      while (lait_ajouter < 0) {
        println("Entrez la quantité de lait à ajouter (en litres, positif) :")
        lait_ajouter = scala.io.StdIn.readInt()
        if (lait_ajouter < 0) println("La quantité doit être positive. Réessayez.")
      }


      coffeeStocks(machineId - 1) =  coffeeStocks(machineId - 1) + cafe_ajouter
      sugarStocks(machineId - 1) =  sugarStocks(machineId - 1) + sucre_ajouter
      milkStocks(machineId - 1) = milkStocks(machineId - 1) + lait_ajouter

      println("Les stocks ont été mis à jour avec succès.")
      println("Retour au menu principal...")
    }


    while(continue){ //tant qu'on continue (que le choixmenu n'est pas egal à 3)
      println("\tNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val choix = StdIn.readInt()

      if (choix == 1) {
        // Mode client
        var machineId = -1
        while (machineId < 1 || machineId > nbMachines) {
          println(s"Entrez l'ID de la machine (1 à ${nbMachines}) :")
          machineId = StdIn.readInt()
          if (machineId < 1 || machineId > nbMachines) {
            println("ID de machine invalide. Veuillez réessayer.")
          }
        }
        serveClient(machineId, cafeStock, sucreStock, laitStock)

      } else if (choix == 2) {
        // Mode admin
        var machineId = -1
        // Tant que l'ID de la machine est invalide, continuer à demander
        while (machineId < 1 || machineId > nbMachines) {
          println(s"Entrez l'ID de la machine (1 à ${nbMachines}) :")
          machineId = StdIn.readInt()
          if (machineId < 1 || machineId > nbMachines) {
            println("ID de machine invalide. Veuillez réessayer.")
          }
        }

        // Une fois un ID valide, demander le PIN
        if (validatePin(machineId, machinePins)) {
          var action = 0
          // Tant que l'action n'est pas valide, continuer à demander
          while (action != 1 && action != 2) {
            println("1) Réapprovisionner")
            println("2) Mettre à jour le PIN")
            print("> ")
            action = StdIn.readInt()
            if (action != 1 && action != 2) {
              println("Action invalide. Veuillez choisir 1 ou 2.")
            }
          }

          // Exécuter l'action choisie
          if (action == 1) {
            restockMachine(machineId, cafeStock, sucreStock, laitStock)
          } else if (action == 2) {
            updatePin(machineId, machinePins)
          }
        } else {
          continue = false
          println("PIN invalide. Accès refusé.")
        }
      }else if (choix == 3) {
        // Quitter
        continue = false
      } else {
        println("Choix invalide.")
      }
    }
  }
}