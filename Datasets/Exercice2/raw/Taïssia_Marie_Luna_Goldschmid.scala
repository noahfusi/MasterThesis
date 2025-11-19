import scala.io.StdIn.readLine
import scala.util.Random
import scala.language.postfixOps
import scala.math.{min, random}
import scala.reflect.internal.NoPhase
import scala.reflect.internal.NoPhase.{<, >}
import scala.reflect.internal.NoPhase.assignsFields.||
import scala.reflect.internal.util.TriState.True
object Main {
  // main
  def main(args: Array[String]): Unit = {
    var cafe = 50;
    var sucre = 30;
    var lait = 0.500;
    var taille = "";
    var prix = 0.00;
    var prixsucre = 0.00;
    var prixlait = 0.00;
    var cmblait = "";
    var laitsupp = "";

    // nbmachines
    val nbmachine: Int = 5

    // stocks machines
    var coffeeStocks: Array[Int] = Array(100, 200, 300, 400, 500) // stocks de cafe
    var sugarStocks: Array[Int] = Array(150, 250, 350, 450, 550) // stocks de sucre
    var milkStocks: Array[Int] = Array(300, 200, 500, 400, 600) // stocks de lait

    // machine pins
    var machinePins: Array[String] = Array("434343", "434343", "434343", "434343", "434343")

    // validate pin
    def validatePin(machineId: Int): Boolean = {
      var tentatives = 3
      val correctPin = machinePins(machineId)

      while (tantatives > 0) {
        val pin = readLine("Entrez le code Pin: 434343")
        if (pin == correctPin) {
          return true
        } else {
          tentatives -= 1
          println("Code Pin incorrect, il vous reste 2 tentstives")
        }
      }
      println("Tentatives épuisées: programme terminé")
      System.exit(0) // fin programme si tentatives épuisées
      false
    }

    // update pin
    def updatePin(machineId: Int): Unit = {
      val pin = 6
      var nouveauPin = readLine("Entrez nouveau code pin: 6 chiffres")

      machinePins(machineId) = nouveauPin
      println("Code Pin de la machine mis à jour")
    }

    // serve client
    def serveClient(machinbeId: Int): Unit = {
      println("\n Vous utilisez la machine: machineId")

      // choix boisson
      println("Choississez votre boisson")
      println("\n 1) Expresso - CHF 2.00")
      println("\n 2) Cappuccion - CHF 2.50")
      println("\n 3) Latte \n >")
      val choix = readLine("Choix (1/2/3)")
      var boisson = readLine()
      if (boisson == "3") {
        println("Quel taille")
        println(" \n 1) Petit - CHF 2.70")
        println("\n 2) Moyen - CHF 3.20")
        println("\n 3) Grand - CHF 3.70")
        val taille = readLine("Taille (1/2/3)")
        println("Ajouter du sucre : \n 1) Pas de sucre \n 2) Peu de sucre (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - 0.30")
        var cmbsucre = readLine()
        if (boisson == "2" || taille == "1" || taille == "2" || taille == "3") {
          println("Souhaitez-vous du lait en supplément ? \n 1) Oui \n 2) Non \n >")
          laitsupp = readLine()
          if (laitsupp == "2") {
            laitsupp = "Non";
          }
          if (laitsupp == "1") {
            laitsupp = "Oui";
            println("Ajouter du lait : \n 1) une dose (50ml) - CHF 0.05 \n 2) deux doses (100ml)- CHF 0.10 \n 3) trois doses (150ml) - CHF 0.15")
            cmblait = readLine()
          }
        }
        if (boisson == "1") {
          cafe = cafe - 8;
          prix = prix - 2.00;
          boisson = "Expresso";
        }
        if (boisson == "2") {
          cafe = cafe - 6;
          lait = lait - 0.100;
          prix = prix + 2.50;
          boisson = "Cappuccino";
        }
        if (boisson == "3" && taille == "1") {
          cafe = cafe - 6;
          lait = lait - 0.120;
          prix = prix + 2.70;
        }
        if (boisson == "3" && taille == "2") {
          cafe = cafe - 8;
          lait = lait - 0.150;
          prix = prix + 3.20;
        }
        if (boisson == "3" && taille == "3") {
          cafe = cafe - 12;
          lait = lait - 0.200;
          prix = prix + 3.70;
        }
        if (cmbsucre == "1") {
          sucre = sucre - 0;
          cmbsucre = "Pas de sucre";
        }
        if (cmbsucre == "2") {
          sucre = sucre - 5;
          prixsucre = prixsucre + 0.10;
          cmbsucre = "Peu";
        }
        if (cmbsucre == "3") {
          sucre = sucre - 10;
          prixsucre = prixsucre + 0.20;
          cmbsucre = "Moyen";
        }
        if (cmbsucre == "4") {
          sucre = sucre - 15;
          prixsucre = prixsucre + 0.30;
          cmbsucre = "Beaucoup";
        }
        if (cmblait == "1") {
          lait = lait - 0.05;
          cmblait = "50ml";
          prixlait = prixlait + 0.05;
        }
        if (cmblait == "2") {
          lait = lait - 0.10;
          cmblait = "100ml";
          prixlait = prixlait + 0.10;
        }
        if (cmblait == "3") {
          lait = lait - 0.15;
          cmblait = "150ml";
          prixlait = prixlait + 0.15;
        }

        // restock machine
        def restockMachine(machineId: Int): Unit = {
          println("Reapprovisionnement machine: machineId")

          var coffeeToadd = readLine("Quantité à ajouter (en gr)").toInt
          while (coffeeToadd <= 0) {
            println("Quantité positive")
            coffeeToadd = readLine("Quantité à ajouter (en gr)").toInt
          }

          var sugarToadd = readLine("Quantité à ajouter (en gr)").toInt
          while (sugarToadd <= 0) {
            println("Quantité positive")
            sugarToadd = readLine("Quantité à ajouter (en gr)").toInt
          }

          var milkToadd = readLine("Quantité à ajouter (en ml)").toInt
          while (milkToadd <= 0) {
            println("Quantité positive")
            milkToadd = readLine("Quantité à ajouter (en ml)").toInt
          }

          coffeeStocks(machineId) += coffeeToadd
          sugarStocks(machineId) += sugarToadd
          milkStocks(machineId) += milkToadd

          println("Restock effectué, nouveau stock pour: machineId")
          println("Café:coffeeStocks8machineId)g, Sucre:sugarStocks(machineId)g, Lait:milkStocks(machineId)ml")
        }

        // paiement
        println("Paiement : \n")
        val twint = (math.random * (99999 - 0) + 0).toInt
        println("Boisson séléctionnée : " + boisson + "\n Niveau de sucre : " + cmbsucre)
        if (boisson == "2" || boisson == "3") {
          println("\n Lait supplémentaire : " + laitsupp)
        }
        if (lait == 0 || cafe == 0 || sucre == 0) {
          if (lait == 0) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          if (cafe == 0) {
            println("Erreur : Quantité de cafe insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          if (sucre == 0) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          System.exit(0)
        }
        var prixtotal = prix + prixsucre + prixlait;
        println("Prix total : CHF " + prix + " + CHF " + prixsucre + " + CHF " + prixlait + " = " + prixtotal)
        println("\n Veuillez payer en utilisant twint")
        println("Paiement via Twint, le code : " + twint)
        println("(en attente de validation du payement...)")
        Thread.sleep(3000)
        println("Merci! votre payement a été accepté")
        println("Préparation de votre boisson...")
        println("[...]")
        if (boisson == "1") {
          println("Votre expresso est prêt! bonne dégustation!")
        }
      }
      if (boisson == "2") {
        println("Votre cappuccino est prêt! bonne dégustation!")
      }
      if (boisson == "3") {
        println("Votre latte est prêt! bonne dégustation!")

        // mode client & admin
        def main(args: Array[String]): Unit = {
          println("Nospresso cafe!")

          while (true) {
            println("\n Choisissez le mode")
            println("1. mode client")
            println("2. mode admin")
            println("3. quitter")
            val choice = readLine("Vhoix (1/2/3)")

            // mode admin
            val machineId = readLine("Sélectionner machine (0à4)").toInt
            if (machineId >= 0 && machineId < nbmachine) {
              if (validatePin((machineId))) {
                println("1. mettre à jour Pin")
                println("2. restock machine")
                val adminChoice = readLine("Choix (1/2)")
              } else {
                println("Id machine invalide")
              }

              // quitter
              println("Au revoir!")
              System.exit(0)
            }
          }
        }
      } else {

      }
    } : Unit
  }
}







