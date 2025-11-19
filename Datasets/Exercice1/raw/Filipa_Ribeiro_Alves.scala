import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  // Le stock initial du distributeur
  // Quantité en grammes
  var coffeePowder= 50.0
  var sugar = 30.0
  // Quantité en litre
  var milk = 0.5
  //// Code PIN pour l'accès admin
  val adminPin = "434343"

  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      // Menu principal
      println("Nospresso Café\n")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter\n>")
      val choice = readLine()

      // Gestion des choix utilisateur pour le mode
      if (choice == "1") clientMode()//choix du mode client
      else if (choice == "2") adminMode()//choix du mode admin
      else if (choice == "3") running = false//choix du mode quitter le programme
      //validation
      else println("Entrée invalide, veuillez réessayer.")
    }
    println("Merci d'avoir utilisé Nospresso. Au revoir !")
  }


  def clientMode(): Unit = {
    // Menu sélection des boissons
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
    val drinkChoice = readLine()

    // Initialisation par défaut pour la boisson
    var basePrice = 0.0
    var coffeeNeeded = 0.0
    var defaultMilkNeeded = 0.0

    // Détermination des besoins en fonction de la boisson choisie
    //Expresso
    if (drinkChoice == "1") {
      basePrice = 2.0
      coffeeNeeded = 8.0
      defaultMilkNeeded = 0.0
      //Cappucino
    } else if (drinkChoice == "2") {
      basePrice = 2.5
      coffeeNeeded = 6.0
      defaultMilkNeeded = 0.1
      //Latte
    } else if (drinkChoice == "3") {
      //choix taille du Latte
      println("Choisissez une taille : 1) Petit 2) Moyen 3) Grand\n>")
      val sizeChoice = readLine()
      //Latte petit
      if (sizeChoice == "1") {
        basePrice = 2.7
        coffeeNeeded = 6.0
        defaultMilkNeeded = 0.12
        //Latte moyen
      } else if (sizeChoice == "2") {
        basePrice = 3.2
        coffeeNeeded = 8.0
        defaultMilkNeeded = 0.15
        //latte grand
      } else if (sizeChoice == "3") {
        basePrice = 3.7
        coffeeNeeded = 12.0
        defaultMilkNeeded = 0.2
      } else {
        println("Taille invalide, retour au menu principal.")
        return
      }
    } else {
      println("Choix invalide, retour au menu principal.")
      return
    }


    //choix d'ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30\n>")
    val sugarChoice = readLine()

    var sugarNeeded = 0.0
    var sugarPrice = 0.0

    // Détermination de la quantité et du prix du sucre
    if (sugarChoice == "1") {
      sugarNeeded = 0.0
      sugarPrice = 0.0
    } else if (sugarChoice == "2") {
      sugarNeeded = 5.0
      sugarPrice = 0.1
    } else if (sugarChoice == "3") {
      sugarNeeded = 10.0
      sugarPrice = 0.2
    } else if (sugarChoice == "4") {
      sugarNeeded = 15.0
      sugarPrice = 0.3
    } else {
      println("Choix invalide, retour au menu principal.")
      return
    }


    //Choix d'ajout de lait
    var milkNeeded = defaultMilkNeeded
    // café avec option de lait
    if (milkNeeded > 0) {
      println("Souhaitez-vous ajouter du lait ?")
      println("1) Oui")
      println("2) Non\n>")
      val milkChoice = readLine()
      if (milkChoice == "1") {
        // Si l'utilisateur souhaite ajouter du lait, combien de doses
        println("Combien de doses ? (1 à 3)")
        val doseChoice = readLine()
        if (doseChoice == "1" || doseChoice == "2" || doseChoice == "3") {
          val doses = doseChoice.toInt
          // Chaque dose contient 50 ml (0.05 litre)
          val extraMilkNeeded = doses * 0.05
          // Vérifie si le stock de lait est suffisant pour couvrir la demande
          if (milk >= milkNeeded + extraMilkNeeded) {
            milkNeeded += extraMilkNeeded
            basePrice += doses * 0.05 // Chaque dose coûte 0.05 CHF
          } else {
            println("Erreur : Quantité de lait insuffisante pour les doses demandées.")
            return
          }
        } else {
          println("Choix de doses invalide, retour au menu principal.")
          return
        }
      } else if (milkChoice != "2") {
        println("Choix invalide, retour au menu principal.")
        return
      }
    }


    // Calcul du prix total
    val totalPrice = basePrice + sugarPrice

    // Vérification des stocks avant préparation du café
    if (coffeePowder < coffeeNeeded) {
      //manque de café
      println("Erreur : Quantité de café insuffisante.")
      return
    }
    if (sugar < sugarNeeded) {
      //manque de sucre
      println("Erreur : Quantité de sucre insuffisante.")
      return
    }
    if (milk < milkNeeded) {
      //manque de lait
      println("Erreur : Quantité de lait insuffisante.")
      return
    }

    // Paiement
    printf("Prix total : CHF %.2f\n", totalPrice)
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + generatePaymentCode())
    Thread.sleep(3000) // Simulation du paiement

    // Préparation de la boisson
    println("Paiement confirmé.")
    prepareDrink(drinkChoice, sugarChoice, milkNeeded > 0)

    // Mise à jour des stocks
    coffeePowder -= coffeeNeeded
    sugar -= sugarNeeded
    milk -= milkNeeded
  }

  def prepareDrink(drink: String, sugar: String, withMilk: Boolean): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    val drinkName = if (drink == "1") "Expresso"
    else if (drink == "2") "Cappuccino"
    else if (drink == "3") "Latte"
    else "Boisson inconnue"
    printf("Votre %s est prêt ! Bonne dégustation !\n", drinkName)
    Thread.sleep(2000)
  }

  def adminMode(): Unit = {
    // mode admin pour gérer les stocks
    println("Entrez le code PIN :")
    val pin = readLine()
    if (pin == adminPin) {
      println("Accès autorisé.")
      // Affichage des stocks actuels
      printf("Stocks actuels :\nCafé : %.2f g\nSucre : %.2f g\nLait : %.2f L\n", coffeePowder, sugar, milk)
      println("Entrez les quantités à ajouter (en grammes ou litres) :")
      print("Café : ")
      coffeePowder += readLine().toDouble
      print("Sucre : ")
      sugar += readLine().toDouble
      print("Lait : ")
      milk += readLine().toDouble
      println("Stocks mis à jour.")
    } else {
      println("Code PIN incorrect.")
      Thread.sleep(2000)
    }
  }

  // Génère un code aléatoire pour le paiement
  def generatePaymentCode(): String = {
    Random.alphanumeric.take(5).mkString
  }
}

