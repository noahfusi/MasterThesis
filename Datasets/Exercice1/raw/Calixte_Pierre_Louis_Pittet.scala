import scala.io.StdIn.readLine
import scala.util.Random

// NospressoMachine simule une machine a café.
object NospressoMachine {
  // Store initial ingredient stocks
  var coffeePowder = 50.0 // in grams
  var sugar = 30.0 // in grams
  var milk = 500.0 // in ml (0.5L = 500ml)

  // Admin PIN
  val ADMIN_PIN = "434343"

  // Point d'entrée principal du programme
  def main(args: Array[String]): Unit = {
    while (true) {
      displayMainMenu()
      val choice = readLine("> ")

      if (choice == "1") {
        clientMode()
      } else if (choice == "2") {
        adminMode()
      } else if (choice == "3") {
        println("Merci d'avoir utilisé Nospresso. Au revoir!")
        sys.exit(0)
      } else {
        println("Choix invalide, veuillez réessayer.")
      }
    }
  }


  // Afficher le menu principal
  def displayMainMenu(): Unit = {
    println("Nospresso Cafe")
    println("Veuillez sélectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")
  }

  // Mode client
  def clientMode(): Unit = {
    println("\nVeuillez sélectionner votre boisson :")
    println("1) Espresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val beverageChoice = readLine("> ")

    // Variables pour stocker les détails des boissons
    val (beverage, size, defaultMilk) = beverageChoice match {
      case "1" => ("Espresso", "Standard", 0.0) // Espresso n'a pas de lait
      case "2" => ("Cappuccino", "Standard", 100.0) // Cappuccino a du lait en standard
      case "3" =>
        // Latte : différentes taille avec quantité de lait différent
        println("\nSelect size:")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        val sizeChoice = readLine("> ")
        val size = sizeChoice match {
          case "1" => "Petit"
          case "2" => "Moyen"
          case "3" => "Grand"
          case _ => return // Quitter si l'utilisateur choisit une option invalide
        }
        val defaultMilk = size match {
          case "Petit" => 120.0
          case "Moyen" => 150.0
          case "Grand" => 200.0
        }
        ("Latte", size, defaultMilk)
      case _ => return // Quitter si l'utilisateur choisit une boisson invalide
    }

    // Check si les ingrédients sont disponibles
    val (requiredCoffee, requiredMilk) = getIngredientRequirements(beverage, size, defaultMilk)
    if (coffeePowder < requiredCoffee) {
      println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en Admin.")
      return
    }
    if (milk < requiredMilk) {
      println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      return
    }

    // Sélection de sucre
    println("\nSouhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    val sugarChoice = readLine("> ")
    val sugarAmount = sugarChoice match {
      case "1" => 0
      case "2" => 5
      case "3" => 10
      case "4" => 15
      case _ => return // Quitter si l'utilisateur choisit une mauvaise option
    }

    // Check si assez de sucre dispo
    if (sugarAmount > sugar) {
      println("Erreur: Quantité de sucre insuffisante.")
      return
    }

    // Option pour ajouter du lait en supp pour Capuccino et Latte
    var extraMilkDoses = 0
    if (beverage != "Espresso") {
      println("\nSouhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")

      if (readLine("> ") == "1") {
        println("Combien de dose doses? (1-3)")
        extraMilkDoses = readLine("> ").toInt
        if (extraMilkDoses < 1 || extraMilkDoses > 3) return // Check pour dose valide ou non
        if (milk < requiredMilk + (extraMilkDoses * 50)) {
          println("Erreur: Quantité de lait insuffisante pour la ou les dose(s) supplémentaires.")
          return
        }
      }
    }

    // Calcule du prix total après la commande
    val basePrice = getBasePrice(beverage, size)
    val sugarPrice = getSugarPrice(sugarAmount)
    val extraMilkPrice = extraMilkDoses * 0.05
    val totalPrice = basePrice + sugarPrice + extraMilkPrice

    // Afficher le récap de la commande
    println(s"\nBoisson sélectionnée: $beverage ${if(size != "Standard") s"($size)" else ""}")
    println(s"Niveau de sucre : ${getSugarLevel(sugarAmount)}")
    if (extraMilkDoses > 0) println(s"Lait en supplément: $extraMilkDoses dose(s)")
    println(f"Prix total: CHF $basePrice + CHF $sugarPrice + $extraMilkPrice%.2f = CHF $totalPrice%.2f")

    // processus du paiement
    processPayment(totalPrice)

    // MAJ du stock et préparation de la boisson
    updateStocks(beverage, size, sugarAmount, defaultMilk, extraMilkDoses)
    prepareBeverage(beverage, size)
  }

  // mode Admin pour vérifier et rajouter des ingédients
  def adminMode(): Unit = {
    println("Entrez le code PIN:")
    val pin = readLine("> ")

    if (pin != ADMIN_PIN) {
      println("Code PIN incorrect ")
      return
    }

    println("\nStocks:")
    println(f"Poudre de café: ${coffeePowder}%.1fg")
    println(f"Lait: ${milk/1000}%.1fL")
    println(f"Sucre: ${sugar}%.1fg")

    // Admin ajout stock
    println("\nRéapprovisionnement des stocks ... Ajout :")
    println("Poudre de café (g):")
    val addCoffee = readLine("> ").toDouble
    println("Lait (L):")
    val addMilk = readLine("> ").toDouble
    println("Sucre (g):")
    val addSugar = readLine("> ").toDouble

    // MAJ stock
    coffeePowder += addCoffee
    milk += addMilk * 1000 // Convertir litres à millilitres
    sugar += addSugar

    println("\nNiveau de stock mis à jour.")
    println("Retour au menu principal...")
  }

  // Calcule le café et le lait recquis pour la boisson
  def getIngredientRequirements(beverage: String, size: String, defaultMilk: Double): (Double, Double) = {
    beverage match {
      case "Espresso" => (8.0, 0.0)
      case "Cappuccino" => (6.0, defaultMilk)
      case "Latte" => (
        size match {
          case "Petit" => 6.0
          case "Moyen" => 8.0
          case "Grand" => 12.0
        },
        defaultMilk
      )
    }
  }

  // Retour aux prix de base
  def getBasePrice(beverage: String, size: String): Double = {
    beverage match {
      case "Espresso" => 2.00
      case "Cappuccino" => 2.50
      case "Latte" => size match {
        case "Petit" => 2.70
        case "Moyen" => 3.20
        case "Grand" => 3.70
      }
    }
  }

  //Cout en fonction du niveau de sucre
  def getSugarPrice(sugarAmount: Int): Double = {
    sugarAmount match {
      case 0 => 0.00
      case 5 => 0.10
      case 10 => 0.20
      case 15 => 0.30
    }
  }

  // Renvoie une chaine pour la description du niveau de sucre
  def getSugarLevel(sugarAmount: Int): String = {
    sugarAmount match {
      case 0 => "Sans sucre"
      case 5 => "Peu (5g)"
      case 10 => "Moyen (10g)"
      case 15 => "Beaucoup (15g)"
    }
  }

  // Simule le paiement et le délai
  def processPayment(amount: Double): Unit = {
    val code = Random.alphanumeric.take(5).mkString
    println(s"\nVeuillez payer en utilisant Twint.")
    println(s"Votre code de paiement est : $code")
    println("(En attente de confirmation...)")
    Thread.sleep(2000) // Simule le déalais
    println("Paiement confirmé.")
  }

  // MAJ après préparation de la boisson
  def updateStocks(beverage: String, size: String, sugarAmount: Int, defaultMilk: Double, extraMilkDoses: Int): Unit = {
    val (requiredCoffee, requiredMilk) = getIngredientRequirements(beverage, size, defaultMilk)
    coffeePowder -= requiredCoffee
    milk -= requiredMilk + (extraMilkDoses * 50)
    sugar -= sugarAmount
  }

  // Simule la préparation de la boisson
  def prepareBeverage(beverage: String, size: String): Unit = {
    println(s"\nPréparation de votre $beverage ${if(size != "Standard") s"($size)" else ""}...")
    Thread.sleep(2000) // Simule le délais pour la préparation
    println(s"Votre $beverage est prêt ! bonne dégustation !\n")
  }
}