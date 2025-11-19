import scala.io.StdIn
import scala.util.Random

object Nospresso {
  // PIN Admin
  val adminPin = "434343"

  // Stocks initiaux
  var stockCafe = 50 // en grammes
  var stockSucre = 30 // en grammes
  var stockLait = 0.5 // en litres

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      afficherMenuPrincipal()
      StdIn.readLine("> ").trim match {
        case "1" => modeClient()
        case "2" => modeAdmin()
        case "3" => 
          println("Merci d'avoir utilisé Nospresso. À bientôt !")
          continuer = false
        case _ =>
          println("Entrée invalide. Veuillez choisir une option valide.")
      }
    }
  }

  def afficherMenuPrincipal(): Unit = {
    println("\nNospresso Café")
    println("Veuillez sélectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")
  }

  def modeClient(): Unit = {
    println("\n=== Mode Client ===")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte (Petit: CHF 2.70, Moyen: CHF 3.20, Grand: CHF 3.70)")
    println("4) Retour au menu principal")

    StdIn.readLine("> ").trim match {
      case "1" => commanderBoisson("Expresso", cafe = 8, lait = 0, sucre = selectionSucre(), prixBase = 2.0)
      case "2" => commanderBoisson("Cappuccino", cafe = 6, lait = 0.1, sucre = selectionSucre(), prixBase = 2.5, laitSup = selectionLaitSup())
      case "3" => commanderLatte()
      case "4" => println("Retour au menu principal.")
      case _ =>
        println("Entrée invalide. Veuillez sélectionner une option valide.")
        modeClient()
    }
  }

  def commanderLatte(): Unit = {
    println("\nVeuillez sélectionner la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    println("4) Retour")

    StdIn.readLine("> ").trim match {
      case "1" => commanderBoisson("Latte (Petit)", cafe = 6, lait = 0.12, sucre = selectionSucre(), prixBase = 2.7, laitSup = selectionLaitSup())
      case "2" => commanderBoisson("Latte (Moyen)", cafe = 8, lait = 0.15, sucre = selectionSucre(), prixBase = 3.2, laitSup = selectionLaitSup())
      case "3" => commanderBoisson("Latte (Grand)", cafe = 12, lait = 0.2, sucre = selectionSucre(), prixBase = 3.7, laitSup = selectionLaitSup())
      case "4" => modeClient()
      case _ =>
        println("Entrée invalide. Veuillez sélectionner une option valide.")
        commanderLatte()
    }
  }

  def selectionSucre(): Int = {
    println("\nSouhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    StdIn.readLine("> ").trim match {
      case "1" => 0
      case "2" => 5
      case "3" => 10
      case "4" => 15
      case _ =>
        println("Entrée invalide. Veuillez sélectionner un niveau de sucre valide.")
        selectionSucre()
    }
  }

  def selectionLaitSup(): Int = {
    println("\nSouhaitez-vous ajouter du lait en supplément ? (1 dose = 50ml, max 3 doses)")
    println("1) Oui")
    println("2) Non")

    StdIn.readLine("> ").trim match {
      case "1" =>
        println("Combien de doses ? (1-3)")
        StdIn.readLine("> ").trim.toIntOption match {
          case Some(doses) if doses >= 1 && doses <= 3 => doses
          case _ =>
            println("Entrée invalide. Veuillez choisir entre 1 et 3 doses.")
            selectionLaitSup()
        }
      case "2" => 0
      case _ =>
        println("Entrée invalide. Veuillez sélectionner une option valide.")
        selectionLaitSup()
    }
  }

  def commanderBoisson(nom: String, cafe: Int, lait: Double, sucre: Int, prixBase: Double, laitSup: Int = 0): Unit = {
    val prixLaitSup = laitSup * 0.05
    val prixSucre = sucre match {
      case 5 => 0.10
      case 10 => 0.20
      case 15 => 0.30
      case _ => 0.0
    }
    val prixTotal = prixBase + prixLaitSup + prixSucre
    val laitTotal = lait + laitSup * 0.05

    if (stockCafe < cafe) {
      println(s"Erreur : Stock insuffisant de café pour préparer $nom.")
    } else if (stockLait < laitTotal) {
      println(s"Erreur : Stock insuffisant de lait pour préparer $nom.")
    } else if (stockSucre < sucre) {
      println(s"Erreur : Stock insuffisant de sucre pour préparer $nom.")
    } else {
      println(s"Boisson sélectionnée : $nom")
      println(s"Niveau de sucre : ${if (sucre == 0) "Sans sucre" else s"$sucre g"}")
      println(f"Prix total : CHF $prixTotal%.2f")
      attendrePaiement()
      stockCafe -= cafe
      stockLait -= laitTotal
      stockSucre -= sucre
      println(s"Votre $nom est prêt ! Bonne dégustation !")
    }
  }

  def attendrePaiement(): Unit = {
    val codePaiement = Random.alphanumeric.take(5).mkString
    println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codePaiement")
    println("(En attente de paiement...)")
    Thread.sleep(3000) // Attend 3 secondes
    println("Paiement confirmé.")
  }

  def modeAdmin(): Unit = {
    println("\n=== Mode Admin ===")
    println("Veuillez entrer le code PIN :")
    val pin = StdIn.readLine("> ").trim
    if (pin == adminPin) {
      println("Accès autorisé. Gestion des stocks :")
      afficherStocks()
      reapprovisionner()
    } else {
      println("Code PIN incorrect. Retour au menu principal.")
    }
  }

  def afficherStocks(): Unit = {
    println(f"Stocks actuels : Café ($stockCafe g), Sucre ($stockSucre g), Lait ($stockLait%.2f L)")
  }

  def reapprovisionner(): Unit = {
    println("Quantité de café à ajouter :")
    val cafeAjoute = StdIn.readLine("> ").trim.toIntOption.getOrElse(0)
    println("Quantité de sucre à ajouter :")
    val sucreAjoute = StdIn.readLine("> ").trim.toIntOption.getOrElse(0)
    println("Quantité de lait à ajouter (en litres) :")
    val laitAjoute = StdIn.readLine("> ").trim.toDoubleOption.getOrElse(0.0)
    stockCafe += cafeAjoute
    stockSucre += sucreAjoute
    stockLait += laitAjoute
    println("Stocks mis à jour.")
    afficherStocks()
  }
}