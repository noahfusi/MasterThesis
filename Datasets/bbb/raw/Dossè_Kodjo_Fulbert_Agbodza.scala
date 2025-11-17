import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  val nbMachines = 5

  // Tableaux pour gérer les stocks et les codes PIN des machines
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50) // Café en grammes
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30)  // Sucre en grammes
  val milkStocks: Array[Double] = Array.fill(nbMachines)(0.5) // Lait en litres
  val machinePins: Array[String] = Array.fill(nbMachines)("434343") // PIN par défaut

  val random = new Random()
  val lettreetnumero = ('A' to 'Z') ++ ('0' to '9')

  def main(args: Array[String]): Unit = {
    var lancer = true
    while (lancer) {
      println("\n\n        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      readIntOption("> ") match {
        case Some(1) => modeClient()
        case Some(2) => modeAdmin()
        case Some(3) =>
          println("Au revoir et à bientôt !")
          lancer = false
        case _ => println("Choix invalide. Veuillez réessayer.")
      }
    }
  }

  // Mode Client
  def modeClient(): Unit = {
    println("Mode Client - Commandez votre boisson")
    var machineId = demanderChoix("Sélectionnez une machine (1-5) :", 1, nbMachines) - 1
    println(s"Machine ${machineId + 1} sélectionnée.")

    var continuer = true
    while (continuer) {
      println("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte\n4) Retour")
      readIntOption("> ") match {
        case Some(1) =>
          if (verifierStock(machineId, 8, 0, 0.0)) {
            traiterBoisson(machineId, "Expresso", 8, 0.0, 2.00, sucreInclus = true, laitInclus = false)
            continuer = false
          } else {
            println("Erreur : Stock insuffisant pour préparer un Expresso.")
            machineId = demanderChoix("Choisissez une autre machine (1-5) :", 1, nbMachines) - 1
          }
        case Some(2) =>
          if (gererCappuccino(machineId)) continuer = false
          else {
            machineId = demanderChoix("Choisissez une autre machine (1-5) :", 1, nbMachines) - 1
          }
        case Some(3) =>
          if (gererLatte(machineId)) continuer = false
          else {
            machineId = demanderChoix("Choisissez une autre machine (1-5) :", 1, nbMachines) - 1
          }
        case Some(4) =>
          println("Retour au menu principal.")
          continuer = false
        case _ => println("Choix invalide.")
      }
    }
  }

  // Fonction générique pour traiter une boisson
  def traiterBoisson(machineId: Int, nom: String, cafeNecessaire: Int, laitNecessaire: Double, prixBase: Double, sucreInclus: Boolean, laitInclus: Boolean): Unit = {
    val (sucreNecessaire, prixSucre) = if (sucreInclus) choisirSucre() else (0, 0.0)
    val prixSupplementLait = if (laitInclus) choisirSupplementLait() else 0.0

    if (verifierStock(machineId, cafeNecessaire, sucreNecessaire, laitNecessaire + prixSupplementLait)) {
      coffeeStocks(machineId) -= cafeNecessaire
      sugarStocks(machineId) -= sucreNecessaire
      milkStocks(machineId) -= (laitNecessaire + prixSupplementLait)
      val prixTotal = prixBase + prixSucre + prixSupplementLait
      afficherRecapitulatif(nom, prixBase, prixSucre, prixSupplementLait, prixTotal)
      effectuerPaiement()
      preparerBoisson(nom)
    } else {
      println(s"Erreur : Stock insuffisant pour préparer votre $nom. Choisissez une autre boisson.")
    }
  }

  def gererCappuccino(machineId: Int): Boolean = {
    val (sucreNecessaire, prixSucre) = choisirSucre()
    val prixSupplementLait = choisirSupplementLait()

    if (verifierStock(machineId, 6, sucreNecessaire, 0.1 + prixSupplementLait)) {
      coffeeStocks(machineId) -= 6
      sugarStocks(machineId) -= sucreNecessaire
      milkStocks(machineId) -= (0.1 + prixSupplementLait)
      val prixTotal = 2.50 + prixSucre + prixSupplementLait
      afficherRecapitulatif("Cappuccino", 2.50, prixSucre, prixSupplementLait, prixTotal)
      effectuerPaiement()
      preparerBoisson("Cappuccino")
      true
    } else {
      println("Erreur : Stock insuffisant.")
      false
    }
  }

  def gererLatte(machineId: Int): Boolean = {
    val tailleLatte = demanderChoix("Quelle taille de Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70", 1, 3)
    val (cafeRequis, laitRequis, prixBase) = tailleLatte match {
      case 1 => (6, 0.12, 2.70)
      case 2 => (8, 0.15, 3.20)
      case 3 => (12, 0.2, 3.70)
    }

    val (sucreNecessaire, prixSucre) = choisirSucre()
    val prixSupplementLait = choisirSupplementLait()

    if (verifierStock(machineId, cafeRequis, sucreNecessaire, laitRequis + prixSupplementLait)) {
      coffeeStocks(machineId) -= cafeRequis
      sugarStocks(machineId) -= sucreNecessaire
      milkStocks(machineId) -= (laitRequis + prixSupplementLait)
      val prixTotal = prixBase + prixSucre + prixSupplementLait
      afficherRecapitulatif("Latte", prixBase, prixSucre, prixSupplementLait, prixTotal)
      effectuerPaiement()
      preparerBoisson("Latte")
      true
    } else {
      println("Erreur : Stock insuffisant.")
      false
    }
  }

  // Mode Admin
  def modeAdmin(): Unit = {
    val machineId = demanderChoix("Sélectionnez une machine (1-5) :", 1, nbMachines) - 1
    if (validatePin(machineId)) {
      println(s"Accès accordé à la Machine ${machineId + 1}")
      println("1) Réapprovisionner\n2) Mettre à jour le code PIN\n3) Retour")
      readIntOption("> ") match {
        case Some(1) => restockMachine(machineId)
        case Some(2) => updatePin(machineId)
        case Some(3) => println("Retour au menu principal...")
        case _ => println("Choix invalide.")
      }
    } else println("Accès refusé. Retour au menu principal.")
  }

  def validatePin(machineId: Int): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN de la Machine ${machineId + 1}:")
      if (readLine() == machinePins(machineId)) return true
      tentatives -= 1
      println(s"PIN incorrect. ${tentatives} tentative(s) restante(s).")
    }
    println("Trop de tentatives échouées. Le programme va se fermer.")
    System.exit(0)  // Arrêt complet du programme
    false // Cette ligne ne sera jamais atteinte mais elle est nécessaire pour la méthode
  }

  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    val newPin = readLine()
    if (newPin.matches("\\d{6}")) {
      machinePins(machineId) = newPin
      println("Code PIN mis à jour avec succès.")
    } else println("Le code PIN n’a pas été mis à jour.\nRetour au menu principal...")
  }

  def restockMachine(machineId: Int): Unit = {
    println("Niveaux de stock actuels :")
    println("     Poudre de café: " + coffeeStocks(machineId) + "g")
    println("     Lait          : " + milkStocks(machineId) + "L")
    println("     Sucre         : " + sugarStocks(machineId) + "g")
    println("\nEntrez les quantités à ajouter :")
    coffeeStocks(machineId) += demanderQuantite("Café (g) :")
    sugarStocks(machineId) += demanderQuantite("Sucre (g) :")
    milkStocks(machineId) += demanderQuantiteDouble("Lait (L) :")
    println("Stocks mis à jour avec succès.")
  }

  def demanderQuantite(msg: String): Int = { print(msg); readIntOption("> ").getOrElse(0).max(0) }
  def demanderQuantiteDouble(msg: String): Double = { print(msg); readDoubleOption("> ").getOrElse(0.0).max(0.0) }

  def verifierStock(id: Int, cafe: Int, sucre: Int, lait: Double): Boolean =
    coffeeStocks(id) >= cafe && sugarStocks(id) >= sucre && milkStocks(id) >= lait

  def choisirSucre(): (Int, Double) = {
    val niveau = demanderChoix("Sucre :\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30", 1, 4)
    niveau match { case 1 => (0, 0.0); case 2 => (5, 0.10); case 3 => (10, 0.20); case 4 => (15, 0.30) }
  }

  def choisirSupplementLait(): Double = {
    val choix = demanderChoix("Ajouter du lait supplémentaire ?\n1) Oui\n2) Non", 1, 2)
    if (choix == 1) demanderChoix("Doses de lait :\n1) 50ml\n2) 100ml\n3) 150ml", 1, 3) match {
      case 1 => 0.05; case 2 => 0.10; case 3 => 0.15
    } else 0.0
  }

  def afficherRecapitulatif(nom: String, base: Double, sucre: Double, lait: Double, total: Double): Unit = {
    println(f"\nRécapitulatif : $nom")
    println(f"Prix base : CHF $base%.2f, Sucre : CHF $sucre%.2f, Lait : CHF $lait%.2f, Total : CHF $total%.2f")
  }

  def effectuerPaiement(): Unit = {
    val code = (1 to 5).map(_ => lettreetnumero(random.nextInt(lettreetnumero.length))).mkString
    println(s"Veuillez payer via Twint. Code de paiement : $code")
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("Paiement confirmé. Merci !")
  }

  def preparerBoisson(nom: String): Unit = {
    println(s"Préparation de votre $nom...")
    Thread.sleep(2000)
    println(s"Votre $nom est prêt ! Bonne dégustation !")
  }

  def demanderChoix(msg: String, min: Int, max: Int): Int = {
    println(msg)
    var choix = readIntOption("> ")
    while (choix.isEmpty || choix.get < min || choix.get > max) {
      println(s"Veuillez entrer un choix valide entre $min et $max.")
      choix = readIntOption("> ")
    }
    choix.get
  }

  def readIntOption(prompt: String): Option[Int] = { print(prompt); try Some(readLine().toInt) catch { case _: Exception => None } }
  def readDoubleOption(prompt: String): Option[Double] = { print(prompt); try Some(readLine().toDouble) catch { case _: Exception => None } }
}
