import scala.io.StdIn
import scala.util.Random

object MultiMachineNospresso {
  val nbMachines = 5 // Nombre total de machines
  var stockCafe = Array.fill(nbMachines)(50.0)
  var stockSucre = Array.fill(nbMachines)(30.0)
  var stockLait = Array.fill(nbMachines)(500.0)
  val machinePins = Array.fill(nbMachines)("434343") // Codes PIN initiaux pour toutes les machines
  var continue = true

  def main(args: Array[String]): Unit = {
    while (continue) {
      println("\nNospresso Café - Multi-Machines")
      println("Veuillez sélectionner un mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      StdIn.readLine() match {
        case "1" => clientMode()
        case "2" => adminMode()
        case "3" =>
          println("Merci de votre visite. À bientôt !")
          continue = false
        case _ => println("Sélection invalide, veuillez réessayer.")
      }
    }
  }

  // Sélection de la machine
  def selectionMachine(): Int = {
    println(s"Veuillez sélectionner une machine (0-${nbMachines - 1}):")
    try {
      val machineId = StdIn.readInt()
      if (machineId >= 0 && machineId < nbMachines) {
        machineId
      } else {
        println("Numéro de machine invalide. Essayez à nouveau.")
        selectionMachine()
      }
    } catch {
      case _: Exception =>
        println("Entrée invalide. Essayez à nouveau.")
        selectionMachine()
    }
  }

  // Mode Client
  def clientMode(): Unit = {
    val machineId = selectionMachine()
    println(s"\nBienvenue au mode client de la Machine $machineId.")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    StdIn.readLine() match {
      case "1" => preparationBoisson(machineId, "Expresso", 2.00, 8, 0)
      case "2" => preparationBoisson(machineId, "Cappuccino", 2.50, 6, 100)
      case "3" => preparationLatte(machineId)
      case _ => println("Sélection invalide.")
    }
  }

  // Préparer une boisson
  def preparationBoisson(machineId: Int, nomBoisson: String, prixBoisson: Double, cafeNecessaire: Double, laitNecessaire: Double): Unit = {
    if (stockCafe(machineId) >= cafeNecessaire && stockLait(machineId) >= laitNecessaire) {
      stockCafe(machineId) -= cafeNecessaire
      stockLait(machineId) -= laitNecessaire
      val prixSucre = ajoutSucre(machineId)
      val prixLaitSup = ajoutLaitSup(machineId)
      val prixTotal = prixBoisson + prixSucre + prixLaitSup

      println(s"Le prix total de votre boisson est : CHF $prixTotal.")
      val twintCode = Random.alphanumeric.take(5).mkString
      println(s"Veuillez payer avec Twint. Votre code de paiement est : $twintCode.")
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)

      println("Merci ! Votre paiement a été accepté.")
      println(s"Préparation de votre $nomBoisson...")
      Thread.sleep(5000)

      println(s"Votre $nomBoisson est prêt ! Bonne dégustation !")
    } else {
      println("Stock insuffisant pour préparer cette boisson.")
    }
  }

  // Préparer un Latte
  def preparationLatte(machineId: Int): Unit = {
    println("Veuillez sélectionner la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    print("> ")

    val (prixTaille, cafeNecessaire, laitNecessaire) = StdIn.readLine() match {
      case "1" => (2.70, 6, 120)
      case "2" => (3.20, 8, 150)
      case "3" => (3.70, 12, 200)
      case _ =>
        println("Sélection invalide. Retour au menu principal.")
        return
    }

    preparationBoisson(machineId, "Latte", prixTaille, cafeNecessaire, laitNecessaire)
  }

  // Ajouter du sucre
  def ajoutSucre(machineId: Int): Double = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    StdIn.readLine() match {
      case "1" => 0.0
      case "2" if stockSucre(machineId) >= 5 =>
        stockSucre(machineId) -= 5
        0.10
      case "3" if stockSucre(machineId) >= 10 =>
        stockSucre(machineId) -= 10
        0.20
      case "4" if stockSucre(machineId) >= 15 =>
        stockSucre(machineId) -= 15
        0.30
      case _ =>
        println("Stock insuffisant ou sélection invalide.")
        0.0
    }
  }

  // Ajouter du lait supplémentaire
  def ajoutLaitSup(machineId: Int): Double = {
    println("Souhaitez-vous ajouter des doses supplémentaires de lait ?")
    println("0) Pas de lait supplémentaire")
    println("1) Une dose (50ml) - CHF 0.05")
    println("2) Deux doses (100ml) - CHF 0.10")
    println("3) Trois doses (150ml) - CHF 0.15")
    print("> ")

    StdIn.readLine() match {
      case "0" => 0.0
      case "1" if stockLait(machineId) >= 50 =>
        stockLait(machineId) -= 50
        0.05
      case "2" if stockLait(machineId) >= 100 =>
        stockLait(machineId) -= 100
        0.1
      case "3" if stockLait(machineId) >= 150 =>
        stockLait(machineId) -= 150
        0.15
      case _ =>
        println("Stock insuffisant ou entrée invalide. Pas de lait supplémentaire.")
        0.0
    }
  }

  // Mode Administrateur
  def adminMode(): Unit = {
    println("\n--- Mode Administrateur ---")
    val machineId = selectionMachine()
    if (validationPin(machineId)) {
      println("\nOptions administrateur :")
      println("1) Réapprovisionner la machine")
      println("2) Mettre à jour le code PIN")
      println("3) Retour")
      print("> ")

      StdIn.readLine() match {
        case "1" => restockMachine(machineId)
        case "2" => updatePin(machineId)
        case "3" => println("Retour au menu principal...")
        case _ => println("Sélection invalide.")
      }
    }
  }

  // Validation du PIN
  def validationPin(machineId: Int): Boolean = {
    println(s"Entrez le code PIN pour la machine $machineId :")
    var tentatives = 3
    while (tentatives > 0) {
      val pin = StdIn.readLine()
      if (pin == machinePins(machineId)) {
        println(s"Accès accordé à la Machine $machineId.")
        return true
      } else {
        tentatives -= 1
        println(s"Code PIN incorrect. $tentatives tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    false
  }

  // Réapprovisionner une machine
  def restockMachine(machineId: Int): Unit = {
    println(s"Niveaux de stock actuels - Machine $machineId :")
    println(f"Poudre de café : ${stockCafe(machineId)}%.2fg")
    println(f"Sucre : ${stockSucre(machineId)}%.2fg")
    println(f"Lait : ${stockLait(machineId)}%.2fL")

    println("Entrez la quantité de poudre de café à ajouter (en g) :")
    stockCafe(machineId) += StdIn.readDouble()

    println("Entrez la quantité de sucre à ajouter (en g) :")
    stockSucre(machineId) += StdIn.readDouble()

    println("Entrez la quantité de lait à ajouter (en litres) :")
    stockLait(machineId) += StdIn.readDouble() * 1000 // Conversion de litres en millilitres

    println("Les stocks ont été mis à jour avec succès. Retour au menu principal...")
  }

  // Mettre à jour le PIN
  def updatePin(machineId: Int): Unit = {
    println(s"Mise à jour du code PIN pour la Machine $machineId.")
    while (true) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = StdIn.readLine()
      if (newPin.matches("\\d{6}")) {
        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succès.")
        return
      } else {
        println("Code PIN invalide. Le PIN doit contenir exactement 6 chiffres.")
      }
    }
  }
}