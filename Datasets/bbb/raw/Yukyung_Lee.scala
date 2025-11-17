import scala.io.StdIn.readLine
import scala.util.Random

object VendingMachineApp {

  val nbMachines: Int = 5
  var PCstock: Array[Int] = Array.fill(nbMachines)(50) // PC= poudre du café (g)
  var sucreStock: Array[Int] = Array.fill(nbMachines)(30) //g
  var laitStock: Array[Int] = Array.fill(nbMachines)(500) // ml
  var pin: Array[String] = Array.fill(nbMachines)("434343") // pin initiale
  // ml --> l afficher ? > work on later

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      println("Choisissez votre mode:")
      println("1. Mode Client")
      println("2. Mode Admin")
      println("3. Quitter")
      print("> ")

      readLine() match {
        case "1" => modeClient()
        case "2" => modeAdmin()
        case "3" =>
          println("Merci!")
          continuer = false
        case _ =>
          println("Option invalide.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Choisissez le numéro de votre machine (1-5): ")
    print("> ")
    val machineIdx = readLine().toIntOption.getOrElse(0) - 1

    if (!isValidMachine(machineIdx)) {
      println("Veuillez choisir un autre option.")
      return
    }

    println("Menu Boissons:")
    println("1. Expresso - CHF 2.00")
    println("2. Cappuccino - CHF 2.50")
    println("3. Latte Petit - CHF 2.70")
    println("4. Latte Moyen - CHF 3.20")
    println("5. Latte Grand - CHF 3.70")
    print("Veuillez choisir une boisson: ")
    print(">")

    val choix = readLine() // use array for this part
    val (boisson, cafeBesoin, laitBesoin, prixBase) = choix match {
      case "1" => ("Expresso", 8, 0, 2.00)
      case "2" => ("Cappuccino", 6, 150, 2.50)
      case "3" => ("Latte Petit", 6, 120, 2.70)
      case "4" => ("Latte Moyen", 8, 160, 3.20)
      case "5" => ("Latte Grand", 10, 200, 3.70)
      case _ =>
        println("Option invalide. Retour au menu principale.")
        return
    }

    if (!Stockcheck(machineIdx, cafeBesoin, laitBesoin)) {
      println("Stock insuffisante. Veuillez choisir un autre boisson.")
      return
    }

    val sucrePrix = ajouterSucre(machineIdx)
    val laitPrix = ajouterLait(machineIdx)

    val prixTotal = prixBase + sucrePrix + laitPrix
    println(f"Prix total: CHF $prixTotal%.2f")

    val twintCode = generateTwintCode()
    println(s"Veuillez payer en utilisant Twint.")
    println(s"Votre code de paiement est : $twintCode")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)

    println("Paiement accepté. Préparation de votre boisson...")
    Thread.sleep(3000)
    Stockupdate(machineIdx, cafeBesoin, laitBesoin, (sucrePrix / 0.1).toInt * 5)
    println(s"Votre boisson est prêt!")
  }

  def modeAdmin(): Unit = {
    println("Entrez le mot de passe Admin (par défaut : 434343) :")
    val adminPin = readLine()

    if (adminPin != pin(0)) {
      println("Mot de passe incorrect.")
      return
    }

    var continuerAdmin = true
    while (continuerAdmin) {
      println("1. Réapprovisionner les stocks d'une machine")
      println("2. Changer le PIN d'une machine")
      println("3. Afficher les PINs de toutes les machines")
      println("4. Retour au menu principal")
      print("> ")

      readLine() match {
        case "1" => reapprovisionner()
        case "2" => changerPin()
        case "3" => afficherPins()
        case "4" =>
          println("Retour au menu principal.")
          continuerAdmin = false
        case _ =>
          println("Option invalide.")
      }
    }
  }

  def isValidMachine(machineIdx: Int): Boolean = {
    machineIdx >= 0 && machineIdx < nbMachines
  }

  def Stockcheck(machineIdx: Int, cafeBesoin: Int, laitBesoin: Int): Boolean = {
    PCstock(machineIdx) >= cafeBesoin && laitStock(machineIdx) >= laitBesoin
  }

  def Stockupdate(machineIdx: Int, cafeUtilise: Int, laitUtilise: Int, sucreUtilise: Int): Unit = {
    PCstock(machineIdx) -= cafeUtilise
    laitStock(machineIdx) -= laitUtilise
    sucreStock(machineIdx) -= sucreUtilise
  }

  def ajouterSucre(machineIdx: Int): Double = {
    println("Voulez-vous ajouter du sucre ? (1. Oui / 2. Non)")
    if (readLine() == "1") {
      println("1. Peu - CHF 0.1")
      println("2. Moyen - CHF 0.2")
      println("3. Beaucoup - CHF 0.3")
      readLine() match {
        case "1" if sucreStock(machineIdx) >= 5 =>
          sucreStock(machineIdx) -= 5
          0.1
        case "2" if sucreStock(machineIdx) >= 10 =>
          sucreStock(machineIdx) -= 10
          0.2
        case "3" if sucreStock(machineIdx) >= 15 =>
          sucreStock(machineIdx) -= 15
          0.3
        case _ =>
          println("Stock insuffisant ou option invalide.")
          0.0
      }
    } else 0.0
  }

  def ajouterLait(machineIdx: Int): Double = {
    println("Voulez-vous ajouter du lait ? (1. Oui / 2. Non)")
    if (readLine() == "1") {
      println("1. 50ml - CHF 0.05")
      println("2. 100ml - CHF 0.10")
      println("3. 150ml - CHF 0.15")
      readLine() match {
        case "1" if laitStock(machineIdx) >= 50 =>
          laitStock(machineIdx) -= 50
          0.05
        case "2" if laitStock(machineIdx) >= 100 =>
          laitStock(machineIdx) -= 100
          0.10
        case "3" if laitStock(machineIdx) >= 150 =>
          laitStock(machineIdx) -= 150
          0.15
        case _ =>
          println("Option invalide.")
          0.0
      }
    } else 0.0
  }

  def reapprovisionner(): Unit = {
    println("Veuillez chosir le numéro de la machine (1-5) :")
    val machineIdx = readLine().toInt - 1

    if (!isValidMachine(machineIdx)) {
      println("Numéro invalide.")
      return
    }

    println("Quantité de café à ajouter (g) :")
    val cafeAjout = readLine().toInt
    println("Quantité de sucre à ajouter (g) :")
    val sucreAjout = readLine().toInt
    println("Quantité de lait à ajouter (L) :")
    val laitAjout = (readLine().toDouble * 1000).toInt // ml --> l check again b4 submitiing

    PCstock(machineIdx) += cafeAjout
    sucreStock(machineIdx) += sucreAjout
    laitStock(machineIdx) += laitAjout

    println(s"Réapprovisionnement terminé pour la machine ${machineIdx + 1}.")
  }

  def changerPin(): Unit = {
    println("Choisissez le numéro de la machine (1-5) :")
    val machineIdx = readLine().toInt - 1

    if (!isValidMachine(machineIdx)) {
      println("Numéro invalide.")
      return
    }

    println(s"PIN actuel pour la machine ${machineIdx + 1}: ${pin(machineIdx)}")
    println("Entrez un nouveau PIN (6 chiffres) :")
    val newPin = readLine()

    if (newPin.length == 6 && newPin.forall(_.isDigit)) {
      pin(machineIdx) = newPin
      println(s"Le nouveau PIN pour la machine ${machineIdx + 1} est : $newPin")
      println(s"PIN 배열 상태: ${pin.mkString(", ")}") // 디버깅 출력
      afficherPins() // 업데이트 후 바로 호출
    } else {
      println("PIN invalide. mise à jour annulé.")
    }
  }

  def afficherPins(): Unit = {
    println("PINs de toutes les machines :")
    for (i <- pin.indices) {
      println(s"Machine ${i + 1}: ${pin(i)}")
    }
  }

  def generateTwintCode(): String = {
    Random.alphanumeric.filter(_.isLetterOrDigit).take(5).mkString
  }
}
