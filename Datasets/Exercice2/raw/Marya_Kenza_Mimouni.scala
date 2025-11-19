import scala.io.StdIn
import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = {
    while (true) {
      AfficherMenu()
    }
  }

  // Définition des variables globales
  val nbMachines = 5
  var machinePins = Array.fill(nbMachines)(434343)

  var machineId = 0
  private var DoseDeLait = 0

  var Stockscaffe = Array.fill(nbMachines)(50)
  private var NiveauSucre = ""
  var StocksSucre = Array.fill(nbMachines)(30)
  var StockLait = Array.fill(nbMachines)(500)

  // Afficher le menu principal
  def AfficherMenu(): Boolean = {
    machineId = StdIn.readLine("Machine sélectionnée> ").toInt
    println("\n")

    println("Nospresso Cafe")
    println("Veuillez sélectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")

    val ModeSelection = StdIn.readLine(">")

    ModeSelection match {
      case "1" => serveClient(machineId, Stockscaffe, StocksSucre, StockLait)
      case "2" => AdminMode(machineId)
      case "3" => ExitMode()
      case _ =>
        println("Choix invalide.")
        true
    }
  }

  // Mode Client
  private def serveClient(Id: Int, CafeArray: Array[Int], sucreArray: Array[Int], laitArray: Array[Int]): Boolean = {
    // Sélectionner la boisson
    println("Veuillez sélectionner votre boisson :")
    println("1) Espresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val BoissonSelectionne = StdIn.readLine(">")

    // Choisir quantité de sucre
    NiveauSucre = QuantiteDeSucre()
    // Choisir quantité de lait
    DoseDeLait = QuantiteDeCafe()

    def QuantiteDeSucre(): String = {
      println("\nSouhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")

      val Choix = StdIn.readLine(">")
      Choix match {
        case "1" => "1"
        case "2" => "2"
        case "3" => "3"
        case "4" => "4"
        case _ =>
          println("Choix invalide, par défaut sans sucre.")
          "1"
      }
    }

    def QuantiteDeCafe(): Int = {
      if (BoissonSelectionne == "2" || BoissonSelectionne == "3") {
        println("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")

        val Choix = StdIn.readLine(">")
        Choix match {
          case "1" =>
            println("Combien de doses ? (1-3)")
            val doses = StdIn.readLine().toInt
            if (doses >= 1 && doses <= 3) {
              println(s"$doses dose(s) de lait.")
              doses
            } else {
              println("Non valide. Sans lait supplémentaire.")
              0
            }
          case "2" =>
            println("Sans lait supplémentaire.")
            0
          case _ =>
            println("Sans lait supplémentaire.")
            0
        }
      } else 0
    }

    var reussi = false
    BoissonSelectionne match {
      case "1" => reussi = FaireEspresso(Id, NiveauSucre)
      case "2" => reussi = FaireCappucino(Id, NiveauSucre, DoseDeLait)
      case "3" => reussi = FaireLatte(Id, NiveauSucre, DoseDeLait)
      case _ =>
        println("Boisson inconnue.")
        return false
    }

    if (reussi) {
      // Calcul du prix
      val PrixCafe: Double = BoissonSelectionne match {
        case "1" => 2.00 // Espresso
        case "2" => 2.50 // Cappuccino
        case "3" =>
          DoseDeLait match {
            case 1 => 2.70 // Petit Latte
            case 2 => 3.20 // Moyen Latte
            case 3 => 3.70 // Grand Latte
          }
      }

      val NomDeLaBoisson: String = BoissonSelectionne match {
        case "1" => "Espresso"
        case "2" => "Cappuccino"
        case "3" => "Latte"
        case _ => "Boisson Inconnue"
      }

      println(s"Boisson sélectionnée : $NomDeLaBoisson")

      val PrixDuSucre: Double = NiveauSucre match {
        case "1" => 0.00
        case "2" => 0.10
        case "3" => 0.20
        case "4" => 0.30
      }

      val NomNiveauSucre = NiveauSucre match {
        case "1" => "Pas de sucre"
        case "2" => "Léger (5g)"
        case "3" => "Moyen (10g)"
        case "4" => "Beaucoup (15g)"
      }
      println(s"Niveau de sucre: $NomNiveauSucre")

      val NomLait = DoseDeLait match {
        case 0 => "Non"
        case 1 => "1 dose"
        case 2 => "2 doses"
        case 3 => "3 doses"
      }
      println(s"Lait supplémentaire: $NomLait")

      val prixTotal: Double = PrixCafe + PrixDuSucre
      if (NomNiveauSucre == "Pas de sucre") {
        println(f"Prix total: CHF $PrixCafe%.2f = CHF $prixTotal%.2f")
      } else {
        println(f"Prix total: CHF $PrixCafe%.2f + CHF $PrixDuSucre%.2f = CHF $prixTotal%.2f")
      }

      // Générateur de code de paiement
      val setalphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      val CodePaiement = (for (_ <- 1 to 5) yield setalphanum(Random.nextInt(setalphanum.length))).mkString
      println("\nVeuillez payer avec Twint.")
      println(s"Votre code de paiement est : $CodePaiement")
      println("(En attente du paiement...)")
      Thread.sleep(3000)
      println("Paiement accepté.\n")
      Preparation(NomDeLaBoisson)

      def Preparation(nom: String): Unit = {
        println("Boisson en préparation...")
        println("[...]")
        Thread.sleep(3000)
        println(s"Votre $nom est prêt ! Profitez!")
      }

      true
    } else {
      false
    }
  }

  def ModifierPin(machineId: Int, machineP: Array[Int]): Unit = {
    println(s"Changement de PIN pour la machine $machineId.")
    var NouveauCodePin = ""
    while (NouveauCodePin.length != 6) {
      NouveauCodePin = StdIn.readLine("Entrez un PIN de 6 chiffres >")
      if (NouveauCodePin.length == 6) {
        machinePins(machineId - 1) = NouveauCodePin.toInt
      } else {
        println("Le code PIN doit comporter 6 chiffres.")
      }
    }
  }

  def RechargerLaMachine(Id: Int, CafeArray: Array[Int], sucreArray: Array[Int], laitArray: Array[Int]): Unit = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)
    var StockDeLait = StockLait(Id - 1)
    // Afficher les stocks
    println(s"Stocks:\n Poudre Café: $StockPoudreCafe g\n Lait: ${StockDeLait / 1000.0} L\n Sucre: $StockDeSucre g")

    val Cafe = StdIn.readLine("Combien de grammes de café en poudre voulez-vous ajouter? >").toInt
    Stockscaffe(Id - 1) += Cafe
    println(s"Poudre Café: + $Cafe g")

    val Sucre = StdIn.readLine("Combien de grammes de sucre voulez-vous ajouter? >").toInt
    StocksSucre(Id - 1) += Sucre
    println(s"Sucre: + $Sucre g")

    val Lait = StdIn.readLine("Combien de litres de Lait voulez-vous ajouter? >").toInt
    StockLait(Id - 1) += (Lait * 1000)
    println(s"Lait: +$Lait L")

    StockPoudreCafe = Stockscaffe(Id - 1)
    StockDeSucre = StocksSucre(Id - 1)
    StockDeLait = StockLait(Id - 1)

    println("Réapprovisionnement des stocks...")
    println(s"Niveaux de stock mis à jour:\n Poudre de Café: $StockPoudreCafe g\n Lait: ${StockDeLait / 1000.0} L\n Sucre: $StockDeSucre g")
    println("Niveaux de stock mis à jour.\nRetour au menu principal...")
  }

  def ValiderCodePin(ID: Int, machinePins: Array[Int]): Boolean = {
    val validPin = machinePins(ID - 1)
    var pin = StdIn.readLine("Entrer le PIN admin: ").toInt
    var essai = 1
    var pinCorrect = (pin == validPin)

    while (essai < 3 && !pinCorrect) {
      val remain = 3 - essai
      pin = StdIn.readLine(s"PIN Incorrect. $remain essai(s) restant(s) >").toInt
      pinCorrect = (pin == validPin)
      essai += 1
    }

    pinCorrect
  }

  // Mode Admin
  private def AdminMode(Id: Int): Boolean = {
    var machineAdmId = StdIn.readLine("Machine sélectionnée (1-5) >").toInt
    val pinValider = ValiderCodePin(machineAdmId, machinePins)

    if (!pinValider) {
      ExitMode()
    } else {
      println("Accès accepté.")
      println("1) Changer Pin \n2) Réapprovisionner les stocks")
      val Choix = StdIn.readLine(">")

      if (Choix == "1") {
        ModifierPin(machineAdmId, machinePins)
        println("Pin changé.\nRetour au menu principal...")
      } else if (Choix == "2") {
        RechargerLaMachine(machineAdmId, Stockscaffe, StocksSucre, StockLait)
      } else {
        println("Choix invalide.")
      }
    }
    true
  }

  def extraireValeurSucre(sucreL: String): Int = {
    sucreL match {
      case "1" => 0
      case "2" => 5
      case "3" => 10
      case "4" => 15
      case _   => 0
    }
  }

  // Préparation des cafés

  private def FaireEspresso(Id: Int, sucreL: String): Boolean = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)

    val sucVal = extraireValeurSucre(sucreL)

    // Vérifications avant préparation
    if (StockPoudreCafe < 8) {
      println("Poudre de café insuffisante pour un Espresso.")
      return false
    }
    if (StockDeSucre < sucVal) {
      println("Sucre insuffisant pour préparer cette boisson.")
      return false
    }

    // Mise à jour des stocks
    StockPoudreCafe -= 8
    Stockscaffe(Id - 1) =  StockPoudreCafe

    StockDeSucre -= sucVal
    StocksSucre(Id - 1) =  StockDeSucre

    true
  }

  private def FaireCappucino(Id: Int, sucreL: String, laitL: Int): Boolean = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)
    var StockDeLait = StockLait(Id - 1)

    val sucVal = extraireValeurSucre(sucreL)

    // Vérification avant préparation
    if (StockPoudreCafe < 6) {
      println("Poudre de café insuffisante pour préparer le Cappuccino.")
      return false
    }
    // Ici, on considère qu'un cappuccino requiert 100ml de lait
    if (StockDeLait < 100) {
      println("Lait insuffisant pour préparer le Cappuccino.")
      return false
    }
    if (StockDeSucre < sucVal) {
      println("Sucre insuffisant pour préparer cette boisson.")
      return false
    }

    // Mise à jour des stocks
    StockPoudreCafe -= 6
    Stockscaffe(Id - 1) =  StockPoudreCafe

    StockDeLait -= 100
    StockLait(Id - 1) =  StockDeLait

    StockDeSucre -= sucVal
    StocksSucre(Id - 1) =  StockDeSucre

    true
  }

  private def FaireLatte(Id: Int, sucreL: String, laitL: Int): Boolean = {
    println("Veuillez sélectionner la taille de votre Latte :")
    println("1) Petit")
    println("2) Moyen")
    println("3) Grand")

    val LatteChoix = StdIn.readLine(">")
    LatteChoix match {
      case "1" => FairePetitLatte(Id, sucreL, laitL)
      case "2" => FaireMoyenLatte(Id, sucreL, laitL)
      case "3" => FaireGrandLatte(Id, sucreL, laitL)
      case _ =>
        println("Choix de taille invalide.")
        false
    }
  }

  private def FairePetitLatte(Id: Int, sucreL: String, laitL: Int): Boolean = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)
    var StockDeLait = StockLait(Id - 1)

    val sucVal = extraireValeurSucre(sucreL)

    // Petit Latte nécessite 6g de café, 120ml de lait
    if (StockPoudreCafe < 6) {
      println("Poudre de café insuffisante pour un Petit Latte.")
      return false
    }
    if (StockDeLait < 120) {
      println("Lait insuffisant pour un Petit Latte.")
      return false
    }
    if (StockDeSucre < sucVal) {
      println("Sucre insuffisant.")
      return false
    }

    // Mise à jour des stocks
    StockPoudreCafe -= 6
    Stockscaffe = Stockscaffe.updated(Id - 1, StockPoudreCafe)

    StockDeLait -= 120
    StockLait = StockLait.updated(Id - 1, StockDeLait)

    StockDeSucre -= sucVal
    StocksSucre = StocksSucre.updated(Id - 1, StockDeSucre)

    true
  }

  private def FaireMoyenLatte(Id: Int, sucreL: String, laitL: Int): Boolean = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)
    var StockDeLait = StockLait(Id - 1)

    val sucVal = extraireValeurSucre(sucreL)

    // Moyen Latte nécessite 8g de café, 150ml de lait
    if (StockPoudreCafe < 8) {
      println("Poudre de café insuffisante pour un Latte Moyen.")
      return false
    }
    if (StockDeLait < 150) {
      println("Lait insuffisant pour un Latte Moyen.")
      return false
    }
    if (StockDeSucre < sucVal) {
      println("Sucre insuffisant.")
      return false
    }

    // Mise à jour
    StockPoudreCafe -= 8
    Stockscaffe(Id - 1) =  StockPoudreCafe

    StockDeLait -= 150
    StockLait(Id - 1) = StockDeLait

    StockDeSucre -= sucVal
    StocksSucre(Id - 1) =  StockDeSucre

    true
  }

  private def FaireGrandLatte(Id: Int, sucreL: String, laitL: Int): Boolean = {
    var StockPoudreCafe = Stockscaffe(Id - 1)
    var StockDeSucre = StocksSucre(Id - 1)
    var StockDeLait = StockLait(Id - 1)

    val sucVal = extraireValeurSucre(sucreL)

    // Grand Latte nécessite 12g de café, 200ml de lait
    if (StockPoudreCafe < 12) {
      println("Poudre de café insuffisante pour un Grand Latte.")
      return false
    }
    if (StockDeLait < 200) {
      println("Lait insuffisant pour un Grand Latte.")
      return false
    }
    if (StockDeSucre < sucVal) {
      println("Sucre insuffisant.")
      return false
    }

    // Mise à jour
    StockPoudreCafe -= 12
    Stockscaffe(Id - 1) =  StockPoudreCafe

    StockDeLait -= 200
    StockLait(Id - 1) =  StockDeLait

    StockDeSucre -= sucVal
    StocksSucre(Id - 1) =  StockDeSucre

    true
  }

  private def ExitMode(): Boolean = {
    println("Merci d'avoir utilisé Nospresso Cafe. Au revoir !")
    System.exit(0)
    true
  }

}









