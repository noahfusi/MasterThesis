import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  val nbMachines = 5

  // Tableaux pour stocker les données des machines
  val stocksCafe = Array.fill(nbMachines)(50.0)
  val stocksSucre = Array.fill(nbMachines)(30.0)
  val stocksLait = Array.fill(nbMachines)(0.5)
  val codesPIN = Array.fill(nbMachines)("434343")

  def lireChoixValide(min: Int, max: Int): Int = {
    var choix = 0
    var valide = false
    while (!valide) {
      choix = readInt()
      if (choix >= min && choix <= max) {
        valide = true
      } else {
        println("Veuillez entrer un nombre valide entre " +min+ " et " +max)
      }
    }
    choix
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    var valide = false

    while (tentatives > 0 && !valide) {
      println(s"Entrez le code PIN (${tentatives} tentatives restantes) :")
      print("> ")
      val pin = readLine()

      if (pin == machinePins(machineId)) {
        valide = true
        println("Accès accordé.")
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println(s"Code PIN incorrect. ${tentatives} tentatives restantes.")
        } else {
          println("Code PIN incorrect. 0 tentative restante.")
          println("Trop de tentatives échouées. Fin du programme.")

        }
      }
    }
    valide
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}")
    var nouveauPin = ""
    var valide = false

    while (!valide) {
      println("Entrez un nouveau code PIN à 6 chiffres")
      print("> ")
      nouveauPin = readLine()
      if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
        valide = true
      }
    }

    machinePins(machineId) = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def verifierStocks(machineId: Int, cafe: Double, sucre: Double, lait: Double): Boolean = {
    stocksCafe(machineId) >= cafe && stocksSucre(machineId) >= sucre && stocksLait(machineId) >= lait
  }

  def mettreAJourStocks(machineId: Int, cafe: Double, sucre: Double, lait: Double): Unit = {
    stocksCafe(machineId) -= cafe
    stocksSucre(machineId) -= sucre
    stocksLait(machineId) -= lait
  }

  def gererNiveauSucre(): (String, Double, Double) = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val choixSucre = lireChoixValide(1, 4)
    val baseQuantiteSucre = 5.0

    if (choixSucre == 1) {
      ("Sans sucre", 0.0, 0.0)
    } else if (choixSucre == 2) {
      ("Peu (5g)", 0.10, baseQuantiteSucre)
    } else if (choixSucre == 3) {
      ("Moyen (10g)", 0.20, baseQuantiteSucre * 2)
    } else {
      ("Beaucoup (15g)", 0.30, baseQuantiteSucre * 3)
    }
  }

  def gererLaitSupplementaire(quantiteBase: Double): (String, Double, Double) = {
    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) Oui")
    println("2) Non")
    print("> ")

    if (lireChoixValide(1, 2) == 1) {
      println("Entrer le nombre de doses (1-3)")
      val doses = lireChoixValide(1, 3)
      val prixDose = 0.05 * doses
      val quantiteLait = quantiteBase + (0.05 * doses)
      (doses.toString, prixDose, quantiteLait)
    } else {
      ("NON", 0.0, quantiteBase)
    }
  }

  def traiterPaiement(prix: Double): Boolean = {
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + codeTwint)
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
    true
  }

  def preparerExpresso(machineId: Int): Boolean = {
    val quantiteCafe = 8.0
    val prixBase = 2.00
    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val prixTotal = prixBase + prixSucre

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre, 0)) {
      println(s"Boisson Sélectionnée : Expresso")
      println(s"Niveau de sucre: $niveauSucre")
      println(f"Prix : CHF $prixBase%.2f + CHF $prixSucre%.2f = CHF $prixTotal%.2f")

      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, 0)
        println("Préparation de votre boisson...")
        println("Votre Expresso est prêt ! Bonne dégustation !")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, 0)
      false
    }
  }

  def preparerCappuccino(machineId: Int): Boolean = {
    val quantiteCafe = 6.0
    val quantiteLaitBase = 0.1
    val prixBase = 2.50

    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val (dosesLait, prixLait, quantiteLaitTotal) = gererLaitSupplementaire(quantiteLaitBase)
    val prixTotal = prixBase + prixSucre + prixLait

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)) {
      println(s"Boisson Sélectionnée : Cappuccino")
      println(s"Niveau de sucre: $niveauSucre")
      println(s"Doses supplémentaires de lait: $dosesLait")
      println(f"Prix : CHF $prixBase%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF $prixTotal%.2f")

      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
        println("Préparation de votre boisson...")
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
      false
    }
  }

  def preparerLatte(machineId: Int): Boolean = {
    println("1) Taille petite")
    println("2) Taille moyenne")
    println("3) Taille grande")
    print("> ")

    val taille = lireChoixValide(1, 3)
    var tailleMsg = ""
    var quantiteCafe = 0.0
    var quantiteLaitBase = 0.0
    var prixBase = 0.0

    if (taille == 1) {
      tailleMsg = "petit"
      quantiteCafe = 6.0
      quantiteLaitBase = 0.12
      prixBase = 2.70
    } else if (taille == 2) {
      tailleMsg = "moyen"
      quantiteCafe = 8.0
      quantiteLaitBase = 0.15
      prixBase = 3.20
    } else {
      tailleMsg = "grand"
      quantiteCafe = 12.0
      quantiteLaitBase = 0.2
      prixBase = 3.70
    }

    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val (dosesLait, prixLait, quantiteLaitTotal) = gererLaitSupplementaire(quantiteLaitBase)
    val prixTotal = prixBase + prixSucre + prixLait

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)) {
      println(s"Boisson Sélectionnée : Latte $tailleMsg")
      println(s"Niveau de sucre: $niveauSucre")
      println(s"Doses supplémentaires de lait: $dosesLait")
      println(f"Prix : CHF $prixBase%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF $prixTotal%.2f")

      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
        println("Préparation de votre boisson...")
        println("Votre Latte est prêt ! Bonne dégustation !")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
      false
    }
  }

  def afficherErreurStock(machineId: Int, cafe: Double, sucre: Double, lait: Double): Unit = {
    if (stocksCafe(machineId) < cafe) println("Erreur : Quantité de café insuffisante")
    if (stocksSucre(machineId) < sucre) println("Erreur : Quantité de sucre insuffisante")
    if (stocksLait(machineId) < lait) println("Erreur : Quantité de lait insuffisante")
    println("Veuillez vérifier les stocks en mode Admin")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val choixBoisson = lireChoixValide(1, 3)

    if (choixBoisson == 1) {
      preparerExpresso(machineId)
    } else if (choixBoisson == 2) {
      preparerCappuccino(machineId)
    } else {
      preparerLatte(machineId)
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Double], sugarStocks: Array[Double], milkStocks: Array[Double]): Unit = {
    println(s"Niveaux de stock actuels pour la Machine ${machineId + 1}:")
    println(s"Poudre de café : ${coffeeStocks(machineId)}g")
    println(s"Sucre : ${sugarStocks(machineId)}g")
    println(s"Lait : ${milkStocks(machineId)}L")

    println("\nEntrez les quantités à ajouter :")

    print("Poudre de café > ")
    val addCafe = readDouble().max(0)
    coffeeStocks(machineId) += addCafe

    print("Sucre > ")
    val addSucre = readDouble().max(0)
    sugarStocks(machineId) += addSucre

    print("Lait > ")
    val addLait = readDouble().max(0)
    milkStocks(machineId) += addLait

    println("Les stocks ont été mis à jour avec succès.")
  }

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      println("\n     Nospresso Café     ")
      println("Sélectionnez une machine (1-5):")
      val machineId = lireChoixValide(1, nbMachines) - 1

      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val choixMode = lireChoixValide(1, 3)

      if (choixMode == 1) {
        serveClient(machineId, stocksCafe, stocksSucre, stocksLait)
      } else if (choixMode == 2) {
        if (validatePin(machineId, codesPIN)) {
          println("1) Réapprovisionner les stocks")
          println("2) Modifier le code PIN")
          val choixAdmin = lireChoixValide(1, 2)

          if (choixAdmin == 1) {
            restockMachine(machineId, stocksCafe, stocksSucre, stocksLait)
          } else {
            updatePin(machineId, codesPIN)
          }
        }
      } else {
        println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
        continuer = false
      }
    }
  }
}