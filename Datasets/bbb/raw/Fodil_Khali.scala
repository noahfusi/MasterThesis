import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object NospressoMachine {


  val nbMachines = 5
  val defaultPin = "434343"

  var coffeeStocks = Array(50, 50, 50, 50, 50)
  var sugarStocks = Array(30, 30, 30, 30, 30)
  var milkStocks = Array(500, 500, 500, 500, 500)
  var machinePins = Array.fill(nbMachines)(defaultPin)


  val prixExpresso = 2.00
  val prixCappuccino = 2.50
  val prixLattePetit = 2.70
  val prixLatteMoyen = 3.20
  val prixLatteGrand = 3.70
  val prixDoseSucre = 0.10
  val prixDoseLait = 0.05


  def main(args: Array[String]): Unit = {
    var continuer = true


    while (continuer) {
      println("\nBienvenue à Nospresso Café")
      print("Sélectionnez une machine (1 à " + nbMachines + ") > ")
      val machineId = readInt()

      if (machineId >= 1 && machineId <= nbMachines) {
        afficherMenuPrincipal(machineId - 1)
      } else {
        println("Choix invalide. Veuillez sélectionner une machine entre 1 et " + nbMachines + ".")
      }
    }
  }


  def afficherMenuPrincipal(machineId: Int): Unit = {
    var continuer = true

    while (continuer) {
      println("\nMachine sélectionnée : " + (machineId + 1)) // Affiche la machine 1 à 5
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Retourner au menu principal")
      val choix = readInt()

      if (choix == 1) {
        modeClient(machineId)
      } else if (choix == 2) {
        if (validerPin(machineId)) {
          modeAdmin(machineId)
        } else {
          println("Trop de tentatives échouées. Fin du programme.")
          continuer = false
        }
      } else if (choix == 3) {
        continuer = false
      } else {
        println("Choix invalide. Veuillez réessayer.")
      }
    }
  }

  def validerPin(machineId: Int): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN : ")

      val pin = readLine()
      if (pin == machinePins(machineId)) {
        println("Accès accordé à la Machine " + (machineId + 1) + ".")
        return true
      }
      if (pin != machinePins(machineId)) {
        tentatives -= 1
        if (tentatives == 2) {
          print("Code PIN incorrect. " + tentatives + " tentatives restantes. ")
        }
        if (tentatives == 1) {
          print("Code PIN incorrect. " + tentatives + " tentative restante. ")
        }
      }
    }
    false
  }


  def modeAdmin(machineId: Int): Unit = {
    println("Mode Admin - Machine " + (machineId + 1))
    println("1) Réapprovisionner les stocks")
    println("2) Mettre à jour le code PIN")
    val choix = readInt()

    if (choix == 1) {
      reapprovisionner(machineId)
    } else if (choix == 2) {
      println("Mise à jour du code PIN a été mis à jour avec succès.")
      mettreAJourPin(machineId)
      println("Retour au menu principal...")
    } else {
      println("Choix invalide.")
    }
  }

  def reapprovisionner(machineId: Int): Unit = {
    println("Entrez la quantité de café à ajouter :")
    print("Poudre de café > ")
    val ajoutcafe = readInt()
    print("Sucre > ")
    val ajoutsucre = readInt()
    print("Lait > ")
    val ajoutlait = readInt()

    if (ajoutcafe >= 0 && ajoutsucre >= 0 && ajoutlait >= 0) {
      coffeeStocks(machineId) += ajoutcafe
      sugarStocks(machineId) += ajoutsucre
      milkStocks(machineId) += ajoutlait
      println("Stocks mis à jour avec succès.")
      println("Retour au menu principal...")
    } else {
      println("Choix invalide.")
    }
  }

  // Fonction pour mettre à jour le code PIN
  def mettreAJourPin(machineId: Int): Unit = {

    print("Entrez un nouveau code PIN à 6 chiffres > ")
    var nouveauPin = readLine()
    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      nouveauPin = readLine()
    }
    machinePins(machineId) = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def modeClient(machineId: Int): Unit = {
    println("Sélectionnez votre boisson :")
    println("1) Expresso")
    println("2) Cappuccino")
    println("3) Latte")
    val choix = readInt()

    var prix = 0.0
    var cafeRequis = 0
    var laitRequis = 0

    if (choix == 1) {
      prix = prixExpresso
      cafeRequis = 8
    } else if (choix == 2) {
      prix = prixCappuccino
      cafeRequis = 6
      laitRequis = 100
    } else if (choix == 3) {
      println("Choisissez la taille : 1) Petit 2) Moyen 3) Grand")
      val taille = readInt()
      if (taille == 1) { prix = prixLattePetit; laitRequis = 120; cafeRequis = 6 }
      else if (taille == 2) { prix = prixLatteMoyen; laitRequis = 150; cafeRequis = 8 }
      else if (taille == 3) { prix = prixLatteGrand; laitRequis = 200; cafeRequis = 12 }
      else { println("Choix invalide."); return }
    } else {
      println("Choix invalide.")
      return
    }

    println("Souhaitez-vous ajouter du sucre ? (0: Aucun, 1: Peu, 2: Moyen, 3: Beaucoup)")
    val sucreChoisi = readInt()
    val sucreRequis = sucreChoisi * 5
    prix += sucreChoisi * prixDoseSucre

    var dosesLait = 0
    if (choix == 2 || choix == 3) {
      println("Combien de doses de lait supplémentaires (0-3) ?")
      dosesLait = readInt()
      prix += dosesLait * prixDoseLait
    }

    if (coffeeStocks(machineId) < cafeRequis || milkStocks(machineId) < (laitRequis + dosesLait * 50) || sugarStocks(machineId) < sucreRequis) {
      println("Erreur : Stock insuffisant pour préparer la boisson.")
      return
    }

    println("Prix total : " + prix + " CHF")
    val codePaiement = genererCodeTwint()
    println("Veuillez payer via Twint. Votre code est : " + codePaiement)
    println("En attente de paiement...")
    Thread.sleep(3000)
    println("Paiement validé. Préparation de la boisson...")

    coffeeStocks(machineId) -= cafeRequis
    milkStocks(machineId) -= (laitRequis + dosesLait * 50)
    sugarStocks(machineId) -= sucreRequis

    println("Votre boisson est prête. Bonne dégustation !")
  }


  def genererCodeTwint(): String = {
    val random = new Random()
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    for (_ <- 1 to 5) {
      code += caracteres(random.nextInt(caracteres.length))
    }
    code
  }
}
