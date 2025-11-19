import scala.io.StdIn._

object Main {
  val nbMachines = 5
  val machinePins = Array.fill(nbMachines)("434343")
  val stocksCafe = Array.fill(nbMachines)(50)
  val stocksSucre = Array.fill(nbMachines)(30)
  val stocksLait = Array.fill(nbMachines)(0.5)

  def main(args: Array[String]): Unit = {
    var enCours = true

    while (enCours) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      val mode = readInt()

      if (mode == 1) {
        modeClient()
      } else if (mode == 2) {
        modeAdmin()
      } else if (mode == 3) {
        enCours = false
      } else {
        println("Entrée invalide, veuillez réessayer.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Sélectionnez l'identifiant de la machine (0-4) :")
    print("> ")
    val machineId = readInt()
    if (machineId < 0 || machineId >= nbMachines) {
      println("Machine invalide.")
      return
    }
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val boisson = readInt()
    var taille = 0

    if (boisson == 1 || boisson == 2 || boisson == 3) {
      if (boisson == 3) {
        println("Veuillez choisir la taille du Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")
        taille = readInt()
      }

      var sucre = -1
      while (sucre < 1 || sucre > 4) {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        sucre = readInt()
        if (sucre < 1 || sucre > 4) {
          println("Erreur : Entrez un nombre entre 1 et 4.")
        }
      }

      if (sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4) {
        var doses = 0
        if (boisson != 1) {
          var optionLait = -1
          while (optionLait < 1 || optionLait > 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            optionLait = readInt()
            if (optionLait < 1 || optionLait > 2) {
              println("Erreur : Entrez un nombre entre 1 et 2.")
            }
          }

          if (optionLait == 1) {
            doses = -1
            while (doses < 1 || doses > 3) {
              println("Combien de doses (1-3) ?")
              print("> ")
              doses = readInt()
              if (doses < 1 || doses > 3) {
                println("Erreur : Entrez un nombre entre 1 et 3.")
              }
            }
          }
        }

        // Vérification des stocks
        var poudreCafeNecessaire = 0
        if (boisson == 1) poudreCafeNecessaire = 8
        else if (boisson == 2) poudreCafeNecessaire = 6
        else if (boisson == 3) {
          if (taille == 1) poudreCafeNecessaire = 6
          else if (taille == 2) poudreCafeNecessaire = 8
          else if (taille == 3) poudreCafeNecessaire = 12
        }

        var laitNecessaire = 0.0
        if (boisson == 2) laitNecessaire = 0.1 + (doses * 0.05)
        else if (boisson == 3) {
          if (taille == 1) laitNecessaire = 0.12 + (doses * 0.05)
          else if (taille == 2) laitNecessaire = 0.15 + (doses * 0.05)
          else if (taille == 3) laitNecessaire = 0.2 + (doses * 0.05)
        }

        var sucreNecessaire = 0
        if (sucre == 2) sucreNecessaire = 5
        else if (sucre == 3) sucreNecessaire = 10
        else if (sucre == 4) sucreNecessaire = 15

        if (poudreCafeNecessaire > stocksCafe(machineId)) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        } else if (laitNecessaire > stocksLait(machineId)) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        } else if (sucreNecessaire > stocksSucre(machineId)) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        } else {
          traiterCommande(machineId, boisson, sucre, doses, taille, poudreCafeNecessaire, laitNecessaire, sucreNecessaire)
        }
      } else {
        println("Entrée invalide.")
      }
    } else {
      println("Entrée invalide.")
    }
  }

  def modeAdmin(): Unit = {
    println("Sélectionnez l'identifiant de la machine (0-4) :")
    print("> ")
    val machineId = readInt()
    if (machineId < 0 || machineId >= nbMachines) {
      println("Machine invalide.")
      return
    }
    if (validatePin(machineId, machinePins)) {
      println("Accès autorisé.")
      println(s"Poudre de café: ${stocksCafe(machineId)} g")
      println(s"Lait : ${stocksLait(machineId)} L")
      println(s"Sucre : ${stocksSucre(machineId)} g")
      println("1) Réapprovisionner")
      println("2) Mettre à jour le code PIN")
      print("> ")
      val choix = readInt()
      if (choix == 1) {
        println("Ajouter des stocks (ex: 20 0.5 10) : Poudre de café (g) Lait (L) Sucre (g)")
        val inputStock = readLine().split(" ").map(_.toDouble)
        stocksCafe(machineId) += inputStock(0).toInt
        stocksLait(machineId) += inputStock(1)
        stocksSucre(machineId) += inputStock(2).toInt
        println("Niveaux de stock mis à jour.")
      } else if (choix == 2) {
        updatePin(machineId, machinePins)
      } else {
        println("Entrée invalide.")
      }
    } else {
      println("Code PIN incorrect.")
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN :")
      val inputPin = readLine()
      if (inputPin == machinePins(machineId)) {
        return true
      } else {
        attempts -= 1
        println(s"Code PIN incorrect. $attempts tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var newPin = readLine()
    while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
      println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer :")
      newPin = readLine()
    }
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def traiterCommande(machineId: Int, boisson: Int, sucre: Int, doses: Int, taille: Int, poudreCafeNecessaire: Int, laitNecessaire: Double, sucreNecessaire: Int): Unit = {
    val prixBase = if (boisson == 1) 2.00 else if (boisson == 2) 2.50 else if (taille == 1) 2.70 else if (taille == 2) 3.20 else 3.70
    val prixSucre = if (sucre == 2) 0.10 else if (sucre == 3) 0.20 else if (sucre == 4) 0.30 else 0.00
    val prixLait = doses * 0.05
    val prixTotal = prixBase + prixSucre + prixLait

    println(s"Prix total : CHF $prixTotal")
    println("Veuillez payer en utilisant Twint.")
    val codePaiement = scala.util.Random.alphanumeric.take(5).mkString
    println(s"Votre code de paiement est : $codePaiement")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Paiement confirmé.")
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    println("Votre boisson est prête ! Bonne dégustation !")

    // Mise à jour des stocks
    stocksCafe(machineId) -= poudreCafeNecessaire
    stocksLait(machineId) -= laitNecessaire
    stocksSucre(machineId) -= sucreNecessaire

    println("Stocks mis à jour après la transaction.")
    println("Retour au menu principal...")
  }
}
