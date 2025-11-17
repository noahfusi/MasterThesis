import io.StdIn._
import scala.util.Random


object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val essaisMaximum = 3
    var essaisRestant = essaisMaximum

    while (essaisRestant > 0) {
      var pinSaisi = ""
      var pinValide = false

      // Boucle explicite pour valider le PIN
      while (!pinValide) {
        println(s"Entrez le code PIN pour la machine: ${machineId + 1}")
        pinSaisi = readLine()

        if (pinSaisi.length == 6 && pinSaisi.forall(_.isDigit)) {
          pinValide = true
        } else {
          println("Le code PIN doit contenir exactement 6 chiffres. Veuillez réessayer.")
        }
      }

      // Vérification du contenu du PIN
      if (machinePins(machineId) == pinSaisi) {
        println(s"Accès autorisé à la machine: ${machineId + 1}")
        return true
      } else {
        essaisRestant -= 1
        println(s"Code PIN incorrect. Il reste seulement: $essaisRestant essais.")
      }
    }

    // Si l'utilisateur a dépassé le nombre d'essais
    println("Accès refusé. Vous avez dépassé le nombre d'essais autorisés.")
    println("Retour au menu principal.")
    return false // Retourner false pour indiquer que l'accès est refusé
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Veuillez mettre à jour le nouveau code pin >")
    var nouveauPin = readLine()
    var isValid = false
    val essaisMaximum = 3
    var essaisrestant = essaisMaximum
    println(" Mise à jour du code Pin pour la machine:" + (machineId + 1))
    while (!isValid) {
      println("Entrer un nouveau code à 6 chifferes > ")
      if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
        machinePins(machineId) = nouveauPin
        isValid = true
        println("Le nouveau code Pin a été mis à jour avec succès.")
        // Après la mise à jour réussie, retourner au menu principal
        return
      } else {
        essaisrestant -= 1
        if (essaisrestant <= 0) {
          println("Le nombre maximal de tentative a été atteint. Retour au menu principal")
          // Après avoir atteint le nombre maximum de tentatives, retourner au menu principal
          return
        }
      }
    }
  }


  def serveClient(machineId: Int, stockCafe: Array[Int], stockSucre: Array[Int], stockLait: Array[Int]): Boolean = {

    var boisson = 0
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      boisson = readInt()
    }

    var prixBase = 0.0
    var quantiteCafe = 0
    var quantiteLait = 0


    if (boisson == 1) {
      println("Vous avez choisi : Expresso")
      prixBase = 2.00
      quantiteCafe = 8
    } else if (boisson == 2) {
      println("Vous avez choisi : Cappuccino")
      prixBase = 2.50
      quantiteCafe = 6
      quantiteLait = 100
    } else if (boisson == 3) {
      println("Vous avez choisi : Latte")
      println("Choisissez la taille :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print("> ")

      val taille = readLine()
      if (taille == "1") {
        prixBase = 2.70
        quantiteCafe = 6
        quantiteLait = 120
      } else if (taille == "2") {
        prixBase = 3.20
        quantiteCafe = 8
        quantiteLait = 150
      } else if (taille == "3") {
        prixBase = 3.70
        quantiteCafe = 12
        quantiteLait = 200
      } else {
        println("Choix invalide.")
        return false
      }
    }

    // Personnalisation
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre (0g) - CHF 0.0")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val quantiteSucre = readInt()
    var sucrePrix = 0.0
    if (quantiteSucre == 1) {
      sucrePrix = 0.00

    } else if (quantiteSucre == 2 && stockSucre(machineId) >= 5) {
      stockSucre(machineId) -= 5
      sucrePrix = 0.10
    } else if (quantiteSucre == 3 && stockSucre(machineId) >= 10) {
      stockSucre(machineId) -= 10
      sucrePrix = 0.20
    } else if (quantiteSucre == 4 && stockSucre(machineId) >= 15) {
      stockSucre(machineId) -= 15
      sucrePrix = 0.30
    } else {
      println("Erreur, quantité insuffisante de sucre, veuillez refaire un choix")
      return false
    }

    // Ajout de lait supplémentaire
    var prixLaitSupplementaire = 0.0
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")
      print("> ")
      val laitChoix = readLine()
      if (laitChoix == "1") {
        println("Combien de doses ? (1 dose = 0.05l, max 3 doses)")
        print("> ")
        val dosesLait = readInt()
        if (dosesLait > 0 && dosesLait <= 3 && stockLait(machineId) >= dosesLait * 50) {
          quantiteLait += dosesLait * 50
          prixLaitSupplementaire = dosesLait * 0.05
        } else {
          println("Erreur : stock de lait insuffisant")
          return false
        }
      }
    }

    if (stockCafe(machineId) >= quantiteCafe && stockSucre(machineId) >= quantiteSucre && stockLait(machineId) >= quantiteLait) {
      val prixFinal = prixBase + sucrePrix + prixLaitSupplementaire
      // Paiement
      println("Prix total : CHF " + prixFinal)
      println("Veuillez payer en utilisant Twint.")
      val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      val tailleCode = 5 // Nombre total de caractères dans le code
      var codePaiement = 0.0 // Variable pour accumuler les caractères

      for (_ <- 1 to tailleCode) {

        codePaiement += caracteres(Random.nextInt(caracteres.length))
      }

      println(f"Votre code de paiement est : $codePaiement%.2f")
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("Paiement confirmé.")

      println("Préparation de votre boisson...")
      Thread.sleep(3000)
      println("votre boisson est prête ! Bonne dégustation !")

      stockCafe(machineId) = stockCafe(machineId) - quantiteCafe
      stockSucre(machineId) = stockSucre(machineId) - quantiteSucre
      stockLait(machineId) = stockLait(machineId) - quantiteLait
      true
    } else {
      if (stockCafe(machineId) - quantiteCafe < 0) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionner.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
      if (stockSucre(machineId) - quantiteSucre < 0) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionner.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
      if (stockLait(machineId) - quantiteLait < 0) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionner.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        false
      } else {
        true
      }
    }
  }


  def restockMachine(machineId: Int, stockCafe: Array[Int], stockSucre: Array[Int], stockLait: Array[Int]): Unit = {
    var running = true
    while (running == true) {

      println("Accès autorisé")
      println("Stocks actuels :")
      println("Poudre de café : " + stockCafe(machineId) + " g")
      println("Lait : " + (stockLait(machineId).toDouble / 1000) + " L")
      println("Sucre : " + stockSucre(machineId) + "g")

      println("Réapprovisionnement des stocks...")
      println("Quantités à ajouter :")
      print("Poudre de café (en grammes) : ")
      var ajoutCafe = readInt()
      print("Lait (en litres) : ")
      var ajoutLait = (readDouble() * 1000).toInt
      print("Sucre (en grammes : ")
      var ajoutSucre = readInt()

      //Mise à jour des stocks
      stockCafe(machineId) += ajoutCafe
      stockLait(machineId) += ajoutLait
      stockSucre(machineId) += ajoutSucre
      // Confirmation de mise à jour
      println("Niveaux de stock mis à jour avec succès.")
      println("Retour au menu principal...")
      return
    }
  }

  def main(args: Array[String]): Unit = {

    val stockCafe = Array(50, 50, 50, 50, 50)
    val stockSucre = Array(30, 30, 30, 30, 30)
    val stockLait = Array(500, 500, 500, 500, 500)
    val machinePins = Array("434343", "434343", "434343", "434343", "434343")
    var choixMachine = 1
    var machineId = choixMachine - 1
    val nombreMachines = 5
    val running = true

    while (running) {
      println("Bienvenue dans Nospresso Café")
      println("1) Client")
      println("2) Admin >")
      println("3) Quitter")
      print(">")
      var choix = readInt()

      if (choix == 1) {
        // Mode Client
        println("Bienvenue dans le Mode Client")
        println("Veuillez sélectionner la machine : (de 1 à 5)")
        print(">")
        choixMachine = readInt()
        machineId = choixMachine - 1

        if (machineId < 0 || machineId >= nombreMachines) {
          println("Erreur. Ce numéro de machine n'est pas valide.")
          // Retourner directement au menu principal
          return
        } else {
          println(s"Vous avez choisi la machine numéro : $choixMachine")
          serveClient(machineId, stockCafe, stockSucre, stockLait)
        }
      }
      else if (choix == 2) {
        // Mode Admin
        println("Bienvenue dans le Mode Admin")
        println("Veuillez sélectionner la machine : (de 1 à 5) >")
        print(">")
        choixMachine = readInt()
        machineId = choixMachine - 1

        if (machineId < 0 || machineId >= nombreMachines) {
          println("Erreur. Ce numéro de machine n'existe pas.")
          // Retourner directement au menu principal
          return
        } else {
          // Validation PIN
          if (validatePin(machineId, machinePins)) {
            println("Que voulez-vous faire ?")
            println("1) Réapprovisionner les stocks ")
            println("2) Faire une mise à jour du code PIN")
            print(">")
            var choixdeadmin = readInt()

            if (choixdeadmin == 1) {
              restockMachine(machineId, stockCafe, stockSucre, stockLait)
              // Retourner au menu principal après l'action d'admin
            } else if (choixdeadmin == 2) {
              updatePin(machineId, machinePins)
              // Retourner au menu principal après mise à jour du PIN
            } else {
              println("Ce choix n'est pas valide!")
              // Retourner au menu principal après une erreur
              return
            }
          } else {
            println("Accès refusé. Code PIN incorrect.")
          }
        }
      }
      else if (choix == 3) {
        // Quitter
        println("Vous avez quitté Nospresso. Merci !")
        return
      }
      else {
        // Cas où l'utilisateur entre une option invalide
        println("Choix invalide. Retour au menu principal.")
        return
      }
    }
  }
}


