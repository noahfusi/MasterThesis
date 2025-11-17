import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.util.Random

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    println("Entrez le code PIN de la machine " + machineId + " :")
    print("> ")
    val tentatives = 3
    var code_pin = readLine()
    for (t <- 1 to 3) {
      if (code_pin != machinePins(machineId)) {
        println("Code PIN incorrect. " + (tentatives - t) + " restantes.")
        if (tentatives - t != 0) {
          print("> ")
          code_pin = readLine()
        }
      } else {
        println("Accès accordé.")
        return true
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    Thread.sleep(2000)
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + machineId + ".")
    print("Entrez un nouveau code PIN à 6 chiffres > ")
    var code_pin = readLine()
    while (code_pin.length() != 6) {
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      code_pin = readLine()
    }
    machinePins(machineId) = code_pin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {

    var prix_sucre = 0.0
    var prix_cafe = 0.0
    var prix_lait = 0.0
    val prix_un_sucre = 5
    val dose_de_lait = 0.05
    var consommation_cafe = 0
    var consommation_lait = 0.0
    var consommation_supplement_lait = 0.0
    var consommation_supplement_sucre = 0
    val caractères = "A1B2C3D4E5F6G7H8I9J1K2L3MN5OP7QR9S6T5U4V3W2X1YZ"

    print("Quel café voulez vous?:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    var choix_de_cafe = readInt()
    while ((choix_de_cafe < 1) || (choix_de_cafe > 3)) {
      println("Le choix de café n'est pas correct\n")
      print("Quel café voulez vous?:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
      choix_de_cafe = readInt()
    }
    if (choix_de_cafe == 1) {
      prix_cafe = 2.0
      consommation_cafe = 8
    }
    if (choix_de_cafe == 2) {
      prix_cafe = 2.5
      consommation_cafe = 6
      consommation_lait = 0.1
    }
    var taille_latte = 0
    if (choix_de_cafe == 3) {
      print("Quel type de latte voulez vous?:\n1) CHF 2.70 (Petit) \n2)CHF 3.20 (Moyen)\n3)CHF 3.70 (Grand)\n>")
      taille_latte = readInt()
      while ((taille_latte < 1) || (taille_latte > 3)) {
        println("Le choix de de latte n'est pas correct\n")
        print("Quel type de latte voulez vous?:\n1) CHF 2.70 (Petit) \n2)CHF 3.20 (Moyen)\n3)CHF 3.70 (Grand)\n>")
        taille_latte = readInt()
      }
      if (taille_latte == 1) {
        prix_cafe = 2.7
        consommation_cafe = 6
        consommation_lait = 0.12
      }
      else if (taille_latte == 2) {
        prix_cafe = 3.2
        consommation_cafe = 8
        consommation_lait = 0.15
      }
      else if (taille_latte == 3) {
        prix_cafe = 3.7
        consommation_cafe = 12
        consommation_lait = 0.2
      }
    }
    var supplement_lait = 0
    if (choix_de_cafe == 2 || choix_de_cafe == 3) {
      print("Souhaitez-vous ajouter du lait en supplement ?\n1) Oui\n2) Non\n>")
      supplement_lait = readInt()
      while ((supplement_lait < 1) || (supplement_lait > 2)) {
        println("Le choix de supplement de lait n'est pas correct\n")
        print("Souhaitez-vous ajouter du lait en supplement ?\n1) Oui\n2) Non\n>")
        supplement_lait = readInt()
      }

      if (supplement_lait == 1) {
        print("Combien de doses voulez-vous?\n1) 1 doses\n2) 2 doses\n3) 3 doses\n >")
        var nb_doses_de_lait = readInt()
        if ((nb_doses_de_lait < 0) || (nb_doses_de_lait > 3)) {
          while ((nb_doses_de_lait < 0) || (nb_doses_de_lait > 3)) {
            println("Le choix de doses de lait n'est pas correct\n")
            print("Combien de doses voulez-vous?\n1) 1 doses\n2) 2 doses\n3) 3 doses\n >")
            nb_doses_de_lait = readInt()
          }
        }
        else if (nb_doses_de_lait == 1) {
          prix_lait = 0.05
          consommation_supplement_lait = dose_de_lait * nb_doses_de_lait
        }
        else if (nb_doses_de_lait == 2) {
          prix_lait = 0.1
          consommation_supplement_lait = dose_de_lait * 2
        }
        else if (nb_doses_de_lait == 3) {
          prix_lait = 0.15
          consommation_supplement_lait = dose_de_lait * 1
        }
      }
    }
    print("Souhaitez vous ajouter du sucre?: \n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
    var qte_sucre = readInt()
    while ((qte_sucre < 1) || (qte_sucre > 4)) {
      println("Le choix de sucre n'est pas correct\n")
      print("Souhaitez vous ajouter du sucre?: \n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
      qte_sucre = readInt()
    }
    if (qte_sucre == 2) {
      prix_sucre = 0.1
      consommation_supplement_sucre = prix_un_sucre * (qte_sucre - 1)
    }
    else if (qte_sucre == 3) {
      prix_sucre = 0.2
      consommation_supplement_sucre = prix_un_sucre * (qte_sucre - 1)
    }
    else if (qte_sucre == 4) {
      prix_sucre = 0.3
      consommation_supplement_sucre = prix_un_sucre * (qte_sucre - 1)
    }
    val prix_final = prix_sucre + prix_lait + prix_cafe
    if (consommation_cafe > coffeeStocks(machineId)) {
      println("Erreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une autre boisson ou verifier les stocks en mode Admin pour la machine séléctionée.\n")
      return false
    }
    else if ((consommation_supplement_lait + consommation_lait) > milkStocks(machineId)) {
      println("Erreur : Quantite de lait insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson pour la machine séléctionée.\n")
      return false
    }
    else if (consommation_supplement_sucre > sugarStocks(machineId)) {
      println("Erreur : Quantite de sucre insuffisante pour preparer la boisson selectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson pour la machine séléctionée.\n")
      return false
    }
    else {
      if (choix_de_cafe == 1) {
        println("Boisson séléctionnée : Expresso")
      }
      if (choix_de_cafe == 2) {
        println("Boisson séléctionnée : Cappucino")
      }
      if ((choix_de_cafe == 3) && (taille_latte == 1)) {
        println("Boisson séléctionnée : Latte (Petit) ")
      }
      if ((choix_de_cafe == 3) && (taille_latte == 2)) {
        println("Boisson séléctionnée : Latte (Moyen) ")
      }
      if ((choix_de_cafe == 3) && (taille_latte == 3)) {
        println("Boisson séléctionnée : Latte (Grand) ")
      }
      if (qte_sucre == 1) {
        println("Niveau de sucre : Sans sucre ")
      }
      if (qte_sucre == 2) {
        println("Niveau de sucre : Peu (5g) ")
      }
      if (qte_sucre == 3) {
        println("Niveau de sucre : Moyen (10g) ")
      }
      if (qte_sucre == 4) {
        println("Niveau de sucre : Beaucoup (15g) ")
      }
      if (supplement_lait == 2) {
        println("Lait en supplément: Non")
      }
      if (dose_de_lait == 1) {
        println("Lait en supplément: Peu (0.05L) ")
      }
      if (dose_de_lait == 2) {
        println("Lait en supplément: Moyen (0.1L) ")
      }
      if (dose_de_lait == 3) {
        println("Lait en supplément: Non: Beaucoup (0.15L) ")
      }
      if ((prix_sucre > 0) && (prix_lait == 0)) {
        println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_sucre + " = CHF " + prix_final + "\n")
      }
      if ((prix_sucre == 0) && (prix_lait > 0)) {
        println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_lait + " = CHF " + prix_final + "\n")
      }
      if ((prix_sucre > 0) && (prix_lait > 0)) {
        println("Prix Total = " + "CHF " + prix_cafe + " + CHF " + prix_sucre + " + CHF " + prix_lait + " = CHF " + prix_final + "\n")
      }
      if ((prix_sucre == 0) && (prix_lait == 0)) {
        println("Prix Total " + "= CHF " + prix_final + "\n")
      }

      sugarStocks(machineId) = sugarStocks(machineId) - consommation_supplement_sucre
      milkStocks(machineId) = milkStocks(machineId) - consommation_lait - consommation_supplement_lait
      coffeeStocks(machineId) = coffeeStocks(machineId) - consommation_cafe

      println("Veuillez payer en utilisant Twint.")
      var twint = ""
      var i = 0
      while (i < 5) {
        val nombre_et_caractères = Random.nextInt(caractères.length)
        val carac = caractères.charAt(nombre_et_caractères)
        twint = twint + carac
        i = i + 1
      }
      println("Votre code de paiement: " + twint)
      println("En attente de paiement...")
      Thread.sleep(3000)
      println("\nPaiement confirmé\n")
      println("Préparation de votre boisson...\n")
      Thread.sleep(5000)
      if (choix_de_cafe == 1) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
      }
      if (choix_de_cafe == 2) {
        println("Votre Cappucino est prêt ! Bonne dégustation !")
      }
      if (choix_de_cafe == 3) {
        println("Votre Latte est prêt ! Bonne dégustation !")
      }
    }
    return true
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("Stock disponible: \n")
    print("Stock de sucre: " + sugarStocks(machineId) + "g" + "\nStock de poudre de café: " + coffeeStocks(machineId) + "g" + "\nStock de lait: " + milkStocks(machineId) + "L" + "\n\n")
    println("Voulez-vous réapprovisionner les stocks?:\n1)Oui \n2)Non\n")
    var choix_du_reapprovisionnement = readInt()
    if (choix_du_reapprovisionnement < 1 || choix_du_reapprovisionnement > 2) {
      while (choix_du_reapprovisionnement < 1 || choix_du_reapprovisionnement > 2) {
        println("Le choix est incorrect\n")
        println("Voulez-vous réapprovisionner les stocks?:\n1)Oui \n2)Non\n")
        choix_du_reapprovisionnement = readInt()
      }
    }
    if (choix_du_reapprovisionnement == 1) {
      println("Réapprovisionnement des stocks...")
      println("Ajout:\n")
      println("Poudre à café:")
      val reapprovisionnement_poudre_cafe = readInt()
      println("Lait:")
      val reapprovisionnement_lait = readInt()
      println("Sucre:")
      val reapprovisionnement_sucre = readInt()
      sugarStocks(machineId) = sugarStocks(machineId) + reapprovisionnement_sucre
      milkStocks(machineId) = milkStocks(machineId) + reapprovisionnement_lait
      coffeeStocks(machineId) = coffeeStocks(machineId) + reapprovisionnement_poudre_cafe
      println("Niveaux des stocks mis à jour.")
    }
  }


  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    var machinePins = Array.fill(nbMachines)("434343")
    var coffeeStocks = Array.fill(nbMachines)(50)
    var sugarStocks = Array.fill(nbMachines)(30)
    var milkStocks = Array.fill(nbMachines)(0.5)
    var continue = true

    while (continue) {
      print("Veuillez sélectionner le mode:\n1) Client\n2) Admin\n3) Quitter\n> ")
      var mode = readInt()
      while ((mode < 1) || (mode > 3)) {
        println("Le mode inscrit est incorrect\n")
        print("Veuillez sélectionner le mode:\n1) Client\n2) Admin\n3) Quitter\n > ")
        mode = readInt()
      }

      if (mode == 1) {
        print("\nVeuillez séléctionner une machine (0-5) > ")
        var machineID = readInt()
        while (machineID < 0 || machineID > 5) {
          print("Veuillez séléctionner une machine (0-5) > ")
          machineID = readInt()
        }
        println("\nMachine séléctionée : " + machineID)
        var transaction = serveClient(machineID, coffeeStocks, sugarStocks, milkStocks)
        while (!transaction) {
          print("Veuillez séléctionner une machine (0-5) > ")
          var machineID = readInt()
          while (machineID < 0 || machineID > 5) {
            print("Veuillez séléctionner une machine (0-5) > ")
            machineID = readInt()
          }
          println("\nMachine séléctionée : " + machineID)
          transaction = serveClient(machineID, coffeeStocks, sugarStocks, milkStocks)
        }
        println("Retour au menu principal...")
        Thread.sleep(2000)

      } else if (mode == 2) {
        print("Veuillez séléctionner une machine (0-5) > ")
        var machineID = readInt()
        while (machineID < 0 || machineID > 5) {
          print("Veuillez séléctionner une machine (0-5) > ")
          machineID = readInt()
        }
        val validation = validatePin(machineID, machinePins)
        if (!validation) {
          continue = false
        } else {
          println("\nMachine séléctionée : " + machineID)
          print("\nVeuillez sélétionner une action à éxécuter sur la machine " + machineID + "\n1) Restockage\n2) Mise à jour du PIN\n> ")
          var action = readInt()
          while (action != 1 && action != 2) {
            println("\nAction incorrecte\n")
            print("\nVeuillez sélétionner une action à éxécuter sur la machine " + machineID + "\n1) Restockage\n2) Mise à jour du PIN\n> ")
            print("> ")
            action = readInt()
          }
          if (action == 1) {
            restockMachine(machineID, coffeeStocks, sugarStocks, milkStocks)
          } else if (action == 2) {
            updatePin(machineID, machinePins)
          }
          println("Retour au menu principal...")
          Thread.sleep(2000)
        }

      } else if (mode == 3) {
        println("Aurevoir.\n\n")
        Thread.sleep(2000)
        continue = false
      }
    }
  }
}
