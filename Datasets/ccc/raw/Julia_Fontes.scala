
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io._

object Main {

  var fileErrorOccurred = false

  class Machine(val id: Int, var pincode: String,var cafe: Int, var sucre: Int, var lait: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      val ing = ingredient.toLowerCase
      if (ing == "cafe") {
        cafe += amount
      }
      else if (ing == "sucre") {
        sucre += amount
      }
      else if (ing == "lait") {
        lait += amount
      }
      else {
        println("Ingrédient inconnu : " + ingredient)
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      val ing = ingredient.toLowerCase
      if (ing == "cafe") {
        if (cafe >= amount) {
          cafe -= amount
          true
        } else false
      }
      else if (ing == "sucre") {
        if (sucre >= amount) {
          sucre -= amount
          true
        } else false
      }
      else if (ing == "lait") {
        if (lait >= amount) {
          lait -= amount
          true
        } else false
      }
      else {
        false
      }
    }
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
   println("Chargement des machines depuis " + filename + "..." )
    val buffer = ArrayBuffer[Machine]()
    try {
      // Avec entête: PINCODE,CAFE,SUCRE,LAIT
      val lines = Source.fromFile(filename).getLines().drop(1).zipWithIndex
      for ((line, idx) <- lines) {
        val parts = line.split(",")
        if (parts.length == 4) {
          val code     = parts(0)
          val cafeVal  = parts(1).toInt
          val sucreVal = parts(2).toInt
          val laitVal  = parts(3).toInt
          val machine = new Machine(idx + 1, code, cafeVal, sucreVal, laitVal)
          buffer += machine

          // Affichage des détails de la machine chargée
          println("Machine " + (idx + 1) + " chargée: \nID:" + machine.id  )
          println("Code PIN: " + machine.pincode)
          println("Lait: " + (machine.lait / 1000.0) + "L")
          println("Sucre: " + machine.sucre + "g")
          println("Café: " + machine.cafe + "g")
        }
      }
    } catch {
      case _: FileNotFoundException =>
        println("Erreur: Fichier CSV introuvable.")
        fileErrorOccurred = true
      case e: IOException =>
        println("Erreur lecture du CSV: " + e.getMessage)
        fileErrorOccurred = true
    }
    println("Chargement réussi de " + buffer.size + " machine(s).")
    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))
      // Entête
      writer.write("PINCODE,CAFE,SUCRE,LAIT\n")
      for (m <- machines) {
        writer.write(m.pincode + "," + m.cafe + "," + m.sucre + "," + m.lait + "\n")
      }
      writer.close()
    } catch {
      case e: IOException =>
        println("Erreur écriture CSV: " + e.getMessage)
        fileErrorOccurred = true
    }
  }

  var machines = ArrayBuffer[Machine]()
  var continuerClient = true

  def main(args: Array[String]): Unit = {
    machines = loadcsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv")
    if (fileErrorOccurred) {
      println("Arrêt du programme suite à erreur de fichier.")
      return
    }

    while (continuerClient) {
      println("Nospressocafé \nVeuillez sélectionner votre mode:")
      println("1) Client\n2) Admin\n3) Quitter\n> ")
      val choix = readInt()
      if (choix == 1) {
        serveClient()
      }
      else if (choix == 2) {
        modeAdmin()
      }
      else if (choix == 3) {
        println("Merci et à bientôt!")
        continuerClient = false
      }
      else {
        println("Choix invalide.")
      }
    }

    if (!fileErrorOccurred) {
      savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
    }
  }


  def serveClient(): Unit = {
    println("Veuillez sélectionner une machine (1-" + machines.size + "): ")
    var input = readInt()
    while (input < 1 || input > machines.size) {
      println("Machine invalide, réessayez (1-" + machines.size + "): ")
      input = readInt()
    }
    val machineIndex = input - 1


    var pin = ""
    val car = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for (i <- 1 to 5) {
      val twint = (Math.random() * car.length).toInt
      pin += car(twint)
    }

    println("Veuillez sélectionner votre boisson: ")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    println("> ")
    var boissonDesiree = readInt()

    while (boissonDesiree != 1 && boissonDesiree != 2 && boissonDesiree != 3) {
      println("Entrée invalide, veuillez entrer '1', '2' ou '3'")
      boissonDesiree = readInt()
    }

    var sucre = 0
    var prix = 0.0
    var ingredientsSuffisants = true

    if (boissonDesiree == 1) {
      println("Vous avez choisi un Expresso.")
      prix = 2.00

      // Vérif café (8g) et minimum 5g de sucre si l'utilisateur en veut
      val haveCafe = machines(machineIndex).removeIngredient("cafe", 8)
      if (!haveCafe) {
        println("Il manque du café pour préparer l'expresso.")
        ingredientsSuffisants = false
      }

      // Choix sucre
      if (ingredientsSuffisants) {
        println("Souhaitez-vous ajouter du sucre? ")
        println("1) Sans sucre ")
        println("2) Peu (5g) - CHF 0.10 ")
        println("3) Moyen (10g) - CHF 0.20 ")
        println("4) Beaucoup (15g) - CHF 0.30 ")
        var sucreChoix = readInt()
        while (sucreChoix != 1 && sucreChoix != 2 && sucreChoix != 3 && sucreChoix != 4) {
          println("Entrée invalide. Entrez 1, 2, 3 ou 4.")
          sucreChoix = readInt()
        }
        if (sucreChoix == 2) {
          if (machines(machineIndex).sucre < 5) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 5)
            sucre = 5
            prix += 0.10
          }
        }
        else if (sucreChoix == 3) {
          if (machines(machineIndex).sucre < 10) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 10)
            sucre = 10
            prix += 0.20
          }
        }
        else if (sucreChoix == 4) {
          if (machines(machineIndex).sucre < 15) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 15)
            sucre = 15
            prix += 0.30
          }
        }
      }

      if (ingredientsSuffisants) {
        println("Votre expresso contient " + sucre + "g de sucre.")
        println("Le prix total est " + prix + " CHF.")
      }
      else {
        println("Impossible de préparer l'expresso.")
      }
    }


    if (boissonDesiree == 2) {
      println("Vous avez choisi un Cappuccino.")
      var cappuccinoPrix = 2.50
      // cappuccino => 6 g café, 100 ml lait
      val haveCafe = machines(machineIndex).removeIngredient("cafe", 6)
      if (!haveCafe) {
        println("Il manque du café pour préparer le cappuccino.")
        ingredientsSuffisants = false
      }
      val haveMilk = machines(machineIndex).removeIngredient("lait", 100)
      if (!haveMilk) {
        println("Il manque du lait pour préparer le cappuccino.")
        ingredientsSuffisants = false
      }

      if (ingredientsSuffisants) {
        println("Souhaitez-vous ajouter du sucre? ")
        println("1) Sans sucre ")
        println("2) Peu (5g) - CHF 0.10 ")
        println("3) Moyen (10g) - CHF 0.20 ")
        println("4) Beaucoup (15g) - CHF 0.30 ")
        var sucreChoix = readInt()
        while (sucreChoix != 1 && sucreChoix != 2 && sucreChoix != 3 && sucreChoix != 4) {
          println("Entrée invalide. Entrez 1, 2, 3 ou 4.")
          sucreChoix = readInt()
        }
        if (sucreChoix == 2) {
          if (machines(machineIndex).sucre < 5) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 5)
            sucre = 5
            cappuccinoPrix += 0.10
          }
        }
        else if (sucreChoix == 3) {
          if (machines(machineIndex).sucre < 10) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 10)
            sucre = 10
            cappuccinoPrix += 0.20
          }
        }
        else if (sucreChoix == 4) {
          if (machines(machineIndex).sucre < 15) {
            println("Il manque du sucre.")
            ingredientsSuffisants = false
          } else {
            machines(machineIndex).removeIngredient("sucre", 15)
            sucre = 15
            cappuccinoPrix += 0.30
          }
        }
      }

      if (ingredientsSuffisants) {
        println("Souhaitez-vous ajouter du lait en supplément? ")
        println("1) Oui\n2) Non\n> ")
        var choixLait = readInt()
        while (choixLait != 1 && choixLait != 2) {
          println("Entrée invalide. Entrez 1 ou 2.")
          choixLait = readInt()
        }
        if (choixLait == 1) {
          println("Combien de doses de lait ? (max 3)")
          var doses = readInt()
          while (doses < 1 || doses > 3) {
            println("Doses invalides. Entrez entre 1 et 3.")
            doses = readInt()
          }
          // 1 dose = 50 ml = +0.05 CHF
          var neededMilk = doses * 50
          var costMilk = 0.05 * doses
          if (!machines(machineIndex).removeIngredient("lait", neededMilk)) {
            println("Il manque du lait supplémentaire.")
            ingredientsSuffisants = false
          } else {
            cappuccinoPrix += costMilk
          }
        }
      }

      if (ingredientsSuffisants) {
        println("Votre cappuccino contient " + sucre + "g de sucre.")
        println("Le prix total est " + cappuccinoPrix + " CHF.")
      }
      else {
        println("Impossible de préparer le cappuccino.")
      }
      prix = cappuccinoPrix
    }

    if (boissonDesiree == 3) {
      println("Vous avez choisi un Latte.")
      println("Quelle taille? ")
      println("1) Petit (6g café, 120 ml lait) - 2.70 CHF")
      println("2) Moyen (8g café, 150 ml lait) - 3.20 CHF")
      println("3) Grand (12g café, 200 ml lait) - 3.70 CHF")
      var taille = readInt()
      while (taille != 1 && taille != 2 && taille != 3) {
        println("Entrée invalide, 1/2/3 seulement.")
        taille = readInt()
      }

      if (taille == 1) {
        prix = 2.70
        val haveCafe = machines(machineIndex).removeIngredient("cafe", 6)
        val haveMilk = machines(machineIndex).removeIngredient("lait", 120)
        if (!haveCafe) {
          println("Il manque du café pour le Latte Petit.")
          ingredientsSuffisants = false
        }
        if (!haveMilk) {
          println("Il manque du lait pour le Latte Petit.")
          ingredientsSuffisants = false
        }
      }
      else if (taille == 2) {
        prix = 3.20
        val haveCafe = machines(machineIndex).removeIngredient("cafe", 8)
        val haveMilk = machines(machineIndex).removeIngredient("lait", 150)
        if (!haveCafe) {
          println("Il manque du café pour le Latte Moyen.")
          ingredientsSuffisants = false
        }
        if (!haveMilk) {
          println("Il manque du lait pour le Latte Moyen.")
          ingredientsSuffisants = false
        }
      }
      else if (taille == 3) {
        prix = 3.70
        val haveCafe = machines(machineIndex).removeIngredient("cafe", 12)
        val haveMilk = machines(machineIndex).removeIngredient("lait", 200)
        if (!haveCafe) {
          println("Il manque du café pour le Latte Grand.")
          ingredientsSuffisants = false
        }
        if (!haveMilk) {
          println("Il manque du lait pour le Latte Grand.")
          ingredientsSuffisants = false
        }
      }

      // Choix de sucre
      if (ingredientsSuffisants) {
        println("Quel quantité de sucre? ")
        println("1) Pas de sucre")
        println("2) Peu de sucre (5g) +0.10 CHF")
        println("3) Moyen (10g) +0.20 CHF")
        println("4) Beaucoup (15g) +0.30 CHF")
        var sucreChoix = readInt()
        while (sucreChoix != 1 && sucreChoix != 2 && sucreChoix != 3 && sucreChoix != 4) {
          println("Entrée invalide, tapez 1/2/3/4.")
          sucreChoix = readInt()
        }
        if (sucreChoix == 2) {
          if (!machines(machineIndex).removeIngredient("sucre", 5)) {
            println("Manque de sucre.")
            ingredientsSuffisants = false
          } else {
            sucre = 5
            prix += 0.10
          }
        }
        else if (sucreChoix == 3) {
          if (!machines(machineIndex).removeIngredient("sucre", 10)) {
            println("Manque de sucre.")
            ingredientsSuffisants = false
          } else {
            sucre = 10
            prix += 0.20
          }
        }
        else if (sucreChoix == 4) {
          if (!machines(machineIndex).removeIngredient("sucre", 15)) {
            println("Manque de sucre.")
            ingredientsSuffisants = false
          } else {
            sucre = 15
            prix += 0.30
          }
        }
      }

      // Lait supplémentaire
      if (ingredientsSuffisants) {
        println("Souhaitez-vous ajouter du lait en supplément? ")
        println("1) Oui\n2) Non\n> ")
        var choixLait = readInt()
        while (choixLait != 1 && choixLait != 2) {
          println("Entrée invalide, 1 ou 2.")
          choixLait = readInt()
        }
        if (choixLait == 1) {
          println("Combien de doses de lait? (max 3)")
          var doses = readInt()
          while (doses < 1 || doses > 3) {
            println("Doses invalides.")
            doses = readInt()
          }
          val neededMilk = doses * 50
          val costMilk = doses * 0.05
          if (!machines(machineIndex).removeIngredient("lait", neededMilk)) {
            println("Il manque du lait supplémentaire.")
            ingredientsSuffisants = false
          } else {
            prix += costMilk
          }
        }
      }

      if (ingredientsSuffisants) {
        println("Latte commandé avec " + sucre + "g de sucre.")
        println("Prix total = " + prix + " CHF.")
      } else {
        println("Impossible de préparer le Latte.")
      }
    }

    // Vérification si tout est OK
    if (ingredientsSuffisants) {
      println("Veuillez payer en utilisant Twint. \nVotre code de paiement est: " + pin + " \nEn attente de validation du paiement...")
      Thread.sleep(3000)
      println("Merci! Paiement accepté. Préparation en cours...")
      Thread.sleep(2000)
      println("Votre boisson est prête! Bonne dégustation.")

      // On peut immédiatement sauvegarder les stocks mis à jour
      if (!fileErrorOccurred) {
        savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
      }
    } else {
      println("La commande a échoué, retour au menu principal.")
    }
  }

  def modeAdmin(): Unit = {
    println("Veuillez sélectionner une machine (1-" + machines.size + "): ")
    val input = readInt()
    if (input < 1 || input > machines.size) {
      println("Machine invalide.")
      return
    }
    val machineIndex = input - 1

    if (!validatePin(machineIndex)) {
      return
    }

    println("Que voulez-vous faire ?")
    println("1) Gérer stocks\n2) Mettre à jour PIN")
    val choixAdmin = readInt()
    if (choixAdmin == 1) {
      restockMachine(machineIndex)
    }
    else if (choixAdmin == 2) {
      updatePin(machineIndex)
    }
    else {
      println("Choix invalide, retour menu principal.")
    }
  }

  def validatePin(index: Int): Boolean = {
    var attempts = 3
    println("Entrez le code PIN de la machine " + (index + 1) + ": ")
    var pin = readLine()
    while (attempts > 0) {
      if (pin == machines(index).pincode) {
        println("Accès Admin accordé.")
        return true
      } else {
        attempts -= 1
        if (attempts == 0) {
          println("Trop de tentatives. Fin du programme.")
          return false
        } else {
          println("Code PIN incorrect. " + attempts + " tentatives restantes.")
          pin = readLine()
        }
      }
    }
    false
  }

  def restockMachine(index: Int): Unit = {
    val m = machines(index)
    println("Stocks actuels (machine " + (index + 1) + "):")
    println("Café:  " + m.cafe + " g")
    println("Sucre: " + m.sucre + " g")
    println("Lait:  " + m.lait + " ml")

    println("Actions:\n1) Réappro café\n2) Réappro sucre\n3) Réappro lait\n4) Retour\n>")
    val action = readInt()
    if (action == 1) {
      println("Quantité de café (g) à ajouter?")
      val addCafe = readInt()
      m.addIngredient("cafe", addCafe)
      println("Nouveau stock café: " + m.cafe + " g")
      if (!fileErrorOccurred) {
        savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
      }
    }
    else if (action == 2) {
      println("Quantité de sucre (g) à ajouter?")
      val addSucre = readInt()
      m.addIngredient("sucre", addSucre)
      println("Nouveau stock sucre: " + m.sucre + " g")
      if (!fileErrorOccurred) {
        savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
      }
    }
    else if (action == 3) {
      println("Quantité de lait à ajouter (en litres)?")
      val laitLitres = readDouble()
      m.addIngredient("lait", (laitLitres * 1000).toInt)
      println("Nouveau stock lait: " + m.lait/1000.0 + " L")
      if (!fileErrorOccurred) {
        savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
      }
    }
    else if (action == 4) {
      println("Retour au menu principal.")
    }
    else {
      println("Choix invalide, retour au menu principal.")
    }
  }

  def updatePin(index: Int): Unit = {
    println("Mise à jour du code PIN (6 chiffres) pour la machine " + (index + 1))
    var newPin = readLine()
    while (!isValidPin(newPin)) {
      println("PIN invalide, 6 chiffres requis.")
      newPin = readLine()
    }
    machines(index).pincode = newPin
    println("PIN machine " + (index + 1) + " mis à jour.")
    if (!fileErrorOccurred) {
      savecsv("/Users/juliafontes/IdeaProjects/exercice 3 final/src/machines.csv", machines)
    }
  }

  def isValidPin(pin: String): Boolean = {
    if (pin.length != 6) return false
    for (ch <- pin) {
      if (ch < '0' || ch > '9') return false
    }
    true
  }
}