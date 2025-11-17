import scala.io.StdIn
import java.io.{PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk") {
      if (milk >= amount) {
        milk -= amount
        true
      } else {
        println("Stock de lait insuffisant. Stock actuel:" + milk + "ml")
        false
      }
    } else if (ingredient == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else {
        println("Stock de sucre insuffisant. Stock actuel: " + sugar + "g")
        false
      }
    } else if (ingredient == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else {
        println("Stock de café insuffisant. Stock actuel: " + coffee + " g")
        false
      }
    } else {
      false
    }
  }
}

object Nospresso {

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()

    try {
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines

      if (!lignefr.isEmpty) {

        val ligne = lignefr.next
      }

      var index = 1

      while (!lignefr.isEmpty) {
        val ligne = lignefr.next
        val data = ligne.split(",")
        val pincode = data(0)
        val milk = data(1).toInt
        val sugar = data(2).toInt
        val coffee = data(3).toInt

        machines += new Machine(index, pincode, milk, sugar, coffee)
        index += 1
      }

      fr.close()
      println(machines.size + " machine(s) chargée(s) avec succès.")

    } catch {
      case _: java.io.FileNotFoundException =>
        println(" le fichier n'existe pas ")
        return null
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(new FileWriter(filename))
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      fw.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case _: java.nio.file.AccessDeniedException =>
        println("Erreur : Échec de l'écriture dans " + filename)
        println("Le fichier peut être verrouillé ou en lecture seule.")
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans " + filename)
        println(" Erreur d' entrée / sortie ")
      case _: java.io.FileNotFoundException =>
        println("Erreur : Échec de l'écriture dans " + filename)
        println(" le fichier n'existe pas ")
    }
  }


  def validatePin(machine: Machine, maxAttempts: Int = 3): Boolean = {
    var attempts = 0

    while (attempts < maxAttempts) {
      println("Entrez le code PIN administrateur :")
      val inputPin = StdIn.readLine()
      if (inputPin == machine.pincode) {
        println("Code correct.")
        return true
      } else {
        attempts += 1
        println("Code incorrect. Tentatives restantes : " + (maxAttempts - attempts))
      }
    }

    println("Trop de tentatives, la machine est bloquée.")

    false
  }

  // Mise à jour du code PIN
  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN (6 chiffres) :")
    val newPin = StdIn.readLine()
    if (newPin.length == 6 && newPin.forall(_.isDigit)) {
      machine.pincode = newPin
      println("Code PIN mis à jour avec succès.")
    } else {
      println("Le code PIN doit contenir exactement 6 chiffres.")
    }
  }

  def restockMachine(machine: Machine): Unit = {
    println("Réapprovisionnement des stocks")
    println("Stock actuel - Lait: " + "%.2f".format(machine.milk / 1000.0) + " L, Sucre: " + machine.sugar + " g, Café: " + machine.coffee + " g")

    println("Ajouter du lait (ml) :")
    val milk = StdIn.readInt()
    if (milk < 0) {
      println("Quantité négative non autorisée pour le lait. Aucune modification effectuée.")
    } else {
      machine.addIngredient("milk", milk)
    }

    println("Ajouter du sucre (g) :")
    val sugar = StdIn.readInt()
    if (sugar < 0) {
      println("Quantité négative non autorisée pour le sucre. Aucune modification effectuée.")
    } else {
      machine.addIngredient("sugar", sugar)
    }

    println("Ajouter du café (g) :")
    val coffee = StdIn.readInt()
    if (coffee < 0) {
      println("Quantité négative non autorisée pour le café. Aucune modification effectuée.")
    } else {
      machine.addIngredient("coffee", coffee)
    }

    println("Réapprovisionnement terminé.")
    println("Stock actuel - Lait: " + "%.2f".format(machine.milk / 1000.0) + " L, Sucre: " + machine.sugar + " g, Café: " + machine.coffee + " g")
  }

  def serveClient(machine: Machine): Boolean = {
    println("\nMachine " + machine.id + " en mode Client")
    var prixFinal: Double = 0.0
    var prixBoisson: Double = 0.0
    var prixSucre: Double = 0.0
    var prixLaitSupplementaire: Double = 0.0
    var commandeReussie: Boolean = false

    println("Veuillez choisir votre boisson :")
    println("1) Expresso (2.00 CHF)")
    println("2) Cappuccino (2.50 CHF)")
    println("3) Latte")
    var choixBoisson = StdIn.readInt()
    while (choixBoisson < 1 || choixBoisson > 3) {
      println(" vous devez choisir un nombre entre 1 et 3 ")
      choixBoisson = StdIn.readInt()
    }

    var cafenecessaire = 0
    var laitnecessaire = 0
    var laitsupp = 0
    var sucrenecessaire = 0

    if (choixBoisson == 1) {
      prixBoisson = 2.00
      cafenecessaire = 8
      println(" boisson selectionner = expresso ")
    } else if (choixBoisson == 2) {
      prixBoisson = 2.50
      cafenecessaire = 6
      laitnecessaire = 100
      println(" boisson selectionner = capucinno ")
    } else if (choixBoisson == 3) {
      println("Choisissez la taille :")
      println("1) Petit (2.70 CHF)")
      println("2) Moyen (3.20 CHF)")
      println("3) Grand (3.70 CHF)")
      var taille = StdIn.readInt()
      while (taille < 1 || taille > 3) {
        println(" vous devez choisir un nombre entre 1 et 3 ")
        taille = StdIn.readInt()
      }

      if (taille == 1) {
        prixBoisson = 2.70
        cafenecessaire = 6
        laitnecessaire = 120
        println(" boisson selectionner = petit Latte ")
        commandeReussie = true
      } else if (taille == 2) {
        prixBoisson = 3.20
        cafenecessaire = 8
        laitnecessaire = 150
        println(" boisson selectionner = moyen latte ")
        commandeReussie = true
      } else if (taille == 3) {
        prixBoisson = 3.70
        cafenecessaire = 12
        laitnecessaire = 200
        println(" boisson selectionner = grand Latte ")
        commandeReussie = true
      }
    }

    if (machine.coffee < cafenecessaire) {
      println("Erreur : Stock insuffisant de café pour préparer la boisson.")
      return false
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Non")
    println("2) Peu (0.10 CHF)")
    println("3) Moyen (0.20 CHF)")
    println("4) Beaucoup (0.30 CHF)")
    var choixSucre = StdIn.readInt()
    while (choixSucre < 1 || choixSucre > 4) {
      println(" vous devez choisir un nombre entre 1 et 4 ")
      choixSucre = StdIn.readInt()
    }

    if (choixSucre == 1) {
      println(" sucre = pas de sucre ")
      commandeReussie = true
    } else if (choixSucre == 2) {
      sucrenecessaire = 5
      prixSucre = 0.10
      println(" sucre = peu de sucre ")
      commandeReussie = true
    } else if (choixSucre == 3) {
      sucrenecessaire = 10
      prixSucre = 0.20
      println(" sucre = moyen de sucre ")
      commandeReussie = true
    } else if (choixSucre == 4) {
      sucrenecessaire = 15
      prixSucre = 0.30
      println(" sucre = beaucoup de sucre ")
      commandeReussie = true
    }

    if (machine.sugar < sucrenecessaire) {
      println("Erreur : Stock insuffisant de sucre.")
      return false
    }

    if (choixBoisson == 2 || choixBoisson == 3) {

      if (machine.milk < laitnecessaire) {
        println("Erreur : Stock insuffisant de lait pour la boisson.")
        return false
      }

      println("Souhaitez-vous ajouter du lait supplémentaire ? (0.05 CHF par dose, max 3 doses)")
      println("1) Non  2) Oui")
      var ajoutLait = StdIn.readInt()
      while (ajoutLait < 1 || ajoutLait > 2) {
        println("Vous devez choisir un nombre entre 1 et 2")
        ajoutLait = StdIn.readInt()
      }

      if (ajoutLait == 2) {
        println("Combien de doses souhaitez-vous ajouter ? (1 à 3)")
        var laitSupplementaire = StdIn.readInt()
        while (laitSupplementaire < 1 || laitSupplementaire > 3) {
          println("Vous devez choisir un nombre entre 1 et 3")
          laitSupplementaire = StdIn.readInt()
        }

        if (laitSupplementaire == 1) {
          laitsupp = 50
        } else if (laitSupplementaire == 2) {
          laitsupp = 100
        } else if (laitSupplementaire == 3) {
          laitsupp = 150
        }

        if (machine.milk < (laitnecessaire + laitsupp)) {
          println("Erreur : Stock insuffisant de lait pour le supplément demandé.")
          return false
        }

        prixLaitSupplementaire = laitSupplementaire * 0.05
      }
    }
    machine.removeIngredient("coffee", cafenecessaire)

    if (sucrenecessaire > 0) {
      machine.removeIngredient("sugar", sucrenecessaire)
    }

    if (laitnecessaire > 0) {
      machine.removeIngredient("milk", laitnecessaire)
      if (laitsupp > 0) {
        machine.removeIngredient("milk", laitsupp)
      }
    }

    prixFinal = prixBoisson + prixSucre + prixLaitSupplementaire


    if (prixBoisson > 0 && prixSucre > 0 && prixLaitSupplementaire > 0) {
      printf(
        " le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson) + %.2f CHF (sucre) + %.2f CHF ( lait)\n  ",
        prixFinal, prixBoisson, prixSucre, prixLaitSupplementaire
      )
    } else if (prixBoisson > 0 && prixSucre == 0 && prixLaitSupplementaire > 0) {
      printf(
        " le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  + %.2f CHF ( lait)\n  ",
        prixFinal, prixBoisson, prixLaitSupplementaire
      )
    } else if (prixBoisson > 0 && prixSucre > 0 && prixLaitSupplementaire == 0) {
      printf(
        " le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  + %.2f CHF (sucre)\n  ",
        prixFinal, prixBoisson, prixSucre
      )
    } else if (prixBoisson > 0 && prixSucre == 0 && prixLaitSupplementaire == 0) {
      printf(
        " le prix de la boisson est de : %.2f CHF = %.2f CHF (boisson)  \n  ",
        prixFinal, prixBoisson
      )
    }

    println("\n veuillez procédez au paiement par twint ")

    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code_twint = ""
    for (_ <- 1 to 5) {
      val indexAleatoire = Random.nextInt(chars.length)
      code_twint += chars(indexAleatoire)
    }

    println("Votre code twint  est : " + code_twint)
    println("(en attente de la confirmation de votre paiement) \n  Merci ! votre paiement est confirmé ")

    println(" préparation de votre boisson...")
    Thread.sleep(3000)

    if (choixBoisson == 1) {
      println(" votre expresso est prêt")
    } else if (choixBoisson == 2) {
      println(" votre capucinno est prêt")
    } else if (choixBoisson == 3) {
      println(" votre Latte est prêt")
    }

    true
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)
    if (machines != null) {
      println("Bienvenue dans le système Nospresso!")

      for (machine <- machines) {
        println(
          "ID: " + machine.id +
            ", Code PIN: " + machine.pincode +
            ", Lait: " + "%.2f".format(machine.milk / 1000.0) + " L, Sucre: " + machine.sugar + " g, Café: " + machine.coffee + " g"
        )
      }

      var stop = false
      var mode = 0

      while (!stop) {
        println("Sélectionnez un mode : 1) Client, 2) Admin, 3) Quitter")
        mode = StdIn.readInt()

        if (mode == 1 || mode == 2) {
          println("Sélectionnez l'ID de la machine à utiliser :")
          val machineId = StdIn.readInt()
          var machinedisponible = false
          var selectedMachine: Machine = null

          for (machine <- machines) {
            if (machine.id == machineId) {
              machinedisponible = true
              selectedMachine = machine
            }
          }

          if (machinedisponible) {
            if (mode == 1) {
              // Mode Client
              serveClient(selectedMachine)
            } else if (mode == 2) {
              // Mode Admin
              val pinOk = validatePin(selectedMachine)
              if (pinOk) {
                println("Options Admin : 1) Réapprovisionnement 2) Mise à jour du PIN")
                val choixAdmin = StdIn.readInt()
                if (choixAdmin == 1) {
                  restockMachine(selectedMachine)
                } else if (choixAdmin == 2) {
                  updatePin(selectedMachine)
                } else {
                  println("Option invalide.")
                }
              } else {

                println(" arrêt du programme après 3 échecs PIN.")
                savecsv(filename, machines)
                stop = true
              }
            }
          } else {
            println("ID de machine invalide.")
          }

        } else if (mode == 3) {

          savecsv(filename, machines)
          stop = true
        } else {
          println("Option invalide.")
        }
      }

    } else {
      println(" il n' est pas possible de lire le fichier ")
    }
  }
}
