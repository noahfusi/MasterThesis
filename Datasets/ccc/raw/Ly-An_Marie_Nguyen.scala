import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import java.io.{PrintWriter,FileWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    try {
      if (ingredient == "Milk") milk += amount
      else if (ingredient == "Sugar") sugar += amount
      else if (ingredient == "Coffee") coffee += amount
    } catch {
      case _: Exception => println("Une erreur s'est produite lors de l'ajout d'un ingrédient.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    try {
      if (ingredient == "Milk") {
        milk -= amount
        return true
      }
      else if (ingredient == "Sugar") {
        sugar -= amount
        return true
      }
      else if (ingredient == "Coffee") {
        coffee -= amount
        return true
      }
      else return false
    } catch {
      case _: Exception => println("Une erreur s'est produite lors du retirage d'un ingrédient.")
        return false
    }
  }
}


object Main {
  def main(args: Array[String]): Unit = {

    val filename = "machines.csv"
    println("Chargement des machines depuis " + filename + "...")
    var machines = ArrayBuffer[Machine]()
    var mode = 0
    var machineSelected = 0
    var machineid = 0
    var nbMachines = machines.length

    try {
      machines = loadcsv(filename)
      for (i <- 0 until machines.length) {
        println("Machine " + machines(i).id + " Chargée :")
        println("    ID:" + machines(i).id)
        println("    Code PIN: " + machines(i).pincode)
        var milkDouble = machines(i).milk.toDouble
        milkDouble = milkDouble / 1000
        printf("    Lait: %.3fL\n", milkDouble)
        println("    Sucre: " + machines(i).sugar + "g")
        println("    Café: " + machines(i).coffee + "g")
        println()
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")
      println()
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("\nErreur : Le fichier " + filename + " est introuvable. Vérifier le chemin d'accès et réessayez.\nFermeture du programme.")
        return
      case ex: Exception =>
        println("\nErreur : Echec du chargement des machines.\nFermeture du programme.")
        return
    }

    do {
      try {
        // ------------------ Séléction de la machine et du mode ---------------------
        println("       Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client\n2) Admin\n3) Quitter")
        mode = readLine("> ").toInt
        while (mode != 1 && mode != 2 && mode != 3) {
          try {
            println("Veuillez entrer un mode valable")
            mode = readLine("> ").toInt
          } catch {
            case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
          }
        }
        if (mode != 3) {
          println("Veuillez sélectionner une machine (1-" + nbMachines + ") :")
          machineSelected = 0
          var valid = false
          while (!valid) {
            try {
              machineSelected = readLine("> ").toInt
              if (machineSelected >= 1 && machineSelected <= nbMachines) {
                valid = true
              }
              else {
                println("Veuillez entrer une machine valable : (1-" + nbMachines + ") :")
              }
            } catch {
              case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
            }
          }
          machineid = machineSelected - 1
        }
        // -------------------- Mode Client --------------------------
        if (mode == 1) {
          serveClient(machineid, machines)
        }
        // -------------------- Mode Admin --------------------------
        else if (mode == 2) {
          if (validatePin(machineid, machines)) {
            println("1) Mettre à jour les stocks\n2) Changer le code PIN")
            var admin = readLine("> ").toInt
            while (admin != 1 && admin != 2) {
              println("Veuillez entrer un mode valide.")
              admin = readLine("> ").toInt
            }
            if (admin == 1) {
              restockMachine(machineid, machines)
            }
            else if (admin == 2) {
              updatePin(machineid, machines)
            }
          }
          else {
            mode = 3
          }
        }
      } catch {
        case _: Exception => println("Une erreur est survenue. Veuillez réessayer.\n")
      }
    } while (mode != 3)
    savecsv(filename, machines)
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var machines = ArrayBuffer[Machine]()
    val file = Source.fromFile(filename)
    val line = file.reset.getLines()
    var nextline = line.next()
    var id = 0
    var linenumber = 1
    while (!line.isEmpty) {
      nextline = line.next()
      linenumber = linenumber + 1
      val parts = nextline.split(",")
      if (parts.length == 4 && parts(0).length == 6 && parts(0).forall(_.isDigit)) {
        try {
          var pincode = parts(0)
          var milk = parts(1).toInt
          var sugar = parts(2).toInt
          var coffee = parts(3).toInt
          id += 1
          machines += new Machine(id, pincode, milk, sugar, coffee)
        } catch {
          case _: NumberFormatException => println("Les valeurs sur la ligne " + linenumber + " ne sont pas numériques. La ligne a été ignorée. (Ligne : \"" + nextline + "\")")
        }
      }
      else {
        println("La ligne " + linenumber + " est incorrecte et est donc ignorée. (Ligne : \"" + nextline + "\")")
      }
    }
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        println("\nSauvegarde de " + machines.length + " machine(s) dans machines.csv...")
        val write = new PrintWriter(filename)
        write.println("PINCODE,MILK,SUGAR,COFFEE")

        for (machine <- machines) {
          write.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
        }
        write.close()
        println("Fichier sauvegardé avec succès.")
      } catch {
        case ex: java.io.FileNotFoundException => println("Erreur : le fichier " + filename + " n'existe pas.")
        case ex: Exception => println("Une erreur est survenue lors de la sauvegarde du fichier.")
      }
    }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
      try {
        var nbEssai = 2
        var pin = "000000"
        println("Entrez le code PIN : ")
        pin = readLine("> ")
        while (machines(machineId).pincode != pin && nbEssai > 0) {
          if (nbEssai > 1) {
            println("Code PIN incorrect. " + nbEssai + " tentatives restantes.")
          }
          else {
            println("Code PIN incorrect. " + nbEssai + " tentative restante.")
          }
          pin = readLine("> ")
          nbEssai -= 1
        }
        if (nbEssai == 0) {
          println("Trop de tentatives échouée. Fin du Programme")
          return false
        }
        else {
          println("Accès accordé à la machine " + (machineId + 1) + ".")
          return true
        }
      } catch {
        case _: Exception => println("Erreur lors da la validation du code PIN")
          return false
      }
    }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
      try {
        val machineNb = machineId + 1
        var newPin = "0"
        println("Mise à jour du code PIN pour la machine " + machineNb + ".\n")
        while (newPin.length != 6 || !newPin.forall(_.isDigit)) {
          newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        }
        machines(machineId).pincode = newPin
        println("\nLe code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...\n")
        Thread.sleep(500)
      } catch {
        case _: Exception => println("Erreur lors de la mise à jour du code PIN.")
      }
    }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
      try {
        var boisson = 0
        var latteTaille = 0
        var sucre = 0
        var lait = 0
        var doseLait = 0
        var valid = false

        println("Veuillez sélectionner votre boisson :")
        println("1) Espresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte")

        while (!valid) {
          try {
            boisson = readLine("> ").toInt
            if (boisson == 1 || boisson == 2 || boisson == 3) valid = true
            else println("Veuillez entrer une boisson valable.")
          } catch {
            case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
          }
        }

        // Séléction de la taille du Latte
        if (boisson == 3) {
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          valid = false
          while (!valid) {
            try {
              latteTaille = readLine("> ").toInt
              if (latteTaille == 1 || latteTaille == 2 || latteTaille == 3) valid = true
              else println("Veuillez entrer une taille valable.")
            } catch {
              case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
            }
          }
        }

        // Sélection du sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        valid = false
        while (!valid) {
          try {
            sucre = readLine("> ").toInt
            if (sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4) valid = true
            else println("Veuillez entrer une quantité de sucre valable.")
          } catch {
            case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
          }
        }

        // Sélection du Lait
        if (boisson == 2 || boisson == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui\n2) Non")
          valid = false
          while (!valid) {
            try {
              lait = readLine("> ").toInt
              if (lait == 1 || lait == 2) valid = true
              else println("Veuillez entrer une valeur valable.")
            } catch {
              case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
            }
          }

          if (lait == 1) {
            println("Combien de dose ?")
            valid = false
            while (!valid) {
              try {
                doseLait = readLine("> ").toInt
                if (doseLait == 1 || doseLait == 2 || doseLait == 3) valid = true
                else println("Veuillez entrer une quantité de lait valable.")
              } catch {
                case _: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre.")
              }
            }
          }
        }

        // variable pour faciliter les prints (convertit les chiffres en leur nom en strings)
        // déclaration des prix
        // Manipulation des stocks des ingrédients
        var boissonName = ""
        var sucreName = ""
        var laitName = ""
        var tailleName = ""

        var boissonPrix = 0.0
        var sucrePrix = 0.0
        var laitSuppPrix = 0.0
        var total = 0.0

        // Tyype de boisson
        if (boisson == 1) {
          boissonName = "Expresso"
          boissonPrix = 2.00
          machines(machineId).removeIngredient("Coffee", 8)
        }
        else if (boisson == 2) {
          boissonName = "Cappuccino"
          boissonPrix = 2.50
          machines(machineId).removeIngredient("Coffee", 6)
          machines(machineId).removeIngredient("Milk", 8)
        }
        else boissonName = "Latte"

        // Quantité de sucre
        if (sucre == 1) sucreName = "Sans sucre"
        else if (sucre == 2) {
          sucreName = "Peu (5g)"
          machines(machineId).removeIngredient("Sugar", 5)
          sucrePrix = 0.10
        }
        else if (sucre == 3) {
          sucreName = "Moyen (10g)"
          machines(machineId).removeIngredient("Sugar", 10)
          sucrePrix = 0.20
        }
        else {
          sucreName = "Beaucoup (15g)"
          machines(machineId).removeIngredient("Sugar", 15)
          sucrePrix = 0.30
        }

        // Quantité de lait
        if (lait == 2) laitName = "Non"
        else {
          laitName = "Oui (" + doseLait + ")"
          machines(machineId).removeIngredient("Milk", doseLait * 50)
        }

        // Taille du Latte
        if (latteTaille == 1) {
          tailleName = "petit"
          boissonPrix = 2.70
          machines(machineId).removeIngredient("Coffee", 6)
          machines(machineId).removeIngredient("Milk", 120)
        }
        else if (latteTaille == 2) {
          tailleName = "moyen"
          boissonPrix = 3.20
          machines(machineId).removeIngredient("Coffee", 8)
          machines(machineId).removeIngredient("Milk", 150)
        }
        else if (latteTaille == 3) {
          tailleName = "grand"
          boissonPrix = 3.70
          machines(machineId).removeIngredient("Coffee", 12)
          machines(machineId).removeIngredient("Milk", 200)
        }

        // Résumé de la commande
        println("Machine sélectionnée : " + (machineId + 1))
        print("Boisson sélectionnée : " + boissonName)
        if (boisson == 3)
          println(" (" + tailleName + ")")
        else println()
        println("Niveau de sucre : " + sucreName)
        if (boisson == 2 || boisson == 3) {
          println("Lait en supplément : " + laitName)
        }

        // Vérification des stocks
        if (machines(machineId).sugar >= 0 && machines(machineId).coffee >= 0 && machines(machineId).milk >= 0) {
          // Paiement
          if (lait == 1) {
            laitSuppPrix = 0.05 * doseLait
            total = boissonPrix + sucrePrix + laitSuppPrix
            printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, laitSuppPrix, total)
          }
          else {
            total = boissonPrix + sucrePrix
            printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonPrix, sucrePrix, total)
          }

          println()
          println("Veuillez payer en utilisant Twint.")

          print("Votre code de paiement est : ")
          val codeChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          var index = 0
          for (_ <- 0 to 4) {
            index = (math.random * 36).toInt
            print(codeChars(index))
          }
          println()
          println("(En attente de validation du paiement...)")
          // programme attend 3 secondes
          Thread.sleep(3000)
          println()
          println("Merci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre " + boissonName + " est prêt ! Bonne Dégustation !\n")
          return true
        }
        else {
          if (machines(machineId).sugar <= 0) {
            println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine.\n")
            machines(machineId).sugar = 0
          }
          else if (machines(machineId).coffee <= 0) {
            println("\nErreur : Quantité de poucre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine.\n")
            machines(machineId).coffee = 0
          }
          else if (machines(machineId).milk <= 0 && (latteTaille == 2 || latteTaille == 3)) {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite, essayer une autre boisson ou changer de machine\n")
            machines(machineId).milk = 0
          }
          else if (machines(machineId).milk <= 0) {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson, vérifier les stocks en mode Admin ou changer de machine\n")
            machines(machineId).milk = 0
          }
          return false
        }
      } catch {
        case _: Exception => println("Une erreur est survenue lors de la transaction avec le client. Veuillez réessayer.")
          return false
      }
    }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
      try {
        println("Niveaux de stock actuels :")
        println("Poudre de café : " + machines(machineId).coffee + "g")
        println("Sucre :          " + machines(machineId).sugar + "g")
        var milk = machines(machineId).milk.toDouble
        milk = milk / 1000
        println("Lait :           " + milk + "L")
        println("\nEntrez les quantités à ajouter : ")
        var valid = false
        while (!valid) {
          try {
            var coffee = readLine("Poudre de Café > ").toInt
            if (coffee >= 0) {
              machines(machineId).addIngredient("Coffee", coffee)
              valid = true
            }
            else {
              println("Veuillez entrer un nombre positif.")
            }
          } catch {
            case ex: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre entier.")
          }
        }
        valid = false
        while (!valid) {
          try {
            var sugar = readLine("Sucre > ").toInt
            if (sugar >= 0) {
              machines(machineId).addIngredient("Sugar", sugar)
              valid = true
            }
            else {
              println("Veuillez entrer un nombre positif.")
            }
          } catch {
            case ex: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre entier.")
          }
        }
        valid = false
        while (!valid) {
          try {
            var milk = readLine("Lait > ").toInt
            if (milk >= 0) {
              machines(machineId).addIngredient("Milk", milk)
              valid = true
            }
            else {
              println("Veuillez entrer un nombre positif.")
            }
          } catch {
            case ex: NumberFormatException => println("Entrée invalide. Veuillez entrer un nombre entier.")
          }
        }

        println("\nLes stock ont été mis à jour avec succés.\n")
        println("Nouveaux Niveaux de stock :")
        println("Poudre de café : " + machines(machineId).coffee + "g")
        println("Sucre :          " + machines(machineId).sugar + "g")
        milk = machines(machineId).milk.toDouble
        milk = milk / 1000
        println("Lait :           " + milk + "L\n")
        println("Retour au menu principal...\n")
        Thread.sleep(500)
      } catch {
        case _: Exception => println("Erreur lors de la mise à jour des stocks. Veuillez réessayer.")
      }
    }
}