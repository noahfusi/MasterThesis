import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io._


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient inconnu : $ingredient")
    }
  }

  // retirer ingrédient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println(s"Quantité insuffisante ou ingrédient inconnu : $ingredient"); false
    }
  }

  override def toString: String = s"Machine(id=$id, pincode=$pincode, milk=$milk, sugar=$sugar, coffee=$coffee)"
}

// méthode pour gérer les fichiers csv
object MachineManager {

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val machines = ArrayBuffer[Machine]()
      val lines = Source.fromFile(filename).getLines()


      lines.next()

      for ((line, index) <- lines.zipWithIndex) {
        val parts = line.split(",")
        if (parts.length == 4) {
          val pincode = parts(0)
          val milk = parts(1).toInt
          val sugar = parts(2).toInt
          val coffee = parts(3).toInt
          machines += new Machine(index + 1, pincode, milk, sugar, coffee)
        }
      }
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
      machines
    } catch {
      case _: FileNotFoundException =>
        println(s"Erreur : Fichier $filename introuvable. Vérifiez le chemin d'accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        System.exit(0)
        ArrayBuffer.empty[Machine]
      case ex: Exception =>
        println(s"Erreur inattendue lors du chargement du fichier $filename : ${ex.getMessage}")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        System.exit(0)
        ArrayBuffer.empty[Machine]
    }
  }

  // sauvegarde machines dans fichier csv
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))

      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
      for (machine <- machines) {
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      writer.close()
    } catch {
      case _: IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        System.exit(0)
    }
  }
}


  // programme principal
object Main {
  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")


    val machines = MachineManager.loadcsv("machines.csv")

    // afficher les infos de chaque machine
    machines.foreach { machine =>
      println(s"Machine ${machine.id} chargée :")
      println(s"  ID : ${machine.id}")
      println(s"  Code PIN : ${machine.pincode}")
      println(f"  Lait : ${machine.milk / 1000.0}%.3fL")
      println(s"  Sucre : ${machine.sugar}g")
      println(s"  Café : ${machine.coffee}g")
      println()
    }

    println(s"${machines.length} machine(s) chargée(s) avec succès.\n")

    // menu princip
    var continuer = true
    while (continuer) {
      var mode = 0
      while (mode != 1 && mode != 2 && mode != 3) {
        println("        Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine("> ").toIntOption.getOrElse(0)
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Option invalide. Veuillez réessayer.")
        }
      }

      mode match {
        case 1 => serveClient(machines)
        case 2 => adminMode(machines)
        case 3 =>
          println(s"Sauvegarde de ${machines.size} machine(s) dans machines.csv...")
          MachineManager.savecsv("machines.csv", machines)
          println("Fichier sauvegardé avec succès.")
          println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
          continuer = false
      }
    }
    println("Programme terminé.")
  }

  def serveClient(machines: ArrayBuffer[Machine]): Unit = {
    println("Mode Client :")
    var machineId = -1
    while (machineId < 0 || machineId >= machines.length) {
      println(s"Choisissez une machine (1 à ${machines.length}) :")
      machineId = readLine("> ").toIntOption.getOrElse(0) - 1
      if (machineId < 0 || machineId >= machines.length) {
        println("Machine invalide. Veuillez réessayer.")
      }
    }

    val machine = machines(machineId)
    println(s"Vous utilisez la machine ${machine.id}.")

    var cafeNecessaire = 0
    var laitNecessaire = 0
    var sucreNecessaire = 0
    var prixBase = 0.0
    var prixSucre = 0.0
    var prixLaitSupp = 0.0
    var prixTotal = 0.0
    var niveauSucre = "Sans sucre"
    var laitSupplementaire = "Non"


    var boisson = 0
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      boisson = readLine("> ").toIntOption.getOrElse(0)
    }

    if (boisson == 1) {
      cafeNecessaire = 8
      prixBase = 2.00
    } else if (boisson == 2) {
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.50
    } else if (boisson == 3) {
      var taille = 0
      while (taille != 1 && taille != 2 && taille != 3) {
        println("Veuillez choisir la taille de votre Latte :")
        println("(1) Petit")
        println("(2) Moyen")
        println("(3) Grand")
        taille = readLine("> ").toIntOption.getOrElse(0)
      }

      if (taille == 1) {
        cafeNecessaire = 6
        laitNecessaire = 120
        prixBase = 2.70
      } else if (taille == 2) {
        cafeNecessaire = 8
        laitNecessaire = 150
        prixBase = 3.20
      } else if (taille == 3) {
        cafeNecessaire = 12
        laitNecessaire = 200
        prixBase = 3.70
      }
    }

    // vérifié stocks
    if (machine.coffee < cafeNecessaire) {
      println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
      return
    }
    if (machine.milk < laitNecessaire) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      return
    }


    var sucre = 0
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("(1) Sans sucre")
      println("(2) Peu (5g)")
      println("(3) Moyen (10g)")
      println("(4) Beaucoup (15g)")
      sucre = readLine("> ").toIntOption.getOrElse(0)
    }

    if (sucre == 1) {
      niveauSucre = "Sans sucre"
      prixSucre = 0.0
      sucreNecessaire = 0
    } else if (sucre == 2) {
      niveauSucre = "Peu (5g)"
      prixSucre = 0.10
      sucreNecessaire = 5
    } else if (sucre == 3) {
      niveauSucre = "Moyen (10g)"
      prixSucre = 0.20
      sucreNecessaire = 10
    } else if (sucre == 4) {
      niveauSucre = "Beaucoup (15g)"
      prixSucre = 0.30
      sucreNecessaire = 15
    }

    if (machine.sugar < sucreNecessaire) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      return
    }

    // lait supp
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      var choixLait = 0
      while (choixLait != 1 && choixLait != 2) {
        choixLait = readLine("> ").toIntOption.getOrElse(0)
      }

      if (choixLait == 1) {
        var dosesLait = 0
        while (dosesLait < 1 || dosesLait > 3) {
          println("Combien de dose ? (1 à 3 doses)")
          dosesLait = readLine("> ").toIntOption.getOrElse(0)
        }

        if (machine.milk >= dosesLait * 50) {
          laitSupplementaire = s"$dosesLait dose(s)"
          prixLaitSupp = dosesLait * 0.05
          laitNecessaire += dosesLait * 50
        } else {
          println("Erreur : Quantité de lait insuffisante pour le supplément.")
        }
      }
    }

    // mise à jour stocks
    machine.removeIngredient("coffee", cafeNecessaire)
    machine.removeIngredient("milk", laitNecessaire)
    machine.removeIngredient("sugar", sucreNecessaire)


    prixTotal = prixBase + prixSucre + prixLaitSupp


    println("Boisson sélectionnée : " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else s"Latte (${if (laitNecessaire == 120) "Petit" else if (laitNecessaire == 150) "Moyen" else "Grand"})"))
    println("Niveau de sucre : " + niveauSucre)
    println("Lait supplémentaire : " + laitSupplementaire)
    println("Prix total : CHF " + "%.2f".format(prixBase) + " + CHF " + "%.2f".format(prixSucre) + " + CHF " + "%.2f".format(prixLaitSupp) + " = CHF " + "%.2f".format(prixTotal))
    println("\nVeuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + generateCodeTwint())
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("\nPaiement confirmé.")
    println("Préparation de votre boisson...")
    println("Votre " + (if (boisson == 1) "Expresso" else if (boisson == 2) "Cappuccino" else "Latte") + " est prêt ! Bonne dégustation !")
  }

  def generateCodeTwint(): String = {
    val caracteres = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to 5).map(_ => caracteres((Math.random() * caracteres.length).toInt)).mkString
  }


  def adminMode(machines: ArrayBuffer[Machine]): Unit = {
    println("Mode Admin :")
    var machineId = -1
    while (machineId < 0 || machineId >= machines.length) {
      println(s"Choisissez une machine (1 à ${machines.length}) :")
      machineId = readLine("> ").toIntOption.getOrElse(0) - 1
      if (machineId < 0 || machineId >= machines.length) {
        println("Machine invalide. Veuillez réessayer.")
      }
    }

    val machine = machines(machineId)
    println(s"Machine ${machine.id}: Entrez le code PIN :")
    var validPin = false
    var attempts = 3

    while (attempts > 0 && !validPin) {
      val pin = readLine("> ")
      if (pin == machine.pincode) {
        validPin = true
        println(s"Accès accordé à la machine ${machine.id}.")
      } else {
        attempts -= 1
        if (attempts > 0) {
          println(s"Code PIN incorrect. $attempts tentative(s) restante(s).")
        } else {
          println("Trop de tentatives échouées. Retour au menu principal.")
          return
        }
      }
    }

    if (validPin) {
      var action = 0
      while (action != 1 && action != 2) {
        println("Que souhaitez-vous faire ?")
        println("1) Réapprovisionner les stocks")
        println("2) Modifier le code PIN")
        action = readLine("> ").toIntOption.getOrElse(0)
      }

      action match {
        case 1 =>
          println("Réapprovisionnement des stocks :")
          println(s"Poudre de café : ${machine.coffee}g")
          println(s"Sucre : ${machine.sugar}g")
          println(f"Lait : ${machine.milk / 1000.0}%.1fL")

          println("Entrez les quantités à ajouter :")
          print("Poudre de café > ")
          machine.addIngredient("coffee", readLine().toInt)

          print("Sucre > ")
          machine.addIngredient("sugar", readLine().toInt)

          print("Lait (en litres) > ")
          val laitAjoute = readLine().replace(",", ".").toDouble
          machine.addIngredient("milk", (laitAjoute * 1000).toInt)

          println("Les stocks ont été mis à jour avec succès.")
        case 2 =>
          println("Mise à jour du code PIN.")
          var newPin = ""
          do {
            println("Entrez un nouveau code PIN à 6 chiffres :")
            newPin = readLine()
          } while (newPin.length != 6 || !newPin.forall(_.isDigit))

          machine.pincode = newPin
          println("Le code PIN a été mis à jour avec succès.")
      }
    }
  }
}

