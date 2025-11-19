import scala.io.Source
import scala.io.StdIn._
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient inconnu.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => false
    }
  }
}

object Main {
  val filename = "machines.csv"
  var machines = ArrayBuffer[Machine]()

  def main(args: Array[String]): Unit = {
    try {
      machines = loadcsv(filename)

      println("\n \n--- Informations sur les machines ---")
      machines.foreach { machine =>
        println(f"\nMachine ID: ${machine.id}%d")
        println(f"   Code PIN: ${machine.pincode}%s")
        println(f"   Lait: ${machine.milk / 1000.0}%.2f L")
        println(f"   Sucre: ${machine.sugar}%d g")
        println(f"   Café: ${machine.coffee}%d g")

      }

      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur lors du chargement des machines : ${e.getMessage}")
        return
    }

    var programmeEnCours = true

    while (programmeEnCours) {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val modechoisi = readValidChoice(List(1, 2, 3))

      modechoisi match {
        case 1 => clientMode()
        case 2 =>
          val accessGranted = adminMode()
          if (!accessGranted) {
            println("Trop de tentatives échouées. Fermeture du programme.")
            programmeEnCours = false
          }
        case 3 =>
          println("Sauvegarde des machines en cours...")
          try {
            savecsv(filename, machines)
            println("Fichier sauvegardé avec succès.")
          } catch {
            case e: Exception => println(s"Erreur lors de la sauvegarde : ${e.getMessage}")
          }
          programmeEnCours = false
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    val file = Source.fromFile(filename)
    val lines = file.getLines().drop(1) // Ignore l'en-tête
    for ((line, index) <- lines.zipWithIndex) {
      val cols = line.split(",").map(_.trim)
      buffer += new Machine(index + 1, cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt)
    }
    file.close()
    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val writer = new PrintWriter(new File(filename))
    writer.println("PINCODE,MILK,SUGAR,COFFEE")
    machines.foreach { m =>
      writer.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
    }
    writer.close()
  }

  def clientMode(): Unit = {
    var clientSession = true
    while (clientSession) {
      println("Sélectionnez une machine (ID) :")
      machines.foreach(m => println(s"Machine ${m.id}"))
      print("> ")
      val machineId = readValidChoice(machines.map(_.id).toList)
      val machine = machines(machineId - 1)

      val caract = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var twint = ""
      for (_ <- 1 to 5) {
        val chxcaract = (Math.random() * 36).toInt
        twint += caract(chxcaract)
      }

      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Capuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      val boisson = readValidChoice(List(1, 2, 3))

      var prixtotal = 0.0
      var coffeeNeeded = 0
      var milkNeeded = 0
      var boissonNom = ""
      var sucreNiveau = "Sans sucre"
      var laitSupplementaire = "Non"

      if (boisson == 1) {
        prixtotal = 2.00
        coffeeNeeded = 8
        milkNeeded = 0
        boissonNom = "Expresso"
      } else if (boisson == 2) {
        prixtotal = 2.50
        coffeeNeeded = 6
        milkNeeded = 100
        boissonNom = "Capuccino"
      } else if (boisson == 3) {
        println("Dimension du Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")
        val dimensionlatte = readValidChoice(List(1, 2, 3))
        if (dimensionlatte == 1) {
          prixtotal = 2.70
          coffeeNeeded = 6
          milkNeeded = 120
          boissonNom = "Latte (Petit)"
        } else if (dimensionlatte == 2) {
          prixtotal = 3.20
          coffeeNeeded = 8
          milkNeeded = 150
          boissonNom = "Latte (Moyen)"
        } else if (dimensionlatte == 3) {
          prixtotal = 3.70
          coffeeNeeded = 12
          milkNeeded = 200
          boissonNom = "Latte (Grand)"
        }
      }

      if (!machine.removeIngredient("coffee", coffeeNeeded)) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez sélectionner une autre machine.")
        clientSession = true // Revenir au choix des machines
      } else {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        val sucre = readValidChoice(List(1, 2, 3, 4))
        val prixSucre = if (sucre == 1) 0.00 else if (sucre == 2) 0.10 else if (sucre == 3) 0.20 else 0.30

        if (sucre == 2) sucreNiveau = "Peu (5g)"
        else if (sucre == 3) sucreNiveau = "Moyen (10g)"
        else if (sucre == 4) sucreNiveau = "Beaucoup (15g)"

        if (!machine.removeIngredient("sugar", if (sucre == 1) 0 else if (sucre == 2) 5 else if (sucre == 3) 10 else 15)) {
          println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez sélectionner une autre machine.")
          clientSession = true // Revenir au choix des machines
        } else {
          var extraMilkPrice = 0.0
          var extraMilkAmount = 0
          if (boisson == 2 || boisson == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print("> ")
            val ajoutLait = readValidChoice(List(1, 2))
            if (ajoutLait == 1) {
              println("Combien de doses de lait supplémentaire (1 à 3 doses, 50ml par dose, CHF 0.05 par dose) ?")
              print("> ")
              val doses = readValidChoice(List(1, 2, 3))
              extraMilkPrice = doses * 0.05
              extraMilkAmount = doses * 50
              laitSupplementaire = "Oui"
            }
          }

          if (!machine.removeIngredient("milk", milkNeeded + extraMilkAmount)) {
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez sélectionner une autre machine.")
            clientSession = true // Revenir au choix des machines
          } else {
            prixtotal += prixSucre + extraMilkPrice

            println(s"Boisson sélectionnée : $boissonNom")
            println(s"Niveau de sucre : $sucreNiveau")
            println(s"Lait supplémentaire : $laitSupplementaire")

            println(f"Prix total : CHF ${prixtotal - prixSucre - extraMilkPrice}%.2f + CHF $prixSucre%.2f (sucre) + CHF $extraMilkPrice%.2f (lait supplémentaire) = CHF $prixtotal%.2f")
            println(s"Votre code de paiement Twint est : $twint")
            println("Validation du paiement...")
            Thread.sleep(3000)
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            Thread.sleep(3000)
            println("Votre boisson est prête ! Bonne dégustation !")

            clientSession = false // Terminer la session après une commande réussie
          }
        }
      }
    }
  }

  def adminMode(): Boolean = {
    println("Sélectionnez une machine (ID) :")
    machines.foreach(m => println(s"Machine ${m.id}"))
    print("> ")
    val machineId = readValidChoice(machines.map(_.id).toList)
    val machine = machines(machineId - 1)

    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN :")
      print("> ")
      val pin = readLine().trim
      if (pin == machine.pincode) {
        println("Accès autorisé. Options :")
        println("1) Réapprovisionnement")
        println("2) Mise à jour du PIN")
        print("> ")
        val adminChoice = readValidChoice(List(1, 2))
        adminChoice match {
          case 1 =>
            println(s"Stocks actuels pour la machine ${machine.id} :")
            println(f"Lait : ${machine.milk / 1000.0}%.2f L")
            println(f"Sucre : ${machine.sugar} g")
            println(f"Café : ${machine.coffee} g")

            println("Entrez la quantité de lait à ajouter (en mL, peut être négative pour déduire) :")
            print("> ")
            machine.addIngredient("milk", readLine().toInt)
            println("Entrez la quantité de sucre à ajouter (peut être négative pour déduire) :")
            print("> ")
            machine.addIngredient("sugar", readLine().toInt)
            println("Entrez la quantité de café à ajouter (peut être négative pour déduire) :")
            print("> ")
            machine.addIngredient("coffee", readLine().toInt)

            println("Réapprovisionnement terminé.")
          case 2 =>
            var validPin = false
            while (!validPin) {
              println("Entrez un nouveau code PIN à 6 chiffres :")
              print("> ")
              val newPin = readLine().trim
              if (newPin.matches("\\d{6}")) {
                machine.pincode = newPin
                println("PIN mis à jour avec succès.")
                validPin = true
              } else {
                println("Code PIN invalide. Veuillez réessayer.")
              }
            }
        }
        return true
      } else {
        attempts -= 1
        if (attempts > 0) println(s"Code PIN incorrect. Tentatives restantes : $attempts.")
      }
    }

    false
  }

  def readValidChoice(validChoices: List[Int]): Int = {
    var choice = -1
    while (!validChoices.contains(choice)) {
      print("> ")
      choice = readLine().toIntOption.getOrElse(-1)
    }
    choice
  }
}
