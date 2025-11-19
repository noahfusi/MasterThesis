import scala.io.{Source, StdIn}
import java.io.{PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer

// Classe Machine pour créer les machines Nospresso
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  // Ajouter des ingrédients
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient inconnu.")
    }
  }

  // Retirer des ingrédients
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println(s"Stock insuffisant pour $ingredient."); false
    }
  }
  //affichage des machines
  def affiche(): Unit = {
    println(s"Machine ID: $id")
    println(s"PIN Code: $pincode")
    println(s"Milk: ${milk / 1000.0} L, Sugar: $sugar g, Coffee: $coffee g")
    println("-----------------------------")
  }

  def saveCSV: String = s"$pincode,$milk,$sugar,$coffee"
}

//objet Main de l'exercice
object NospressoMachine {
  val machines = ArrayBuffer[Machine]()

  // Définir les prix des boissons
  val PrixExpresso = 2.00
  val PrixCappuccino = 2.50
  val PrixLattePetit = 2.70
  val PrixLatteMoyen = 3.20
  val PrixLatteGrand = 3.70

  // Charger les machines depuis un fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val loadedMachines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile("src/" + filename)
      var index = 0
      for (line <- source.getLines().drop(1)) {
        val Array(pin, milk, sugar, coffee) = line.split(",")
        loadedMachines += new Machine(index + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
        index += 1
      }
      source.close()
      println(s"${loadedMachines.size} machine(s) chargée(s) depuis $filename.")
    } catch {
      case _: Exception => println("Erreur : Fichier introuvable ou inaccessible.")
    }
    loadedMachines
  }

  // Sauver les machines dans un fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter("src/" + filename, false))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.saveCSV)
      }
      writer.close()
      println(s"Sauvegarde de ${machines.size} machine(s) dans $filename.")
    } catch {
      case _: Exception => println("Erreur : Échec de la sauvegarde des machines.")
    }
  }

  // Menu principal
  def mainMenu(): Unit = {
    var running = true
    while (running) {
      println("\nNospresso Café\nVeuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      StdIn.readLine("Votre choix : ") match {
        case "1" => clientMenu()
        case "2" => adminMenu()
        case "3" =>
          savecsv("machines.csv", machines)
          running = false
          println("Merci d'avoir utilisé Nospresso. À bientôt !")
        case _ => println("Choix invalide. Réessayez.")
      }
    }
  }

  // Mode client
  def clientMenu(): Unit = {
    println("Mode Client : Sélectionnez une machine.")
    val machine = selectMachine()
    if (machine.isDefined) {
      var continueCommande = true
      while (continueCommande) {
        serveClient(machine.get)
        println("Voulez-vous commander une autre boisson ? (1: Oui, 2: Non)")
        continueCommande = StdIn.readLine("Votre choix : ") == "1"
      }
    }
  }

  // méthode serveClient suite de exercice 2
  def serveClient(machine: Machine): Unit = {
    println("\nChoisissez une boisson :")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - (Petit, Moyen, Grand)")

    val choice = StdIn.readLine("Votre choix : ").toInt
    var prixBoisson = 0.0
    val (coffeeNeeded, milkNeeded) = choice match {
      case 1 =>
        prixBoisson = PrixExpresso
        (50, 0)
      case 2 =>
        prixBoisson = PrixCappuccino
        (50, 100)
      case 3 =>
        println("Quelle taille souhaitez-vous ?")
        println("1) Petit - 2.70 CHF")
        println("2) Moyen - 3.20 CHF")
        println("3) Grand - 3.70 CHF")
        StdIn.readLine("Votre choix : ").toInt match {
          case 1 => prixBoisson = PrixLattePetit; (75, 150)
          case 2 => prixBoisson = PrixLatteMoyen; (100, 200)
          case 3 => prixBoisson = PrixLatteGrand; (125, 250)
          case _ =>
            println("Choix invalide."); return
        }
      case _ =>
        println("Choix invalide."); return
    }

    // Ajouter du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10 CHF")
    println("3) Moyen (10g) - 0.20 CHF")
    println("4) Beaucoup (15g) - 0.30 CHF")
    val choixSucre = StdIn.readLine("Votre choix : ").toInt
    val sucreSupplementaire = choixSucre match {
      case 1 => 0
      case 2 => prixBoisson += 0.10; 5
      case 3 => prixBoisson += 0.20; 10
      case 4 => prixBoisson += 0.30; 15
      case _ =>
        println("Choix invalide."); return
    }

    // Ajouter du lait supplémentaire
    println("Voulez-vous ajouter du lait supplémentaire ? (1: Oui, 2: Non)")
    val laitSupplementaire = if (StdIn.readLine("Votre choix : ").toInt == 1) {
      println("Combien de doses (0 à 3) ?")
      val doses = StdIn.readLine("Votre choix : ").toInt.max(0).min(3)
      prixBoisson += doses * 0.05
      doses * 50
    } else 0

    // Vérification des stocks et préparation
    if (
      machine.removeIngredient("coffee", coffeeNeeded) &&
        machine.removeIngredient("milk", milkNeeded + laitSupplementaire) &&
        machine.removeIngredient("sugar", sucreSupplementaire)
    ) {
      // Génération du code de paiement
      val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var codePaiement = ""
      for (_ <- 1 to 5) {
        val randomIndex = (math.random() * caracteres.length).toInt
        codePaiement += caracteres(randomIndex)
      }

      println("Votre commande est prête !")
      println("Boisson : " + (if (choice == 1) "Expresso" else if (choice == 2) "Cappuccino" else "Latte"))
      println(f"Prix total : CHF $prixBoisson%.2f")
      println("Sucre : " + (if (choixSucre == 1) "Sans sucre" else if (choixSucre == 2) "Peu" else if (choixSucre == 3) "Moyen" else "Beaucoup"))
      println("Code de paiement : " + codePaiement)
      println("Paiement confirmé. Préparation de la boisson...")
      println("Votre boisson est prête ! Bonne dégustation !")
    } else {
      println("Stock insuffisant pour cette commande.")
    }
  }

  // Préparer une boisson
  def prepareDrink(machine: Machine, ingredient: String, amount: Int): Boolean = {
    if (machine.removeIngredient(ingredient, amount)) {
      println(s"Votre $ingredient est prêt !")
      true
    } else {
      println("Commande échouée. Stock insuffisant.")
      false
    }
  }

  // Mode administrateur
  def adminMenu(): Unit = {
    var adminRunning = true
    while (adminRunning) {
      println("Mode Administrateur :")
      println("1) Afficher les machines")
      println("2) Réapprovisionner une machine")
      println("3) Modifier le PIN d'une machine")
      println("4) Retour au menu principal")
      StdIn.readLine("Votre choix : ") match {
        case "1" => displayMachines()
        case "2" => restockMachine()
        case "3" => updatePin()
        case "4" => adminRunning = false
        case _ => println("Choix invalide.")
      }
    }
  }

  // Afficher les machines
  def displayMachines(): Unit = {
    for (machine <- machines) machine.affiche()
  }

  // Réapprovisionner une machine
  def restockMachine(): Unit = {
    val machine = selectMachine()
    if (machine.isDefined) {
      val milk = StdIn.readLine("Quantité de lait à ajouter (mL) : ").toInt
      val sugar = StdIn.readLine("Quantité de sucre à ajouter (g) : ").toInt
      val coffee = StdIn.readLine("Quantité de café à ajouter (g) : ").toInt
      machine.get.addIngredient("milk", milk)
      machine.get.addIngredient("sugar", sugar)
      machine.get.addIngredient("coffee", coffee)
      println("Réapprovisionnement effectué avec succès.")
      savecsv("machines.csv", machines)
    }
  }

  // Modifier le PIN d'une machine
  def updatePin(): Unit = {
    val machine = selectMachine()
    if (machine.isDefined) {
      val newPin = StdIn.readLine("Entrez le nouveau PIN (6 chiffres) : ")
      machine.get.pincode = newPin
      println("PIN mis à jour avec succès.")
      savecsv("machines.csv", machines)
    }
  }

  // Sélectionner une machine par ID
  def selectMachine(): Option[Machine] = {
    val id = StdIn.readLine("Entrez l'ID de la machine : ").toInt
    machines.find(_.id == id)
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    machines ++= loadcsv("machines.csv")
    mainMenu()
  }
}
