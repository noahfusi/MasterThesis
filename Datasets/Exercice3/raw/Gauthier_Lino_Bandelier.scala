import io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.{Try, Using}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (amount < 0) {
      println("Quantité invalide. Elle doit être positive.")
      return
    }
    if (ingredient.toLowerCase == "milk") milk += amount
    else if (ingredient.toLowerCase == "sugar") sugar += amount
    else if (ingredient.toLowerCase == "coffee") coffee += amount
    else println("Ingrédient inconnu.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (amount < 0) {
      println("Quantité invalide. Elle doit être positive.")
      return false
    }
    if (ingredient.toLowerCase == "milk" && milk >= amount) {
      milk -= amount; true
    } else if (ingredient.toLowerCase == "sugar" && sugar >= amount) {
      sugar -= amount; true
    } else if (ingredient.toLowerCase == "coffee" && coffee >= amount) {
      coffee -= amount; true
    } else {
      println("Stock insuffisant pour " + ingredient + ".")
      false
    }
  }

  override def toString: String = {
    "ID: " + id + " | PIN: " + pincode + " | Lait: " + (milk / 1000.0) + " L | Sucre: " + sugar + " g | Café: " + coffee + " g"
  }
}

object Main {
  val filename = "machines.csv"
  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    try {

      println("Chargement des machines depuis " + filename + "...")


      Using(Source.fromFile(filename)) { source =>
        val lines = source.getLines().drop(1)
        for ((line, index) <- lines.zipWithIndex) {
          val values = line.split(",")
          val pincode = values(0).filterNot(_.isWhitespace)
          val milk = values(1).filterNot(_.isWhitespace).toInt
          val sugar = values(2).filterNot(_.isWhitespace).toInt
          val coffee = values(3).filterNot(_.isWhitespace).toInt

          val machine = new Machine(index + 1, pincode, milk, sugar, coffee)
          buffer += machine


          println("Machine " + machine.id + " chargée :")
          println(machine)
        }
      }
      if (buffer.isEmpty) {
        println("Aucune machine trouvée dans le fichier.")
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        sys.exit(1)
      case e: java.io.IOException =>
        println("Erreur : Impossible de lire le fichier " + filename + ". Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println("Erreur : Échec du chargement du fichier " + filename + ".")
        e.printStackTrace()
        sys.exit(1)
    }

    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println("Sauvegarde des machines dans " + filename + "...")
      Using(new PrintWriter(new File(filename))) { writer =>
        writer.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
        }
      }
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans " + filename + ". Le fichier peut être verrouillé ou en lecture seule.")
      case e: Exception =>
        println("Erreur : " + e.getMessage)
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN :")
      val inputPin = readLine()
      if (inputPin == machine.pincode) {
        println("Code PIN valide. Accès autorisé.")
        return true
      }
      tentatives -= 1
      println("Code PIN incorrect. Tentatives restantes : " + tentatives)
    }
    println("Nombre de tentatives dépassé.")
    false
  }

  def restockMachine(machine: Machine): Unit = {
    println(machine)
    println("Ajoutez des quantités :")
    println("Lait (L) avec un . :")
    val milk = readLine().toDouble
    println("Sucre (g) :")
    val sugar = readLine().toInt
    println("Café (g) :")
    val coffee = readLine().toInt
    machine.addIngredient("milk", (milk * 1000).toInt)
    machine.addIngredient("sugar", sugar)
    machine.addIngredient("coffee", coffee)
    println("Réapprovisionnement effectué avec succès.")
  }

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN (6 chiffres) :")
    var newPin = ""
    do {
      newPin = readLine()
      if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
        println("Code PIN invalide. Assurez-vous d'entrer exactement 6 chiffres.")
      }
    } while (newPin.length != 6 || !newPin.forall(_.isDigit))
    machine.pincode = newPin
    println("Code PIN mis à jour avec succès.")
  }

  def serviceClient(machine: Machine): Boolean = {
    println("Sélectionnez une boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - Petit, Moyen, Grand")
    var choixboisson = readLine().toInt

    while (!(choixboisson >= 1 && choixboisson <= 3)) {
      println("Choix invalide. Veuillez sélectionner une boisson parmi les options disponibles.")
      choixboisson = readLine().toInt
    }

    var cafeNess = 0
    var laitNess = 0
    var prixBase = 0.0
    var nomBoisson = ""

    if (choixboisson == 1) {
      cafeNess = 8
      prixBase = 2.00
      nomBoisson = "Expresso"
    } else if (choixboisson == 2) {
      cafeNess = 6
      laitNess = 100
      prixBase = 2.50
      nomBoisson = "Cappuccino"
    } else {
      println("Choisissez une taille :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
      var choixTaille = readLine().toInt
      while (!(choixTaille >= 1 && choixTaille <= 3)) {
        println("Choix invalide. Sélectionnez une taille disponible.")
        choixTaille = readLine().toInt
      }
      if (choixTaille == 1) {
        cafeNess = 6
        laitNess = 120
        prixBase = 2.70
        nomBoisson = "Latte Petit"
      } else if (choixTaille == 2) {
        cafeNess = 8
        laitNess = 150
        prixBase = 3.20
        nomBoisson = "Latte Moyen"
      } else {
        cafeNess = 12
        laitNess = 200
        prixBase = 3.70
        nomBoisson = "Latte Grand"
      }
    }

    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    val choixSucre = readLine().toInt
    var sucreNess = 0
    var prixSucre = 0.0
    var niveauSucre = ""

    if (choixSucre == 1) {
      sucreNess = 0
      prixSucre = 0.0
      niveauSucre = "sans sucre"
    } else if (choixSucre == 2) {
      sucreNess = 5
      prixSucre = 0.10
      niveauSucre = "Peu (5g)"
    } else if (choixSucre == 3) {
      sucreNess = 10
      prixSucre = 0.20
      niveauSucre = "Moyen (10g)"
    } else if (choixSucre == 4) {
      sucreNess = 15
      prixSucre = 0.30
      niveauSucre = "Beaucoup (15g)"
    }

    var laitSupp = 0
    if (choixboisson == 2 || choixboisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? (1: Oui, 2: Non)")
      laitSupp = readLine().toInt
      while (!(laitSupp == 1 || laitSupp == 2)) {
        println("Choix invalide. Veuillez sélectionner une option valide (1 ou 2).")
        laitSupp = readLine().toInt
      }
    }

    var prixLaitsupp = 0.0
    if (laitSupp == 1) {
      println("Combien de doses de lait ? (max 3)")
      var doselait = readLine().toInt
      while (doselait < 1 || doselait > 3) {
        println("Nombre de doses invalide. Veuillez choisir entre 1 et 3.")
        doselait = readLine().toInt
      }
      prixLaitsupp = doselait * 0.05
      laitNess += doselait * 50
    }


    if (!machine.removeIngredient("coffee", cafeNess) ||
      !machine.removeIngredient("milk", laitNess) ||
      !machine.removeIngredient("sugar", sucreNess)) {
      println("Stocks insuffisants pour préparer votre commande. Veuillez choisir une autre machine.")
      return false
    }

    val prixTotal = prixBase + prixSucre + prixLaitsupp
    val codePaiement = Random.alphanumeric.take(5).mkString

    println("Boisson sélectionnée : " + nomBoisson)
    println("Niveau de sucre : " + niveauSucre)
    printf("Prix total : CHF %.2f\n", prixTotal)
    println("Veuillez payer en utilisant Twint. Votre code de paiement est : " + codePaiement)
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("Paiement confirmé.")
    println("Préparation de votre " + nomBoisson + "...")
    Thread.sleep(5000)
    println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

    true
  }

  def main(args: Array[String]): Unit = {
    machines = loadcsv(filename)


    if (machines.isEmpty) {
      println("Aucune machine disponible. Le programme va se fermer.")
      sys.exit(1)
    }

    var continuer = true
    while (continuer) {
      println("\nNospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = readLine().toInt
      if (choix == 3) {
        println("Sauvegarde des données...")
        savecsv(filename, machines)
        continuer = false
      } else if (choix == 1 || choix == 2) {
        println("Sélectionnez une machine (ID) :")
        var machineId = -1
        var machineOption: Option[Machine] = None

        do {
          machineId = readLine().toInt
          machineOption = machines.find(_.id == machineId)
          if (machineOption.isEmpty) {
            println("Machine invalide. Veuillez saisir un numéro de machine valide (1-5) :")
          }
        } while (machineOption.isEmpty)

        val machine = machineOption.get
        println("Machine sélectionnée : " + machine)

        if (choix == 1) {
          serviceClient(machine)
        } else if (validatePin(machine)) {
          println("1) Réapprovisionner\n2) Modifier le code PIN")
          val adminChoice = readLine().toInt
          if (adminChoice == 1) restockMachine(machine)
          else if (adminChoice == 2) updatePin(machine)
        }
      } else {
        println("Choix invalide.")
      }
    }
  }
}




