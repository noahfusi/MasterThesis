import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.PrintWriter
import java.io.FileWriter
import scala.util.Random

// Classe représentant une machine Nospresso
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") milk += amount
    else if (ingredient == "sugar") sugar += amount
    else if (ingredient == "coffee") coffee += amount
    else println("Ingrédient non reconnu.")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk" && milk >= amount) { milk -= amount; true }
    else if (ingredient == "sugar" && sugar >= amount) { sugar -= amount; true }
    else if (ingredient == "coffee" && coffee >= amount) { coffee -= amount; true }
    else {
      println("Stock insuffisant ou ingrédient non reconnu.")
      false
    }
  }

  def display(): Unit = {
    println("ID: " + id + " | PIN: " + pincode + " | Lait: " + milk / 1000.0 + "L | Sucre: " + sugar + "g | Café: " + coffee + "g")
  }
}

// Gestion des fichiers CSV
object CSV {
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines()
      if (!lines.isEmpty) lines.next() // Ignorer la ligne d'en-tête

      var id = 1
      for (line <- lines) {
        val values = line.split(",")
        val pincode = values(0)
        val milk = values(1).toInt
        val sugar = values(2).toInt
        val coffee = values(3).toInt
        val machine = new Machine(id, pincode, milk, sugar, coffee)
        machines += machine
        println("Machine " + id + " chargée :")
        machine.display()
        id += 1
      }
      source.close()
      println(machines.length + " machine(s) chargée(s) avec succès.")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        return machines
      case _: java.io.IOException =>
        println("Erreur : Échec du chargement. Vérifiez le fichier.")
        return machines
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      writer.close()
      println("Données sauvegardées avec succès dans " + filename)
    } catch {
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule.")
    }
  }
}

// Programme principal
object Main {
  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = CSV.loadcsv(filename)

    if (machines.isEmpty) {
      println("Erreur : Aucun fichier machine valide chargé. Fin du programme.")
      return
    }

    var running = true
    while (running) {
      println("\nNospresso Café\n1) Client\n2) Admin\n3) Quitter\n>")
      val choix = readValidatedInt()
      if (choix == 1) { // Mode Client
        println("Sélectionnez une machine (ID) :")
        machines.foreach(_.display())
        val id = readValidatedInt()
        if (id > 0 && id <= machines.length) {
          serveClient(machines(id - 1))
        } else {
          println("ID invalide.")
        }
      } else if (choix == 2) { // Mode Admin
        println("Sélectionnez une machine (ID) :")
        machines.foreach(_.display())
        val id = readValidatedInt()
        if (id > 0 && id <= machines.length) {
          if (validatePin(machines(id - 1))) {
            adminMode(machines(id - 1))
          } else {
            println("Accès refusé.")
          }
        } else {
          println("ID invalide.")
        }
      } else if (choix == 3) { // Quitter
        println("Sauvegarde des données...")
        CSV.savecsv(filename, machines)
        println("Programme terminé.")
        running = false
      } else {
        println("Choix invalide. Veuillez réessayer.")
      }
    }
  }

  def validatePin(machine: Machine): Boolean = {
    println("Entrez le code PIN :")
    val pin = scala.io.StdIn.readLine()
    pin == machine.pincode
  }

  def serveClient(machine: Machine): Unit = {
    var retourMenu = false

    while (!retourMenu) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      var choixBoisson = 0
      var choixBoissonValide = false
      while (!choixBoissonValide) {
        try {
          choixBoisson = scala.io.StdIn.readInt()
          if (choixBoisson >= 1 && choixBoisson <= 3) {
            choixBoissonValide = true
          } else {
            println("Choix invalide. Veuillez entrer un nombre entre 1 et 3.")
          }
        } catch {
          case _=> println("Entrée invalide. Veuillez entrer un nombre.")
        }
      }

      var cafedose = 0
      var laitdose = 0
      var prixBoisson = 0.0
      var boisson = ""

      if (choixBoisson == 1) {
        cafedose = 8
        laitdose = 0
        prixBoisson = 2.0
        boisson = "Expresso"
      } else if (choixBoisson == 2) {
        cafedose = 6
        laitdose = 100
        prixBoisson = 2.5
        boisson = "Cappuccino"
      } else if (choixBoisson == 3) {
        println("Quelle taille de Latte souhaitez-vous ?\n1) Petit\n2) Moyen\n3) Grand")
        val taille = try {
          scala.io.StdIn.readInt()
        } catch {
          case _=> println("Entrée invalide. Taille non reconnue.")
        }

        if (taille == 1) {
          cafedose = 6
          laitdose = 120
          prixBoisson = 2.70
          boisson = "Petit Latte"
        } else if (taille == 2) {
          cafedose = 8
          laitdose = 150
          prixBoisson = 3.20
          boisson = "Moyen Latte"
        } else if (taille == 3) {
          cafedose = 12
          laitdose = 200
          prixBoisson = 3.70
          boisson = "Grand Latte"
        } else {
          println("Taille invalide.")
          return
        }
      } else {
        println("Choix de boisson invalide.")
        return
      }

      // Sucre
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      var choixSucre = 0
      var choixSucreValide = false

      while (!choixSucreValide) {
        try {
          choixSucre = scala.io.StdIn.readInt()
          if (choixSucre >= 1 && choixSucre <= 4) {
            choixSucreValide = true // Entrée valide
          } else {
            println("Choix invalide. Veuillez entrer un nombre entre 1 et 4.")
          }
        } catch {
          case _=> println("Entrée invalide. Veuillez entrer un nombre entre 1 et 4.")
        }
      }

      var sucredose = 0
      var prixSucre = 0.0
      if (choixSucre == 2) {
        sucredose = 5
        prixSucre = 0.10
      } else if (choixSucre == 3) {
        sucredose = 10
        prixSucre = 0.20
      } else if (choixSucre == 4) {
        sucredose = 15
        prixSucre = 0.30
      }

      // Lait supplémentaire
      var laitSupplementaire = 0
      if (choixBoisson == 2 || choixBoisson == 3) {
        var choixLaitSuppValide = false
        var choixLaitSupp = 0

        // Boucle pour valider le choix de lait supplémentaire
        while (!choixLaitSuppValide) {
          try {
            println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
            choixLaitSupp = scala.io.StdIn.readInt()
            if (choixLaitSupp == 1 || choixLaitSupp == 2) {
              choixLaitSuppValide = true
            } else {
              println("Choix invalide. Veuillez entrer 1 pour Oui ou 2 pour Non.")
            }
          } catch {
            case _=> println("Entrée invalide. Veuillez entrer 1 pour Oui ou 2 pour Non.")
          }
        }

        if (choixLaitSupp == 1) {
          var laitSupplementaireValide = false

          // Boucle pour valider la quantité de lait supplémentaire
          while (!laitSupplementaireValide) {
            try {
              println("Combien de doses supplémentaires de lait souhaitez-vous ? (en ml)")
              laitSupplementaire = scala.io.StdIn.readInt()
              if (laitSupplementaire >= 0) {
                laitSupplementaireValide = true
              } else {
                println("Quantité invalide. Veuillez entrer un nombre positif.")
              }
            } catch {
              case _=> println("Entrée invalide. Veuillez entrer un nombre positif.")
            }
          }

          // Mise à jour du prix de la boisson avec le lait supplémentaire
          prixBoisson += laitSupplementaire * 0.05
        }
      }

      // Validation du stock
      if (machine.coffee < cafedose || machine.milk < (laitdose + laitSupplementaire) || machine.sugar < sucredose) {
        println("Stock insuffisant pour préparer votre boisson.")
      } else {
        // Mise à jour des stocks
        machine.removeIngredient("coffee", cafedose)
        machine.removeIngredient("milk", laitdose + laitSupplementaire)
        machine.removeIngredient("sugar", sucredose)

        // Paiement
        val prixTotal = prixBoisson + prixSucre
        println("Prix total : " + prixTotal + " CHF")
        val codeTwint = Random.alphanumeric.take(5).mkString
        println("Veuillez payer avec Twint. Code : " + codeTwint)
        Thread.sleep(5000)
        println("Paiement accepté. Merci pour votre achat.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre " + boisson + " est prêt ! Bonne dégustation !")
      }

      // Retour au menu principal
      retourMenu = true
    }
  }

  def adminMode(machine: Machine): Unit = {
    println("1) Ajouter des ingrédients\n2) Réinitialiser le code PIN")
    val choixAdmin = readValidatedInt()
    if (choixAdmin == 1) {
      println("Quantité de lait à ajouter (en ml) :")
      machine.addIngredient("milk", readValidatedInt())
      println("Quantité de sucre à ajouter :")
      machine.addIngredient("sugar", readValidatedInt())
      println("Quantité de café à ajouter :")
      machine.addIngredient("coffee", readValidatedInt())
    } else if (choixAdmin == 2) {
      println("Nouveau PIN :")
      machine.pincode = scala.io.StdIn.readLine()
    } else {
      println("Choix invalide.")
    }
  }

  def readValidatedInt(): Int = {
    var valide = false
    var resultat = 0

    while (!valide) {
      try {
        resultat = scala.io.StdIn.readInt()
        valide = true
      } catch {
        case _=> println("Entrée invalide. Veuillez entrer un nombre.")
      }
    }
    resultat
  }
}














