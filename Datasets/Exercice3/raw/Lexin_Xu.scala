import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  //Méthode à implémenter dans la class machine
  //1) Ajoute une quantité spécifiée d'un ingrédient au stock de la machine
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    } else {
      println(s"Ingrédient inconnu: $ingredient")
    }
  }

  //2) Retirer une quantité spécifiée d'un infredient du stock de la machine si le stock est suffisant.
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk") {
      if (milk >= amount) {
        milk -= amount
        true
      } else {
        println(s"Stock de lait insuffisant.")
        false
      }
    } else if (ingredient == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else {
        println(s"Stock de sucre insuffisant.")
        false
      }
    } else if (ingredient == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else {
        println(s"Stock de café insuffisant.")
        false
      }
    } else {
      println(s"Ingredient inconnu: $ingredient")
      false
    }
  }
}


object Main {
  val filename = "machines.csv"

  // Déclaration des machines sous forme d'une collection ArrayBuffer
  var machines = loadcsv(filename)


  def main(args: Array[String]): Unit = {

    //Quand c'est vide, on va quitter notre programme, parce qu'il y a un erreur(manque de fichier).
    if (machines.isEmpty) {
      println(s"Fichier introuvable. Vérifiez le chemin d'accès et réessayez. Erreur lors du chargement du fichier.")
      println(s"Erreur : Echec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
      println("Chargement échoué. Programme arrêté.")
      return
    }
    println(s"${machines.size} machines chargées avec succès.")


    //Ici c'est le menu tout debut pour le choix de la machine.
    var continuer = true
    while (continuer) {
      println("\nBienvenue chez Nospresso Café - Multi-machines")
      println(s"Sélectionnez une machine (1 à ${machines.size}) :")
      val machineId = readLine("> ").toInt - 1

      if (machineId >= 0 && machineId < machines.size) {
        afficherStocks(machineId)
        menuPrincipal(machineId)
      } else {
        println("Numéro de machine invalide.")
      }
      //Lorsqu'on choisit de quitter le programme, on va afficher tous les information des 5 machines.
      println("\nVoulez-vous quitter le programme et afficher les stocks sauvegardés? (oui/non)")
      val quitter = readLine(">")
      if (quitter.toLowerCase == "oui") {
        continuer = false
        println("\n Affichage des stocks")
        afficherTousLesStocks()
      }
    }
    savecsv(filename, machines)

    println("Merci d'avoir utilisé Nospresso Café!")
  }


  //Méthode pour l'affichage des informations de tous les machines avant de quitter le programme.
  def afficherTousLesStocks(): Unit = {
    println("\nInventaire de toutes les machines")
    for (i <- machines.indices) {
      println(s"\nMachine${i + 1}:")
      afficherStocks(i)
    }
  }



  def afficherStocks(machineId: Int): Unit = {
    val machine = machines(machineId)
    println(s"\nMachine ${machineId + 1} chargée :")
    println(s"ID: ${machine.id}")
    println(s"Code PIN: ${machine.pincode}")
    println(s"Stocks de la machine ${machineId + 1} :")
    println(s"Café : ${machine.coffee}g")
    println(s"Sucre : ${machine.sugar}g")
    println(s"Lait : ${machine.milk}ml")
  }


  //Menu principal des modes.
  def menuPrincipal(machineId: Int): Unit = {
    var redemarrer = true
    while (redemarrer) {
      println(s"\n--- Machine ${machineId + 1} ---")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Retour")

      val choix = readLine("> ").toInt
      if (choix == 1) {
        if (!serveClient(machineId)) {
          println("Retour au menu principal...")
          redemarrer = false
        }
      } else if (choix == 2) {
        if (!validatePin(machineId)) {
          println("Accès administrateur refusé.")
        }
      } else if (choix == 3) {
        redemarrer = false
      } else {
        println("Option invalide.")
      }
    }
  }



  def validatePin(machineId: Int): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN pour la machine ${machineId + 1} :")
      val pin = readLine("> ")
      if (pin == machines(machineId).pincode) {
        menuAdmin(machineId)
        return true
      } else {
        tentatives -= 1
        println(s"Code PIN incorrect. Tentatives restantes: $tentatives")
      }
    }
    false
  }



  def menuAdmin(machineId: Int): Unit = {
    var continuer = true
    while (continuer) {
      println("\nMode Administrateur - Sélectionnez une option :")
      println("1) Réapprovisionner les stocks")
      println("2) Mettre à jour le code PIN")
      println("3) Afficher les stocks")
      println("4) Retour")

      val choix = readLine("> ").toInt
      if (choix == 1) {
        restockMachine(machineId)
      } else if (choix == 2) {
        updatePin(machineId)
      } else if (choix == 3) {
        afficherStocks(machineId)
      } else if (choix == 4) {
        continuer = false
      } else {
        println("Option invalide.")
      }
    }
  }



  def restockMachine(machineId: Int): Unit = {
    println("\nRéapprovisionnement des stocks :")
    println("Ajoutez la quantité de café (en grammes) :")
    val cafeAjoute = readLine("> ").toInt
    machines(machineId).addIngredient("coffee", cafeAjoute)
    println("Ajoutez la quantité de sucre (en grammes) :")
    val sucreAjoute = readLine("> ").toInt
    machines(machineId).addIngredient("sugar", sucreAjoute)
    println("Ajoutez la quantité de lait (en millilitres) :")
    val laitAjoute = readLine("> ").toInt
    machines(machineId).addIngredient("milk", laitAjoute)

    println("Stocks mis à jour avec succès.")
    afficherStocks(machineId)
  }



  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var newPin = ""
    do {
      newPin = readLine("> ")
      if (newPin.matches("\\d{6}")) {
        machines(machineId).pincode = newPin
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Format invalide. Le code PIN doit contenir exactement 6 chiffres.")
      }
    } while (!newPin.matches("\\d{6}"))
  }



  def serveClient(machineId: Int): Boolean = {
    var recommencer = true
    while (recommencer) {
      println("\nMode Client - Sélectionnez une boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      println("4) Retour")

      val choixBoisson = readLine("> ").toInt
      if (choixBoisson == 1) {
        if (!commandeBoisson(machineId, "Expresso", 8, 0, 2.00)) return false
      } else if (choixBoisson == 2) {
        if (!commandeBoisson(machineId, "Cappuccino", 6, 100, 2.50)) return false
      } else if (choixBoisson == 3) {
        if (!tailleLatte(machineId)) return false
      } else if (choixBoisson == 4) {
        recommencer = false
      } else {
        println("Option invalide.")
      }
    }
    true
  }



  def tailleLatte(machineId: Int): Boolean = {
    println("Choisissez la taille du Latte :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")

    val taille = readLine("> ").toInt
    if (taille == 1) {
      if (!commandeBoisson(machineId, "Latte Petit", 6, 120, 2.70)) return false
    } else if (taille == 2) {
      if (!commandeBoisson(machineId, "Latte Moyen", 8, 150, 3.20)) return false
    } else if (taille == 3) {
      if (!commandeBoisson(machineId, "Latte Grand", 12, 200, 3.70)) return false
    } else {
      println("Option invalide.")
    }
    true
  }



  def commandeBoisson(machineId: Int, nom: String, cafeReq: Int, laitReq: Int, prixBase: Double): Boolean = {
    val machine = machines(machineId)
    if (machine.coffee >= cafeReq && machine.milk >= laitReq) {
      val sucre = ajouterSucre()
      val laitSup = if (nom == "Cappuccino" || nom.contains("Latte")) ajouterLaitSupplementaire() else (0, 0.0)

      val prixTotal = prixBase + sucre._2 + laitSup._2

      machine.removeIngredient("coffee", cafeReq)
      machine.removeIngredient("milk", laitReq + laitSup._1)
      machine.removeIngredient("sugar", sucre._1)

      println(f"Commande réussie : $nom pour CHF $prixTotal%.2f")
      true
    } else {
      println(s"Stock insuffisant pour $nom.")
      false
    }
  }



  def ajouterSucre(): (Int, Double) = {
    var choix = 0
    while (choix != 1 && choix != 2 && choix != 3 && choix != 4) {
      println("Ajoutez du sucre :")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")

      val input = readLine("> ").toInt
      if (input == 1 || input == 2 || input == 3 || input == 4) {
        choix = input
      } else {
        println("Option invalide, veuillez choisir entre 1 et 4.")
      }
    }

    if (choix == 2) {
      (5, 0.10)
    } else if (choix == 3) {
      (10, 0.20)
    } else if (choix == 4) {
      (15, 0.30)
    } else {
      (0, 0.0)
    }
  }



  def ajouterLaitSupplementaire(): (Int, Double) = {
    var choix = 0
    while (choix != 1 && choix != 2 && choix != 3 && choix != 4) {
      println("Ajoutez du lait supplémentaire (en litres) :")
      println("1) Non")
      println("2) 1 dose (0.05L) - CHF 0.05")
      println("3) 2 doses (0.1L) - CHF 0.10")
      println("4) 3 doses (0.15L) - CHF 0.15")
      val input = readLine("> ").toInt
      if (input == 1 || input == 2 || input == 3 || input == 4) {
        choix = input
      } else {
        println("Option invalide, veillez choisir entre 1 et 4.")
      }
    }

    if (choix == 2) {
      (50, 0.05)
    } else if (choix == 3) {
      (100, 0.10)
    } else if (choix == 4) {
      (150, 0.15)
    } else {
      (0, 0.0)
    }
  }



  //2) Ce méthode sauvegarde l'état actuel de toutes les machines dans le fichier CSV. Chaque machine doit être enregistré format présenté précédemment.
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    //Gestion des exception, mais n'arrêt pas le programme.
    try {
      println(s"Sauvegarde des machines dans machines.csv...")
      val fw = new PrintWriter(new FileWriter(filename))
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      fw.close()
    } catch {
      case ex: java.io.IOException =>
        println(s"Echec de l'écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule. Erreur lors de la sauvegarde du fichier.")
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines.")
      case ex: Exception =>
        println(s"Echec de l'écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule. Erreur lors de la sauvegarde du fichier.")
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines.")
    }
  }



  //Gestion du fichier CSV
  //1) Ce méthode lit le fichier et retourne une collection ArrayBuffer. Chaque ligne du fichier correspond à une machine.
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machinesBuffer = ArrayBuffer[Machine]()
    //try et catch: gestion des exceptions et arrêt le programme en cas d'erreur.
    try {
      println(s"Chargement des machines depuis machines.csv...")
      val fr = Source.fromFile(filename)
      //On saute la première ligne parce que ce sont les noms des attributs.
      val linesfr = fr.getLines().drop(1)
      var id = 1
      for (line <- linesfr) {
        val attributs = line.split(",").map(_.trim)
        if (attributs.length == 4) {
          val pin = attributs(0)
          val milk = attributs(1).toInt
          val sugar = attributs(2).toInt
          val coffee = attributs(3).toInt
          machinesBuffer += new Machine(id, pin, milk, sugar, coffee)
          id += 1
        }
      }
      fr.close()
    } catch {
      case ex: java.io.FileNotFoundException =>
        println(s"Fichier introuvable. Vérifiez le chemin d'accès et réessayez. Erreur lors du chargement du fichier.")
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        return ArrayBuffer.empty[Machine]

      case ex: java.io.IOException =>
        println(s"Echec de l'écriture dans machines.csv. Le fichier peut être verrouillé ou en lecture seule. Erreur lors de la sauvegarde du fichier.")
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines.")

      case ex: Exception =>
        println(s"Fichier introuvable. Vérifiez le chemin d'accès et réessayez. Erreur lors du chargement du fichier.")
        println(s"Erreur : Echec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
        return ArrayBuffer.empty[Machine]
    }
    machinesBuffer
  }
}