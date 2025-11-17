import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine

// Définition de la classe Machine
case class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Méthode pour ajouter des ingrédients
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println(s"Ingrédient invalide: $ingredient")
    }
  }

  // Méthode pour retirer des ingrédients
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount =>
        milk -= amount
        true
      case "sugar" if sugar >= amount =>
        sugar -= amount
        true
      case "coffee" if coffee >= amount =>
        coffee -= amount
        true
      case _ =>
        println(s"Stock insuffisant ou ingrédient invalide: $ingredient")
        false
    }
  }

  // Affichage de la machine sous forme de chaîne CSV
  override def toString: String = s"$pincode,$milk,$sugar,$coffee"
}

// Object NospressoMultiMachines
object NospressoMultiMachines {
  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  // Chargement des machines depuis le fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println(s"Chargement des machines depuis $filename...")
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().drop(1) // Ignorer la première ligne (en-têtes)
      val machineBuffer = ArrayBuffer[Machine]()

      for ((line, index) <- lines.zipWithIndex) {
        val columns = line.split(",").map(_.trim)
        if (columns.length == 4) {
          machineBuffer.append(Machine(index + 1, columns(0), columns(1).toInt, columns(2).toInt, columns(3).toInt))
        } else {
          println(s"Ligne invalide ignorée : $line")
        }
      }

      source.close()
      machineBuffer
    } catch {
      case e: java.io.FileNotFoundException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur lors du chargement du fichier : ${e.getMessage}")
        sys.exit(1)
    }
  }

  // Sauvegarde des machines dans le fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Sauvegarde des machines dans $filename...")
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach(machine => writer.println(machine.toString))
      writer.close()
      println(s"Fichier sauvegardé avec succès dans $filename.")
    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename. Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur lors de la sauvegarde : ${e.getMessage}")
        sys.exit(1)
    }
  }

  // Affichage du menu principal
  def afficherMenu(): Unit = {
    var continuer = true
    while (continuer) {
      println("Bienvenue ! Nospresso Multi-Machines")
      println("1. Mode client")
      println("2. Mode admin")
      println("3. Quitter")

      val choix = lireEntreeValide("Entrez votre choix (1, 2, 3) : ")
      choix match {
        case 1 => modeClient()
        case 2 => modeAdmin()
        case 3 =>
          println("Au revoir!")
          continuer = false
        case _ => println("Choix invalide, veuillez réessayer.")
      }
    }
  }

  // Mode client (sélection d'une machine et commande)
  def modeClient(): Unit = {
    val machineId = selectMachine()
    if (machineId >= 0) {
      println("Commande client...")
      // Vous pouvez ajouter ici des fonctionnalités comme la commande de café
    }
  }

  // Mode admin (gestion des machines)
  def modeAdmin(): Unit = {
    val machineId = selectMachine()
    if (machineId >= 0 && validPin(machineId)) {
      println("1. Afficher les stocks")
      println("2. Réapprovisionner les stocks")
      println("3. Mettre à jour le code PIN")
      println("4. Retour")

      val choixAdmin = lireEntreeValide("Entrez votre choix : ")
      choixAdmin match {
        case 1 => affichageStocks(machineId)
        case 2 => reapprovisionnerStocks(machineId)
        case 3 => updatePin(machineId)
        case _ => println("Choix invalide.")
      }
    }
  }

  // Sélectionner une machine
  def selectMachine(): Int = {
    val machineId = lireEntreeValide(s"Sélectionnez une machine (1-${machines.size}) : ")
    if (machineId >= 1 && machineId <= machines.size) {
      machineId - 1
    } else {
      println("ID de machine invalide.")
      -1
    }
  }

  // Vérifier le code PIN pour l'accès administrateur
  def validPin(machineId: Int): Boolean = {
    val enteredPin = lireEntreeValide("Entrez le code PIN : ")
    if (enteredPin.toString == machines(machineId).pincode) {
      println("Accès accordé.")
      true
    } else {
      println("Code PIN incorrect.")
      false
    }
  }

  // Afficher les stocks de la machine sélectionnée
  def affichageStocks(machineId: Int): Unit = {
    val machine = machines(machineId)
    println(s"Machine ${machine.id}: Lait=${machine.milk}ml, Sucre=${machine.sugar}g, Café=${machine.coffee}g")
  }

  // Réapprovisionner les stocks de la machine
  def reapprovisionnerStocks(machineId: Int): Unit = {
    val machine = machines(machineId)
    machine.addIngredient("milk", lireEntreeValide("Ajoutez lait (ml) : "))
    machine.addIngredient("sugar", lireEntreeValide("Ajoutez sucre (g) : "))
    machine.addIngredient("coffee", lireEntreeValide("Ajoutez café (g) : "))
    println("Réapprovisionnement réussi.")
  }

  // Mettre à jour le code PIN de la machine
  def updatePin(machineId: Int): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}.")
    var pinValide = false
    while (!pinValide) {
      val newPin = lireEntreeValide("Entrez un nouveau code PIN à 6 chiffres : ")
      if (newPin.toString.length == 6) {
        machines(machineId).pincode = newPin.toString
        println("Le code PIN a été mis à jour avec succès.")
        pinValide = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres.")
      }
    }
  }

  // Lecture d'une entrée valide
  def lireEntreeValide(message: String): Int = {
    var entreeValide = false
    var resultat = 0
    while (!entreeValide) {
      try {
        print(message)
        resultat = readLine().toInt
        entreeValide = true
      } catch {
        case _: NumberFormatException =>
          println("Veuillez entrer un nombre valide.")
      }
    }
    resultat
  }

  // Point d'entrée du programme
  def main(args: Array[String]): Unit = {
    val nomFichier = "machines.csv"
    machines ++= loadcsv(nomFichier)
    afficherMenu()
    savecsv(nomFichier, machines)
  }
}