import io.StdIn._
import scala.util.Random
import collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
 def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient non reconnu")
    }
  }

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
      case _ => false
    }
  }
}

object Main {
  var machines = new ArrayBuffer[Machine]()
  val filename = "machines.csv"
  var machineId = 0

  def main(args: Array[String]): Unit = {
    machines = loadcsv(filename)
    if (machines.isEmpty) {
      println("Erreur : Échec du chargement des machines.")
      println("Fermeture du programme")
      return
    }

    var continuer = true
    while (continuer) {
      println("\nNospresso Cafe")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      lireChoixValide(1, 3) match {
        case 1 =>
          println("\nselection machine (1-5)")
          print("> ")
          machineId = lireChoixValide(1, 5) - 1
          serveClient(machineId)
        case 2 =>
          println("\nselection machine (1-5)")
          print("> ")
          machineId = lireChoixValide(1, 5) - 1
          if (validatePin(machineId)) {
            println("\nMenu Admin:")
            println("1) Voir et mettre à jour les stocks")
            println("2) Modifier le PIN")
            println("3) Retour")
            print("> ")

            lireChoixValide(1, 3) match {
              case 1 => restockMachine(machineId)
              case 2 => updatePin(machineId)
              case 3 =>
            }
          }
        case 3 =>
          continuer = false
          println("Sauvegarde des données avant la fermeture...")
          savecsv(filename, machines)
          println("Merci de votre visite. Bonne journée!")
      }
    }
  }

  def lireChoixValide(min: Int, max: Int): Int = {
    var choix = 0
    var valide = false
    while (!valide) {
      try {
        choix = readInt()
        if (choix >= min && choix <= max) {
          valide = true
        } else {
          println(s"Veuillez entrer un nombre entre $min et $max")
          print("> ")
        }
      } catch {
        case _: NumberFormatException =>
          println("Veuillez entrer un nombre valide")
          print("> ")
      }
    }
    choix
  }

  def validatePin(machineId: Int): Boolean = {
    var tentatives = 3
    var valide = false

    while (tentatives > 0 && !valide) {
      println(s"Entrez le code PIN (${tentatives} tentatives restantes) :")
      print("> ")
      val pin = readLine()

      if (pin == machines(machineId).pincode) {
        valide = true
        println("Accès accordé.")
      } else {
        tentatives -= 1
        if (tentatives > 0) {
          println(s"Code PIN incorrect. ${tentatives} tentatives restantes.")
        } else {
          println("Trop de tentatives échouées.")
        }
      }
    }
    valide
  }

  def updatePin(machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres:")
    var nouveauPin = ""
    do {
      nouveauPin = readLine()
      if (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
        println("Le PIN doit contenir exactement 6 chiffres. Réessayez:")
      }
    } while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit))

    machines(machineId).pincode = nouveauPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def verifierStocks(machineId: Int, cafe: Int, sucre: Int = 0, lait: Int = 0): Boolean = {
    val machine = machines(machineId)
    machine.coffee >= cafe && machine.sugar >= sucre && machine.milk >= lait
  }

  def mettreAJourStocks(machineId: Int, cafe: Int, sucre: Int, lait: Int): Unit = {
    val machine = machines(machineId)
    machine.removeIngredient("coffee", cafe)
    if (sucre > 0) machine.removeIngredient("sugar", sucre)
    if (lait > 0) machine.removeIngredient("milk", lait)
  }

  def gererNiveauSucre(): (String, Double, Int) = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    lireChoixValide(1, 4) match {
      case 1 => ("Sans sucre", 0.0, 0)
      case 2 => ("Peu (5g)", 0.10, 5)
      case 3 => ("Moyen (10g)", 0.20, 10)
      case 4 => ("Beaucoup (15g)", 0.30, 15)
    }
  }

  def gererLaitSupplementaire(quantiteBase: Int): (String, Double, Int) = {
    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) Oui")
    println("2) Non")
    print("> ")

    if (lireChoixValide(1, 2) == 1) {
      println("Entrer le nombre de doses (1-3)")
      val doses = lireChoixValide(1, 3)
      (doses.toString, 0.05 * doses, quantiteBase + (50 * doses))
    } else {
      ("NON", 0.0, quantiteBase)
    }
  }

  def traiterPaiement(prix: Double): Boolean = {
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
    println(f"Prix total: CHF $prix%.2f")
    println("Veuillez payer en utilisant Twint.")
    println(s"Votre code de paiement est : $codeTwint")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
    true
  }

  def preparerBoisson(nom: String): Unit = {
    println("Préparation de votre boisson...")
    Thread.sleep(3000)
    println(s"Votre $nom est prêt ! Bonne dégustation !")
  }

  def preparerExpresso(machineId: Int): Boolean = {
    val quantiteCafe = 8
    val prixBase = 2.00
    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val prixTotal = prixBase + prixSucre

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre)) {
      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, 0)
        preparerBoisson("Expresso")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, 0)
      false
    }
  }

  def preparerCappuccino(machineId: Int): Boolean = {
    val quantiteCafe = 6
    val quantiteLaitBase = 100
    val prixBase = 2.50

    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val (_, prixLait, quantiteLaitTotal) = gererLaitSupplementaire(quantiteLaitBase)
    val prixTotal = prixBase + prixSucre + prixLait

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)) {
      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
        preparerBoisson("Cappuccino")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
      false
    }
  }

  def preparerLatte(machineId: Int): Boolean = {
    println("Choisissez la taille:")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    print("> ")

    val (quantiteCafe, quantiteLaitBase, prixBase) = lireChoixValide(1, 3) match {
      case 1 => (6, 120, 2.70)
      case 2 => (8, 150, 3.20)
      case 3 => (12, 200, 3.70)
    }

    val (niveauSucre, prixSucre, quantiteSucre) = gererNiveauSucre()
    val (_, prixLait, quantiteLaitTotal) = gererLaitSupplementaire(quantiteLaitBase)
    val prixTotal = prixBase + prixSucre + prixLait

    if (verifierStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)) {
      if (traiterPaiement(prixTotal)) {
        mettreAJourStocks(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
        preparerBoisson("Latte")
        true
      } else false
    } else {
      afficherErreurStock(machineId, quantiteCafe, quantiteSucre, quantiteLaitTotal)
      false
    }
  }

  def afficherErreurStock(machineId: Int, cafe: Int, sucre: Int, lait: Int): Unit = {
    val machine = machines(machineId)
    if (machine.coffee < cafe) println("Erreur : Quantité de café insuffisante")
    if (machine.sugar < sucre) println("Erreur : Quantité de sucre insuffisante")
    if (machine.milk < lait) println("Erreur : Quantité de lait insuffisante")
    println("Veuillez vérifier les stocks en mode Admin")
  }

  def serveClient(machineId: Int): Boolean = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    lireChoixValide(1, 3) match {
      case 1 => preparerExpresso(machineId)
      case 2 => preparerCappuccino(machineId)
      case 3 => preparerLatte(machineId)
    }
  }

  def restockMachine(machineId: Int): Unit = {
    val machine = machines(machineId)
    println(s"Niveaux de stock actuels pour la Machine ${machineId + 1}:")
    println(f"Café : ${machine.coffee}g")
    println(f"Sucre : ${machine.sugar}g")
    println(f"Lait : ${machine.milk}ml")

    println("\nEntrez les quantités à ajouter :")

    print("Café (g) > ")
    val addCafe = readInt().max(0)
    print("Sucre (g) > ")
    val addSucre = readInt().max(0)
    print("Lait (ml) > ")
    val addLait = readInt().max(0)

    machine.addIngredient("coffee", addCafe)
    machine.addIngredient("sugar", addSucre)
    machine.addIngredient("milk", addLait)

    println("\nStocks mis à jour avec succès:")
    println(f"Café : ${machine.coffee}g (+${addCafe}g)")
    println(f"Sucre : ${machine.sugar}g (+${addSucre}g)")
    println(f"Lait : ${machine.milk}ml (+${addLait}ml)")
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = Source.fromFile(filename)
      val lines = fr.getLines()
      val machines = new ArrayBuffer[Machine]()

      lines.next()

      while (lines.hasNext) {
        val Array(pincode, laitEnMl, sucreEnGrammes, cafeEnGrammes) = lines.next().split(",")

        val milk = laitEnMl.toInt
        val sugar = sucreEnGrammes.toInt
        val coffee = cafeEnGrammes.toInt

        machines += new Machine(
          id = machines.size + 1,
          pincode = pincode,
          milk = milk,
          sugar = sugar,
          coffee = coffee
        )

        val laitEnLitres = milk / 1000.0

        println(s"\nMachine ${machines.size} chargée :")
        println(s"ID: ${machines.size}")
        println(s"Code PIN: $pincode")
        println(f"Lait: ${laitEnLitres}%.3fL")
        println(s"Sucre: ${sugar}g")
        println(s"Café: ${coffee}g")
      }

      fr.close()
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
      machines

    } catch {
      case _: java.io.FileNotFoundException =>
        println(s"Erreur : Le fichier $filename n'existe pas.")
        println("Le programme ne peut pas fonctionner sans fichier de configuration.")
        new ArrayBuffer[Machine]()
      case e: Exception =>
        println(s"Erreur lors du chargement du fichier: ${e.getMessage}")
        println("Le programme ne peut pas fonctionner sans fichier de configuration valide.")
        new ArrayBuffer[Machine]()
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Sauvegarde de ${machines.size} machines dans machines.csv...")
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: Exception =>
        println("Erreur : Échec de l'écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        println(s"Détail de l'erreur : ${e.getMessage}")
    }
  }
}