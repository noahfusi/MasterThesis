import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}
import io.StdIn._
import util.Random

class Machine(val id: Int, var pincode: String, var lait: Int, var sucre: Int, var cafe: Int) {

  private val ingredients = scala.collection.mutable.Map[String, Int]()
  def getIngredients: Map[String, Int] = ingredients.toMap

  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "lait" => lait += amount
      case "sucre" => sucre += amount
      case "cafe" => cafe += amount
      case _ =>

        if (ingredients.contains(ingredient)) {
          ingredients(ingredient) += amount
        } else {
          ingredients(ingredient) = amount
          println(s"Nouvel ingrédient ajouté : $ingredient avec $amount unité(s).")
        }
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "lait" =>
        if (lait >= amount) {
          lait -= amount
          true
        } else false
      case "sucre" =>
        if (sucre >= amount) {
          sucre -= amount
          true
        } else false
      case "cafe" =>
        if (cafe >= amount) {
          cafe -= amount
          true
        } else false
      case _ =>
        if (ingredients.contains(ingredient) && ingredients(ingredient) >= amount) {
          ingredients(ingredient) -= amount
          if (ingredients(ingredient) == 0) ingredients -= ingredient
          true
        } else {
          println(s"Erreur : Quantité insuffisante pour $ingredient ou ingrédient inexistant.")
          false
        }
    }
  }
}

object Nospresso {
  private var listeMachines = new ArrayBuffer[Machine]()
  private val fichierCSV = "machines.csv"

  val listeBoissons = Array("Expresso", "Cappuccino", "Latte")
  val prixExpresso = 2.00
  val prixCappuccino = 2.50
  val prixLatte = Array(2.70, 3.20, 3.70)

  val poudreCafeExpresso = 8
  val poudreCafeCappuccino = 6
  val poudreCafeLatte = Array(6, 8, 12)
  val laitCappuccino = 100  // mL
  val laitLatte = Array(120, 150, 200)  // mL

  val quantiteSucre = Array(0, 5, 10, 15)
  val prixSucre = Array(0.00, 0.10, 0.20, 0.30)
  val prixDoseLaitSupplement = 0.05


  def loadcsv(nomFichier: String): ArrayBuffer[Machine] = {
    println(s"Chargement des machines depuis ${nomFichier}...")
    try {
      val source = Source.fromFile(nomFichier)
      val lignes = source.getLines().toList
      source.close()

      val donneesMachine = lignes.tail // Séléctionne toutes les lignes sauf la première
      val listeMachinesChargees = new ArrayBuffer[Machine]()

      for ((lignes, index) <- donneesMachine.zipWithIndex) { // Boucle et récupère lignes + index, zipWithIndex afin d'itérer
        val champs = lignes.split(",")

        listeMachinesChargees += new Machine(
          id = index + 1,
          pincode = champs(0),
          lait = champs(1).toInt,
          sucre = champs(2).toInt,
          cafe = champs(3).toInt
        )
      }

      listeMachinesChargees.foreach { machine =>
        println(s"""\nMachine ${machine.id} chargee :
                   |ID: ${machine.id}
                   |Code PIN: ${machine.pincode}
                   |Lait: ${machine.lait/1000.0}L
                   |Sucre: ${machine.sucre}g
                   |Cafe: ${machine.cafe}g""".stripMargin)
      }
      println(s"\n${listeMachinesChargees.size} machine(s) chargee(s) avec succes.")

      listeMachinesChargees
    } catch {
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Verifiez le chemin d'acces et reessayez.")
        System.exit(1)
        new ArrayBuffer[Machine]()
      case e: Exception =>
        println(s"Erreur lors du chargement du fichier : ${e.getMessage}")
        System.exit(1)
        new ArrayBuffer[Machine]()
    }
  }

  def savecsv(nomFichier: String, listeMachines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(nomFichier))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      listeMachines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.lait},${machine.sucre},${machine.cafe}")
      }
      writer.close()
      println(s"Sauvegarde de ${listeMachines.size} machines dans machines.csv...")
      println("Fichier sauvegarde avec succes.")
    } catch {
      case e: Exception =>
        println(s"Erreur : echec de l'ecriture dans machines.csv.")
        println("Le fichier peut etre verrouille ou en lecture seule.")
        System.exit(1)
    }
  }

  def choisirMachine(): Machine = {
    println(s"\nVeuillez choisir une machine (1 a ${listeMachines.size}) :")
    var choix = readInt()
    while (choix < 1 || choix > listeMachines.size) {
      println(s"Erreur : veuillez entrer un nombre entre 1 et ${listeMachines.size}.")
      choix = readInt()
    }
    listeMachines(choix - 1)
  }

  def validatePin(machine: Machine): Boolean = {
    var tentatives = 0
    val tentativesMax = 3
    while (tentatives < tentativesMax) {
      println("Entrez le code PIN :")
      val saisiePin = readLine()
      if (saisiePin == machine.pincode) return true
      tentatives += 1
      println(s"Code PIN incorrect. ${tentativesMax - tentatives} tentatives restantes.")
    }
    false
  }

  def updatePin(machine: Machine): Unit = {
    println(f"Mise a jour du code PIN pour la machine ${machine.id}:")
    println("Entrez un nouveau code PIN a 6 chiffres >")
    var nouveauPin = readLine()
    while (nouveauPin.length != 6) {
      println("Erreur : Le code PIN doit contenir exactement 6 chiffres. Veuillez reessayer :")
      nouveauPin = readLine()
    }
    machine.pincode = nouveauPin
    println("Le code PIN a ete mis a jour avec succes.")
    println("Retour au menu principal...")
  }

  def serveClient(machine: Machine): Boolean = {
    println("\nVeuillez selectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    var choixBoisson = readInt()
    while (choixBoisson < 1 || choixBoisson > 3) {
      println("Erreur : veuillez entrer un nombre entre 1 et 3.")
      choixBoisson = readInt()
    }

    var poudreCafeRequise = 0
    var laitRequis = 0
    var prixBoisson = 0.0

    if (choixBoisson == 1) {
      poudreCafeRequise = poudreCafeExpresso
      laitRequis = 0
      prixBoisson = prixExpresso
    } else if (choixBoisson == 2) {
      poudreCafeRequise = poudreCafeCappuccino
      laitRequis = laitCappuccino
      prixBoisson = prixCappuccino
    } else {
      println("\nVeuillez choisir une taille pour le Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print("> ")

      var choixTailleLatte = readInt()
      while (choixTailleLatte < 1 || choixTailleLatte > 3) {
        println("Erreur : veuillez entrer un nombre entre 1 et 3.")
        choixTailleLatte = readInt()
      }

      poudreCafeRequise = poudreCafeLatte(choixTailleLatte - 1)
      laitRequis = laitLatte(choixTailleLatte - 1)
      prixBoisson = prixLatte(choixTailleLatte - 1)
    }

    // Verification des stocks
    if (machine.cafe < poudreCafeRequise) {
      println("Erreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnee.")
      println("Veuillez choisir une autre boisson ou verifier les stocks en mode Admin.")
      return false
    } else if (machine.lait < laitRequis) {
      println("Erreur : Quantite de lait insuffisante pour preparer la boisson selectionnee.")
      println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    }

    // Ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    var choixSucre = readInt()
    while (choixSucre < 1 || choixSucre > 4) {
      println("Erreur : veuillez entrer un nombre entre 1 et 4.")
      choixSucre = readInt()
    }

    val quantiteSucreChoisi = quantiteSucre(choixSucre - 1)
    val prixSucreChoisi = prixSucre(choixSucre - 1)

    if (machine.sucre < quantiteSucreChoisi) {
      println("Erreur : pas assez de sucre en stock.")
      return false
    }

    // Supplement lait
    var laitSupplement = 0
    var prixLaitSupplement = 0.0
    if (choixBoisson == 2 || choixBoisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplement ?")
      println("1) Oui")
      println("2) Non")
      print("> ")

      var choixLaitSupplementOption = readInt()
      while (choixLaitSupplementOption < 1 || choixLaitSupplementOption > 2) {
        println("Erreur : veuillez entrer un nombre entre 1 et 2.")
        choixLaitSupplementOption = readInt()
      }

      if (choixLaitSupplementOption == 1) {
        println("Combien de doses ?")
        print("> ")
        var choixNombreDoses = readInt()
        while (choixNombreDoses < 1 || choixNombreDoses > 3) {
          println("Erreur : veuillez entrer un nombre entre 1 et 3.")
          choixNombreDoses = readInt()
        }

        laitSupplement = choixNombreDoses * laitRequis
        prixLaitSupplement = choixNombreDoses * prixDoseLaitSupplement
      }
    }

    // Verification finale des stocks et preparation
    if (machine.lait < laitRequis + laitSupplement) {
      println("Erreur : Quantite de lait insuffisante pour preparer la boisson selectionnee.")
      return false
    }

    val prixTotal = prixBoisson + prixSucreChoisi + prixLaitSupplement
    println(f"Prix total : CHF $prixTotal%.2f")
    println("Veuillez payer en utilisant Twint.")
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase()
    println(s"Votre code de paiement est : $codeTwint")
    println("(En attente de validation du paiement...)")
    println("Merci ! Votre paiement a ete accepte.")
    println(s"Preparation de votre ${listeBoissons(choixBoisson - 1)}...")

    // Utilisation des ingredients
    machine.removeIngredient("cafe", poudreCafeRequise)
    machine.removeIngredient("lait", laitRequis + laitSupplement)
    machine.removeIngredient("sucre", quantiteSucreChoisi)

    println(s"Votre ${listeBoissons(choixBoisson - 1)} est pret ! Bonne degustation !")
    true
  }

  def restockMachine(machine: Machine): Unit = {
    println(s"\nStocks actuels de la machine ${machine.id} :")
    println(s"- Cafe : ${machine.cafe} g")
    println(s"- Lait : ${machine.lait/1000.0} L")
    println(s"- Sucre : ${machine.sucre} g")
    // Affichage des ingrédients supplémentaires
    if (machine.getIngredients.nonEmpty) {
      println("\nIngrédients supplémentaires :")
      machine.getIngredients.foreach { case (ingredient, quantite) =>
        println(s"- $ingredient : $quantite unité(s)")
      }
    } else {
      println("Pas d'ingrédients supplémentaires.")
    }

    println("\nReapprovisionnement des stocks :")
    println("Entrez les quantites a ajouter :")

    println("Poudre de cafe (g) : ")
    val ajoutPoudreCafe = readInt()
    machine.addIngredient("cafe", ajoutPoudreCafe)

    println("Lait (mL) : ")
    val ajoutLait = readInt()
    machine.addIngredient("lait", ajoutLait)

    println("Sucre (g) : ")
    val ajoutSucre = readInt()
    machine.addIngredient("sucre", ajoutSucre)

    // Réapprovisionnement des ingrédients supplémentaires
    if (machine.getIngredients.nonEmpty) {
      println("\nRéapprovisionnement des ingrédients supplémentaires :")
      machine.getIngredients.keys.foreach { ingredient =>
        println(s"Quantité à ajouter pour $ingredient : ")
        val quantiteAjout = readInt()
        machine.addIngredient(ingredient, quantiteAjout)
      }
    }

    // Confirmation des nouveaux stocks
    println("\nLes stocks ont été mis à jour avec succès :")
    println(s"- Cafe : ${machine.cafe} g")
    println(s"- Lait : ${machine.lait / 1000.0} L")
    println(s"- Sucre : ${machine.sucre} g")

    if (machine.getIngredients.nonEmpty) {
      println("\nIngrédients supplémentaires :")
      machine.getIngredients.foreach { case (ingredient, quantite) =>
        println(s"- $ingredient : $quantite unité(s)")
      }
    } else {
      println("Pas d'ingrédients supplémentaires.")
    }

    if (machine.getIngredients.nonEmpty) {
      println("\nIngrédients supplémentaires :")
      machine.getIngredients.foreach { case (ingredient, quantite) =>
        println(s"- $ingredient : $quantite unité(s)")
      }
    } else {
      println("\nPas d'ingrédients supplémentaires.")
    }

    println("Retour au menu principal...")
  }


  def modeClient(): Unit = {
    val machine = choisirMachine()
    serveClient(machine)
  }

  def modeAdmin(): Unit = {
    val machine = choisirMachine()

    if (validatePin(machine)) {
      var quitterAdmin = false
      while (!quitterAdmin) {
        println(s"- Cafe : ${machine.cafe} g")
        println(s"- Lait : ${machine.lait/1000.0} L")
        println(s"- Sucre : ${machine.sucre} g")

        if (machine.getIngredients.nonEmpty) {
          println("\nIngrédients supplémentaires :")
          machine.getIngredients.foreach { case (ingredient, quantite) =>
            println(s"- $ingredient : $quantite unité(s)")
          }
        } else {
          println("Pas d'ingrédients supplémentaires.")
        }

        println("\nMenu Admin :")
        println("1) Reapprovisionner les stocks")
        println("2) Modifier le code PIN")
        println("3) Ajouter un ingredient")
        println("4) Retirer un ingrédient")
        println("5) Retour au menu principale")
        print("> ")

        val choix = readInt()
        choix match {
          case 1 =>
            restockMachine(machine)

          case 2 =>
            updatePin(machine)

          case 3 =>
            println("Entrez le nom du nouvel ingrédient à ajouter :")
            val ingredient = readLine().toLowerCase
            println(s"Combien d'unités de $ingredient souhaitez-vous ajouter ?")
            val quantite = readInt()
            machine.addIngredient(ingredient, quantite)
            println(s"$quantite unité(s) de $ingredient ajoutée(s) avec succès.")

          case 4 =>
            println("Entrez le nom de l'ingrédient à retirer :")
            val ingredientARetirer = readLine().toLowerCase
            println(s"Combien d'unités de $ingredientARetirer souhaitez-vous retirer ?")
            val quantiteARetirer = readInt()

            if (machine.removeIngredient(ingredientARetirer, quantiteARetirer)) {
              println(s"$quantiteARetirer unité(s) de $ingredientARetirer retirée(s) avec succès.")
            } else {
              println(s"Erreur : Impossible de retirer $quantiteARetirer unité(s) de $ingredientARetirer. Vérifiez la quantité disponible.")
            }

          case 5 =>
            quitterAdmin = true
          case _ =>
            println("Erreur : option invalide. Veuillez entrer un nombre entre 1 et 2.")
        }
      }
    } else {
      println("Trop de tentatives echouees. Retour au menu principal.")
    }
  }

  def main(args: Array[String]): Unit = {
    listeMachines = loadcsv(fichierCSV)

    var quitter = false
    while (!quitter) {
      println("\n          Nospresso Cafe")
      println("Veuillez selectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      try {
        val choixMode = readInt()
        choixMode match {
          case 1 => modeClient()
          case 2 => modeAdmin()
          case 3 =>
            savecsv(fichierCSV, listeMachines)
            quitter = true
          case _ => println("Erreur : veuillez entrer un nombre entre 1 et 3.")
        }
      } catch {
        case _: NumberFormatException =>
          println("Erreur : veuillez entrer un nombre valide.")

      }
    }
  }
}


