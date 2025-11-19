
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._
import java.io.{File, PrintWriter}
import scala.io.Source

object Main{

  // Classe Machine
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    // Ajouter des ingrédients
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk"  => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println("Ingrédient invalide.")
      }
    }

    // Retirer des ingrédients
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
          println("Stock insuffisant.")
          false
      }
    }
  }

  // Collection de machines
  val machines = ArrayBuffer[Machine]()

  // Charger les données CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val lines = Source.fromFile(filename).getLines().toArray
      if (lines.isEmpty) throw new Exception("Fichier CSV vide.")
      for ((line, index) <- lines.drop(1).zipWithIndex) {
        val Array(pin, milk, sugar, coffee) = line.split(",").map(_.trim)
        machines += new Machine(index + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
      }
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
      machines
    } catch {
      case _: Exception =>
        println("Erreur : fichier introuvable ou illisible. Vérifier le chemin d'accès et réessayez")
        System.exit(1)
        ArrayBuffer()
    }
  }


  // Sauvegarder les données CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println(s"Souvegarde de ${machines.size} machine(s) dans $filename...")
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("fichier sauvegardé avec succès.")
    } catch {
      case _: Exception =>
        println("Erreur : Échec de la sauvegarde dans le fichier. Le fichier peut être verouillé ou en lecture seule.")
    }
  }

  // Validation du code PIN
  def validatePin(machine: Machine): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN pour la machine ${machine.id} :")
      val pin = readLine()
      if (pin == machine.pincode) {
        println("Code PIN correct.")
        return true
      } else {
        tentatives -= 1
        println(s"Code PIN incorrect. Tentatives restantes : $tentatives.")
      }
    }
    println("Accès refusé après trop de tentatives")
    false
  }

  // Mode client
  def serveClient(machine: Machine): Unit = {
    println(s"Vous utilisez la machine ${machine.id}.")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - 2.00 CHF")
    println("2) Cappuccino - 2.50 CHF")
    println("3) Latte - 2.70 CHF")

    val choice = lireEntreeNumerique()
    var cafeNecessaire = 0
    var laitNecessaire = 0
    var prixBase = 0.0

    if (choice == 1) { // Expresso
      cafeNecessaire = 8
      prixBase = 2.00
    } else if (choice == 2) { // Cappuccino
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.50
    } else if (choice == 3) { // Latte
      println("Choisissez la taille :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      val taille = lireEntreeNumerique()
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
    } else {
      println("Choix invalide.")
      return
    }

    // Ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre - CHF 0.00")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")

    var sucreNecessaire = 0
    var prixSucre = 0.0
    var choixSucreValide = false

    while (!choixSucreValide) {
      val choixSucre = lireEntreeNumerique()
      if (choixSucre == 1) {
        sucreNecessaire = 0
        prixSucre = 0.0
        choixSucreValide = true
      } else if (choixSucre == 2) {
        sucreNecessaire = 5
        prixSucre = 0.10
        choixSucreValide = true
      } else if (choixSucre == 3) {
        sucreNecessaire = 10
        prixSucre = 0.20
        choixSucreValide = true
      } else if (choixSucre == 4) {
        sucreNecessaire = 15
        prixSucre = 0.30
        choixSucreValide = true
      } else {
        println("Choix invalide, veuillez réessayer.")
      }
    }

    // Ajout de lait (supplémentaire)
    var laitSupplementaire = 0
    var prixLait = 0.0

    if (choice == 2 || choice == 3) { // Cappuccino et Latte uniquement
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")

      val choixLait = lireEntreeNumerique()
      if (choixLait == 1) {
        println("Combien de doses ? (Maximum 3) - CHF 0.05 par dose")
        var doses = -1
        var dosesValides = false

        while (!dosesValides) {
          doses = lireEntreeNumerique()
          if (doses >= 0 && doses <= 3) {
            dosesValides = true
          } else {
            println("Erreur : Veuillez entrer un nombre entre 0 et 3.")
          }
        }

        laitSupplementaire = doses * 50 // Chaque dose = 50 ml
        prixLait = doses * 0.05 // Prix = 0.05 CHF par dose
      }
    }

    // Vérification des stocks
    if (!machine.removeIngredient("coffee", cafeNecessaire) ||
      !machine.removeIngredient("milk", laitNecessaire + laitSupplementaire) ||
      !machine.removeIngredient("sugar", sucreNecessaire)) {
      println("Préparation impossible. Stock insuffisant.")
      return
    }

    // Calcul du prix total
    val prixTotal = prixBase + prixSucre + prixLait
    printf("Le prix total est : CHF %.2f\n", prixTotal)

    // Simuler le paiement avec Twint
    println("Veuillez payer avec Twint. \nVotre code de paiement est : " + genererCodeTwint())
    println("En attente de validation du paiement...")
    Thread.sleep(3000)
    println("Paiement confirmé. Préparation de votre boisson...")

    println("Votre boisson est prête ! Bonne dégustation.")
  }


  // Mode administrateur
  def adminMenu(machine: Machine): Unit = {
    println(s"1) Réapprovisionner\n2) Modifier le PIN")
    val choice = lireEntreeNumerique()
    if (choice == 1) {
      // Affichage des stocks avant le réapprovisionnement
      println(s"Stocks actuels de la machine ${machine.id}:")
      afficherStocks(machine)

      // Ajout des ingrédients
      println("Ajout de café (g) :")
      machine.addIngredient("coffee", lireEntreeNumerique())
      println("Ajout de sucre (g) :")
      machine.addIngredient("sugar", lireEntreeNumerique())
      println("Ajout de lait (ml) :")
      machine.addIngredient("milk", lireEntreeNumerique())

      // Affichage des stocks après le réapprovisionnement
      println(s"Stocks mis à jour pour la machine ${machine.id}:")
      afficherStocks(machine)

      println("Réapprovisionnement effectué.")

    } else if (choice == 2) { //changer pin
      println("Entrez le nouveau PIN :")
      val newPin = readLine()
      if (newPin.forall(_.isDigit) && newPin.length == 6) {
        machine.pincode = newPin
        println("Code PIN mis à jour.")
      } else {
        println("Code PIN invalide.")
      }
    } else {
      println("Option invalide.")
    }
  }

    //fontion pour afficher les stocks
  def afficherStocks(machine: Machine): Unit = {
    println(s"- Café : ${machine.coffee} g")
    println(s"- Sucre : ${machine.sugar} g")
    println(s"- Lait : ${machine.milk} ml")
    println("-" * 30)
  }


  // Lecture des choix utilisateur
  def lireEntreeNumerique(): Int = {
    try {
      readLine().toInt
    } catch {
      case _: NumberFormatException => -1
    }
  }

  //code twint
  def genererCodeTwint(): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    //caractere 1
    val randomIndex1 = (math.random * caracteres.length).toInt
    val caractere1 = caracteres(randomIndex1)

    //caractere 2
    val randomIndex2 = (math.random * caracteres.length).toInt
    val caractere2 = caracteres(randomIndex2)

    //caractere 3
    val randomIndex3 = (math.random * caracteres.length).toInt
    val caractere3 = caracteres(randomIndex3)

    //caractere 4
    val randomIndex4 = (math.random * caracteres.length).toInt
    val caractere4 = caracteres(randomIndex4)

    //caractere 5
    val randomIndex5 = (math.random * caracteres.length).toInt
    val caractere5 = caracteres(randomIndex5)

    //additionner tout les caracteres pour cree le code
    caractere1.toString + caractere2.toString + caractere3.toString + caractere4.toString + caractere5.toString
  }

  // Main
  def main(args: Array[String]): Unit = {
    loadcsv("machines.csv")
    var running = true
    while (running) {
      println("1) Client\n2) Admin\n3) Quitter")
      val choix = lireEntreeNumerique()
      choix match {
        case 1 =>
          println("Sélectionnez une machine (1-5) :")
          val id = lireEntreeNumerique()
          if (id >= 1 && id <= machines.size) serveClient(machines(id - 1))
          else println("Machine invalide.")
        case 2 =>
          println("Sélectionnez une machine (1-5) :")
          val id = lireEntreeNumerique()
          if (id >= 1 && id <= machines.size && validatePin(machines(id - 1))) adminMenu(machines(id - 1))
          else println("Accès refusé.")
        case 3 =>
          savecsv("machines.csv", machines)
          running = false
        case _ => println("Choix invalide.")

      }
    }
  }
}