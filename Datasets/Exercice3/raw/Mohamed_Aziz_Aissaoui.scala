import scala.io.{BufferedSource, Source, StdIn}
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Classe Machine
case class Machine(id: Int, var pincode: String, var coffee: Int, var sugar: Int, var milk: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = ingredient match {
    case "coffee" => coffee += amount
    case "sugar" => sugar += amount
    case "milk" => milk += amount
    case _ => println(s"Ingrédient invalide : $ingredient")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient match {
    case "coffee" if coffee >= amount =>
      coffee -= amount
      true
    case "sugar" if sugar >= amount =>
      sugar -= amount
      true
    case "milk" if milk >= amount =>
      milk -= amount
      true
    case _ =>
      println(s"Stock insuffisant pour $ingredient.")
      false
  }
}

object NospressoMultiMachine {
  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    machines ++= loadcsv(filename) // Charger les machines depuis le CSV

    var running = true
    while (running) {
      println("Nospresso Café - Menu principal")
      println("1) Client")
      println("2) Administrateur")
      println("3) Quitter")
      print("> ")

      StdIn.readLine() match {
        case "1" => serveClient()
        case "2" => adminMode()
        case "3" =>
          println("Sauvegarde des machines...")
          savecsv(filename, machines)
          println("Sauvegarde terminée. Au revoir !")
          running = false
        case _ => println("Option invalide. Veuillez sélectionner 1, 2 ou 3.")
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val file: BufferedSource = Source.fromFile(filename)
      val lines = file.getLines().toArray
      file.close()

      lines.tail.zipWithIndex.foreach { case (line, index) =>
        val parts = line.split(",")
        val pincode = parts(0)
        val coffee = parts(1).toInt
        val sugar = parts(2).toInt
        val milk = parts(3).toInt
        val machine = Machine(index + 1, pincode, coffee, sugar, milk)
        machines += machine

        // Sortie pour chaque machine chargée
        println(s"Machine ${index + 1} chargée :")
        println(f"ID: ${machine.id}")
        println(f"Code PIN: ${machine.pincode}")
        println(f"Lait: ${machine.milk / 1000.0}%.3f L")
        println(f"Sucre: ${machine.sugar}g")
        println(f"Café: ${machine.coffee}g")
      }
      println(s"${machines.size} machine(s) chargée(s) avec succès.")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Le programme va se terminer.")
        sys.exit(1)
      case _: NumberFormatException =>
        println("Erreur : Format incorrect dans le fichier CSV. Le programme va se terminer.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}. Le programme va se terminer.")
        sys.exit(1)
    }
  machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val file = new BufferedWriter(new FileWriter(new File(filename)))
      file.write("PINCODE,COFFEE,SUGAR,MILK\n") // Écrire l'en-tête
      machines.foreach { machine =>
        file.write(s"${machine.pincode},${machine.coffee},${machine.sugar},${machine.milk}\n")
      }
      file.close()
      println(s"Fichier $filename sauvegardé avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur : Échec de l'écriture dans $filename. ${e.getMessage}")
    }
  }

  def serveClient(): Unit = {
    println("Veuillez sélectionner une machine :")
    machines.foreach(m => println(s"${m.id}) Machine ${m.id}"))
    val machineId = StdIn.readInt() - 1

    if (machineId >= 0 && machineId < machines.size) {
      val machine = machines(machineId)
      println(s"Bienvenue à la machine ${machine.id}.")
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")

      val drinkChoice = StdIn.readLine()
      var coffeeNeeded = 0
      var milkNeeded = 0
      var basePrice = 0.0
      var drinkName = ""
      var allowExtraMilk = false

      drinkChoice match {
        case "1" =>
          coffeeNeeded = 8
          milkNeeded = 0
          basePrice = 2.0
          drinkName = "Expresso"
          allowExtraMilk = false
        case "2" =>
          coffeeNeeded = 6
          milkNeeded = 100
          basePrice = 2.5
          drinkName = "Cappuccino"
          allowExtraMilk = true
        case "3" =>
          println("Quelle taille voulez-vous pour votre Latte ?")
          println("1) Petit (CHF 2.70)")
          println("2) Moyen (CHF 3.20)")
          println("3) Grand (CHF 3.70)")
          print("> ")

          StdIn.readLine() match {
            case "1" =>
              coffeeNeeded = 6
              milkNeeded = 120
              basePrice = 2.7
              drinkName = "Latte (Petit)"
              allowExtraMilk = true
            case "2" =>
              coffeeNeeded = 8
              milkNeeded = 150
              basePrice = 3.2
              drinkName = "Latte (Moyen)"
              allowExtraMilk = true
            case "3" =>
              coffeeNeeded = 12
              milkNeeded = 200
              basePrice = 3.7
              drinkName = "Latte (Grand)"
              allowExtraMilk = true
            case _ =>
              println("Choix invalide. Retour au menu principal.")
              return
          }
        case _ =>
          println("Choix invalide. Retour au menu principal.")
          return
      }

      if (!machine.removeIngredient("coffee", coffeeNeeded) || !machine.removeIngredient("milk", milkNeeded)) {
        println("Désolé, pas assez de ressources pour préparer cette boisson.")
        return
      }

      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")

      val sugarChoice = StdIn.readLine()
      var sugarNeeded = 0
      var sugarPrice = 0.0

      sugarChoice match {
        case "2" => sugarNeeded = 5; sugarPrice = 0.10
        case "3" => sugarNeeded = 10; sugarPrice = 0.20
        case "4" => sugarNeeded = 15; sugarPrice = 0.30
        case "1" => // Pas de sucre
        case _   => println("Option invalide. Aucun sucre ajouté.")
      }

      if (sugarNeeded > 0 && !machine.removeIngredient("sugar", sugarNeeded)) {
        println("Désolé, pas assez de sucre pour cette commande.")
        return
      }

      var extraMilkUsed = 0
      var extraMilkPrice = 0.0
      if (allowExtraMilk) {
        println("Souhaitez-vous ajouter du lait en supplément ? (CHF 0.05 par dose de 50 ml)")
        println("1) Oui")
        println("2) Non")
        print("> ")

        if (StdIn.readLine() == "1") {
          println("Combien de doses de lait voulez-vous ajouter ? (1 à 3)")
          val doses = StdIn.readInt()
          if (doses >= 1 && doses <= 3) {
            extraMilkUsed = doses * 50
            extraMilkPrice = doses * 0.05
            if (!machine.removeIngredient("milk", extraMilkUsed)) {
              println("Désolé, pas assez de lait pour ce supplément.")
              return
            }
          } else {
            println("Nombre de doses invalide. Aucun supplément ajouté.")
          }
        }
      }

      val totalPrice = basePrice + sugarPrice + extraMilkPrice
      val paymentCode = Random.alphanumeric.take(5).mkString
      println(f"Votre commande : $drinkName - CHF $totalPrice%.2f")
      println(s"Veuillez payer en utilisant Twint. Votre code de paiement est : $paymentCode")
      Thread.sleep(3000)
      println("Paiement confirmé. Préparation en cours...")
      Thread.sleep(5000)
      println(s"Votre $drinkName est prêt ! Bonne dégustation.")
    } else {
      println("Numéro de machine invalide.")
    }
  }

  def adminMode(): Unit = {
    println("Veuillez sélectionner une machine :")
    machines.foreach(m => println(s"${m.id}) Machine ${m.id}"))
    val machineId = StdIn.readInt() - 1

    if (machineId >= 0 && machineId < machines.size) {
      val machine = machines(machineId)

      // Vérification du code PIN
      println("Entrez le code PIN pour accéder au mode administrateur :")
      val enteredPin = StdIn.readLine()
      if (enteredPin != machine.pincode) {
        println("Code PIN incorrect. Accès refusé.")
        return
      }

      // Si le code PIN est correct, continuer avec les options administratives
      println(s"Mode Administrateur pour la machine ${machine.id}.")
      println("1) Voir les stocks")
      println("2) Ajouter des ingrédients")
      println("3) Mettre à jour le code PIN")
      print("> ")

      StdIn.readLine() match {
        case "1" =>
          println(s"Machine ${machine.id} - Café = ${machine.coffee} g, Sucre = ${machine.sugar} g, Lait = ${machine.milk / 1000.0} L")
        case "2" =>
          println("Quel ingrédient voulez-vous ajouter (coffee, sugar, milk) ?")
          val ingredient = StdIn.readLine()
          println("Quantité à ajouter :")
          val amount = StdIn.readInt()
          machine.addIngredient(ingredient, amount)
          println(s"Stock mis à jour : Café = ${machine.coffee} g, Sucre = ${machine.sugar} g, Lait = ${machine.milk / 1000.0} L")
        case "3" =>
          println("Entrez un nouveau code PIN :")
          val newPin = StdIn.readLine()
          if (newPin.matches("\\d{6}")) {
            machine.pincode = newPin
            println("Code PIN mis à jour avec succès.")
          } else {
            println("Code PIN invalide. Le code doit contenir exactement 6 chiffres.")
          }
        case _ =>
          println("Option invalide.")
      }
    } else {
      println("Numéro de machine invalide.")
    }
  }

}
