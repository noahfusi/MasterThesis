import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{BufferedWriter, FileWriter, IOException}


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  // Ajoute une quantité spécifiée d'un ingrédient au stock de la machine
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case "coffee" => coffee += amount
      case _ => println("Ingrédient inconnu.")
    }
  }

  // Retire une quantité spécifiée d'un ingrédient du stock de la machine si le stock est suffisant
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case "coffee" if coffee >= amount => coffee -= amount; true
      case _ => println("Stock insuffisant ou ingrédient inconnu."); false
    }
  }

  def afficherStock(): Unit = {
    println(s"Stock actuel de la Machine ${id}:")
    println(s"Poudre de café : ${coffee}g")
    println(s"Sucre : ${sugar}g")
    println(s"Lait : ${milk}ml")
  }
}

object NospressoMultiMachine {
  val machines: ArrayBuffer[Machine] = loadcsv("src/machines.csv")
 // Lit le fichier CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    try {
      for ((line, index) <- Source.fromFile(filename).getLines().zipWithIndex) {
        if (index > 0) {
          val Array(pincode, milk, sugar, coffee) = line.split(",")
          buffer += new Machine(index, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        }
      }
    } catch {
      case e: IOException =>
        println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        sys.exit(0)
      case e: Exception =>
        println(s"Erreur : Échec du chargement du fichier. ${e.getMessage}")
        sys.exit(0)
    }
    buffer
  }

  // Sauvegarde l'état actuel de toutes les machines dans le fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))
      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
      for (machine <- machines) {
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      writer.close()
      println("Fichier sauvegardé avec succès.")
    } catch {
      case e: IOException => println(s"Erreur : ${e.getMessage}")
    }
  }

  // Valide le code PIN pour la machine sélectionnée
  def validatePin(machine: Machine): Boolean = {
    for (attempt <- 1 to 3) {
      println("Entrez le code PIN :")
      if (readLine(" > ") == machine.pincode) {
        println(s"Accès accordé à la Machine ${machine.id}.")
        return true
      }
      println(s"Code PIN incorrect. ${3 - attempt} tentatives restantes.")
    }
    println("Trop de tentatives échouées.")
    sys.exit(0)
  }

  // Met à jour le code PIN de la machine sélectionnée
  def updatePin(machine: Machine): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machine.id}.")
    var newPin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres >")
      newPin = readLine(" > ")
    } while (!newPin.matches("\\d{6}"))
    machine.pincode = newPin
    println("Le code PIN a été mis à jour avec succès.")
  }

  // Réaprovisionne les ingrédients pour la machine sélectionnée
  def restockMachine(machine: Machine): Unit = {
    println("Entrez les quantités à ajouter :")
    println("Poudre de café :")
    machine.addIngredient("coffee", readLine(" > ").toInt)

    println("Lait :")
    machine.addIngredient("milk", (readLine(" > ").toDouble * 1000).toInt)

    println("Sucre :")
    machine.addIngredient("sugar", readLine(" > ").toInt)

    machine.afficherStock()

    println("Niveaux de stock mis à jour. Retour au menu principal... ")
  }

  def calculCout(choix: Int, sucre: Int): Double = {
    val prix = Map(
      1 -> 2.00,  // Expresso Standard
      2 -> 2.50,  // Cappuccino
      3 -> 2.70   // Latte Petit
    )
    val prixsucre = Map(
      1 -> 0.00,  // Sans Sucre
      2 -> 0.10,  // Peu (5g)
      3 -> 0.20,  // Moyen (10g)
      4 -> 0.30   // Beaucoup (15g)
    )

    prix.getOrElse(choix, 2.00) + prixsucre.getOrElse(sucre, 0.00)
  }

  def serveClient(machine: Machine): Boolean = {
    println("Veuillez sélectionner votre boisson:")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    val choix = readLine(" > ").toInt
    val success = choix match {
      case 1 => machine.removeIngredient("coffee", 8)
      case 2 => machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)
      case 3 => machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 120)
      case _ => println("Choix invalide."); false
    }

    if (success) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      val sucreChoix = readLine(" > ").toInt


      // Mise à jour du stock en fonction du sucre ajouté
      sucreChoix match {
        case 1 =>
        case 2 => machine.removeIngredient("sugar", 5)
        case 3 => machine.removeIngredient("sugar", 10)
        case 4 => machine.removeIngredient("sugar", 15)
        case _ => println("Choix invalide.")
      }
      println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")

      val laitChoix = readLine(" > ").toInt
      laitChoix match {
        case 1 =>
          println("Combien de dose ?")
          val doseLait = readLine(" > ").toInt
          doseLait match {
            case 1 => machine.removeIngredient("milk", 50)
            case 2 => machine.removeIngredient("milk", 100)
            case 3 => machine.removeIngredient("milk", 150)
            case _ => println("Choix invalide.")
          }
        case 2 => machine.removeIngredient("milk", 50)
        case 3 => machine.removeIngredient("milk", 100)
        case 4 => machine.removeIngredient("milk", 150)
        case _ => println("Choix invalide.")
      }
      val cost = calculCout(choix, sucreChoix)
      println(s"Le coût de votre boisson est : ${"%.2f".format(cost)} CHF")

      println("Veuillez payer en utilisant Twint.")
      val code = scala.util.Random.alphanumeric.take(5).mkString
      println(s"Votre code de paiement est : $code")
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      true
    } else {
      println("Stock insuffisant pour la boisson choisie.")
      false
    }
  }

  def selectionnerMachine(): Machine = {
    println("Machine sélectionnée (1-" + machines.length + ")")
    val id = readLine(" > ").toIntOption.getOrElse(0)
    machines.find(_.id == id).getOrElse(selectionnerMachine())
  }
}

object TP3 {
  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis machines.csv...")
    println(s"${NospressoMultiMachine.machines.length} machine(s) chargée(s) avec succès.")

    while (true) {
      println("Nospresso Café. Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      val choix = readLine(" > ")
      choix match {
        case "1" =>
          val machine = NospressoMultiMachine.selectionnerMachine()
          NospressoMultiMachine.serveClient(machine)
        case "2" =>
          val machine = NospressoMultiMachine.selectionnerMachine()
          if (NospressoMultiMachine.validatePin(machine)) {
            println("1) Changer le stock")
            println("2) Changer le code PIN")
            val choixAdmin = readLine(" > ")
            choixAdmin match {
              case "1" => NospressoMultiMachine.restockMachine(machine)
              case "2" => NospressoMultiMachine.updatePin(machine)
              case _ => println("Choix invalide.")
            }
          }
        case "3" =>
          NospressoMultiMachine.savecsv("machines.csv", NospressoMultiMachine.machines)
          println("Au revoir !")
          sys.exit(0)
        case _ => println("Choix invalide.")
      }
    }
  }
}
