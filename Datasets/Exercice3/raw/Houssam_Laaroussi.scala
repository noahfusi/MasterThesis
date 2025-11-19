import scala.io.Source
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.{BufferedWriter, File, FileWriter}

case class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "milk"   => milk += amount
      case "sugar"  => sugar += amount
      case "coffee" => coffee += amount
      case _        => println(s"Ingrédient inconnu : $ingredient")
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
      case _ =>
        println(s"Stock insuffisant pour l'ingrédient : $ingredient")
        false
    }
  }
}

object Main {
  val filename = "machines.csv"
  val machines = ArrayBuffer[Machine]()

  def main(args: Array[String]): Unit = {
    try {
      println("Chargement des machines depuis le fichier CSV...")
      loadcsv(filename).foreach(machines += _)
      println(s"${machines.length} machine(s) chargée(s) avec succès.")
    } catch {
      case e: Exception =>
        println(s"Erreur : Impossible de charger les machines. ${e.getMessage}")
        return
    }

    var running = true
    while (running) {
      println("Nospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      readInt() match {
        case 1 => modeClient()
        case 2 => modeAdmin()
        case 3 =>
          println("Sauvegarde des machines dans le fichier CSV...")
          try {
            savecsv(filename, machines)
            println("Fichier sauvegardé avec succès.")
          } catch {
            case e: Exception =>
              println(s"Erreur : Impossible de sauvegarder les machines. ${e.getMessage}")
          }
          running = false
        case _ => println("Entrée invalide, veuillez réessayer.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Sélectionnez l'identifiant de la machine :")
    print("> ")
    val machineId = readInt()

    machines.find(_.id == machineId) match {
      case Some(machine) =>
        println(s"Machine ${machine.id} sélectionnée.")
        var continuer = true

        while (continuer) {
          println("Que souhaitez-vous faire ?")
          println("1) Commander une boisson")
          println("2) Retour au menu principal")
          print("> ")

          readInt() match {
            case 1 => demanderOptionsBoisson(machine)
            case 2 => continuer = false
            case _ => println("Option invalide. Veuillez réessayer.")
          }
        }

      case None =>
        println("Machine invalide. Retour au menu principal.")
    }
  }

  def demanderOptionsBoisson(machine: Machine): Unit = {
    var boisson = 0
    while (boisson < 1 || boisson > 3) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      boisson = readInt()
      if (boisson < 1 || boisson > 3) println("Erreur : veuillez entrer un nombre entre 1 et 3.")
    }

    var taille = 0
    if (boisson == 3) {
      while (taille < 1 || taille > 3) {
        println("Veuillez choisir la taille du Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")
        taille = readInt()
        if (taille < 1 || taille > 3) println("Erreur : veuillez entrer un nombre entre 1 et 3.")
      }
    }

    var sucre = -1
    while (sucre < 1 || sucre > 4) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      sucre = readInt()
      if (sucre < 1 || sucre > 4) println("Erreur : veuillez entrer un nombre entre 1 et 4.")
    }

    var dosesLait = 0
    if (boisson != 1) {
      var optionLait = -1
      while (optionLait < 1 || optionLait > 2) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print("> ")
        optionLait = readInt()
        if (optionLait < 1 || optionLait > 2) println("Erreur : veuillez entrer un nombre entre 1 et 2.")
      }
      if (optionLait == 1) {
        while (dosesLait < 1 || dosesLait > 3) {
          println("Combien de doses (1-3) ?")
          print("> ")
          dosesLait = readInt()
          if (dosesLait < 1 || dosesLait > 3) println("Erreur : veuillez entrer un nombre entre 1 et 3.")
        }
      }
    }

    traiterCommande(machine, boisson, taille, sucre, dosesLait)
  }

  def modeAdmin(): Unit = {
    println("Sélectionnez l'identifiant de la machine :")
    print("> ")
    val machineId = readInt()
    machines.find(_.id == machineId) match {
      case Some(machine) =>
        if (validatePin(machine)) {
          println(s"Accès administrateur pour la machine $machineId.")
          println(s"Lait : ${machine.milk} ml, Sucre : ${machine.sugar} g, Café : ${machine.coffee} g")
          println("1) Réapprovisionner")
          println("2) Mettre à jour le code PIN")
          print("> ")
          readInt() match {
            case 1 =>
              println("Ajoutez des quantités (exemple : milk 200 sugar 50 coffee 100)")
              val inputs = readLine().split(" ")
              inputs.grouped(2).foreach {
                case Array(ingredient, amount) => machine.addIngredient(ingredient, amount.toInt)
                case _                         => println("Format d'entrée incorrect.")
              }
              println("Stocks mis à jour.")
            case 2 =>
              println("Entrez un nouveau PIN :")
              machine.pincode = readLine()
              println("Code PIN mis à jour avec succès.")
            case _ => println("Option invalide.")
          }
        }
      case None => println("Machine invalide.")
    }
  }

  def validatePin(machine: Machine): Boolean = {
    println("Entrez le code PIN :")
    val inputPin = readLine()
    inputPin == machine.pincode
  }

  def traiterCommande(machine: Machine, boisson: Int, taille: Int, sucre: Int, dosesLait: Int): Unit = {
    val poudreCafeNecessaire: Int = boisson match {
      case 1 => 8
      case 2 => 6
      case 3 if taille == 1 => 6
      case 3 if taille == 2 => 8
      case 3 if taille == 3 => 12
    }

    val laitNecessaire: Int = (boisson match {
      case 2 => (0.1 + (dosesLait * 0.05)) * 1000
      case 3 if taille == 1 => (0.12 + (dosesLait * 0.05)) * 1000
      case 3 if taille == 2 => (0.15 + (dosesLait * 0.05)) * 1000
      case 3 if taille == 3 => (0.2 + (dosesLait * 0.05)) * 1000
      case _ => 0.0
    }).toInt

    val sucreNecessaire: Int = sucre match {
      case 2 => 5
      case 3 => 10
      case 4 => 15
      case _ => 0
    }

    if (poudreCafeNecessaire > machine.coffee) {
      println("Erreur : Quantité de café insuffisante.")
      return
    }

    if (!machine.removeIngredient("milk", laitNecessaire)) {
      println("Erreur : Quantité de lait insuffisante.")
      return
    }

    if (!machine.removeIngredient("sugar", sucreNecessaire)) {
      println("Erreur : Quantité de sucre insuffisante.")
      return
    }

    machine.removeIngredient("coffee", poudreCafeNecessaire)


    val prixBase = boisson match {
      case 1 => 2.00
      case 2 => 2.50
      case 3 if taille == 1 => 2.70
      case 3 if taille == 2 => 3.20
      case 3 if taille == 3 => 3.70
    }

    val prixSucre = sucre match {
      case 2 => 0.10
      case 3 => 0.20
      case 4 => 0.30
      case _ => 0.00
    }

    val prixLait = dosesLait * 0.05
    val prixTotal = prixBase + prixSucre + prixLait

    println(s"Prix total : CHF $prixTotal")
    println("Veuillez payer en utilisant Twint.")
    val codePaiement = scala.util.Random.alphanumeric.take(5).mkString
    println(s"Votre code de paiement est : $codePaiement")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Paiement confirmé.")
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    println("Votre boisson est prête ! Bonne dégustation !")

  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val buffer = ArrayBuffer[Machine]()
    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines().drop(1)
      for ((line, idx) <- lines.zipWithIndex) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        buffer += Machine(idx + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
    } finally {
      source.close()
    }
    buffer
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    try {
      bw.write("PINCODE,MILK,SUGAR,COFFEE\n")
      machines.foreach { machine =>
        bw.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
    } finally {
      bw.close()
    }
  }
}
