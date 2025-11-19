import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.util.Random

// Classe Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  // Ajout d'un ingrédient
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient.toLowerCase == "milk") {
      milk += amount
    } else if (ingredient.toLowerCase == "sugar") {
      sugar += amount
    } else if (ingredient.toLowerCase == "coffee") {
      coffee += amount
    } else {
      println("Ingrédient inconnu : " + ingredient)
    }
  }

  // Retirer une quantité d'un ingrédient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient.toLowerCase == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (ingredient.toLowerCase == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (ingredient.toLowerCase == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println("Stock insuffisant ou ingrédient inconnu : " + ingredient)
      false
    }
  }

  // Afficher les détails de la machine
  def displayDetails(): Unit = {
    println(s"Machine $id chargée :")
    println(s"  ID: $id")
    println(s"  Code PIN: $pincode")
    println(s"  Lait: ${milk.toDouble / 1000}L")
    println(s"  Sucre: $sugar g")
    println(s"  Café: $coffee g")
  }
}

object Main {

  val machines = ArrayBuffer[Machine]()
  val filename = "machines.csv"

  // Charger les machines à partir d'un fichier CSV
  def loadcsv(): Unit = {
    try {
      val lines = Source.fromFile(filename).getLines().toList
      if (lines.isEmpty) {
        println("Erreur : Le fichier est vide.")
        System.exit(1)
      }
      for (i <- 1 until lines.length) {
        val data = lines(i).split(",")
        if (data.length != 4) {
          println(s"Erreur : Format invalide à la ligne ${i + 1}")
          System.exit(1)
        }
        val machine = new Machine(i, data(0), data(1).toInt, data(2).toInt, data(3).toInt)
        machines += machine
        println(s"Machine $i chargée :")
        println(s"  ID: ${machine.id}")
        println(s"  Code PIN: ${machine.pincode}")
        println(s"  Lait: ${machine.milk.toDouble / 1000}L")
        println(s"  Sucre: ${machine.sugar}g")
        println(s"  Café: ${machine.coffee}g")
      }
      println(s"${machines.length} machine(s) chargée(s) avec succès.")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Veuillez vérifier le chemin et réessayer.")
        System.exit(1)
      case e: Exception =>
        println("Erreur lors de la lecture du fichier : " + e.getMessage)
        System.exit(1)
    }
  }

  // Sauvegarde des machines dans un fichier CSV
  def savecsv(): Unit = {
    try {
      val content = new StringBuilder
      content.append("PINCODE,MILK,SUGAR,COFFEE\n")
      for (machine <- machines) {
        content.append(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      Files.write(Paths.get(filename), content.toString().getBytes(StandardCharsets.UTF_8))
      println(s"Sauvegarde de ${machines.length} machine(s) dans $filename...")
    } catch {
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans le fichier. Vérifiez les permissions.")
      case e: Exception => println("Erreur lors de l'écriture du fichier : " + e.getMessage)
    }
  }

  def genererCodeTwint(): String = {
    val random = new Random()
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to 5).map(_ => caracteres(random.nextInt(caracteres.length))).mkString
  }

  def modeClient(machineId: Int): Unit = {
    val machine = machines(machineId)
    println("Sélectionnez votre boisson :")
    println("1) Expresso")
    println("2) Cappuccino")
    println("3) Latte")

    val choix = readInt()
    var prix = 0.0
    var cafeRequis = 0
    var laitRequis = 0
    var dosesLait = 0

    if (choix == 1) {
      prix = 2.0
      cafeRequis = 8
    } else if (choix == 2 || choix == 3) {
      if (choix == 2) {
        prix = 2.5
        cafeRequis = 6
        laitRequis = 100
      } else {
        println("Choisissez la taille : 1) Petit 2) Moyen 3) Grand")
        val taille = readInt()
        if (taille == 1) {
          prix = 2.7
          laitRequis = 120
          cafeRequis = 6
        } else if (taille == 2) {
          prix = 3.2
          laitRequis = 150
          cafeRequis = 8
        } else if (taille == 3) {
          prix = 3.7
          laitRequis = 200
          cafeRequis = 12
        } else {
          println("Taille invalide.")
          return
        }
      }
      println("Souhaitez-vous ajouter des doses supplémentaires de lait ? (0-3)")
      dosesLait = readInt()
      if (dosesLait < 0 || dosesLait > 3) {
        println("Nombre de doses invalide.")
        return
      }
      laitRequis += dosesLait * 50
      prix += dosesLait * 0.5
    } else {
      println("Choix invalide.")
      return
    }

    println("Souhaitez-vous ajouter du sucre ? (0: Aucun, 1: Peu, 2: Moyen, 3: Beaucoup)")
    val sucreChoisi = readInt()
    val sucreRequis = sucreChoisi * 5
    prix += sucreChoisi * 0.10

    if (machine.coffee < cafeRequis || machine.milk < laitRequis || machine.sugar < sucreRequis) {
      println("Erreur : Stock insuffisant pour préparer la boisson.")
      return
    }

    println(s"Boisson sélectionnée : ${if (choix == 1) "Expresso" else if (choix == 2) "Cappuccino" else "Latte"}")
    println(s"Niveau de sucre : ${if (sucreChoisi == 0) "Aucun" else if (sucreChoisi == 1) "Peu (5g)" else if (sucreChoisi == 2) "Moyen (10g)" else "Beaucoup (15g)"}")
    println(s"Lait en supplément : ${if (dosesLait > 0) s"${dosesLait * 50}ml" else "Non"}")
    println(f"Prix total : CHF $prix%.2f")

    val codePaiement = genererCodeTwint()
    println("Veuillez payer via Twint. Votre code est : " + codePaiement)
    println("En attente de paiement...")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")

    machine.coffee -= cafeRequis
    machine.milk -= laitRequis
    machine.sugar -= sucreRequis

    println("Votre boisson est prête. Bonne dégustation !")
  }

  def modeAdmin(machineId: Int): Unit = {
    val machine = machines(machineId)
    println("Mode Admin - Machine " + (machineId + 1))
    println("1) Réapprovisionner les stocks")
    println("2) Mettre à jour le code PIN")
    val choix = readInt()

    if (choix == 1) {
      println("Entrez la quantité de café à ajouter :")
      val ajoutCafe = readInt()
      println("Entrez la quantité de sucre à ajouter :")
      val ajoutSucre = readInt()
      println("Entrez la quantité de lait à ajouter :")
      val ajoutLait = readInt()
      machine.addIngredient("coffee", ajoutCafe)
      machine.addIngredient("sugar", ajoutSucre)
      machine.addIngredient("milk", ajoutLait)
      println("Stocks mis à jour avec succès.")
    } else if (choix == 2) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      var nouveauPin = readLine()
      while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
        println("Code PIN invalide. Essayez encore :")
        nouveauPin = readLine()
      }
      machine.pincode = nouveauPin
      println("Le code PIN a été mis à jour avec succès.")
    } else {
      println("Choix invalide.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis " + filename + "...")
    loadcsv()

    var exit = false
    while (!exit) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = readInt()

      if (choix == 1) {
        println("Sélectionnez une machine (1 à " + machines.length + ")")
        val machineId = readInt() - 1
        if (machineId >= 0 && machineId < machines.length) {
          modeClient(machineId)
        } else {
          println("ID de machine invalide.")
        }
      } else if (choix == 2) {
        println("Sélectionnez une machine (1 à " + machines.length + ")")
        val machineId = readInt() - 1
        if (machineId >= 0 && machineId < machines.length) {
          modeAdmin(machineId)
        } else {
          println("ID de machine invalide.")
        }
      } else if (choix == 3) {
        println("Sauvegarde des machines...")
        savecsv()
        exit = true
      } else {
        println("Option invalide.")
      }
    }
  }
}
