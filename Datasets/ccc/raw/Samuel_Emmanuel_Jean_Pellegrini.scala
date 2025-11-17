import scala.io.StdIn._
import scala.util.Random
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      val lowerIngredient = ingredient.toLowerCase
      if (lowerIngredient == "milk") {
        milk += amount
      } else if (lowerIngredient == "sugar") {
        sugar += amount
      } else if (lowerIngredient == "coffee") {
        coffee += amount
      } else {
        println(s"Ingrédient inconnu : $ingredient")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      val lowerIngredient = ingredient.toLowerCase
      if (lowerIngredient == "milk" && milk >= amount) {
        milk -= amount
        true
      } else if (lowerIngredient == "sugar" && sugar >= amount) {
        sugar -= amount
        true
      } else if (lowerIngredient == "coffee" && coffee >= amount) {
        coffee -= amount
        true
      } else {
        println(s"Stock insuffisant pour $ingredient")
        false
      }
    }
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    val file = new File(filename)
    if (!file.exists()) {
      println("")
      println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
      sys.exit(1)
    } else {
      val source = Source.fromFile(file)
      val lines = source.getLines().drop(1) // Ignore header
      for ((line, idx) <- lines.zipWithIndex) {
        val Array(pin, milk, sugar, coffee) = line.split(",")
        machines += new Machine(idx + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
      }
      source.close()
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val file = new File(filename)
    if (!file.canWrite) {
      println("Erreur Échec de l'écriture dans machines.csv.")
      println("Le fichier peut être verrouillé ou en lecture seule.")
      println("Erreur Échec du chargement ou de la sauvegarde des machines. Fermeture du programme.")
      sys.exit(1)
    } else {
      val writer = new PrintWriter(file)
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { m =>
        writer.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      writer.close()
      println(s"Fichier $filename sauvegardé avec succès.")
    }
  }

  def CodePaiement(): String = {
    Random.alphanumeric.take(5).mkString
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val machine = machines(machineId)

    println("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    val choixBoisson = readInt()

    var utilisationCafe = 0
    var utilisationLait = 0
    var utilisationSucre = 0
    var prixBoisson = 0.0
    var CodeTwint = CodePaiement()

    if (choixBoisson == 1) {
      prixBoisson = 2.00
      utilisationCafe = 8
      utilisationLait = 0
    } else if (choixBoisson == 2) {
      prixBoisson = 2.50
      utilisationCafe = 6
      utilisationLait = 10
    } else if (choixBoisson == 3) {
      println("Quelle taille de latte souhaitez-vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
      print("> ")
      val tailleLatte = readInt()
      if (tailleLatte == 1) {
        prixBoisson = 2.70
        utilisationCafe = 6
        utilisationLait = 12
      } else if (tailleLatte == 2) {
        prixBoisson = 3.20
        utilisationCafe = 8
        utilisationLait = 15
      } else if (tailleLatte == 3) {
        prixBoisson = 3.70
        utilisationCafe = 12
        utilisationLait = 20
      }
    } else {
      println("Choix invalide.")
      return false
    }

    if (utilisationCafe > machine.coffee || utilisationLait > machine.milk) {
      println("Stock insuffisant pour préparer la boisson sélectionnée.")
      return false
    }

    println("Voulez-vous du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    print("> ")
    val choixSucre = readInt()
    if (choixSucre == 2) {
      utilisationSucre = 5
      prixBoisson += 0.10
    } else if (choixSucre == 3) {
      utilisationSucre = 10
      prixBoisson += 0.20
    } else if (choixSucre == 4) {
      utilisationSucre = 15
      prixBoisson += 0.30
    }

    if (utilisationSucre > machine.sugar) {
      println("Stock de sucre insuffisant.")
      return false
    }

    machine.removeIngredient("coffee", utilisationCafe)
    machine.removeIngredient("milk", utilisationLait)
    machine.removeIngredient("sugar", utilisationSucre)

    println(f"Prix total : CHF $prixBoisson%.2f")
    //Paiement
    println("Veuillez payer en utilisant Twint." + "\n" +
      "Votre code de paiement est : " + CodeTwint + "\n" +
      "(En attente de validation du paiement...)")

    Thread.sleep(2000)

    print("\n")
    println("Merci ! Votre paiement a été accepté.")
    println("\n")
    println("Préparation de votre boisson..." + " \n" + "[...]")
    Thread.sleep(2000)
    println("Votre boisson est prête ! Bonne dégustation !")

    savecsv("machines.csv", machines)
    true
  }

  def main(args: Array[String]): Unit = {
    val machines = loadcsv("machines.csv")

    // Variables pour la gestion des choix
    var choixMode = 0
    var choixAdmin = 0
    var choixMachine = 0

    def validatePin(machineId: Int): Boolean = {
      var attempts = 3
      while (attempts > 0) {
        println(s"Entrez le code PIN:")
        print("> ")
        val pin = readLine()
        if (pin == machines(machineId).pincode) {
          println(s"Accès accordé à la Machine ${machines(machineId).id}.")
          return true
        } else {
          attempts -= 1
          println(s"Code PIN incorrect. $attempts tentatives restantes.")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      false
    }

    def restockMachine(machineId: Int): Unit = {
      val machine = machines(machineId)
      println("Niveau de stock actuels :")
      println(s"   Poudre de café > ${machine.coffee}g")
      println(s"   Sucre > ${machine.sugar}g")
      println(s"   Lait > ${machine.milk}mL")

      println("Entrez les quantités à ajouter :")

      println("Poudre de café")
      print("> ")
      val coffeeToAdd = readInt()
      machine.addIngredient("coffee", coffeeToAdd)

      println("Sucre")
      print("> ")
      val sugarToAdd = readInt()
      machine.addIngredient("sugar", sugarToAdd)

      println("Lait")
      print("> ")
      val milkToAdd = readInt()
      machine.addIngredient("milk", milkToAdd)

      println("Les stocks ont été mis à jour avec succès.")
    }

    def updatePin(machineId: Int): Unit = {
      val machine = machines(machineId)
      println(s"Mise à jour du code PIN pour la Machine ${machine.id}.")
      var newPin = ""
      do {
        println("Entrez un nouveau code PIN à 6 chiffres:")
        print("> ")
        newPin = readLine()
        if (newPin.length != 6 || !newPin.forall(_.isDigit)) {
          println("Le code PIN doit comporter exactement 6 chiffres.")
          print("> ")
        }
      } while (newPin.length != 6 || !newPin.forall(_.isDigit))
      machine.pincode = newPin
      println("Le code PIN a été mis à jour avec succès.")
    }

    // Programme principal
    do {
      print("    Nospresso Café" + "\n" +
        "Veuillez sélectionner votre mode :" + "\n" +
        "1) Client" + "\n" +
        "2) Admin" + "\n" +
        "3) Quitter" + "\n" +
        "> ")
      choixMode = readInt()

      if (choixMode == 1) {
        print("Sélectionnez une machine (0-4): > ")
        choixMachine = readInt()
        if (choixMachine >= 0 && choixMachine < machines.length) {
          serveClient(choixMachine, machines)
        } else {
          println("ID de machine invalide.")
        }
      } else if (choixMode == 2) {
        print("Sélectionnez une machine : ")
        for (i <- machines.indices) {
          println(s"$i) Machine ${machines(i).id} : Lait=${machines(i).milk}, Sucre=${machines(i).sugar}, Café=${machines(i).coffee}")
        }
        print("> ")
        choixMachine = readInt()
        if (choixMachine >= 0 && choixMachine < machines.length && validatePin(choixMachine)) {
          println("1) Réapprovisionner les stocks\n2) Mettre à jour le code PIN")
          print("> ")
          choixAdmin = readInt()
          if (choixAdmin == 1) {
            restockMachine(choixMachine)
          } else if (choixAdmin == 2) {
            updatePin(choixMachine)
          } else {
            println("Choix invalide.")
          }
        } else {
          println("ID de machine invalide ou échec de l'authentification.")
        }
      } else if (choixMode == 3) {
        println("Sauvegarde des données avant de quitter...")
        savecsv("machines.csv", machines)
      } else {
        println("Choix invalide.")
      }




    } while (choixMode != 3)
  }
}
