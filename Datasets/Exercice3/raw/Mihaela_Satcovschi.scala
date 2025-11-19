import scala.io.Source
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

object Main {
  def main(args: Array[String]): Unit = {
    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
      def addIngredient(ingredient: String, amount: Int): Unit = {
        if (ingredient == "milk") milk += amount
        else if (ingredient == "sugar") sugar += amount
        else if (ingredient == "coffee") coffee += amount
        else println(s"Ingrédient inconnu : $ingredient")
      }

      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        if (ingredient == "milk" && milk >= amount) {
          milk -= amount
          true
        } else if (ingredient == "sugar" && sugar >= amount) {
          sugar -= amount
          true
        } else if (ingredient == "coffee" && coffee >= amount) {
          coffee -= amount
          true
        } else {
          println(s"Stock insuffisant de : $ingredient")
          false
        }
      }
    }

    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()
      try {
        println(s"Chargement des machines depuis $filename...")
        val lines = Source.fromFile(filename).getLines().drop(1)
        for ((line, index) <- lines.zipWithIndex) {
          val Array(pincode, milk, sugar, coffee) = line.split(",")
          machines += new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        }
        machines.foreach(machine => {
          println(s"Machine ${machine.id} chargée :")
          println(s"  ID : ${machine.id}")
          println(s"  Code PIN : ${machine.pincode}")
          println(f"  Lait : ${machine.milk / 1000.0}%.3fL")
          println(s"  Sucre : ${machine.sugar}g")
          println(s"  Café : ${machine.coffee}g")
        })
        println(s"${machines.length} machine(s) chargée(s) avec succès.")
      } catch {
        case _: java.io.FileNotFoundException =>
          println(s"Erreur : Fichier introuvable. Vérifiez le chemin d'accès pour '$filename'.")
        case e: Exception =>
          println(s"Erreur lors du chargement des données : ${e.getMessage}")
      }
      machines
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        val resultat = new PrintWriter(new File(filename))
        resultat.println("PINCODE,MILK,SUGAR,COFFEE")
        for (machine <- machines) {
          resultat.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
        }
        resultat.close()
        println(s"Sauvegarde de ${machines.length} machines dans $filename...")
        println("Fichier sauvegardé avec succès.")
      } catch {
        case _: Exception =>
          println("Erreur : Échec de l'écriture ou de la sauvegarde de machines.")
          println("Fermeture du programme.")
      }
    }


    def serveClient(machine: Machine, machines: ArrayBuffer[Machine]): Boolean = {
      var prix = 0.00
      var runningClient = true
      while (runningClient) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        println("4) Quitter")
        print("> ")
        val choixBoisson = readLine()

        if (choixBoisson == "1") {
          if (machine.removeIngredient("coffee", 8)) {
            prix += 2.00
            println("Expresso sélectionné.")
          } else {
            println("Stock de café insuffisant.")
          }
          runningClient = false
        } else if (choixBoisson == "2") {
          if (machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)) {
            prix += 2.50
            println("Cappuccino sélectionné.")
          } else {
            println("Ingrédients insuffisants.")
          }
          runningClient = false
        } else if (choixBoisson == "3") {
          println("Sélectionnez la taille de votre Latte :")
          println("1) Petit (CHF 2.70)")
          println("2) Moyen (CHF 3.20)")
          println("3) Grand (CHF 3.70)")
          print("> ")
          val tailleLatte = readLine()
          var coffeeNecessaire = 0
          var milkNecessaire = 0

          if (tailleLatte == "1") {
            coffeeNecessaire = 6
            milkNecessaire = 120
            prix += 2.70
          } else if (tailleLatte == "2") {
            coffeeNecessaire = 8
            milkNecessaire = 150
            prix += 3.20
          } else if (tailleLatte == "3") {
            coffeeNecessaire = 12
            milkNecessaire = 200
            prix += 3.70
          } else {
            println("Choix invalide.")
          }

          if (coffeeNecessaire > 0 && milkNecessaire > 0 && machine.removeIngredient("coffee", coffeeNecessaire) && machine.removeIngredient("milk", milkNecessaire)) {
            println(s"Latte sélectionné.")
          } else {
            println("Ingrédients insuffisants.")
          }
          runningClient = false
        } else if (choixBoisson == "4") {
          println("Merci d'avoir utilisé nos services. À bientôt !")
          runningClient = false
        } else {
          println("Choix invalide. Veuillez réessayer.")
        }

        if (choixBoisson == "2" || choixBoisson == "3") {
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print("> ")
          val ajoutLait = readLine()
          if (ajoutLait == "1") {
            println("Combien de doses ?")
            println("1) 50ml - CHF 0.10")
            println("2) 100ml - CHF 0.20")
            println("3) 150ml - CHF 0.30")
            print("> ")
            val doseLait = readLine()
            if (doseLait == "1" && machine.removeIngredient("milk", 50)) {
              prix += 0.10
            } else if (doseLait == "2" && machine.removeIngredient("milk", 100)) {
              prix += 0.20
            } else if (doseLait == "3" && machine.removeIngredient("milk", 150)) {
              prix += 0.30
            } else {
              println("Stock de lait insuffisant ou choix invalide.")
            }
          }
        }

        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        val sugarChoice = readLine().toInt
        if (sugarChoice == 2 && machine.removeIngredient("sugar", 5)) {
          prix += 0.10
        } else if (sugarChoice == 3 && machine.removeIngredient("sugar", 10)) {
          prix += 0.20
        } else if (sugarChoice == 4 && machine.removeIngredient("sugar", 15)) {
          prix += 0.30
        } else if (sugarChoice != 1) {
          println("Stock de sucre insuffisant ou choix invalide.")
        }
      }

      if (prix > 0) {
        savecsv("src/machines.csv", machines)
        println(f"Prix total : CHF $prix%.2f")
        println("Veuillez payer via Twint.")
        Thread.sleep(5000)
        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("Bonne dégustation !")
        true
      } else {
        false
      }
    }

    val machines = loadcsv("src/machines.csv")
    var running = true
    while (running) {
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      val choix = readLine()
      if (choix == "1") {
        println("Veuillez entrer le numéro de la machine :")
        print("> ")
        val machineId = readLine().toInt
        machines.find(_.id == machineId) match {
          case Some(machine) => serveClient(machine, machines)
          case None => println("Numéro de machine invalide.")
        }
      } else if (choix == "2") {
        println("Veuillez entrer le numéro de la machine :")
        print("> ")
        val machineId = readLine().toInt
        machines.find(_.id == machineId) match {
          case Some(machine) =>
            println("Entrez le code PIN :")
            val enteredPin = readLine()
            if (enteredPin == machine.pincode) {
              println("Accès autorisé.")
              println("Entrez les quantités à ajouter (lait, sucre, café) :")
              print("Lait (ml) > ")
              machine.addIngredient("milk", readLine().toInt)
              print("Sucre (g) > ")
              machine.addIngredient("sugar", readLine().toInt)
              print("Café (g) > ")
              machine.addIngredient("coffee", readLine().toInt)
              savecsv("src/machines.csv", machines)
            } else {
              println("Code PIN incorrect.")
            }
          case None => println("Numéro de machine invalide.")
        }
      } else if (choix == "3") {
        savecsv("src/machines.csv", machines)
        running = false
      } else {
        println("Choix invalide.")
      }
    }
  }
}
