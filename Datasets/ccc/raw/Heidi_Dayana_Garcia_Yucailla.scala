import scala.io.StdIn._
import scala.util.Random
import java.io.{FileWriter, PrintWriter}
import scala.io.Source._
import scala.collection.mutable.ArrayBuffer

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    // Méthodes obligatoires:
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      } else if (ingredient == "coffee") {
        coffee += amount
      }
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
      } else false
    }

    // Autres méthodes
    def affichageInfoMachine(): Unit = {
      println("Machine " +  id + " chargée : ")
      println("ID : " + id)
      println("Code PIN : " + pincode)
      println("Lait : " + milk + "ml")
      println("Sucre : " + sugar + "g")
      println("Café : " + coffee + "g")
      println()
    }

    def verifierStocks(coffeeNecessaire: Int, sugarNecessaire: Int, milkNecessaire: Int): Boolean = {
      if (coffee >= coffeeNecessaire && sugar >= sugarNecessaire && milk >= milkNecessaire) {
        true
      } else if (coffee < coffeeNecessaire) {
        println("Erreur : Quantité de café insuffisante pour la machine " + id)
        false
      } else if (sugar < sugarNecessaire) {
        println("Erreur : Quantité de sucre insuffisante pour la machine " + id)
        false
      } else {
        println("Erreur : Quantité de lait insuffisante pour la machine " + id)
        false
      }
    }

    def updatePin(machines: ArrayBuffer[Machine]): Unit = {
      println("Machine sélectionnée : " + id)
      println("Entrez le nouveau code PIN : ")
      val nouveauPin = readLine()

      if (nouveauPin.nonEmpty) {
        machines(id).pincode = nouveauPin
        println("Le code PIN de la machine " + (id) + " a été mis à jour avec succès.")
      } else {
        println("Erreur : Le code PIN ne peut pas être vide.")
      }
    }

    def validatePin(): Boolean = {
      val maxAttempts = 3
      var attempts = 0
      while (attempts < maxAttempts) {
        println("Entrez le code PIN :")
        val inputPin = readLine()
        if (inputPin == pincode) {
          println("Accès accordé à la machine : " + id)
          return true
        } else {
          attempts += 1
          println("Code PIN incorrect. Tentatives restantes : " + (maxAttempts - attempts))
        }
      }
      println("Trop de tentatives échouées. Accès refusé.")
      false
    }

    def preparationBoisson(): Unit = {
      var coffeeNecessaire = 0
      var milkNecessaire = 0
      var sugarNecessaire = 0
      var supplementMilk = 0
      var doseMilk = 0
      var price = 0.0

      // Choix boisson
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte Petit - CHF 2.70")
      println("4) Latte Moyen - CHF 3.20")
      println("5) Latte Grand - CHF 3.70 ")

      val boissonChoice = readInt()

      if (boissonChoice == 1) {
        coffeeNecessaire = 8
      } else if (boissonChoice == 2) {
        coffeeNecessaire = 6
        milkNecessaire = 100
      } else if (boissonChoice == 3) {
        coffeeNecessaire = 6
        milkNecessaire = 120
      } else if (boissonChoice == 4) {
        coffeeNecessaire = 8
        milkNecessaire = 150
      } else if (boissonChoice == 5) {
        coffeeNecessaire = 12
        milkNecessaire = 200
      } else {
        println("Choix invalide.")
        return
      }

      // Choix sucre
      println("Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10 ")
      println("3) Moyen (10g) - CHF 0.20 ")
      println("4) Beaucoup (15g) - CHF 0.30 ")
      print("> ")

      val sugarChoice = readInt()

      if (sugarChoice == 1) {
        sugarNecessaire = 0
      } else if (sugarChoice == 2) {
        sugarNecessaire = 5
      } else if (sugarChoice == 3) {
        sugarNecessaire = 10
      } else if (sugarChoice == 4) {
        sugarNecessaire = 15
      }

      // Choix lait supplémentaire
      if (boissonChoice >= 2 && boissonChoice <= 5) {
        var choixValide = false
        do {
          println("Souhaitez-vous ajouter du lait en supplément ? ")
          println("1) Oui ")
          println("2) Non ")
          print("> ")

          supplementMilk = readInt()
          if (supplementMilk == 1) {
            do {
              println("Combien de dose(s) ? (3 doses maximales par boisson)")
              print("> ")
              doseMilk = readInt()
              if (doseMilk >= 1 && doseMilk <= 3) {
                milkNecessaire += (doseMilk * 50)
                choixValide = true
              } else {
                println("Erreur : 3 doses maximales par boisson ! ")
              }
            } while (doseMilk < 1 || doseMilk > 3)
          } else if (supplementMilk == 2) {
            println("Pas de dose de lait supplémentaire.")
            choixValide = true
          } else {
            println("Choix du lait supplémentaire incorrect.")
          }
        } while (!choixValide)
      }

      if (verifierStocks(coffeeNecessaire, sugarNecessaire, milkNecessaire)) {
        removeIngredient("coffee", coffeeNecessaire)
        removeIngredient("sugar", sugarNecessaire)
        removeIngredient("milk", milkNecessaire)

        val prixTotal = calculerPrix(boissonChoice, sugarChoice, supplementMilk, doseMilk)
        //println("Boisson sélectionnée :" + boissonChoice.)
        //println("Niveau de sucre :" + sugarChoice. + "g")
        println(prixTotal)
        interfacePaiement()
      } else {
        println("Erreur : stocks insuffisants.")
      }
    }

    def calculerPrix(choixBoisson: Int, choixSugar: Int, supplementMilk: Int, doseMilk: Int): Double = {
      val expresso = 2.00
      val cappuccino = 2.50
      val lattePetit = 2.70
      val latteMoyen = 3.20
      val latteGrand = 3.70

      val peuSucre = 0.10
      val moyenSucre = 0.20
      val bcpSucre = 0.30

      var prixBoisson = 0.0
      if (choixBoisson == 1) {
        prixBoisson = expresso
      } else if (choixBoisson == 2) {
        prixBoisson = cappuccino
      } else if (choixBoisson == 3) {
        prixBoisson = lattePetit
      } else if (choixBoisson == 4) {
        prixBoisson = latteMoyen
      } else if (choixBoisson == 5) {
        prixBoisson = latteGrand
      }

      var prixSucre = 0.0
      if (choixSugar == 1) {
        prixSucre = 0.0
      } else if (choixSugar == 2) {
        prixSucre = peuSucre
      } else if (choixSugar == 3) {
        prixSucre = moyenSucre
      } else if (choixSugar == 4) {
        prixSucre = bcpSucre
      }

      var prixMilk = 0.0
      if (supplementMilk == 1){
        prixMilk = doseMilk * 0.05
      } else {
        prixMilk = 0.0
      }

      val prixTotal = prixBoisson + prixSucre + prixMilk
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixSucre, prixMilk, prixTotal)

      prixTotal
    }

    def interfacePaiement(): Unit = {
      var code = Random.alphanumeric.take(5).mkString.toUpperCase
      Thread.sleep(1000)
      println("Veuillez payer en utilisant Twint. ")
      println("Votre code de paiement est : " + code)
      println("(En attente de paiement ...) ")
      Thread.sleep(3000)
      println("Paiement confirmé")
      println()
    }

  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val lines = fromFile(filename).getLines().drop(1)
      for (line <- lines) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        machines += new Machine(machines.length + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
      }
    } catch {
      case _: Exception => println("Erreur : Impossible de charger les machines.")
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println((machine.pincode), (machine.milk), (machine.sugar), (machine.coffee))
      }
      writer.close()
    } catch {
      case _: Exception => println("Erreur : Impossible de sauvegarder les machines.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Chargement des machines depuis le fichier CSV...")

    val machines = loadcsv("machines.csv")

    if (machines.nonEmpty) {
      val machine = machines.head
      machine.affichageInfoMachine()

      var continuer = true
      while (continuer) {
        println("Nospresso Café ")
        println("Veuillez sélectionner votre mode : ")
        println("1) Client ")
        println("2) Admin ")
        println("3) Quitter ")
        print("> ")

        val choixMode = readInt()
        // Choix invalide
        if (choixMode != 1 && choixMode != 2 && choixMode != 3) {
          println("Veuillez sélectionner un mode : 1) Client, 2) Admin ou 3) Quitter ")
        } else if (choixMode == 3) {
          // Quitter
          continuer = false
          println("Fin du programme.")
        } else if (choixMode == 1) {
          machine.preparationBoisson()
        } else if (choixMode == 2) {
          var choixAdmin = 0
          do {
            println("1) Mettre à jour le code PIN ")
            println("2) Réapprovisionner les ingrédients ")
            print("> ")

            choixAdmin = readLine().toInt

            if (choixAdmin < 1 || choixAdmin > 2) {
              println("Veuillez sélectionner : 1) Mettre à jour le code PIN ou 2) Réapprovisionner les ingrédients.")
            }
          } while (choixAdmin < 1 || choixAdmin > 2)

          // J'ai eu des examens très chargés cette semaine et des horaires de travail peu flexibles ces derniers semaines, je n'ai pas résussi à le finir à temps
          // l'exercice était beaucoup plus compliqué de ce que je m'attendais pour pouvoir le finir
        }
      }
    }
  }
}
