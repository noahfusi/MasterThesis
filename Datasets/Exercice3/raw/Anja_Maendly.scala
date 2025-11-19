import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}

// Classe Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    } else {
      println("L'entrée au clavier n'est pas un ingrédient connu.")
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
    } else {
      false
    }
  }
} // Fin de la class Machine

object Main {
  val machines: ArrayBuffer[Machine] = ArrayBuffer()

  // Chargement machines depuis le fichier machines.csv
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val lignes = Source.fromFile(filename).getLines().toList
      val donneesMachine = lignes.drop(1)

      var index = 1
      for (ligne <- donneesMachine) {
        val colonne = ligne.split(",")
        if (colonne.length == 4) {
          machines += new Machine(id = index, pincode = colonne(0), milk = colonne(1).toInt, sugar = colonne(2).toInt, coffee = colonne(3).toInt)
          index += 1
        } else {
          println("La ligne " + ligne + " n'est pas valide dans le fichier machines.csv")
        }
      }
      println(machines.size + " machine(s) chargée(s) avec succès depuis " + filename + ".")
    } catch {
      case ex: Exception => println("Erreur du chargement du fichier machines.csv.")
    }
    machines
  }

  // Enregistrement des machines dans le fichier machines.csv
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val enregistrement = new PrintWriter(new File(filename))
      enregistrement.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        enregistrement.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      enregistrement.close()
      println(machines.size + " machine(s) sauvegardée(s) avec succès dans " + filename + ".")
    } catch {
      case ex: Exception => println("Erreur lors de l'enregistrement dans le fichier machines.csv.")
    }
  }

  def validatePIN(machine: Machine): Boolean = {
    var attemptsLeft = 3
    while (attemptsLeft > 0) {
      print("Entrez le code PIN :\n> ")
      val pin = readLine()
      if (pin == machine.pincode) {
        println("Accès accordé.")
        return true
      } else {
        attemptsLeft -= 1
        println("Code PIN incorrect. " + attemptsLeft + " tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  def restockMachine(machine: Machine): Unit = {
    println("\nNiveaux de stock actuels :\n\tPoudre de café : " + machine.coffee + " g")
    println("\tSucre : " + machine.sugar + " g")
    printf("\tLait : %.2f L\n", machine.milk.toDouble / 1000)
    println()
    println("Entrez les quantités à ajouter :")
    print("\tPoudre de café > ")
    val coffee = readInt()
    machine.addIngredient("coffee", coffee)
    print("\tSucre > ")
    val sugar = readInt()
    machine.addIngredient("sugar", sugar)
    print("\tLait > ")
    val milk = readInt()
    machine.addIngredient("milk", milk)

    println("Stocks mis à jour avec succès.")
  }

  def serveClient(machine: Machine):Unit = {
    val stockInitialLait = machine.milk
    val stockInitialSucre = machine.sugar
    val stockInitialCafe = machine.coffee

    print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    val choixBoisson = readInt()
    println()

    val (cafeNecessaire, laitNecessaire, nomBoisson, prixBoisson) =
      if (choixBoisson == 1) {
        (8, 0, "Expresso", 2.00)
      } else if (choixBoisson == 2) {
        (6, 100, "Cappuccino", 2.50)
      } else if (choixBoisson == 3) {
        print("1) CHF 2.70 (Petit), 2) CHF 3.20 (Moyen), 3) CHF 3.70 (Grand)\n> ")
        val choixLatte = readInt()
        println()
        if (choixLatte == 1) {
          (6, 120, "Latte (Petit)", 2.70)
        } else if (choixLatte == 2) {
          (8, 150, "Latte (Moyen)", 3.20)
        } else if (choixLatte == 3) {
          (12, 200, "Latte (Grand)", 3.70)
        } else {
          println("L'entrée clavier n'est pas une valeur autorisée.")
          return
        }
      } else {
        println("L'entrée clavier n'est pas une valeur autorisée.")
        return
      }

    var choixSucre = 0
    var quantiteSucre = 0
    var prixSucre = 0.0
    while (choixSucre < 1 || choixSucre > 4) {
      print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
      choixSucre = readInt()
      if (choixSucre == 1) {
        quantiteSucre = 0
        prixSucre = 0.0
      } else if (choixSucre == 2) {
        quantiteSucre = 5
        prixSucre = 0.10
      } else if (choixSucre == 3) {
        quantiteSucre = 10
        prixSucre = 0.20
      } else if (choixSucre == 4) {
        quantiteSucre = 15
        prixSucre = 0.30
      } else {
        println("L'entrée clavier n'est pas une valeur autorisée.")
      }
    }

    var laitSupp = 0
    var quantiteLaitSupp = 0
    var prixLaitSupp = 0.0
    if (choixBoisson == 2 || choixBoisson == 3) {
      print("Souhaitez-vous ajouter une dose de lait supplémentaire ?\n1) Oui\n2) Non\n> ")
      laitSupp = readInt()
      println()
      if (laitSupp == 1) {
        var nbdose = 0
        do {
          print("Combien de doses ?\n> ")
          nbdose = readInt()
          println()
          if (nbdose < 1 || nbdose > 3) {
            println("L'entrée clavier n'est pas une valeur autorisée.")
          }
        } while (nbdose < 1 || nbdose > 3)
        quantiteLaitSupp = nbdose * 50
        prixLaitSupp = nbdose * 0.05
      }
    }

    if (machine.removeIngredient("coffee", cafeNecessaire) && machine.removeIngredient("milk", laitNecessaire + quantiteLaitSupp) && machine.removeIngredient("sugar", quantiteSucre)) {
      println()
      val prixFinal = prixBoisson + prixSucre + prixLaitSupp
      printf("Prix total : %.2f CHF ", prixBoisson)
      printf("+ %.2f CHF ", prixSucre)
      printf("= %.2f CHF ", prixFinal)
      println()

      // Paiement comme ex2
      println("Veuillez payer en utilisant Twint.")
      print("Votre code de paiement est : ")
      for (i <- 1 to 5) { // fais 5 fois car code de longeur 5
        var rand = (math.random() * 36).toInt // On prend un nombre aléatoire entre 0 et 36 (0-9 + A-Z) (0à9 = 10 "cases" et AàZ = 26 cases)
        if (rand >= 10) { // Si le nombre aléatoire est entre 10 et 36, on a donc une lettre
          rand -= 10 // on lui enlève 10 -> 0 et 26
          print(('A' + rand).toChar) // 'A' quand on additionne, on décalle du nombre rand depuis la lettre A
          // ('A' + rand) ca c'est un nombre en ascii, on le transforme en char et on l' imprime
        } else { // si on est la, on print direct parce qu'on a un chiffre de 0 a 9
          print(rand)
        }
      }

      println()
      // Attente de 3 secondes avant validation du paiement
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println()
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre boisson est prête ! Bonne dégustation !")
      true

    } else {
      println("Le stock est insuffisant. Veuillez réessayer.")
      machine.milk = stockInitialLait
      machine.sugar = stockInitialSucre
      machine.coffee = stockInitialCafe
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    loadcsv(filename)

    var running = true
    while (running) {
      print("\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      val mode = readInt()
      println()

      if (mode == 1) {
        print("\t\tNospresso Café\nVeuillez sélectionner une machine (1-5) > ")
        val id = readInt()
        println()
        var machine: Option[Machine] = None
        for (m <- machines) {
          if (m.id == id) {
            machine = Some(m)
          }
        }
        machine match {
          case Some(m) => serveClient(m)
          case None => println("Machine introuvable.")
        }
      } else if (mode == 2) {
        print("Veuillez sélectionnez une machine (1-5) > ")
        val id = readInt()
        println()
        var machine: Option[Machine] = None
        for (m <- machines) {
          if (m.id == id) {
            machine = Some(m)
          }
        }
        machine match {
          case Some(m) =>
            if (validatePIN(m)) {
              print("Que voulez-vous faire ?\n1) Ajouter du stock\n2) Modifier le PIN\n3) Quitter\n> ")
              val choixAdmin = readInt()
              println()
              if (choixAdmin == 1) {
                restockMachine(m)
              } else if (choixAdmin == 2) {
                print("Entrez un nouveau PIN à 6 chiffres > ")
                val newPIN = readLine()
                m.pincode = newPIN
                println("PIN modifié avec succès.")
              } else if (choixAdmin == 3) {
                println("Enregistrement des machines et fin du programme.")
                savecsv(filename, machines)
                running = false
              } else {
                println("L'entrée clavier n'est pas une valeur autorisée.")
              }
            }
          case None => println("Machine introuvable.")
        }
      } else if (mode == 3) {
        println("Enregistrement des machines et fin du programme.")
        savecsv(filename, machines)
        running = false
      } else {
        println("L'entrée clavier n'est pas une valeur autorisée.")
      }
    }
  }

} // Fin de object Main