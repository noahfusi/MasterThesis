import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._
import scala.util.Random

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int,
                var sugar: Int, var coffee: Int) {

    def affiche(): Unit = {
      println("    ID: " + id)
      println("    Code PIN: " + pincode)
      println("    Lait: " + milk + "ml")
      println("    Sucre: " + sugar + "g")
      println("    Café: " + coffee + "g")
    }

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "coffee") {
        coffee += amount
      }
      if (ingredient == "milk") {
        milk += amount
      }
      if (ingredient == "sugar") {
        sugar += amount
      }
    }


    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      var ret = false
      if (ingredient == "coffee" && coffee >= amount) {
        coffee -= amount
        ret = true
      } else if (ingredient == "milk" && milk >= amount) {
        milk -= amount
        ret = true
      } else if (ingredient == "sugar" && sugar >= amount) {
        sugar -= amount
        ret = true
      }
      ret
    }
  }
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("Chargement des machines depuis " + filename + "...")

    val fr = Source.fromFile(filename)
    val content = fr.reset.getLines
    val mac = ArrayBuffer[Machine]()

    content.next // on jète l'entête du fichier
    var id = 0
    while(!content.isEmpty) {
      var ligne = content.next
      var uneMachine = ligne.split(",")
      mac += new Machine(id, uneMachine(0), uneMachine(1).toInt, uneMachine(2).toInt, uneMachine(3).toInt)
      println("Machine " + (id + 1) + " chargée :")
      mac(id).affiche
      id += 1

    }

    println( id + " machine(s) chargée(s) avec succès.")
    fr.close
    mac
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val fw = new PrintWriter(filename)
    fw.println("PINCODE,MILK,SUGAR,COFFEE")
    for(x <- machines)
      fw.println(x.pincode + "," + x.milk + "," + x.sugar + "," + x.coffee)
    fw.close
  }

  def main(args: Array[String]): Unit = {


    // modalités
    val client = 1
    val admin = 2
    val quitter = 3
    var mode = 0

    var machineId = 0
    var machines = ArrayBuffer[Machine]()
    var nbMachines = 0
    try {
      machines = loadcsv("machines.csv")
      nbMachines = machines.length
    }
    catch {
      case ex : java.io.FileNotFoundException => {
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        System.exit(0)
      }
    }


    //fonction pour choisir la machine
    def choixMachine(): Int = {
      var machineId = 0
      do {
        print("\nMachine sélectionnée (1-" + nbMachines + ") > ")
        machineId = readInt()
      } while (machineId < 1 || machineId > nbMachines)
      return (machineId - 1)
    }


    // itération principale
    do {
      // bienvenue
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode: ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      mode = readInt()

      if (mode != client && mode != admin && mode != quitter) {
        println("Le mode sélectionné n'est pas valable.")
        println("Veuillez entrer un nombre valable: " + client + ", " + admin + " ou " + quitter + ".")
      }
      if (mode == client) {
        machineId = choixMachine()
        serveClient(machines(machineId)) //if mode est client --> serveClient
      }
      if (mode == admin) {
        adminMode(machines)
      }
    } while (mode != quitter)
    println("Sauvegarde de " + nbMachines + " machines dans machines.csv...")
    try {
      savecsv("machines.csv", machines)
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case ex : java.io.FileNotFoundException => {
        // j'attendais java.nio.file.AccessDeniedException
        // mais avec le fichier en seule lecture, je reçois java.io.FileNotFoundException
        println("Erreur : Échec de l’écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
      }
    }

    def checkStock(stock: Int, quantity: Int, typeStock: String): Boolean = {
      if (stock > quantity) {
        return (true)
      } else {
        println("Erreur : Quantité de " + typeStock + " insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
    }

    def serveClient(machine : Machine): Boolean = {

      //selection des boissons
      val expresso = 1
      val cappuccino = 2
      val latte = 3
      var boisson = 0
      var sucreSup = 0
      var tailleLatte = 0
      var returnCode = false
      //sélection du boisson
      do {
        println("=====Mode Client=====")
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        boisson = readInt()
        if (boisson != 1 && boisson != 2 && boisson != 3) {
          println("La boisson sélectionnée n'est pas valable.")
          println("Veuillez entrer une boisson valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
        }
      } while (boisson != 1 && boisson != 2 && boisson != 3)

      if (boisson == latte) {
        do {
          println("Quelle taille voulez-vous?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          print("> ")
          tailleLatte = readInt()
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("La taille sélectionnée n'est pas valable.")
            println("Veuillez entrer une taille valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
          }
        } while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3)
      }

      if (boisson == expresso || boisson == cappuccino || boisson == latte) {
        do {
          println("Souhaitez-vous ajouter du sucre?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          sucreSup = readInt()
          if (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4) {
            println("La quantité sélectionnée n'est pas valable.")
            println("Veuillez entrer une quantité valable: " + 1 + ", " + 2 + ", " + 3 + " ou " + 4 + ".")
          }
        } while (sucreSup != 1 && sucreSup != 2 && sucreSup != 3 && sucreSup != 4)
      }

      var laitSup = 0
      var doseLaitSup = 0
      if (boisson == cappuccino || boisson == latte) {
        do {
          println("Souhaitez-vous ajouter du lait en supplément? (Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          print("> ")
          laitSup = readInt()
          if (laitSup != 1 && laitSup != 2) {
            println("La touche sélectionnée n'est pas valable.")
            println("Veuillez entrer une touche valable: " + 1 + " ou " + 2 + ".")
          }
        } while (laitSup != 1 && laitSup != 2)
      }

      if (laitSup == 1) {
        println("Combien de doses?")
        print("> ")
        doseLaitSup = readInt()
        while (doseLaitSup < 1 || doseLaitSup > 3) {
          println("La dose sélectionnée n'est pas valable.")
          println("Veuillez entrer une dose valable: " + 1 + ", " + 2 + " ou " + 3 + ".")
          doseLaitSup = readInt()
        }
      }

      var stockSuffisantCafe = true
      var stockSuffisantLait = true
      var stockSuffisantSucre = true

      //éléments nécessaires
      var poudreCafeEx = 8
      var poudreCafeCap = 6
      var laitCap = 100
      var poudreCafeLat1 = 6
      var laitLat1 = 120
      var poudreCafeLat2 = 8
      var laitLat2 = 150
      var poudreCafeLat3 = 12
      var laitLat3 = 200

      // vérification stock
      if (boisson == 1) {
        stockSuffisantCafe = checkStock(machine.coffee, poudreCafeEx, "poudre de café")
        stockSuffisantSucre = checkStock(machine.sugar, ((sucreSup - 1) * 5), "sucre")
      } else if (boisson == 2) {
        stockSuffisantCafe = checkStock(machine.coffee, poudreCafeCap, "poudre de café")
        stockSuffisantLait = checkStock(machine.milk, ((laitCap) + (doseLaitSup * 50)), "lait")
        stockSuffisantSucre = checkStock(machine.sugar, ((sucreSup - 1) * 5), "sucre")
      } else if (boisson == 3) {
        if (tailleLatte == 1) {
          stockSuffisantCafe = checkStock(machine.coffee, poudreCafeLat1, "poudre de café")
          stockSuffisantLait = checkStock(machine.milk, ((laitLat1) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(machine.sugar, ((sucreSup - 1) * 5), "sucre")
        } else if (tailleLatte == 2) {
          stockSuffisantCafe = checkStock(machine.coffee, poudreCafeLat2, "poudre de café")
          stockSuffisantLait = checkStock(machine.milk, ((laitLat2) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(machine.sugar, ((sucreSup - 1) * 5), "sucre")
        } else if (tailleLatte == 3) {
          stockSuffisantCafe = checkStock(machine.coffee, poudreCafeLat3, "poudre de café")
          stockSuffisantLait = checkStock(machine.milk, ((laitLat3) + (doseLaitSup * 50)), "lait")
          stockSuffisantSucre = checkStock(machine.sugar, ((sucreSup - 1) * 5), "sucre")
        }
      }

      // paiement
      if (stockSuffisantCafe && stockSuffisantLait && stockSuffisantSucre) {
        var prix = 0.0
        if (boisson == expresso) {
          prix = 2
        }
        else if (boisson == cappuccino) {
          prix = 2.50
        }
        else if (tailleLatte == 1) {
          prix = 2.70
        }
        else if (tailleLatte == 2) {
          prix = 3.20
        }
        else if (tailleLatte == 3) {
          prix = 3.70
        }

        val prixLait = (doseLaitSup) * 0.05
        val prixSucre = (sucreSup - 1) * 0.10
        val prixTotal = prix + prixSucre + prixLait

        println("Prix total: " + "CHF " + prix.toFloat + " +" + " CHF " + prixSucre.toFloat + " +" + " CHF " + prixLait.toFloat + " = CHF " + prixTotal.toFloat + ".")
        val twintCode = Random.alphanumeric.take(5).mkString.toUpperCase() //méthode trouvé dans la librairie Scala
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + twintCode + ".")
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")

        // deductionm stock
        if (boisson == expresso) {
          machine.removeIngredient("coffee", poudreCafeEx)
        }
        if (boisson == cappuccino) {
          machine.removeIngredient("coffee", poudreCafeCap)
          machine.removeIngredient("milk", laitCap)
        }
        if (boisson == latte && tailleLatte == 1) {
          machine.removeIngredient("coffee", poudreCafeLat1)
          machine.removeIngredient("milk", laitLat1)
        }
        if (boisson == latte && tailleLatte == 2) {
          machine.removeIngredient("coffee", poudreCafeLat2)
          machine.removeIngredient("milk", laitLat2)
        }
        if (boisson == latte && tailleLatte == 3) {
          machine.removeIngredient("coffee", poudreCafeLat3)
          machine.removeIngredient("milk", laitLat3)
        }
        machine.removeIngredient("sugar", ((sucreSup - 1) * 5))
        machine.removeIngredient("milk", (doseLaitSup * 50))

        //preparation boisson
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        returnCode = true
        if (boisson == 1) {
          println("Votre Expresso est prêt! Bonne dégustation!")
        }
        else if (boisson == 2) {
          println("Votre Cappuccino est prêt! Bonne dégustation!")
        }
        else if (boisson == 3) {
          println("Votre Latte est prêt! Bonne dégustation!")
        }
      }
      return (returnCode)
    }

    // fonction pour valider le PIN
    def validatePin(machine : Machine): Boolean = {
      var correctPin = machine.pincode
      var tentatives = 3
      var found = false
      do {
        var tentativePin = readLine("Entrez le code PIN: ")
        tentatives -= 1
        if (correctPin == tentativePin) {
          println("Accès accordé à la Machine " + (machine.id + 1) + " .")
          found = true
        } else if (tentatives == 1) {
          println("Code PIN incorrect. " + tentatives + " tentative restante.")
        } else {
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
        }
      } while (tentatives > 0 && !found)

      return found
    }

    // fonction mode Admin
    def adminMode(machines : ArrayBuffer[Machine]): Unit = {

      println("==========Mode Admin==========")
      var operationAdmin = 0
      do {
        println("Quelle opération voulez vous faire? ")
        println("1) Restock")
        println("2) Changement du PIN")
        print("> ")
        operationAdmin = readInt()
      } while (operationAdmin != 1 && operationAdmin != 2)

      machineId = choixMachine()
      if (!validatePin(machines(machineId))) {
        println("Trop de tentatives échouées. Fin du programme.")
        return
      }

      if (operationAdmin == 1) {
        restockMachine(machines(machineId))
      } else if (operationAdmin == 2) {
        updatePin(machines(machineId))
      }
    }

    def updatePin(machine: Machine): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machine.id + 1) + ".")
      var newPin = 0.toString
      do {
        print("Entrez un nouveau code PIN à 6 chiffres > ")
        newPin = readLine()
      } while (newPin.size != 6 || !newPin.toIntOption.isDefined)

      machine.pincode = newPin
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")

    }

    def restockMachine(machine: Machine): Unit = {

      println("Niveaux de stock actuels:")
      println("Poudre de café : " + machine.coffee)
      println("Lait : " + machine.milk)
      println("Sucre : " + machine.sugar)

      println("Entrez les quantités à ajouter:")
      print("Poudre de café: ")
      machine.addIngredient("coffee", readInt())
      print("Lait: ")
      machine.addIngredient("milk", readInt())
      print("Sucre: ")
      machine.addIngredient("sugar", readInt())
      println("Les stocks ont été mis à jour avec succès.")
      println("Retour au menu principal...")
      Thread.sleep(1000)

    }


  }
}