import java.io.{FileWriter, IOException, PrintWriter}
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`buffer AsJavaList`
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random




object main {

  case class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "MILK") {
        milk = milk + amount
      } else if (ingredient == "SUGAR") {
        sugar = sugar + amount
      } else if (ingredient == "COFFEE") {
        coffee = coffee + amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "MILK") {
        milk = milk - amount
        if (milk < 0) {
          return false
        } else {
          return true
        }
      } else if (ingredient == "SUGAR") {
        sugar = sugar + amount
        if (sugar < 0) {
          return false
        } else {
          return true
        }
      } else if (ingredient == "COFFEE") {
        coffee = coffee + amount
        if (coffee < 0) {
          return false
        } else {
          return true
        }
      } else {
        return false
      }
    }
  }


  def main(args: Array[String]): Unit = {

    // initialisation des variables

    var enMarche = true //variable pour le boucle principale


    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      println("Chargement des machines depuis machines.csv...")
      val machines: ArrayBuffer[Machine] = ArrayBuffer()
      var id = 0
      var n = 1

      try {
        for (line <- Source.fromFile(filename).getLines) {
          val Array(pincode, milk, sugar, coffee) = line.split(",")
          if (pincode != "PINCODE") {
            machines += Machine(id, pincode.toString, milk.toInt, sugar.toInt, coffee.toInt)
            println(s"Machine $n chargée \n")
            println(s"    ID: $id\n")
            println(s"    Code PIN: ${pincode.toInt}\n")
            println(s"    Lait: ${milk.toInt * 0.001}L\n")
            println(s"    Sucre: ${sugar.toInt}g\n")
            println(s"    Café1: ${coffee.toInt}g\n")
            id += 1
            n += 1
          }
        }
        println(s"${machines.length} machine(s) chargé(s) avec succès.")


      } catch {
        case _: java.io.FileNotFoundException => println("Erreur: Fichier introuvable. Vérifiez le chemin d’accès et réessayez.\n")
      }

      machines

    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      println(s"Sauvegarde de ${machines.length} machines dans machines.csv...\n")
        try {
          val writer = new PrintWriter(new FileWriter(filename))
          machines.foreach(machine => writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}"))
          writer.close()
          println(s"Fichier Sauvegardé avec succès\n")
        }catch {
          case _: IOException => {
            println("Erreur: Echec de l'écriture dans machines.csv...\nLe fichier peut être verrouillé ou en lecture seule.\n")
            println("Erreur: Echec du chargement ou de la sauvegarde des machines.\n")
            println("Fermeture du programme.\n")
          }
        }
    }

    val machines = loadcsv("machines.csv")
    if (machines.length == 0) {
      enMarche = false
      println("Erreur: Echec du chargement ou de la sauvegarde des machines.\n")
      println("Fermeture du programme.\n")
    }

    val nbMachines = machines.length
    val pins: ArrayBuffer[String] = ArrayBuffer()
    val stocksCafe: ArrayBuffer[Int] = ArrayBuffer()
    val stocksSucre: ArrayBuffer[Int] = ArrayBuffer()
    val stocksLait: ArrayBuffer[Int] = ArrayBuffer()
    machines.foreach(machine => {
      pins += machine.pincode
      stocksCafe += machine.coffee
      stocksSucre += machine.sugar
      stocksLait += machine.milk
    })


    //boucle principale
    while (enMarche){

      //interface utilisateur
      println("Nospresso Café \nVeuillez choisir votre mode :\n1) Client\n2) Admin\n3) Quitter")

      var choixUtilisateur = readInt() //choix de l'utilisateur

      if (choixUtilisateur == 1){ // MODE CLIENT -----------------------------------------------------------------------

        // on initialise le choix de la machine à 0
        var choixMachine = 0
        while (choixMachine < 1 || choixMachine > nbMachines){
          println(s"Machine sélectionnée (1-$nbMachines)")
          choixMachine = readInt()
        }

        if (!serveClient(choixMachine-1, stocksCafe, stocksSucre, stocksLait)){
          println("Erreur : Impossible de préparer la boisson. Veuillez essayer une autre machine.\n")
        }

      } else if (choixUtilisateur == 2){ // MODE ADMIN -----------------------------------------------------------------

        // on initialise le choix de la machine à 0
        var choixMachine = 0
        while (choixMachine < 1 || choixMachine > nbMachines) {
          println(s"Machine sélectionnée (1-$nbMachines)")
          choixMachine = readInt()
        }

        if (validatePin(choixMachine-1, pins)) {
          println("Accès accordé. Que voulez-vous faire ?\n1) Réapprovisionner\n2) Mettre à jour le code PIN")
          var choixAdmin = readInt()

          while (choixAdmin < 1 || choixAdmin > 2) {
            println("Choix invalide. Veuillez choisir une option valide.")
            println("Que voulez-vous faire ?\n1) Réapprovisionner\n2) Mettre à jour le code PIN")
            choixAdmin = readInt()
          }

          if(choixAdmin==1){
            restockMachine(choixMachine-1, stocksCafe, stocksSucre, stocksLait)
          }else if (choixAdmin==2){
            updatePin(choixMachine-1, pins)
          }

        } else {
          println("Code PIN incorrect après 3 tentatives. Fin du programme.")
          enMarche = false
        }

      } else if (choixUtilisateur == 3){
        savecsv("machines.csv", machines)
        enMarche = false
      }


    }



    def serveClient(machineId: Int, coffeeStocks: ArrayBuffer[Int], sugarStocks: ArrayBuffer[Int], milkStocks: ArrayBuffer[Int]): Boolean = {
      println("Mode Client - Machine " + machineId)


      //valeur de retour
      var operationReussie = false
      var prixTotal = 0.0
      var textPrixTotal = ""

      // on initialise le choix de la boisson à 0
      var choixBoisson: Int = 0
      // on initialise le texte de la boisson pour le résumé de la commande
      var boissonText = ""
      // on initialise la quantité de café pour le résumé de la commande
      var cafeQuantite = 0
      // on initialise la quantité de lait pour le résumé de la commande
      var laitQuantite = 0

      //tant que le choix de la boisson n'est pas valide, on redemande à l'utilisateur de choisir
      while (choixBoisson < 1 || choixBoisson > 3) {
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte (Petit) - CHF 2.70" +
          ", CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choixBoisson = readInt()
      }

      // on ajoute le prix de la boisson au prix total
      var choixTaille: Int = 0

      // on initialise le texte du latte pour le résumé de la commande
      var latteText = ""

      // on met à jour le prix et la quantité de café en fonction du choix de l'utilisateur

      if (choixBoisson == 1) {
        boissonText = "Expresso"
        prixTotal += 2.0
        cafeQuantite = 8
        textPrixTotal = "CHF 2.00 "
      } else if (choixBoisson == 2) {
        boissonText = "Cappuccino"
        prixTotal += 2.5
        cafeQuantite = 6
        laitQuantite = 100
        textPrixTotal = "CHF 2.50 "
      } else if (choixBoisson == 3) {
        boissonText = "Latte"
        while (choixTaille < 1 || choixTaille > 3) {
          println("Veuillez sélectionner la taille de votre Latte :\n1) Petit\n2) Moyen\n3) Grand")
          choixTaille = readInt()
        }
        if (choixTaille == 1) {
          latteText = "(Petit)"
          prixTotal += 2.7
          cafeQuantite = 6
          laitQuantite = 120
          textPrixTotal = "CHF 2.70 "
        } else if (choixTaille == 2) {
          latteText = "(Moyen)"
          prixTotal += 3.2
          cafeQuantite = 8
          laitQuantite = 180
          textPrixTotal = "CHF 3.20 "
        } else if (choixTaille == 3) {
          latteText = "(Grand)"
          prixTotal += 3.7
          cafeQuantite = 12
          laitQuantite = 200
          textPrixTotal = "CHF 3.70 "
        }
      }


      // on demande à l'utilisateur s'il veut du sucre
      var sucre: Int = 0
      // on initialise le texte du sucre pour le résumé de la commande
      var sucreText = ""
      // on initialise la quantité de sucre pour le résumé de la commande
      var sucreQuantite = 0
      while (sucre < 1 || sucre > 4) {
        println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)")
        sucre = readInt()
      }

      // on met à jour le prix, la quantité de sucre et le texte du sucre en fonction du choix de l'utilisateur

      if (sucre == 1) {
        sucreText = "Sans sucre"
        prixTotal += 0.0
        sucreQuantite = 0
      } else if (sucre == 2) {
        sucreText = "Peu"
        prixTotal += 0.1
        sucreQuantite = 5
        textPrixTotal += "+ CHF 0.10"
      } else if (sucre == 3) {
        sucreText = "Moyen"
        prixTotal += 0.2
        sucreQuantite = 10
        textPrixTotal += "+ CHF 0.20"
      } else if (sucre == 4) {
        sucreText = "Beaucoup"
        prixTotal += 0.3
        sucreQuantite = 15
        textPrixTotal += "+ CHF 0.30"
      }

      //ajouter du lait supplémentaire pour le latte et le cappuccino
      var choixAjoutLait: Int = 0
      // on initialise le texte du lait pour le résumé de la commande
      var laitText = ""


      if (choixBoisson == 2 || choixBoisson == 3) {
        while (choixAjoutLait < 1 || choixAjoutLait > 2) {
          println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
          choixAjoutLait = readInt()
        }
        if (choixAjoutLait == 1) {
          laitText = "Oui"

          var doseLait = 0 //dose de lait max 3

          while (doseLait < 1 || doseLait > 3) {
            println("Combien de dose ? (max 3)")
            doseLait = readInt()
          }

          // on met à jour le prix et la quantité de lait en fonction du choix de l'utilisateur
          prixTotal += doseLait * 0.05
          laitQuantite += doseLait * 50
          textPrixTotal += " + CHF "+doseLait * 0.05
        } else {
          laitText = "Non"
        }
      }

      // résumé de la commande
      println("\nBoisson sélectionnée : " + boissonText + " " + latteText)
      println("Niveau de sucre : " + sucreText)
      if (choixBoisson == 2 || choixBoisson == 3) {
        println("Lait supplémentaire : " + laitText)
      }
      println(f"Prix total : ${textPrixTotal} = CHF ${prixTotal}\n")

      //on vérifie les quantités de stock
      if (sugarStocks(machineId) < sucreQuantite) {
        println("\nQuantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
      } else if (coffeeStocks(machineId) < cafeQuantite) {
        println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
      } else if (milkStocks(machineId) < laitQuantite) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
      } else {

        //interface de paiement via twint

        var codeTwint = Random.alphanumeric.take(5).mkString

        println("\nVeuillez payen en utilisant Twint\nVotre code de paiement est : " + codeTwint)

        println("(En attente de validation du paiement")
        Thread.sleep(3000)

        println("\nMerci ! Votre paiement a été accepté")

        // on déduit les quantités de stock
        sugarStocks(machineId) -= sucreQuantite
        coffeeStocks(machineId) -= cafeQuantite
        milkStocks(machineId) -= laitQuantite


        // on affiche la préparation de la boissons

        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre + " + boissonText + " est prêt ! Bonne dégustation !\n")


        Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)

        operationReussie = true

      }

      return operationReussie


    }


    def validatePin(machineId: Int, machinePins: ArrayBuffer[String]): Boolean = {
      //variable pour le nombre de tentatives
      var tentatives = 3
      //tant que le nombre de tentatives n'est pas
      //épuisé, on demande à l'utilisateur de rentrer le code PIN
      while (tentatives > 0 ) {
        println("Entrez le code PIN :")
        val pin = readLine()
        if (pin == machinePins(machineId)) {
          return true
        } else {
          tentatives -= 1
          println(s"Code PIN incorrect. ${tentatives} tentatives restantes.")
        }
      }
      return false
    }


    def updatePin(machineId: Int, machinePins: ArrayBuffer[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine " + machineId + ":")
      println("Entrez un nouveau code PIN `a 6  chiffres :")
      var nouveauPin = ""
      while (nouveauPin.length != 6) {
        nouveauPin = readLine()
        if (nouveauPin.length != 6 ) {
          println("Le code PIN doit comporter exactement 6 chiffres. Réessayez :")
        }
      }
      machinePins(machineId) = nouveauPin
      println("Le code PIN a été mis à jour avec succès.")
    }


    def restockMachine(machineId: Int, coffeeStocks: ArrayBuffer[Int], sugarStocks: ArrayBuffer[Int], milkStocks: ArrayBuffer[Int]): Unit = {

      //affichage des stocks actuels
      println("Stock actuel :")
      println("Poudre de café : " + coffeeStocks(machineId) + "g")
      println("Lait : " + milkStocks(machineId) + "ml")
      println("Sucre : " + sugarStocks(machineId) + "g")

      //ajout de stocks
      println("Ajout de stocks :\nEntrez la quantité de poudre de café à ajouter (g):")
      coffeeStocks(machineId) += readInt()
      println("Entrez la quantité de sucre à ajouter (g):")
      sugarStocks(machineId) += readInt()
      println("Entrez la quantité de lait à ajouter (ml):")
      milkStocks(machineId) += readInt()


      //affichage des stocks mis à jour
      println("Ajout :")
      println("Poudre de café : " + coffeeStocks(machineId))
      println("Lait : " + milkStocks(machineId))
      println("Sucre : " + sugarStocks(machineId))
      println("Niveau de stock mis à jour")
      println("Retour au menu principal\n")


    }

  }


}
