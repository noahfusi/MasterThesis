

import io.StdIn.{readLine,readInt}
import util.Random
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk: Double, var sugar: Double, var coffee: Double) {
  def addIngredient(ingredient: String, amount: Double): Unit = {
    val lowerCaseIngredient = ingredient.toLowerCase
    if (lowerCaseIngredient == "milk") {
      milk += amount
    } else if (lowerCaseIngredient == "sugar") {
      sugar += amount
    } else if (lowerCaseIngredient == "coffee") {
      coffee += amount
    } else {
      println(s"Unknown ingredient: $ingredient")
    }
  }

  def removeIngredient(ingredient: String, amount: Double): Boolean = {
    val lowerCaseIngredient = ingredient.toLowerCase

    if (lowerCaseIngredient == "milk" && milk >= amount) {
      milk -= amount
      true
    } else if (lowerCaseIngredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true
    } else if (lowerCaseIngredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true
    } else {
      println(s"Not enough $ingredient in stock.")
      false
    }
  }


  def validatePin(machineId : Int, machines: ArrayBuffer[Machine]) : Boolean = {
    var nbTentatives = 3
    var isPinOk = false
    println("Code PIN: ")
    while (nbTentatives > 0) {
      val codePIN = readLine("> ")

      if (codePIN == machines(machineId).pincode) {
        println("Accès accordé à la machine " + machineId.toString)
        return true
      } else {
        println("Code PIN invalide. " + (nbTentatives-1).toString + " tentatives restantes.")
        nbTentatives = nbTentatives - 1
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    System.exit(0)
    return isPinOk
  }

  def updatePin(machineId : Int, machines: ArrayBuffer[Machine]) : Boolean = {
    var inputPin = ""

    println("Mise à jour du code PIN pour la Machine " + machineId.toString + ".")
    while(inputPin.length != 6){
      inputPin = readLine("Entrez un nouveau code PIN à 6 chiffres >")
      if(inputPin.length != 6){
        println("Erreur de saisie.")
      }
    }
    machines(machineId).pincode = inputPin
    println("Le code PIN à été mis à jour avec succès.")
    println("Retour au menu principal...")
    return true
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Stock: Café = " +  machines(machineId).coffee)
    println("Stock: Sucre = " + machines(machineId).sugar)
    println("Stock: Lait = " + machines(machineId).milk)

    println("Vous-voulez ajouter des stocks ?\n1) Oui \n2) Non")

    val reponserestock = readLine ("> ").toInt
    if (reponserestock == 1) {
      println("Quantitée de poudre de café que vous ajouter?")
      machines(machineId).addIngredient("coffee", readLine("> ").toInt)
      println("Quantitée de sucre que vous ajouter?")
      machines(machineId).addIngredient("sugar", readLine("> ").toInt)
      println("Quantitée de lait que vous ajouter?")
      machines(machineId).addIngredient("milk", readLine("> ").toInt)
      println("Réapprovisionnement des stocks ... ")

      println("Stock mis à jour")
      println("Stock: Café = " + machines(machineId).coffee)
      println("Stock: Sucre = " + machines(machineId).sugar)
      println("Stock: Lait = " + machines(machineId).milk)
    } else {
      println("Annulation de restockage.")
    }
  }


  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    //Variables pour les prix et stocks (réaprovisionnement et vérification après choix de boisson)
    var prixBoisson = 0.0
    var cafeUtilise = 0.0
    var sucreUtilise = 0.0
    var laitUtilise = 0.0
    var prixSucre = 0.0
    var prixLait = 0.0

    var dosesLait = 0

    var choixBoisson = 0
    var quantiteSucre = 0
    var supplementLait = "Non"

    var serveSucessfull = false

    //Mode Client: choix de boisson (+ toute information concernant prix et stock)
    println("Veuillez sélectionner votre boisson : ")
    println("1) Expresso - CHF 2.00")
    println("2) Cappucino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")



    while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3){
      choixBoisson = readLine("> ").toInt
      if (choixBoisson == 1) {
        //Informations Expresso: 1 seule taille disponible
        cafeUtilise = 8.0
        prixBoisson = 2.0

        machines(machineId).removeIngredient("coffee",cafeUtilise)
        machines(machineId).removeIngredient("milk",laitUtilise)

        println("Expresso - CHF 2.00")
        serveSucessfull = true
      } else if (choixBoisson == 2) {
        //Informations Cappucino: 1 seule taille disponible + lait en supplément
        cafeUtilise = 6.0
        laitUtilise = 0.100
        prixBoisson = 2.5
        machines(machineId).removeIngredient("coffee",cafeUtilise)
        machines(machineId).removeIngredient("milk",laitUtilise)
        println("Cappucino - CHF 2.50")
        serveSucessfull = true

      } else if (choixBoisson == 3) {
        //Informations Latte: 3 tailles + lait en supplément
        println("Choisissez la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen -3.20\n3) Grand - 3.70")

        var tailleLatte = readLine("> ").toInt
        if (tailleLatte == 1) {
          //Petit latte
          cafeUtilise = 6.0
          laitUtilise = 0.120
          prixBoisson = 2.7

          machines(machineId).removeIngredient("coffee",cafeUtilise)
          machines(machineId).removeIngredient("milk",laitUtilise)

          println("Petit Latte - CHF 2.7")
          serveSucessfull = true

        } else if (tailleLatte == 2) {
          //Latte moyen
          cafeUtilise = 8.0
          laitUtilise = 0.150
          prixBoisson = 3.2

          machines(machineId).removeIngredient("coffee",cafeUtilise)
          machines(machineId).removeIngredient("milk",laitUtilise)
          machines(machineId).removeIngredient("sugar",sucreUtilise)
          println("Latte moyen - CHF 3.2")
          serveSucessfull = true

        } else if (tailleLatte == 3) {
          //Grand Latte
          cafeUtilise = 12.0
          laitUtilise = 0.200
          prixBoisson = 3.7
          machines(machineId).removeIngredient("coffee",cafeUtilise)
          machines(machineId).removeIngredient("milk",laitUtilise)
          println("Grand Latte - CHF 3.7")
          serveSucessfull = true

        } else {
          println("Erreur de saisie.")
        }
      } else println("Erreur de saisie.")
    }


    //Supplément sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) 5g (peu)- CHF 0.10")
    println("3) 10g (moyen) - CHF 0.20")
    println("4) 15g (beaucoup) - CHF 0.30")



    while (quantiteSucre != 1 && quantiteSucre != 2 && quantiteSucre != 3 && quantiteSucre != 4){
      quantiteSucre = readLine("> ").toInt
      if (quantiteSucre == 1) {
        //Sans sucre
        serveSucessfull = true
      } else if (quantiteSucre == 2) {
        //Ajout de 5g de supplément de sucre
        sucreUtilise = 5.0
        machines(machineId).removeIngredient("sugar",sucreUtilise)
        prixSucre = 0.10
        serveSucessfull = true
      } else if (quantiteSucre == 3) {
        //Ajout de 10g de supplément de sucre
        sucreUtilise = 10.0
        machines(machineId).removeIngredient("sugar",sucreUtilise)
        prixSucre = 0.2
        serveSucessfull = true
      } else if (quantiteSucre == 4) {
        //Ajout de 15g de supplément de sucre
        sucreUtilise = 15.0
        machines(machineId).removeIngredient("sugar",sucreUtilise)
        prixSucre = 0.3
        serveSucessfull = true

      }else println("Erreur de saisie.")

    }


    if (choixBoisson == 2 || choixBoisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ? \n (Disponible seulement pour Cappuccino et Latte)")
      println("1) Oui\n2) Non")

      while (supplementLait != "1" && supplementLait != "2"){
        supplementLait = readLine("> ")
        if (supplementLait == "1") {
          //Si il veut du lait supplémentaire, combien de doses ?
          println("Combien de dose (1 dose = 50 mL) ?\n1) 1\n2) 2 \n3) 3 ")
          while (dosesLait != 1 && dosesLait != 2 && dosesLait != 3){
            dosesLait = readLine("> ").toInt
            if ((dosesLait == 1) || (dosesLait == 2) || (dosesLait == 3)) {
              //calcul général pour calcul de la quantité de lait nécessaire, pour dédudction des stocks
              if (dosesLait ==1) {
                machines(machineId).removeIngredient("milk",0.05)
                prixLait += 0.05
                serveSucessfull = true
              } else if (dosesLait ==2) {
                machines(machineId).removeIngredient("milk",0.100)
                prixLait += 0.10
                serveSucessfull = true

              } else if (dosesLait ==3) {
                machines(machineId).removeIngredient("milk",0.150)
                prixLait += 0.15
                serveSucessfull = true
              }
            } else println("Erreur de saisie.")
          }
        } else println("Erreur de saisie.")
      }
    }

    // Vérifier les stock avant de passer à la préparation de la boisson.
    if (cafeUtilise > machines(machineId).coffee || sucreUtilise > machines(machineId).sugar || laitUtilise > machines(machineId).milk) {
      if (cafeUtilise > machines(machineId).coffee) {
        println("Stock insuffisant de café.")
        serveSucessfull = false
      } else if (sucreUtilise > machines(machineId).sugar) {
        println("Stock insuffisant de sucre.")
        serveSucessfull = false
      } else if (laitUtilise > machines(machineId).milk) {
        println("Stock insuffisant de lait.")
        serveSucessfull = false
      }
      println("Veuillez choisir une autre boisson ou une autre quantitée!")

    } else {
      //Le stock est suffisant, on prépare la boisson

      // Affichage du résumé de la commande
      println("\nRésumé de votre commande :")
      println("Boisson sélectionnée : " + (if (choixBoisson == 1) "Expresso" else if (choixBoisson == 2) "Cappuccino" else "Latte"))
      println("Niveau de sucre : " + {
        if (quantiteSucre == 1) "Sans sucre"
        else if (quantiteSucre == 2) "Peu (5g)"
        else if (quantiteSucre == 3) "Moyen (10g)"
        else if (quantiteSucre == 4) "Beaucoup (15g)"
        else "Erreur"
      })

      println("Doses de lait : " + {
        if (dosesLait == 1) "Une dose de lait"
        else if (dosesLait == 2) "Deux doses de lait"
        else if (dosesLait == 3) "Moyen (10g)"
        else "Erreur"
      })

      if (dosesLait == 1) dosesLait * 5.0/100.0
      else if (dosesLait == 2) dosesLait * 5.0/100.0
      else if (dosesLait == 3) dosesLait * 5.0/100.0


      // Affichage du prix total
      val prixtotal = prixBoisson + prixSucre + prixLait
      printf("%.2f + %.2f + %.2f = %.2f \n", prixBoisson, prixSucre, prixLait, prixtotal)

      //Paiement par TWINT
      println("Vous allez être redirigé pour le paiement.")
      println("Veuillez payer en utilisant TWINT.")
      val codeTWINT = Random.alphanumeric.take(5).mkString
      println("Votre code de paiement est: " + codeTWINT)

      println("Validation du paiement ... ")
      Thread.sleep(5000) // Attend pendant 5000 millisecondes

      println("Paiement confirmé.")
      println("Préparation de la boisson ... ")
      println("MERCI!")

      choixBoisson = 0

    }
    return serveSucessfull
  }

}



object Main {
  var machineId = -1
  def main(args: Array[String]): Unit = {
    var ProgrammeActif: Boolean = true
    var machines: ArrayBuffer[Machine] = ArrayBuffer()

    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machine = ArrayBuffer[Machine]()
      try {
        println("Chargement des machines depuis machines.csv...")
        val lines = Source.fromFile(filename).getLines()
        lines.next() // Skip header
        for ((line, index) <- lines.zipWithIndex) {
          val Array(pin, milk, sugar, coffee) = line.split(",")
          machine += new Machine(index + 1, pin, milk.toDouble, sugar.toDouble, coffee.toDouble)
          println(s"Machine ${index + 1} chargée :")
          println("ID: " + machine.last.id + ", Code PIN: " + machine.last.pincode + ", Lait: " + machine.last.milk + "L, Surcre:  " + machine.last.sugar + "g, Café: " + machine.last.coffee + "g")

        }
        println(s"${machine.size} machine(s) chargée(s) avec succès.")
      } catch {
        case _: Exception =>
          println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
          System.exit(1)
      }
      machine
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      try {
        val file  = new PrintWriter(new File(filename))
        file .println("PINCODE,MILK,SUGAR,COFFEE")
        machines.foreach { machine =>
          file .println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
        }
        file .close()
        println(s"Sauvegarde de ${machines.size} machine(s) dans $filename réussie.")
      } catch {
        case _: Exception =>
          println("Erreur : Échec de l’écriture dans le fichier. Le fichier peut être verrouillé ou en lecture seule.\nErreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
          System.exit(1)
      }
    }

    machines = loadcsv("machines.csv")
    var nbMachines = machines.length

    // Function to select the id of the machine wanted
    def selectMachineId(): Int = {
      machineId = -1
      while (machineId <= 0  || machineId > nbMachines){
        machineId = readLine("Machine sélectionnée (1-" + (nbMachines).toString + ") >").toInt
        if(machineId <= 0 || machineId > nbMachines){
          println("Erreur de saisie. (Mauvaise identifiant.)")
        }
      }
      machineId = machineId - 1
      machines(machineId).validatePin(machineId,machines)
      return machineId
    }


    while (ProgrammeActif == true) {
      //Menu principal, choix du mode
      println("\nNospresso Café ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client\n2) Admin \n3) Quitter")
      var choixMode = readLine("> ").toInt
      if (choixMode == 1) {
        machineId = selectMachineId()

        if(machines(machineId).serveClient(machineId, machines)){
          println("Service OK")
        }else{
          println("Service NOT OK")
          savecsv("machines.csv",machines)
        }
      } else if (choixMode == 2 ) {
        var choixAdmin = -1
        println("1) Update PIN\n2) Restock")

        while (choixAdmin < 0 || choixAdmin > nbMachines - 1){
          choixAdmin = readLine("> ").toInt
          if(choixAdmin < 0 || choixAdmin > nbMachines - 1){
            println("Erreur de saisie.")
          }
        }

        if(choixAdmin == 1){
          machineId = selectMachineId()
          if(machines(machineId).updatePin(machineId, machines)){
            println("Update PIN OK")
          }else{
            println("Update PIN NOT OK")
            savecsv("machines.csv",machines)
          }
        }else if(choixAdmin == 2) {
          machineId = selectMachineId()
          machines(machineId).restockMachine(machineId, machines)
        }
      }else if (choixMode == 3) {
        ProgrammeActif = false
        println("Merci d'avoir utilisé Nospresso.")
        savecsv("machines.csv",machines)
      } else {
        println("Erreur de saisie.")
      }
    }
  }
}