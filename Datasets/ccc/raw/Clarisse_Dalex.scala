import io.StdIn._
import math._
import scala.util.Random
import io.StdIn.readLine
import scala.reflect.internal.util.TriState.{False, True}
import java.io.{BufferedWriter, FileNotFoundException, FileWriter, IOException, PrintWriter,File}
import scala.io.Source
import collection.mutable.ArrayBuffer

//Definition de la Classe Machine
class Machine (var id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  //Pour sauvegarder machines dans fichier CSV
  def saveCSV: String = {
    id+","+pincode+","+{milk / 1000.0}+","+sugar+","+coffee
  }


  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "lait"| "milk" => milk += amount
      case "sucre"|"sugar" => sugar +=amount
      case "café" | "coffee" => coffee += amount
      case _ => println("Ingrédient non valide : " + ingredient )
    }

  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "lait"| "milk" if milk >= amount =>
        milk -= amount
        true

      case "sucre"|"sugar" if sugar >= amount =>
        sugar -= amount
        true

      case "café" | "coffee" if coffee >= amount =>
        coffee -= amount
        true
      case "milk" => println("Stock insuffisant de lait ")
        false

      case "sugar" => println("Stock insuffisant de sucre" )
        false

      case "coffee" => println("Stock insuffisant de café")
        false

      case _ => println("Ingredient non valide : " + ingredient)
        false

    }


  }



}




object Main {
  var machines: ArrayBuffer[Machine] = ArrayBuffer()
  //Méthode de gestion du CSV

  def loadcsv(filename:String): ArrayBuffer[Machine]= {


    try {
      val fr  = Source.fromFile(filename)

      try {
        val lines = fr.getLines().toList
        val dataLines= lines.drop(1) //Pour ignorer l'en-tête
        var idCounter = 1

        for (line<- dataLines){
          val data = line.split(",")
          if (data.length == 5){
            val machine = new Machine( data(0).toInt, data(1), (data(2).toDouble * 1000).toInt, data(3).toInt, data(4).toInt)
            machines+=machine
            println("\n\nMachine " + idCounter + " chargée:")
            println("ID :" + idCounter)
            println("Code PIN : " + machine.pincode)
            println("Lait : " + "%.2f L".format(machine.milk / 1000.0))
            println ("Sucre : " + machine.sugar + "g")
            println("Café : " + machine.coffee + "g")
            idCounter +=1
          }
          else {
            println("Ligne ignorée: " + line + " (Format incorrect)")
          }
        }
      }
      finally {
        fr.close()
      }
    } catch {
      case _: FileNotFoundException => println("Erreur : le fichier " + filename + " est introuvable."); sys.exit(1)
     case ex: Exception => println ("Erreur inatendue : " + ex.getMessage); sys.exit(1)
    }
    machines
    //retourner liste machines
  }

  //Pour sauvegarder machines dans fichier CSV
  def savecsv (filename:String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val pw = new PrintWriter (new File(filename))
      try {
        pw.println("ID, PINCODE, MILK, SUGAR, COFFEE")
        machines.foreach(machine => pw.println(machine.saveCSV))
      } finally {
        pw.close()
      }
    } catch {
      case ex: java.io.IOException => println("Erreur d'entrée/sortie : " + ex.getMessage)
      case ex: Exception => println("Erreur innatendue : " + ex.getMessage)
    }

  }


  //MÉTHODE
  def validatePin(machineId: Int, machinePins: Array[String], machines: ArrayBuffer[Machine]): Boolean = {

    //Faire en sorte que ce soit 434343 puis si ca doit chqnger ce sera l'updatePin qui sera le nouveau PIN.
    var maxessaie = 3
    for (i <- 1 to maxessaie) {
      println("\nEntrez le code PIN:")
      print(">")
      var codePIN = readLine()
      if (codePIN == machines(machineId).pincode) {
        println("\nAccès accordé à la machine " + (machineId + 1))
        return true
        //Code Pin correct
      }
      else {
        val essaierestant = maxessaie - i
        if (essaierestant > 0) {
          println("Code PIN incorrect. " + essaierestant + " tentatives restantes.")
        } else {
          println("Trop de tentatives échouées. Fin du programme. ")
          return false
        }
      }
    }
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String], machines: Machine): Unit = {
    var newPin = ""
    var valid = false
    while (!valid) {
      print("\n\nEntrez un nouveau code PIN à 6 chiffres >")
      newPin = readLine()

      //Vérifiions si les conditions sont réspectées soit: longueur 6 chiffres et Que des chiffres

      if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
        valid = true //Pin Valide
      } else if (newPin.isEmpty) {
        println("Erreur : Le code PIN ne peux pas être vide.")
        print(">")
        newPin = readLine()
        if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
          valid = true //Pin Valide
        }
      } else {
        println("Erreur : Le code PIN doit contenir 6 CHIFFRES ")
        print(">")
        newPin = readLine()
        if (newPin.length == 6 && newPin.forall(x => x >= '0' && x <= '9')) {
          valid = true //Pin Valide
        }
      }

      //Mise à jour du tableau avec le nouveau PIN


    }
    machinePins(machineId) = newPin
    machines.pincode = newPin

    println("Le nouveau code PIN a été mis a jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(5000)

  }

  def serveClient(machines: Machine): Boolean = {
    val expresso: Double = 2.00
    val Cappuccino: Double = 2.50
    val lattepetit: Double = 2.70
    val lattemoyen: Double = 3.20
    val lattegrand: Double = 3.70

    //tableau des stocks initiaux


    //variable different sucre prix
    var sanssucre: Double = 0.0
    var peusucre: Double = 0.10
    var moyensucre: Double = 0.20
    var beaucoupsucre: Double = 0.30

    //variable grammage sucre

    var sisucrepeu = 5
    var sisucremoyen = 10
    var sisucrebeaucoup = 15




    //Consommation des ingrédients par type de boisson: en gramme
    var consoexpresso = 8
    var consocappuccinocafe = 6
    var consocappuccinolait = 100 //en millilitre
    var consolattepetitcafe = 6
    var consolattepetitlait = 100 // en millilitre
    var consolattemoyencafe = 8
    var consolattemoyenlait = 150 //en millilitre
    var consolattegrandcafe = 12
    var consolattegrandlait = 200 // en millilitre
    var sucre = 0
    var encore = True

    //Pour avoir présentation à la fin correcte
    var boissonnom = ""
    var sucrenom = ""
    var laitnom = ""

    println("\n\nVeuillez sélectionner votre boisson:")
    println("1) Expresso - " + expresso + " CHF")
    println("2) Cappuccino -" + Cappuccino + "CHF")
    println("3) Latte - " + " Latte petit " + lattepetit + " CHF ")
    println("4) Latte - " + " Latte Moyen " + lattemoyen + " CHF ")
    println("5) Latte -" + " Latte Grand " + lattegrand + " CHF ")
    print(">")

    //variable choix boisson
    var boisson = readInt()
    var prixtotal = 0.0
    var prixboisson = 0.0
    var prixsucre = 0.0
    var laitconso = 0
    var sucreconso = 0
    var cafeconso = 0
    var prixdoselait = 0.0

    while (boisson != 1 && boisson != 2 && boisson != 3 && boisson != 4 && boisson != 5) {
      println("Boisson inexistante, veuillez réessayer: ")
      boisson = readInt()

    }

    if (boisson == 1) {
      boissonnom = "Expresso"
      println("\n\nBoisson séléctionnée: " + boissonnom)
      prixboisson = expresso
      cafeconso += consoexpresso

    }
    else if (boisson == 2) {
      boissonnom = "Cappuccino"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = Cappuccino
      cafeconso += consocappuccinocafe
      laitconso += consocappuccinolait

    }
    else if (boisson == 3) {
      boissonnom = "Latte petit"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattepetit
      cafeconso += consolattepetitcafe
      laitconso += consolattepetitlait

    }
    else if (boisson == 4) {
      boissonnom = "Latte moyen"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattemoyen
      cafeconso += consolattemoyencafe
      laitconso += consolattemoyenlait

    }
    else if (boisson == 5) {
      boissonnom = "Latte grand"
      println("Boisson séléctionné: " + boissonnom)
      prixboisson = lattegrand
      cafeconso += consolattegrandcafe
      laitconso += consolattegrandlait

    } else {
      println("Boisson n'existe pas, réessayer : ")
      boisson = readInt()
    }


    //pour rajouter du sucre
    if (boisson >= 1 && boisson <= 5) {
      println("\n\nSouhaitez-vous ajouter du sucre ?")
      println("1)Sans sucre")
      println("2)Peu (5g) -" + peusucre + "CHF")
      println("3)Moyen (10g) -" + moyensucre + "CHF")
      println("4)Beaucoup (15g)-" + beaucoupsucre + "CHF")
      print(">")
      var sucre = readInt()

      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        println("Veuillez choisir entre 1 à 4 ")
        sucre = readInt()

      }

      if (sucre == 1) {
        sucrenom = "Sans sucre"
        println("Niveau de sucre : " + sucrenom)
        prixsucre = sanssucre
      } else if (sucre == 2) {
        sucrenom = "Peusucre (5g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucrepeu
        prixsucre = peusucre
      } else if (sucre == 3) {
        sucrenom = "Moyen sucre (10g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucremoyen
        prixsucre = moyensucre


      } else if (sucre == 4) {
        sucrenom = "Beaucoup sucre (15g)"
        println("Niveau de sucre : " + sucrenom)
        sucreconso += sisucrebeaucoup
        prixsucre = beaucoupsucre

      }


      var choixlaitsupp = 0
      var dose = 4
      //rajouter du lait ou non dans cappucino et latte
      if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5) {
        println("\n\nSouhaitez-vous ajouter du lait en supplément ?")

        println("1) Oui")
        println("2) Non")
        print(">")
        choixlaitsupp = readInt()

        while (choixlaitsupp != 1 && choixlaitsupp != 2) {
          println("Merci d'entrer un choix valide : ")
          print(">")
          choixlaitsupp = readInt()
        }
        if (choixlaitsupp == 1) {
          laitnom = "Oui"
          println("Lait supplémentaire: " + laitnom)

          while (dose > 3) {
            println("Combien de dose ? (Maximum 3 doses)")
            dose = readInt()
            if (dose == 1 || dose == 2 || dose == 3) {
              println("Le nombre de dose choisi est : " + dose)
              laitconso += dose * 50 //COMMENT FAIRE
              prixdoselait = dose * 0.05 //COMMENT FAIRE
            }
          }
        }
        else {
          laitnom = "Non"
          println("\n\nLait supplémentaire : " + laitnom)
        }

      }


      //Résumé de commande
      println("\n\nBoisson Séléctionnée : " + boissonnom)
      println("Niveau de sucre : " + sucrenom)

      if (boisson == 2 || boisson == 3 || boisson == 4 || boisson == 5) {
        println("Lait en supplément : " + laitnom)
      }
      //faisons les stocks


      if (sucreconso > 0 && !machines.removeIngredient("sucre", sucreconso)) {
        println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre machine.")
        return false

      }
      if ( laitconso > 0 && !machines.removeIngredient("lait", laitconso)){
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre machine.")
        return false

      }
      if (!machines.removeIngredient("café" , sucreconso)) {
        println("Quantité de cafe insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou essayer une autre machine.")
        return false

      }



      else  {
        machines.removeIngredient("café", cafeconso)
        machines.removeIngredient("sucre", sucreconso)
        machines.removeIngredient("lait", laitconso)

        //Paiement

        prixtotal = prixboisson + prixsucre + prixdoselait
        //println("Prix Total :  CHF " + prixboisson+ " + CHF " + prixsucre + "= CHF " + prixtotal)
        printf("\n\nPrix Total :  CHF  %.2f + CHF %.2f + CHF %.2f  = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
        println("\nVeuillez payer en utilisant Twint.")
        val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        var code = ""
        for (i <- 1 to 5) {
          val randomChar = chars(Random.nextInt(chars.length))
          code += randomChar
        }

        println("Votre code de paiement est : " + code)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        println("\n\nMerci ! Votre paiement a été accepté")

        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre " + boissonnom + " est prêt ! Bonne dégustation !")
        //faisons les stocks
        Thread.sleep(5000)
      }
    }
    return false //PAS SURE


  }


  def restockMachine(machine: Machine): Unit = {



    //montrer les stocks avant réapprovisionnement
    println("\n\nNiveaux de stocks actuels : ")
    println("Poudre de café: " + machine.coffee + "g")
    // val laitenlitres= milkStocks(machineId)/1000             //de mL en L
    println("Lait          : " + "%.2f".format(machine.milk / 1000.0)+ "L") //Pas sur
    println("Sucre         :" + machine.sugar + "g")

    println("\n\nEntrez les quantité à ajouter : ")

    var acafe = -1
    var alait = -1
    var asucre = -1

    while (acafe < 0 || alait < 0 || asucre < 0) {

      println(f"Poudre de café: ")
      acafe = readInt
      println(f"Lait          : ")
      alait = (readDouble() * 1000).toInt //Voir comment faire pour lait
      println(f"Sucre          : ")
      asucre = readInt

      if (acafe < 0 || alait < 0 || asucre < 0) {
        println("Veuillez mettre des valeurs positives.")
      }


    }

    println("Les stocks ont été mis à jour avec succès...")
    machine.addIngredient("milk", acafe)
    machine.addIngredient("milk", alait)
    machine.addIngredient("sugar", asucre)
    println("Retour au menu principal...")
    Thread.sleep(5000)

  }


  def main(args: Array[String]): Unit = {


    val filename= "src/machines.csv"
    var machines = loadcsv(filename)
    var machinesPins = Array.fill(machines.length)("434343")


    //prix boisson
    val expresso: Double = 2.00
    val Cappuccino: Double = 2.50
    val lattepetit: Double = 2.70
    val lattemoyen: Double = 3.20
    val lattegrand: Double = 3.70


    var running = true
    while (running == true) {
      println("\n\nNospresso Café")
      println()
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print(">")

      val choix = readInt()

      if (choix == 1) {
        println("Mode Client")


          println("\nSélectionner la machine: (1 à 5)")
          print(">")
          var choixmachine = readInt()
         var  machineId = choixmachine - 1



        while (machineId < 0 || machineId >= machines.length) {
          println("Erreur : Numéro de machine invalide.")
          println("\n\nVeuillez rentrer un numéro de machine valide :")
          print(">")
          choixmachine = readInt()
          machineId = choixmachine - 1

        }

        if (machineId > 0 || machineId <= machines.length) {
          println("\n\nVous avez sélectionné la machine : " + choixmachine)
        }


        serveClient(machines(machineId))
        savecsv("machines.csv", machines)

      }
      if (choix == 2) {

        println("Mode Admin")
        println("\nSélectionner la machine: (1 à 5)")
        print(">")
       var  choixmachine = readInt()
        var machineId = choixmachine - 1

        while (machineId < 0 || machineId >= machines.length) {
          println("Erreur : Numéro de machine invalide.")
          println("\n\nVeuillez rentrer un numéro de machine valide :")
          print(">")
          choixmachine = readInt()
          machineId = choixmachine - 1

        }

        if (machineId > 0 || machineId <= machines.length) {
          println("\n\nVous avez sélectionné la machine : " + choixmachine)
        }


        if (validatePin(machineId, machinesPins, machines)) {


          println("\n\nQue Souhaitez-vous faire ? \n\n 1) Réapprovisonner les stocks \n\n 2) Faire une mise à jour du code PIN")
          print(">")
          var choixadmin = readInt()

          if (choixadmin == 1) {
            println("\n\nNiveaux de stocks actuels  de la machine " + choixmachine + " : ")
            // val laitenlitres= milkStocks(machineId)/1000             //de mL en L
            println("Lait          : " + "%.2f".format(machines(machineId).milk/ 1000.0)+ "L") //Pas sur
            println("Sucre         : " + machines(machineId).sugar  + "g")
            println("Poudre de café: " + machines(machineId).coffee + "g")

            println("\n\nEntrez les quantités à ajouter : ")


              println(f"Lait (ml) : ")
              println(f">")
              var laitamount = readInt()
              println(f"Sucre (g) : ")
              println(f">")
              var sucreamount = readInt()
              println(f"Café (g) : ")
              println(f">")
              var cafeamount = readInt()

              if (laitamount < 0 || sucreamount < 0 || cafeamount < 0) {
                println("Veuillez mettre des valeurs positives.")
              }

            machines(machineId).addIngredient("lait", laitamount)
            machines(machineId).addIngredient("sucre", sucreamount)
            machines(machineId).addIngredient("café", cafeamount)

            //sauvegarder les changements
            savecsv("machines.csv", machines)
            println("Les quantités ont été ajoutées et les données sauvegardées.")

          } else if (choixadmin == 2) {
           println("Mise à jour du code PIN pour la Machine " + choixmachine)
          updatePin(machineId, machinesPins, machines(machineId))
            savecsv("machines.csv", machines)
          }


          else {
            println("Choix invalide")
          }

        }
        else
          return


      }
      if (choix == 3) {
        running = false
        savecsv(filename, machines)
      }
      println("Vous avez quitté, Merci d'avoir utilisé Nospresso")


    }


  }


}

//FAIRE AJOUT DE STOCK PAR LA MAIN EST IL FAUT QUE CA SE RAJOUTE PAR MAIN ET RESTOCK EGALEMENT
//regler probleme de selection de machines
//revoir la code PIN
//revoir comment dans le serve client si ca retire vraiment les stocks
//revoir comment afficher les nouvelles données du fichier CSV