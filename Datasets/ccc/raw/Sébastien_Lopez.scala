
import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random

object Main {

  def serveClient(machine: Machine): Boolean = {

    println("Souhaitez-vous retirer une quantitée d'un ingrédient ?")
    println(" 1) Oui")
    println(" 2) Non ")
    var elecion = readLine(" >").toInt

    while ((elecion < 1) || (elecion > 2)) {
      println(" Souhaitez-vous retirer une quantitée d'un ingrédient ? ")
      println(" 1) Oui ")
      println(" 2) Non ")
      elecion = readLine(" >").toInt
    }
    if (elecion == 1) {
      println(" A présent, veuillez choisir quel ingrédient sera retiré. ")
      println(" Le lait, le sucre ou le café ?")
      var ingredient = readLine(" >")

      while (ingredient != "lait" & ingredient != "sucre" & ingredient != "café") {
        println(" Le lait, le sucre ou le café ?")
        ingredient = readLine(" >")
      }

      var amount = 0
      if (ingredient == "lait" ){
        println(" Quelle quantitée souhaitez-vous retirer ?")
        amount = (readLine(" >").toDouble * 1000).toInt
        while (amount < 0) {
          println(" Quelle quantitée souhaitez-vous retirer ?")
          amount = (readLine(" >").toDouble * 1000).toInt
        }
      }
      else {
        println(" Quelle quantitée souhaitez-vous retirer ?")
        amount = readLine(" >").toInt
        while (amount < 0) {
          println(" Quelle quantitée souhaitez-vous retirer ?")
          amount = readLine(" >").toInt
        }
      }
      machine.removeIngredient(ingredient, amount)

      while (elecion == 1) {
        println(" Souhaitez vous retirer un autre ingrédient ?")
        println(" 1) Oui")
        println(" 2) Non ")
        elecion = readLine(" >").toInt

        while ((elecion < 1) || (elecion > 2)) {
          println(" Souhaitez-vous retirer une quantitée d'un ingrédient ? ")
          println(" 1) Oui ")
          println(" 2) Non ")
          elecion = readLine(" >").toInt
        }

        if (elecion == 1) {
          println(" Le lait, le sucre ou le café ?")
          var ingredient = readLine(" >")

          while (ingredient != "lait" & ingredient != "sucre" & ingredient != "café") {
            println(" Le lait, le sucre ou le café ?")
            ingredient = readLine(" >")
          }

          if (ingredient == "lait" ){
            println(" Quelle quantitée souhaitez-vous retirer ?")
            amount = (readLine(" >").toDouble * 1000).toInt
            while (amount < 0) {
              println(" Quelle quantitée souhaitez-vous retirer ?")
              amount = (readLine(" >").toDouble * 1000).toInt
            }
          }
          else {
            println(" Quelle quantitée souhaitez-vous retirer ?")
            amount = readLine(" >").toInt
            while (amount < 0) {
              println(" Quelle quantitée souhaitez-vous retirer ?")
              amount = readLine(" >").toInt
            }
          }
          machine.removeIngredient(ingredient, amount)
        }
      }
    }


    if (machine.coffee < 0) {
      println
      println(" Erreur = Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une taille plus petite ou essayer une autre boisson")
      println
    }
    if (machine.milk < 0) {
      println
      println(" Erreur = Quantité de lait est insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      println
    }
    if (machine.sugar < 0) {
      println
      println(" Erreur = Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println(" Veuillez choisir une autre boisson, une quantité plus faible de sucre ou alors vérifier les stocks en mode Admin.")
      println
    }


    if (machine.coffee < 0 || machine.sugar < 0 || machine.milk < 0) {
      return false
    } else {
      return true
    }
  }

  def validatePin(machine: Machine): Boolean = {
    var nbTentatives: Int = 1
    var tentatives_restantes: Int = 2
    val code_Pin = machine.pincode
    var code = readLine(" >")
    var Pin: Boolean = false
    while ((code != code_Pin) && (nbTentatives < 3)) {
      println(" Code Pin incorrect. " + tentatives_restantes + " tentatives restantes.")
      code = readLine(" >")
      nbTentatives += 1
      tentatives_restantes -= 1
      if (tentatives_restantes == 0) {
        Pin = false
      }
    }
    if (code == code_Pin) {
      Pin = true
    }
    return Pin

  }


  def updatePin(machine: Machine): Unit = {
    var NouveaucodePin = readLine(" >")

    while ((NouveaucodePin.length != 6) || (!NouveaucodePin.forall(_.isDigit))) {
      println(" Entrez un nouveau code Pin à 6 chiffres :")
      NouveaucodePin = readLine(" >")
    }

    machine.pincode = NouveaucodePin

  }

  def restockMachine(machine: Machine): Unit = {

    println(" Souahitez-vous ajouter une quantitée d'un ingrédient dans la Machine ")
    println(" 1) Oui ")
    println(" 2) Non ")

    var choice = readLine (" >").toInt
    while ((choice < 1) || (choice > 2)) {
      println(" Souahitez-vous ajouter une quantitée d'un ingrédient dans la Machine ")
      println(" 1) Oui ")
      println(" 2) Non ")
      choice = readLine(" >").toInt
    }

    if (choice == 1){
      println(" A présent, veuillez choisir quel ingrédient sera ajouté ")
      println(" Le lait, le sucre ou le café ?")
      var ingredient = readLine(" >")
      while (ingredient != "lait" & ingredient != "sucre" & ingredient != "café") {
        println(" Le lait, le sucre ou le café ?")
        ingredient = readLine(" >")
      }

      var amount = 0
      if (ingredient == "lait" ){
        println(" Quelle quantitée souhaitez-vous ajouter ?")
        amount = (readLine(" >").toDouble * 1000).toInt
        while (amount < 0) {
          println(" Quelle quantitée souhaitez-vous ajouter ?")
          amount = (readLine(" >").toDouble * 1000).toInt
        }
      }
      else {
        println(" Quelle quantitée souhaitez-vous ajouter ?")
        amount = readLine(" >").toInt
        while (amount < 0) {
          println(" Quelle quantitée souhaitez-vous ajouter ?")
          amount = readLine(" >").toInt
        }
      }
      machine.addingredient(ingredient, amount)

      while (choice == 1) {
        println(" Souhaitez vous ajouter un autre ingrédient ?")
        println(" 1) Oui")
        println(" 2) Non ")
        choice = readLine(" >").toInt

        while ((choice < 1) || (choice > 2)) {
          println(" Souhaitez-vous ajouter un autre ingrédient ? ")
          println(" 1) Oui ")
          println(" 2) Non ")
          choice = readLine(" >").toInt
        }

        if (choice == 1) {
          println(" Le lait, le sucre ou le café ?")
          var ingredient = readLine(" >")
          while (ingredient != "lait" & ingredient != "sucre" & ingredient != "café") {
            println(" Le lait, le sucre ou le café ?")
            ingredient = readLine(" >")
          }

          if (ingredient == "lait" ){
            println(" Quelle quantitée souhaitez-vous ajouter ?")
            amount = (readLine(" >").toDouble * 1000).toInt
            while (amount < 0) {
              println(" Quelle quantitée souhaitez-vous ajouter ?")
              amount = (readLine(" >").toDouble * 1000).toInt
            }
          }
          else {
            println(" Quelle quantitée souhaitez-vous ajouter ?")
            amount = readLine(" >").toInt
            while (amount < 0) {
              println(" Quelle quantitée souhaitez-vous ajouter ?")
              amount = readLine(" >").toInt
            }
          }
          machine.addingredient(ingredient, amount)
        }
      }
    }
  }

  class Machine (val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int){
    val machineId = id
    var Pin = pincode
    var lait = milk
    var sucre = sugar
    var cafe = coffee


    def addingredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "lait") {
        lait = lait + amount
      }
      if (ingredient == "sucre") {
        sucre = sucre + amount
      }
      if (ingredient == "café") {
        coffee = coffee + amount
      }

    }

    def removeIngredient(ingredient: String, amount: Int ) : Boolean = {
      var good = true
      if (ingredient == "lait") {
        lait = lait - amount
        if (lait < 0) {
          good = false
          lait = lait + amount
        }
      }
      if (ingredient == "sucre") {
        sucre = sucre - amount
        if (sucre < 0) {
          good = false
          sucre = sucre + amount
        }
      }
      if (ingredient == "café") {
        coffee = coffee - amount
        if (cafe < 0) {
          good = false
          coffee = coffee + amount
        }
      }
      if ( good == false ) {
        println(" La quantitée ne peut pas être retirée car votre demande est supérieure à celle de la machine.")

      }
      return good
    }

  }




  def loadcsv (filename : String) : ArrayBuffer[Machine] = {
    val filename = "machines.csv"
    println
    println("Chargement des machines depuis " + filename + "...")
    val fr = Source.fromFile(filename)
    val lignefr = fr.reset.getLines().drop(1)// ignorer la première ligne car c'est un en-tête
    val allMachines = new ArrayBuffer[Machine]

    //println(allMachines)

    try {
      var ID: Int = 0
      while (!lignefr.isEmpty) {
        val ligne = lignefr.next
        val machine = ligne.split(",")
        val pin = machine(0)
        val leche = machine(1).toInt
        val azucar = machine(2).toInt
        val cafe = machine(3).toInt
        allMachines += new Machine(ID, pin, leche, azucar, cafe)

        ID += 1
      }
      fr.close()
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
      case ex: java.io.IOException => println("Erreur d'entrée du fichier. Vérifiez de nouveau et réessayez.")
      case ex: java.nio.file.AccessDeniedException => println("Erreur : Votre demande est une opération non autorisée dans le syst. de fichiers")
    }
    return allMachines

  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]) : Unit = {
    val filemame = "machines.csv"
    val printWriter =  new PrintWriter(new FileWriter(filename, false))
    try {
      val filemame = "machines.csv"
      val printWriter =  new PrintWriter(new FileWriter(filename, false))
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for(machine <- machines) {
        val line = machine.pincode + "," + machine.lait + "," + machine.sucre + "," + machine.coffee
        printWriter.println(line)
      }
      printWriter.close
    }
    catch {
      case ex : java.io.IOException => println("  Erreur : Echec du chargement ou de la sauvegarde des machines. \n, Le fichier peut être vérouillé ou en lecture seule")
      case ex : java.nio.file.AccessDeniedException => println (" Erreur : Echec du chargement ou de la sauvegarde des machines. \n, Fermeture du porgramme." )
    }
    println
    println(" Fichier sauvegardé avec succès.")
  }

  def affichage (machines : ArrayBuffer[Machine] ) : Unit = {
    for (i <- 0 to machines.length-1) {
      var machine = machines(i)
      println
      println("Machine " + (machine.id +1 ) + " chargée : ")
      println("Code PIN : " + machine.pincode)
      println("Lait : " + machine.lait)
      println("Sucre : " + machine.sucre)
      println("Café : " + machine.coffee)
      println (machine.machineId+1 + " machine(s) chargée(s) avec succès.")
    }
  }


  def main(args: Array[String]): Unit = {


    var Num_Machine: Int = 0
    var mode: Int = 0
    var boisson: Int = 0
    var sucre: Int = 0
    var Latte: Int = 0
    var lait: Int = 0
    var dose: Int = 0

    var prix_lait: Double = 0.00
    var prix_sucre: Double = 0.00
    var prix_Latte: Double = 0.00
    var prix_Expresso: Double = 2.00
    var prix_Cappuccino: Double = 2.50

    var boisson_selectionnee: String = "."
    var niveaudesucre: String = "."
    var laitsupp: String = "."

    var deductionStock_lait: Int = 0
    var deductionStock_sucre: Int = 0
    var deductionStock_laitsupp: Int = 0
    var deductionStock_cafe: Int = 0

    var Stock_valide: Boolean = true
    val s = 17

    var allMachines = loadcsv("machines.csv")
    var machine = allMachines(Num_Machine)
    affichage(allMachines)



    do {



      println("         Nospresso Café")
      println(" Veuilez sélectionner votre mode :")
      println(" 1) Client")
      println(" 2) Admin")
      println(" 3) Quitter")
      mode = readLine(" >").toInt
      println
      while ((mode < 1) || (mode > 3)) {
        if ((mode < 1) || (mode > 3)) {
          println(" Veuilez sélectionner votre mode :")
          println(" 1) Client")
          println(" 2) Admin")
          println(" 3) Quitter")
          mode = readLine(" >").toInt
          println
        }
      }

      do {
        if ((mode == 1) || (mode == 2)) {
          println(" Veuillez sélectionner la machine :")
          println(" 1) Machine n°1")
          println(" 2) Machine n°2")
          println(" 3) Machine n°3")
          println(" 4) Machine n°4")
          println(" 5) Machine n°5")
          Num_Machine = readLine(" Machine sélectionnée (1-5) >").toInt
          println

          while ((Num_Machine < 1) || (Num_Machine > 5)) {
            if ((Num_Machine < 1) || (Num_Machine > 5)) {
              println(" Veuillez sélectionner la machine :")
              println(" 1) Machine n°1")
              println(" 2) Machine n°2")
              println(" 3) Machine n°3")
              println(" 4) Machine n°4")
              println(" 5) Machine n°5")
              Num_Machine = readLine(" Machine sélectionnée (1-5) >").toInt
              println
            }
          }
          machine = allMachines(Num_Machine-1)
        }


        if (mode == 1) {
          println(" Veuillez sélectionner votre boissson :")
          println(" 1)Expresso - CHF 2.00")
          println(" 2)Cappuccino - CHF 2.50")
          println(" 3)Latte - CHF 2.70(Petit), CHF 3.20(Moyen), CHF 3.70(Grand)")
          boisson = readLine(" >").toInt
          println

          while ((boisson < 1) || (boisson > 3)) {
            if ((boisson < 1) || (boisson > 3)) {
              println(" Veuillez sélectionner votre boissson :")
              println(" 1)Expresso - CHF 2.00")
              println(" 2)Cappuccino - CHF 2.50")
              println(" 3)Latte - CHF 2.70(Petit), CHF 3.20(Moyen), CHF 3.70(Grand)")
              boisson = readLine(" >").toInt
              println
            }
          }


          if (boisson == 3) {
            println("Veuillez sélectionner votre Latte :")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            Latte = readLine(" >").toInt
            println

            while ((Latte < 1) || (Latte > 3)) {
              println("Veuillez sélectionner votre Latte :")
              println("1) Petit - CHF 2.70")
              println("2) Moyen - CHF 3.20")
              println("3) Grand - CHF 3.70")
              Latte = readLine(" >").toInt
              println
            }
            if (Latte == 1) {
              prix_Latte = 2.70
            }
            if (Latte == 2) {
              prix_Latte = 3.20
            }
            if (Latte == 3) {
              prix_Latte = 3.70
            }
          }


          println(" Souhaitez-vous ajouter du sucre ?")
          println(" 1) Sans sucre")
          println(" 2) Peu (5g) - CHF 0.10")
          println(" 3) Moyen (10g) - CHF 0.20")
          println(" 4) Beaucoup (15g) - CHF 0.30")
          sucre = readLine(" >").toInt
          println

          while ((sucre < 1) || (sucre > 4)) {
            println(" Souhaitez-vous ajouter du sucre ?")
            println(" 1) Sans sucre")
            println(" 2) Peu (5g) - CHF 0.10")
            println(" 3) Moyen (10g) - CHF 0.20")
            println(" 4) Beaucoup (15g) - CHF 0.30")
            sucre = readLine(" >").toInt
            println
          }
          if (sucre == 1) {
            prix_sucre = 0.00
          }
          if (sucre == 2) {
            prix_sucre = 0.10
          }
          if (sucre == 3) {
            prix_sucre = 0.20
          }
          if (sucre == 4) {
            prix_sucre = 0.30
          }


          if ((boisson == 2) || (boisson == 3)) {

            println(" Souhaitez-vous ajouter du lait en supplément ?")
            println(" (Disponible uniquement pour Cappuccino et Latte)")
            println(" 1) Oui")
            println(" 2) Non")
            lait = readLine(" >").toInt
            println

            while ((lait < 1) || (lait > 2)) {
              println(" Souhaitez-vous ajouter du lait en supplément ?")
              println(" (Disponible uniquement pour Cappuccino et Latte)")
              println(" 1) Oui")
              println(" 2) Non")
              lait = readLine(" >").toInt
              println
            }


            if (lait == 1) {
              println(" Combien de dose ? (entre 1 et 3)")
              dose = readLine(" >").toInt
              println

              while ((dose < 1) || (dose > 3)) {
                println(" Combien de dose ? (entre 1 et 3)")
                dose = readLine(" >").toInt
                println
              }
              if (dose == 1) {
                prix_lait = 0.05
              }
              if (dose == 2) {
                prix_lait = 0.10
              }
              if (dose == 3) {
                prix_lait = 0.15
              }
            }

            if (lait == 2) {
              dose = 0
              if (dose == 0) {
                prix_lait = 0.00
              }
            }
          }


          if (boisson == 1) {
            boisson_selectionnee = "Expresso"
          }
          if (boisson == 2) {
            boisson_selectionnee = "Cappuccino"
          }
          if (boisson == 3) {
            if (Latte == 1) {
              boisson_selectionnee = "Latte (Petit)"
            }
            if (Latte == 2) {
              boisson_selectionnee = "Latte (Moyen)"
            }
            if (Latte == 3) {
              boisson_selectionnee = "Latte (Grand)"
            }
          }

          if (sucre == 1) {
            niveaudesucre = "Sans sucre"
          }
          if (sucre == 2) {
            niveaudesucre = "Peu (5g)"
          }
          if (sucre == 3) {
            niveaudesucre = "Moyen (10g)"
          }
          if (sucre == 4) {
            niveaudesucre = "Beaucoup (15g)"
          }

          if (lait == 1) {
            laitsupp = "Oui"
          }
          if (lait == 2) {
            laitsupp = "Non"
          }


          println(" Boisson sélectionnée : " + boisson_selectionnee)
          println(" Niveau de sucre : " + niveaudesucre)
          if ((boisson == 2) || (boisson == 3)) {
            println(" Lait supplémentaire : " + laitsupp)
          }

          if (boisson == 1) {
            deductionStock_cafe = 8
            deductionStock_lait = 0
          }

          if (boisson == 2) {
            deductionStock_cafe = 6
            deductionStock_lait = 100
          }

          if (boisson == 3) {
            if (Latte == 1) {
              deductionStock_cafe = 6
              deductionStock_lait = 120
            }
            if (Latte == 2) {
              deductionStock_cafe = 8
              deductionStock_lait = 150

            }
            if (Latte == 3) {
              deductionStock_cafe = 12
              deductionStock_lait = 200
            }
          }

          if ((boisson == 1) || (boisson == 2) || (boisson == 3)) {
            if (niveaudesucre == "Sans sucre") {
              deductionStock_sucre = 0
            }
            if (niveaudesucre == "Peu (5g)") {
              deductionStock_sucre = 5
            }
            if (niveaudesucre == "Moyen (10g)") {
              deductionStock_sucre = 10
            }
            if (niveaudesucre == "Beaucoup (15g)") {
              deductionStock_sucre = 15
            }
          }

          if (boisson == 1) {
            deductionStock_laitsupp = 0
          }
          if ((boisson == 2) || (boisson == 3)) {
            if (lait == 2) {
              deductionStock_laitsupp = 0
            }
            if (lait == 1) {
              if (dose == 1) {
                deductionStock_laitsupp = 50
              }
              if (dose == 2) {
                deductionStock_laitsupp = 100
              }
              if (dose == 3) {
                deductionStock_laitsupp = 150
              }
            }
          }


          machine.coffee = machine.coffee - deductionStock_cafe
          machine.lait = machine.lait - deductionStock_lait - deductionStock_laitsupp
          machine.sucre = machine.sucre - deductionStock_sucre


          Stock_valide = serveClient(machine)

          if (Stock_valide == true) {
            (machine.coffee >= 0) || (machine.lait>= 0) || (machine.sucre >= 0)
          } else {
            (machine.coffee < 0) || (machine.lait< 0) || (machine.sucre < 0)
            machine.coffee = machine.coffee + deductionStock_cafe
            machine.lait = machine.lait + deductionStock_lait + deductionStock_laitsupp
            machine.sucre = machine.sucre + deductionStock_sucre
          }


          if (Stock_valide == true) {

            if (boisson == 1) {
              prix_Expresso = prix_Expresso + prix_sucre
            }
            if (boisson == 2) {
              prix_Cappuccino = prix_Cappuccino + prix_sucre + prix_lait
            }
            if (boisson == 3) {
              prix_Latte = prix_Latte + prix_sucre + prix_lait
            }


            if (boisson == 1) {
              if (sucre == 1) {
                printf(" Prix total : CHF 2.00 + CHF 0.00 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 2) {
                printf(" Prix total : CHF 2.00 + CHF 0.10 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 3) {
                printf(" Prix total : CHF 2.00 + CHF 0.20 = CHF %.2f \n", prix_Expresso)
              }
              if (sucre == 4) {
                printf(" Prix total : CHF 2.00 + CHF 0.30 = CHF %.2f \n", prix_Expresso)
              }
            }
            if (boisson == 2) {
              if ((sucre == 1) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 1) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 2) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 3) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 0)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 1)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 2)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Cappuccino)
              }
              if ((sucre == 4) && (dose == 3)) {
                printf(" Prix total : CHF 2.50 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Cappuccino)
              }
            }
            if (boisson == 3) {
              if (Latte == 1) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 2.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
              if (Latte == 2) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.20 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }

              }
              if (Latte == 3) {
                if ((sucre == 1) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 1) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.00 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 2) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.10 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 3) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.20 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 0)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.00 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 1)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.05 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 2)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.10 = CHF %.2f \n", prix_Latte)
                }
                if ((sucre == 4) && (dose == 3)) {
                  printf(" Prix total : CHF 3.70 + CHF 0.30 + CHF 0.15 = CHF %.2f \n", prix_Latte)
                }
              }
            }

            println

            if (boisson == 1) {
              prix_Expresso = prix_Expresso - prix_sucre
            }
            if (boisson == 2) {
              prix_Cappuccino = prix_Cappuccino - prix_sucre
              prix_Cappuccino = prix_Cappuccino - prix_lait
            }
            if (boisson == 3) {
              prix_Latte = prix_Latte - prix_sucre
              prix_Latte = prix_Latte - prix_lait
            }


            val code_paiement = Random.alphanumeric.take(5).mkString
            println(" Veuillez payer en utilisant Twint.")
            println(" Votre code de paiement est : " + code_paiement)
            println(" (En attente de validation du paiement...)")
            Thread.sleep(3000)
            println
            println(" Merci! Votre paiement a été accepté.")
            println(" Préparation de votre boisson... ")
            println(" Votre " + boisson_selectionnee + " est prêt ! Bonne dégustation !")
            println
            println

          }
        }


        if (mode == 2) {
          println(" Mode Admin")
          println(" Entrez le code Pin :")

          val OuvertureAdmin = validatePin(machine)
          if (OuvertureAdmin == true) {

            println(" Accès accordé à la Machine " + Num_Machine + ".")
            println
            println(" Choississez une de ces deux options : ")
            println(" Option n°1 = Changez le code administrateur de la machine ")
            println(" Option n°2 = Visualiser les stocks + possibilité de modifier les ingrédients")
            var choix = readLine(" >").toInt

            while ((choix < 1) || (choix > 2)) {
              println(" Choississez une de ces deux options : ")
              println(" Option n°1 = Changez le code administrateur de la machine ")
              println(" Option n°2 = Visualiser les stocks + possibilité de  modifier les ingrédients")
              choix = readLine(" >").toInt
            }

            if (choix == 1) {
              println
              println(" Mise à jour du code Pin pour la machine " + Num_Machine)
              println(" Entrez un nouveau code Pin à 6 chiffres :")
              updatePin(machine)
              println
              println(" Le code PIN a été mis à jour avec succès.")
              println(" Retour au menu principal...")
              println
            }

            if (choix == 2) {

              println(" Niveaux de stocks actuels :")
              println("    Poudre de café : " + machine.coffee + "g")
              printf("    Lait : %.2fL \n", machine.lait / 1000.0)
              println("    Sucre : " + machine.sucre + "g")
              println
              restockMachine(machine)
              println
              println(" Les stock ont été mis à jour avec succès.")
              println(" Retour au menu principal...")
              println
            }


          } else {
            println(" Code Pin incorrect. 0 tentatives restantes.")
            println
            println(" Trop de tentatives échouées. Fin du programme.")
            println(" Retour au menu principal...")
            println
          }
        }

        if (mode == 3) {
          println(" Vous avez quitté le programme Nospresso Café.")
        }

      } while (Stock_valide == false)

    } while ((s != 6) && (mode != 3))

    savecsv("machines.csv", allMachines)
  }
}
