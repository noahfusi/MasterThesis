
object Main {
  import scala.io.StdIn._
  import scala.util._
  import scala.io.Source
  import scala.collection.mutable.ArrayBuffer
  import scala.language.postfixOps
  import java.io.PrintWriter

  def main(args: Array[String]): Unit = {
    class Machine(id: Int, pinCode: String, milk: Int, sugar: Int, coffee: Int){
      var idMachine:Int = id
      var pinCodeMachine: String = pinCode
      var milkMachine: Int = milk
      var sugarMachine: Int = sugar
      var coffeeMachine: Int = coffee
      //println(id + ": " + pinCodeMachine + ", " + milkMachine + ", " + sugarMachine + ", " + coffeeMachine)
      def addIngredient(ingredient: String, amount: Int): Unit = {
        if (ingredient=="milk"){
          milkMachine += amount
        }
        else if (ingredient=="sugar"){
          sugarMachine += amount
        }
        else if (ingredient=="coffee"){
          coffeeMachine += amount
        }
        else {
          println("Erreur, veuillez sélectionner un ingrédient valable")
        }
      }

      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        if (ingredient=="milk" && milkMachine >= amount){
          milkMachine -= amount
          true
        } else if (ingredient=="milk" && milkMachine<amount){println("Il n'y a plus de stock de lait."); false}
        else if (ingredient=="sugar" && sugarMachine >= amount){
          sugarMachine -= amount
          true
        } else if (ingredient=="sugar" && sugarMachine<amount){println("Il n'y a plus de stock de sucre.");false}
        else if (ingredient=="coffee" && coffeeMachine >= amount){
          coffeeMachine -= amount
          true
        } else if (ingredient=="coffee" && coffeeMachine<amount){println("il n'y a plus de stock de café.");false}
        else {"Erreur. Ce type d'ingrédient n'existe pas.";false}
      }
    }

    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      var machines: ArrayBuffer[Machine] = new ArrayBuffer[Machine]
      Try {
        println("Chargement des machines depuis machines.csv...")

        val delimiter: String = ","
        val file = Source.fromFile(filename)
        var id: Int = 1
        for (line <- file.getLines().drop(1)) {
          var fields: Array[String] = line.split(delimiter).map(_.trim)
          println("Machine " + id + " chargée :")
          var pinCode: String = fields(0)
          println("   Code PIN: " + pinCode)
          var milk: Int = fields(1) toInt;
          if (milk >= 1000) {
            println("   Lait: " + (milk / 1000 toInt) + "." + milk % 1000 + "L")
          } else {
            println("   Lait: " + "0." + milk + "L")
          }
          //println("   Lait: " + milk)
          var sugar: Int = fields(2) toInt;
          println("   Sucre: " + sugar + "g")
          var coffee: Int = fields(3) toInt;
          println("   Café: " + coffee + "g")
          machines :+= new Machine(id, pinCode, milk, sugar, coffee)
          id += 1

        }
        if (machines.length==0){
          println("Erreur: Aucune machine n'a été enregistrée dans " +filename+ " - Veuillez vérifier le contenu de ce fichier. Fermeture du programme...")
          sys.exit(1)
        } else {println("Fichier chargé avec succès.")
          return machines}
      } match {
        case Success(_) => println("Fichier chargé avec succès."); return machines
        case ex: java.lang.NumberFormatException =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines - les données ne sont pas de Type valide. Fermeture du programme...")
          sys.exit(1)
        case ex: java.io.FileNotFoundException =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines - le fichier n'a pas pu être trouvé. Fermeture du programme...")
          sys.exit(1)
        case _ =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines. Fermeture du programme... ")
          sys.exit(1)
      }

    }
    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      var csv = new PrintWriter(filename)
      println("Sauvegarde de " + machines.length + " machines...")
      Try {
        csv.println("PINCODE,MILK,SUGAR,COFFEE")
        for (m <- machines) {
          csv.println(m.pinCodeMachine + "," + m.milkMachine + "," + m.sugarMachine + "," + m.coffeeMachine)
        }
      } match {
        case Success(_) => println("Fichier sauvegardé avec succès.")
        case ex: java.lang.NumberFormatException =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines - les données ne sont pas de Type valide. Fermeture du programme...")
          new ArrayBuffer[Machine]
          sys.exit(1)
        case ex: java.io.FileNotFoundException =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines - le fichier n'a pas pu être trouvé. Fermeture du programme...")
          new ArrayBuffer[Machine]
          sys.exit(1)
        case _ =>
          println("Erreur: échec du chargement ou de la sauvegarde des machines. Fermeture du programme... ")
          new ArrayBuffer[Machine]
          sys.exit(1)
      }
      csv.close()

    }

    //======================= INITIALISATION ========================//
    var machines: ArrayBuffer[Machine]= new ArrayBuffer[Machine]
    machines = loadcsv("machines.csv")
    //AFFICHAGES
    val affichageInitial: String ="        Nospresso Café \nVeuillez Sélectionner votre mode: \n1) Client\n2) Admin\n3) Quitter"
    val affichageInput: String = "> " //AFFICHAGE LORSQUE L'ON ATTEND UNE REPONSE UTILISATEUR
    val affichageSelectBoisson: String ="Veuillez sélectionner votre boisson:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"
    val affichageTailleLatte: String ="Veuillez sélectionner la taille de votre Latte:\n1) Latte (Petit) - CHF 2.70\n2) Latte (Moyen) - CHF 3.20\n3) Latte (Grand) - CHF 3.70"
    val affichageSelectSucre: String ="Souhaitez-vous ajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30"
    val affichageSelectLait: String ="Souhaitez-vous ajouter du lait en supplément?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non"
    val affichageSelectDoses: String ="Combien de dose(s)?\n"
    val affichageErreur: String ="Erreur! Veuillez saisir une entrée valide!"
    val affichageSelectionMachine: String = "Veuillez sélectionner une machine:"

    var affichagePrixBoisson: String = ""
    var affichagePrixSuppSucre: String = ""
    var affichagePrixSuppLait: String = ""

    //VARIABLES
    var choixBoisson: Int = 0
    var choixTailleLatte: Int = 0
    var choixSucre: Int = 0
    var choixLait: Int = 0
    var dosesLait: Int = 0
    var prixBoissonFinale: Double = 0.0
    var codePaiementTwint: String ="XXXXX"

    //TIP CECI DEVRA ETRE SUPPRIME
    //TABLEAUX
    /*val nbMachines = 5
    val codePIN: Int = 434343
    val codePINInitial: String = codePIN.toString
    var codesPIN: Array[String] = new Array[String](0)
    while (codesPIN.length < nbMachines){
      codesPIN :+= codePINInitial
    }*/
   // println(codesPIN.mkString("Array(", ", ", ")"))
    var tentativeCodePIN: Int = 0

    //PRIX
    val pEspresso: Double = 2.00
    val pCappuccino: Double = 2.50
    val pLattePetit: Double = 2.70
    val pLatteMoyen: Double = 3.20
    val pLatteGrand: Double = 3.70
    val pDoseLait: Double = 0.05

    //QUANTITES COMMANDEES
    var qCommandePoudreCafe: Int = 0
    var qCommandeSucre: Int = 0
    var qCommandeLait: Int = 0

    //STOCKS INITIAUX
    var stockPoudreCafe: Int = 50   //Unité: g
    var stockSucre: Int = 30        //Unité: g
    var stockLait: Int = 500        //Unité: ml

    //TIP CECI DEVRA ETRE SUPPRIME

    /*var stockPoudreCafeMachine: Array[Int] = new Array[Int](0)
    var stockSucreMachine: Array[Int] = new Array[Int](0)
    var stockLaitMachine: Array[Int] = new Array[Int](0)
    while (stockPoudreCafeMachine.length < nbMachines){
      stockPoudreCafeMachine :+= stockPoudreCafe
      stockSucreMachine :+= stockSucre
      stockLaitMachine :+= stockLait
    }*/

    //"Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre"


    var mode: Int = 0 //la variable "mode" sera la variable qui définit quel mode de la machine tourne, ainsi que quand revenir à l'état initial
    //Mode Client : Permet à l’utilisateur-trice de commander une boisson
    //Mode Admin : Permet à l’administrateur-trice de réapprovisionner les stocks de la machine.
    //Quitter: Revient à l'affichage initial

    def validatePin(machine: Machine): Boolean = {
      var id = machine.idMachine
      var codeCorrect = machine.pinCodeMachine
      var tentativeCode = readLine("Machine numéro " + (machine.idMachine) + " sélectionnée.\nEntrez code PIN:\n> ")
      if (tentativeCode != codeCorrect){println("PIN Erroné."); false} else {true}
    }
    /*def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var machine = machineId
      var codeCorrect = machinePins(machine)
      var tentativeCode = readLine("Machine numéro " + (machine+1) + " sélectionnée.\nEntrez code PIN:\n> ")
      if (tentativeCode != codeCorrect){println("PIN Erroné."); false} else {true}
    }*/
    def updatePin(machine: Machine): Unit = {
      var id = machine.idMachine
      var codeAChanger = machine.pinCodeMachine
      println("Machine numéro " + (machine) + " sélectionnée.\nEntrez le nouveau code PIN:")
      print(affichageInput)
      var nouveauCode: String = readInt().toString
      while (nouveauCode.length != 6){
        println("Erreur. Veuillez entrer un code PIN valide (6 chiffres):")
        print(affichageInput)
        nouveauCode = readInt().toString
      }
      machine.pinCodeMachine = nouveauCode
    }
    var nbMachines = machines.length
    /*def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      var machine = machineId
      var codeAChanger = machinePins(machine)
      println("Machine numéro " + (machine+1) + " sélectionnée.\nEntrez le nouveau code PIN:")
      print(affichageInput)
      var nouveauCode: String = readInt().toString
      while (nouveauCode.length != 6){
        println("Erreur. Veuillez entrer un code PIN valide (6 chiffres):")
        print(affichageInput)
        nouveauCode = readInt().toString
      }
      machinePins(machine) = nouveauCode
    }*/
    def serveClient(machine: Machine): Boolean = {
      prixBoissonFinale = 0
      stockPoudreCafe = machine.coffeeMachine
      stockSucre = machine.sugarMachine
      stockLait = machine.milkMachine
      //println("Option 1: Mode Client sélectionné, Machine numéro " + (machineId+1) + "sélectionnée.")
      //TIP Pour vérif le stock de la machine sélectionnée:

      //println("Stock actuels de la machine numéro " + (machineId+1) + ": \nPoudre à café: " + stockPoudreCafeMachine(machineId) + "g\nSucre: " + stockSucreMachine(machineId) + "g\nLait: " + stockLaitMachine(machineId) + "ml")
      //======================= CHOIX DE LA BOISSON =======================//
      println(affichageSelectBoisson)
      print(affichageInput)
      choixBoisson = readInt()
      while (choixBoisson < 1 || choixBoisson > 3){
        println(affichageErreur)
        println(affichageSelectBoisson)
        print(affichageInput)
        choixBoisson = readInt()
      }

      if (choixBoisson==1){
        print("CHF +2.00 Option 1: Espresso sélectionnée")
        prixBoissonFinale += 2.0
        qCommandePoudreCafe += 8
        affichagePrixBoisson = "CHF 2.00"
      }
      else if (choixBoisson==2){
        print("CHF +2.50 Option 2: Cappuccino sélectionnée")
        prixBoissonFinale += 2.5
        qCommandePoudreCafe += 6
        qCommandeLait += 100
        affichagePrixBoisson = "CHF 2.50"
      }
      else {
        println(affichageTailleLatte)
        print(affichageInput)
        choixTailleLatte = readInt()
        while (choixTailleLatte < 1 || choixTailleLatte > 3){
          println(affichageErreur)
          println(affichageTailleLatte)
          print(affichageInput)
          choixTailleLatte = readInt()
        }

        if (choixTailleLatte == 1){
          println("CHF +2.70 Option 1: Latte (Petit) sélectionnée")
          prixBoissonFinale += 2.7
          qCommandePoudreCafe += 6
          qCommandeLait += 120
          affichagePrixBoisson = "CHF 2.70"
        }
        else if (choixTailleLatte == 2){
          println("CHF +3.20 Option 2: Latte (Moyen) sélectionnée")
          prixBoissonFinale += 3.2
          qCommandePoudreCafe += 8
          qCommandeLait += 150
          affichagePrixBoisson = "CHF 3.20"
        }
        else{
          println("CHF +3.70 Option 3: Latte (Grand) sélectionnée")
          prixBoissonFinale += 3.7
          qCommandePoudreCafe += 12
          qCommandeLait += 200
          affichagePrixBoisson = "CHF 3.70"
        }
      }
      //println("Prix actuel de votre boisson: " + prixBoissonFinale)

      //======================= AJOUT DU SUCRE =======================//
      println(affichageSelectSucre)
      print(affichageInput)
      choixSucre = readInt()
      while (choixSucre < 1 || choixSucre > 4){
        println(affichageErreur)
        println(affichageSelectSucre)
        print(affichageInput)

        choixSucre = readInt()
      }

      if (choixSucre == 1){
        println("CHF +0.00 Option 1: Sans Sucre sélectionnée")
        affichagePrixSuppSucre = ""
      }
      else if (choixSucre == 2){
        println("CHF +0.10 Option 2: Peu (5g) sélectionnée")
        prixBoissonFinale += 0.1
        qCommandeSucre += 5
        affichagePrixSuppSucre = " + CHF 0.10"
      }
      else if (choixSucre == 3){
        println("CHF +0.20 Option 3: Moyen (10g) sélectionnée")
        prixBoissonFinale += 0.2
        qCommandeSucre += 10
        affichagePrixSuppSucre = " + CHF 0.20"
      }
      else {
        println("CHF +0.30 Option 3: Beaucoup (15g) sélectionnée")
        prixBoissonFinale += 0.3
        qCommandeSucre += 15
        affichagePrixSuppSucre = " + CHF 0.30"
      }
      //println("Prix actuel de votre boisson: " + prixBoissonFinale)

      //======================= AJOUT DU LAIT =======================//
      if (choixBoisson != 1) {
        println(affichageSelectLait)
        print(affichageInput)
        choixLait = readInt()
        while (choixLait < 1 || choixLait > 2) {
          println(affichageErreur)
          println(affichageSelectLait)
          print(affichageInput)

          choixLait = readInt()
        }

        if (choixLait == 1){
          println("Combien de doses?")
          print(affichageInput)
          dosesLait = readInt()

          while (dosesLait < 1 || dosesLait > 3){
            println(affichageErreur)
            println(affichageSelectDoses)
            print(affichageInput)

            dosesLait = readInt()
          }
          println("CHF +" + (dosesLait*pDoseLait).toString.take(4) + ", " + dosesLait + " doses de lait commandées.")
          qCommandeLait += dosesLait*50
          prixBoissonFinale += dosesLait*pDoseLait
          affichagePrixSuppLait = " + CHF " + (((dosesLait*pDoseLait).toString) + "0").take(4)
          //println(affichagePrixSuppLait)
          //println("Prix actuel de votre boisson: " + prixBoissonFinale)
        }
      }
      else {
        affichagePrixSuppLait = ""
      }
      //========================== VERIFICATION STOCKS ========================//
      //println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
      //println("Stock nécessaire: \n" + qCommandePoudreCafe +"g de poudre de café \n" + qCommandeLait + "ml de lait \n" + qCommandeSucre + "g de sucre")
      if (qCommandeLait > stockLait || qCommandeSucre > stockSucre || qCommandePoudreCafe > stockPoudreCafe){
        println("Erreur de stock.")

        if (qCommandeLait > stockLait){
          println("Il n'y a plus assez de Lait en stock pour satisfaire cette commande!")
        }
        else if (qCommandeSucre > stockSucre) {
          println("Il n'y a plus assez de Sucre en stock pour satisfaire cette commande!")
        }
        else if (qCommandePoudreCafe > stockPoudreCafe){
          println("Il n'y a plus assez de Poudre de Café en stock pour satisfaire cette commande!")
        }
        return false
      }
      else {
        println("Prix total: " + affichagePrixBoisson + affichagePrixSuppSucre + affichagePrixSuppLait + " = CHF " + (prixBoissonFinale.toFloat.toString + "0").take(4))

        codePaiementTwint = Random.alphanumeric.take(5).mkString.toUpperCase()
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est: " + codePaiementTwint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")
        machine.removeIngredient("coffee", qCommandePoudreCafe)
        machine.removeIngredient("milk", qCommandeLait)
        machine.removeIngredient("sugar", qCommandeSucre)
        println("Préparation de votre boisson...")
        //Thread.sleep(1000)
        println("[...]")
        //Thread.sleep(4000)
        println("Votre boisson est prête! Bonne dégustation!")
        return true
      }
    }
    /*def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      prixBoissonFinale = 0
      stockPoudreCafe = stockPoudreCafeMachine(machineId)
      stockSucre = stockSucreMachine(machineId)
      stockLait = stockLaitMachine(machineId)
      //println("Option 1: Mode Client sélectionné, Machine numéro " + (machineId+1) + "sélectionnée.")
      //TIP Pour vérif le stock de la machine sélectionnée:

      //println("Stock actuels de la machine numéro " + (machineId+1) + ": \nPoudre à café: " + stockPoudreCafeMachine(machineId) + "g\nSucre: " + stockSucreMachine(machineId) + "g\nLait: " + stockLaitMachine(machineId) + "ml")
      //======================= CHOIX DE LA BOISSON =======================//
      println(affichageSelectBoisson)
      print(affichageInput)
      choixBoisson = readInt()
      while (choixBoisson < 1 || choixBoisson > 3){
        println(affichageErreur)
        println(affichageSelectBoisson)
        print(affichageInput)
        choixBoisson = readInt()
      }

      if (choixBoisson==1){
        print("CHF +2.00 Option 1: Espresso sélectionnée")
        prixBoissonFinale += 2.0
        qCommandePoudreCafe += 8
        affichagePrixBoisson = "CHF 2.00"
      }
      else if (choixBoisson==2){
        print("CHF +2.50 Option 2: Cappuccino sélectionnée")
        prixBoissonFinale += 2.5
        qCommandePoudreCafe += 6
        qCommandeLait += 100
        affichagePrixBoisson = "CHF 2.50"
      }
      else {
        println(affichageTailleLatte)
        print(affichageInput)
        choixTailleLatte = readInt()
        while (choixTailleLatte < 1 || choixTailleLatte > 3){
          println(affichageErreur)
          println(affichageTailleLatte)
          print(affichageInput)
          choixTailleLatte = readInt()
        }

        if (choixTailleLatte == 1){
          println("CHF +2.70 Option 1: Latte (Petit) sélectionnée")
          prixBoissonFinale += 2.7
          qCommandePoudreCafe += 6
          qCommandeLait += 120
          affichagePrixBoisson = "CHF 2.70"
        }
        else if (choixTailleLatte == 2){
          println("CHF +3.20 Option 2: Latte (Moyen) sélectionnée")
          prixBoissonFinale += 3.2
          qCommandePoudreCafe += 8
          qCommandeLait += 150
          affichagePrixBoisson = "CHF 3.20"
        }
        else{
          println("CHF +3.70 Option 3: Latte (Grand) sélectionnée")
          prixBoissonFinale += 3.7
          qCommandePoudreCafe += 12
          qCommandeLait += 200
          affichagePrixBoisson = "CHF 3.70"
        }
      }
      //println("Prix actuel de votre boisson: " + prixBoissonFinale)

      //======================= AJOUT DU SUCRE =======================//
      println(affichageSelectSucre)
      print(affichageInput)
      choixSucre = readInt()
      while (choixSucre < 1 || choixSucre > 4){
        println(affichageErreur)
        println(affichageSelectSucre)
        print(affichageInput)

        choixSucre = readInt()
      }

      if (choixSucre == 1){
        println("CHF +0.00 Option 1: Sans Sucre sélectionnée")
        affichagePrixSuppSucre = ""
      }
      else if (choixSucre == 2){
        println("CHF +0.10 Option 2: Peu (5g) sélectionnée")
        prixBoissonFinale += 0.1
        qCommandeSucre += 5
        affichagePrixSuppSucre = " + CHF 0.10"
      }
      else if (choixSucre == 3){
        println("CHF +0.20 Option 3: Moyen (10g) sélectionnée")
        prixBoissonFinale += 0.2
        qCommandeSucre += 10
        affichagePrixSuppSucre = " + CHF 0.20"
      }
      else {
        println("CHF +0.30 Option 3: Beaucoup (15g) sélectionnée")
        prixBoissonFinale += 0.3
        qCommandeSucre += 15
        affichagePrixSuppSucre = " + CHF 0.30"
      }
      //println("Prix actuel de votre boisson: " + prixBoissonFinale)

      //======================= AJOUT DU LAIT =======================//
      if (choixBoisson != 1) {
        println(affichageSelectLait)
        print(affichageInput)
        choixLait = readInt()
        while (choixLait < 1 || choixLait > 2) {
          println(affichageErreur)
          println(affichageSelectLait)
          print(affichageInput)

          choixLait = readInt()
        }

        if (choixLait == 1){
          println("Combien de doses?")
          print(affichageInput)
          dosesLait = readInt()

          while (dosesLait < 1 || dosesLait > 3){
            println(affichageErreur)
            println(affichageSelectDoses)
            print(affichageInput)

            dosesLait = readInt()
          }
          println("CHF +" + (dosesLait*pDoseLait).toString.take(4) + ", " + dosesLait + " doses de lait commandées.")
          qCommandeLait += dosesLait*50
          prixBoissonFinale += dosesLait*pDoseLait
          affichagePrixSuppLait = " + CHF " + (((dosesLait*pDoseLait).toString) + "0").take(4)
          //println(affichagePrixSuppLait)
          //println("Prix actuel de votre boisson: " + prixBoissonFinale)
        }
      }
      else {
        affichagePrixSuppLait = ""
      }
      //========================== VERIFICATION STOCKS ========================//
      //println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
      //println("Stock nécessaire: \n" + qCommandePoudreCafe +"g de poudre de café \n" + qCommandeLait + "ml de lait \n" + qCommandeSucre + "g de sucre")

      if (qCommandeLait > stockLait || qCommandeSucre > stockSucre || qCommandePoudreCafe > stockPoudreCafe){
        println("Erreur de stock.")

        if (qCommandeLait > stockLait){
          println("Il n'y a plus assez de Lait en stock pour satisfaire cette commande!")
        }
        else if (qCommandeSucre > stockSucre) {
          println("Il n'y a plus assez de Sucre en stock pour satisfaire cette commande!")
        }
        else if (qCommandePoudreCafe > stockPoudreCafe){
          println("Il n'y a plus assez de Poudre de Café en stock pour satisfaire cette commande!")
        }
        return false
      }
      else {
        println("Prix total: " + affichagePrixBoisson + affichagePrixSuppSucre + affichagePrixSuppLait + " = CHF " + (prixBoissonFinale.toFloat.toString + "0").take(4))

        codePaiementTwint = Random.alphanumeric.take(5).mkString.toUpperCase()
        println("Veuillez payer en utilisant Twint.\nVotre code de paiement est: " + codePaiementTwint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")
        stockPoudreCafeMachine(machineId) -= qCommandePoudreCafe
        stockLaitMachine(machineId) -= qCommandeLait
        stockSucreMachine(machineId) -= qCommandeSucre
        println("Préparation de votre boisson...")
        //Thread.sleep(1000)
        println("[...]")
        //Thread.sleep(4000)
        println("Votre boisson est prête! Bonne dégustation!")
        return true
      }
    }*/
    def restockMachine(machine: Machine): Unit ={
      println("Accès Autorisé.")
      println("\n")
      println("Stock actuel: \n" + machine.coffeeMachine +"g de poudre de café \n" + machine.milkMachine + "ml de lait \n" + machine.sugarMachine + "g de sucre")
      print("Réapprovisionnement des stocks...\nAjout:\nPoudre de café: ")
      machine.addIngredient("coffee", readInt())

      print("Lait: ")
      machine.addIngredient("milk", readInt())

      print("Sucre: ")
      machine.addIngredient("sugar", readInt())

      println("Niveau de stock mis à jour.")
      println("Retour au menu principal...")
    }

    //======================= SELECTION DU MODE =======================//
    while (mode == 0){
      println(affichageInitial)
      print(affichageInput)
      mode = readInt()
      while (mode > 3 || mode < 1){
        println(affichageErreur)
        println(affichageInitial)
        print(affichageInput)
        mode = readInt()
      }

      while (mode <= 3 || mode >= 1){
        //======================= MODE CLIENT =======================//
        if (mode == 1){
          var transactionReussie: Boolean = false
          println("Option 1: Mode Client sélectionné.")
          while (!transactionReussie){
            println(affichageSelectionMachine)
            print(affichageInput)
            var machineSelectionnee: Int = readInt() - 1
            while (machineSelectionnee >= nbMachines || machineSelectionnee < 0) {
              println(affichageErreur)
              println(affichageInput)
              machineSelectionnee = readInt() - 1
            }
            transactionReussie = serveClient(machines(machineSelectionnee))
          }

          //println("Stock actuel: \n" + stockPoudreCafe +"g de poudre de café \n" + stockLait + "ml de lait \n" + stockSucre + "g de sucre")
          mode = 0
        }
        //======================= MODE ADMIN =======================//
        else if (mode == 2){
          var accesAutorise: Boolean = false
          var tentatives: Int = 0
          println("Option 2: Mode Admin sélectionné.")
          println(affichageSelectionMachine)
          print(affichageInput)
          var machineSelectionnee: Int = readInt() - 1
          while (machineSelectionnee > (nbMachines - 1) || machineSelectionnee < 0){
            println(affichageErreur)
            print(affichageInput)
            machineSelectionnee = readInt() - 1
          }
          while (!accesAutorise && tentatives < 3){
            accesAutorise = validatePin(machines(machineSelectionnee))
            tentatives += 1
          }
          if (accesAutorise){
            println("Accès Autorisé. Que voulez-vous faire? ")
            println("1) Choisir un nouveau PIN pour cette machine\n2) Re-stocker la machine")
            print(affichageInput)
            var choixAdmin: Int = readInt()
            while (choixAdmin > 2 || choixAdmin < 1){
              println(affichageErreur)
              print(affichageInput)
              choixAdmin = readInt()
            }
            if (choixAdmin ==1){
              updatePin(machines(machineSelectionnee))
            }
            else {restockMachine(machines(machineSelectionnee))}
          }

          println(affichageInitial)
          print(affichageInput)

          mode = readInt()
          savecsv("machines.csv", machines)
        }
        else if (mode == 3){
          println("Bonne Journée! Client/Utilisateur suivant!")
          qCommandeLait = 0
          qCommandePoudreCafe = 0
          qCommandeSucre = 0
          Thread.sleep(5000)
          /*println(affichageInitial)
          print(affichageInput)
          mode = readInt()*/
          savecsv("machines.csv", machines)
          sys.exit(0)
        }
        else {
          println(affichageErreur)
          println(affichageInitial)
          println(affichageInput)
          mode = readInt()
          savecsv("machines.csv", machines)
        }
      }
    }
  }
}