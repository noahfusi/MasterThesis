import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import java.io.{FileNotFoundException, FileWriter, PrintWriter}

object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "LAIT" ) {
        milk += amount
      } else if (ingredient== "SUCRE") {
        sugar += amount
      } else if (ingredient == "CAFE") {
        coffee += amount
      } else {

      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "LAIT" ) {
        if (milk >= amount){
          milk -= amount
          return true
        } else {
          return false
        }
      } else if (ingredient == "SUCRE") {
        if (sugar >= amount){
          sugar -= amount
          return true
        } else {
          return false
        }
      } else if (ingredient == "CAFE") {
        if (coffee >= amount){
          coffee -= amount
          return true
        } else {
          return false
        }
      } else {
        return false
      }
    }

  }
  def loadcsv(filename:String):ArrayBuffer[Machine]={
    var acces = false
    try{
      val file = fromFile(filename)
      acces = true
      if(acces){
        val lignes = file.reset.getLines
        val machines = new ArrayBuffer[Machine] //array de type (la classe) Machine
        lignes.next() //saute le titre
        var num_id= 1
        while(!lignes.isEmpty){
          val id = num_id
          val next_ligne = lignes.next()
          val attributs = next_ligne.split(",") //chaque attribut de la ligne(machine) stockés dans un tableau
          val pincode = attributs(0)
          val milk = attributs(1).toInt //chaque attribut de la classe correspond à l'index du tableau attribut
          val sugar = attributs(2).toInt
          val coffee = attributs(3).toInt
          machines += new Machine(id,pincode,milk,sugar,coffee) //nouvelle objet MACHINE => chaque machine = une ligne
          num_id+=1
        }
        return machines
      }
      else{
        return null
      }
    }
    catch{
      case ex: FileNotFoundException=>println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        acces = false
        return null
    }
  }

  def savecsv(filename:String,machines:ArrayBuffer[Machine]):Unit={
    val filesave = new PrintWriter(new FileWriter(filename))//REECRITURE
    filesave.println("PINCODE,MILK,SUGAR,COFFEE") //TITRE
    var id = 0
    while(id<=4){
      filesave.print(machines(id).pincode)
      filesave.print(",")
      filesave.print(machines(id).milk)
      filesave.print(",")
      filesave.print(machines(id).sugar)
      filesave.print(",")
      filesave.print(machines(id).coffee)
      filesave.print("\n")
      id+=1
    }
    filesave.close()
  }
  // Validation du code Pin
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    println("Veuillez sélectionner le code PIN. ")
    val codePin = readLine(">")

    if (codePin == machines(machineId-1).pincode) {
      return true
    } else {
      return false
    }
  }
  // Actualisation du code Pin
  def updatePin(machineId:Int,machines:ArrayBuffer[Machine]):Unit = {
    println("Veuillez entrer un nouveau code Pin")
    var nombre = false
    var nouveaucodePin = readLine(">")
    try{
      nouveaucodePin.toInt
      nombre = true
    }
    catch{
      case ex: NumberFormatException=> println("Veuillez saisir un nombre.")
        nombre = false
    }

    while(nouveaucodePin.length!=6 || !nombre){
      println("Valeur erronée. Veuillez entrer un code à 6 chiffres.")
      nouveaucodePin = readLine(">")
      try{
        nouveaucodePin.toInt
        nombre = true
      }
      catch{
        case ex: NumberFormatException=> println("Veuillez saisir un nombre.")
          nombre = false
      }

    }
    if(nouveaucodePin.length==6 && nombre){
      machines(machineId-1).pincode = nouveaucodePin
      println("Le code PIN a été mis à jour avec succès")
      println("Retour au menu principal")
    }
  }
  // definition pour servir le client
  def serveClient(machineId:Int,machines:ArrayBuffer[Machine]): Unit = {
    val Expresso = 11
    val cappucinno = 12
    val Latte = 13
    var PoudredeCafe = 0
    var StockLait = 0
    var Stocksucre = 0
    var Prix = 0.00F
    var Prixsucre = 0.00F
    var PrixLait = 0.00F
    var Prixfinal = 0.00F
    // Sélection de la boisson
    println(" Quelle est la boisson désirée ? ")
    println(" 11) Expresso - CHF 2.00 ")
    println(" 12) cappucinno - CHF 2.50 ")
    println(" 13) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
    print(" > ")
    var Boisson = readInt()

    // Boucle d'erreur, si la sélection de boisson est erronnée

    while ((Boisson != Expresso) && (Boisson != cappucinno) && (Boisson != Latte)) {
      println("La valeur saisie est fausse, veulliez en choisir une bonne")
      println("Quelle est la boisson désiré")
      println(" 11) Expresso - CHF 2.00 ")
      println(" 12) cappucinno - CHF 2.50 ")
      println(" 13) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) ")
      print(" > ")
      Boisson = readInt()
    }
    if (Boisson == Expresso) {
      println("Boisson sélectionner : Expresso. ")
      Prix = 2.00F
      PoudredeCafe = 8
    } else if (Boisson == cappucinno) {
      println("Boisson sélectionner :  Cappucinno. ")
      Prix = 2.50F
      PoudredeCafe = 6
      StockLait = 100
    } else if (Boisson == Latte){
      println("Boisson sélectionner : Latte. ")
    }

    // Choix de la taille du Latte
    if (Boisson == Latte) {
      println("Quelle est la taille souhaiter ? ")
      println(" 1) petit - CHF 2.70 ")
      println(" 2) moyen - CHF 3.20 ")
      println(" 3) grand - CHF 3.70 ")
      print(" > ")
      var Taille = readInt()

      // Boucle d'érreur pour la taille du Latte
      while ((Taille != 1) && (Taille != 2) && (Taille != 3)) {
        println("Valeure éronner, veuillez en choisir une bonne. ")
        println(" 1) petit - CHF 2.70 ")
        println(" 2) moyen - CHF 3.20 ")
        println(" 3) grand - CHF 3.70 ")
        print(" > ")
        Taille = readInt()
      }
      if (Taille == 1) {
        println("Boisson sélectionner : Latte (Petit). ")
        PoudredeCafe = 6
        StockLait = 120
        Prix = 2.70F

      } else if (Taille == 2) {
        println("Boisson sélectionner : Latte (Moyen). ")
        PoudredeCafe = 8
        StockLait = 150
        Prix = 3.20F

      } else if (Taille == 3) {
        println("Boisson sélectionner : Latte (Grand) ")
        PoudredeCafe = 12
        StockLait = 200
        Prix = 3.70F
      }
    }
    // Choix du sucre
    if ((Boisson == Expresso) || (Boisson == cappucinno) || (Boisson == Latte)) {
      println("Voulez-vous du sucre ?")
      println(" 22) oui ")
      println(" 21) non ")
      print(" > ")
      var sucre = readInt()

      // Boucle d'erreur pour le sucre
      while ((sucre != 21) && (sucre != 22)) {
        println(" Valeur erronée, veuillez en choisir une bonne. ")
        println(" Voulez-vous du sucre ? ")
        println(" 22) oui ")
        println(" 21) non ")
        print(" > ")
        sucre = readInt()
      }
      if (sucre == 21) {
        println("Vous ne souhaitez pas ajouter du sucre. ")
      } else if (sucre == 22) {

        // Sélection de la quantité de sucre
        println("Quelle est la quantité souhaité ? ")
        println("31) peu (5g) ")
        println("32) moyen (10g) ")
        println("33) beaucoup (15g) ")
        print(" > ")
        var quantite = readInt()

        // Boucle d'erreur, si la sélection de la quantité est érronner
        while ((quantite != 31) && (quantite != 32) && (quantite != 33)) {
          println(" Valeure éronner, veuillez en choisir une nouvelle. ")
          println(" Quelle est la quantité souhaité ? ")
          println(" 31) peu (5g) ")
          println(" 32) moyen (10g) ")
          println(" 33) beaucoup (15g) ")
          print(" > ")
          quantite = readInt()
        }
        if (quantite == 31) {
          println("Niveau de sucre : peu (5g). ")
          Prixsucre = 0.10F
          Stocksucre = 5
        } else if (quantite == 32) {
          println("Niveau de sucre : moyen (10g). ")
          Prixsucre = 0.20F
          Stocksucre = 10
        } else if (quantite == 33) {
          println("Niveau de sucre : beaucoup (15g). ")
          Stocksucre = 15
          Prixsucre = 0.30F
        }
      }
      // Sélection du Lait
      if ((Boisson == cappucinno) || (Boisson == Latte)) {
        println("Souhaitez-vous ajouter du lait ? ")
        println(" 42) oui ")
        println(" 41) non ")
        print(" > ")
        var Lait = readInt()

        // Boucle d'érreure, si la sélection du lait est érronner
        while ((Lait != 41) && (Lait != 42)) {
          println("Valeure érroner, veuillez en choisir une bonne. ")
          println("Souhaitez-vous ajouter du lait ? ")
          println(" 42) oui ")
          println(" 41) non ")
          print(" > ")
          Lait = readInt()
        }
        if (Lait == 41) {
          println("Vous n'ajoutez pas de lait. ")

          // Sélection des dosses de lait
        } else if (Lait == 42) {
          println("Vous ajouter du Lait. ")
          println(" Combien de dose voulez-vous ? ")
          println(" 1) un ")
          println(" 2) deux ")
          println(" 3) trois ")
          print(" > ")
          var Dose = readInt()

          // Boucle d'érreure des dosses de Lait
          while ((Dose != 1) && (Dose != 2) && (Dose != 3)) {
            println("Valeur erronée, veuillez en choisir une bonne. ")
            println(" 1) un ")
            println(" 2) deux ")
            println(" 3) trois ")
            print(" > ")
            Dose = readInt()
          }
          if (Dose == 1) {
            println("Vous ajouter une dose de lait à votre café. ")
            PrixLait = 0.05F
            StockLait += 50
          } else if (Dose == 2) {
            println("Vous ajouter deux doses de lait à votre café. ")
            PrixLait = 0.05F * 2.00F
            StockLait += 100
          } else if (Dose == 3) {
            println("Vous ajouter trois doses de lait à votre café. ")
            PrixLait = 0.05F * 3.00F
            StockLait += 150
          }
        }
      }
    }

    // Paiement, si le stock est suffisant
    // if ((machines[machineId].coffee >= PoudredeCafe) && (machines[machineId].sugar >= Stocksucre) && ([machineId].milk >= StockLait))
    if((machines(machineId-1).coffee >= PoudredeCafe) && (machines(machineId-1).sugar >= Stocksucre) && (machines(machineId-1).milk >= StockLait)){
      Prixfinal = Prix + Prixsucre + PrixLait
      printf ("Votre prix est %.2f + %.2f + %.2f  =  %.2f CHF ",Prix, Prixsucre, PrixLait, Prixfinal)
      println()
      println("Sélectionner, payement par Twint. ")
      val Paiement = Random.alphanumeric.take(5).mkString
      println("Votre code de payement est : " + Paiement)
      println("En attente de validation du payement. ")
      Thread.sleep(3000)
      println("Merci ! Votre payement a été accepté. ")
      println("Préparation de votre boisson ...")
      Thread.sleep(3000)
      if(Boisson == Expresso){
        println("Votre Expresso est prêt ! Bonne dégustation !")
      }else
        if(Boisson == cappucinno){
          println("Votre Cappucinno est prêt ! Bonne dégustation !")
        }else
          if(Boisson == Latte){
            println("Votre Latte est prêt ! Bonne dégustation ! ")
          }

      // Déduction des stocks
      machines(machineId-1).removeIngredient("LAIT", StockLait)
      machines(machineId-1).removeIngredient("CAFE", PoudredeCafe)
      machines(machineId-1).removeIngredient("SUCRE", Stocksucre)

    } else {
      println("Le stock est insufisant. Veuillez choisir une autre machine. ")
    }
  }
  // definition pour actualiser les stock des machines

  def restockMachine(machineId:Int,machines:ArrayBuffer[Machine]):Unit = {
    println("Niveaux de stocks actuels : ")

    println("Café : " + machines(machineId-1).coffee+"g" )
    println("Sucre : " + machines(machineId-1).sugar +"g" )
    println("Lait : " + machines(machineId-1).milk +"ml" )


    print("Poudre de Café (g) > ")
    var AjoutCafe = readInt()
    while(AjoutCafe<0){
      println("Valeur erronée. Veuillez entrer une valeur positive")
      print("Poudre de Café (g) > ")
      AjoutCafe = readInt()
    }
    print("Sucre (g) > ")
    var AjoutSucre = readInt()
    while(AjoutSucre<0){
      println("Valeur erronée. Veuillez entrer une valeur positive")
      print("Sucre (g) > ")
      AjoutSucre = readInt()
    }
    print("Lait (mL) > ")
    var AjoutLait = readInt()
    while(AjoutLait<0){
      println("Valeur erronée. Veuillez entrer une valeur positive")
      print("Lait (mL)> ")
      AjoutLait = readInt()
    }

    machines(machineId-1).addIngredient("CAFE", AjoutCafe)
    machines(machineId-1).addIngredient("LAIT", AjoutLait)
    machines(machineId-1).addIngredient("SUCRE", AjoutSucre)

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def main(args: Array[String]): Unit = {
    val ModeClient = 1
    val ModeAdmin = 2
    val Quitter = 3
    var choixdumode = 0
    var Tentatives = 1
    val machines = loadcsv("machines.csv")
    val nbMachines = 5

    println("Chargement des machines...")
    if(machines==null){
      choixdumode = Quitter
      println("Erreur : Échec du chargement ou de la sauvegarde de la machine.\nFermeture du programme.")
      Thread.sleep(500)
    }
    else{
      for(i<-0 until 5){
        val milk2 = machines(i).milk.toDouble //changer en litres
        println("\nMachine " + (i+1) + " chargée :" + "\nID: " + machines(i).id + "\nPINCODE: " + machines(i).pincode)
        printf("LAIT: %.3f",milk2/1000)
        print("L")
        println("\nSUCRE: " + machines(i).sugar + "g" + "\nCAFÉ: " + machines(i).coffee + "g" )
        Thread.sleep(750)
      }
      println("\n5 machines chargées avec succès")
    }

    while(choixdumode != Quitter) {
      // Demande choix de la machine
      println("Machine séléctionnée 1-5 ")
      print(" >")
      var machineId = readInt()

      // Tant Que Choix Machine Pas Valable
      while (machineId > nbMachines || machineId < 1){
        println("La valeur saisie est érronée, veuillez en choisir une adéquate. ")
        print(">")
        machineId = readInt
      }
      println("Machine sélectionnée: " + machineId)
      // Sélection du mode
      println("       Nospresso Café ")
      println("Veuillez sélectionner votre mode")
      println(" 1) Client ")
      println(" 2) Admin ")
      println(" 3) Quitter ")
      print(" > ")
      choixdumode = readInt()

      // Boucle d'erreur, si le choix du mode erroné
      while ((choixdumode != ModeClient) && (choixdumode != ModeAdmin) && (choixdumode != Quitter)) {
        println(" La valeur choisie est erronée, veuillez en choisir une correcte. ")
        println("Veuillez sélectionner votre mode.")
        println(" 1) Client ")
        println(" 2) Admin ")
        println(" 3) Quitter ")
        print(" > ")
        choixdumode = readInt()
      }

      if (choixdumode == ModeClient) {
        println("Vous avez sélectionné le mode client.")
        serveClient(machineId, machines)
      }

      // Mode admin
      if (choixdumode == ModeAdmin) {
        // Tentatives pour le code Pin
        while(Tentatives!=4) {
          val validerPIN = validatePin(machineId, machines)
          if(!validerPIN){
            println("Code PIN erroné . " + (3-Tentatives) + " tentatives restantes")
            Tentatives+=1
            if(Tentatives==4){
              println("Trop de tentatives échouées. Fin du programme. ")
              choixdumode=Quitter
            }
          }
          else if (validerPIN) {
            println("Accès autorisé à la machine " + machineId)
            Tentatives = 4
            println("Quelle est la prochaine action souhaitée : ")
            println("61) Modifier le code PIN")
            println("62) Changer les stocks ")
            print(" > ")

            var choix = readInt()
            while(choix!=61 && choix!=62){
              println("Valeur erronée. Veuillez en séléctionner une adéquate.")
              println("Quelle est la prochaine action souhaitée : ")
              println("61) Modifier le code PIN")
              println("62) Changer les stocks")
              print(">")
              choix = readInt()
            }
            if(choix==61){

              updatePin(machineId,machines)
            }
            else if(choix==62){
              restockMachine(machineId,machines)
            }
          }
        }
      }
      Tentatives = 1
    }
    // Quitter
    if (choixdumode == Quitter) {
      if(machines==null){

      }
      else{
        println("Sauvegarde des machines dans machines.csv...")
        try{
          savecsv("machines.csv",machines)
          println("Fichier sauvegardé avec succès !")
          Thread.sleep(500)
        }
        catch{
          case ex: FileNotFoundException=>println("Erreur : Échec de l'écriture dans machines.csv. Le fichier est verrouillé ou en lecture seule.")
            println("Erreur : Échec du chargement ou de la sauvegarde de la machine.\nFermeture du programme.")
            Thread.sleep(1500)
        }
      }
      println("Vous avez quitté le programme. ")
    }
  }
}