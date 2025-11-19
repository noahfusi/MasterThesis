import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import java.io.{FileNotFoundException, FileWriter, PrintWriter}

object Main {
  var pincode = ""
  var milk = 0
  var sugar = 0
  var coffee = 0
  class Machine(val id:Int,var pincode:String, var milk:Int, var sugar:Int, var coffee:Int){

    def removeIngredient(ingredient: String, amount: Int):Boolean={
      if(ingredient=="café" || ingredient=="Café"){
        if(coffee>=amount){
          coffee-=amount
          true
        }
        else{
          false
        }
      }
      else if (ingredient == "sucre" || ingredient == "Sucre"){
        if(sugar>=amount){
          sugar-=amount
          true
        }
        else{
          false
        }
      }
      else{
        if(milk>=amount){
          milk-=amount
          true
        }
        else{
          false
        }
      }
    }
    def addIngredient(ingredient:String, amount:Int):Unit={
      if(ingredient=="café" || ingredient =="Café"){
        coffee+=amount
      }
      else if(ingredient == "sucre" || ingredient == "Sucre"){
        sugar += amount
      }
      else{
        milk+=amount
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine]={
    try{
      val source = fromFile(filename)
      val Ligne_source = source.reset.getLines() //positionne sur la première ligne
      val machines = new ArrayBuffer[Machine]//nv objet
      Ligne_source.next()
      for(i<-0 to 4){
        val id = i + 1
        machines += new Machine(id,pincode,milk,sugar,coffee)
        val ligne = Ligne_source.next
        val index = ligne.split(",") //tableau de chaque valeur de la ligne
        machines(i).pincode = index(0)
        machines(i).milk = index(1).toInt
        machines(i).sugar = index(2).toInt
        machines(i).coffee = index(3).toInt

      }
      machines
    }
    catch{
      case ex : FileNotFoundException=>println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        Thread.sleep(1000)
        null
    }
  }

  def savecsv(filename:String,machines:ArrayBuffer[Machine]):Unit={
    val sauvegarde = new PrintWriter(new FileWriter(filename, false))
    sauvegarde.println("PINCODE,MILK,SUGAR,COFFEE") //1ère ligne
    for(i<-machines){
      sauvegarde.println(i.pincode + "," + i.milk+ "," + i.sugar+ "," + i.coffee)
    }
    sauvegarde.close()
  }


  // VALIDER PIN AVEC DEF
  def validatePin(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {
    println(" Entrez le Code PIN.")
    print(" > ")
    val codepin = readLine("")
    //SI LE CODE PIN N'EST PAS EGAL
    if (codepin != machines(machineId-1).pincode) {
      return false
    }
    // SI LE CODE PIN EST EGAL
    else {
      return true
    }
  }
  // METTRE A JOUR CODE PIN AVEC DEF
  def updatePin(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    println("Veuillez entrer un nouveau Code PIN à 6 chiffres.")
    var codePINMAJ = ""
    while(codePINMAJ.length!=6){
      codePINMAJ = readLine(">")
      val codePINMAJtest = codePINMAJ.toInt
      if(codePINMAJ.length == 6) {
        machines(machineId - 1).pincode = codePINMAJ
        println("Le code PIN a été mis à jour avec succès")
      }
      else{
        println("Veuillez saisir un code à 6 chiffres")
      }

    }
  }
  // MODE CLIENT SELECTION DE BOISSON AVEC DEF
  def serveClient(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {

    var choixdumode = 0
    val modeclient = 1
    var Lait = 0
    var Dose = 0
    var consCafe = 0
    var consLait = 0
    var quantiteSucre = 0
    var prixboisson = 0.00
    var Prixfinal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00


    // CHOIX DE LA BOISSON
    val Expresso = 1
    val Cappuccino = 2
    val Latte = 3
    println(" Veuillez séléctionner votre boisson :")
    println(" 1) Expresso (CHF 2.00) ")
    println(" 2) Cappuccino (CHF 2.50) ")
    println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
    print("> ")

    var choixboisson = readInt()

    // ERREUR CHOIX DE LA BOISSON
    while ((choixboisson != Expresso) && (choixboisson != Cappuccino) && (choixboisson != Latte)) {
      println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
      println(" Veuillez séléctionner votre boisson :")
      println(" 1) Expresso (CHF 2.00) ")
      println(" 2) Cappuccino (CHF 2.50) ")
      println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
      print("> ")
      choixboisson = readInt()
    }
    prixsucre = 0
    prixlait = 0

    // CHOIX BOISSON = EXPRESSO
    if (choixboisson == Expresso) {
      println("Vous avez séléctionné un Expresso. ")
      prixboisson = 2.00
      consCafe = 8
    }

    //CHOIX BOISSON = CAPPUCCINO
    if (choixboisson == Cappuccino) {
      println("Vous avez séléctionné un Cappuccino. ")
      prixboisson = 2.50
      consCafe = 6
      consLait = 100
    }
    // CHOIX BOISSON = LATTE
    if (choixboisson == Latte) {
      println("Vous avez séléctionné un Latte. ")
    }
    // CHOIX TAILLE LATTE
    if (choixboisson == Latte) {
      val LattePetit = 1
      val LatteMoyen = 2
      val LatteGrand = 3

      println("Quelle taille de Latte désirez-vous ?")
      println("1) Petit (CHF 2.70)")
      println("2) Moyen (CHF 3.20)")
      println("3) Grand (CHF 3.70)")
      print(">")

      var taille = readInt()

      // ERREUR CHOIX TAILLE LATTE
      while ((taille != 1) && (taille != 2) && (taille != 3)) {
        println("La valeur choisie est fausse, choisissez-en une nouvelle")
        println("Quelle taille de Latte désirez-vous ?")
        println("1) Petit (CHF 2.70)")
        println("2) Moyen (CHF 3.20)")
        println("3) Grand (CHF 3.70)")
        print(">")
        taille = readInt()
      }
      // LATTE TAILLE PETIT
      if (taille == LattePetit) {
        println("Vous avez choisi un Petit Latte (CHF 2.70). ")
        prixboisson = 2.70
        consCafe = 6
        consLait = 120
      }
      // LATTE TAILLE MOYEN
      if (taille == LatteMoyen) {
        println("Vous avez choisi un Latte Moyen (CHF 3.20). ")
        prixboisson = 3.20
        consCafe = 8
        consLait = 150
      }
      // LATTE TAILLE GRAND
      if (taille == LatteGrand) {
        println("Vous avez choisi un Grand Latte (CHF 3.70). ")
        prixboisson = 3.70
        consCafe = 12
        consLait = 200
      }
    }
    //CHOIX SUCRE
    val sucreOui = 1
    val sucreNon = 2

    println("Voulez-vous ajouter du sucre ?")
    println("1) oui")
    println("2) non")
    print(">")

    var choixsucre = readInt()

    //SI ERREUR CHOIX SUCRE
    while (!(choixsucre == sucreOui) && !(choixsucre == sucreNon)) {
      println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
      println("Voulez-vous ajouter du sucre ?")
      println("1) oui")
      println("2) non")
      print(">")
      choixsucre = readInt()
    }
    // SI AUCUN SUCRE
    if (choixsucre == sucreNon) {
      println("Vous ne souhaitez pas de sucre.")
      quantiteSucre = 0
    }
    val peudesucre = 1
    val moyensucre = 2
    val beaucoupsucre = 3

    // SI CHOIX SUCRE
    if (choixsucre == sucreOui) {
      println("Vous désirez du sucre en supplément. ")
      println("Quelle quantité de sucre désirez-vous ?")
      println("1) Peu (5g CHF 0.10)")
      println("2) Moyen (10g CHF 0.20)")
      println("3) Beaucoup (15g CHF 0.30).")
      print(">")

      choixsucre = readInt()

      // CHOIX SUCRE = PEU
      if (choixsucre == peudesucre) {
        println("Vous avez choisi peu de sucre. ")
        prixsucre = 0.10
        quantiteSucre = 5
      }
      // CHOIX SUCRE = MOYEN
      else if (choixsucre == moyensucre) {
        println("Vous avez choisi le niveau moyen de sucre. ")
        prixsucre = 0.20
        quantiteSucre = 10
      }
      // CHOIX SUCRE = BEAUCOUP
      else if (choixsucre == beaucoupsucre) {
        println("Vous avez choisi beaucoup de sucre. ")
        prixsucre = 0.30
        quantiteSucre = 15
      }
    }
    //SI ERREUR DOSE SUCRE
    while (!(choixsucre == peudesucre) && !(choixsucre == moyensucre) && !(choixsucre == beaucoupsucre)) {
      println("La valeur saisie est éronée, veuillez en choisir une adéquate .")
      println("Quelle quantité de sucre désirez-vous ?")
      println("1) Peu (5g CHF 0.10)")
      println("2) Moyen (10g CHF 0.20)")
      println("3) Beaucoup (15g CHF 0.30).")
      print(">")
      choixsucre = readInt()
    }

    //LAIT EN SUPPLEMENT (CAPPUCCINO ET LATTE)
    if ((choixboisson == Cappuccino) || (choixboisson == Latte)) {

      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print(">")

      var Lait = readInt()

      // SI ERREUR CHOIX LAIT
      while ((Lait != 1) && (Lait != 2)) {
        println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")

        Lait = readInt()
      }

      // SI LAIT = NON
      if (Lait == 2) {
        println("Vous ne souhaitez pas de lait en supplément.")

      }
      // SI LAIT = OUI
      if (Lait == 1) {
        println("Vous souhaitez du lait en supplément.")
        println("Combien de doses de lait désirez-vous ?")
        println("1) 1 dose ")
        println("2) 2 doses")
        println("3) 3 doses")
        print(">")

        Dose = readInt()
      }
      var dose1 = 1
      var dose2 = 2
      var dose3 = 3

      if (Dose == 1) {
        println("Vous avez séléctionné 1 dose de lait.")
        prixlait = 0.05
        consLait+=50
      }
      if (Dose == 2) {
        println("Vous avez séléctionné 2 doses de lait.")
        prixlait = 0.10
        consLait+=100
      }
      if (Dose == 3) {
        println("Vous avez séléctionné 3 doses de lait.")
        prixlait = 0.15
        consLait+=150
      }
    }
    // PAIEMENT
    if ((machines(machineId-1).sugar >= quantiteSucre) && (machines(machineId-1).milk >= consLait) && (machines(machineId-1).coffee >= consCafe)) {
      Prixfinal = prixboisson + prixsucre + prixlait
      printf("Votre prix est %.2f + %.2f + %.2f = %.2f CHF ", prixboisson, prixsucre, prixlait, Prixfinal)
      println("Veuillez payer en utilisant TWINT.")
      var paiement = Random.alphanumeric.take(5).mkString
      println("Votre code de paiement est :" + paiement)
      println("(En attente de validation du paiement...)")
      Thread.sleep(5000)
      println(" Merci ! Votre paiement été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      machines(machineId-1).removeIngredient("café",consCafe)
      machines(machineId-1).removeIngredient("lait",consLait)
      machines(machineId-1).removeIngredient("sucre",quantiteSucre)

      if (choixboisson == Expresso) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
      }
      if (choixboisson == Cappuccino) {
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
      }
      if (choixboisson == Latte) {
        println("Votre Latte est prêt ! Bonne dégustation !")
      }
      true
    }
    // STOCKS INSUFFISANTS
    else {
      println("Le stock est insuffisant. ")
      println("Veuillez choisir une autre boisson. ")
      println("Retour au menu principal... ")
      false
    }
  }

  def restockMachine(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    var sucreAjout = -10
    var cafeAjout = -10
    var laitAjout = -10

    println("Stocks actuels: ")
    println("Sucre: " + machines(machineId-1).sugar + "g")
    println("Café: " + machines(machineId-1).coffee + "g")
    println("Lait: " + machines(machineId-1).milk + "mL")

    // AJOUT NEGATIF DE SUCRE
    while(sucreAjout<0) {
      print("Ajout sucre >")
      sucreAjout = readInt()
      if(sucreAjout<0){
        println("Veuillez saisir une valeur positive.")
      }
    }
    // AJOUT NEGATIF DE CAFE
    while(cafeAjout<0) {
      print("Ajout café >")
      cafeAjout = readInt()
      if(cafeAjout<0){
        println("Veuillez saisir une valeur positive.")
      }
    }
    // AJOUT NEGATIF DE LAIT
    while(laitAjout<0) {
      print("Ajout Lait >")
      laitAjout = readInt()
      if(laitAjout<0){
        println("Veuillez saisir une valeur positive.")
      }
    }

    machines(machineId-1).addIngredient("sucre",sucreAjout)
    machines(machineId-1).addIngredient("lait",laitAjout)
    machines(machineId-1).addIngredient("café",cafeAjout)

    println("Niveaux de stocks mis à jour.")
    println("Retour au menu principal...")

  }

  def main(args: Array[String]): Unit = {




    val modeadmin = 2
    val modeclient = 1
    val quitter = 3
    var choixsucre = 0
    var choixdumode = 0
    var Lait = 0
    var Dose = 0

    var consCafe = 0
    var consLait = 0
    var quantiteSucre = 0
    var prixboisson = 0.00
    var Prixfinal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    var Essais = 1
    var action1 = 1
    var action2 = 2
    val nbMachines = 5

    println("Chargement des machines...")
    val machines = loadcsv("machines.csv")
    if(machines!=null){
      for(i<-0 to 4){
        println("Machine " + machines(i).id + " chargée :")
        println("ID : " + machines(i).id)
        println("PINCODE: " + machines(i).pincode)
        val litres = machines(i).milk.toDouble //en litres
        printf("MILK: %.3f", litres/1000)
        print("L")
        println("\nSUGAR: " + machines(i).sugar + "g")
        println("COFFEE: " + machines(i).coffee + "g\n")
        Thread.sleep(700)
      }
      println("5 Machines chargées avec succès.")
      Thread.sleep(700)
    }
    else{
      choixdumode = quitter
      println("Erreur : Échec du chargement ou de la sauvegarde des machines. \nFermeture de programme.")
    }



    while (choixdumode != quitter) {
      println("Machine séléctionnée (1-5)")
      print(">")
      var machineId = readInt()

      // TANT QUE CHOIX MACHINE PAS VALABLE
      while ((machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4) && (machineId != 5)) {
        println("La valeur saisie est érronée, veuillez en choisir une adéquate.")
        println("Machine séléctionnée (1-5)")
        print(">")
        machineId = readInt()
      }
      if (machineId == 1) {
        println("Machine séléctionnée : 1")
      }

      if (machineId == 2) {
        println("Machine séléctionnée : 2")
      }

      if (machineId == 3) {
        println("Machine séléctionnée : 3")
      }

      if (machineId == 4) {
        println("Machine séléctionnée : 4")
      }

      if (machineId == 5) {
        println("Machine séléctionnée : 5")
      }
      println(" Nospresso Café")
      println("Veuillez séléctionner votre mode :")
      println("1) Client ")
      println("2) Admin ")
      println("3) Quitter ")
      print("> ")

      choixdumode = readInt()

      // ERREUR SELECTION MODE
      while ((choixdumode != 1) && (choixdumode != 2) && (choixdumode != 3)) {
        println("La valeur saisie est érronée, veuillez en choisir une adéquate.")
        println("Veuillez séléctionner votre mode :")
        println("1) Client ")
        println("2) Admin ")
        println("3) Quitter ")
        print("> ")
        choixdumode = readInt()
      }

      //CHOIX MODE CLIENT
      if (choixdumode == modeclient) {
        println("Vous avez choisi le mode client.")

        serveClient(machineId, machines)
      }

      // CHOIX MODE ADMIN
      if (choixdumode == modeadmin) {
        println("Vous avez choisi le mode admin.")

        // CHOIX MACHINE
        val nbMachines = 5

        Essais = 1
        // GESTION TENTATIVES CODE PIN
        while (Essais != 4) {
          val validPin = validatePin(machineId, machines)
          if(!validPin){
            println("Code PIN érronné." + (3-Essais) + "essais restants." )
            Essais += 1
            if(Essais==4){
              println("Nombre d'essais échoué. Fin du Programme.")
              choixdumode = quitter
            }
          }
          else if (validPin) {
            println("Accès autorisé à la machine " + machineId)
            Essais = 4
            println("Que souhaitez-vous effectuer ?")
            println("1) Vérifier ou mettre à jour les stocks.")
            print("2) Changer le Code PIN de la machine.> ")
            var action = readInt()
            // CHOIX D'ACTION PAS VALABLE
            while((action != 1) && (action!= 2)){
              println(" Valeur saisie érronnée. Séléctionnez une valeur adéquate.")
              println("Que souhaitez-vous effectuer ?")
              println("1) Vérifier ou mettre à jour les stocks. ")
              print("2) Changer le Code PIN de la machine > ")
              action = readInt()
            }
            // ACCEDER AUX STOCKS
            if(action == 1) {
              restockMachine(machineId,machines)
            }
            // ACTUALISER LE CODE PIN
            else if (action == 2) {
              var chiffres = false //vérifier si c'est bien des chiffres
              while(!chiffres){
                try{
                  updatePin(machineId, machines)
                  chiffres = true
                }
                catch{
                  case ex: NumberFormatException=>println("Veuillez saisir des chiffres.")
                }
              }
            }
          }
        }
      }
    }
    // Mode séléctionné = quitter
    if (choixdumode == quitter) {
      if(machines!=null){
        try{
          println("Sauvegarde des 5 machines dans machines.csv...")
          savecsv("machines.csv",machines)
          Thread.sleep(500)
          println("Fichier sauvegardé avec succès.")
        }
        catch{
          case ex: FileNotFoundException=>println("Erreur : Échec de l'écriture dans machines.csv. \nLe fichier peut être verrouilé ou en lecture seule. ")
            Thread.sleep(1000)
            println("Erreur : Échec du chargement ou de la sauvegarde des machines. \nFermeture de programme.")
        }
      }

      println("Vous quittez le programme.")
    }
  }
}