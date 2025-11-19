object Main {
  import io.StdIn._
  import scala.collection.mutable.ArrayBuffer
  import scala.io.Source
  import java.io.{FileWriter,PrintWriter}
  import java.io.FileNotFoundException

  class Machine (var Id: Int, var pincode: String, var lait: Int, var sucre: Int, var cafe: Int){
    //methode pour ajouter des ingredients
    def addIngredient (ingredient: String, quantite: Int): Unit = {
      if (ingredient == "lait"){
        lait += quantite
      }
      else if (ingredient == "sucre"){
        sucre += quantite
      }
      else if (ingredient == "cafe"){
        cafe += quantite
      }
      else println ("ingrédient invalide. Veuillez saisir un autre ingrédient ")
    }

    //methode pour retirer des ingredients
    def removeIngredient (ingredient: String, quantite: Int): Boolean = {
    if (ingredient == "lait" && lait >= quantite){
      lait -= quantite
      return true
    }
    else if (ingredient == "sucre" && sucre >= quantite){
      sucre -= quantite
      return true
    }
    else if (ingredient == "cafe" && cafe >= quantite){
      cafe -= quantite
      return true
    }
    else {
      //println (" il n'y a pas assez de stock pour retirer cette quantité de " + ingredient )
      return false
    }
    }


    def affichage(machines : Machine)= {
      println("chargement des machines depuis machines.csv... " )
      println()
      println("Machine " + machines.Id + " chargée : ")
      println("    ID: " + machines.Id)
      println("    Code PIN: " + machines.pincode)
      println("    Lait: " + machines.lait)
      println ("    Sucre: " + machines.sucre)
      println ("    Café: "+ machines.cafe)
      println


    }
  }
  //méthode pour lire un fichier et remplir un ArrayBuffer à partir des données lues dans le fichier

  def loadcsv (filename: String): ArrayBuffer[Machine] = {
    //val fr = Source.fromFile(filename) // ouverture du fichier
    var machines = new ArrayBuffer[Machine]() // initialisation de l'array buffer

    try { //gestion des erreurs
       val fr = Source.fromFile(filename) // ouverture du fichier

      val lignesFr = fr.getLines().drop(1) //creation d'un iterateur pour lire ligne par ligne
       // drop(1) pour ignorer l'en-tete

      var compteurMachines = 0
      while (!lignesFr.isEmpty) { //tant que l'iterateur n'est pas vide on lit la ligne suivante
          val ligne = lignesFr.next()
          val parties = ligne.split(",") //on recupere les donnes dans les differentes parties du fichier (separees par la virgule)

          //on extrait les donnes des differentes parties
          val pincode = parties(0)
          val lait = parties(1).toInt
          val sucre = parties(2).toInt
          val cafe = parties(3).toInt

          //creation d'une nouvelle machine et l'ajouter dans l'arrayBuffer
          val nouvelleMachine = new Machine(machines.size + 1, pincode, lait, sucre, cafe)
          machines += nouvelleMachine

      }
      fr.close() //fermeture du fichier

      //println(machines.size + "machine(s) chargée(s) avec succès ")
    }

      catch {
        case ex:FileNotFoundException
          => println (" Erreur: Fichier introuvable. Vérifier le chemin d'accès et réessayez")
          return null
        case ex: java.io.IOException
        => println ("Erreur d'entrée/sortie")
          return null
        case ex: Exception
        => println("Une erreur est survenue")
          return null
      }
    //fr.close() //fermeture du fichier

      return machines //on retourne l'arraybuffer
  }




  def savecsv(filename: String, machines: ArrayBuffer[Machine]) : Unit = {
    try {
      //val printWriter = new PrintWriter (new FileWriter ("machines.csv" ))
      val printWriter = new PrintWriter ( "machines.csv" )

      printWriter.println("PINCODE,LAIT,SUCRE,CAFE")
      for (machine <- machines){
        printWriter.println(machine.pincode + "," + machine.lait +"," + machine.sucre + "," + machine.cafe)
      }

      printWriter.close()
      println ("Fichier sauvgardé avec succès." )
    }
    catch {
      case ex : java.io.FileNotFoundException
        => println ("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez")

      case ex: java.io.IOException
      => println ("Erreur d'entrée/sortie")


      case ex: Exception
      => println("Une erreur est survenue")

    }

  }



  //méthodes

  //méthode pour valider le code PIN pour la machine selectionnnée
  def validatePin(machineId: Int,  machines: ArrayBuffer[Machine]): Boolean = { //modif  de l'attribut machinePins avant --> machinePins: Array[String]
    val tentativesMax = 3
    var tentative = 0
    for (i <- 1 to tentativesMax){
      tentative += 1
      println ("Entrez le Code PIN : ******")
      print ("> ")
      var codePinSaisi = readLine()

      if (codePinSaisi == machines(machineId).pincode){ //modif avant--> codePinSaisi == machinePins(machineId)
        println("Accès autorisé.")
        return true
      }
      else {
        println("Code PIN incorrect. " + (tentativesMax-tentative) + " tentatives restante(s)")
      }
    }
    //on sort de la boucle
    println()
    println ("Trop de tentatives échouées. Fin du programme. ")
    return false
  }

  //méthode pour mettre à jour le PIN
  def updatePin (machineId: Int,  machines: ArrayBuffer[Machine] ): Unit = { //modif  de l'attribut machinePins avant --> machinePins :Array[String] suggestion : machines: ArrayBuffer[Machine]
    //verification de la validité du code PIN
    var modifCodePIN = " "
    var PINValide = false // on initialise PINValide comme false pour entrer dans la boucle while

    while (!PINValide) {
      print("Entrer un nouveau code PIN à 6 chiffres > ")
      modifCodePIN = readLine()

      //on vérifie si le code saisi a bien 6 chiffre
      if (modifCodePIN.size != 6) {
        println("Le code doit contenir exactement 6 chiffres ")
        PINValide = false // on retourne au début de la boucle while
      }
      //si on arrive jusqu'ici c'est que e code contient bien 6 chiffres il faut maintenant vérifier que chacun se situe entre 0 et 9
      else {
        PINValide = true
        for (i <- modifCodePIN) {
          if (i < '0' || i > '9') {
            PINValide = false // on retourne au début de la boucle while
          }
        }
      }
      //println ("code modifié avec succès")
    }
    machines(machineId).pincode = modifCodePIN //on remplace le PIN de la machineId par le nouveau PIN //modif du nom de la variabble avant --> machinePins(machineId)
  }

  def serveClient(machineId: Int, StockCafe: Int, StockSucre:Int, StockLait: Int, machines :ArrayBuffer[Machine] ): Boolean = {

    var SCpoudreCafe = 0
    var SClait = 0
    var SCsucre = 0


    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    var boisson = readInt()
    while (!((boisson == 1) || (boisson == 2) || (boisson == 3))) {
      boisson = readInt()
    }

    //initialisation du prix
    var prixStandard = 0.0
    var recapBoisson = " "
    var recapQuantSucre = " "
    var recapLait = " "
    var prixSucreSupp = 0.0
    var prixLaitSupp = 0.0
    var prixTotal = 0.0


    //boisson : Expresso
    if (boisson == 1) {
      println(" Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      var sucre = readInt()
      while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
        sucre = readInt()}
      //stock consommé
      SCpoudreCafe = 8
      SCsucre = (sucre - 1) * 5

      //prix + recaps commande
      prixStandard = 2.00
      recapBoisson = "Expresso"

      if (sucre == 1){
        recapQuantSucre = "Sans sucre"
      }
      else if (sucre == 2){
        recapQuantSucre = "Peu (5g)"
      }
      else if (sucre == 3){
        recapQuantSucre = "Moyen (10g)"
      }
      else if (sucre == 4){
        recapQuantSucre = "Beaucoup (15g)"
      }
      prixSucreSupp = (sucre - 1) * 0.10
      prixTotal = prixStandard + prixSucreSupp

    }

    // boisson : Cappuccino
    if (boisson == 2) {
      println(" Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      var sucre = readInt()
      while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
        sucre = readInt()
      }
      //stock consommé de poudre café lait et si choisi sucre
      SCpoudreCafe = 6
      SClait = 100
      SCsucre = (sucre - 1) * 5

      //prix + recaps commande
      prixStandard = 2.50
      recapBoisson = "Cappuccino"
      if (sucre == 1){
        recapQuantSucre = "Sans sucre"
      }
      else if (sucre == 2){
        recapQuantSucre = "Peu (5g)"
      }
      else if (sucre == 3){
        recapQuantSucre = "Moyen (10g)"
      }
      else if (sucre == 4){
        recapQuantSucre = "Beaucoup (15g)"
      }

      prixSucreSupp = (sucre - 1) * 0.10

      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print("> ")
      var lait = readInt()
      while (!((lait == 1) || (lait == 2))) {
        lait = readInt()
      }
      if (lait == 1) {
        println("combien de doses ? ")
        print("> ")
        var doselait = readInt()
        while (!((doselait == 1) || (doselait == 2) || (doselait == 3))) {
          doselait = readInt()
        }
        SClait += doselait * 50 //stock consommé lait si supplément

        recapLait = "Oui, " + doselait + " dose(s)"
        prixLaitSupp = doselait * 0.05
      }
      else {
        recapLait = "Non"
      }
      prixTotal = prixStandard + prixSucreSupp + prixLaitSupp
    }

    //boisson: latte
    if (boisson == 3) {
      println("veuillez selectionner la taille de votre boisson ")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      print("> ")
      var taillelatte = readInt()
      while (!((taillelatte == 1) || (taillelatte == 2) || (taillelatte == 3))) {
        taillelatte = readInt()
      }

      //stock consommé en fonction de la taille
      if (taillelatte == 1) {
        SCpoudreCafe = 6
        SClait = 120
        recapBoisson = "Latte (Petit)"
        prixStandard = 2.70
      }
      else if (taillelatte == 2) {
        SCpoudreCafe = 8
        SClait = 150
        recapBoisson = "Latte (Moyen)"
        prixStandard = 3.20
      }
      else {
        SCpoudreCafe = 12
        SClait = 200
        recapBoisson = "Latte (Grand)"
        prixStandard = 3.70
      }

      println(" Souhaitez-vous ajouter du sucre ? ")
      println("1) Sans sucre ")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      var sucre = readInt()
      while (!((sucre == 1) || (sucre == 2) || (sucre == 3) || (sucre == 4))) {
        sucre = readInt()
      }
      //stock consommé de sucre si choisi
      SCsucre = (sucre - 1) * 5

      //prix supp lait
      if (sucre == 1){
        recapQuantSucre = "Sans sucre"
      }
      else if (sucre == 2){
        recapQuantSucre = "Peu (5g)"
      }
      else if (sucre == 3){
        recapQuantSucre = "Moyen (10g)"
      }
      else if (sucre == 4){
        recapQuantSucre = "Beaucoup (15g)"
      }
      prixSucreSupp = (sucre - 1) * 0.10

      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      print("> ")
      var lait = readInt()
      while (!((lait == 1) || (lait == 2))) {
        lait = readInt()
      }
      if (lait == 1) {
        println("combien de doses ? ")
        print("> ")
        var doselait = readInt()
        while (!((doselait == 1) || (doselait == 2) || (doselait == 3))) {
          doselait = readInt()
        }
        SClait += doselait * 50 //stock consommé lait si supplément

        recapLait = "Oui, " + doselait + " dose(s)"
        prixLaitSupp = doselait * 0.05

      }
      else {
        recapLait = "Non"

      }
      prixTotal = prixStandard + prixSucreSupp + prixLaitSupp
    } //fin de personnalisation des boissons


    //gestion des stock
    /*if ((SCpoudreCafe <= machines(machineId).cafe) && (SCsucre <= machines(machineId).sucre) && (SClait <= machines(machineId).lait)) {
      machines(machineId).cafe -= SCpoudreCafe
      machines(machineId).sucre -= SCsucre
      machines(machineId).lait -= SClait */

    if (machines(machineId).lait >= SClait && machines(machineId).sucre >= SCsucre &&
      machines(machineId).cafe >= SCpoudreCafe) {

      machines(machineId).removeIngredient("lait", SClait)
      machines(machineId).removeIngredient("sucre", SCsucre)
      machines(machineId).removeIngredient("cafe", SCpoudreCafe)


      //println("Ingrédients retirés avec succès.")

      //Interface de Paiement (code Twint)
      val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var CodePaiement = " "
      for (i <- 1 to 5) {
        var caractere = alphanumerique((math.random * alphanumerique.length).toInt).toString
        CodePaiement += caractere
      }
      // récap géneralisé de la commande + interface de paiement
      if (boisson == 1) {
        println()
        println("Boisson selectionnée : " + recapBoisson)
        println("Niveau de sucre: " + recapQuantSucre)
        printf ("prix Total : CHF %.2f + CHF %.2f = CHF %.2f ",prixStandard,prixSucreSupp,prixTotal)
        println()
        println()
      }
      else if ((boisson == 2) || (boisson == 3)){
        println()
        println("Boisson selectionnée : " + recapBoisson)
        println("Niveau de sucre: " + recapQuantSucre)
        println ("Lait supplémentaire : " + recapLait)
        printf ("prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ",prixStandard,prixSucreSupp,prixLaitSupp,prixTotal)
        println()
        println()
      }

      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + CodePaiement)
      println("(En attente de validation du paiement...")
      Thread.sleep(3000) // attente de 3 secondes
      println(" ") // saut de ligne
      println("Merci ! Votre paiement a été accepté. ")
      println ("Préparation de votre boisson...")
      Thread.sleep(5000) //attente de 5 secondes
      println ("Votre " + recapBoisson + " est prêt ! Bonne dégustation !")

    }


    //gestion des erreurs en cas de stock insuffisants
    else {
      println ("Erreur : quantité de stock insuffisante pour préparer la boisson sélectionnée ")
      println ("Veuillez sélectionner une autre machine")
      /*if (SCpoudreCafe > machines(machineId).cafe) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez sélectionner une autre machine ")
      }

      if (SCsucre > machines(machineId).sucre) {
        println("Erreur : la quantité de sucre est insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez sélectionner une autre machine ")
      }

      if (SClait > machines(machineId).lait) {
        if (boisson == 2) {
          println(" Erreur : la quantité de lait est insuffisante pour préparer votre commande")
          println("Veuillez sélectionner une autre machine")
        }
        else if (boisson == 3){
          println ("Erreur : la quantité de lait est insuffisante pour préparer la boisson sélectionnée")
          println ("Veuillez sélectionner une autre machine")
        }
      }*/

    }
    return false

  }

  def restockMachine(machineId: Int, StockCafe: Int, StockSucre: Int, StockLait:Int, machines: ArrayBuffer[Machine]) : Unit = {
    var reapprovisCafe = 0
    var reapprovisLait = 0
    var reapprovisSucre = 0

    println("Réapprovisionnement des stocks ...")
    println("Ajout :")
    reapprovisCafe = readLine("    Poudre de café : ").toInt
    reapprovisLait = readLine("    Lait           : ").toInt
    reapprovisSucre = readLine("    Sucre          : ").toInt

    //vérification que les valeurs saisies soient positives
    while ((reapprovisCafe < 0) || (reapprovisSucre < 0) || (reapprovisLait <0)){
      println ("Erreur. Les valeurs saisies doivent êtres positives")
      println("Réapprovisionnement des stocks ...")
      println("Ajout :")
      reapprovisCafe = readLine("    Poudre de café : ").toInt
      reapprovisLait = readLine("    Lait           : ").toInt
      reapprovisSucre = readLine("    Sucre          : ").toInt
    }

    machines(machineId).addIngredient("cafe", reapprovisCafe)
    machines(machineId).addIngredient("lait", reapprovisLait)
    machines(machineId).addIngredient("sucre", reapprovisSucre)

    /*machines(machineId).cafe += reapprovisCafe
    machines(machineId).lait += reapprovisLait
    machines(machineId).sucre += reapprovisSucre*/


  }


  def main(args: Array[String]): Unit = {
    var retourmenu =0

    //decla de l'arrayBuffer
    val machines = loadcsv("machines.csv")
    if (machines != null){
      for (i<- 0 to machines.size-1){
        machines(i).affichage(machines(i))
      }
      println(machines.size + " machine(s) chargée(s) avec succès ")

    }
    else if (machines == null){
      retourmenu = 1
    }


    //chargement des machines

    //val NbMachines = 5 (ex2)
    //tableau: chaque machine a son identifiant qui correspond ici à son index
    //initialisation des variables de la machine et de son identifiant
    var machineSelectionnee = 0
    var machineId = machineSelectionnee - 1

    //Tableau : initiation des valeurs de stock initiaux pour les 5 machines
    /*var StockCafe = Array.fill(NbMachines)(50) // 50g de cafe par machine
    var StockSucre = Array.fill(NbMachines)(30) // 30 g de sucre par machine
    var StockLait = Array.fill(NbMachines)(500) //500ml de lait par machine
        (ex2) */
    //tableau : initialisation code PIN
    //var machinePins = Array.fill (NbMachines)("434343") //par défaut, chaque machine a comme code PIN 434343, peut etre modifié par la suite


    //boucle de retour au menu
   // var retourmenu = 0
    while (retourmenu == 0){

      //lancement du programme
      println (" ")
      println("     " + "Nospresso Café")
      println("Veuillez selectionner votre mode : ")
      println("1) Client")
      println("2) Admin ")
      println("3) Quitter ")
      print("> ") // l'utilisateur doit choisir son mode 1,2 ou 3
      var mode = readInt()
      while (!((mode == 1) || (mode == 2) || (mode == 3))) {
        mode = readInt()
      }

      //mode client
      if (mode == 1) {


        //l'admin choisit la machine qu'il veut utiliser
        print("Veuillez sélectionnez une machine (1-5) > ")
        machineSelectionnee = readInt()
        machineId = machineSelectionnee - 1 // pour que l'identifiant corresponde à l'index
        while (machineSelectionnee < 1 || machineSelectionnee > 5) {
          print("Veuillez selectionner une machine entre 1 et 5 > ")
          machineSelectionnee = readInt()
        }

        //serveClient(machineId, StockCafe, StockSucre, StockLait) (exercice2)
        serveClient (machineId, machines(machineId).cafe, machines(machineId).sucre, machines(machineId).lait, machines: ArrayBuffer[Machine])

      } //fin du mode client


      //mode admin
      if (mode == 2) {
        //l'admin choisit la machine qu'il veut utiliser
        print ("Veuillez sélectionnez une machine (1-5) > ")
        machineSelectionnee = readInt()
        machineId = machineSelectionnee -1 // pour que l'identifiant corresponde à l'index
        while (machineSelectionnee < 1 || machineSelectionnee > 5) {
          print ("Veuillez selectionner une machine entre 1 et 5 > ")
          machineSelectionnee = readInt()
        }

        //une fois qu'il a selectionner sa machine, l'admin doit fournir le code PIN respectif
        if (validatePin(machineId, machines)) { //si la méthode validatePin renvoie la valeur true --> accès autorisé
          println()
          //l'admin choisit si il veut consulter les stocks ou modifier le code pin
          println("sélectionnez 1) ou 2) ")
          println("1) consulter et réapprovisionner les stocks")
          println("2) modifier le code PIN")
          print("> ")
          var choixAdmin = readInt()
          while (!((choixAdmin == 1) || (choixAdmin == 2))) {
            print("sélectionnez 1) ou 2) > ")
            choixAdmin = readInt()
          }
          //si l'admin choisit de consulter les stocks
          if (choixAdmin == 1) {

            println()
            println("Stocks : ")
            println("Poudre de café : " + machines(machineId).cafe + "g")
            println("Lait           : " + machines(machineId).lait + "ml")
            println("Sucre          : " + machines(machineId).sucre + "g")
            println()

            restockMachine(machineId, machines(machineId).cafe, machines(machineId).sucre, machines(machineId).lait, machines:ArrayBuffer[Machine])
            //verification des stocks :
            //println()
            //println (" stock café " + StockCafe.mkString("|"))
            //println (" stock lait " + StockLait.mkString("|"))
            //println (" stock sucre " + StockSucre.mkString("|"))




          }
          else {
            //l'utilisateur choisit de modifier le code PIN
            println ("Mise à jour du code PIN pour la machine " + machineSelectionnee)
            updatePin(machineId, machines)
            println()
            println("Le code PIN a été mis à jour avec succès. ")
            println ("Retour au menu principal...")
          }
        }

        else {
          //lutilisateur à épuisé le nb max de tentatives --> accès non autorisé
          retourmenu = 1 // pour sortir de la boucle et mettre fin au programme
        }
        // !!! provisoire pour vérifier modification des codes pin
        //println (machinePins.mkString("|"))

      } //fin du mode admin


      //mode quitter
      if (mode == 3) {
        retourmenu = 1  //vuque retour menu n'est pas = a 0, on sort de la boucle de retour au menu et le programme s'achève
      }

      //sauvgarder les modifications dans le fichier
      println()
      println("sauvgarde des machines...")
      savecsv("machines.csv", machines)

    } // fin de la boucle de retour au menu
  }
}

