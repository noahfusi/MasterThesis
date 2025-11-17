object Main {
  import io.StdIn._
  import math._
  //méthodes

  //méthode pour valider le code PIN pour la machine selectionnnée
  def validatePin(machineId: Int, machinePins: Array [String]): Boolean = {
    val tentativesMax = 3
    var tentative = 0
    for (i <- 1 to tentativesMax){
      tentative += 1
      println ("Entrez le Code PIN : ******")
      print ("> ")
       var codePinSaisi = readLine()

      if (codePinSaisi == machinePins(machineId)){
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
  def updatePin (machineId: Int, machinePins: Array[String]): Unit = {
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
    machinePins(machineId) = modifCodePIN //on remplace le PIN de la machineId par le nouveau PIN
  }

  def serveClient(machineId: Int, StockCafe: Array[Int], StockSucre: Array[Int], StockLait: Array[Int] ): Boolean = {
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
      if ((SCpoudreCafe <= StockCafe(machineId)) && (SCsucre <= StockSucre(machineId)) && (SClait <= StockLait(machineId))) {
        StockCafe(machineId) -= SCpoudreCafe
        StockSucre(machineId) -= SCsucre
        StockLait(machineId) -= SClait

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
        if (SCpoudreCafe > StockCafe(machineId)) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez sélectionner une autre machine ")
      }

       if (SCsucre > StockSucre(machineId)) {
        println("Erreur : la quantité de sucre est insuffisante pour préparer la boisson sélectionnée")
        println("Veuillez sélectionner une autre machine ")
      }

       if (SClait > StockLait(machineId)) {
        if (boisson == 2) {
          println(" Erreur : la quantité de lait est insuffisante pour préparer votre commande")
          println("Veuillez sélectionner une autre machine")
        }
        else if (boisson == 3){
          println ("Erreur : la quantité de lait est insuffisante pour préparer la boisson sélectionnée")
          println ("Veuillez sélectionner une autre machine")
        }
      }

      }
    return false

  }

  def restockMachine(machineId: Int, StockCafe: Array[Int], StockSucre: Array[Int], StockLait: Array[Int]) : Unit = {
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

      StockCafe(machineId) += reapprovisCafe
      StockLait(machineId) += reapprovisLait
      StockSucre(machineId) += reapprovisSucre


  }


  def main(args: Array[String]): Unit = {


    val NbMachines = 5
    //tableau: chaque machine a son identifiant qui correspond ici à son index
    //initialisation des variables de la machine et de son identifiant
    var machineSelectionnee = 0
    var machineId = machineSelectionnee - 1

    //Tableau : initiation des valeurs de stock initiaux pour les 5 machines
    var StockCafe = Array.fill(NbMachines)(50) // 50g de cafe par machine
    var StockSucre = Array.fill(NbMachines)(30) // 30 g de sucre par machine
    var StockLait = Array.fill(NbMachines)(500) //500ml de lait par machine

    //tableau : initialisation code PIN
    var machinePins = Array.fill (NbMachines)("434343") //par défaut, chaque machine a comme code PIN 434343, peut etre modifié par la suite


    //boucle de retour au menu
    var retourmenu = 0
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

          serveClient(machineId, StockCafe, StockSucre, StockLait)

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
        if (validatePin(machineId, machinePins)) { //si la méthode validatePin renvoie la valeur true --> accès autorisé
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
            println("Poudre de café : " + StockCafe(machineId) + "g")
            println("Lait           : " + StockLait(machineId) + "ml")
            println("Sucre          : " + StockSucre(machineId) + "g")
            println()

            restockMachine(machineId, StockCafe, StockSucre, StockLait)
            //verification des stocks :
            //println()
            //println (" stock café " + StockCafe.mkString("|"))
            //println (" stock lait " + StockLait.mkString("|"))
            //println (" stock sucre " + StockSucre.mkString("|"))




          }
          else {
            //l'utilisateur choisit de modifier le code PIN
            println ("Mise à jour du code PIN pour la machine " + machineSelectionnee)
            updatePin(machineId, machinePins)
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
    } // fin de la boucle de retour au menu
  }
}




