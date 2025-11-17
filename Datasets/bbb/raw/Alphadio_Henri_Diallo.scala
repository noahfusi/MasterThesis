import scala.io.StdIn._
import util.Random

object Main {
    // Validation du code Pin
  def validatePin(machineId: Int, machinePins:Array[String]): Boolean = {
    println("Veuillez sélectionner le code PIN. ")
    val codePin = readLine(">")
    if(codePin==machinePins(machineId-1)){
      true
    }
    else{
      false
    }
  }
    // Actualisation du code Pin
  def updatePin(machineId:Int,machinePins:Array[String]):Unit = {
    println("Veuillez entrer un nouveau code Pin")
    var nouveaucodePin = readLine(">")
    while(nouveaucodePin.length!=6){
      println("Valeur erronée. Veuillez entrer un code à 6 chiffres.")
      nouveaucodePin = readLine(">")
    }
    if(nouveaucodePin.length==6){
      machinePins(machineId-1) = nouveaucodePin
      println("Le code PIN a été mis à jour avec succès")
      println("Retour au menu principal")
    }
  }
    // definition pour servire le client
  def serveClient(machineId:Int, StockinitialCafe:Array[Int],StockinitialSucre:Array[Int],StockinitialLait:Array[Int]): Unit = {
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

    // Boucle d'érreur, si la sélection de boisson est érronner

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

      // Boucle d'érreure pour le sucre
      while ((sucre != 21) && (sucre != 22)) {
        println(" Valeure érroner, veulliez en choisir une bonne. ")
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

        // Boucle d'érreur, si la sélection de la quantité est érronner
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
            println("Valeure érroner, veuillez en choisir une bonne. ")
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
            println("Vous ajouter deux dose de lait à votre café. ")
            PrixLait = 0.05F * 2.00F
            StockLait += 100
          } else if (Dose == 3) {
            println("Vous ajouter trois dose de lait à votre café. ")
            PrixLait = 0.05F * 3.00F
            StockLait += 150
          }
        }
      }
    }

    // Paiement, si le stock est suffisant
    if((StockinitialCafe(machineId-1) >= PoudredeCafe) && (StockinitialSucre(machineId-1) >= Stocksucre) && (StockinitialLait(machineId-1) >= StockLait)){
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
    }
    // Déduction des stock
    StockinitialCafe(machineId-1) -= PoudredeCafe
    StockinitialLait(machineId-1) -= StockLait
    StockinitialSucre(machineId-1) -= Stocksucre

    // Si stock inssuffisant
    if ((StockinitialCafe(machineId-1) < PoudredeCafe) || (StockinitialSucre(machineId-1) < Stocksucre) || (StockinitialLait(machineId-1) < StockLait)) {

      println("Le stock est insufisant. Veuillez choisir une autre machine. ")
    }
  }
    // definition pour actualiser les stock des machine
  def restockMachine(machineId:Int,StockinitialCafe:Array[Int],StockinitialSucre:Array[Int],StockinitialLait:Array[Int]):Unit = {
    println("Niveaux de stocks actuels : ")
    println("Café : " + StockinitialCafe(machineId-1)+"g" )
    println("Sucre : " +StockinitialSucre(machineId-1)+"g" )
    println("Lait : " + StockinitialLait(machineId-1)+"ml" )

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

    StockinitialCafe(machineId-1) += AjoutCafe
    StockinitialLait(machineId-1) += AjoutLait
    StockinitialSucre(machineId-1) += AjoutSucre
    println("Les stocks ont été mis à jour avec succès")
    println("Retour au menu principal...")
  }
  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val ModeClient = 1
    val ModeAdmin = 2
    val Quitter = 3
    val StockinitialCafe = Array(50,50,50,50,50)
    val StockinitialSucre = Array(30,30,30,30,30)
    val StockinitialLait = Array(500,500,500,500,500)
    var choixdumode = 0
    var Tentatives = 1
    val Machine1 = 1
    val Machine2 = 2
    val Machine3 = 3
    val Machine4 = 4
    val Machine5 = 5
    val machinePins = Array.fill(nbMachines)("434343")

    while(choixdumode != Quitter){

      // Sélection du mode
      println("       Nospresso Café ")
    println("Veuillez sélectionner votre mode")
    println(" 1) Client ")
    println(" 2) Admin ")
    println(" 3) Quitter ")
    print(" > ")
     choixdumode = readInt()

      // Boucle d'érreur, si le choix du mode éronner
    while ((choixdumode != ModeClient) && (choixdumode != ModeAdmin) && (choixdumode != Quitter)) {
      println(" La valeure choisie est érroner, veuillez en choisir une correcte. ")
      println("Veuilliez sélectionner votre mode.")
      println(" 1) Client ")
      println(" 2) Admin ")
      println(" 3) Quitter ")
      print(" > ")
      choixdumode = readInt()

    }
    if (choixdumode == ModeClient) {
      println(" Vous avez selectionner le mode client.")


      // Demande choix de la machine avec les tableaux
      println("Machine séléctionée 1-5 ")
      print(" >")
      var machineId = readInt()

      // Tant Que Choix Machine Pas Valable
      while ((machineId != Machine1) && (machineId != Machine2) && (machineId != Machine3) && (machineId != Machine4) && (machineId != Machine5) ){
        println("La valeur saisie est éronnée, veuillez en choisir une adéquate. ")
        print(">")
        machineId = readInt
      }
      if(machineId == Machine1 ) {
        println("Machine sélectionner 1")
      }
      if( machineId == Machine2 ) {
        println("Machine sélectionner 2")
      }
      if( machineId == Machine3 ) {
        println("Machine sélectionner 3")
      }
      if( machineId == Machine4 ) {
        println("Machine sélectionner 4")
      }
      if( machineId == Machine5 ) {
        println("Machine sélectionner 6")
      }
    serveClient(machineId, StockinitialCafe, StockinitialSucre, StockinitialLait)
    }

      // Mode admin
      if (choixdumode == ModeAdmin) {
          println("Vous avez selectionner le mode admin.")


        println("Machine séléctinée 1-5 ")
        print(" >")
        var machineId = readInt()

        // Tant Que Choix Machine Pas Valable
        while ((machineId != Machine1) && (machineId != Machine2) && (machineId != Machine3) && (machineId != Machine4) && (machineId != Machine5) ){
          println("La valeur saisie est éronnée, veuillez en choisir une adéquate. ")
          print(">")
          machineId = readInt
        }
        if( machineId == Machine1 ) {
          println("Machine sélectionner 1")
        }
        if( machineId == Machine2 ) {
          println("Machine sélectionner 2")
        }
        if( machineId == Machine3 ) {
          println("Machine sélectionner 3")
        }
        if( machineId == Machine4 ) {
          println("Machine sélectionner 4")
        }
        if( machineId == Machine5 ) {
          println("Machine sélectionner 5")
        }

      // Tentatives puor le code Pin
        while(Tentatives!=4){
          val validerPIN = validatePin(machineId, machinePins)
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
            println("62) Changer les stocks")
            print(">")
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
              updatePin(machineId,machinePins)
            }
            else if(choix==62){
              restockMachine(machineId,StockinitialCafe,StockinitialSucre,StockinitialLait)
            }
          }
        }
      }
      Tentatives = 1

      }
    // Quitter
    if (choixdumode == Quitter) {
      println("Vous avez quitté le programme. ")
    }

  }
}



