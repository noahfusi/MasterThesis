import java.io.PrintWriter
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.io.StdIn.readLine

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {


  //Methode add-----------------------------------------------------
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    } else if (ingredient == "milk") {
      milk += amount
    }
  }
  //Fin methode add-----------------------------------------------------

  //Methode remove--------------------------------------------------------------
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      true

    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      true

    } else if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      true

    } else {
      false
    }
  }

  //Fin methode remove------------------------------------------------------------
}

object Main {
  import io.StdIn
  import scala.util.Random

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val lines = Source.fromFile(filename).getLines()
      val lignes_enArray = lines.toArray
      var i = 1
      while (i < lignes_enArray.length) {
        val parties = lignes_enArray(i).split(",")
        if (parties.length == 4) {
          machines += new Machine(i, parties(0), parties(1).toInt, parties(2).toInt, parties(3).toInt)
        }
        i += 1
      }

    } catch {
      case e: Exception =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      //Itéreation sur toutes les machines
      for (machine <- machines) {

        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
      println("Sauvegarde des machines dans machines.csv...")
    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur bizarre : ${e.getMessage}")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        sys.exit(1)

    }
    println("Fichier sauvegardé avec succès ")
  }
  def validatePin(machine: Machine): Boolean = {
    print("Entrez le code PIN : ")
    var UInput = StdIn.readLine()
    var tries = 2
    while((UInput != machine.pincode) && (tries != 0)){             // Boucle contrôle de PIN
      println(s"Code PIN incorrect. $tries tentatives restantes.")
      UInput = StdIn.readLine()
      tries -= 1
    }
    if(tries == 0 && UInput != machine.pincode){
      println(s"Code PIN incorrect. $tries tentatives restantes.")
      println("Trop de tentatives ´echou´ees. Fin du programme.")
      return false
    }
    else {
      println("Accès accordé.")
      return true
    }
  }
  def updatePin(machine : Machine): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machine.id}.")
    print("Entrez un nouveau code PIN `a 6 chiffres >")
    var nouveaucode = readLine()
    while((nouveaucode.length) != 6 || !nouveaucode.forall(_.isDigit)){
      print("Entrez un nouveau code PIN `a 6 chiffres >")
      nouveaucode = readLine()
    }
    machine.pincode = nouveaucode
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }
  def serveClient(machine : Machine): Boolean ={

    // Variables de stock (à l'état initial) :

    // Variables de l'option Client
    var error = true
    var ChoixCafe = ""
    var PrixInitial = 0.0
    var PrixSucre = 0.0
    var PrixLait = 0.0
    var UtilisationPoudre = 0
    var UtilisationLait = 0
    var AjoutDeSucre = 0
    var NiveauSucreChoisi = ""
    var LaitSupplement = ""
    // Paiement
    var CodeTWINT = ""
    var PrixTotal = 0.0
    while(error){       // Boucle si manque de stock
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print("> ")
      var UInput = StdIn.readInt()

      while(!(UInput >= 1 && UInput <= 3)){ // Controle input
        print("Veuillez indiquer une option correcte.\n> ")
        UInput = StdIn.readInt()
      }

      if(UInput == 1)
      { // If  pour expresso
        ChoixCafe = "Expresso"
        PrixInitial = 2.0
        UtilisationPoudre = 8
      }

      else if (UInput == 2)
      { // If pour capuccino
        ChoixCafe = "Capuccino"
        PrixInitial = 2.50
        UtilisationPoudre = 6
        UtilisationLait = 100
      }

      else if (UInput == 3)
      { // If latte
        println ("Quelle taille ? ")
        println ("1) Petit - CHF 2.70")
        println ("2) Moyen - CHF 3.20")
        print ("3) Grand - CHF 3.70\n> ")
        UInput = StdIn.readInt()

        while (!(UInput >= 1 && UInput <= 3))
        {
          print("Veuillez indiquer une option correcte. \n> ")
          UInput = StdIn.readInt()
        }

        if(UInput == 1)
        {
          ChoixCafe = "Latte (Petit)"
          PrixInitial = 2.70
          UtilisationPoudre = 6
          UtilisationLait = 120
        } else if(UInput == 2)
        {
          ChoixCafe = "Latte (Moyen)"
          PrixInitial = 3.20
          UtilisationPoudre = 8
          UtilisationLait = 150
        }else if(UInput == 3)
        {
          ChoixCafe = "Latte (Grand)"
          PrixInitial = 3.70
          UtilisationPoudre = 12
          UtilisationLait = 200
        }

      }
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      UInput = StdIn.readInt()

      while (!(UInput >= 1 && UInput <= 4)) //Verif entrée utilisateur
      {
        println("Veuillez indiquer une option correcte.")
        UInput = StdIn.readInt()
      }
      NiveauSucreChoisi = "Sans sucre"
      if(UInput == 2)
      {
        AjoutDeSucre = 5
        PrixSucre = 0.1
        NiveauSucreChoisi = "Peu (5g)"
      } else if(UInput == 3)
      {
        AjoutDeSucre = 10
        PrixSucre = 0.2
        NiveauSucreChoisi = "Moyen (10g)"
      } else  if (UInput == 4)
      {
        AjoutDeSucre = 15
        PrixSucre = 0.3
        NiveauSucreChoisi = "Beaucoup (15g)"
      }

      if(ChoixCafe != "Expresso"){ //Traitement doses de lait
        println("Souhaitez-vous ajouter du lait en supplement ?")
        println("(Disponible uniquement pour Capuccino et Latte)")
        println("1) Oui\n2) Non")
        print("> ")
        UInput = StdIn.readInt()

        while (UInput != 1 && UInput != 2) {
          println ("Veuillez indiquer une option correcte.")
          print ("> ")
          UInput  =  StdIn.readInt()
        }
        if(UInput  == 1){
          LaitSupplement  = "Oui"
          println ("Combien de dose ? \n> ")
          UInput   =  StdIn.readInt()

          while(!(UInput>=1 && UInput <= 3)){
            println("Veuillez indiquer une option correcte.")
            print("> ")
            UInput   =  StdIn.readInt()
          }

          PrixLait = UInput * 0.05
          UtilisationLait += UInput * 50

        } else {LaitSupplement = "Non"}

      } // if pas expresso

      error = false // On sort provisoirement de la boucle, nous rentreront si il y a erreur

      // Erreurs
      if(UtilisationPoudre > machine.coffee){
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        error = true
        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0
      }
      if(UtilisationLait > machine.milk){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        error = true
        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0
      }
      if(AjoutDeSucre > machine.sugar){
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        error = true
        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0
      }
      if(error){

        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        return false
      }

      if(!error){ //Si pas erreur - Paiement
        // Mise à jour du stock :
        machine.removeIngredient("coffee", UtilisationPoudre)
        machine.removeIngredient("milk", UtilisationLait)
        machine.removeIngredient("sugar", AjoutDeSucre)



        CodeTWINT = Random.alphanumeric.take(5).mkString.toUpperCase()
        println("Boisson selectionnée :" + ChoixCafe)
        println(s"Niveau de sucre : $NiveauSucreChoisi")
        if (ChoixCafe != "Expresso") {
          println("lait supplémentaire:" + LaitSupplement)
        }
        PrixTotal = PrixLait + PrixInitial + PrixSucre
        println(f"Prix total : CHF $PrixInitial%.2f + CHF $PrixSucre%.2f + CHF $PrixLait%.2f = $PrixTotal%.2f")

        println("Veuillez payer en utilisant TWINT.")
        println("Votre code de payement est :" + CodeTWINT)
        println("En attente de payement...")
        Thread.sleep(3000)

        println("Payement confirmé.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println(s"Votre $ChoixCafe est prêt ! Bonne dégustation !")
      }


    }
    return true
  }
  def restockMachine(machine:Machine): Unit = {
    //Affichage stocks et MaJ du stock manuellement - ATTENTION Input et affichange en Litres mais variable stock de lait en mL
    println("Stocks: ")
    println(f"Poudre de café: ${machine.coffee} g\nLait          : ${machine.milk*0.001}%.2f L\nSucre         : ${machine.sugar} g")

    println("Réapprovisionnement des stocks...")

    print("Poudre de café > ")
    var ReapPoudre = StdIn.readInt()
    print("Lait (en L)>")
    var ReapLait = StdIn.readDouble()
    print("Sucre >")
    var ReapSucre = StdIn.readInt()

    println("Ajout :")
    println(s"\tPoudre de cafe : $ReapPoudre g")
    println(s"\tLait           : " + ReapLait + "L")
    println(s"\tSucre          : $ReapSucre g")
    machine.addIngredient("milk", (ReapLait*1000).toInt)
    machine.addIngredient("coffee", ReapPoudre)
    machine.addIngredient("sugar", ReapSucre)


    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }
  def main(args: Array[String]):Unit = {
    val nom_du_fichier = "machines.csv"
    val machines = loadcsv(nom_du_fichier)
    println("\nMachines chargées :")
    for (machine <- machines) {
      println(s"Machine ${machine.id} chargée :")
      println(f"\tID: ${machine.id}")
      println(f"\tCode PIN: ${machine.pincode}")
      println(f"\tLait: ${machine.milk.toDouble / 1000}%.3fL")
      println(f"\tSucre: ${machine.sugar}g")
      println(f"\tCafé: ${machine.coffee}g")
      println()
    }

    var machineId = 0
    var choix_menu = 0
    while(choix_menu != 3){
      choix_menu = 0
      while(choix_menu<1 || choix_menu>3){
        print("Veuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n>")
        choix_menu = readLine().toInt
      }
      if(choix_menu == 1){
        machineId = -1
        var serve = false
        while (serve == false) {
          machineId = -1
          while(machineId <0 || machineId > machines.length - 1) {
            println(s"Machine sélectionnée (1-${machines.length}) >")
            machineId = readLine().toInt - 1

          }
          val machine = machines(machineId)
          serve = serveClient(machine)
          if(serve == false){
            machineId = -1
            println("Veuillez choisir une autre machine")
          }
        }

      }
      if(choix_menu== 2){
        machineId = -1
        while(machineId <0 || machineId >machines.length - 1) {
          println(s"Machine sélectionnée (1-${machines.length}) >")
          machineId = readLine().toInt - 1
        }
        val machine = machines(machineId)

        var error = validatePin(machine)
        if(error == true){
          var menu_admin = 0
          while(menu_admin != 1 && menu_admin!= 2){
            print("Veuillez sélectionner votre mode : \n1) modifier le pin\n2) restockage\n>")
            menu_admin = readLine().toInt
          }
          if (menu_admin == 1){
            updatePin(machine)
          }
          if(menu_admin==2){
            restockMachine(machine)
          }
        }
        if(error == false){
          choix_menu = 3
        }
      }
    }
    savecsv(nom_du_fichier, machines)


  }
}