import Main.{updatePin, validatePin}

import scala.Console.println
import scala.io.StdIn.readLine

object Main {
  import io.StdIn
  import scala.util.Random
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    print("Entrez le code PIN : ")
    var UInput = StdIn.readLine()
    var tries = 2
    while((UInput != machinePins(machineId)) && (tries != 0)){             // Boucle contrôle de PIN
      println(s"Code PIN incorrect. $tries tentatives restantes.")
      UInput = StdIn.readLine()
      tries -= 1
    }
    if(tries == 0 && UInput != machinePins(machineId)){
      println(s"Code PIN incorrect. $tries tentatives restantes.")
      println("Trop de tentatives ´echou´ees. Fin du programme.")
      return false
    }
    else {
      println("Accès accordé.")
      return true
    }
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise `a jour du code PIN pour la Machine ${machineId+1}.")
    print("Entrez un nouveau code PIN `a 6 chiffres >")
    var nouveaucode = readLine()
    while((nouveaucode.length) != 6 || !nouveaucode.forall(_.isDigit)){
      print("Entrez un nouveau code PIN `a 6 chiffres >")
      nouveaucode = readLine()
    }
    machinePins(machineId) = nouveaucode
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean ={

    // Variables de stock (à l'état initial) :
    var StockCafe = coffeeStocks(machineId)
    var StockSucre = sugarStocks(machineId)
    var StockLait = milkStocks(machineId) // en mL
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
      if(UtilisationPoudre > StockCafe){
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        error = true
        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0
      }
      if(UtilisationLait > StockLait){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        error = true
        UInput = 0
        AjoutDeSucre = 0
        UtilisationLait = 0
        UtilisationPoudre = 0

        PrixLait = 0
        PrixSucre = 0
      }
      if(AjoutDeSucre > StockSucre){
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
        StockCafe -= UtilisationPoudre
        coffeeStocks(machineId) = StockCafe
        StockLait -= UtilisationLait
        milkStocks(machineId) = StockLait
        StockSucre -= AjoutDeSucre
        sugarStocks(machineId) = StockSucre


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
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    //Affichage stocks et MaJ du stock manuellement - ATTENTION Input et affichange en Litres mais variable stock de lait en mL
    println("Stocks: ")
    println(f"Poudre de café: ${coffeeStocks(machineId)} g\nLait          : ${milkStocks(machineId)*0.001}%.2f L\nSucre         : ${sugarStocks(machineId)} g")

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

    coffeeStocks(machineId) += ReapPoudre
    milkStocks(machineId) += (ReapLait*1000).toInt
    sugarStocks(machineId) += ReapSucre

    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }
  def main(args: Array[String]):Unit = {
    var nbMachines = 5
    var machinePins = Array.fill(nbMachines)("434343")
    var coffeeStocks = Array.fill(nbMachines)(50)
    var sugarStocks = Array.fill(nbMachines)(30)
    var milkStocks = Array.fill(nbMachines)(500)
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
          while(machineId <0 || machineId >4) {
            println("Machine sélectionnée (1-5) >")
            machineId = readLine().toInt - 1
          }
          serve = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          if(serve == false){
            machineId = -1
              println("Veuillez choisir une autre machine")
            }
          }

      }
      if(choix_menu== 2){
        machineId = -1
        while(machineId <0 || machineId >4) {
          println("Machine sélectionnée (1-5) >")
          machineId = readLine().toInt - 1
        }
        var error = validatePin(machineId,machinePins)
        if(error == true){
          var menu_admin = 0
          while(menu_admin != 1 && menu_admin!= 2){
            print("Veuillez sélectionner votre mode : \n1) modifier le pin\n2) restockage\n>")
            menu_admin = readLine().toInt
          }
          if (menu_admin == 1){
            updatePin(machineId, machinePins)
          }
          if(menu_admin==2){
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
        }
        if(error == false){
          choix_menu = 3
        }
      }
    }


  }
}