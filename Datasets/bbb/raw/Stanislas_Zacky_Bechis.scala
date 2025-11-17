import scala.io.StdIn
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var fin = 0
    var choix = "0"
    var choixb = 0
    var sugar = 0
    var lait = 0.0
    var laitselection = "Non"
    var selection = "Cappucino"
    var taille = 0
    var price = 0.0
    var essai = 3
    var ajouts = 0.0
    var dose = 0.0
    var x = 0
    var machinePins = Array("434343","434343","434343","434343","434343")
    var machineID = 0
    var ID = machineID + 1
    var coffeeStocks = Array(50, 50, 50, 50, 50) // Stocks de café pour 5 machines
    var sugarStocks = Array(30, 30, 30, 30, 30)   // Stocks de sucre
    var milkStocks = Array(0.500, 0.500, 0.500, 0.500, 0.500)      // Stocks de lait
    def demanderMachineID(): Int = {
      var idValide = false
      var resultat = -1

      while (!idValide) {
        println("Quelle machine utilisez-vous (1-5) >")
        val input = readLine()

        try {
          val entree = input.toInt // Conversion de l'entrée en entier
          if (entree >= 1 && entree <= 5) {
            resultat = entree - 1
            idValide = true
          } else {
            println("Erreur : ID de machine invalide. Veuillez entrer un chiffre entre 1 et 5.")
          }
        } catch {
          case _: NumberFormatException =>
            println("Erreur : Veuillez entrer uniquement des chiffres entre 1 et 5.")
        }
      }

      resultat
    }
    def demanderChiffreVirgule(): Double = {
      var nombreValide = false
      var resultat = 0.0

      while (!nombreValide) {
        println("Veuillez entrer un nombre positif (entier ou à virgule) :")
        val input = scala.io.StdIn.readLine()

        if (input.matches("\\d+(\\.\\d+)?")) { // Vérifie si l'entrée est un nombre entier ou décimal
          resultat = input.toDouble
          nombreValide = true
        } else {
          println("Erreur : Veuillez entrer uniquement des nombres valides (ex. 5 ou 3.14) !")
        }
      }
      resultat
    }


    def demanderChiffre(): Int = {
      var chiffreValide = false
      var resultat = 0

      while (!chiffreValide) {
        println("Veuillez entrer un chiffre entier positif :")
        val input = StdIn.readLine()

        if (input.matches("\\d+")) {
          resultat = input.toInt
          chiffreValide = true
        } else {
          println("Erreur : Veuillez entrer uniquement des chiffres !")
        }
      }
      resultat
    }
    def gestiondestock (machineID: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int],
                        milkStocks: Array[Double]): Unit = {
      println(s"Voici les stock de la machine $ID")
      println(s"stock de café ${coffeeStocks(machineID)}")
      println(s"stock de lait: ${milkStocks(machineID)}")
      println(s"stock de sucre ${sugarStocks(machineID)}")
      println("Réapprovisionnement des stocks...")
      println("de combien voulez vous réapprovisionner la machine en poudre de café (taper 0 si vous ne vouler par rajouter dans ce stock ")
      var ajout = demanderChiffre()
      coffeeStocks(machineID) += ajout
      println("de combien voulez vous réapprovisionner la machine en Lait (taper 0 si vous ne vouler par rajouter dans ce stock ")
      ajouts = demanderChiffreVirgule()
      milkStocks(machineID) = milkStocks(machineID) + ajouts

      println("de combien voulez vous réapprovisionner la machine en Sucre (taper 0 si vous ne vouler par rajouter dans ce stock ")
      ajout= demanderChiffre()
      sugarStocks(machineID) = sugarStocks(machineID) + ajout
      println("Niveaux de stock mis à jour.")
      println("Retour au menu principal...")

    }

    def generateRandomCode(length: Int): String = {
      val alphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      Random.alphanumeric.filter(alphanum.contains(_)).take(length).mkString
    }
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      // Affiche un message pour inviter l'utilisateur à saisir le code PIN
      println(s"Veuillez entrer le code PIN pour la machine $ID :")
      val PIN = readLine()

      // Vérifie si le code PIN saisi correspond à celui stocké pour la machine donnée
      if (PIN == machinePins(machineId)) {
        println("Code PIN correct.")
        true
      } else {
        println(s"Code PIN incorrect. $essai  tentatives restantes.")

        if (essai == 0) {
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(0)
        }
        essai = essai -1
        false
      }
    }
    def updatePin(machineID: Int, machinePins: Array[String]): Unit = {
      var isValid = false
      while (!isValid) {
        // Invite l'administrateur à saisir un nouveau code PIN
        println(s"Veuillez entrer un nouveau code PIN (6 chiffres) pour la machine $ID :")
        val nouveauPin = readLine()

        if (nouveauPin.matches("^\\d{6}$")) {
          machinePins(machineID) = nouveauPin // Mise à jour du tableau machinePins
          println("Le code PIN a été mis à jour avec succès.")
          isValid = true // Sortie de la boucle
        } else {
          println("Erreur : Le code PIN doit contenir exactement 6 chiffres. Veuillez réessayer.")
        }
      }
    }

    def serveClient (machineID: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int],
                     milkStocks: Array[Double]): Boolean = {
      while (choixb !=1 && choixb != 2 && choixb !=3){
        println("\nSelectionnez votre Boisson : :\n  1) Expresso - CHF 2.00 \n  2) Cappuccino - CHF 2.50\n  3) Latte CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n ")
        choixb = readInt()

      }
      if (choixb == 3) {
        while (taille !=1 && taille != 2 && taille !=3){
        println("Quelle taille ? \n 1) Latte CHF 2.70 (Petit) \n 2) CHF 3.20 (Moyen) \n 3) CHF 3.70 (Grand)" )

        taille = readInt()

      }}
      while (sugar !=1 && sugar != 2 && sugar !=3){
      println("\nSouhaitez-vous ajouter du sucre ?\n  1) Sans sucre \n  2) Peu (5g) - CHF 0.10\n  3) Moyen (10g) - CHF 0.20\n 4) Beaucoup (15g) - CHF 0.30\n ")
      sugar = readInt()}
      var sugarlv = "Sans sucre"
      var sugarprice = 0.0
      var sugarS = 0
      if (sugar == 2) {
        sugarlv = "Peu (5g)"
        sugarprice = 0.10
        sugarS = 5


      }
      if (sugar == 3) {
        sugarlv = "Moyen (10g)"
        sugarprice = 0.20
        sugarS = 10

      }
      if (sugar == 4) {
        sugarlv = "Beaucoup (15g)"
        sugarprice = 0.30
        sugarS = 15

      }
      if (choixb == 2 || choixb == 3){
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("\n 1) Oui \n 2) Non \n")
        lait = readInt()
        if (lait == 1){
          println("Combien de dose ? 1 , 2 ou 3 ?")
          x = readInt()
          if (x==1){dose = 0.050}
          if (x==2){dose = 0.10}
          if (x==3){dose = 0.150}
          lait = 0
          laitselection = "Oui"

        }

      }
      if (choixb == 1 ){selection = "Expresso"
        price = 2.00

      }

      if (choixb == 2 ){selection = "Cappuccino"
        price = 2.50
      }
      if (choixb == 3 ){selection = "Latte"
        if (taille == 1){ price= 2.70}
        if (taille == 2){ price= 3.20}
        if (taille == 3){ price= 3.70}

      }
      var laitprice = 0.0
      if (x==1) laitprice = 0.05
      if (x==2) laitprice = 0.10
      if (x==3) laitprice = 0.15



      val prixtot = price + sugarprice + laitprice
      if(selection =="Latte") {
        if (taille == 1) {
          println("Boissons sélectionnée :Latte (petit)")
        }
        if (taille == 2) {
          println("Boissons sélectionnée :Latte (Moyen)")
        }
        if (taille == 3) {
          println("Boissons sélectionnée :Latte (Grand)")
        }
      }

      if(selection != "Latte"){println("Boissons sélectionnée :" + selection  )}
      println ("niveau de sucre :" + sugarlv)
      println("Lait supplémentaire :" + laitselection )

      if (choix=="1" ) {
        if(sugarStocks(machineID) < sugarS) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
          choix = "0"
          choixb = 0
          return false

        }}
      if (choix == "2") {
        if(sugarStocks(machineID) < sugarS) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
          choix = "0"
          choixb = 0
          return false

        }
      }
      if (choix =="3" ) {
        if(sugarStocks(machineID) < sugarS) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
          choix = "0"
          choixb = 0
          return false

        }
      }



      if (choix=="1" && coffeeStocks(machineID) < 8) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        choix = "0"
        choixb = 0
        return false
      }
      if (choix == "2" && coffeeStocks(machineID) < 6) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
        choix = "0"
        choixb = 0
        return false
      }
      if (choix =="3" ) {
        if(taille == 1 && coffeeStocks(machineID) < 6){
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
          choix = "0"
          choixb = 0
          return false
        }
        if(taille == 2 && coffeeStocks(machineID) < 8){
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
          choix = "0"
          choixb = 0
          return false
        }
        if(taille == 3 && coffeeStocks(machineID) < 12){
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
          choix = "0"
          choixb = 0
          return false
        }
      }

      if (choix=="1" ) {
        if (milkStocks(machineID) < dose){
          println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          choix = "0"
          choixb = 0
          return false
        }}
      if (choix == "2" ) {
        if (milkStocks(machineID) < (dose + 0.100)){
          println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          choix = "0"
          choixb = 0
          return false
        }}
      if (choix =="3" ) {
        if(taille == 1 ){
          if (milkStocks(machineID) < (dose + 0.100)){
            println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            choix = "0"
            choixb = 0
            return false
          }}
        if(taille == 2 ){
          if (milkStocks(machineID) < (dose + 0.150)){
            println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            choix = "0"
            choixb = 0
            return false
          }}
        if(taille == 3 ){
          if (milkStocks(machineID) < dose + 0.200){
            println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            choix = "0"
            choixb = 0
            return false
          }}
      }
      if (choix != "0") {

        println("Prix total : CHF " + price +" + " +sugarprice +" CHF  +  " +laitprice +" CHF = "+ prixtot + " CHF "  )



        val paymentCode = generateRandomCode(5)
        println ("Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + paymentCode)
        println( "En attente de validation du paiement...\n")
        Thread.sleep(4000)
        println("Merci ! Votre paiement a été accepté")
        choix = "0"
        choixb = 0
        if (serveClient(machineID: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int],
          milkStocks: Array[Double])){
        if (choixb == 1){
          coffeeStocks(machineID) = coffeeStocks(machineID) - 8
        }
        if (choixb == 2){
          coffeeStocks(machineID) = coffeeStocks(machineID) - 6
          milkStocks(machineID) = milkStocks(machineID) - 0.100 - dose
        }
        if (choixb == 3){
          if (taille == 1){
            coffeeStocks(machineID) = coffeeStocks(machineID) - 6
            milkStocks(machineID) = milkStocks(machineID) - 0.100 - dose
          }
          if (taille == 2){
            coffeeStocks(machineID) = coffeeStocks(machineID) - 8
            milkStocks(machineID) = milkStocks(machineID) - 0.150 - dose
          }
          if (taille == 3){
            coffeeStocks(machineID) = coffeeStocks(machineID) - 12
            milkStocks(machineID) = milkStocks(machineID) - 0.200 - dose
          }
        }
        dose = 0
        taille = 0

        if (sugar != 1) {
          sugarStocks(machineID) = sugarStocks(machineID) - sugarS
          sugar = 0
        }
        println("Préparation de votre boisson...\n")
        println ("[...]")
        Thread.sleep(5000)
        println("Votre " + selection + " est prêt ! Bonne dégustation !")
        choixb = 0





      }}
      return true

    }





    while (fin == 0) {
     demanderMachineID()

      while (choix !="1" && choix !="2" && choix !="3"){
        println()
        println("\nSelectionnez votre mode : :\n  1) Mode Client \n  2) Mode Admin\n  3) Quitter\n ")
        //possibilité d'afficher de mettre un msg d'erreur non demandé
        choix = readLine()
        if (choix == "3"){ fin = 1}
      }
      if (choix == "2")
      {var essai = 0
        while (!validatePin(machineID,machinePins)) {
        validatePin(machineID,machinePins)
          essai += 1
        if (essai == 3) {
          println("Trop de tentatives échouées. Fin du programme.")
          System.exit(0)
        }
      }

          println("Accès autorisé.")
        essai = 0
          println("1) Changement de PIN \n  2) Réapprobisionnement des stocks")
          var pinorstock =readInt()
        while (pinorstock != 1 && pinorstock !=2){
        println("Veuillez effectuer une entrée correcte")
        println("1) Changement de PIN \n  2) Réapprobisionnement des stocks")
          pinorstock = readInt()

      }
        if (pinorstock== 1){
          updatePin(machineID,machinePins)
        }
        if (pinorstock==2){

        gestiondestock(machineID,coffeeStocks,sugarStocks,milkStocks)


      } }
      if (choix == "1") {
        serveClient (machineID: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int],
          milkStocks: Array[Double])


        }}



    }
  }

