import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import math._

object Main {
  def main(args: Array[String]): Unit = {

    var nbMachine = 5 // Nombre de machine

    // Création des tableaux
    val machinePins = Array.fill(nbMachine)("434343")
    val coffeeStocks = Array.fill(nbMachine)(50)
    val sugarStocks = Array.fill(nbMachine)(30)
    val milkStocks = Array.fill(nbMachine)(500)

    var machineId = 0 // Numero de la machine selectionnée
    var tentativePin = 3

    var mode = 0

    var poudrecafeinitial = 0
    var laitinitial = 0
    var sucreinitial = 0

    // ---FONCTION--------------------------------------------------------------------------------------------

    // Cette fonction verifie l'input donné par l'User et correspond
    // au code Pin de la machine selectionnee
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean ={
      println("Entrez le pin de la machine n°"+ machineId +": ")
      val inputPin = readLine()

      // Verifie que le Pin donne par l'User correspond a celui de la machine selectionnee
      if(inputPin == machinePins(machineId-1)) {
        println("Accès accordé à la machine n°"+ machineId)
        return true
      }else{
        println("Accès non autorisé!")
        return false
      }
    }

    // Affiche les stocks de la machine donnée
    def displayStock(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit={
      println()
      println("Niveaux du stocks actuel de la machine n°"+ machineId)
      println(s"Proudre de café : ${coffeeStocks(machineId-1)}g")
      println(s"Sucre : ${sugarStocks(machineId-1)}g")
      println(s"Lait : ${milkStocks(machineId-1)}ml")
      println()
    }

    // Renvoi la une valeur Int
    // Cela correspond a la quantité que l'Admin souhaite rajouter
    def quantiteIngredient(ingredient: String, machineId: Int): Int = {
      var quantite = 0
      do{
        if (quantite < 0){
          println("Erreur valeur négative: "+ ingredient +" Quantité impossible. SVP donner une valeur positive ou nulle")
        }

        println("Quantité de "+ ingredient +" à rajouter dans la machine n°"+ machineId +": ")
        quantite = readInt()
      }while(quantite < 0)


      return quantite
    }

    // Cette fonction ajoute du stock en plus à une machine donnée (machineId)
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println()
      println("Ajout stock dans la machine n°"+ machineId)

      displayStock(machineId, coffeeStocks, sugarStocks, milkStocks)

      var coffee = quantiteIngredient("Poudre de café", machineId)
      coffeeStocks(machineId-1) = coffee

      var sugar = quantiteIngredient("Sucre", machineId)
      sugarStocks(machineId-1) = sugar

      var milk = quantiteIngredient("Lait", machineId)
      milkStocks(machineId-1) = milk

      println("Les stocks ont été mise à jour avec succès.")
      println()
    }

    // Modifier le Pin de la machine selectionner
    def updatePin(machineId: Int, machinePins: Array[String]): Unit ={
      var reglePin = false

      println("Mise à jour du code PIN pour la machine n°"+ machineId)
      println()
      do{
        println("Entrez le nouveau pin de la machine: ")
        val inputPin = readLine()

        reglePin = ((inputPin.length == 6)  && inputPin.forall(_.isDigit))

        // Ici j'utilise une fonction externer jsp si j'ai le droit
        if (reglePin == true){
          machinePins(machineId-1) = inputPin
          println("Le code PIN a été mis à jour avec succès")
        }else{
          println("Erreur : Le code PIN donné ne respecte pas la règle.")
          println("Le code PIN doit avoir 6 chiffres ")
          println()
        }
      }while(reglePin != true)
    }


    def paiementTwint(totalprix: Double): Unit={

      println(f"Prix total: CHF $totalprix%.2f. Veuillez payer en utilisant Twint.")

      val lettre = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var aleatoire = ""

      for (i <- 1 to 5) {
        aleatoire += lettre((Math.random() * lettre.length).toInt)
      }
      println("Votre code de paiement est:"+ aleatoire +" \n(En attente de validation du paiement)")
      Thread.sleep(3000)

      println("Merci ! Votre paiement a été accepté.")
      Thread.sleep(3000)
      println("Préparation de votre boisson...\n[...]")
      Thread.sleep(3000)

      println("Votre boisson est prête ! Bonne dégustation !")
      Thread.sleep(5000)
    }


    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

      var coffee = 0
      var sugar = 0
      var milk = 0
      var prix =  0.0
      var choixBoisson = 0

      do {
        println("Veuillez sélectionner votre boisson:\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
        choixBoisson =  readInt()
        println()
      }
      while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3)


      do {
        println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre - CHF 0.00\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
        sugar = readInt()
      }while (sugar != 1 && sugar != 2 && sugar != 3 && sugar != 4)
      var sucreutilise = 5 * (sugar - 1)
      var prixdusucre = (sugar - 1) * 0.10


      var dose = 0
      var prixLait = 0.0
      if (choixBoisson == 2 || choixBoisson == 3) {
        var supplementLait = 0
        do {
          println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui \n2) Non\n>")
          supplementLait = readInt()
        }while (supplementLait != 1 && supplementLait != 2)

        if (supplementLait == 1) {
          do {
            println("Combien de dose ?\n1) 1 dose (50mL) - CHF 0.05\n2) 2 dose (100mL) - CHF 0.10\n3) 3 dose (150mL) - CHF 0.15\n>")
            dose = readInt()
          }
          while (dose != 1 && dose != 2 && dose != 3)
          prixLait = dose * 0.05
        }
      }

      if (choixBoisson == 1) {
        coffee = 8
        prix = 2.00
      }
      else if(choixBoisson == 2) {
        coffee = 6
        milk = 100
        prix = 2.50
      }
      else if (choixBoisson == 3) {
        var taille = 0
        do {
          println("Veuillez choisir votre taille:\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>")
          taille = readInt()
        }
        while (taille != 1 && taille != 2 && taille != 3)

        if (taille == 1) {
          coffee = 6
          milk = 120
          prix = 2.70

        } else if (taille == 2) {

          coffee = 8
          milk = 150
          prix = 3.20

        } else if (taille == 3) {
          coffee = 12
          milk = 200
          prix = 3.70
        }
      }
      milk = milk + dose * 50


      if ((coffeeStocks(machineId-1)- coffee) < 0){
        println("Erreur: Quantité de café insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
        return true
      }

      if ((sugarStocks(machineId-1)- sugar) < 0){
        println("Erreur: Quantité de sucre  insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
        return true
      }

      if ((milkStocks(machineId-1)- milk) < 0){
        println("Erreur : Quantité de lait  insuffisante pour préparer la boisson selectionnée.\nVeuillez choisir une autre machine\n")
        return true
      }

      var prixTotal = prix + prixdusucre + prixLait
      paiementTwint(prixTotal)

      coffeeStocks(machineId-1) -= coffee
      sugarStocks(machineId-1) -= sugar
      milkStocks(machineId-1) -= milk


      return false
    }

    // ---FIN - FONCTION--------------------------------------------------------------------------------------------




    while (mode == 0 || mode == 1 || mode == 2){

      if (mode == 0){

        do {
          println("Noespresso Café\nVeuillez sélectionner votre mode:\n1) Client\n2) Admin\n3) Quitter\n>")
          mode = readInt()
          println()
        }
        while (mode != 1 && mode != 2 && mode != 3)
      }

      if (mode == 1) {
        do{
          println("Entrer l'Id d'une machine: ")
          machineId = readInt()
          println()

        }while ((machineId< 1) || (machineId > nbMachine))

        var nbError = 0
        var error = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)

        if (error){
          nbError = 1
        }

        // car il faut lui redonner un nouveau id
        // if (error == true){
        //     nbError +=1
        //     println(" Veuillez selectionner une autre machine")

        //     machineId = (math.random * n).toInt + 1

        //     erreur = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        // }

        if (error == true){
          mode = 1
        }
        else{
          println("Retour au menu principal...")
          println()
          mode = 0
        }

      }
      else if (mode == 2) {

        // SCENARIOS
        // 1 & 2
        do{
          println("Mode Admin\nEntrer l'Id d'une machine: ")
          machineId = readInt()
          println()

        }while ((machineId< 1) || (machineId > nbMachine))




        var pidIncorret = 0
        var valide = false

        do {
          valide = validatePin(machineId, machinePins)

          if(valide == false){
            pidIncorret += 1
            println("Code PIN incorret."+ (tentativePin-pidIncorret) +" tentatives restantes.")
            println()
          }
          else{
            valide = true

          }
        }while(valide != true && pidIncorret != 3)

        if (pidIncorret == 3){
          println("Trop de tentatives échouées. Fin du programme.")
          println()
          mode=3
        }else{

          var action = 0

          do {
            println("Bonjour que voulez vous faire:\n1) AJouter du stocks a la machine n°"+ machineId +("\n2) Modifier le PIN de la machine n°"+ machineId)+"\n3) Quitter")
            println()
            action = readInt()

          }while (action != 1 && action != 2 && action != 3)


          if(action == 1){ // Ajouter du stock
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            println("Retour au menu principal...")

          }
          else if (action == 2){ // Changer le PIN
            updatePin(machineId, machinePins)

          }else if (action == 3){
          }
          mode=0
        }

      }
      else{
        println("Quitter!")
      }
    }
  }
}









