import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
// read\write csv file
import java.io.{PrintWriter,FileWriter}
import scala.io.Source



object Main {
  // variable indiquer s'il faut restocker une specifique machine avec les ingredients
  var StockSucreAjout: Boolean = false // become true if the level is lower than initial state
  var StockLaitAjout: Boolean = false
  var StockCafeAjout: Boolean = false // Ajouter du café au stock ou pas

  // Définissez une classe nommée Machine
  class Machine(idv:Int, pincodev:String, milkv:Int, sugarv:Int, coffeev:Int) {
    // avec les attributs suivants :
    var id = idv // Identifiant unique de la machine.
    var pincode = pincodev // Code PIN à six chiffres pour l’accès administrateur.
    var milk = milkv // Stock de lait en millilitres.
    var sugar = sugarv // Stock de sucre en grammes.
    var coffee = coffeev // Stock de café en grammes.

    // Méthodes à Implémenter dans la Classe Machine :
    // display all the instance/object attribute
    def affiche : Unit ={
      println(s"Machine " + (id + 1).toString + " chargée :")
      println(" Id: "+id)
      println(" Pincode: "+pincode)
      println(" Lait: "+milk)
      println(" Sucre: "+sugar)
      println(" Poudre a cafe: "+coffee)
    }
    // define the csv line format
    def saveCSV : String ={
      pincode+","+milk+","+sugar+","+coffee
    }

    // Ajoute une quantité spécifiée d’un ingrédient au stock de la machine.
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "lait") {
        // approvisionne milk
        milk += amount
        println(s"Niveaux mis a jour  du $ingredient: " + milk.toString + "L")
      }
      else if (ingredient == "sucre") {// si le niveau du stock est inferieur du niveau initial de la machine StockSucreAjout est true
        // approvisionne sugar
        sugar += amount
        println(s"Niveaux mis a jour  du $ingredient: " + sugar.toString + "g")
      }
      else if (ingredient == "poudre de café") {// si le niveau du stock est inferieur du niveau initial de la machine stockcafeajout est true
        // approvisionne coffee
        coffee += amount
        println(s"Niveaux mis a jour  du $ingredient: " + coffee.toString + "g")
      }
      else {
        println(" L'ingredient n'est pas inclut dans la machine")
      }
    }

    // Retire une quantité spécifiée d’un ingrédient du stock de la machine si le stock est suffisant.
    //Retourne true si l’opération réussit, false sinon.
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
        if (ingredient == "lait") {
          // approvisionne milk
          milk -= amount
          StockLaitAjout = true
          true
        }
        else if (ingredient == "sucre") {
          // approvisionne sugar
          sugar -= amount
          StockSucreAjout = true
          true
        }
        else if (ingredient == "poudre de café") {
          // approvisionne coffee
          coffee -= amount
          StockCafeAjout = true // Nous allons aussi ajouter du café. Nous allons utiliser cette variable en Mode Admin.
          true
        } else {
          println("L'ingredient n'est pas inclut dans la machine")
          false
        }
    }
  }
  // Gestion de la Collection Machines
    // 1. Lire le fichier CSV et retourne une collection ArrayBuffer[Machine].
    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      // Gérez les exceptions pouvant survenir lors de la lecture.
      try {
        var i = 0 // compteur
        val file = Source.fromFile(filename)
        // Chaque ligne du fichier correspond à une machine.
        val ligne = file.reset.getLines()
        var lignesui = ligne.next()
        println("Chargement des machines depuis machines.csv...")
        while( !ligne.isEmpty ) {
          lignesui = ligne.next()
          // PINCODE,MILK,SUGAR,COFFEE
          var pincodev = lignesui.split(",")(0)
          var milkv = lignesui.split(",")(1).toInt
          var sugarv = lignesui.split(",")(2).toInt
          var coffeev = lignesui.split(",")(3).toInt
          machines += new Machine(i, pincodev, milkv, sugarv, coffeev)
          i += 1
        }
        for (x <- machines) x.affiche
      } catch {
        // Si le fichier n’existe pas, le programme doit afficher un message d’erreur et se terminer.
        case _=> println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
      }
      machines
    }
    // 2. Sauvegarder l’état actuel de toutes les machines dans le fichier CSV.
    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit={
      try{
        // false: effacer l'existant et reecrire --- true: ecrire tout en gardant l'existant ds le fichier
        val fw = new PrintWriter( new FileWriter(filename,false))
        fw.println("PINCODE,MILK,SUGAR,COFFEE")
        for (x <- machines)
          // Chaque machine doit être enregistrée format présenté précédemment. avec la methode saveCSV
          println(x.saveCSV)
          fw.println(x.saveCSV)
        fw.close()
        println("Fichier sauvegardé avec succès.")
      }catch {
        // Gérez les exceptions pouvant survenir lors de l’écriture.
        case _=> println("Erreur : Échec de l’écriture dans machines.csv. \n Le fichier peut etre  verrouillé ou en lecture seule.")
      }
    }
    

//---------------Admin Mode------------------------------------------------
  // select admin mode
  def mode_admin(): Unit = {
    // To validate PIN
    var validMachinenumber = false
    var selectedMachine = -1
    while (!validMachinenumber) {
      print(s"Machine sélectionnée (1-${machines.length}) > ")
      // pour convertir le numéro de la machine à son indexe dans le tableau
      selectedMachine = scala.io.StdIn.readInt() - 1
      if (selectedMachine >= 0 && selectedMachine < machines.length) {
          validMachinenumber = true
      } else {
          println("Numéro de machine invalide. Veuillez choisir un numéro entre 1 et 5.")
      }
      var machine = machines(selectedMachine)
      if(validatePin(machine)) { // should access as admin
        var option: Char = read_choice(5)
        if (option == '1')
          restockMachine(machine)
        if (option == '2')
          update_pin(machine)
          for (x <- machines) println("machine " + x.id + " 's pincode: " + x.pincode)
        println("Retour au menu principal...")
      }
    }
  }
  def validatePin(x: Machine): Boolean = {1
    val maximumAttempts = 3
    var attemptsLeft = maximumAttempts
    while (attemptsLeft > 0) {
      val PINNUMentered = readLine("Entrez le code PIN > ") //.toInt
      if (PINNUMentered == x.pincode) {
        println("Accès accordé.")
        return true
      }else{
        attemptsLeft -= 1
        if (attemptsLeft > 0) {
          println(s"Code PIN incorrect. $attemptsLeft tentative(s) restante(s).")
        }else {
          println("Trop de tentatives échouées. Fin du programme.")
        }
      }
    }
    false
  }
  // update pin
  def update_pin(x: Machine): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${x.id + 1}.")
    var attemptsLeft: Int = 5
    var PINNUMnew = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while(PINNUMnew.length != 6 &&  attemptsLeft > 0){// doit comporter exactement 6 chiffres
      PINNUMnew = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      attemptsLeft -= 1
    }
    if (attemptsLeft>0){
      println("Le code PIN a été mis à jour avec succès.")
      // update the machine with the new code PIN
      x.pincode = PINNUMnew
    }
    println(x.affiche)
  }
  // Réapprovisionner les ingrédients pour la machine sélectionnée.
  // Array items are double type/ or Long can work also :)
  def restockMachine(x: Machine ): Unit = {
    println("Entrez les quantités à ajouter :")
    // restock du lait
    if(StockLaitAjout == true){
      var amount = readLine("Lait > ").toInt
      x.addIngredient("lait", amount)
    }
    if(StockSucreAjout == true){
      // restock du sucre
      var amount = readLine("Sucre > ").toInt
      x.addIngredient("sucre", amount)
    }
    if(StockCafeAjout == true){
      // restock de la poudre de café
      var amount = readLine("Poudre de café > ").toInt
      x.addIngredient("poudre de café", amount)
    }
    println("Les stocks ont été mis à jour avec succès.")
  }

//---------------Client Mode------------------------------------------------
    // Sucre Variables
    var SucreAjoute: Int = 5
    var NiveauSucre: String = " "

    // Lait Variables
    val laitDose: Int = 50
    var laitEnSupplement: String = " "

    // Cafe Variables
    var CafCons: Int = 6 //Café consommé
    val CappuccinoLaitCons: Int = 100

    // Price Variables
    var Price: Double = 0.00
    val PriceExpresso: Double = 2.00
    val PriceCappuccino: Double = 2.50
    val PriceLatteSmall: Double = 2.70
    val PriceLatteMedium: Double = 3.20
    val PriceLatteLarge: Double = 3.70
    var PriceSugar: Double = 0.10
    var PriceMilk: Double = 0.05
    var PriceTotal: Double = 0.0
    var orderSuccess = false
    var TWINTCODE: String = " "
    // Boisson choisie:
    var DrinkChoice = ' '
    var LatteSize = ' '
    val LatteLaitConsSmall: Int = 120
    val LatteLaitConsMedium: Int = 150
    val LatteLaitConsLarge: Int = 200
    var BoissonSelect: String = " "

  // select the client mode
  def mode_client(): Unit = {
    var selectedMachine = 0 // supppose the client use Machine 1
    var DrinkChoice: Char = read_choice(1)// 1: select drink
    while (DrinkChoice != '1' && DrinkChoice != '2' && DrinkChoice != '3') {
      DrinkChoice = read_choice(1) // 1: select drink
    }
    if (DrinkChoice == '1') {// Expresso
      PriceMilk = 0.0
      coffee(machines(selectedMachine), DrinkChoice)// Cafe
      // Sucre en supplément pour l'expresso
      var Sucre: Char = read_choice(2)// 2: select amount of sugar
      while (Sucre != '1' && Sucre != '2' && Sucre != '3' && Sucre != '4')
        Sucre = read_choice(2)
      manage_sugar(machines(selectedMachine), Sucre)
    }else if (DrinkChoice == '2') { // Cappuccino
      coffee(machines(selectedMachine), DrinkChoice)
      // Sucre en supplément pour l'expresso
      var Sucre: Char = read_choice(2) // 2: select amount of sugar
      while (Sucre != '1' && Sucre != '2' && Sucre != '3' && Sucre != '4')
        Sucre = read_choice(2)
      manage_sugar(machines(selectedMachine), Sucre)
      // Ajouter du lait au Cappuccino
      var lait: Char = read_choice(3) // 3: select amount of milk
      while (lait != '1' && lait != '2') lait = read_choice(3)
      manage_milk(machines(selectedMachine), lait)
    }
    println(s"Boisson sélectionnée : $BoissonSelect")
    println(s"Niveau de sucre: $NiveauSucre")
    PriceTotal = PriceSugar + Price + PriceMilk
    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n",
      PriceSugar, Price, PriceMilk, PriceTotal)
    // Paiement: Generating TWINT Code
    def GenerateTWINTCode(length: Int = 5) = {
      Random.alphanumeric.take(length).mkString
    }
    TWINTCODE = GenerateTWINTCode()
    println(s"Votre code de paiement est : $TWINTCODE")
    // Attendre 5 secondes
    println("(En attente de paiement...)")
    Thread.sleep(5000)
    println(s"Votre $BoissonSelect est pret ! Bonne dégustation !")
  }
  // manage coffee order
  def coffee(x:Machine, choice:Char): Unit = {
    // Cafe
    if (CafCons <= x.coffee) {
      if (choice == '1'){
        BoissonSelect = "Expresso"
        Price = PriceExpresso
      }
      if (choice == '2'){
        BoissonSelect = "Cappuccino"
        Price = PriceCappuccino
      }
      if (choice == '3'){
        BoissonSelect = "Cappuccino"
        Price = PriceCappuccino
      }
      x.removeIngredient("poudre de café", CafCons) // Si la quantité du café est suffisante, nous allons déduire du café de la quantité du café disponible
    }
    else {
        println("Quantité de café insuffisante")
        StockCafeAjout = false
        orderSuccess = false
    }
  }
  // manage addition of sugar
  def manage_sugar(x:Machine, sugar_choice:Char): Unit = {
    if (sugar_choice == '1') {
      NiveauSucre = "Sans sucre"
      StockSucreAjout = false
      PriceSugar = 0.00
    } else if (sugar_choice == '2') {
      println("Quantité de sucre ")
      if (SucreAjoute <= x.sugar) {
        x.removeIngredient("sucre", SucreAjoute) //On va déduire la quantité ajouté (5 gr) au café de la quantité de sucre disponible.
        NiveauSucre = "Peu (5g)"
        PriceSugar = 0.10
      }else {
        println("Quantité de sucre insuffisante")
        orderSuccess = false
      }
    }else if (sugar_choice == '3') {
      if (SucreAjoute * 2 <= x.sugar) {
        x.removeIngredient("sucre", SucreAjoute * 2) //On va déduire la quantité ajouté (10 gr) au café de la quantité de sucre disponible.
        NiveauSucre = "Moyen (10g)"
        PriceSugar = 0.10 * 2
      }
      else println("Quantité de sucre insuffisante")
    }else if (sugar_choice == '4') {
      if (SucreAjoute <= x.sugar) {
        x.removeIngredient("sucre", SucreAjoute * 3) //On va déduire la quantité ajouté (15 gr) au café de la quantité de sucre disponible.
        NiveauSucre = "Beaucoup (15g)"
        PriceSugar = 0.10 * 3
      }
    }
  }
  // manage milk
  def manage_milk(x:Machine, lait_choice:Char): Unit = {
    if (lait_choice == '1'){
      laitEnSupplement = "Oui" // Supplément lait à ajouter
      var laitDoseSouhaite: Char = read_choice(4)// 4: select amount of milk
      while (laitDoseSouhaite != '1' && laitDoseSouhaite != '2' && laitDoseSouhaite != '3')
        laitDoseSouhaite = read_choice(4)
      manage_amount_milk(x, laitDoseSouhaite)
    } else if (lait_choice == '2') {
          laitEnSupplement = "Non"
          PriceMilk = 0.00
    }
  }
  // manage amount of milk
  def manage_amount_milk(x:Machine, laitDoseSouhaite:Char): Unit = {
    if (laitDoseSouhaite == '1') { // 1 Dose = 50 ml
      var Laitutilise1 = laitDose + CappuccinoLaitCons //le lait utilisé pour faire cette boisson = la dose ajoutée et le lait nécessaire pour le Cappuccino
      if (Laitutilise1 <= x.milk) {
        x.removeIngredient("lait", Laitutilise1)
        println("50 ml de lait a été ajouté")
        PriceMilk = 0.05
      } else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }else if (laitDoseSouhaite == '2') { // 2 Doses = 100 ml
      var Laitutilise2 = (laitDose * 2) + CappuccinoLaitCons
      if (Laitutilise2 <= x.milk) {
        x.removeIngredient("lait", Laitutilise2)
        PriceMilk = 0.05 * 2
        println("100 ml de lait été ajouté")
      }else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }else if (laitDoseSouhaite == '3') { //  3 Doses = 150 ml
      var Laitutilise3 = (laitDose * 3) + CappuccinoLaitCons
      if (Laitutilise3 <= x.milk) {
        x.removeIngredient("lait", Laitutilise3)
        PriceMilk = 0.05 * 3
        println("150 ml de lait été ajouté")
      } else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }
  }


 //--------------Main Function-------------------------------------------------
  // — Déclarez une ArrayBuffer[Machine] nommée machines.
  var machines = ArrayBuffer[Machine]()
  // main function
  def main(args: Array[String]): Unit = {
    try{
      // Chargement des Machines au Démarrage
      machines = loadcsv("machines.csv")
      println(machines.length +  " machine(s) chargée(s) avec succès.")
      println()
      // 0: select usr mode
      var ModeChoice: Char = read_choice(0)
      while (ModeChoice != '1' && ModeChoice != '2' && ModeChoice != '3') {
        ModeChoice = read_choice(0) // 0: select usr mode
      }
      while (ModeChoice != '3'){
        if (ModeChoice == '1')
          mode_client()
        //Admin Mode (2):
        if (ModeChoice == '2')
          mode_admin()
        // retour au menu principal
        ModeChoice = read_choice(0) // 0: select usr mode
        while (ModeChoice != '1' && ModeChoice != '2' && ModeChoice != '3') {
          ModeChoice = read_choice(0) // 0: select usr mode
        }
        println(ModeChoice)
      }
      if (ModeChoice == '3') {
        // sauvegarder les etats du machine dans un fichier csv
        println("Sauvegarde des machines dans machines.csv...")
        // attention si le fichier est ouvert par une autre application (e.g. texteditor), vous pouvez avoir une erreur
        savecsv("machines.csv", machines)
        println("À bientôt!")
      }
    } catch {
        // Si le fichier n’existe pas, le programme doit afficher un message d’erreur et se terminer.
        case _=> println("Erreur : Échec du chargement ou de la sauvegarde des machines. \n Fermeture du programme.")
    }

  }


//---------------------------------------------------------------
//----------------------Pour une meilleure visibilite du code-----
  def read_choice(choice: Int): Char = {
    var ch: Char = 'a'
    // select mode
    if (choice ==0) {
      println("   Nospresso Café")
      println("Veuillez sélectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2' && ch != '3')
        println("Entrée invalide. Veuillez choisir 1, 2 ou 3.")
    }
    // select drink
    if (choice == 1){
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2' && ch != '3')
        println("Entrée invalide. Veuillez choisir 1, 2 ou 3.")
    }
    // select amount of sugar
    if (choice == 2){
          println("Veuillez choisir la quantité de sucre souhaitée:")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2' && ch != '3' && ch != '4')
        println("Entrée invalide. Veuillez choisir 1, 2, 3 ou 4.")
    }
    // select whether add milk or not
    if (choice == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui ")
      println("2) Non ")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2')
        println("Entrée invalide. Veuillez choisir 1 ou 2.")
    }
    // select amount of milk supplement
    if (choice == 4){
      println("Combien de dose ?")
      println("1) 1 Dose - 50 ml")
      println("2) 2 Doses - 100 ml")
      println("3) 3 Doses - 150 ml")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2' && ch != '3')
        println("Entrée invalide. Veuillez choisir 1, 2 ou 3.")
    }
    if (choice == 5){
      println("Options administratives")
      println("1) restock des machines")
      println("2) mise a jour du code PIN")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2')
        println("Entrée invalide. Veuillez choisir 1 ou 2 .")
    }
    ch
  }

}
