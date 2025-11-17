import scala.io.StdIn.readLine
import scala.io.StdIn.readDouble
import scala.io.StdIn.readInt
import scala.io.StdIn.readChar
import scala.util.Random



object Main {

  // Sucre Variables
    var SucreAjoute: Int = 5
    var SucreDispo: Int = 30
    var StockSucreAjout: Boolean = false
    var NiveauSucre: String = " "

    // Lait Variables
    var laitDoseSouhaite = ' '
    val laitDose: Int = 50
    var laitDispo: Int = 500
    var StockLaitAjout: Boolean = false
    var laitEnSupplement: String = " "

    // Cafe Variables
    var CafeDispo: Int = 50 // Café en stock
    var CafCons: Int = 6 //Café consonnmé
    var StockCafeAjout: Boolean = false // Ajouter du café au stock ou pas
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
    // tableaux:
    val nbMachines = 5 // nombre total des machines
    // Stocks pour chaque machine
    val stockCafe = Array.fill(nbMachines)(50.0)  // 50 grammes de café par machine
    val stockSucre = Array.fill(nbMachines)(30.0) // 30 grammes de sucre par machine
    val stockLait = Array.fill(nbMachines)(0.5) // 500 millilitres de lait par machine

    val codePIN = Array.fill(nbMachines)(434343)

  def main(args: Array[String]): Unit = {
    var ModeChoice: Char = read_choice(0) // 0: select usr mode
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
    if (ModeChoice == '3')
      println ("À bientôt!")
  }

  def read_choice(choice: Int): Char = {
    var ch: Char = 'a'
    // select mode
    if (choice ==0) {
      println("Nospresso Café")
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
      println("1) etats des machines")
      println("2) mise a jour du code PIN")
      ch = scala.io.StdIn.readChar()
      if (ch != '1' && ch != '2')
        println("Entrée invalide. Veuillez choisir 1 ou 2 .")
    }
    ch
  }
  // select admin mode
  def mode_admin(): Unit = {
    var PINNUM:Long = 434343
    var PINUMentered = ""
    // To validate PIN
    // Number of machines
    val nbMachines = 5 // An array containing PINs for each machine
    val machinePins = Array("434343", "123456", "111111", "999999", "777777")
    var validMachinenumber = false
    var selectedMachine = -1
    while (!validMachinenumber) {
      print(s"Machine sélectionnée (1-$nbMachines) > ")
      // pour convertir le numéro de la machine à son indexe dans le tableau
      selectedMachine = scala.io.StdIn.readInt() - 1
      if (selectedMachine >= 0 && selectedMachine < nbMachines) {
          validMachinenumber = true
      } else {
          println("Numéro de machine invalide. Veuillez choisir un numéro entre 1 et 5.")
      }
//      validatePin(selectedMachine, machinePins)
      if(validatePin(selectedMachine, machinePins)) { // should access as admin
        var option: Char = read_choice(5)
        if (option == '1')
          restockMachine(selectedMachine, stockCafe, stockSucre, stockLait)
        if (option == '2')
          update_pin(selectedMachine, machinePins)
        println("Retour au menu principal...")
      }
    }
  }
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {1
    val maximumAttempts = 3
    var attemptsLeft = maximumAttempts
    while (attemptsLeft > 0) {
      val PINNUMentered = readLine("Entrez le code PIN: ") //.toInt
      if (PINNUMentered == machinePins(machineId)) {
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
  def update_pin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la Machine $machineId.")
    var attemptsLeft: Int = 5
    var PINNUMnew = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while(PINNUMnew.length != 6 &&  attemptsLeft > 0){// doit comporter exactement 6 chiffres
      PINNUMnew = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      attemptsLeft -= 1
    }
    if (attemptsLeft>=0){
      var PINNUMnew1 = readLine("Entrez encore le nouveau code PIN : ")
      while(PINNUMnew !=  PINNUMnew1 &&  attemptsLeft > 0){
      println(" Le deux codes doivent se correspondrer.")
      PINNUMnew1 = readLine("Entrez encore le nouveau code PIN : ")
      attemptsLeft -= 1
      }
    }
    if (attemptsLeft>0){
      println("Le code PIN a été mis à jour avec succès.")
      // update the machine with the new code PIN
      machinePins(machineId) = PINNUMnew
//      for (i <- machinePins){
//        println(s"item $i" )
//      }
//      println(s" les pins" + machinePins.mkString(", "))
    }
  }
  // Réapprovisionner les ingrédients pour la machine sélectionnée.
  // Array items are double type/ or Long can work also :)
  def restockMachine(machineId: Int, coffeeStocks: Array[Double],
                     sugarStocks: Array[Double], milkStocks: Array[Double] ): Unit = {
    println("Niveaux de stock actuels :")
    println(" Poudre de café  : " + coffeeStocks(machineId).toString + "g")
    println(" Sucre : " + sugarStocks(machineId).toInt.toString + "g")
    println(s" Lait : " + milkStocks(machineId).toString + "L")
    println("Entrez les quantités à ajouter :")
    // approvisionne coffee
    var sc = readLine(" Poudre de café > ").toDouble
    var prev_sc = coffeeStocks(machineId).toDouble // save previous coffee amount
    while(sc < 0.0 ){
      println("quantite devrait etre positive")
      sc = readLine(" Poudre de café > ").toDouble
    }
    // approvisionne sugar
    var ss = readLine(" Sucre > ").toDouble
    var prev_ss = sugarStocks(machineId) // save previous sugar amount
    while(ss < 0.0 ){
      println("quantite devrait etre positive")
      ss = readLine(" Sucre > ").toDouble
    }
    // approvisionne milk
    var sl = readLine(" Lait > ").toDouble
    var prev_sl = milkStocks(machineId) // save previous milk amount
    while(sl < 0.0 ){
      println("quantite devrait etre positive")
      sl = readLine(" Lait > ").toDouble
    }
    // update the tables
    coffeeStocks(machineId) = prev_sc + sc
    sugarStocks(machineId) = prev_ss + ss
    milkStocks(machineId) = prev_sl + sl
    println("quantite cafe: "+ coffeeStocks(machineId).toString)
    if (coffeeStocks(machineId) >= prev_sc
      && sugarStocks(machineId) >= prev_ss
      && milkStocks(machineId) >= prev_sl) {
      println("Les stocks ont été mis à jour avec succès.")
    }
  }
  // select the client mode
  def mode_client(): Unit = {
    var DrinkChoice: Char = read_choice(1)// 1: select drink
    while (DrinkChoice != '1' && DrinkChoice != '2' && DrinkChoice != '3') {
      DrinkChoice = read_choice(1) // 1: select drink
    }
    if (DrinkChoice == '1') {// Expresso
      PriceMilk = 0.0
      coffee(DrinkChoice)// Cafe
      // Sucre en supplément pour l'expresso
      var Sucre: Char = read_choice(2)// 2: select amount of sugar
      while (Sucre != '1' && Sucre != '2' && Sucre != '3' && Sucre != '4')
        Sucre = read_choice(2)
      manage_sugar(Sucre)
    }else if (DrinkChoice == '2') { // Cappuccino
      coffee(DrinkChoice)
      // Sucre en supplément pour l'expresso
      var Sucre: Char = read_choice(2) // 2: select amount of sugar
      while (Sucre != '1' && Sucre != '2' && Sucre != '3' && Sucre != '4')
        Sucre = read_choice(2)
      manage_sugar(Sucre)
      // Ajouter du lait au Cappuccino
      var lait: Char = read_choice(3) // 3: select amount of milk
      while (lait != '1' && lait != '2') lait = read_choice(3)
      manage_milk(lait)
    }
    println(s"Boisson sélectionnée : $BoissonSelect")
    println(s"Niveau de sucre: $NiveauSucre")
    PriceTotal = PriceSugar + Price + PriceMilk
    printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n",
      PriceSugar, Price, PriceMilk, PriceTotal)
    // Paiement
    //Generating TWINT Code
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
  def coffee(choice:Char): Unit = {
    // Cafe
    if (CafCons <= CafeDispo) {
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
      CafeDispo -= CafCons // Si la quantité du café est suffisante, nous allons déduire du café de la quantité du café disponible
      StockCafeAjout = true // Nous allons aussi ajouter du café. Nous allons utiliser cette variable en Mode Admin.
    }
    else {
        println("Quantité de café insuffisante")
        StockCafeAjout = false
        orderSuccess = false
    }
  }
  // manage addition of sugar
  def manage_sugar(sugar:Char): Unit = {
    if (sugar == '1') {
      NiveauSucre = "Sans sucre"
      StockSucreAjout = false
      PriceSugar = 0.00
    } else if (sugar == '2') {
      println("Quantité de sucre ")
      if (SucreAjoute <= SucreDispo) {
        SucreDispo -= SucreAjoute //On va déduire la quantité ajouté (5 gr) au café de la quantité de sucre disponible.
        StockSucreAjout = true
        NiveauSucre = "Peu (5g)"
        PriceSugar = 0.10
      }else {
        println("Quantité de sucre insuffisante")
        orderSuccess = false
      }
    }else if (sugar == '3') {
      if (SucreAjoute * 2 <= SucreDispo) {
        SucreDispo -= SucreAjoute * 2 //On va déduire la quantité ajouté (10 gr) au café de la quantité de sucre disponible.
        StockSucreAjout = true
        NiveauSucre = "Moyen (10g)"
        PriceSugar = 0.10 * 2
      }
      else println("Quantité de sucre insuffisante")
    }else if (sugar == '4') {
      if (SucreAjoute <= SucreDispo) {
        SucreDispo -= SucreAjoute * 3 //On va déduire la quantité ajouté (15 gr) au café de la quantité de sucre disponible.
        StockSucreAjout = true
        NiveauSucre = "Beaucoup (15g)"
        PriceSugar = 0.10 * 3
      }
    }
  }
  // manage milk
  def manage_milk(lait:Char): Unit = {
    if (lait == '1'){
      laitEnSupplement = "Oui" // Supplément lait à ajouter
      var laitDoseSouhaite: Char = read_choice(4)// 4: select amount of milk
      while (laitDoseSouhaite != '1' && laitDoseSouhaite != '2' && laitDoseSouhaite != '3')
        laitDoseSouhaite = read_choice(4)
      manage_amount_milk(laitDoseSouhaite)
    } else if (lait == '2') {
          laitEnSupplement = "Non"
          PriceMilk = 0.00
    }
  }
  // manage amount of milk
  def manage_amount_milk(laitDoseSouhaite:Char): Unit = {
    if (laitDoseSouhaite == '1') { // 1 Dose = 50 ml
      var Laitutilise1 = laitDose + CappuccinoLaitCons //le lait utilisé pour faire cette boisson = la dose ajoutée et le lait nécessaire pour le Cappuccino
      if (Laitutilise1 <= laitDispo) {
        laitDispo -= Laitutilise1
        println("50 ml de lait a été ajouté")
        StockLaitAjout = true
        PriceMilk = 0.05
      } else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }else if (laitDoseSouhaite == '2') { // 2 Doses = 100 ml
      var Laitutilise2 = (laitDose * 2) + CappuccinoLaitCons
      if (Laitutilise2 <= laitDispo) {
        laitDispo -= Laitutilise2
        PriceMilk = 0.05 * 2
        StockLaitAjout = true
        println("100 ml de lait été ajouté")
      }else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }else if (laitDoseSouhaite == '3') { //  3 Doses = 150 ml
      var Laitutilise3 = (laitDose * 3) + CappuccinoLaitCons
      if (Laitutilise3 <= laitDispo) {
        laitDispo -= Laitutilise3
        StockLaitAjout = true
        PriceMilk = 0.05 * 3
        println("150 ml de lait été ajouté")
      } else {
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        orderSuccess = false
      }
    }
  }
}
