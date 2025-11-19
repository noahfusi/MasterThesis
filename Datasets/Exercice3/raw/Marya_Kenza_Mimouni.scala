import scala.io.StdIn
import scala.util.Random
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import java.nio.file.{Paths, Files}

object Main {


  class Machine(val id: Int, var pincode: String, var milk: Int,var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "coffee"){
        coffee += amount
      }
      else if (ingredient == "sugar") {
        sugar += amount
      }
      else if (ingredient == "milk") {
        milk += amount
      }
      return
    }
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "coffee" && coffee-amount>0){
        coffee -= amount
      }
      else if (ingredient == "sugar" && sugar-amount>0) {
        sugar -= amount
      }
      else if (ingredient == "milk" && milk-amount>0) {
        milk -= amount
      }
      else{
        println("Montant Non valide")
        return false
      }
      return true

    }
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("Chargement des machines depuis machines.csv... ")

    if (!Files.exists(Paths.get(filename))){
      println("Erreur : Fichier introuvable. Vérifiez le chemin d’accèes et réessayez.")
      System.exit(0)
    }


    val buffer = ArrayBuffer[Machine]()
    val lines = Source.fromFile(filename).getLines().toList

    // Skip the header line
    val dataLines = lines.drop(1)

    var id = 1  // Start ID from 0 and increment for each Machine
    var nmach = 0
    for (line <- dataLines) {
      val cols = line.split(",").map(_.trim)
      if (cols.length >= 4) {

        println(s"machine $id chargée: ")
        // Extract and parse the fields
        val pincode = cols(0)
        val milk    = cols(1).toInt
        val sugar   = cols(2).toInt
        val coffee  = cols(3).toInt
        println(s"ID : $id")
        println(s"Code PIN : $pincode")
        println(s"Lait : $milk")
        println(s"Sucre : $sugar")
        println(s"Café : $coffee")
        println("")
        // Create a new Machine object
        val machine = new Machine(id, pincode, milk, sugar, coffee)
        buffer += machine

        println()

        id += 1  // Increment ID for the next Machine
        nmach+=1
      } else {
        println(s"Skipping invalid line: $line")
      }
    }

    println(s"$nmach machine(s) chargée(s) avec succès.")
    buffer
  }


  var machines = loadcsv("machines.csv")




  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    println("Sauvegarde des machines dans machines.csv..")
    if (!Files.exists(Paths.get(filename))) {
      println("Erreur : ´Echec de l’écriture dans machines.csv. \nLe fichier peut être verrouillé ou en lecture seule.")
      System.exit(0)
    }


    val pw = new PrintWriter(new java.io.File(filename))

    // Write the header line
    pw.println("PINCODE,MILK,SUGAR,COFFEE")

    // Write each machine's data
    for (machine <- machines) {
      pw.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
    }


    pw.close()
    var machNumber = machines.length
    println(s"Sauvegarde de $machNumber machines dans machines.csv")
    println("Fichier sauvegardé avec succès")

  }



  def main(args: Array[String]): Unit = {
    while (true){
      AfficherMenu()
    }

  }


  ////DEFINIR LES VARIABLES

  val nbMachines = machines.length

  //var machinePins = Array.fill(nbMachines)(434343)


  var machineId = 0

  private var DoseDeLait = 0

  //var Stockscaffe = Array.fill(nbMachines)(50)
  private var NiveauSucre = ""
  //var StocksSucre = Array.fill(nbMachines)(30)
  //var StockLait = Array.fill(nbMachines)(500)




  /////AFFICHER LE MENU PRINCIPAL
  def AfficherMenu(): Boolean = {
    machineId = StdIn.readLine("Machine séléctionnée>").toInt
    println("\n")

    println("Nospresso Cafe")
    println("Veuillez selectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")

    val ModeSelection = StdIn.readLine(">")


    ModeSelection match {
      case "1" => return serveClient(machineId)
      case "2" => return AdminMode(machineId)
      case "3" =>  return ExitMode()
    }
  }////Afficher le menu principal



  ////Mode client

  private def serveClient(Id:Int) : Boolean = {
    // Selectionner sa bopisson
    println("Veuillez selectionner votre boisson :")
    println("1) Espresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")


    val BoissonSelectionne = StdIn.readLine(">")


    // Ajouter sucre/ Lait 
    NiveauSucre =  QuantiteDeSucre()
    DoseDeLait =  QuantiteDeCafe()

    // Choisir quantite de sucre
    def QuantiteDeSucre() : String = {
      println("\n")
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      //println(">")

      val Choix = StdIn.readLine(">")

      Choix match {
        case "1" => "1" // Sans sucre
        case "2" => "2" // Léger - 5g
        case "3" => "3" // Moyen - 10g
        case "4" => "4" // Beaucoup - 15g
        case _ =>
          println("Choix non invalide, par défaut sans sucre.")
          "Sans sucre" // Par défaut sans sucre

      }
    }///Fin sucre

    // choisir quantité de lait
    def QuantiteDeCafe() : Int = {
      if ( BoissonSelectionne == "2" || BoissonSelectionne == "3" ){
        println("\n")
        println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)")

        println("1) Oui")
        println("2) Non")
        //println(">")
        val Choix = StdIn.readLine(">")
        Choix match {
          case "1" =>
            println("Combien de dose ? (1-3)")
            val doses = StdIn.readInt()
            if (doses >= 1 && doses <= 3) {
              println(s"$doses doses de lait.")
              doses // Renvoi le nombnre de dose de lait
            } else {
              println("Non valide. Sans lait supplémentaire.")
              0 // Non valide, pas de lait
            }
          case "2" =>
            println("Sans lait supplémentaire.")
            0 // Sans lait
          case _ =>
            println("Sans lait supplémentaire.")
            0 // non valide defaut pas de lait
        }
      }
      else{0}


    }///Fin de la dose de lait
    var reussi = false
    BoissonSelectionne match {
      case "1" => reussi = FaireEspresso(machineId,NiveauSucre)
      case "2" => reussi = FaireCappucino(machineId,NiveauSucre,DoseDeLait)
      case "3" => reussi = FaireLatte(machineId,NiveauSucre,DoseDeLait)
    }

    if (reussi){


      // choisir le recap de la boisson
      val PrixCafe : Double = BoissonSelectionne match {
        case "1" => 2.00 // Espresso
        case "2" => 2.50 // Cappuccino
        case "3" =>
          DoseDeLait match {
            case 1 => 2.70 // Petit Latte
            case 2 => 3.20 // Moyen Latte
            case 3 => 3.70 // Grand Latte
          }
      }
      val NomDeLaBoisson: String = BoissonSelectionne match {
        case "1" => "Espresso"
        case "2" => "Cappuccino"
        case "3" => "Latte"
        case _ => "Boisson Inconnue" // Par défaut
      }

      println(s"Boisson sélectionnée : $NomDeLaBoisson")

      // Caslculer prix du sucre
      val PrixDuSucre : Double = NiveauSucre match {
        case "1" => 0.00 // Pas de sucre
        case "2" => 0.10 // Peu
        case "3" => 0.20 // Moyen
        case "4" => 0.30 // Beaucoup
      }

      val NomNiveauSucre = NiveauSucre match {
        case "1" => "Pas de sucre"
        case "2" => "Léger (5g)"
        case "3" => "Moyen (10g)"
        case "4" => "Beaucoup (15g)"
      }
      println(s"Niveau de sucre: $NomNiveauSucre")

      val NomLait = DoseDeLait match {
        case 0 => "Non"
        case 1 => "1 dose"
        case 2 => "2 doses"
        case 3 => "3 doses"
      }
      println(s"Lait suplémentaire: $NomLait")

      // Calculer prix total
      val prixTotal : Double = PrixCafe + PrixDuSucre
      if (NomNiveauSucre == "Pas de sucre" ){
        println(f"Prix total: CHF $PrixCafe%.2f = CHF $prixTotal%.2f")
      }
      else{
        println(f"Prix total: CHF $PrixCafe%.2f + CHF $PrixDuSucre%.2f = CHF $prixTotal%.2f")
      }




      // Generateur de code de sécurité aleatoire
      val setalphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      val CodePaiement = (for (_ <- 1 to 5) yield setalphanum(Random.nextInt(setalphanum.length))).mkString
      println("\n")
      println("Veuillez payer avec twint.")
      println(s"Votre code de paiement est : $CodePaiement")
      println("(En attente du paiement...)")
      Thread.sleep(3000) // attendre 3s pour la validation confirmation du paiement
      println("Paiement accepté.")
      println("\n")
      Preparation()


      def Preparation() : Unit = {
        // preparer la boisson
        println("Boisson en préparation..")
        println("[...]")
        Thread.sleep(5000)  // attendfre 5 secondes
        println(s"Votre $NomDeLaBoisson est prêt ! Profitez!")

      }
      return true
    }

    else{
      return false
    }

    //AfficherMenu()


  } //Fin du mode client


  def ModifierPin(machineId: Int): Unit={
    println(s"Changement de PIN pour la machine $machineId.")
    var NouveauCodePin = ""
    while (NouveauCodePin.length != 6){
      var NouveauCodePin = readLine("Entrez un PIN de 6 chiffres >")
      //println(NouveauCodePin.length)
      //println(NouveauCodePin.length != 6)
      if (NouveauCodePin.length == 6){
        machines(machineId-1).pincode = NouveauCodePin
        return
      }

    }
  }////Modifier le pin des machines de maniere independantee




  def RechargerLaMachine(Id: Int,addOrRemove: Int): Unit = {

    var StockPoudreCafe = machines(Id-1).coffee
    var StockDeSucre = machines(Id-1).sugar
    var StockDeLait = machines(Id-1).milk
    // Afficher les stocks
    println(s"Stocks:\n Poudre Cafe: $StockPoudreCafe g\n  Lait: ${StockDeLait / 1000.0} L\n  Sucre: $StockDeSucre g")

    if(addOrRemove == 1){
      // ajouter du café
      //println()
      val Cafe = scala.io.StdIn.readLine("Combien de grammes de café en poudre voulez vous ajouter? >").toInt

      machines(Id - 1).addIngredient("coffee", Cafe)
      //machines(Id-1).coffee += Cafe
      println(s"Poudre Café: + $Cafe g")

      // ajouter du sucre
      //println(s)
      val Sucre = scala.io.StdIn.readLine("Combien de grammes de sucre voulez vous ajouter?>").toInt
      machines(Id - 1).addIngredient("sugar", Sucre)
      println(s"Sucre: + $Sucre g")

      // ajouter du lait
      //println()
      val Lait = scala.io.StdIn.readLine("Combien de Litres de Lait voulez vous ajouter?>").toInt
      machines(Id - 1).addIngredient("milk", Lait*1000)
      println(s" Lait: +$Lait L")

      println("Fin")

      StockPoudreCafe = machines(Id - 1).coffee
      StockDeSucre = machines(Id - 1).sugar
      StockDeLait = machines(Id - 1).milk
      // afficher les nouveaux stocks
      println("Réapprovisionnement des stocks...")

    }

    else{
      // ajouter du café
      //println()
      val Cafe = scala.io.StdIn.readLine("Combien de grammes de café en poudre voulez vous enlever? >").toInt

      var res = machines(Id - 1).removeIngredient("coffee", Cafe)
      if (!res){
        return
      }
      //machines(Id-1).coffee += Cafe
      println(s"Poudre Café: - $Cafe g")

      // enlever du sucre
      //println(s)
      val Sucre = scala.io.StdIn.readLine("Combien de grammes de sucre voulez vous enlever?>").toInt
      res = machines(Id - 1).removeIngredient("sugar", Sucre)
      if (!res) {
        return
      }
      println(s"Sucre: - $Sucre g")

      // enlever du lait
      //println()
      val Lait = scala.io.StdIn.readLine("Combien de Litres de Lait voulez vous enlever?>").toInt
      res = machines(Id - 1).removeIngredient("milk", Lait * 1000)
      if (!res) {
        return
      }
      println(s" Lait: -$Lait L")

      println("Fin")

      StockPoudreCafe = machines(Id - 1).coffee
      StockDeSucre = machines(Id - 1).sugar
      StockDeLait = machines(Id - 1).milk
      // afficher les nouveaux stocks
      println("Changement des stocks...")
    }

    println(s"Niveaux de stock mis à jour:\n Poudre de Cafe: $StockPoudreCafe g\n  Lait: ${StockDeLait / 1000.0} L\n  Sucre: $StockDeSucre g")
    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")




  }//On recharge les machines de maniere independante dans une fonction pour la visibikité du code et lexecution




  def ValiderCodePin(ID:Int): Boolean = {
    val validPin = machines(ID-1).pincode.toInt
    var pin = scala.io.StdIn.readLine("Entrer le PIN admin:").toInt
    var essai = 1
    var pinCorrect = false
    if (pin == validPin){
      pinCorrect = true
    }
    else{
      pinCorrect = false
    }
    while (essai < 3 && pinCorrect == false){
      var remain = 3 - essai
      var pin = scala.io.StdIn.readLine(s"PIN Incorrect. $remain essai restants >").toInt
      if (pin == validPin){
        pinCorrect = true
      }
      else{
        pinCorrect = false
        essai += 1
      }
    }
    if (pinCorrect){
      return true
    }
    else{
      return false
    }
  }///FOnction de validation du code pin, au bout dfe 3 esssais nous quittons le programmme



  ///// Mode de l'administrateur --> changer le pin et recharger la machine
  private def AdminMode(Id:Int): Boolean = {


    var Id = scala.io.StdIn.readLine("Machine selectionnée (1-5) >").toInt
    var pinValider = ValiderCodePin(Id)
    if (!pinValider){
      ExitMode()
    }
    else{


      println("Accés accepté.")

      println("1) Changer Pin \n2) Réaprovisionner les stocks\n3) Enlever des stocks\n")
      var Choix = readLine(">")
      if (Choix == "1"){
        ModifierPin(Id)
        println("Pin changé.")
        println("retour au menu principal....")
        //true
      }
      else if (Choix == "2"){
        RechargerLaMachine(Id,1)

        //Recharger la machine en stocks


      }

      else{
        RechargerLaMachine(Id,2)
      }


    }


    //}
    return true

  }////FIN du mode admin




  def extraireValeurSucre(input: String): Int = {
    val sugG = NiveauSucre match {
      case "1" => 0
      case "2" => 5
      case "3" => 10
      case "4" => 15
    }
    sugG
  }



  ////// Faire les idffeents caffe 

  //ESPRESSO
  private def FaireEspresso(Id:Int,sucreL: String): Boolean = {



    var StockPoudreCafe = machines(Id-1).coffee
    var StockDeSucre = machines(Id-1).sugar
    var StockDeLait = machines(Id-1).milk
    // Make Espresso
    if (StockPoudreCafe >= 8) {
      StockPoudreCafe -= 8
      machines(Id-1).coffee = StockPoudreCafe
      return true

    }
    else {
      println(s"Boisson sélectionnée : Espresso")
      println(s"Niveau de sucre : $sucreL")
      println("")
      println("Poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

      println("\n")
      return false

    }
    var sucVal = extraireValeurSucre(sucreL)
    if (StockDeSucre >= sucVal) {
      StockDeSucre -= sucVal
      machines(Id-1).sugar = StockDeSucre
      return true

    }
    else {
      println(s"Boisson selectionnée : Espresso")
      println(s"Niveau de sucre : $sucreL")
      println("")
      println("Sucre insuffisant pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

      println("\n")
      return false

    }

  }
  //CAPPUCIONO
  private def FaireCappucino(Id:Int,sucreL: String,laitL: Int) : Boolean = {
    var StockPoudreCafe = machines(Id-1).coffee
    var StockDeSucre = machines(Id-1).sugar
    var StockDeLait = machines(Id-1).milk
    // Faire Cappuccino
    if (StockPoudreCafe >= 6) {
      StockPoudreCafe -= 6
      machines(Id-1).coffee = StockPoudreCafe
      return true

    } else {
      println(s"Boisson selectionnée : Cappuccino")
      println(s"Niveau de sucre : $sucreL")
      println(s"Lait supplémentaire : $laitL")
      println("")
      println("Poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

      println("\n")
      return false
    }
    if (StockDeLait >= 100 ) {
      StockDeLait -= 100


      machines(Id-1).milk = StockDeLait
      return true

    } else {
      println("Manque de lait pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      println("\n")
      return false

    }

    var sucVal = extraireValeurSucre(sucreL)
    if (StockDeSucre >= sucVal) {
      StockDeSucre -= sucVal
      machines(Id-1).sugar = StockDeSucre
      return true

    }
    else {
      println(s"Boisson selectionnée : Cappucino")
      println(s"Niveau de sucre : $sucreL")
      println("")
      println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

      println("\n")
      return false
      //AfficherMenu()
    }
  }


  ///FAIRE LATTE
  private def FaireLatte(Id:Int,sucreL: String,laitL: Int) : Boolean = {

    println("Please select beverage size:")
    println("1) Petit")
    println("2) Moyen")
    println("3) Grand")
    //println(">")

    val LatteChoix = StdIn.readLine(">")
    var reussi = true
    LatteChoix match {
      case "1" => reussi = FairePetitLatte(Id,sucreL,laitL)
      case "2" => reussi = FaireMoyenLatte(Id,sucreL,laitL)
      case "3" => reussi = FaireGrandLatte(Id,sucreL,laitL)
    }
    reussi
  }



  private def FairePetitLatte(Id:Int,sucreL: String,laitL: Int) : Boolean = {
    var StockPoudreCafe = machines(Id-1).coffee
    var StockDeSucre = machines(Id-1).sugar
    var StockDeLait = machines(Id-1).milk
    // Faire petit latte
    if (StockPoudreCafe >= 6) {
      StockPoudreCafe -= 6
      machines(Id-1).coffee = StockPoudreCafe


    } else {
      println(s"Boisson selectionnée : Petit Latte")
      println(s"Niveau de sucre : $sucreL")
      println(s"Lait supplémentaire : $laitL")
      println("")
      println("Poudre de café insuffisante pour préparer la boisson selectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
      println("\n")
      return false
    }
    if (StockDeLait >= 120 ) {
      StockDeLait -= 120
      machines(Id-1).milk = StockDeLait
      return true

    } else {
      println(s"Boisson sélectionnée : Petit Latte")
      println(s"Niveau de sucre : $sucreL")
      println(s"Lait supplémentaire : $laitL")
      println("")
      println(" Lait insuffisant pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
      println("\n")
      return false
    }

    var sucVal = extraireValeurSucre(sucreL)
    if (StockDeSucre >= sucVal) {
      StockDeSucre -= sucVal
      machines(Id-1).sugar = StockDeSucre
      return true

    }
    else {
      println(s"Selected beverage: Petit Latte")
      println(s"Niveau de sucre: $sucreL")
      println("")
      println("Sucre insuffisant pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

      println("\n")
      return false
      //AfficherMenu()
    }
  }



  private def FaireMoyenLatte(Id:Int,sucreL: String,laitL: Int) : Boolean = {
    var StockPoudreCafe = machines(Id-1).coffee
    var StockDeSucre = machines(Id-1).sugar
    var StockDeLait = machines(Id-1).milk
    // Faire Moyen Latte
    if (StockPoudreCafe >= 8) {
      StockPoudreCafe -= 8
      machines(Id-1).coffee = StockPoudreCafe
      return true
      //println("Preparing Medium Latte...")
    } else {
      println(s"Boisson sélectionnée : Latte Moyen")
      println(s"Niveau de sucre : $sucreL")
      println(s"Lait supplémentaire : $laitL")
      println("")
      println("Poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      println("\n")
      return false
    }
    if (StockDeLait >= 150 ) {
      StockDeLait -= 150
      machines(Id-1).milk = StockDeLait
      //listupdateStockLait.updated(Id - 1, StockDeLait)
      return true
      //println("Preparing Medium Latte...")
    } else {
      println(s"Boisson sélectionnée : Latte Moyen")
      println(s"Niveau de sucre: $sucreL")
      println(s"Lait en supplément : $laitL")
      println("")
      println(" Pas assez de lait pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      println("\n")
      return false
    }

    var sucVal = extraireValeurSucre(sucreL)
    if (StockDeSucre >= sucVal) {
      StockDeSucre -= sucVal
      machines(Id-1).sugar = StockDeSucre
      return true

    }
    else {
      println(s"Boisson sélectionnée : Latte Moyen")
      println(s"Niveau de sucre : $sucreL")
      println("")
      println("Sucre insuffisant pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")

      println("\n")
      return false
      //AfficherMenu()
    }
  }


  ////FAURE UN GRAND LATTE
  private def FaireGrandLatte(Id:Int,sucreL: String,laitL: Int) : Boolean = {
    var StockPoudreCafe = machines(Id - 1).coffee
    var StockDeSucre = machines(Id - 1).sugar
    var StockDeLait = machines(Id - 1).milk
    // Faire latte large
    if (StockPoudreCafe >= 12) {
      StockPoudreCafe -= 12
      machines(Id - 1).coffee = StockPoudreCafe
      return true

    } else {
      println(s"Boisson sélectionnée : Grand Latte")
      println(s"Niveau de sucre : $sucreL")
      println(s"Lait supplémentaire : $laitL")
      println("")
      println("Pas assez de café en poudre pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      println("\n")
      return false
    }
    if (StockDeLait >= 200 ) {
      StockDeLait -= 200
      machines(Id-1).milk = StockDeLait
      return true

    } else {
      println(s"Boisson sélectionnée: Grand Latte")
      println(s"Niveau de sucre: $sucreL")
      println(s"Lait supplémentaire: $laitL")
      println("")
      println(" Lait insuffisant pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
      println("\n")
      return false
    }

    var sucVal = extraireValeurSucre(sucreL)
    if (StockDeSucre >= sucVal) {
      StockDeSucre -= sucVal
      machines(Id-1).sugar = StockDeSucre
      return true
      //println("Preparing Espresso...")
    }
    else {
      println(s"Boisson sélectionnée : Grand Latte")
      println(s"Niveau de sucre : $sucreL")
      println("")
      println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")

      println("\n")
      return false
      //AfficherMenu()
    }
  }

  /////FIn de faire caffe




  // quitter programme
  private def ExitMode(): Boolean = {
    println("Merci d'avoir utilisé Nospresso. ... Au revoir!")
    savecsv("machines.csv", machines)
    System.exit(0) // Stops Program
    return true
  }
}