import scala.io.StdIn._

// Objet nospresso
object Nospresso {


  def updatePin(machineId: Int, machinePins: Array[String]): Unit ={
    println("Entrez un nouveau code PIN pour la machine " + machineId + " (6  chiffres) :")
    val nouveauPin = readLine("> ")
    if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) { //methode authorize?
      machinePins(machineId) = nouveauPin
      println("Code PIN mis à jour avec succès.")
    print("Retour au menu principal ...")}
    else {
      println("Format invalide. Le code PIN doit comporter 6 chiffres.")}}

  def validatePin(machineId: Int, machinePins: Array[String]):Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println("Entrez le code PIN pour la machine " + machineId)
      var saisiePin = readLine(" > ")
      if (saisiePin == machinePins(machineId)) return true
      tentatives -= 1
      println("Code PIN incorrect. Tentatives restantes :" + tentatives)}

    println("Trop de tentatives echouees. Fin du programme")
    return false}



  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]):Boolean ={
    println("Choix de votre boisson :  ")
    println("1) Expresso - 2.00 CHF")
    println ("2) Cappuccino - 2.50 CHF ")
    println("3) Latte - 2.70 CHF (Petit), 3.20 CHF ( Moyen), 3.70 CHF (Grand)")
    val choixboisson = readLine("> ").toInt

    if (choixboisson == 1) {
      if (coffeeStocks(machineId) >= 8) {
        coffeeStocks(machineId) -= 8
       println("Votre expresso est prêt. Bonne dégustation !")
        return true}
      else {
        println("Stock insuffisant pour préparer un expresso.")
        return false}}
     else if (choixboisson == 2) {
      if ((coffeeStocks(machineId) >= 6) && milkStocks(machineId) >= 100) {
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 100
       println("Votre cappuccino est prêt. Bonne dégustation !")
        return true} // encombrant
      else {
        println("Stock insuffisant pour préparer un Cappuccino.")
        return false}}
    else if (choixboisson == 3) {
      println("Choisissez la taille du Latte : 1) Petit \n 2) Moyen \n3) Grand")
      val tailledeLatte = readLine("> ").toInt
      var cafenecessaire = 0
      var milknecessaire = 0
        if (tailledeLatte == 1) {
          cafenecessaire = 6
       milknecessaire = 120}
        else if (tailledeLatte == 2) {cafenecessaire = 8
        milknecessaire = 150}
      else if (tailledeLatte == 3) {cafenecessaire = 12
      milknecessaire = 200}


      if (coffeeStocks(machineId) >= cafenecessaire && milkStocks(machineId) >= milknecessaire) {
        coffeeStocks(machineId) -= cafenecessaire
        milkStocks(machineId) -= milknecessaire
        println("Votre latte est prêt. Bonne dégustation !")
        return true}
      else {
        println("Stock insuffisant pour préparer un latte.")
        return false}
    } else {
      println("Choix   invalide .")
     return false}}

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit ={
    //ne verifie que le format int...
    println("Quantité de café à ajouter :")
    coffeeStocks(machineId) += readLine("> ").toInt
    print ("Qua ntité de sucre à ajouter :")
    sugarStocks(machineId) += readLine("> ").toInt
    println("Quantité de lait à ajouter (en mL) :")
    milkStocks(machineId) += readLine("> ").toInt
    println("Stock mis à jour avec succès.")}
}

// programme principal
object Main {
  def main(args: Array[String]): Unit = {

    var nbMachines = 5
    var CodePinInitial = "434343"
    var coffeeStocks: Array[Int] = Array.fill(nbMachines)(50)
    var sugarStocks: Array[Int] = Array.fill(nbMachines)(30)
    var milkStocks: Array[Int] = Array.fill(nbMachines)(500) //convertir en litres
    var machinePins: Array[String] = Array.fill(nbMachines)(CodePinInitial)
    var machineId = Array.tabulate(3)(i => i)

    var continueRprogramme = true

    while(continueRprogramme){
    println("Bienvenue dans  Nospresso ")
    while(true) {//penser simplifier

      print("choisir votre mode : ")
      println("1) Client  ")
      println("2) Admin ")
      println("3) Quitter  ")
      var mode = readLine(" > ").toInt

      if (mode == 1) {
        var machineId =0
        while ((machineId < 1) || (machineId >= nbMachines)) {
        println("selectionner une machine (1 à 5) :")
       machineId = readLine("> ").toInt
          if (machineId < 0 || machineId >= nbMachines) println("Machine invalide.")}

        Nospresso.serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)}


        //admin?

      else if(mode == 2) {
        var machineId = 0
        while ((machineId < 1) || machineId >= nbMachines) {
          print("Veuillez sélectionner une machine (1 à 5) :")
          machineId = readLine(" > ").toInt
        if (machineId< 1 || machineId >= nbMachines) println("Machine invalide.")}

        if (Nospresso.validatePin(machineId, machinePins)) {println("1) Réapprovisionner")
          println("Niveaux de stock actuels\n " + coffeeStocks, sugarStocks, milkStocks) //probleme affichage
        println("2) Mettre à jour le code PIN")
          val choixAdmin = readLine("> ").toInt
          if (choixAdmin == 1) {
          Nospresso.restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)}
        else if (choixAdmin == 2) {
            Nospresso.updatePin(machineId, machinePins)}}}
      else if (mode == 3) {//verifier sortie du programme et retour menu de choix
        println("Au revoir ")
      return} //retour ?

      else {println("Choix invalide. Veuillez réessayer.")
      }}
  }
}}