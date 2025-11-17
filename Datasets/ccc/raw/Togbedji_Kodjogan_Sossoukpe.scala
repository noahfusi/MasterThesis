import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.PrintWriter

//
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if(ingredient == "Lait") {milk += amount}
    else if (ingredient == "sucre") {sugar += amount}
    else if(ingredient == "cafe") {coffee += amount}
    else {println("Les ingredients sont cafe, sucre et lait...")}}


  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "Lait" && milk >= amount) {
      milk -= amount
      true}
    else if  (ingredient == "sucre" && sugar >= amount) {
      sugar -= amount
      return true}
    else if(ingredient == "cafe" && coffee >= amount) {
      coffee -= amount
      true}
    else {return false}}}


object Nospresso {

  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN pour la machine " + machine.id + " (6 chiffres ) :")
    val nouveauPin = readLine("> ")
    if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)) {
      machine.pincode = nouveauPin
      println("Code PIN mis à jour avec succès.")
      print("Retour au menu principal .....")}
    else {println("Format invalide. Le code PIN doit comporter 6 chiffres.")}}

  def validatePin(machine: Machine): Boolean = {
    var essais = 3
    while (essais > 0) {println("Entrez le code PIN pour la machine " + machine.id)
      val saisiePin = readLine(" > ")
      if (saisiePin == machine.pincode) return true
      essais -= 1
      println("Code PIN incorrect. Tentatives restantes : " + essais)}
    println("Trop de tentatives échouées. Fin du programme")
    false}

  def serveClient(machine: Machine): Boolean =  { //
    println("Choix de votre boisson : ")
    println("1) Expresso - 2.00 CHF" )
    println("2) Cappuccino- 2.50 CHF")
    println("3) Latte - 2.70 CHF (Petit), 3.20 CHF (Moyen), 3.70 CHF (Grand)")
    var choixdeBoisson = readLine("> ").toInt

    if (choixdeBoisson == 1) {
      if(machine.removeIngredient("coffee", 8)) {println("Votre expresso est prêt. Bonne dégustation !")
        true}
      else {println("Stock insuffisant pour préparer un expresso.")
        return false}}
    else if (choixdeBoisson == 2) {
      if (machine.removeIngredient("cafe", 6) && machine.removeIngredient("lait", 100)) {
        println("Votre cappuccino est prêt. Bonne dégustation !")
        return true}
      else{ println("Stock insuffisant pour préparer cette boisson.")
        false}}

    else if (choixdeBoisson == 3){println("Choisissez la taille du Latte : 1) Petit \n 2) Moyen \n 3) Grand")
      val tailleduLatte = readLine("> ").toInt

      var cafenecess = 0
      var laitnecess = 0

      if(tailleduLatte == 1) {cafenecess =  6
        laitnecess = 120}
      else if(tailleduLatte == 2){ cafenecess = 8
        laitnecess = 150}
      else if (tailleduLatte == 3) {cafenecess = 12
        laitnecess = 200}

      if(machine.removeIngredient("cafe", cafenecess) && machine.removeIngredient("lait", laitnecess)){
        println("Votre latte est prêt. Bonne dégustation !")
        return true}
      else {println("Stock insuffsant pour préparer un latte.")
        false } }
    else {println("Choix invalide.")
      return false}}

  def restockMachine(machine: Machine): Unit = {

    println("Quantité de café à ajouter : ")

    machine.addIngredient("cafe", readLine(" > ").toInt)
    println("Quantité de sucre à ajouter :")

    machine.addIngredient("sucre", readLine(" > ").toInt)
    println("Quantité de lait à ajouter (en ml) :")
    machine.addIngredient("Lait", readLine(" > ").toInt)
    println("Stocks mis a jour  avec succès.")}

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try{
      val lignes = Source.fromFile(filename).getLines().toArray
      for (i <- 1 until lignes.length) {
        val partiesLignes = lignes(i).split(", ")

        machines += new Machine(i, partiesLignes(0), partiesLignes(1).toInt, partiesLignes(2).toInt, partiesLignes(3).toInt)
      println("Machine chargee avec succes")}}

    catch {
      case e: Exception => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        return ArrayBuffer[Machine]()}
return machines }

  def savecsv( filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val filename = "machines.csv"
      val fichier = new java.io.File(filename)
      new PrintWriter(new java.io.File(filename)) {
        write("PINCODE,MILK ,SUGAR ,CAFE \n")
        //println()
        for ( machine <- machines) {
      write(machine.pincode + "," + machine.milk + ", " + machine.sugar + "," + machine.coffee + " \n")}
        close()}}
    catch {
      case e: Exception => println("Erreur : Échec de l'écriture dans " + filename + ".")
        println("Le fichier peut être verrouillé ou en lecture seule.")

        return}}

}

//
object Main {
  def main(args: Array[String]): Unit = {
    var nomfichiercsv =  "machines.csv"
    var machines = Nospresso.loadcsv(nomfichiercsv)

    var continuer = true
    while (continuer) {println("  Nospresso Cafe   ")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      var mode = readLine("> ").toInt

      if (mode==1){println("Sélectionner une machine (1 à " + machines.size + ") : ")

        val machineId = readLine("> ").toInt - 1
        if (machineId >= 0 && machineId < machines.length) {
       Nospresso.serveClient(machines(machineId))}
        else{println("Machine invalide.")}}

      else if(mode== 2) { println("Veuillez sélectionner une machine (1 à " + machines.length + ") :")

        val machineId = (readLine("> ").toInt - 1) //souci d'index

     if (machineId >= 0 && machineId < machines.length) {

          if (Nospresso.validatePin(machines(machineId))) {
            println("1) Réapprovisionner")
            println("2) Mettre à jour le code PIN")
        val choixAdmin = readLine("> ").toInt
            if( choixAdmin == 1) {Nospresso.restockMachine(machines(machineId))}
        else if (choixAdmin == 2) {
              Nospresso.updatePin(machines(machineId))}}}
           else {println("Machine invalide.")}}

      else if(mode == 3) {Nospresso.savecsv(nomfichiercsv, machines)
        println("Au revoir")
      continuer = false}
      else {
        println("Choix invalide. Veuillez réessayer.")}}

  }
}
