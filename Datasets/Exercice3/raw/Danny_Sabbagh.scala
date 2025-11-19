import java.io.PrintWriter
import java.io.FileWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source.fromFile
import scala.io.StdIn._
import scala.util.Random

object Main {
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      println("Sauvegarde de " + machines.length + " machine(s) dans " + filename )
      val printWriter = new PrintWriter(new FileWriter(filename, false))
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")

      for (i <- 0 until machines.length) {
        printWriter.println(machines(i).pincode + "," + machines(i).milk + "," + machines(i).sugar + "," + machines(i).coffee)
      }
      printWriter.close
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Fichier n'existe pas")
    }
  }

  def loadcsv(nomCSV: String): ArrayBuffer[Machine] = {
    try{
      val fr = fromFile(nomCSV)
      val lignefr = fr.reset.getLines
      val machines = new ArrayBuffer[Machine]()
      var i = 0
      println("Chargement des machines depuis " + nomCSV + "...")
      while (!lignefr.isEmpty) {
        var ligne = lignefr.next
        if (i!=0){
          var machine = ligne.split(",")
          machines += new Machine(i,machine(0),machine(1).toInt,machine(2).toInt,machine(3).toInt)
          println("Machine " + (i) + " chargée :")
          println("    ID: " + (i))
          println("    Code PIN: " + machine(0))
          println("    Lait: " + (machine(1).toDouble/1000.0).toString + "L")
          println("    Sucre: " + machine(2) + "g")
          println("    Café: " + machine(3) + "g")
        }
        i += 1
      }
      println("")
      println(machines.length + " machine(s) chargée(s) avec succès.")
      println("")
      return machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Fichier n'existe pas")
        return null
    }
  }

  def main(args: Array[String]): Unit = {
    var machines = loadcsv("machines.csv")
    while (true) {
      var choix = 0
      while (choix != 1 && choix != 2 && choix != 3) {
        println("Nospresso Cafe \nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter")
        choix = readInt()
      }
      if (choix == 3) return

      var choixmachine = 0
      while (choixmachine != 1 && choixmachine != 2 && choixmachine != 3 && choixmachine != 4 && choixmachine != 5) {
        print("Machine sélectionnée (1-" + machines.length +") > ")
        choixmachine = readInt()
      }

      if (choix == 1){
        serveClient(choixmachine-1, machines)
      } else if (choix == 2) {
        println("Entrez le code PIN : ")

        var nbressais =  0
        while(!validatePin(machineId = choixmachine-1, machines = machines)){
          nbressais += 1
          if (nbressais < 3){
            println("Code PIN incorrect. " + (3 - nbressais) + " tentatives restantes")
          } else if (nbressais == 2){
            println("Code PIN incorrect. " + (3 - nbressais) + " tentative restante")
          }else{
            println("Trop de tentatives échouées. Fin du programme")
            return
          }
        }
        println("Accès accordé.")
        var choixAdmin = 0
        while (choixAdmin !=1 && choixAdmin!= 2){
          println("Choisissez votre mode: \n 1) Modification du code PIN \n 2) Recharge des stocks")
          choixAdmin = readInt()
        }
        if (choixAdmin == 1){
          updatePin(choixmachine-1, machines)
        }else{
          restockMachine(choixmachine-1, machines)
        }

      } else if (choix == 3) {
        return
      } else {
        print("Valeur non autorisée")
      }
      savecsv("machines.csv", machines)
      print("\n")
    }
  }
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean ={
    print("> ")
    var PINattempt = readInt()
    return PINattempt == machines(machineId).pincode.toInt
  }
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit ={
    println("Mise à jour du code PIN pour la Machine " + machineId + ".")
    print("Entrez un nouveau code PIN à 6 chiffres > ")
    var newPIN = readInt()
    while (newPIN.toString.length != 6){
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPIN = readInt()
    }
    machines(machineId).pincode = newPIN.toString
    println("Le code PIN à été mis à jour avec succès. \nRetour au menu principal... ")
  }
  def serveClient(machineId: Int, machines:ArrayBuffer[Machine]): Boolean ={
    val boisson = Array("Expresso", "Cappuccino", "Latte (Petit)", "Latte (Moyen)", "Latte (Grand)")
    val tableausucre = Array("Sans sucre", "Peu (5g)", "Moyen (10g)", "Beacoup (15g)")
    val tableaulait = Array("Oui", "Non")
    val tableaupoudre = Array(8, 6, 6, 8, 12)
    val tableauqtesucre = Array(0, 5, 10, 15)
    val tableauqtelait = Array(0, 100, 120, 150, 200)

    val prixboisson = Array(2.00, 2.50, 2.70, 3.20, 3.70)
    val prixsucre = Array(0.00, 0.10, 0.20, 0.30)
    val prixlait = 0.05
    var choixboisson = 0
    while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3) {
      println("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      choixboisson = readInt()
    }
    var choixtaille = 0
    if (choixboisson == 3) {
      while (choixtaille != 1 && choixtaille != 2 && choixtaille != 3) {
        println("\nVeuillez sélectionner la taille de votre latte: \n1) Petit \n2) Moyen \n3) Grand")
        choixtaille = readInt()
      }
    }
    var choixsucre = 0
    while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
      println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      choixsucre = readInt()
    }
    var choixlait = 0
    var choixdose = 0
    if (choixboisson != 1) {
      while (choixlait != 1 && choixlait != 2) {
        println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte) \n1) Oui\n2) Non")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        while (choixdose != 1 && choixdose != 2 && choixdose != 3) {
          println("\nCombien de dose ?")
          choixdose = readInt()
        }
      }
    }

    if (choixboisson == 3) {
      println("\nBoisson sélectionnée : " + boisson(choixboisson - 1 + choixtaille - 1))

    } else {
      println("\nBoisson sélectionnée : " + boisson(choixboisson - 1))

    }
    println("Niveau de sucre : " + tableausucre(choixsucre - 1))
    if (choixboisson != 1) {
      println("Lait supplémentaire: " + tableaulait(choixlait - 1))

    }
    var poudreqte = 0
    if (choixboisson == 3) {
      poudreqte = tableaupoudre(choixboisson - 1 + choixtaille - 1)

    } else {
      poudreqte = tableaupoudre(choixboisson - 1)

    }
    var laitqte = 0
    if (choixboisson == 2) {
      laitqte = tableauqtelait(choixboisson - 1)
    }
    if (choixboisson == 3) {
      laitqte = tableauqtelait(choixboisson - 1 + choixtaille - 1)
    }
    if (choixlait == 1) {
      laitqte += choixdose * 50
    }
    var sucreqte = tableauqtesucre(choixsucre - 1)
    if (!machines(machineId).removeIngredient("COFFEE",poudreqte)) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    }
    if (!machines(machineId).removeIngredient("MILK",laitqte)) {
      println("Erreur : Quantité́ de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    }
    if (!machines(machineId).removeIngredient("SUGAR",sucreqte)) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou vérifier les stocks en mode Admin.")
      return false
    }

    var prixtexte = ""
    var prix = 0.00
    if (choixboisson == 3) {
      prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1 + choixtaille - 1))
      prix = prixboisson(choixboisson - 1 + choixtaille - 1)
    } else {
      prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1))
      prix = prixboisson(choixboisson - 1)
    }

    if (choixsucre != 1) {
      prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, prixsucre(choixsucre - 1))
      prix += prixsucre(choixsucre - 1)

    }
    if (choixlait == 1) {
      prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, (choixdose * prixlait))
      prix += choixdose * prixlait

    }
    prixtexte += " = CHF " + "%.2f".formatLocal(java.util.Locale.US, prix)
    println("Prix total : " + prixtexte)
    println("\nVeuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString("").toUpperCase())
    println("(En attente de paiement...)\n")
    Thread.sleep(3000)
    println("Paiement confirmé.\nPréparation de votre boisson...")
    println("[...]")
    Thread.sleep(5000)
    println("Votre " + boisson(choixboisson - 1).split(" ")(0) + " est prêt ! Bonne dégustation !")

    return true
  }
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit ={
    println("Accès autorisé.\n\nStocks :")
    println("  Poudre de café : " + machines(machineId).coffee + "g")
    println("  Lait           : " + machines(machineId).milk / 1000.0 + "L")
    println("  Sucre          : " + machines(machineId).sugar + "g")
    println("\nRéapprovisionnement des stocks...")

    println("Ajout :")
    print("  Poudre de café: ")
    val ajoutPoudre = readInt()
    print("  Lait          : ")
    val ajoutLait = readDouble()
    print("  Sucre         : ")
    val ajoutSucre = readInt()

    machines(machineId).addIngredient("COFFEE",ajoutPoudre)
    machines(machineId).addIngredient("SUGAR",ajoutSucre)
    machines(machineId).addIngredient("MILK",(ajoutLait*1000).toInt)

    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }
}
class Machine(val id: Int, var pincode: String, var milk: Int,
              var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "MILK"){
      milk += amount
    } else if (ingredient == "SUGAR"){
      sugar += amount
    } else if (ingredient == "COFFEE"){
      coffee += amount
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "MILK"){
      if (milk < amount){
        return false
      }
      milk -= amount
    } else if (ingredient == "SUGAR"){
      if (sugar < amount){
        return false
      }
      sugar -= amount
    } else if (ingredient == "COFFEE"){
      if (coffee < amount){
        return false
      }
      coffee -= amount
    }
    return true
  }
}
