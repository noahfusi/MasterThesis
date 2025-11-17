import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
object Main {
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentative = 3
    var saisiPIN = ""
    println("Entrez le code PIN : ")
    while(tentative > 0 && saisiPIN != machinePins(machineId)){
      saisiPIN = readLine(">")
      if(saisiPIN != machinePins(machineId)){
        tentative -=1
        if(tentative > 1)
          println("Code PIN incorrect. "+ tentative +" tentatives restantes.")
        else if(tentative == 1)
          println("Code PIN incorrect. "+ tentative +" tentative restante.")
        else
          return false
      }
      else{
        return true
      }
    }
    return false
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var nouveauPin = ""
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while(nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)){
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(machineId) = nouveauPin
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var cafeConsomme = 0
    var laitConsomme = 0
    var sucreAjoute = 0
    var laitAjoute = 0
    val prix = ArrayBuffer[Double]()
    var sucreSupp = Array("Sans sucre", "Peu (5g)", "Moyen (10g)", "Beaucoup (15g)")
    var laitSupp = Array("Non", "1 dose", "2 doses", "3 doses")
    var boisson = ""
    var boissonTaille = ""


    while(boisson != "1" && boisson != "2" && boisson != "3"){
      println("\nVeuillez sélcectionner votre boisson : \n1) Expresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      boisson = readLine(">")
      if(boisson != "1" && boisson != "2" && boisson != "3")
        println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
    }
    if(boisson == "1"){
      boisson = "Expresso"
      prix += 2.00
      cafeConsomme = 8
    }
    else if(boisson == "2"){
      boisson = "Cappuccino"
      prix += 2.50
      cafeConsomme = 6
      laitConsomme = 100
    }
    else if(boisson == "3"){
      boisson = "Latte"

      while (boissonTaille != "1" && boissonTaille != "2" && boissonTaille != "3") {
        println("\nVeuillez sélectionner la taille de votre boisson : \n1) Petit\n2) Moyen\n3) Grand")
        boissonTaille = readLine(">")
        if (boissonTaille != "1" && boissonTaille != "2" && boissonTaille != "3")
          println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
      }

      if(boissonTaille == "1"){
        boissonTaille = "(Petit)"
        prix += 2.70
        cafeConsomme = 6
        laitConsomme = 120
      }
      else if(boissonTaille == "2"){
        boissonTaille = "(Moyen)"
        prix += 3.20
        cafeConsomme = 8
        laitConsomme = 150
      }
      else if(boissonTaille == "3"){
        boissonTaille = "(Grand)"
        prix += 3.70
        cafeConsomme = 12
        laitConsomme = 200
      }
    }

    var choix = ""
    while(choix != "1" && choix != "2" && choix != "3" && choix != "4"){
      println("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      choix = readLine(">")
      if(choix != "1" && choix != "2" && choix != "3" && choix != "4")
        println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
    }
    sucreAjoute = choix.toInt - 1
    if(choix.toInt != 1)
      prix += (sucreAjoute * 0.10)

    if(boisson != "Expresso") {
      choix = ""
      while(choix != "1" && choix != "2"){
        println("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
        choix = readLine(">")
        if(choix != "1" && choix != "2")
          println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
      }
      if(choix == "1"){
        choix = ""
        while(choix != "1" && choix != "2" && choix != "3"){
          println("\nCombien de dose ? \n1) 1 dose : 50mL\n2) 2 doses : 100mL\n3) 3 doses : 150mL")
          choix = readLine(">")
          if(choix != "1" && choix != "2" && choix != "3" && choix != "4")
            println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
        }
        laitAjoute = choix.toInt
        prix += laitAjoute * 0.05
      }
      else{
        laitAjoute = 0
      }
    }

    if(cafeConsomme <= coffeeStocks(machineId) &&
      ((laitAjoute * 50) + laitConsomme) <= milkStocks(machineId) &&
      (sucreAjoute * 5 <= sugarStocks(machineId))
    ){
      coffeeStocks(machineId) -= cafeConsomme
      milkStocks(machineId) -= (laitAjoute * 50) + laitConsomme
      sugarStocks(machineId) -= sucreAjoute * 5

      println("-------------------------------------------------------------------------------------")
      println("Boisson sélectionnée : " + boisson + " " + boissonTaille)
      println("Niveau de sucre : " + sucreSupp(sucreAjoute))
      if(boisson != "Expresso")
        println("Lait en supplément : " + laitSupp(laitAjoute))
      if (prix.length == 1) {
        println("Prix total : CHF " + "%.2f".format(prix.sum))
      }
      else {
        var formattedPrix = ""
        for (i <- prix.indices) {
          formattedPrix += "%.2f".format(prix(i))
          if (i < prix.length - 1) formattedPrix += " + CHF "
        }
        println("Prix total : CHF " + formattedPrix + " = CHF " + "%.2f".format(prix.sum))
      }

      println("\nVeuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString)
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      return true
    }
    else{
      return false
    }
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var quantite = ""
    println("\nNiveaux de stock actuels :")
    println("    Poudre de café : " + coffeeStocks(machineId) + "g")
    println("    Sucre : " + sugarStocks(machineId) + "g")
    println("    Lait : " + (milkStocks(machineId) * 0.001) + "L\n")
    println("Entrez les quantités à ajouter :")

    quantite = readLine("Poudre de café >").trim
    while(quantite.length == 0 || !quantite.forall(_.isDigit) || (quantite.forall(_.isDigit) && quantite.toInt < 0)){
      println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre entier positif.")
      quantite = readLine("Poudre de café >").trim
    }
    coffeeStocks(machineId) += quantite.toInt
    quantite = ""

    quantite = readLine("Sucre >").trim
    while(quantite.length == 0 || !quantite.forall(_.isDigit) || (quantite.forall(_.isDigit) && quantite.toInt < 0)){
      println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre entier positif.")
      quantite = readLine("Sucre >").trim
    }
    sugarStocks(machineId) += quantite.toInt
    quantite = ""

    quantite = readLine("Lait >").trim
    while(quantite.length == 0 || !quantite.forall(_.isDigit) || (quantite.forall(_.isDigit) && quantite.toInt < 0)){
      println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer un nombre entier positif.")
      quantite = readLine("Lait >").trim
    }
    milkStocks(machineId) += quantite.toInt
    quantite = ""

    println("\nLes stocks ont été mis à jour avec succès.")
    println("\nNiveaux de stock actuels :")
    println("    Poudre de café : " + coffeeStocks(machineId) + "g")
    println("    Sucre : " + sugarStocks(machineId) + "g")
    println("    Lait : " + (milkStocks(machineId) * 0.001) + "L")
    println("\nRetour au menu principal...\n")
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var programRunning = true
    var machinePins = Array.fill(nbMachines)("434343")
    var coffeeStocks = Array.fill(nbMachines)(50)
    var milkStocks = Array.fill(nbMachines)(500)
    var sugarStocks = Array.fill(nbMachines)(30)

    while(programRunning){
      var machineId = 0
      var mode = 0
      var choix = ""
      println("\n               Nospresso Café")

      choix = readLine("Machine sélectionnée (1-" + nbMachines + ") > ").trim
      while (choix.length == 0 || !choix.forall(_.isDigit) || (choix.forall(_.isDigit) && (choix.toInt <= 0 || choix.toInt > nbMachines))) {
        if (choix.length == 0 || !choix.forall(_.isDigit)) {
          println("Erreur : Vous avez fait une saisie incorrecte. Veuillez entrer uniquement des chiffres entre les valeurs spécifiées.")
        }
        else if (choix.toInt <= 0 || choix.toInt > nbMachines) {
          println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
        }
        choix = readLine("Machine sélectionnée (1-" + nbMachines + ") > ").trim
      }
      machineId = choix.toInt - 1

      choix = ""
      while(choix != "1" && choix != "2" && choix != "3"){
        println("\nVeuillez sélectionner votre mode: \n1) Client\n2) Admin\n3) Quitter")
        choix = readLine(">")
        if(choix != "1" && choix != "2" && choix != "3")
          println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
      }
      mode = choix.toInt

      if(mode == 1){
        if (serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
          println("\nPaiement confirmé.")
          println("Préparation de votre boisson...")
          println("Votre boisson est prêt ! Bonne dégustation !")
          println("-------------------------------------------------------------------------------------")
        } else {
          print("\nErreur : Les stocks de cette machine sont insuffisants pour la boisson choisie. Veuillez choisir une autre boisson ou une autre machine.\n")
        }
      }
      else if(mode == 2){
        println("Mode Admin")
        if(validatePin(machineId, machinePins)){
          println("Accès accordé à la Machine " + (machineId + 1) +".")
          choix = ""
          while (choix != "1" && choix != "2") {
            println("\nVeuillez choisir une action :\n1) Ajouter un nouveau stock\n2) Modifier le code PIN")
            choix = readLine(">")
            if (choix != "1" && choix != "2")
              println("Erreur : Votre sélection n'est pas valide. Veuillez réessayer.")
          }
          if(choix == "1"){
            restockMachine(machineId, coffeeStocks,sugarStocks,milkStocks)
          }
          else if(choix == "2"){
            updatePin(machineId, machinePins)
          }
        }
        else{
          println("Trop de tentatives échouées. Fin du programme.")
          programRunning = false
        }
      }
      else{
        println("Le programme a été terminé.")
        programRunning = false
      }
    }
  }
}