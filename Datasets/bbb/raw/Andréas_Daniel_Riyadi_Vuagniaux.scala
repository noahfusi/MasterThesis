import scala.io.StdIn._

object main {
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean={
    println("Entrez le code PIN :")
    val pass = readLine().toInt
    if (pass.toString != machinePins(machineId)){
      false
    }
    else{
      true
    }
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit={
    // Demander un nouveau code PIN valide
    var validPin = false
    while (!validPin) {
      println("Entrez le nouveau code PIN à 6 chiffres:")
      var newPin = readLine().toInt
      if (newPin.toString.length == 6){
        validPin = true
        machinePins(machineId)= newPin.toString
      }
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {


    var boisson = ""
    var boissonPrix = 0.00
    var cafeUtiliser = 0
    var laitUtiliser = 0

    var sucreNivNom = ""
    var sucreUtiliser = 0
    var sucrePrix = 0.00

    var laitSupNom = ""
    var dosePrix = 0.00

    var erreur = false
    println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
    var choixBoisson = readLine(">").toInt
    while ((choixBoisson != 1) && (choixBoisson != 2) && (choixBoisson != 3)) {
      println("Veuillez sélectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappuccino - CHF 2.50\n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF3.70 (Grand)")
      choixBoisson = readLine(">").toInt
    }
    if (choixBoisson == 1) {
      boisson = "Expresso"
      boissonPrix = 2.00
      cafeUtiliser = 8
      laitUtiliser = 0
    }

    else if (choixBoisson == 2) {
      boisson = "Cappuccino"
      boissonPrix = 2.50
      cafeUtiliser = 6
      laitUtiliser = 100
    }
    else {
      println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
      var taille = readLine(">").toInt
      while ((taille != 1) && (taille != 2) && (taille != 3)) {
        println("Sélectionner la taille de votre latte :\n\t1) Petit - CHF 2.70  \n\t2) Moyen - CHF 3.20  \n\t3) Grand - CHF 3.70 ")
        taille = readLine(">").toInt
      }
      boisson = "Latte (Petit)"
      boissonPrix = 2.70
      cafeUtiliser = 6
      laitUtiliser = 120
      if (taille == 2) {
        boisson = "Latte (Moyen)"
        boissonPrix = 3.20
        cafeUtiliser = 8
        laitUtiliser = 150
      }
      else if (taille == 3) {
        boisson = "Latte (Grand)"
        boissonPrix = 3.70
        cafeUtiliser = 12
        laitUtiliser = 200
      }
    }
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    var sucreNiv = readLine(">").toInt
    while ((sucreNiv != 1) && (sucreNiv != 2) && (sucreNiv != 3) && (sucreNiv != 4)) {
      println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g)- CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      sucreNiv = readLine(">").toInt
    }
    if (sucreNiv == 1) {
      sucreNivNom = "Sans Sucre"
      sucreUtiliser = 0
    }

    if (sucreNiv == 2) {
      sucreNivNom = "Peu (5g)"
      sucreUtiliser = 5
      sucrePrix = 0.10
    }
    else if (sucreNiv == 3) {
      sucreNivNom = "Moyen (10g)"
      sucreUtiliser = 10
      sucrePrix = 0.20
    }
    else if (sucreNiv == 4) {
      sucreNivNom = "Grand (15g)"
      sucreUtiliser = 15
      sucrePrix = 0.30
    }
    if ((choixBoisson == 2) || (choixBoisson == 3)) {
      println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
      var laitSup = readLine("").toInt
      while ((laitSup != 1) && (laitSup != 2)) {
        println(" Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
        laitSup = readLine("").toInt
      }

      if (laitSup == 1) {
        laitSupNom = "Oui"
        println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
        var dose = readLine(">").toInt
        while ((dose != 0) && (dose != 1) && (dose != 2) && (dose != 3)) {
          println("Combien de dose ?\n(3 doses maximale par boisson, une dose contiens 50ml de lait)")
          dose = readLine(">").toInt
        }
        laitUtiliser += 50
        dosePrix = dose * 0.05
      }
      else {
        laitSupNom = "Non"
        dosePrix = 0.00
      }


    }
    //Gestion de stock et d'erreur
    if (choixBoisson == 1) {
      if (coffeeStocks(machineId) < cafeUtiliser) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        erreur = true
      }
      if (sugarStocks(machineId) < sucreUtiliser) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        erreur = true
      }
    }
    else if (choixBoisson == 2) {
      if (coffeeStocks(machineId) < cafeUtiliser) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        erreur = true
      }
      if (sugarStocks(machineId) < sucreUtiliser) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        erreur = true
      }
      if (milkStocks(machineId) < laitUtiliser) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        erreur = true
      }
    }
    else {
      if (coffeeStocks(machineId) < cafeUtiliser) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
        erreur = true
      }
      if (sugarStocks(machineId) < sucreUtiliser) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
        erreur = true
      }
      if (milkStocks(machineId) < laitUtiliser) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
        erreur = true
      }
    }

    if (!erreur){
      // resume de la boisson personnaliser
      println("Boisson sélectionnée : " + boisson + "\nNiveau de sucre : " + sucreNivNom + "\nLait en supplément : " + laitSupNom)
      if(laitSupNom == "Oui"){
        val prixFinal = boissonPrix + sucrePrix + dosePrix
        println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " + CHF "+dosePrix + " = CHF " + prixFinal)
      }
      else{
        val prixFinal = boissonPrix + sucrePrix
        println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " = CHF " + prixFinal)

      }

      //payment
      // On genere le code de twint
      val Alphanumerique = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      var codetwint = ""
      for (i <- 0 to 4) {
        codetwint = codetwint + Alphanumerique((math.random() * Alphanumerique.length).toInt)
      }
      println(" Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)")
      Thread.sleep(3000) // Attend pendant 3000 millisecondes (31 secondes)
      println("Merci ! Votre paiement a été accepté.")
      //preparation de boisson
      println("Préparation de votre boisson...\n[...]\nVotre "+ boisson +" est prêt ! Bonne dégustation !")
      //aprés chaque transaction réussie, les quantités d’ingrédients utilisées
      //(poudre de café, sucre, lait) sont automatiquement déduites des stocks
      coffeeStocks(machineId) = coffeeStocks(machineId) - cafeUtiliser
      sugarStocks(machineId) = sugarStocks(machineId) - sucreUtiliser
      milkStocks(machineId) = milkStocks(machineId) - laitUtiliser
      true
    }
    else{
      println("Selectionner Une autre machines")
      false
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var ajoutPoudre = readLine("\tPoudre de café : ").toInt
    while(ajoutPoudre<0){
      ajoutPoudre = readLine("\tPoudre de café : ").toInt
    }
    coffeeStocks(machineId) = coffeeStocks(machineId) + ajoutPoudre
    var ajoutlait = readLine("\tLait : ").toInt
    while(ajoutlait<0){
      ajoutlait = readLine("\tLait : ").toInt
    }
    milkStocks(machineId) = milkStocks(machineId) + ajoutlait
    var ajoutSucre = readLine("\tSucre : ").toInt
    while(ajoutSucre<0){
      ajoutSucre = readLine("\tSucre : ").toInt
    }
    sugarStocks(machineId) = sugarStocks(machineId) + ajoutSucre
  }

  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    val  machinePins = Array.fill(nbMachines)("434343")

    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)

    var choixMode = 1

    while(choixMode!=3){
      println("\tNospresso Café\n Veuillez sélectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter")
      choixMode = readLine(">").toInt
      while((choixMode!=1)&&(choixMode!=2)&&(choixMode!=3)){
        println("\tNospresso Café\n Veuillez sélectionner votre mode :\n 1) Client\n 2) Admin\n 3) Quitter")
        choixMode = readLine(">").toInt
      }
      if (choixMode == 1) {
        println("Machine selectionnee (1-"+nbMachines+") ")
        var machineNum = readLine(">").toInt
        while((machineNum<1)||(machineNum>nbMachines)){
          println("Machine selectionnee (1-"+nbMachines+") ")
          machineNum = readLine(">").toInt
        }
        val machineId = machineNum -1
        val service = serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      }
      // Choix de mode admin
      else if (choixMode == 2) {
        println("Mode Admin")
        println("Machine selectionnee (1-"+nbMachines+") ")
        var machineNum = readLine(">").toInt
        while((machineNum<1)||(machineNum>nbMachines)){
          println("Machine selectionnee (1-"+nbMachines+") ")
          machineNum = readLine(">").toInt
        }
        val machineId = machineNum -1

        println("Mode Admin")
        var tentatives = 3
        while(tentatives>0){
          val pin = validatePin(machineId, machinePins)
          //test de pin
          if(pin) {
            println("Accès autorisé.\n")
            println("Choisir ce que vous voulez faire\n1) Reapprovisionner les ingredients\n2) Mettre a jour le code PIN")
            var choixAd = readLine(">").toInt
            while((choixAd!=1)&&(choixAd!=2)){
              println("Choisir ce que vous voulez faire\n1) Reapprovisionner les ingredients\n2) Mettre a jour le code PIN")
              choixAd = readLine(">").toInt
            }
            if(choixAd==1){
              println("Stocks:\n\tPoudre de café : " + coffeeStocks(machineId) + "g\n\tLait : " + milkStocks(machineId) + "L\n\tSucre : " + sugarStocks(machineId) + "g")
              println("\nRéapprovisionnement des stocks...\nAjout :")
              val restock = restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
              println("Les stocks ont ete mis a jour avec succes.\n Retour au menu principal...")
              tentatives = 0
            }
            else{
              val nouveauPin = updatePin(machineId, machinePins)
              println("Le code PIN a ete mis a jour avec succes.\n Retour au menu principal...")
              tentatives = 0
            }

          }
          else{
            tentatives -=1
            println("Code PIN incorrect. "+tentatives+" tentatives restantes.")
            if (tentatives == 0){
              println("Trop de tentatives echouees. Fin du programme.")
              choixMode = 3
            }
          }
        }

      }
    }

  }
}