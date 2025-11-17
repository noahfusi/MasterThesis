import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import scala.io._

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {


  // Methode pour enlever les ingrédients dans les stocks
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    // Gestion de tous les cas
    if (ingredient == "coffee" && coffee >= amount) {
      coffee = coffee - amount
      true

    } else if (ingredient == "sugar" && sugar >= amount) {
      sugar = sugar - amount
      true


    } else if (ingredient == "milk" && milk >= amount) {
      milk = milk -  amount
      true

    } else {
      false
    }
  }

  // Méthode pour add les ingrédients dans les stocks
  def addIngredient(ingredient: String, amount: Int): Unit = {

    if (ingredient == "coffee") {
      coffee = coffee + amount


    } else if (ingredient == "sugar") {
      sugar = sugar + amount


    } else{
      milk = milk + amount

    }
  }


}


object Main {
  def restockMachine(machine : Machine): Unit={
    print("Stocks:\n" )
    println( f"Poudre de café: ${machine.coffee} g\nLait : ${machine.milk*0.001}%.2f L\nSucre :"+{machine.sugar}+"g")
    println( "Réapprovisionnement des stocks...\nAjout :")
    val coffeeStocks = readLine("\tPoudre de café:").toInt //on lui demande les valeures à ajouter
    val milkStocks =  ((readLine("\tLait :").toFloat)*1000).toInt
    val sugarStocks=  readLine("\tSucre :").toInt
    machine.addIngredient("coffee", coffeeStocks)
    machine.addIngredient("milk", milkStocks)
    machine.addIngredient("sugar", sugarStocks)
    println( "Niveaux de stock mis à jour.\nRetour au menu principal...")
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Sauvegarde de ${machines.length} machines dans $filename.")
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()

    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Échec de l'écriture dans $filename.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        sys.exit(1)
    }
    println("Fichier sauvegardé avec succès")
  }

  def serveClient(machine: Machine): Boolean = {

    var supplement = 0 //déclaration des variables qui doivent être reset (le "s" à la fin des variables signifie string)("incr" signifie incrémental)
    var stockcafe = machine.coffee
    var stocksucre = machine.sugar
    var stocklait = machine.milk
    var dose = 0
    var choixs = ""
    var sucreprix = 0.0
    var prix = 0.0
    var cafeincr = 0
    var laitincr = 0
    var sucres = ""
    var sucreincr = 0
    var laitprix = 0.0
    var choix = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
    while (!(choix >= 1 && choix <= 3)) { //demande du choix de la boisson
      choix = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
    }
    if (choix == 1) { //cas 1 et 2 sont simples, on déclare leurs valeurs propres à chaqu'une
      choixs = "Expresso"
      prix = 2
      cafeincr = cafeincr + 8
    }
    if (choix == 2) {
      choixs = "Capuccino"
      cafeincr = cafeincr + 6
      laitincr = 100
      prix = 2.5

    }
    if (choix == 3) { //cas latte, on demande la taille du latte
      choixs = "Latte"
      var taille = 0
      while (!(taille >= 1 && taille <= 3)) { //boucle pour un input juste
        taille = readLine("Quelle taille choisissez vous pour votre Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>").toInt
      }

      if (taille == 1) { //chaque taille a ses valeurs propres
        choixs = choixs + " (Petit)"
        prix = 2.7
        laitincr = 120
        cafeincr = 6
      }
      if (taille == 2) {
        choixs = choixs + " (Moyen)"
        prix = 3.2
        laitincr = 150
        cafeincr = 8
      }
      if (taille == 3) {
        choixs = choixs + " (Grand)"
        prix = 3.7
        laitincr = 200
        cafeincr = 12
      }
    }
    var nbrsucre = 0
    while (!(nbrsucre >= 1 && nbrsucre <= 4)) { //on demande la quantité de sucre
      nbrsucre = readLine("Souhaitez-vous ajouter du sucre ?  \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10  \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHF 0.30  \n>").toInt
    }
    if (nbrsucre == 1) { //chaque quantité a ses valeurs propres
      sucreincr = 0
      sucreprix = 0
      sucres = "Sans sucre"
    }
    else if (nbrsucre == 2) {
      sucreincr = 5
      sucreprix = 0.1
      sucres = "Peu (5g)"
    }
    else if (nbrsucre == 3) {
      sucreincr = 10
      sucreprix = 0.2
      sucres = "Moyen (10g)"
    }
    else if (nbrsucre == 4) {
      sucreincr = 15
      sucreprix = 0.3
      sucres = "Beaucoup (15g)"
    }
    if (choix != 1) { //si c'est un latte ou un cappuccino

      while (!(supplement >= 1 && supplement <= 2)) { //on demande le lait supplémentaire
        supplement = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toInt
      }
      if (supplement != 2) {

        while (!(dose >= 1 && dose <= 3)) { // on demande la dose
          dose = readLine("Combien de doses voulez vous ajouter ? (maximum 3)\n>").toInt
        }
        if (dose == 1) { //on ajoute selon la dose
          laitprix = 0.05
          laitincr = laitincr + 50

        }
        if (dose == 2) {
          laitprix = 0.1
          laitincr = laitincr + 100
        }
        if (dose == 3) {
          laitprix = 0.15
          laitincr = laitincr + 150
        }
      }
    }
    println(s"Boisson sélectionnée : $choixs \nNiveau de sucre : $sucres") //on print les texts

    if (!(choix == 1) && supplement == 1) {
      println(s"Lait en supplément: Oui, $dose dose(s)")
    }

    if (!(choix == 1) && supplement == 2) {

      println("Lait en supplément: Non")

    }
    var camarche = true
    if (stockcafe < cafeincr) { //on regarde toutes les erreurs liés aux stock

      laitincr = 0
      cafeincr = 0
      sucreincr = 0
      choix = 1
      camarche = false
      println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
    } else if (stocksucre < sucreincr) {
      println("Erreur : Quantité de poudre de sucre insuffisante pour\npr´eparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
      laitincr = 0
      cafeincr = 0
      sucreincr = 0
      camarche = false

    } else if (laitincr > stocklait && choix == 3) { // le texte est différent pour le lait selon si la boisson est un latte ou pas
      println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
      laitincr = 0
      cafeincr = 0
      sucreincr = 0
      camarche = false

    } else if (laitincr > stocklait && choix != 3) {
      println("Erreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
      laitincr = 0
      cafeincr = 0
      sucreincr = 0
      camarche = false

    } else if (camarche == true) { // si il n'y a pas d'erreurs nous pouvons valider la commande
      var prixtot = prix + sucreprix + laitprix
      print(f"Prix total : CHF $prix%.2f " + f"CHF $sucreprix%.2f + CHF $laitprix%.2f = CHF $prixtot%.2f\n") //print du prix
      var codetwint = " " //déclaration du code twint
      val chars_alphanum = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      for (x <- 1 to 5) {
        val obj = (math.random() * 36).toInt
        codetwint += chars_alphanum(obj)
      }

      println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)")
      Thread.sleep(3000)

      print("Merci ! Votre paiement a été accepté.\npréparation de votre boisson...\n")
      Thread.sleep(5000)
      println(s"Votre $choixs est prêt ! Bonne dégustation !")
      machine.removeIngredient("coffee", cafeincr)
      machine.removeIngredient("milk", laitincr)
      machine.removeIngredient("sugar", sucreincr)


    }
    if(camarche == true){
      return true
    }else {
      return false
    }
  }
  // METHODE POUR L'EXERCICE 3
  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    val array_machines = ArrayBuffer[Machine]()
    try {
      
      val contenu = Source.fromFile(filename)
      val all_lines = contenu.getLines()
      val all_lines_array = all_lines.toArray
      var k = 1

      while (k < all_lines_array.length) {
        val sections = all_lines_array(k).split(",")
        if (sections.length == 4) {
          array_machines += new Machine(k, sections(0), sections(1).toInt, sections(2).toInt, sections(3).toInt)
        }
        k += 1
      }

    } catch {
      case e: Exception =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
    }
    array_machines
  }

  def updatePin(machine: Machine): Unit = {
    println(s"Mise `a jour du code PIN pour la Machine ${machine.id+1}.")
    var pinutil = ""
    while (!pinutil.forall(_.isDigit) || pinutil.length != 6) {
      pinutil = readLine("Entrez un nouveau code PIN `a 6 chiffres > ")
    }
    machine.pincode = pinutil
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }

  def validatePin(machine: Machine): Boolean = {
    var codeutil = ""
    var essai = 2
    codeutil = readLine("Mode Admin\nEntrez le code PIN :")
    while (codeutil != machine.pincode && essai != 0) { // on demande le code à l'utilisateur
      codeutil = readLine(s"Code PIN incorrect. $essai tentatives restantes.")
      essai -= 1
    }

    if (codeutil == machine.pincode) {
      print("Accès accordé.")
      return true
    } else {
      println("Code PIN incorrect. 0 tentatives restantes.")
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }
  }


  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)
    println("\nMachines chargées :")
    for (machine <- machines) {
      println(s"Machine ${machine.id} chargée :")
      println(f"\tID: ${machine.id}")
      println(f"\tCode PIN: ${machine.pincode}")
      println(f"\tLait: ${machine.milk.toDouble / 1000}%.3fL")
      println(f"\tSucre: ${machine.sugar}g")
      println(f"\tCafé: ${machine.coffee}g")
      println()
    }
    println(s"${machines.length} machine(s) chargée(s) avec succès.")
    var nbMachines = 5

    var sugarStocks = Array.fill (nbMachines) (30)

    var machinePins = Array.fill (nbMachines) ("434343")

    var coffeeStocks = Array.fill (nbMachines) (50)

    var milkStocks = Array.fill (nbMachines)(500)
    var machineId = -1
    var stop = false
    while(stop == false){
      var menu = 0
      while(menu != 1 && menu != 2 && menu !=3){
        menu = readLine("Veuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter\n>").toInt
      }
      if(menu == 1){
        var reussi = false
        machineId = -5
        while(reussi==false){

          machineId = -5
          while(!(machineId>=0&&machineId<= machines.length - 1)){
            machineId = readLine(s"Machine sélectionnée (1-${machines.length}) >").toInt -1
          }
          val machine = machines(machineId)
          reussi = serveClient(machine)
          if(reussi != true){
            machineId = -5
            println("Veuillez changer de machine")
          }
        }
      }
      if(menu == 2){
        machineId = -5
        while(!(machineId>=0&&machineId<=machines.length - 1)){
          machineId = readLine(s"Machine sélectionnée (1-${machines.length}) >").toInt -1
        }
        val machine = machines(machineId)
        if(validatePin(machine)) {
          var admin_menu = 0
          while (!(admin_menu == 1 || admin_menu == 2)) {
            admin_menu = readLine("Veuillez sélectionner votre menu admin : \n1) restockage\n2) modification du pin\n>").toInt
          }
          if (admin_menu == 1) {
            restockMachine(machine)
          }
          if (admin_menu == 2) {
            updatePin(machine)
          }
        } else {
          stop = true
        }
      }
      if(menu ==3){
        savecsv(filename, machines)
        stop = true
      }
    }
  }
}

