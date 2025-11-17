import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit ={
      if (ingredient == "coffee") coffee += amount
      if (ingredient == "sugar") sugar += amount
      if (ingredient == "milk") milk += amount
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean ={
    if (ingredient == "coffee" && coffee >= amount) {
      coffee -= amount
      return true
    } else if (ingredient == "coffee") {
      println("Erreur : Quantité de poudre de café insuffisante.")
      return false
    }
    if (ingredient == "sugar" && sugar >= amount) {
      sugar -= amount
      return true
    } else if (ingredient == "sugar") {
      println("Erreur : Quantité de sucre insuffisante.")
      return false
    }
    if (ingredient == "milk" && milk >= amount) {
      milk -= amount
      return true
    } else if (ingredient == "milk") {
      println("Erreur : Quantité de lait insuffisante.")
      return false
    }
    println("Erreur : Ingrédient inconnu.")
    return false
  }
}


  var essaiPin = 0
  var cafeprovisoir = 0
  var sucreprovisoir = 0
  var laitprovisoir = 0

  def loadcsv(filename: String): ArrayBuffer[Machine] ={
    val machines = ArrayBuffer[Machine]()
    try {
      println("Chargement des machines depuis "+filename)
      val source = Source.fromFile(filename)
      val lignefr = source.reset.getLines
      var ligne = lignefr.next
      var i = 0
      while(!lignefr.isEmpty) {
        ligne = lignefr.next
        val colonne = ligne.split(",")
        machines += new Machine(i, colonne(0), colonne(1).toInt, colonne(2).toInt, colonne(3).toInt)
        i += 1
        println("\nMachine "+i+" chargée :\n" +
          "ID: "+i+"\n" +
          "Code Pin: "+ colonne(0)+"\n" +
          "Lait: "+ colonne(1)+"mL" + "\n" +
          "Sucre: "+ colonne(2)+"g" + "\n" +
          "Café: "+ colonne(3)+"mL" + "\n")
        }
      println("\n"+i+" machine(s) chargée(s) avec succès")
      source.close()
    } catch {
      case _ => println(s"Erreur : Le fichier '$filename' n'existe pas, ou n'est pas reconnu ")
    }
    return machines
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(filename)
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      var i = 0
      for (machine <- machines) {
        writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
        i += 1
      }
      println("Sauvegarde de "+i+" machines dans "+filename)
      writer.close()
    } catch {
      case _=> println("Erreur lors de l'écriture dans le fichier CSV: ")
    }
  }

  def main(args: Array[String]): Unit = {
    val machines = loadcsv("machines.csv")
    if(machines.length == 0){sys.exit(0)}
    val nbMachines = machines.length
    var machineID = readLine("Selectionnez votre machine entre 1 et "+nbMachines+" >").toInt
    while(machineID<1 || machineID>nbMachines){
      machineID = readLine("Saisie incorrect,\n" +
      "Choisissez entre 1 et "+nbMachines+" >").toInt
    }
    machineID-=1
    var choixmode = 0
    do{
      println("     Nospresso     \n" +
        "Veuillez selectionner votre mode :\n" +
        "1) Client \n2) Admin \n3) Quitter\n>")
      choixmode = readInt()
      while((choixmode > 3) || (choixmode == 0)) {
        println("Saisie incorrect, veuillez choisir entre 1, 2 et 3\n>")
        choixmode = readInt()
      }
      if (choixmode == 1) {
        val machine = machineID + 1
        println("Vous avez choisi la machine n°" + machine)
        serveClient(machineID,machines)
      }
      else if (choixmode == 2) {
        val machine = machineID + 1
        println("Vous avez choisi la machine n°" + machine)
        val validationPin = validatePin(machineID, machines)
        if (validationPin) {
          println("Accès accordé à la Machine " + machine +
            "\nChoisissez l'action a effectuer :\n" +
            "1) Accéder aux stocks\n" +
            "2) Changer le code pin")
          val admin = readInt()
          while (admin > 2 || admin < 0) {
            println("Saisie incorrect,\n" +
              "Choisissez 1, ou 2")
            val admin = readInt()
          }
          if (admin == 1) restockMachine(machineID, machines)
          if (admin == 2) updatePin(machineID, machines)
        }
      }
      else if (choixmode == 3) {
        println("Vous quittez le programme.")
        savecsv("machines.csv",machines)
      }
    } while (choixmode != 3)

  }
  def validatePin(machineID: Int, machines: ArrayBuffer[Machine]): Boolean = {
    essaiPin = 3
    while(essaiPin > 0){
      println("Entrez le code pin :")
      val code = readInt().toString
      if(code == machines(machineID).pincode){
        println("Accès accordé")
        return true
      }
      else {
        essaiPin -= 1
        println("Code PIN incorrect. "+ essaiPin +" tentavives restante")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }
  def updatePin(machineID: Int, machines: ArrayBuffer[Machine]): Unit = {
    while(true){
      val machine = machineID + 1
      println("Mise à jour du code PIN pour la Machine "+ machine)
      val newCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if ((newCode.length == 6) && newCode.forall(Character.isDigit)){
        machines(machineID).pincode = newCode
        println("Le code PIN a été mis à jour avec succès.\n" +
          "Retour au menu principal...")
        savecsv("machines.csv",machines)
        return
      }
    }
  }
  def restockMachine(machineID: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Stocks:\n" +
      "Poudre de café: "+machines(machineID).coffee +"g\n" +
      "Lait : "+machines(machineID).milk+"mL\n" +
      "Sucre : "+machines(machineID).sugar+"g\n\n")
    println("Réapprovisionnement des stocks...\nAjout :\n")
    machines(machineID).coffee += readLine("Poudre de café: ").toInt
    machines(machineID).milk += readLine("Lait : ").toInt
    machines(machineID).sugar += readLine("Sucre : ").toInt
    println("Niveaux de stock mis à jour.\n" +
      "Retour au menu principal...")
    savecsv("machines.csv",machines)
  }
  def serveClient(machineID: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var prix = 0.00
    var prixcafe = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00

    var nomcafe = ""
    println("Choisissez votre boisson :\n" +
      "1) Expresso - CHF 2.00\n" +
      "2) Cappuccino - CHF 2.50\n" +
      "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
    var choixcafe = readInt()
    while((choixcafe>3) || (choixcafe==0)){
      println("Saisie incorrect, veuillez choisir entre 1, 2 et 3\n>")
      choixcafe = readInt()
    }
    if (choixcafe == 1){
      println("Boisson sélectionnée : Expresso")
      cafeprovisoir += 8
      prix += 2.00
      prixcafe += 2.00
      nomcafe = "Expresso"
    }
    else if (choixcafe == 2){
      println("Boisson sélectionnée : Cappuccino")
      cafeprovisoir += 6
      laitprovisoir += 100
      prix += 2.50
      prixcafe += 2.50
      nomcafe = "Cappuccino"
    }
    else if (choixcafe == 3){
      nomcafe = "Latte"
      println("Choisissez la taille :\n" +
        "1) Petit\n" +
        "2) Moyen\n" +
        "3) Grand\n>")
      var choixtaille = readInt()
      while((choixtaille>3) || (choixtaille==0)){
        println("Saisie incorrect, veuillez choisir entre 1, 2 et 3\n>")
        choixtaille = readInt()
      }
      if (choixtaille == 1){
        println("Boisson sélectionnée : Petit Latte")
        cafeprovisoir += 6
        laitprovisoir += 120
        prixcafe += 2.70
        prix += 2.70
      }else if (choixtaille == 2){
        println("Boisson sélectionnée : Moyen Latte")
        cafeprovisoir += 8
        laitprovisoir += 150
        prixcafe += 3.20
        prix += 3.20
      }else{
        println("Boisson sélectionnée : Grand Latte")
        cafeprovisoir += 12
        laitprovisoir += 200
        prixcafe += 3.70
        prix += 3.70
      }
    }

    println("Choisissez la quantitée de sucre souhaitée :\n" +
      "1) Sans sucre\n" +
      "2) Peu (5g) - CHF 0.10\n" +
      "3) Moyen (10g) - CHF 0.20\n" +
      "4) Beaucoup (15g) - CHF 0.30\n>")
    var choixsucre = readInt()
    while((choixsucre>4) || (choixsucre==0)){
      println("Saisie incorrect, veuillez choisir entre 1, 2, 3 et 4\n>")
      choixsucre = readInt()
      sucreprovisoir = 0
    }
    if(choixsucre == 2){
      sucreprovisoir += 5
      prixsucre += 0.10
      prix += 0.10
    }
    if(choixsucre == 3){
      sucreprovisoir += 10
      prixsucre += 0.20
      prix += 0.20
    }
    if(choixsucre == 4){
      sucreprovisoir += 15
      prixsucre += 0.30
      prix += 0.30
    }
    if(choixcafe != 1){
      println("Souhaitez-vous ajouter du lait en supplément ?\n" +
        "(Disponible uniquement pour Cappuccino et Latte)\n" +
        "1)Oui\n2)Non\n>")
      var choixlait = readInt()
      while((choixlait>2) || (choixlait==0)){
        println("Saisie incorrect, veuillez choisir entre 1 et 2\n>")
        choixlait = readInt()
      }
      if(choixlait==1){
        println("Combien de dose ?\n>")
        var dose = readInt()
        while((dose>3) || (dose==0)) {
          println("Vous ne pouvez choisir qu'entre 1 et 3 doses\n>")
          dose = readInt()
        }
        if(dose == 1) {
          laitprovisoir += 50
          prixlait += 0.05
          prix += 0.05
        }
        if(dose == 2){
          laitprovisoir += 100
          prixlait += 0.10
          prix += 0.10
        }
        if(dose == 3){
          laitprovisoir += 150
          prixlait += 0.15
          prix += 0.15
        }
      }
    }
    if((cafeprovisoir>machines(machineID).coffee)||(sucreprovisoir>machines(machineID).sugar)||(laitprovisoir>machines(machineID).milk)) {
      if(cafeprovisoir>machines(machineID).coffee){
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n" +
          "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
      else if(sucreprovisoir>machines(machineID).sugar){
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n" +
          "Veuillez choisir une plus petite quantité de sucre ou vérifier les stocks en mode Admin.")
        return false
      }
      else if(laitprovisoir>machines(machineID).milk){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
          "Veuillez choisir une plus petite quantité de sucre ou vérifier les stocks en mode Admin.")
        return false
      }
    }
    if((machines(machineID).milk >=laitprovisoir) && (machines(machineID).sugar >=sucreprovisoir) && (machines(machineID).coffee >=cafeprovisoir)){
      if((prixsucre==0) && (prixlait==0)){
        printf("Prix total : CHF "+prixcafe+"  = CHF "+prix+"\n")
      }
      if((prixlait!=0) && (prixsucre==0)){
        printf("Prix total : CHF "+prixcafe+" + CHF "+prixlait+"  = CHF "+prix+"\n")
      }
      if((prixlait==0) && (prixsucre!=0)){
        printf("Prix total : CHF "+prixcafe+" + CHF "+prixsucre+"  = CHF "+prix+"\n")
      }
      if((prixlait!=0) && (prixsucre!=0)){
        printf("Prix total : CHF "+prixcafe+" + CHF "+prixsucre+" + CHF "+prixlait+" = CHF "+prix+"\n")
      }
      val alphabet_code = "ACDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
      var code = ""
      var index = 0
      for(i <- 0 to 4){
        index = (math.random()*alphabet_code.length).toInt
        code += alphabet_code(index)
      }
      println("Veuillez payer en utilisant Twint.\n" +
        "Votre code de paiement est :" + code +"\n" +
        "(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...\n[...]")
      println("Votre " + nomcafe + " est prêt ! Bonne dégustation !")
      machines(machineID).coffee -= cafeprovisoir
      machines(machineID).milk -= laitprovisoir
      machines(machineID).sugar -= sucreprovisoir
      code = ""
      prixcafe = 0
      prixsucre = 0
      prixlait = 0
      prix = 0
      cafeprovisoir = 0
      sucreprovisoir = 0
      laitprovisoir = 0
    }
    savecsv("machines.csv",machines)
    false
  }
}