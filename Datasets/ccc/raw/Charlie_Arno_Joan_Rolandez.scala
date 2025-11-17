import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int){

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient=="milk") milk+=amount
      else if (ingredient=="sugar") sugar+=amount
      else if (ingredient=="coffee") coffee+=amount
      else println("Ingrédient n'existe pas")
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient=="milk") {
        if (amount>milk) return false
        else {milk-=amount
        return true}
      }

      else if (ingredient=="sugar") {
        if (amount>sugar) return false
        else {sugar-=amount
          return true}
      }

      else if (ingredient=="coffee") {
        if (amount>coffee) return false
        else {coffee-=amount
          return true}
      }
      else return false
    }
  }



  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var tryPIN = readLine("Entrez le code PIN :\n> ")
    return machines(machineId).pincode == tryPIN
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var mach_Wr = machineId + 1
    println("Mise à jour du code PIN pour la Machine " + mach_Wr + ".")
    var newPIN = 0
    while (newPIN.toString.length != 6) {
      try{
      newPIN = readLine("Entrez un nouveau code PIN à 6 chiffres > ").toInt
      }
      catch{
        case _ => println("Veuillez fournir une entrée valide")
      }
    }
    machines(machineId).pincode = newPIN.toString
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...\n")
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var choixboisson = readLine("> ").toInt

    //Test Valeur correcte
    while (choixboisson < 1 || choixboisson > 3) {
      println("Veuillez fournir une entrée valide")
      choixboisson = readLine("> ").toInt
    }

    var taille: Int = 0
    if (choixboisson == 3) {
      println("Veuillez sélectionner la taille")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      taille += readLine("> ").toInt

      //Test Valeur correcte
      while (taille < 1 || taille > 3) {
        println("Veuillez fournir une entrée valide")
        taille = readLine("> ").toInt
      }
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    var addsucre = readLine("> ").toInt
    //Test Valeur correcte
    while (addsucre < 1 || addsucre > 4) {
      println("Veuillez fournir une entrée valide")
      addsucre = readLine("> ").toInt
    }

    var doses: Int = 0
    if (choixboisson == 2 || choixboisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      var addmilk = readLine("> ").toInt
      //Test Valeur correcte
      while (addmilk < 1 || addmilk > 2) {
        println("Veuillez fournir une entrée valide")
        addmilk = readLine("> ").toInt
      }
      if (addmilk == 1) {
        println("Combien de dose ?")
        doses = readLine("> ").toInt
        //Test Valeur correcte
        while (doses < 0 || doses > 3) {
          println("Veuillez fournir une entrée valide")
          doses = readLine("> ").toInt
        }
      }
    }
    //Test des quantités
    var quant_cafe: Int = 0
    var quant_lait: Int = 0
    var quant_sucre: Int = 0
    var prixboisson: Double = 0
    var prixsucre: Double = 0
    var prixlait: Double = 0

    if (choixboisson == 1) {
      println("\nBoisson sélectionnée : Expresso")
      quant_cafe += 8
      prixboisson += 2.00
    }
    else if (choixboisson == 2) {
      println("\nBoisson sélectionnée : Cappuccino")
      quant_cafe += 6
      quant_lait += 100
      prixboisson += 2.50
    }
    else if (choixboisson == 3) {
      print("\nBoisson sélectionnée : Latte")
      if (taille == 1) {
        println(" (Petit)")
        quant_cafe += 6
        quant_lait += 120
        prixboisson += 2.70
      }
      else if (taille == 2) {
        println(" (Moyen)")
        quant_cafe += 8
        quant_lait += 150
        prixboisson += 3.20
      }
      else if (taille == 3) {
        println(" (Grand)")
        quant_cafe += 12
        quant_lait += 200
        prixboisson += 3.70
      }
    }

    print("Niveau de sucre : ")
    if (addsucre != 1 && addsucre != 0) {
      if (addsucre == 2) println("Peu (5g)")
      else if (addsucre == 3) println("Moyen (10g)")
      else if (addsucre == 4) println("Beaucoup (15g)")
      quant_sucre += addsucre * 5 - 5
      prixsucre += addsucre * 0.1 - 0.1
    }
    else println("Sans Sucre")

    print("Lait en supplément: ")
    if (doses == 0) {
      println("Non")
    }
    else {
      println("Oui, " + doses + " doses")
      quant_lait += doses * 50
      prixlait += doses * 0.05
    }


    if (quant_cafe<=machines(machineId).coffee){
      if (quant_lait<=machines(machineId).milk){
        if (quant_sucre<=machines(machineId).sugar){
          var total = prixboisson + prixlait + prixsucre
          printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixboisson, prixlait, prixsucre, total)
          machines(machineId).coffee-=quant_cafe
          machines(machineId).milk-=quant_lait
          machines(machineId).sugar-=quant_sucre

          //Génération Code Twint
          val MDP_Gen = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          val Twint_Code = Random.alphanumeric.filter(char => MDP_Gen.contains(char)).take(5).mkString

          println("\nVeuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + Twint_Code)
          println("(En attente de validation du paiement...)")
          Thread.sleep((3000))
          println("\nMerci ! Votre paiement a été accepté.")
          println("Préparation de votre boisson...\n[...]")
          Thread.sleep(5000)
          var boisson: String = ""
          if (choixboisson == 1) boisson = "Expresso"
          else if (choixboisson == 2) boisson = "Cappuccino"
          else boisson = "Latte"
          println("Votre " + boisson + " est prêt ! Bonne dégustation !\n\n")
          return true
        }
        else {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
          return false
        }
      }
      else {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        return false
      }
    }
    else {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
      return false
    }
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Réapprovisionnement des stocks...")
    println("Ajout:")

    var err = true
    while (err) {
      try {
        //Ajout des quantités
        var addCoffee = readLine("Poudre de café: ").toInt
        machines(machineId).addIngredient("coffee", addCoffee)
        err = false
      }
      catch {
        case _ => println("Veuillez fournir une entrée valide")
      }
    }

    err = true
    while (err) {
      try{
        var addMilk = readLine("Lait          : ").toInt
        machines(machineId).addIngredient("milk",addMilk)
        err = false
      }
      catch{
        case _ => println("Veuillez fournir une entrée valide")
      }
    }

    err = true
    while (err) {
      try {
        var addSugar = readLine("Sucre         : ").toInt
        machines(machineId).addIngredient("sugar", addSugar)
        err = false
      }
      catch {
        case _ => println("Veuillez fournir une entrée valide")
      }
    }

    println("Niveaux de stock mis à jour.\n")
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    var machines = ArrayBuffer[Machine]()

    try{
      println("Chargement des machines depuis "+filename)
      val source = Source.fromFile(filename)
      val itera = source.reset.getLines
      var ligne = itera.next
      var i = 1

      while (itera.nonEmpty){
        ligne = itera.next

        var machine_ite = ligne.split(",")
        machines += new Machine(i,machine_ite(0),machine_ite(1).toInt,machine_ite(2).toInt,machine_ite(3).toInt)
        println("\n\nMachine "+i+" chargée :\n")
        println("ID: "+i)
        println("Code PIN: "+ machine_ite(0))
        println("Lait: "+ machine_ite(1)+"mL")
        println("Sucre: "+ machine_ite(2)+"g")
        println("Café: "+machine_ite(3)+"g")
        i+=1

      }
      i-=1
      println("\n"+i+" machine(s) chargée(s) avec succès")
      source.close

    }
    catch{
      case _ => println("\nErreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
    }


    return machines
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    try{
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      var i=0
      for (elem <- machines){
        fw.println(elem.pincode+","+elem.milk+","+elem.sugar+","+elem.coffee)
        i+=1
      }
      println("Sauvegarde de "+i+" machines dans "+filename)
      fw.close()
    }
    catch{
      case _ => println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
    }
  }


  def main(args: Array[String]): Unit = {


    var machine_ON = false
    var machines=loadcsv("machines.csv")

    if (machines.nonEmpty) machine_ON = true
    else println("\n---\n\nErreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")



    while (machine_ON) {
      var nb_machines=machines.length
      var machineId = 0

      //Test valeur fausse
      var test_Err = 0
      while (machineId<1 || machineId > nb_machines) {
        if (test_Err!=0) println("Veuillez fournir une entrée valide")
        test_Err=1
        try {
          machineId = readLine("Machine sélectionnée (1-" + nb_machines + ") > ").toInt
        }
        catch {
          case _ => println("Veuillez fournir une entrée valide")
            test_Err = 0
        }

      }
      machineId-=1
      println("       Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choixmenu = readLine("> ").toInt

      //Test Valeur correcte
      while (choixmenu < 1 || choixmenu > 3) {
        println("Veuillez fournir une entrée valide")
        choixmenu = readLine("> ").toInt
      }

      //Quitter
      if (choixmenu == 3) {machine_ON = false
        savecsv("machines.csv",machines)}

      //Mode Admin
      else if (choixmenu == 2) {

        println("Mode Admin")
        var tentatives = 3
        while (tentatives>0){

          if (validatePin(machineId,machines)){

            println("Accès Autorisé\n\nStocks:")
            println("Poudre de café: " + machines(machineId).coffee + "g")
            println("Lait          : " + machines(machineId).milk + "mL")
            println("Sucre         : " + machines(machineId).sugar + "g")
            println("")
            var action = 1
            while (action == 1 || action==2) {
              println("Veuillez séléctionner l'action à effectuer:")
              println("1) Changement de code Pin")
              println("2) Réapprovisionnement des stocks")
              println("3) Quitter le mode admin")

              action = readLine("> ").toInt
              while (action < 1 || action > 3) action = readLine("Veuillez fournir une entrée valide\n> ").toInt
              println()
              if (action==1) updatePin(machineId,machines)
              else if (action==2) restockMachine(machineId,machines)
              else {println("Retour au menu principal...\n")
                action = 0
              }
            }
            tentatives = -1
          }

          else {tentatives -= 1
            println("Code PIN incorrect. "+ tentatives + " tentatives restantes")
          }
        }
        if (tentatives==0) {println("Trop de tentatives échouées. Fin du programme.")
        machine_ON=false}
      }

      else if (choixmenu == 1) {
        serveClient(machineId,machines)
      }
    }
  }
}