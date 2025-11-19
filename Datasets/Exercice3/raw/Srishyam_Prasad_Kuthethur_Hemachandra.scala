import scala.io.StdIn._
import scala.util.Random
import collection.mutable.ArrayBuffer
import java.io.{PrintWriter, File}
import scala.io.Source._

object Main {

  def validatePin(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {
    var essaie=3
    var machinePIN=machines(machineId-1).pincode
    var entrezPIN = readLine("Entrez le code PIN pour la Machine " + machineId + " :")
    while(essaie>0) {
      if (entrezPIN == machinePIN) {
        println("Accès accordé.")
        return true
      }
      else {
        essaie -= 1
        if(essaie==0){
          println("Code PIN incorrect. " +essaie+ " tentatives restante.")
        }
        else if(essaie>0){
          println("Code PIN incorrect. Vous avez " +essaie+ " tentatives")
          entrezPIN=readLine("> ")
        }
      }
    }
    false
  }


  def updatePin(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    println("\nMise à jour du code PIN pour la Machine " +machineId)
    var nouveauPIN=readLine("Entrez un nouveau code PIN à 6 chiffres: ")

    while (nouveauPIN.length != 6) {
      nouveauPIN = readLine("Entrez un nouveau code PIN à 6 chiffres: ")
    }

    var valide=true
    for(i <- nouveauPIN){
      if(i<'0'|| i>'9'){
        valide=false
        println("Erreur: Le caractère " +i+ " n'est pas valide")
      }
    }
    if (nouveauPIN.length == 6 && valide) {
      machines(machineId-1).pincode = nouveauPIN
      println("\nLe code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")
    }
    else {
      println("Le code PIN n'a pas été mis à jour.")
      println("Retour au menu principal...")
    }
  }


  def serveClient(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {

    //Stockage
    var mac=machines(machineId)

    //Erreur
    var ErreurPoudredeCafe = 0
    var ErreurLait = 0.0
    var ErreurSucre = 0

    //Stockage diminuer
    var PoudreDeCafeDiminue = mac.coffee
    var SucreDiminue = mac.sugar
    var LaitDiminue = mac.milk

    var select1 = 0
    while (select1 == 0) {

      println("\nVeuillez selectionner votre boisson:")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      var client = readLine("> ").toInt

      //Erreur de numero client:
      while (client != 1 && client != 2 && client != 3) {
        println("Erreur!! Numero entree n'est pas valide, \nVeuillez selectionner votre boisson:")
        client = readLine("> ").toInt
      }

      var prix = 0.0
      var Latte = 0
      //Gestion des stocks
      if (client == 1 && mac.coffee >= 8) {
        mac.coffee -= 8
        prix = 2.00
      }
      else if (client == 1 && mac.coffee < 8) { //erreur
        ErreurPoudredeCafe = 1
        mac.coffee = mac.coffee
      }

      if (client == 2 && mac.coffee >= 6 && mac.milk >= 100) {
        mac.coffee -= 6
        mac.milk -= 100
        prix = 2.50
      }
      else if (client == 2) { //erreur
        if (mac.coffee < 6) {
          ErreurPoudredeCafe = 1
          mac.coffee = mac.coffee
        }
        if (mac.milk < 100) {
          ErreurLait = 1
          mac.milk = mac.milk
        }
      }

      else {
        if (client == 3) {
          println("Quel taille vous voulez votre Latte ?")
          println("1) Latte (Petit)")
          println("2) Latte (Moyen)")
          println("3) Latte (Grand)")
          Latte = readLine("> ").toInt

          //Erreur de Latte
          while (Latte != 1 && Latte != 2 && Latte != 3) {
            println("Erreur!! Numero entree n'est pas valide, \nQuel taille vous voulez votre Latte:")
            Latte = readLine("> ").toInt
          }

          if (Latte == 1 && mac.coffee >= 6 && mac.milk >= 120) {
            mac.coffee -= 6
            mac.milk -= 120
            prix = 2.70
          }
          else if (Latte == 1) { //erreur
            if (mac.coffee < 6) {
              ErreurPoudredeCafe = 1
              mac.coffee = mac.coffee
            }
            else if (mac.milk < 120) {
              ErreurLait = 1
              mac.milk = mac.milk
            }
          }

          if (Latte == 2 && mac.coffee >= 8 && mac.milk >= 150) {
            mac.coffee -= 8
            mac.milk -= 150
            prix = 3.20
          }
          else if (Latte == 2) { //erreur
            if (mac.coffee < 8) {
              ErreurPoudredeCafe = 1
              mac.coffee = mac.coffee
            }
            else if (mac.milk < 150) {
              ErreurLait = 1
              mac.milk = mac.milk
            }
          }

          if (Latte == 3 && mac.coffee >= 12 && mac.milk >= 200) {
            mac.coffee -= 12
            mac.milk -= 200
            prix = 3.70
          }
          else if (Latte == 3) { //erreur
            if (mac.coffee < 12) {
              ErreurPoudredeCafe = 1
              mac.coffee = mac.coffee
            }
            else if (mac.milk < 200) {
              ErreurLait = 1
              mac.milk = mac.milk
            }
          }
        }
      }

      //sucre a ajouter
      var sucre2 = 0
      var prixsucre = 0.0
      if (client == 1 || client == 2 || client == 3) {
        println("\nSouhaitez-vous ajouter du sucre?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        sucre2 = readLine("> ").toInt
      }

      //Erreur de numero sucre:
      while (sucre2 != 1 && sucre2 != 2 && sucre2 != 3 && sucre2 != 4) {
        println("Erreur!! Numero entree n'est pas valide, \nSouhaitez-vous ajouter du sucre?")
        sucre2 = readLine("> ").toInt
      }


      if (sucre2 == 1) {
        mac.sugar = mac.sugar
        prixsucre = 0.0
      }
      if (sucre2 == 2 && mac.sugar >= 5) {
        mac.sugar -= 5
        prixsucre = 0.10
      }
      else if (sucre2 == 2 && mac.sugar < 5) { //erreur
        ErreurSucre = 1
        mac.sugar = mac.sugar
      }

      if (sucre2 == 3 && mac.sugar >= 10) {
        mac.sugar -= 10
        prixsucre = 0.20
      }
      else if (sucre2 == 3 && mac.sugar < 10) { //erreur
        ErreurSucre = 1
        mac.sugar = mac.sugar
      }

      if (sucre2 == 4 && mac.sugar >= 15) {
        mac.sugar -= 15
        prixsucre = 0.30
      }
      else if (sucre2 == 4 && mac.sugar < 15) { //erreur
        ErreurSucre = 1
        mac.sugar = mac.sugar
      }


      //lait supplementaire
      var lait = 0
      var prixlait = 0.05
      if (client == 2 || client == 3) {
        println("\nSouhaitez-vous ajouter du lait en supplement ?\n(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        lait = readLine("> ").toInt

        //Erreur de numero lait:
        while (lait != 1 && lait != 2) {
          println("Erreur!! Numero entree n'est pas valide, \nSouhaitez-vous ajouter du lait supplementaire?")
          lait = readLine("> ").toInt
        }

        if (lait == 1) {
          println("\nCombien de dose ? (0.05CHF par dose)")
          var dose = readLine("> ").toInt

          //Erreur de numero dose:
          while (dose != 1 && dose != 2 && dose != 3) {
            println("Erreur!! Numero entree n'est pas valide, \nCombien de dose?")
            dose = readLine("> ").toInt
          }

          if (dose == 1 && mac.milk >= 50) {
            mac.milk -= 50
            prixlait = prixlait * 1
          }
          else if (dose == 1 && mac.milk < 50) { //erreur
            ErreurLait = 1
            mac.milk = mac.milk
          }

          if (dose == 2 && mac.milk >= 100) {
            mac.milk -= (50 * 2)
            prixlait = prixlait * 2
          }
          else if (dose == 2 && mac.milk < 100) { //erreur
            ErreurLait = 1
            mac.milk = mac.milk
          }

          if (dose == 3 && mac.milk >= 150) {
            mac.milk -= (50 * 3)
            prixlait = prixlait * 3.0
          }
          else if (dose == 3 && mac.milk < 150) { //erreur
            ErreurLait = 1
            mac.milk = mac.milk
          }
        }
        else {
          prixlait = 0.0
        }
      }
      else if (client == 1) {
        prixlait = 0.0
      }


      //Commande de Boisson Reussie
      if (client == 1) {
        println("\nBoisson Selectionnee : Expresso")
      }
      else if (client == 2) {
        println("\nBoisson Selectionnee : Cappuccino")
      }
      else {
        if (client == 3 && Latte == 1) {
          println("\nBoisson Selectionnee : Latte (Petit)")
        }
        else if (client == 3 && Latte == 2) {
          println("\nBoisson Selectionnee : Latte (Moyen)")
        }
        else {
          println("\nBoisson Selectionnee : Latte (Grand)")
        }
      }

      if (sucre2 == 1) {
        println("Niveau de sucre : Sans sucre")
      }
      else if (sucre2 == 2) {
        println("Niveau de sucre : Peu (5g)")
      }
      else if (sucre2 == 3) {
        println("Niveau de sucre : Moyen (10g)")
      }
      else {
        println("Niveau de sucre : Beaucoup (15g)")
      }

      if (lait == 1) {
        println("Lait en supplement: OUI")
      }
      else {
        println("Lait en supplement: NON")
      }


      //Erreur
      if (ErreurPoudredeCafe == 1 ) {
        println("\nErreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnee. \nVeuillez choisir une autre boisson ou verifier les stocks en mode Admin.")
      }
      if (ErreurLait == 1) {
        println("\nErreur : Quantite de lait insuffisante pour preparer la boisson selectionnee. \nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      }
      if (ErreurSucre == 1 ) {
        println("\nErreur : Quantite de sucre insuffisante pour preparer la boisson selectionnee. \nVerifier les stocks en mode Admin.")
      }
      var PoudreDeCafeM = PoudreDeCafeDiminue - mac.coffee
      var SucreM = SucreDiminue - mac.sugar
      var LaitM = LaitDiminue - mac.milk


      if (ErreurPoudredeCafe == 1 || ErreurSucre == 1 || ErreurLait == 1 ) {
        mac.coffee += PoudreDeCafeM
        mac.sugar += SucreM
        mac.milk += LaitM

        ErreurPoudredeCafe = 0
        ErreurSucre = 0
        ErreurLait = 0
      }


      else if (ErreurPoudredeCafe != 1  && ErreurLait != 1  && ErreurSucre != 1) {

        var prixTotal = (prix + prixsucre + prixlait)
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix, prixsucre, prixlait, prixTotal)

        //Paiement
        println("\nVeuillez payer en utilisant Twint.")

        def RandomTwintCode(): String = {
          Random.alphanumeric.take(5).mkString
        }

        var TwintCode = RandomTwintCode()
        println("Votre code de paiement est : " + TwintCode + " \n(En attente de validation du paiement...)")
        Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)

        var paiement= true
        if(paiement) {
          println("\nMerci ! Votre paiement a ete accepte.")
        }
        else{
          println("\nPaiement échoué")
          mac.coffee=PoudreDeCafeDiminue
          mac.sugar= SucreDiminue
          mac.milk= LaitDiminue
          return false
        }


        //Preparation de la Boisson
        //apres l'affiche de paiement
        println("Preparation de votre boisson...")
        if (client == 1) {
          println("Votre Expresso est pret ! Bonne degustation !")
        }
        else if (client == 2) {
          println("Votre Cappuccino est pret ! Bonne degustation !")
        }
        else {
          println("Votre Latte est pret ! Bonne degustation !")
        }

        //Après avoir finir
       machines(machineId)=mac
        select1 = 1
      }
    }
    true
  }


  def restockMachine(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    var mach=machines(machineId-1)
    println("\nEntrez les quantités à ajouter :")
    var PoudreDeCafeAjout = readLine("Poudre de Cafe > ").toInt
    var LaitAjout = readLine("Lait > ").toDouble
    var LaitAjout1= (LaitAjout*1000.0).toInt
    var SucreAjout = readLine("Sucre > ").toInt

    mach.coffee += PoudreDeCafeAjout
    mach.milk +=  LaitAjout1
    mach.sugar+=  SucreAjout


    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  class Machine(val id:Int, var pincode:String, var milk:Int, var sugar:Int, var coffee:Int) {
    def addIngredient(ingredient:String, amount:Int):Unit={
      if(ingredient=="1") {
        milk += amount
        println("Lait ajouté. Nouveau stock de lait: " + milk/1000.0 + " L")
      }
      else if(ingredient=="2") {
        sugar += amount
        println("Sucre ajouté. Nouveau stock de sucre: " + sugar + " g")
      }
      else if(ingredient=="3") {
        coffee += amount
        println("Café ajouté. Nouveau stock de café: " + coffee + " g")
      }
      else{
        println("\nChoix invalide. Aucun ingrédient ajouté")
        println("Stock initial: Lait: " +milk/1000.0+ "L, Sucre: " +sugar+ "g, Café: " +coffee+ "g")
      }
      if(ingredient=="1" || ingredient=="2" || ingredient=="3") {
        println("Nouveau stock: Lait: " + milk/1000.0 + "L, Sucre: " + sugar + "g, Café: " + coffee + "g")
      }
    }
    def removeIngredient(ingredient:String, amount:Int):Boolean= {
      if (ingredient == "1") {
        if (milk >= amount) {
          milk -= amount
          true
        }
        else {
          println("Stock de lait insuffisant.")
          false
        }
      }

      else if (ingredient == "2") {
        if (sugar >= amount) {
          sugar -= amount
          true
        }
        else {
          println("Stock de sucre insuffisant.")
          false
        }
      }

      else if (ingredient == "3") {
        if (coffee >= amount) {
          coffee -= amount
          true
        }
        else {
          println("Stock de café insuffisant.")
          false
        }
      }
      else {
        println("Choix invalide. Aucun ingrédient retiré")
        println("Stock initial: Lait: " + milk / 1000.0 + "L, Sucre: " + sugar + "g, Café: " + coffee + "g")
        false
      }

      if (ingredient == "1" && milk >= amount || ingredient == "2" && sugar >= amount|| ingredient == "3" && coffee >= amount) {
        println("Nouveau stock: Lait: " + milk/1000.0 + "L, Sucre: " + sugar + "g, Café: " + coffee + "g")
        true
      }
      else{
        println("Stock initial: Lait: " + milk / 1000.0 + "L, Sucre: " + sugar + "g, Café: " + coffee + "g")
        false
      }
    }
  }

  def loadCSV(filename:String):ArrayBuffer[Machine]={
    val classe = new ArrayBuffer[Machine]()
    try {
      val fr = fromFile(filename)
      val lines=fr.getLines().drop(1)
      var i = 1
      for (l<- lines) {
        var col = l.split(",")
        if(col.length==4) {
          var pincode = col(0)
          var milk = col(1).toInt
          var sugar = col(2).toInt
          var coffee = col(3).toInt
          classe += new Machine(id = i , pincode, milk, sugar, coffee)
          i += 1
        }
        else{
          println("Ligne mal formatée: " +l)
        }
      }
      fr.close()
    }
    catch{
      case ex:java.io.FileNotFoundException=>println(" Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return ArrayBuffer()
    }
    return classe
  }

  def saveCSV(filename:String, machines:ArrayBuffer[Machine]):Unit={
    val fw = new PrintWriter(new File(filename))
    fw.println("PINCODE,MILK,SUGAR,COFFEE")
    //Ecrire les donnees de chaque machine
    for (m <- machines) {
      fw.println(m.pincode, + m.milk, + m.sugar, +m.coffee)
    }
    fw.close()
    println("\nSauvegardées dans " +filename+ "...")
  }


  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines= loadCSV(filename)
    var selectionner = 0
    var continuer =true
    while(continuer){
      if(machines.isEmpty){
        return
      }
      println("1) Afficher les machines")
      println("2) Ajouter un ingrédient")
      println("3) Retirer un ingrédient")
      println("4) Quitter")
      var choisi= readLine("Choisissez une option (1-4): ").toInt
      while(choisi<1 || choisi>4){
        choisi= readLine("Le numéro écrit n'est pas valide, veuillez réécrire: ").toInt
      }

      if(choisi==1){
        var id = readLine("Entrez l'ID de la machine:").toInt
        var trouvee:Machine=null

        while (!(id>=1 && id<=machines.length)){
          id=readLine("Veuillez réécrire, le numéro n'existe pas: ").toInt
        }

        for (m <- machines) {
          if (m.id == id) {
            trouvee = m
          }
        }
        if(trouvee==null){
          println("Aucune machine trouvée avec l'ID " +id)
        }
        else{
          println("Chargement des machines depuis machines.csv...")
          println("\nMachine " +id+ " chargée:")
          println("ID: " +trouvee.id)
          println("Code PIN: " +trouvee.pincode)
          println("Lait: " +trouvee.milk/1000.0+ "L")
          println("Sucre: " +trouvee.sugar+ "g")
          println("Café: " +trouvee.coffee+ "g")
          println("\n1 machine(s) chargée(s) avec succès.")
        }
      }

      else if(choisi==2) {
        var id = readLine("Entrez l'ID de la machine:").toInt
        while (!(id>=1 && id<=machines.length)){
          id=readLine("Veuillez réécrire, le numéro n'existe pas: ").toInt
        }
        var machine1=machines(id-1)
        println("Quel ingrédient voulez vous rajouter ?")
        println("1) Milk")
        println("2) Sugar")
        println("3) Coffee")
        var ajout= readLine("> ").toInt
        var amount=0
        try{
          while(ajout<1|| ajout>3){
            ajout=readLine("le nombre écrit n'est pas valide, veuillez réessayer: ").toInt
          }
          if(ajout==1){
            var amount1=readLine("Entrez la quantité de lait à ajouter: ").toDouble
            amount=(amount1*1000.0).toInt
          }
          else if(ajout==2){
            amount=readLine("Entrez la quantité de sucre à ajouter: ").toInt
          }
          else if(ajout==3){
            amount=readLine("Entrez la quantité de café à ajouter: ").toInt
          }
          var IngredientNom=Array("1","2","3")
          var ing=IngredientNom(ajout-1)
          machine1.addIngredient(ing,amount)
        }
        catch{
          case ex: Exception => println("Erreur : Echec de l’écriture dans machines.csv.")
            println("Le fichier peut être verrouillé ou en lecture seule.")
            println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
            return
        }
        saveCSV(filename, machines)
      }

      else if(choisi==3){
        var id = readLine("Entrez l'ID de la machine:").toInt
        while (!(id>=1 && id<=machines.length)){
          id=readLine("Veuillez réécrire, le numéro n'existe pas: ").toInt
        }
        var machine2=machines(id-1)
        println("Quel ingrédient voulez vous retirer ?")
        println("1) Milk")
        println("2) Sugar")
        println("3) Coffee")
        var retirer= readLine("> ").toInt
        var amount=0

        try{
          while(retirer<1|| retirer>3){
            retirer=readLine("le nombre écrit n'est pas valide, veuillez réessayer: ").toInt
          }
          if(retirer==1){
            var amount1=readLine("Entrez la quantité de lait à retirer: ").toDouble
            amount=(amount1*1000.0).toInt
          }
          else if(retirer==2){
            amount=readLine("Entrez la quantité de sucre à retirer: ").toInt
          }
          else if(retirer==3){
            amount=readLine("Entrez la quantité de café à retirer: ").toInt
          }

          var IngredientNom=Array("1","2","3")
          var ing=IngredientNom(retirer-1)
          machine2.removeIngredient(ing,amount)
        }
        catch{
          case ex: Exception => println("Erreur : Echec de l’écriture dans machines.csv.")
            println("Le fichier peut être verrouillé ou en lecture seule.")
        }
        saveCSV(filename, machines)
      }

      else if(choisi ==4){
        saveCSV(filename, machines)
        continuer = false
      }
    }

    //Erreur
    var ErreurPoudredeCafe = 0
    var ErreurLait = 0.0
    var ErreurSucre = 0

    while (selectionner != 3) {
      println("\nNospresso Cafe")
      println("Veuillez selectionner votre mode:")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      selectionner = readLine("> ").toInt

      var machineId = 0
      var success=true
      if (selectionner == 1 ) {
        while (machineId < 1 || machineId > 5) {
          println("\nSélectionnez une machine (1-5) :")
          machineId = readLine("> ").toInt
          if (machineId < 1 || machineId > 5) {
            println("Numéro de machine invalide. Veuillez réessayer.")
          }
        }
        var index = machineId-1

        success = serveClient(index,machines)
        saveCSV(filename,machines)
      }


      if (selectionner == 2) {
        println("Mode Admin")
        var continue = true
        if (continue) {
          println("1. Est-ce que vous voulez changer le mot de passe? ")
          println("2. Est-ce que vous voulez voir le stock ?")
          println("3. Vous voulez Quitter ?")
          var Propose = readLine(">").toInt

          while (Propose != 1 && Propose != 2 && Propose != 3) {
            Propose = readLine("\nLe numéro écrit n'est pas juste, veulez réécrire :").toInt
          }

          var machineId=0
          if (Propose == 1) {
            while (machineId < 1 || machineId > 5) {
              println("\nSélectionnez une machine (1-5) :")
              machineId = readLine("> ").toInt
              if (machineId < 1 || machineId > 5) {
                println("Numéro de machine invalide. Veuillez réessayer.")
              }
            }

            // Validate the PIN
            if (validatePin(machineId, machines)) {
              updatePin(machineId, machines)
            }
            else {
              println("\nTrop de tentatives échouées. Fin du programme")
              return
            }
            saveCSV(filename,machines)
          }


          else if (Propose == 2) {
            var machineId=0
            while (machineId < 1 || machineId > 5) {
              println("Sélectionnez une machine (1-5) :")
              machineId = readLine("> ").toInt
              if (machineId < 1 || machineId > 5) {
                println("Numéro de machine invalide. Veuillez réessayer.")
              }
            }

            if(validatePin(machineId, machines)) {
              var litres=machines(machineId-1).milk/1000.0
              //Stockage apres l'utilisation
              println("\nNiveaux de Stocks actuels :")
              println("Poudre de Cafe : " + machines(machineId-1).coffee + "g")
              println("Lait : " +litres+ "L")
              println("Sucre : " + machines(machineId-1).sugar + "g")

              //erreur
              ErreurPoudredeCafe = 0
              ErreurLait = 0
              ErreurSucre = 0

              restockMachine(machineId, machines)
              saveCSV(filename,machines)
            }

            else{
              println("\nTrop de tentatives échouées. Fin du programme")
              return
            }
          }

          else if (Propose == 3) {
            println("Programme terminé. Merci de votre utilisation !")
            continue = false
          }
        }
      }
    }


    if (selectionner == 3) {
      println(" Sauvegarde des machines dans machines.csv...\n Fichier sauvegardé avec succès.")
    }
  }
}
