import collection.mutable.ArrayBuffer
import scala.io.StdIn._
import java.io.{FileWriter, PrintWriter}
import scala.io.Source._
import scala.util.Random

object Main {
  //Classe de machines
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") {
        milk = milk + amount
      }
      else if (ingredient == "sugar") {
        sugar = sugar + amount
      }
      else if (ingredient == "coffee") {
        coffee = coffee + amount
      }
    }
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "coffee") {
        coffee = coffee - amount
        return true
      }
      else if (ingredient == "sugar") {
        sugar = sugar - amount
        return true
      }
      else if (ingredient == "milk") {
        milk = milk - amount
        return true
      }
      else {
        return false
      }
    }

    def imprimerCSV(): String = {
      return pincode + "," + milk + "," + sugar + "," + coffee
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val machines = new ArrayBuffer[Machine]()
      val fileWriter = fromFile(filename)
      val ligneFile = fileWriter.reset.getLines
      var ligne = ligneFile.next
      var i = 0
      while (!ligneFile.isEmpty) {
        ligne = ligneFile.next
        var machine = ligne.split(",")
        machines += new Machine(i + 1, machine(0), machine(1).toInt, machine(2).toInt, machine(3).toInt)
        i += 1
      }
      return machines
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return null
      case ex: Exception => println("Erreur : Une erreur s'est produite")
        return null
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val printWriter = new PrintWriter(filename)
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for (i <- 0 until machines.length) {
        printWriter.println(machines(i).imprimerCSV())
      }
      printWriter.close()
      println("Fichier sauvegardé avec succès.")
    }
    catch {
      case ex: Exception => println("Erreur : Le fichier peut être verrouillé ou en lecture seule.")
    }
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    //stock consommation
    var consommepoudrecafe = 0
    var consommesucre = 0
    var consommelait = 0

    var typMode = 0
    var typeBoisson = 0
    var typeTaille = 0
    var typeLait = 0

    var extraSucre = 0
    var extraLait = 0
    var doseLait = 0

    var prixBase = 0.0

    while (typeBoisson != 1 && typeBoisson != 2 && typeBoisson != 3){
      println("Veuillez sélectionner votre boisson :")
      println("1)expresso")
      println("2)cappuccino")
      println("3)latte")
      typeBoisson = readLine(">").toInt

      //Expresso
      if(typeBoisson == 1){
        consommepoudrecafe = 8
        prixBase = 2.00
      }
      //Cappuccino
      else if(typeBoisson == 2){
        consommepoudrecafe = 6
        consommelait = 100
        prixBase = 2.50
      }
      //Latte
      else {
        while (typeTaille != 1 && typeTaille != 2 && typeTaille != 3) {
          println("Veuillez sélectionner votre taille de boisson :")
          println("1)petit")
          println("2)moyen")
          println("3)grand")
          typeTaille = readLine(">").toInt
          //petit
          if (typeTaille == 1) {
            consommepoudrecafe = 6
            consommelait = 120
            prixBase = 2.70
          }
          //Moyen
          else if(typeTaille == 2){
            consommepoudrecafe = 8
            consommelait = 150
            prixBase = 3.20
          }
          //Grand
          else{
            consommepoudrecafe = 12
            consommelait = 200
            prixBase = 3.70
          }
        }
      }

      //Supplement de sucre
      while (extraSucre != 1 && extraSucre != 2 && extraSucre != 3 &&  extraSucre != 4) {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
        extraSucre = readLine(">").toInt

        if(extraSucre == 1){
          consommesucre = 0
        }
        else if(extraSucre == 2){
          consommesucre = 5
        }
        else if(extraSucre == 3){
          consommesucre = 10
        }
        else if(extraSucre == 4){
          consommesucre = 15
        }
      }

      //Supplement de lait
      while(typeLait != 1 && typeLait != 2) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("Quantité de poudre de café insuffisante pour\npréparer la boisson s´electionnée.")
        println("Veuillez choisir une taille plus petite ou essayer\nune autre boisson.")
        println("(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
        typeLait = readLine(">").toInt

        if(typeLait == 1){
          while(doseLait < 1 ||  doseLait  > 3){
            println("Combien de dose ?")
            doseLait = readLine(">").toInt
            extraLait = doseLait * 50
          }
        }
        else{
          doseLait = 0
        }
      }

      //Verifier le stock pour preparation le boisson
      if((consommepoudrecafe <= machines(machineId-1).coffee)
        && ((consommelait + extraLait) <= machines(machineId-1).milk)
        && (consommesucre <= machines(machineId-1).sugar)) {

        //upgrade stock
        var isValideStock = true
        isValideStock = machines(machineId-1).removeIngredient("coffee", consommepoudrecafe)
        isValideStock = machines(machineId-1).removeIngredient("sugar", consommesucre)
        isValideStock = machines(machineId-1).removeIngredient("milk", (consommelait + extraLait))


        if(isValideStock){
          //paiment
          println("Le prix total : CHF " + (prixBase + ((extraSucre - 1) * 0.10) + (doseLait * 0.05)))
          //twint
          println("Veuillez payer en utilisant Twint")
          println("code twint : " + Random.alphanumeric.take(5).mkString)
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.")

          typMode = 0
          typeTaille = 0
          typeLait = 0
          extraSucre = 0
          return true
        }

      }
      else{
        typeBoisson = 0
        typeTaille = 0
        typeLait = 0
        extraSucre = 0
        return false
      }
    }
    return false
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var PINsaisi = ""
    var tentative = 3
    while(tentative > 0 && PINsaisi != machines(machineId-1).pincode) {
      println("Entrez le code PIN :")
      PINsaisi = readLine(">")
      tentative = tentative -1

      if(PINsaisi == machines(machineId-1).pincode){
        return true
      }
      else{
        println("Code PIN incorrect. " +tentative+" tentatives restantes.")
      }
    }
    return false
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var ajoutStock = -1
    println("Niveaux de stock actuels : ")
    println("    Poudre de café: " + machines(machineId-1).coffee +"gr")
    println("    Sucre         : " + machines(machineId-1).sugar+" gr")
    println("    Lait          : " + (machines(machineId-1).milk * 0.001)+" L")

    println("Entrez les quantités à ajouter :")
    while(ajoutStock < 0){
      ajoutStock = readLine("Poudre de café (g)>").toInt
    }
    machines(machineId-1).addIngredient("coffee", ajoutStock)

    ajoutStock = -1
    while(ajoutStock < 0){
      ajoutStock = readLine("Sucre (g)>").toInt
    }
    machines(machineId-1).addIngredient("sugar", ajoutStock)

    ajoutStock = -1
    while(ajoutStock < 0){
      ajoutStock = readLine("Lait (ml)>").toInt
    }
    machines(machineId-1).addIngredient("milk", ajoutStock)

    println("La nouvelle niveaux de stock: ")
    println("    Poudre de café: " + machines(machineId-1).coffee +"gr")
    println("    Sucre         : " + machines(machineId-1).sugar+" gr")
    println("    Lait          : " + (machines(machineId-1).milk * 0.001)+" L")

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var PINsaisi = ""
    while(PINsaisi.length!=6 || !PINsaisi.forall(_.isDigit)){
      if(PINsaisi.length!=6 || !PINsaisi.forall(_.isDigit)){
        PINsaisi = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
    }
    machines(machineId-1).pincode = PINsaisi
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  def main(args: Array[String]): Unit = {
    var machineId = 0
    var sortir = false

    println("Chargement des machines depuis machines.csv...")
    val machines: ArrayBuffer[Machine] = loadcsv("machines.csv")
    if (machines != null) {
      for (i <- 0 to machines.length - 1) {
        println("Machine " + (i + 1) + " chargée :")
        println("    ID: " + machines(i).id)
        println("    Code PIN: " + machines(i).pincode)
        printf("    Lait: %.3fL", machines(i).milk * 0.001)
        println("\n    Sucre: " + machines(i).sugar + "g")
        println("    Café: " + machines(i).coffee + "g")
      }
      println(machines.length + " machine(s) chargée(s) avec succès.")

      while(!sortir){
        println("Nospresso café")
        while (machineId <= 0 || machineId > machines.length) {
          machineId = readLine("Machine sélectionnée(1-5) > ").toInt
        }

        var typMode = 0
        while (typMode != 1 && typMode != 2 && typMode != 3) {
          println("Veuillez sélectionner votre mode :")
          println("1)Client")
          println("2)Admin")
          println("3)Quitter")
          typMode = readLine(">").toInt
        }

        //Client Mode
        if(typMode == 1) {
          var result = serveClient(machineId,machines)
          if(result){
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
            machineId = 0
          }
          else{
            println("Les stock n'est pas suffisante")
            machineId = 0
          }
        }

        //Admin Mode
        else if(typMode == 2) {
          println("Mode Admin")

          if(validatePin(machineId, machines)){
            println("Accès accordé à la Machine " + machineId + ".")

            var choisir = 0
            while(choisir != 1 && choisir != 2){
              println("Que souhaitez-vous faire?")
              println("1) Remplir les stocks")
              println("2) Changer le code PIN")
              choisir = readLine(">").toInt
            }

            if(choisir == 1){
              restockMachine(machineId,machines)
              machineId = 0
            }
            else if(choisir == 2){
              updatePin(machineId, machines)
              machineId = 0
            }
          }
          else{
            println("Trop de tentatives échouées. Fin du programme.")
            println("Sauvegarde de " + machines.length + " machines dans machines.csv...")
            savecsv("machines.csv", machines)
            sortir = true
          }
        }

        //Quitter Mode
        else if(typMode == 3) {
          println("Sauvegarde de " + machines.length + " machines dans machines.csv...")
          savecsv("machines.csv", machines)
          println("Le programme est terminée.")
          sortir = true
        }
      }
    }
    else {
      sortir = false
      println("Échec du chargement des machines. Programme en cours de fermeture.")
    }
  }
}

