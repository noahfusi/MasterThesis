import io.StdIn._
import scala.util.Random
object Main {
  val nbMachine = 5
  var sortir = false

  val machinePins = Array.fill(nbMachine)("434343")
  val coffeeStocks = Array.fill(nbMachine)(50)
  val milkStocks = Array.fill(nbMachine)(500)
  val sugarStocks = Array.fill(nbMachine)(30)

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var PINsaisi = ""
    var tentative = 3
    while(tentative > 0 && PINsaisi != machinePins(machineId)) {
      println("Entrez le code PIN :")
      PINsaisi = readLine(">")
      tentative = tentative -1

      if(PINsaisi == machinePins(machineId)){
        return true
      }
      else{
        println("Code PIN incorrect. " +tentative+" tentatives restantes.")
      }
    }
    return false
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var PINsaisi = ""
    while(PINsaisi.length!=6 || !PINsaisi.forall(_.isDigit)){
      if(PINsaisi.length!=6 || !PINsaisi.forall(_.isDigit)){
        PINsaisi = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
    }
    machinePins(machineId-1) = PINsaisi
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
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
      if((consommepoudrecafe <= coffeeStocks(machineId-1)) && ((consommelait + extraLait) <= milkStocks(machineId-1)) && (consommesucre <= sugarStocks(machineId-1))) {
        //upgrade stock
        coffeeStocks(machineId-1) = coffeeStocks(machineId-1) - consommepoudrecafe
        sugarStocks(machineId-1) = sugarStocks(machineId-1) - consommesucre
        milkStocks(machineId-1) = milkStocks(machineId-1) - (consommelait + extraLait)


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
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    var ajoutStock = -1
    println("Niveaux de stock actuels : ")
    println("    Poudre de café: " + coffeeStocks(machineId-1) +"gr")
    println("    Sucre         : " + sugarStocks(machineId-1)+" gr")
    println("    Lait          : " + (milkStocks(machineId-1) * 0.001)+" L")

    println("Entrez les quantités à ajouter :")
    while(ajoutStock < 0){
      ajoutStock = readLine("Poudre de café >").toInt
    }
    coffeeStocks(machineId-1) += ajoutStock

    ajoutStock = -1
    while(ajoutStock < 0){
      ajoutStock = readLine("Sucre >").toInt
    }
    sugarStocks(machineId-1) += ajoutStock

    ajoutStock = -1
    while(ajoutStock < 0){
      ajoutStock = readLine("Lait >").toInt
    }
    milkStocks(machineId-1) += ajoutStock

    println("La nouvelle niveaux de stock: ")
    println("    Poudre de café: " + coffeeStocks(machineId-1) +"gr")
    println("    Sucre         : " + sugarStocks(machineId-1)+" gr")
    println("    Lait          : " + (milkStocks(machineId-1) * 0.001)+" L")

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  def main(args: Array[String]): Unit = {
    var machineId = 0

    while(!sortir){
      println("Nospresso café")
      while (machineId <= 0 || machineId > 5) {
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
        var result = serveClient(machineId,coffeeStocks,sugarStocks, milkStocks)
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

        if(validatePin(machineId-1, machinePins)){
          println("Accès accordé à la Machine " + machineId + ".")

          var choisir = 0
          while(choisir != 1 && choisir != 2){
            println("Que souhaitez-vous faire?")
            println("1) Remplir les stocks")
            println("2) Changer le code PIN")
            choisir = readLine(">").toInt
          }

          if(choisir == 1){
            restockMachine(machineId,coffeeStocks,sugarStocks,milkStocks)
            machineId = 0
          }
          else if(choisir == 2){
            updatePin(machineId, machinePins)
            machineId = 0
          }
        }
        else{
          println("Trop de tentatives échouées. Fin du programme.")
          sortir = true
        }
      }

      //Quitter Mode
      else if(typMode == 3) {
        println("Le programme est terminée.")
        sortir = true
      }
    }
  }
}