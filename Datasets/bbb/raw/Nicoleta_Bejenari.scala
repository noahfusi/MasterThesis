import scala.io.StdIn.readLine
import scala.util.Random

object Main {
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {

    var nbTentatives = 3

    while(nbTentatives>0){

      println("Entrez le code PIN : ")
      var pinEntre = readLine(">")


      if(pinEntre == machinePins(machineId)){
        println("Accès accordé")
        return true
      }
      else{
        nbTentatives-=1;
        println("Code PIN incorrect. "+nbTentatives+" tentative restante(s)")
      }

    }

    println("Trop de tentatives échouées. Fin du programme.")
    return false

  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

      println("Mise à jour du code PIN pour la Machine "+(machineId+1)+".")

      var pinValid = false

      do{
        val nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres >")
        if(nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)){

          machinePins(machineId) = nouveauPin

          println("Le code PIN à été mis à jour avec succès.")
          println("Retour au menu principal...")

          pinValid = true
        }
      }while(pinValid == false)
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

      var choixBoisson = 0
      var choixTailleLatte = 0
      var choixSucre = 0
      var choixSupplementLait = 0
      var nbDosesLait = 0

      do {
        println("")

        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choixBoisson = readLine(">").toInt
      } while (choixBoisson < 1 || choixBoisson > 3)

      if (choixBoisson == 3) {

        do {
          println("Veuillez choisir la taille du latte :")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          choixTailleLatte = readLine(">").toInt
        } while (choixTailleLatte < 1 || choixTailleLatte > 3)

      }

      do {
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        choixSucre = readLine(">").toInt
      } while (choixSucre < 1 || choixSucre > 4)


      if (choixBoisson == 2 || choixBoisson == 3) {

        do {
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          choixSupplementLait = readLine(">").toInt
        } while (choixSupplementLait < 1 || choixSupplementLait > 2)

        if (choixSupplementLait == 1) {
          do {
            println("Combien de dose ?")
            nbDosesLait = readLine(">").toInt
          } while (nbDosesLait < 1 || nbDosesLait > 3)
        }

      }

      var prixBoisson = 0.0
      var prixSucre = 0.0
      var prixSupplementLait = 0.0
      var prixTotal = 0.0

      var qteCafeNecessaire = 0
      var qteSucreNecessaire = 0
      var qteLaitNecessaire = 0

      var nomBoisson = ""
      var tailleBoisson = ""

      if (choixBoisson == 1) {
        nomBoisson = "Expresso"
        prixBoisson = 2
        qteCafeNecessaire = 8
      } else if (choixBoisson == 2) {
        nomBoisson = "Cappuccino"
        prixBoisson = 2.5
        qteCafeNecessaire = 6
        qteLaitNecessaire = 100
      } else {

        nomBoisson = "Latte"

        if (choixTailleLatte == 1) {
          prixBoisson = 2.7
          tailleBoisson = " (Petit)"
          qteCafeNecessaire = 6
          qteLaitNecessaire = 120
        } else if (choixTailleLatte == 2) {
          prixBoisson = 3.2
          tailleBoisson = " (Moyen)"
          qteCafeNecessaire = 8
          qteLaitNecessaire = 150
        } else {
          prixBoisson = 3.7
          tailleBoisson = " (Grand)"
          qteCafeNecessaire = 12
          qteLaitNecessaire = 200
        }
      }

      var quantiteSucre = ""

      if (choixSucre == 1) {
        quantiteSucre = "Sans sucre"
      } else if (choixSucre == 2) {
        quantiteSucre = "Peu (5g)"
        prixSucre = 0.1
        qteSucreNecessaire += 5
      } else if (choixSucre == 3) {
        quantiteSucre = "Moyen (10g)"
        prixSucre = 0.2
        qteSucreNecessaire += 10
      } else {
        quantiteSucre = "Beaucoup (15g)"
        prixSucre = 0.3
        qteSucreNecessaire += 15
      }

      var avecSupplementLait = ""

      if (choixSupplementLait == 2) {
        avecSupplementLait = "Non"
      } else if (choixSupplementLait == 1) {
        avecSupplementLait = nbDosesLait.toString + " dose(s)"
        prixSupplementLait = nbDosesLait * 0.05
        qteLaitNecessaire += nbDosesLait * 50
      }


      //affichage boisson selectionnée et le niveau de sucre
      println("Boisson sélectionnée : " + nomBoisson + tailleBoisson)
      println("Niveau de sucre : " + quantiteSucre)

      //affichage supplément lait si la boisson n'est pas expresso
      if (choixBoisson != 1) {
        println("Lait supplémentaire : " + avecSupplementLait)
      }

      println("")

      var ingredientInsuffisant = ""

      if (qteCafeNecessaire > coffeeStocks(machineId)) {
        ingredientInsuffisant = "poudre de café"
      } else if (qteSucreNecessaire > sugarStocks(machineId)) {
        ingredientInsuffisant = "sucre"
      } else if (qteLaitNecessaire > milkStocks(machineId)) {
        ingredientInsuffisant = "lait"
      }

      if(ingredientInsuffisant != ""){
        println("Erreur : Quantité de "+ingredientInsuffisant+" insuffisante pour pràparer la boisson sàlectionnée.")
        println("Veuillez choisir une autre machine")
      }
      else{

        //réduire les stocks avec la quantité nécessaire
        coffeeStocks(machineId) -= qteCafeNecessaire
        milkStocks(machineId) -= qteLaitNecessaire
        sugarStocks(machineId) -= qteSucreNecessaire

        prixTotal = prixBoisson + prixSucre + prixSupplementLait

        //affichage message prix total
        printf("Prix total : CHF %.2f", prixBoisson)

        //affiche le prix du sucre si ajouté
        if (prixSucre > 0)
          printf(" + CHF %.2f", prixSucre)

        //affiche le prix du supplément lait si ajouté
        if (prixSupplementLait > 0)
          printf(" + CHF %.2f", prixSupplementLait)

        //affiche le total si plusieurs prix ont été ajouté
        if (prixTotal != prixBoisson)
          printf(" = CHF %.2f", prixTotal)

        print("\n")
        println("")

        //générer code Twint
        var codeTwint = ""
        val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

        for (i <- 1 to 5) {
          codeTwint += chars(Random.nextInt(chars.length))
        }

        //affichage messages payment
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + codeTwint)
        println("(En attente de validation du paiement...)")
        println("")
        Thread.sleep(3000)
        println("Paiement confirmé.")

        //affichage messages préparation
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre " + nomBoisson + " est prêt ! Bonne dégustation !")

        return true
      }

      return false

  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    println("Niveaux de stock actuels : ")
    println(" Poudre de café: : "+coffeeStocks(machineId)+"g")
    println(" Sucre : "+sugarStocks(machineId)+"g")
    println(" Lait : "+ milkStocks(machineId).toDouble/1000 +"L")

    println("Entrez les quantités à ajouter ")
    var qteReapCafe = readLine("Poudre de café > ").toInt
    var qteReapSucre = readLine("Sucre > ").toInt
    var qteReapLaitLitres = readLine("Lait > ").toDouble

    var qteReapLaitMLitres = (qteReapLaitLitres*1000).toInt

    coffeeStocks(machineId) += qteReapCafe
    milkStocks(machineId) += qteReapLaitMLitres
    sugarStocks(machineId) += qteReapSucre

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def main(args: Array[String]): Unit = {

    val nbMachines = 5
    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)
    val machinePins = Array.fill(nbMachines)("434343")

    var mode = 0
    var machineId = 0
    var quitterProgramme = false

    do{

      do{
        println("")
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
      }while(mode<1 || mode>3)

      if(mode == 3){
        quitterProgramme = true
      }

      if(mode != 3){
        do{
          machineId = readLine("Machine sélectionnée (1-5) >").toInt -1
        }while(machineId<0 || machineId>nbMachines-1)
      }

      if(mode == 1){
        serveClient(machineId,coffeeStocks, sugarStocks, milkStocks)
      }

      if(mode == 2) {

        var actionAdmin = 0
        if(validatePin(machineId,machinePins)){
          do{
            println("")
            println("Mode Admin")
            println("Veuillez sélectionner votre action :")
            println("1) Changer Pin")
            println("2) Réapprovisionnement machine")
            actionAdmin = readLine(">").toInt
          }while(actionAdmin<1 || actionAdmin>2)

          if(actionAdmin == 1){
            updatePin(machineId, machinePins)
          }else if(actionAdmin == 2) {
            restockMachine(machineId,coffeeStocks,sugarStocks,milkStocks)
          }
        }else{
          quitterProgramme = true
        }
      }


    }while(quitterProgramme == false)
  }
}