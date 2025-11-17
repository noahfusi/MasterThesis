import scala.io.StdIn.{readChar, readLine}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //////////////////////////générales////////////////////////////
    val nbMachines=5
    val coffeeStocks=Array.tabulate(nbMachines)(i=>50) //café g
    val sugarStocks=Array.tabulate(nbMachines)(i=>30)  //sucre g
    val milkStocks=Array.tabulate(nbMachines)(i=>500)  //lait mL
    val machinePins= Array.tabulate(nbMachines)(i=>"434343")
    /////////////////////////////////////////////////////////////
    def NospressoMachine: Unit= {
      //////////////////////commande en cours///////////
      var machineId = 0
      var coffeeNeeded = 0
      var sugarNeeded = 0
      var milkNeeded = 0
      var com1 = ""
      var com2 = ""
      var com3 = ""
      var ad1 = 0
      var ad2 = 0
      var ad3 = 0
      var isLatte = false
      var isEspresso = false
      var choix = ""
      //////////////////////////////////fonctions///////////////////////////////////////////////////////////////////////
      def read(min: Int, max: Int): String = {
        var choix = ""
        do {
          choix = readLine(">")
          if(choix.length!=1) choix=""
        } while (choix < min.toString || max.toString < choix)
        return choix
      }
      def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
        var CodeIn = ""
        var isValid = false
        var count = 3
        println("Entrez votre code PIN:")
        do {
          CodeIn = readLine(">")
          if (machinePins(machineId) == CodeIn) {
            println("Accès autorisé à la machine "+{machineId+1}+"\n")
            isValid = true
            count = 0
          } else {
            count -= 1
            printf("Code PIN incorrect, %d essais restants.\n", count)
          }
        } while (count > 0)
        if (!isValid) println("Trop de tentatives échouées. Fin du programme")
        choix = "end"
        return isValid
      }
      def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
        var input = ""
        var isValid = true
        var max=6
        println("Mise à jour du code PIN pour la machine "+ {machineId + 1})
        do {
          isValid=true
          input = readLine(s"Entrez votre nouveau Code PIN à $max chiffres > ")
          if (input.length == max) {
            for (i <- 0 until max) {
              if (input(i) < '0' || input(i) > '9') isValid = false
            }
          }
          else isValid = false
          if (isValid) {
            machinePins(machineId) = input
            println("Code PIN mis à jour.\nRetour au menu principal...")
          }
        }while (!isValid)
      }
      def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
        if (coffeeStocks(machineId) >= coffeeNeeded && sugarStocks(machineId) >= sugarNeeded
          && milkStocks(machineId) >= milkNeeded) {
          //MàJ stocks début
          coffeeStocks(machineId) -= coffeeNeeded
          sugarStocks(machineId) -= sugarNeeded
          milkStocks(machineId) -= milkNeeded
          //MàJ stocks fin3
          //addition début
          println("\nBoisson choisie   : " + com1 + "\nNiveau de sucre   : " + com2)
          if (!isEspresso) println("Lait en supplément: " + com3)
          print("Prix total: ")
          if (ad1 > 0) printf("CHF %.2f", ad1.toDouble / 100) //boisson
          if (ad2 > 0) printf(" + CHF %.2f", ad2.toDouble / 100) //supplément sucre
          if (ad3 > 0) printf(" + CHF %.2f", ad3.toDouble / 100) //supplément lait
          printf(" = CHF %.2f", {
            ad1 + ad2 + ad3
          }.toDouble / 100)
          //addition fin
          //paiement début
          if (isLatte) com1 = "Latte" //correction du nom
          val twint = Random.alphanumeric.take(5).mkString.toUpperCase //génère "code twint"
          println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + twint + "\n(En attente de paiement...)")
          Thread.sleep(3000) //délai obligatoire de 3000 milisecondes
          println("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + com1 + " est prêt ! Bonne dégustation !")
          //paiement fin
          return true
        }
        //stock suffisant fin
        //Erreurs stock début
        else {
          print("\n")
          if (milkStocks(machineId) >= milkNeeded)
            println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          if (sugarStocks(machineId) >= sugarNeeded)
            println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
          if (coffeeStocks(machineId) >= coffeeNeeded)
            println("Erreur: Quantité de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre machine ou vérifier les stocks en mode Admin.(Machine choisie: Machine "+{machineId+1}+")")
          return false
        }
        //Erreur stock fin
        //contrôle des stocks fin
      }
      def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
        var tpoudre = 0
        var tsucre = 0
        var tlait = 0
        printf("\nStock:\n    Poudre de café:%5d  g\n    Lait          :%5d mL\n    Sucre         :%5d  g\nRéapprovisionnement des stocks...\nAjout :\n", coffeeStocks(machineId), milkStocks(machineId), sugarStocks(machineId))
        tpoudre = readLine("    Poudre de café (g):").toInt
        tlait = readLine("    Lait          (mL):").toInt
        tsucre = readLine("    Sucre          (g):").toInt
        coffeeStocks(machineId) += tpoudre
        sugarStocks(machineId) += tsucre
        milkStocks(machineId) += tlait
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      }
      def Client(): Unit = {
        println("\nMachine sélectionnée 1-5 >")
        machineId = read(0, nbMachines).toInt - 1
        //while (true) {
        println("\nVeuillez sélectionner votre boisson :\n1) Espresso   - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte      - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choix = read(1, 3)
        //sélection type de boisson fin
        //Latte début
        if (choix == "3") {
          isLatte = true
          println("\nVeuillez sélectionner la taille :")
          println("1) Petit\n2) Moyen\n3) Grand")
          choix = read(1, 3)
          if (choix == "1") {
            ad1 = 270
            milkNeeded = 120
            coffeeNeeded = 6
            com1 = "Latte (Petit)"
          }
          else if (choix == "2") {
            ad1 = 320
            milkNeeded = 150
            coffeeNeeded = 8
            com1 = "Latte (Moyen)"
          }
          else if (choix == "3") {
            ad1 = 370
            milkNeeded = 200
            coffeeNeeded = 12
            com1 = "Latte (Grand)"
          }
        }
        //Latte fin
        //esp+capp début
        else if (choix == "1") {
          ad1 = 200
          coffeeNeeded = 8
          com1 = "Espresso"
          isEspresso = true
        }
        else if (choix == "2") {
          println("\nMachine sélectionnée 1-5 >")
          machineId = read(0, nbMachines).toInt
          ad1 = 250
          coffeeNeeded = 6
          milkNeeded = 100
          com1 = "Cappuccino"
        }
        //esp+capp fin
        //supplément sucre début
        println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu      (5g)  - CHF 0.10\n3) Moyen    (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
        choix = read(1, 4)
        sugarNeeded = (choix.toInt - 1) * 5 ////ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
        ad2 = (choix.toInt - 1) * 10
        if (choix == "1") com2 = "Sans Sucre"
        else if (choix == "2") com2 = "Peu (5g)"
        else if (choix == "3") com2 = "Moyen (10g)"
        else com2 = "Beaucoup (15g)"
        //supplément sucre fin
        //supplément lait début
        if (!isEspresso) {
          println("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
          choix = read(0, 2)
          if (choix == "1") {
            println("\nCombien de doses ? (0,05CHF par dose. 3 Doses maximum)")
            do {
              choix = readLine(">")
            }
            while (choix < "0" || choix > "3")
            ad3 = choix.toInt * 5 //ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
            milkNeeded += (choix.toInt * 100)
            if (choix == "1") com3 = "1 Dose"
            else if (choix == "2") com3 = "2 Doses"
            else if (choix == "3") com3 = "3 Dose"
            else com3 = "Non"
          }
          else com3 = "Non"
        }
      }
        def Admin(): Unit = {
          println("\nMachine sélectionnée 1-5")
          machineId = read(1, nbMachines).toInt - 1
          if (validatePin(machineId, machinePins)) {
            println("Que voulez-vous faire?\n1)Changer le code PIN\n2)Mettre à jour les stocks")
            choix = read(1, 2)
            if (choix == "1") updatePin(machineId, machinePins)
            else if (choix == "2") restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
        }
      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////Début machine///////////////////////////////////////////////////////////////////////////////////////////////////////
      do {
        println("\nVeuillez sélectionner le mode")
        println("1)Client\n2)Admin\n3)Quitter")
        choix = read(1, 3)
        //début mode client///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        if (choix=="1") {
          do{
          Client()
          }while(!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks))
          choix=""
        }
        //fin mode client//////////////////////////////////////////////////////////////////////////////////////////////////
        //début mode admin/////////////////////////////////////////////////////////////////////////////////////////////////
        if (choix == "2") {
          Admin()
        }//fin mode admin////////////////////////////////////////////////////////////////
      } while (choix != "3" && choix != "end")
    }
    NospressoMachine
  }
}