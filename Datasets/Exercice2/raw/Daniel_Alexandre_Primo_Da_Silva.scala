import scala.io.StdIn._
import util.Random

object Main {
  // VALIDER PIN AVEC DEF
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    println(" Entrez le Code PIN.")
    print(" > ")
    val codepin = readLine("")
    //SI LE CODE PIN N'EST PAS EGAL
    if (codepin != machinePins(machineId-1)) {
      return false
    }
    // SI LE CODE PIN EST EGAL
    else {
      return true
    }
  }
  // METTRE A JOUR CODE PIN AVEC DEF
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    println("Veuillez entrer un nouveau Code PIN à 6 chiffres.")
    var codePINMAJ = ""
    while(codePINMAJ.length!=6){
        codePINMAJ = readLine(">")
      if(codePINMAJ.length == 6) {
        machinePins(machineId - 1) = codePINMAJ
        println("Le code PIN a été mis à jour avec succès")
      }
      else{
        println("Veuillez saisir un code à 6 chiffres")
      }
    }

  }
// MODE CLIENT SELECTION DE BOISSON AVEC DEF
  def serveClient(machineId: Int, SIcafe: Array[Int], SIsucre: Array[Int], SIlait: Array[Int]): Boolean = {

    var choixdumode = 0
    val modeclient = 1
    var Lait = 0
    var Dose = 0
    var consCafe = 0
    var consLait = 0
    var quantiteSucre = 0
    var prixboisson = 0.00
    var Prixfinal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00


      // CHOIX DE LA BOISSON
      val Expresso = 1
      val Cappuccino = 2
      val Latte = 3
      println(" Veuillez séléctionner votre boisson :")
      println(" 1) Expresso (CHF 2.00) ")
      println(" 2) Cappuccino (CHF 2.50) ")
      println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
      print("> ")

      var choixboisson = readInt()

      // ERREUR CHOIX DE LA BOISSON
      while ((choixboisson != Expresso) && (choixboisson != Cappuccino) && (choixboisson != Latte)) {
        println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
        println(" Veuillez séléctionner votre boisson :")
        println(" 1) Expresso (CHF 2.00) ")
        println(" 2) Cappuccino (CHF 2.50) ")
        println(" 3) Latte Petit (CHF 2.70) Moyen (CHF 3.20) Grand (CHF 3.70) ")
        print("> ")
        choixboisson = readInt()
      }
      prixsucre = 0
      prixlait = 0

      // CHOIX BOISSON = EXPRESSO
      if (choixboisson == Expresso) {
        println("Vous avez séléctionné un Expresso. ")
        prixboisson = 2.00
        consCafe = 8
      }

      //CHOIX BOISSON = CAPPUCCINO
      if (choixboisson == Cappuccino) {
        println("Vous avez séléctionné un Cappuccino. ")
        prixboisson = 2.50
        consCafe = 6
        consLait = 100
      }
      // CHOIX BOISSON = LATTE
      if (choixboisson == Latte) {
        println("Vous avez séléctionné un Latte. ")
      }
      // CHOIX TAILLE LATTE
      if (choixboisson == Latte) {
        val LattePetit = 1
        val LatteMoyen = 2
        val LatteGrand = 3

        println("Quelle taille de Latte désirez-vous ?")
        println("1) Petit (CHF 2.70)")
        println("2) Moyen (CHF 3.20)")
        println("3) Grand (CHF 3.70)")
        print(">")

        var taille = readInt()

        // ERREUR CHOIX TAILLE LATTE
        while ((taille != 1) && (taille != 2) && (taille != 3)) {
          println("La valeur choisie est fausse, choisissez-en une nouvelle")
          println("Quelle taille de Latte désirez-vous ?")
          println("1) Petit (CHF 2.70)")
          println("2) Moyen (CHF 3.20)")
          println("3) Grand (CHF 3.70)")
          print(">")
          taille = readInt()
        }
        // LATTE TAILLE PETIT
        if (taille == LattePetit) {
          println("Vous avez choisi un Petit Latte (CHF 2.70). ")
          prixboisson = 2.70
          consCafe = 6
          consLait = 120
        }
        // LATTE TAILLE MOYEN
        if (taille == LatteMoyen) {
          println("Vous avez choisi un Latte Moyen (CHF 3.20). ")
          prixboisson = 3.20
          consCafe = 8
          consLait = 150
        }
        // LATTE TAILLE GRAND
        if (taille == LatteGrand) {
          println("Vous avez choisi un Grand Latte (CHF 3.70). ")
          prixboisson = 3.70
          consCafe = 12
          consLait = 200
        }
      }
      //CHOIX SUCRE
      val sucreOui = 1
      val sucreNon = 2

      println("Voulez-vous ajouter du sucre ?")
      println("1) oui")
      println("2) non")
      print(">")

      var choixsucre = readInt()

      //SI ERREUR CHOIX SUCRE
      while (!(choixsucre == sucreOui) && !(choixsucre == sucreNon)) {
        println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
        println("Voulez-vous ajouter du sucre ?")
        println("1) oui")
        println("2) non")
        print(">")
        choixsucre = readInt()
      }
      // SI AUCUN SUCRE
      if (choixsucre == sucreNon) {
        println("Vous ne souhaitez pas de sucre.")
        quantiteSucre = 0
      }
      val peudesucre = 1
      val moyensucre = 2
      val beaucoupsucre = 3

      // SI CHOIX SUCRE
      if (choixsucre == sucreOui) {
        println("Vous désirez du sucre en supplément. ")
        println("Quelle quantité de sucre désirez-vous ?")
        println("1) Peu (5g CHF 0.10)")
        println("2) Moyen (10g CHF 0.20)")
        println("3) Beaucoup (15g CHF 0.30).")
        print(">")

        choixsucre = readInt()

        // CHOIX SUCRE = PEU
        if (choixsucre == peudesucre) {
          println("Vous avez choisi peu de sucre. ")
          prixsucre = 0.10
          quantiteSucre = 5
        }
        // CHOIX SUCRE = MOYEN
        else if (choixsucre == moyensucre) {
          println("Vous avez choisi le niveau moyen de sucre. ")
          prixsucre = 0.20
          quantiteSucre = 10
        }
        // CHOIX SUCRE = BEAUCOUP
        else if (choixsucre == beaucoupsucre) {
          println("Vous avez choisi beaucoup de sucre. ")
          prixsucre = 0.30
          quantiteSucre = 15
        }
      }
      //SI ERREUR DOSE SUCRE
      while (!(choixsucre == peudesucre) && !(choixsucre == moyensucre) && !(choixsucre == beaucoupsucre)) {
        println("La valeur saisie est éronée, veuillez en choisir une adéquate .")
        println("Quelle quantité de sucre désirez-vous ?")
        println("1) Peu (5g CHF 0.10)")
        println("2) Moyen (10g CHF 0.20)")
        println("3) Beaucoup (15g CHF 0.30).")
        print(">")
        choixsucre = readInt()
      }

      //LAIT EN SUPPLEMENT (CAPPUCCINO ET LATTE)
      if ((choixboisson == Cappuccino) || (choixboisson == Latte)) {

        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")

        var Lait = readInt()

        // SI ERREUR CHOIX LAIT
        while ((Lait != 1) && (Lait != 2)) {
          println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          print(">")

          Lait = readInt()
        }

        // SI LAIT = NON
        if (Lait == 2) {
          println("Vous ne souhaitez pas de lait en supplément.")

        }
        // SI LAIT = OUI
        if (Lait == 1) {
          println("Vous souhaitez du lait en supplément.")
          println("Combien de doses de lait désirez-vous ?")
          println("1) 1 dose ")
          println("2) 2 doses")
          println("3) 3 doses")
          print(">")

          Dose = readInt()
        }
        var dose1 = 1
        var dose2 = 2
        var dose3 = 3

        if (Dose == 1) {
          println("Vous avez séléctionné 1 dose de lait.")
          prixlait = 0.05
        }
        if (Dose == 2) {
          println("Vous avez séléctionné 2 doses de lait.")
          prixlait = 0.10
        }
        if (Dose == 3) {
          println("Vous avez séléctionné 3 doses de lait.")
          prixlait = 0.15
        }
      }
      // PAIEMENT
      if ((SIsucre(machineId-1) >= quantiteSucre) && (SIlait(machineId-1) >= consLait) && (SIcafe(machineId-1) >= consCafe)) {
        Prixfinal = prixboisson + prixsucre + prixlait
        printf("Votre prix est %.2f + %.2f + %.2f = %.2f CHF ", prixboisson, prixsucre, prixlait, Prixfinal)
        println("Veuillez payer en utilisant TWINT.")
        var paiement = Random.alphanumeric.take(5).mkString
        println("Votre code de paiement est :" + paiement)
        println("(En attente de validation du paiement...)")
        Thread.sleep(5000)
        println(" Merci ! Votre paiement été accepté.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        SIcafe(machineId-1) -= consCafe
        SIlait(machineId-1) -= consLait
        SIsucre(machineId-1) -= quantiteSucre

        if (choixboisson == Expresso) {
          println("Votre Expresso est prêt ! Bonne dégustation !")
        }
        if (choixboisson == Cappuccino) {
          println("Votre Cappuccino est prêt ! Bonne dégustation !")
        }
        if (choixboisson == Latte) {
          println("Votre Latte est prêt ! Bonne dégustation !")
        }
        true
      }
      // STOCKS INSUFFISANTS
      else {
        println("Le stock est insuffisant. ")
        println("Veuillez choisir une autre boisson. ")
        println("Retour au menu principal... ")
         false
      }
    }
    // ACTUALISATION ET AJOUT DES STOCKS AVEC UNE DEF
    def restockMachine(machineId: Int, SIcafe: Array[Int], SIsucre: Array[Int], SIlait: Array[Int]): Unit = {
      var sucreAjout = -10
      var cafeAjout = -10
      var laitAjout = -10

      println("Stocks actuels: ")
      println("Sucre: " + SIsucre(machineId-1) + "g")
      println("Café: " + SIcafe(machineId-1) + "g")
      println("Lait: " + SIlait(machineId-1) + "mL")

// AJOUT NEGATIF DE SUCRE
      while(sucreAjout<0) {
        print("Ajout sucre >")
        sucreAjout = readInt()
        if(sucreAjout<0){
          println("Veuillez saisir une valeur positive.")
        }
      }
// AJOUT NEGATIF DE CAFE
      while(cafeAjout<0) {
        print("Ajout café >")
        cafeAjout = readInt()
        if(cafeAjout<0){
          println("Veuillez saisir une valeur positive.")
        }
      }
// AJOUT NEGATIF DE LAIT
      while(laitAjout<0) {
        print("Ajout Lait >")
        laitAjout = readInt()
        if(laitAjout<0){
          println("Veuillez saisir une valeur positive.")
        }
      }

          SIsucre(machineId-1) += sucreAjout
          SIcafe(machineId-1) += cafeAjout
          SIlait(machineId-1) += laitAjout

          println("Niveaux de stocks mis à jour.")
          println("Retour au menu principal...")

        }

    def main(args: Array[String]): Unit = {

      println(" Nospresso Café")


      val modeadmin = 2
      val modeclient = 1
      val quitter = 3
      var choixsucre = 0
      var choixdumode = 0
      var Lait = 0
      var Dose = 0
      var SIcafe = Array.fill(5)(50)
      var SIsucre = Array.fill(5)(30)
      var SIlait = Array.fill(5)(500)
      var consCafe = 0
      var consLait = 0
      var quantiteSucre = 0
      var prixboisson = 0.00
      var Prixfinal = 0.00
      var prixsucre = 0.00
      var prixlait = 0.00
      var Essais = 1
      var action1 = 1
      var action2 = 2
      val nbMachines = 5
      val machinePins = Array.fill(nbMachines)("434343")
      //Séléction mode = quitter
      while (choixdumode != quitter) {

        println("Veuillez séléctionner votre mode :")
        println("1) Client ")
        println("2) Admin ")
        println("3) Quitter ")
        print("> ")

        choixdumode = readInt()

        // ERREUR SELECTION MODE
        while ((choixdumode != 1) && (choixdumode != 2) && (choixdumode != 3)) {
          println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
          println("Veuillez séléctionner votre mode :")
          println("1) Client ")
          println("2) Admin ")
          println("3) Quitter ")
          print("> ")
          choixdumode = readInt()
        }

        //CHOIX MODE CLIENT
        if (choixdumode == modeclient) {
          println("Vous avez choisi le mode client.")

          println("Machine séléctionnée (1-5)")
          print(">")
          var machineId = readInt()

          // TANT QUE CHOIX MACHINE PAS VALABLE
          while ((machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4) && (machineId != 5)) {
            println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
            println("Machine séléctionnée (1-5)")
            print(">")
            machineId = readInt()
          }
          if (machineId == 1) {
            println("Machine séléctionnée : 1")
          }

          if (machineId == 2) {
            println("Machine séléctionnée : 2")
          }

          if (machineId == 3) {
            println("Machine séléctionnée : 3")
          }

          if (machineId == 4) {
            println("Machine séléctionnée : 4")
          }

          if (machineId == 5) {
            println("Machine séléctionnée : 5")
          }
         serveClient(machineId, SIcafe, SIsucre, SIlait)
        }

        // CHOIX MODE ADMIN
        if (choixdumode == modeadmin) {
          println("Vous avez choisi le mode admin.")

          // CHOIX MACHINE
          val nbMachines = 5
          println("Machine séléctionnée (1-5)")
          print(">")
          var machineId = readInt()

          // TANT QUE CHOIX MACHINE PAS VALABLE
          while ((machineId != 1) && (machineId != 2) && (machineId != 3) && (machineId != 4) && (machineId != 5)) {
            println("La valeur saisie est éronée, veuillez en choisir une adéquate.")
            println("Machine séléctionnée (1-5)")
            print(">")
            machineId = readInt()
          }

          if (machineId == 1) {
            println("Machine séléctionnée : 1")
          }

          if (machineId == 2) {
            println("Machine séléctionnée : 2")
          }

          if (machineId == 3) {
            println("Machine séléctionnée : 3")
          }

          if (machineId == 4) {
            println("Machine séléctionnée : 4")
          }

          if (machineId == 5) {
            println("Machine séléctionnée : 5")
          }
          Essais = 1
          // GESTION TENTATIVES CODE PIN
          while (Essais != 4) {
            val validPin = validatePin(machineId, machinePins)
            if(!validPin){
              println("Code PIN érronné." + (3-Essais) + "essais restants." )
              Essais += 1
              if(Essais==4){
                println("Nombre d'essais échoué. Fin du Programme.")
                choixdumode = quitter
              }
            }
            else if (validPin) {
              println("Accès autorisé à la machine " + machineId)
              Essais = 4
              println("Que souhaitez-vous effectuer ?")
              println("1) Vérifier ou mettre à jour les stocks.")
              print("2) Changer le Code PIN de la machine.> ")
              var action = readInt()
              // CHOIX D'ACTION PAS VALABLE
              while((action != 1) && (action!= 2)){
                println(" Valeur saisie érronnée. Séléctionnez une valeur adéquate.")
                println("Que souhaitez-vous effectuer ?")
                println("1) Vérifier ou mettre à jour les stocks. ")
                print("2) Changer le Code PIN de la machine > ")
                action = readInt()
              }
              // ACCEDER AUX STOCKS
              if(action == 1) {
                restockMachine(machineId, SIcafe, SIsucre, SIlait)
              }
                // ACTUALISER LE CODE PIN
              else if (action == 2) {
                updatePin(machineId, machinePins)
              }
            }
          }
        }
      }
      // Mode séléctionné = quitter
      if (choixdumode == quitter) {
              println("Vous avez décidé de quitter.")
            }
}
}




