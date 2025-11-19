import io.StdIn._
import math._
object Main {

  // methode valider le pin pour le mode admin
  def validatePin(machineID: Int, machinPIN: Array[String]): Boolean = {
    var nbessai = 3
    while (nbessai > 0) {
      var PIN = readLine("Entrez le code PIN : ")
      if (PIN == machinPIN(machineID)) {
        println("PIN Correcte. ")
        return true

      }
      else {
        nbessai -= 1
        println("Code PIN incorrect ." + nbessai + " tentatives restantes")
      }
    }
    println("Trop de tentatives echouees. Fin du programme ")
    return false
  }


  //methode pour le updater le pin
  def updatePin(machineID: Int, machinPIN: Array[String]): Unit = {
    println("mise a jour de code PIN pour la machine " + (machineID + 1))
    var PINapdated = readLine("Entrez un nouveau code PIN a 6 chiffres : ")
    while (PINapdated.length != 6) {
      PINapdated = readLine("Entrez un nouveau code PIN a 6 chiffres : ")
    }
    machinPIN(machineID) = PINapdated
    println("Le code PIN a ete mis a jour avec succes.")
    println("Retour au menu principal......")

  }


  def serveClient(machineID: Int, Cafestock: Array[Int], Sucrestock: Array[Int], Laitstcok: Array[Int]): Boolean = {
    //selectionnement de boisson
    var boisson = readLine("veuillez selectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - 2.70 (Petite) , CHF 3.20 (Moyen) , CHF 3.70 (Grand)\n").toInt

    while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
      println("votre choix de boisson n'est pas valide. Veuillez choisir un boisson valide. ")
      boisson = readLine("veuillez selectionner votre boisson :\n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - 2.70 (Petite) , CHF 3.20 (Moyen) , CHF 3.70 (Grand)\n").toInt
    }


    //selectionnement de taille de latte

    var taillelatte = 0
    if (boisson == 3) {
      taillelatte = readLine("quel taille ? \n 1) Petite \n 2) Moyen \n 3) Grand\n").toInt
      while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
        println("votre choix de taille n'est pas valide. veuillez choisire une taille disponible: ")
        taillelatte = readLine("quel taille ? \n 1) Petite \n 2) Moyen \n 3) Grand\n").toInt
      }
    }

    // selectionnement de niveau de sucre

    var niveausucre = readLine("Souhaitez-Vous ajouter du sucre ? \n 1) sans sucre \n 2) peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n ").toInt
    while (!(niveausucre == 1 || niveausucre == 2 || niveausucre == 3 || niveausucre == 4)) {
      println("votre choix de niveau sucre n'est pas valide. Veuillez selectionner une choix valide.")
      niveausucre = readLine("Souhaitez-Vous ajouter du sucre ? \n 1) sans sucre \n 2) peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n ").toInt
    }

    // selectionnement de lait suplementaire disponible uniquement pour capaccino et latte

    var choixlaitsuplementaire = 0
    if (boisson == 2 || boisson == 3) {
      choixlaitsuplementaire = readLine("Souhaitez-Vous ajouter du lait en supplement ? \n (Disponible uniquement pour Cappacino et Latte) \n 1) Oui \n 2) Non \n").toInt
      while (!(choixlaitsuplementaire == 1 || choixlaitsuplementaire == 2)) {
        println("votre choix de lait n'est pas valide. Veuillez selectionner une choix valide. ")
        choixlaitsuplementaire = readLine("Souhaitez-Vous ajouter du lait en supplement ? \n (Disponible uniquement pour Cappacino et Latte) \n 1) Oui \n 2) Non \n").toInt
      }
    }



    // les ingrediant cafe et lait et les prix

    var cafeconsomme = 0
    var laitconsomme = 0
    var prixbase = 0.0
    var boissonselectionne = ""

    if (boisson == 1) {
      cafeconsomme = 8
      laitconsomme = 0
      prixbase = 2.00
      boissonselectionne = " Expresso "
    }
    else if (boisson == 2) {

      cafeconsomme = 6
      laitconsomme = 100
      prixbase = 2.50
      boissonselectionne = " Cappaccino "
    }
    else if (boisson == 3) {
      if (taillelatte == 1) {
        cafeconsomme = 6
        laitconsomme = 120
        prixbase = 2.70
        boissonselectionne = " Latte (petit) "
      }
      if (taillelatte == 2) {
        cafeconsomme = 8
        laitconsomme = 150
        prixbase = 3.20
        boissonselectionne = " Latte ( Moyen ) "
      }
      else if (taillelatte == 3) {
        cafeconsomme = 12
        laitconsomme = 200
        prixbase = 3.70
        boissonselectionne = " Latte ( Grand ) "
      }
    }


    /// la consommation de sucre
    var sucreconsomme = 0
    var nivsucreaiffichealafin = "sans sucre "
    var prixsucre = 0.0

    if (niveausucre == 2) {
      sucreconsomme = 5
      prixsucre = 0.10
      nivsucreaiffichealafin = "peu (5g)"
    }
    else if (niveausucre == 3) {
      sucreconsomme = 10
      prixsucre = 0.20
      nivsucreaiffichealafin = "Moyen (10g)"
    }
    else if (niveausucre == 4) {
      sucreconsomme = 15
      prixsucre = 0.30
      nivsucreaiffichealafin = "Beaucoup (15g)"
    }

    // lait suplementaire


    var prixlaitsupplementaire = 0.0
    var laitafficher = ""
    if (choixlaitsuplementaire == 1) {
      laitafficher = " oui"
      println("combien de dose de lait ? (max 3 )")
      var doselait = readInt()
      while (doselait < 1 || doselait > 3) {
        println("vous avez entrez une nombre de dose invalide. Veuillez choisir une dose entre 1 et 3 . ")
        doselait = readLine("combien de dose de lait ? (max 3 ) ").toInt
      }
      prixlaitsupplementaire = doselait * 0.05
      laitconsomme += doselait * 50
    }
    else if (choixlaitsuplementaire == 2) {
      laitafficher = " Non"
    }

    var prixtotal = 0.0
    prixtotal = prixbase + prixsucre + prixlaitsupplementaire


    //mettre a jour le stocks

    if (cafeconsomme <= Cafestock(machineID) && laitconsomme <= Laitstcok(machineID) && sucreconsomme <= Sucrestock(machineID)) {

      Cafestock(machineID) -= cafeconsomme
      Laitstcok(machineID) -= laitconsomme
      Sucrestock(machineID) -= sucreconsomme

      println(" \n \n Boisson selectionee : " + boissonselectionne + "\n Niveau de sucre : " + nivsucreaiffichealafin)
      if (boisson == 2 || boisson == 3) {
        println("lait suplementaire : " + laitafficher)
      }

      //printf("prixtotal :  %.2f ", prixtotal)
      printf("\nPrix total : " + prixbase + " + " + prixsucre + " + " + prixlaitsupplementaire + " =  %.2f ", prixtotal)

      var code = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var CodeTWINT = ""
      for (i <- 1 to 5) {
        var choixlettres = (Math.random() * 36).toInt
        CodeTWINT += code(choixlettres)
      }
      println("\nVeuillez payer en utilisant Twint.\n Votre code de paiement est : " + CodeTWINT + " \n (En attente de validation du paiement....")
      Thread.sleep(5000)
      println("\n Paiment confirme. \n \nPreparation de votre boisson... ")
      Thread.sleep(5000)
      println("votre " + boissonselectionne + " est pret ! Bonne degustation ! ")

      return true
    }
    else {
      if (cafeconsomme > Cafestock(machineID)) {
        println(" Erreur : Quantite de paudre de cafe insuffisant pour préparer la boisson selectionne. \n Veuillez choisir une autre machine au verifier les stocks en mode admin")
      }
      else if (sucreconsomme > Sucrestock(machineID)) {
        println("Erreur : Quantite de sucre insuffisant pour preparer la boisson selectionne.\n Veuillez choisir une autre machine au verifier les stocks en mode admin  ")
      }
      else if (laitconsomme > Laitstcok(machineID)) {
        println("Erreur : Quantite de lait insuffisant pour preparer la boisson selectionne. \n Veuillez choisir une autre machine au verifier les stocks en mode admin")
      }
      return false
    }
  }

  // methode restocker la machine
  def restockMachine(machineID: Int, Cafestock: Array[Int], Sucrestock: Array[Int], Laitstcok: Array[Int]): Unit = {
    var ajoutcafe = readLine("\n paudre de cafe: ").toInt
    while (ajoutcafe < 0) {
      ajoutcafe = readLine("\n veuillez entrez une quantite valide. \n Paudre de cafe: ").toInt
    }
    Cafestock(machineID) += ajoutcafe
    var ajoutsucre = readLine("\n sucre : ").toInt
    while (ajoutsucre < 0 ) {
      ajoutsucre = readLine("\n  Veuillez entrez une quantite valide. \n sucre : ").toInt
    }
    Sucrestock(machineID) += ajoutsucre
    var ajoutlait = readLine("\n lait : ").toDouble
    ajoutlait = ajoutlait * 1000
    while(ajoutlait <0){
      ajoutlait = readLine("\n Veuillez entrez une quantite valide \n lait : ").toDouble
    }
    Laitstcok(machineID) += ajoutlait.toInt

    println("Les stock ont ete mis a jour avec succes.")
    println("Retour au menu principal......")
  }


  def main(args: Array[String]): Unit = {
    // stocks initials et variable bocle global
    val nbmachine = 5
    var machineID = 0
    var Sucrestock = Array.fill(nbmachine)(30)
    var Cafestock = Array.fill(nbmachine)(50)
    var Laitstcok = Array.fill(nbmachine)(500)
    var machinPIN = Array.fill(nbmachine)("434343")
    var boclemenu = true


    while (boclemenu) {
      // selectionner de mode menu pricipale
      println("         \n  Nospresso Cafe ")
      var mode = readLine("veuillez selectionner votre mode ( avec les chiffres ) :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("votre choix de mode n'est pas valide veuillez choisir un mode valide. ")
        mode = readLine("veuillez selectionner votre mode :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      }





      // mode 1
      // mode client
      if (mode == 1) {
        machineID = readLine("Machine selectionnee : (1-5)").toInt - 1
        while (machineID < 0 || machineID > 4) {
          machineID = readLine("Machine selectionnee n'est pas valide. Veuillez choisir une machine entre 1 et 5 : (1-5)").toInt - 1
        }
        var resultacommande = serveClient(machineID, Cafestock, Sucrestock, Laitstcok)
      }


      else if (mode == 2) {
        machineID = readLine("Machine selectionnee : (1-5) ").toInt - 1
        while (machineID < 0 || machineID > 4) {
          machineID = readLine("Machine selectionnee n'est pas valide. Veuillez choisir une machine entre 1 et 5  : (1-5)").toInt - 1
        }
        var resulta = validatePin(machineID, machinPIN)
        if (resulta) {
          var objective = readLine("que vous voulez faire ? \n 1) reaprovisionement \n 2) update la pin \n").toInt
          while (objective < 1 || objective > 2) {
            objective = readLine("l'entrez n'est pas valide.Que vous voulez faire ? \n 1) reaprovisionement \n 2) update la pin \n").toInt
          }
          if (objective == 1) {
            println("Acces accorde a la machine " + (machineID + 1))
            println("Niveaux de stock actuels : ")
            println("Paudre de cafe : " + Cafestock(machineID) + "g")
            println("Sucre : " + Sucrestock(machineID) + "g")
            var laitenlitre = Laitstcok(machineID) / 1000.toDouble
            println("Lait : " + laitenlitre + "L")
            println("Entrez les quantites a ajouter : ")
            restockMachine(machineID, Cafestock, Sucrestock, Laitstcok)
          }
          else {
            updatePin(machineID, machinPIN)

          }


        }
        else {
          boclemenu = false
        }


      }
      else if (mode == 3) {
        println(" vous avez quitté le programme.")
        boclemenu = false
      }
    }

  }


}