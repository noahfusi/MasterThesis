import scala.io.Source._
import java.io.{File, FileWriter, PrintWriter}
import collection.mutable.ArrayBuffer
import io.StdIn._
import io.StdIn._
import math._

object Main {
  class Machine(val id: Int = 0, var pincode: String = "", var milk: Int = 0, var sugar: Int = 0, var coffee: Int = 0) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") milk += amount
      else if (ingredient == "sugar") sugar += amount
      else if (ingredient == "coffee") coffee += amount
      else {
        println("l'ingrediant n'est pas valide ")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient == "milk" && milk >= amount) {
        milk -= amount
        return true
      }
      else if (ingredient == "sugar" && sugar >= amount) {
        sugar -= amount
        return true
      }
      else if (ingredient == "coffee" && coffee >= amount) {
        coffee -= amount
        return true
      }
      else {
        return false
      }
    }


    def imprimerCSV(): String = {
      return pincode + "," + milk + "," + sugar + "," + coffee
    }

    def affichage(): Unit = {
      println("ID : " + id)
      println("Code : " + pincode)
      var milklitre = milk/ 1000.toDouble
      println("Lait : " + milklitre + " militre")
      println("sucre : " + sugar + " gramme")
      println("cafe : " + coffee + " gramme")
    }

  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    var files = new File(filename)
    if(!files.exists()){
      println("Erreur : Echec de l'ecriture dans machines.csv.\nLe fichier peut etre verrouille ou en lecture seule")
    }
    else {
      try {
        var creationfichier = new PrintWriter(new FileWriter(filename, false))
        creationfichier.println("PINCODE", "MILK", "SUGAR", "COFFEE")
        for (x <- 0 until machines.length) {
          creationfichier.println(machines(x).imprimerCSV())
        }
        println("Sauvegarde de " + machines.length + " machines dans machines.csv...")
        println("Fichier sauvegarde avec succes.")
        creationfichier.close

      }

      catch {
        case ex: java.io.IOException => println("Echec de l’´ecriture dans machines.csv.\nLe fichier peut etre verrouille ou en lecture seule")
      }
    }
}

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      var tableauxmachine = new ArrayBuffer[Machine]()
      var lirefromeFile = fromFile(filename)
      var lireligneparligne = lirefromeFile.reset.getLines().drop(1)

      var ID = 1
      while (!lireligneparligne.isEmpty) {
        var infosrecupereTexte = lireligneparligne.next
        var infosrecupereTableaux = infosrecupereTexte.split(",")
        var machineT = new Machine(ID, infosrecupereTableaux(0), infosrecupereTableaux(1).toInt, infosrecupereTableaux(2).toInt, infosrecupereTableaux(3).toInt)
        tableauxmachine += machineT
        ID += 1
      }
      lirefromeFile.close
      return tableauxmachine
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Verifiez le chemin dacces et reessayez.")
        return null
    }
  }

  def validatePin(machineID: Machine, codmachin : String): Boolean = {
    var nbessai = 3
    while (nbessai > 0) {
      var PIN = readLine("Entrez le code PIN : ")
      if (PIN == codmachin) {
        println("PIN Correcte. ")
        return true
      }
      else {
        nbessai -= 1
        println("Code PIN incorrect ." + nbessai + " tentatives restantes")
      }
    }
    println("Trop de tentatives echouees. retour au menue principale ")
    return false
  }

  def updatePin(machineID: Machine): Unit = {
    println("mise a jour de code PIN pour la machine " + (machineID.id))
    var PINapdated = readLine("Entrez un nouveau code PIN a 6 chiffres : ")
    while (PINapdated.length != 6) {
      PINapdated = readLine("Entrez un nouveau code PIN a 6 chiffres : ")
    }
    machineID.pincode = PINapdated
    println("Le code PIN a ete mis a jour avec succes.")
    println("Retour au menu principal......")

  }


  def servclien(machinet: Machine): Boolean = {

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

    if(machinet.coffee >= cafeconsomme && machinet.milk >= laitconsomme && machinet.sugar>= sucreconsomme){
      machinet.removeIngredient("coffee", cafeconsomme)
      machinet.removeIngredient("milk", laitconsomme)
      machinet.removeIngredient("sugar", sucreconsomme)
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
      println("Stock de machine insuffisant. Veuillez choisir une autre machine.")
      return false
    }
  }

  def main(args: Array[String]): Unit = {
    var boclemeu = true
    //affichage des machine depuis fichier
    var machine = loadcsv("machines.csv")
    if(machine != null) {
      println("Chargement des machines depuis machines.csv...")
      for (i <- 0 to machine.length - 1) {
        println(i+1 + " machines est chargees. ")
        machine(i).affichage()

      }
      println(machine.length + " machines  chargees avec succes. ")
    }
    else{
      println("Il n'est pas possible de lire le fichier")
      boclemeu = false
    }


    while (boclemeu) {

      //il faut l'appeler quant on choisi mode quitter
      // savecsv("machines.csv",machine)
      // chois mode
      println("         \n  Nospresso Cafe ")
      var mode = readLine("veuillez selectionner votre mode ( avec les chiffres ) :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("votre choix de mode n'est pas valide veuillez choisir un mode valide. ")
        mode = readLine("veuillez selectionner votre mode :  \n 1) Client \n 2) Admin \n 3) Quitter \n ").toInt
      }
      var idm = 0
      if (mode == 1) {
        var choixmachine = readLine("quel machine ? ").toInt
        while (choixmachine <= 0 || choixmachine > machine.length){
          choixmachine = readLine("quel machine ? ").toInt
        }
        idm = choixmachine - 1
        servclien(machine(idm))
      }
      else if(mode == 2 ){
        var choixmachine2 = readLine("quel machine ? ").toInt
        while (choixmachine2 <= 0 || choixmachine2 > machine.length){
          choixmachine2 = readLine("quel machine ? ").toInt
        }
        idm =  choixmachine2 - 1
        var resulta = validatePin(machine(idm),machine(idm).pincode)
        if (resulta) {
          var objective = readLine("que vous voulez faire ? \n 1) reaprovisionement \n 2) update la pin \n").toInt
          while (objective < 1 || objective > 2) {
            objective = readLine("l'entrez n'est pas valide.Que vous voulez faire ? \n 1) reaprovisionement \n 2) update la pin \n").toInt
          }
          if (objective == 1) {
            // id machine addIngredient(choixingrediant,choixquantite)
            println("Acces accorde a la machine " + (machine(idm).id))
            println("Niveaux de stock actuels : ")
            var laitenlitre = machine(idm).milk  / 1000.toDouble
            println("lait : " +laitenlitre + " L")
            println("sucre : " + machine(idm).sugar + " g")
            println("coffee : " + machine(idm).coffee + " g")
            println("Entrez les quantites a ajouter : ")
            var milkajout = readLine("lait : ").toDouble
            while(milkajout <0){
              milkajout = readLine("\n Veuillez entrez une quantite valide \n lait : ").toDouble
            }
            milkajout = milkajout * 1000
            var sucreajout = readLine("sucre : ").toInt
            while (sucreajout < 0 ) {
              sucreajout = readLine("\n  Veuillez entrez une quantite valide. \n sucre : ").toInt
            }
            var cafeajout = readLine("paudre de cafe : ").toInt
            while (cafeajout < 0) {
              cafeajout = readLine("\n veuillez entrez une quantite valide. \n Paudre de cafe: ").toInt
            }
            machine(idm).addIngredient("milk", milkajout.toInt)
            machine(idm).addIngredient("sugar", sucreajout)
            machine(idm).addIngredient("coffee", cafeajout)
            println("Les stock ont ete mis a jour avec succes.")
            println("Retour au menu principal......")
          }
          else {
            updatePin(machine(idm))
          }
        }
      }
      else{
        savecsv("machines.csv",machine)
        println("vous avez quitter le progream ")
        boclemeu = false
      }


    }


  }
}
