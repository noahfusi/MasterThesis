import io.StdIn._
import util._
object Main {
  val nbMachines = 5
  val coffeeStocks = Array(50, 50, 50, 50, 50)
  val sugarStocks = Array(30, 30, 30, 30, 30)
  val milkStocks = Array(500, 500, 500, 500, 500)
  val machinePins = Array("434343", "434343", "434343", "434343", "434343")
  //validation des entrées
  var valide = false

  //vérification des stocks
  var suffit = false

  //commande réussie
  var succes = false
  var successucre = false
  var succescafe = false
  var succeslait = false

  //sortie du programme
  var fin = false

  //choix de l'utilisateur
  var mode = 0
  var cafe = 0
  var sucre = 0
  var lait = 0
  var dose = 0
  var taille = 0

  //variation de stock
  var varlait = 0
  var varcafe = 0
  var varsucre = 0

  //nom des choix
  var boisson = ""
  var qtesucre = ""
  var laitsupp = ""
  var chxtaille = ""

  //prix de chaque élément
  var prixsucre = 0.0
  var prixcafe = 0.0
  var prixlait = 0.0

  //Twint
  var twint = ""
  val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  //Admin
  var nummachine = 0
  var machineId = 0


  //validation des entrées
  def validationentree(entreeclient: Int, valeurmax: Int): Boolean = {
    if ((entreeclient < 1) || (entreeclient > valeurmax)) {
      println("Votre entrée ne correspond pas aux options, veuillez sélectionner une valeur entre 1 et " + valeurmax)
      valide = false
    }
    else {
      valide = true
    }
    return valide
  }

  //verification des stocks
  def stocks(stock : Int, variation :Int): Boolean ={
    if (stock>=variation) {
      suffit = true
    }
    else {
      suffit = false
    }
    return suffit
  }


  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    var isValid = false
    while (attempts > 0 && !isValid) {
      print("Entrez le code PIN pour la machine " + nummachine + "\n" +
        "> ")
      val inputPin = readLine()
      if (inputPin == machinePins(machineId)) {
        println("Accès accordé à la machine " + nummachine + ".")
        isValid = true
      } else {
        attempts -= 1
        if (attempts > 0) println("Code PIN incorrect. " + attempts + " tentative(s) restante(s).")
      }
    }
    if (!isValid) {
      println("Trop de tentatives échouées.")
    }
    return isValid
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit ={
    println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}.")
    var isValid = false
    while (!isValid) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val newPin = readLine()
      if (newPin.length == 6) {
        machinePins(machineId) = newPin
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
        Thread.sleep(2000)
        isValid = true
      } else {
        println("Le code PIN doit contenir exactement 6 chiffres.")
      }
    }
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean  = {
    do {
      print("Veuillez sélectionner votre boisson :\n" +
        "1) Expresso - CHF 2.00\n" +
        "2) Cappuccino - CHF 2.50\n" +
        "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" +
        ">")
      cafe = readInt()
      valide = validationentree(cafe, 3)
    } while (!valide)

    if (cafe == 1) {
      //Expresso
      boisson = "Expresso"
      varcafe = 8
      prixcafe = 2
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
    }
    else if (cafe == 2) {
      boisson = "Cappuccino"
      varcafe = 6
      varlait = 100
      prixcafe = 2.5
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
      do {
        print("Souhaitez-vous ajouter du lait en supplément ?\n" +
          "(Disponible uniquement pour Cappuccino et Latte)\n" +
          "1) Oui\n" +
          "2) Non\n" +
          ">")
        lait = readInt()
        valide = validationentree(lait, 2)
      } while (!valide)
      if (lait == 1) {
        do {
          print("Combien de doses ?\n" +
            ">")
          dose = readInt()
          valide = validationentree(dose, 3)
        } while (!valide)
        prixlait = 0.05 * dose
        varlait = varlait + (50 * dose)
        laitsupp = "Oui"
      }
      else {
        laitsupp = "Non"
      }
    }
    else if (cafe == 3) {
      do {
        boisson = "Latte"
        println("Quelle taille?\n" +
          "1) Petit\n" +
          "2) Moyen\n" +
          "3) Grand\n")
        taille = readInt()
        valide = validationentree(taille, 3)
      } while (!valide)
      if (taille == 1) {
        varcafe = 6
        varlait = 120
        prixcafe = 2.7
        chxtaille = " (Petit)"
      }
      else if (taille == 2) {
        varcafe = 8
        varlait = 150
        prixcafe = 3.2
        chxtaille = " (Moyen)"
      }
      else {
        varcafe = 12
        varlait = 200
        prixcafe = 3.7
        chxtaille = " (Grand)"
      }
      do {
        print("Souhaitez-vous ajouter du sucre ?\n" +
          "1) Sans sucre\n" +
          "2) Peu (5g) - CHF 0.10\n" +
          "3) Moyen (10g) - CHF 0.20\n" +
          "4) Beaucoup (15g) - CHF 0.30\n" +
          ">")
        sucre = readInt()
        valide = validationentree(sucre, 4)
      } while (!valide)
      if (sucre == 1) {
        qtesucre = "Pas de sucre"
      }
      else if (sucre == 2) {
        qtesucre = "Peu (5g)"
        prixsucre = 0.10
        varsucre = 5
      }
      else if (sucre == 3) {
        qtesucre = "Moyen (10g)"
        prixsucre = 0.20
        varsucre = 10
      }
      else {
        qtesucre = "Beaucoup (15g)"
        prixsucre = 0.30
        varsucre = 15
      }
      do {
        print("Souhaitez-vous ajouter du lait en supplément ?\n" +
          "(Disponible uniquement pour Cappuccino et Latte)\n" +
          "1) Oui\n" +
          "2) Non\n" +
          ">")
        lait = readInt()
        valide = validationentree(lait, 2)
      } while (!valide)
      if (lait == 1) {
        do {
          print("Combien de doses ?\n" +
            ">")
          dose = readInt()
          valide = validationentree(dose, 3)
        } while (!valide)
        prixlait = 0.05 * dose
        varlait = varlait + (50 * dose)
        laitsupp = "Oui"
      }
      else {
        laitsupp = "Non"
      }
    }
    successucre = stocks(sugarStocks(machineId), varsucre)
    succescafe = stocks(coffeeStocks(machineId), varcafe)
    succeslait = stocks(milkStocks(machineId), varlait)
    //Réussite
    //Affichage du prix
    if (successucre && succeslait && succescafe) {
      if ((sucre > 1) && (lait == 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
          "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixlait, prixcafe + prixsucre + prixlait)
        println("\n")
      }
      else if ((sucre > 1) && (lait != 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille +"\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixcafe + prixsucre)
        println("\n")
      }
      else if ((sucre == 1) && (lait == 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
          "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixlait, prixcafe + prixlait)
        println("\n")
      }
      else if ((sucre == 1) && (lait != 1)) {
        printf("Boisson sélectionnée : " + boisson + chxtaille + "\n" +
          "Niveau de sucre : " + qtesucre + "\n" +
          "Prix total : CHF %.2f", prixcafe)
        println("\n")
      }
      //Interface de paiement
      for (_ <- 1 to 5) {
        twint += caracteres(Random.nextInt(caracteres.length))
      }
      println("Veuillez payer en utilisant Twint.\n" +
        "Votre code de paiement est : " + twint + "\n" +
        "(En attente de validation du paiement...)\n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.\n")
      //Mise à jour des stocks
      sugarStocks(machineId) -= varsucre
      coffeeStocks(machineId) -= varcafe
      milkStocks(machineId) -= varlait
      println("Préparation de votre boisson...\n" +
        "[...]\n")
      Thread.sleep(3000)
      println("Votre " + boisson + " est prêt ! Bonne dégustation !\n")
      //Remise à zéro des valeurs
      varsucre = 0
      varcafe = 0
      varlait = 0
      twint = ""
      prixcafe = 0
      prixlait = 0
      prixsucre = 0
      succes = true
    }
    return succes
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                       sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    val milkStockL = milkStocks(machineId).toDouble/1000
    println("Niveaux de stock actuels :\n" +
      "Poudre de café : " + coffeeStocks(machineId) + "\n" +
      "Sucre : " + sugarStocks(machineId) + "\n" +
      "Lait : " + milkStockL + "L\n")
    Thread.sleep(1000)
      println("Entrez la quantité de café à ajouter ")
      print("Poudre de café > ")
      val ajoutcafe = readInt()
      if (ajoutcafe > 0) coffeeStocks(machineId) += ajoutcafe

      print("Sucre > ")
      val ajoutsucre = readInt()
      if (ajoutsucre > 0) sugarStocks(machineId) += ajoutsucre

      print("Lait > ")
      val ajoutlaitL = readDouble()
      var ajoutlait = (ajoutlaitL*1000).toInt
      if (ajoutlait > 0) {milkStocks(machineId) += ajoutlait}
    Thread.sleep(2000)
      println("Les stocks ont été mis à jour avec succès.")
      println("Retour au menu principal...")
    Thread.sleep(2000)
    }

    def main(args: Array[String]): Unit = {
      fin = false
      while (!fin) {
        do {
          print("        Nospresso Café\n" +
            "Veuillez sélectionner votre mode :\n" +
            "1) Client\n" +
            "2) Admin\n" +
            "3) Quitter\n" +
            ">")
          mode = readInt()
          validationentree(mode, 3
          )
        } while (!valide)
        if (mode == 1) {
          do {
            println("Machine sélectionnée (1-5) : ")
            machineId = readInt() - 1
            nummachine = machineId + 1
            validationentree(nummachine, nbMachines)
          } while (!valide)
          println("Machine sélectionnée: " + nummachine)
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        }
        else if (mode==2) {
          do {
            println("Machine sélectionnée (1-5) :")
            nummachine = readInt()
            machineId = nummachine - 1
            validationentree(nummachine, nbMachines)
          } while (!valide)
          println("Machine sélectionnée: " + nummachine)
          if (validatePin(machineId, machinePins)) {
            println("1) Réapprovisionner  2) Mettre à jour le code PIN")
            val adminChoice = readInt()
            if (adminChoice == 1) {
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            } else if (adminChoice == 2) {
              updatePin(machineId, machinePins)
            }

          } else {
            println("Fin du programme.")
            fin = true
          }
        }
        else {
          println("Fin du programme.")
          fin = true}
      }
    }
}

