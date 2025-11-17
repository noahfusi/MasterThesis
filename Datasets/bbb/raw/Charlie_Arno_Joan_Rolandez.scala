import scala.io.StdIn._
import scala.util.Random

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tryPIN = readLine("Entrez le code PIN :\n> ")
    return machinePins(machineId) == tryPIN
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise `a jour du code PIN pour la Machine" + machineId + ".")
    var newPIN = "0"
    while (newPIN.length != 6) {
      newPIN = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(machineId) = newPIN
    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {

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
    var quant_lait: Double = 0
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
      quant_lait += 0.1
      prixboisson += 2.50
    }
    else if (choixboisson == 3) {
      print("\nBoisson sélectionnée : Latte")
      if (taille == 1) {
        println(" (Petit)")
        quant_cafe += 6
        quant_lait += 0.12
        prixboisson += 2.70
      }
      else if (taille == 2) {
        println(" (Moyen)")
        quant_cafe += 8
        quant_lait += 0.15
        prixboisson += 3.20
      }
      else if (taille == 3) {
        println(" (Grand)")
        quant_cafe += 12
        quant_lait += 0.2
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
      quant_lait += doses * 0.05
      prixlait += doses * 0.05
    }

    if (quant_cafe <= coffeeStocks(machineId)) {
      if (quant_lait <= milkStocks(machineId)) {
        if (quant_sucre <= sugarStocks(machineId)) {
          var total = prixboisson + prixlait + prixsucre
          printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixboisson, prixlait, prixsucre, total)

          //Génération Code Twint
          val MDP_Gen = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          val Twint_Code = Random.alphanumeric.filter(char => MDP_Gen.contains(char)).take(5).mkString

          println("\nVeuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + Twint_Code)
          println("(En attente de validation du paiement...)")
          Thread.sleep((3000))
          println("\nMerci ! Votre paiement a été accepté.")
          coffeeStocks(machineId) -= quant_cafe
          milkStocks(machineId) -= quant_lait
          sugarStocks(machineId) -= quant_sucre
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

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println("Réapprovisionnement des stocks...")
    println("Ajout:")
    //Ajout des quantités
    coffeeStocks(machineId) += readLine("Poudre de café: ").toInt
    milkStocks(machineId) += readLine("Lait          : ").toDouble
    sugarStocks(machineId) += readLine("Sucre         : ").toInt
    println("Niveaux de stock mis à jour.\n")
  }


  def main(args: Array[String]): Unit = {

    //Var Generales
    val nbMachines = 5
    var machinePins = Array.fill(nbMachines)("434343")


    //Stocks initiaux
    var coffeeStocks = Array.fill(nbMachines)(50)
    var sugarStocks = Array.fill(nbMachines)(30)
    var milkStocks = Array.fill(nbMachines)(0.5)


    var Machine_ON: Boolean = true

    while (Machine_ON) {
      var machineId = readLine("Machine sélectionnée (1-5) > ").toInt
      while (machineId<1 || machineId > 5) machineId=readLine("Veuillez fournir une entrée valide\n> ").toInt
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
      if (choixmenu == 3) Machine_ON = false

      //Mode Admin
      else if (choixmenu == 2) {

        println("Mode Admin")
        var tentatives = 3
        while (tentatives>0){

          if (validatePin(machineId,machinePins)){

            println("Accès Autorisé\n\nStocks:")
            println("Poudre de café: " + coffeeStocks(machineId) + "g")
            println("Lait          : " + milkStocks(machineId) + "L")
            println("Sucre         : " + sugarStocks(machineId) + "g")
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
              if (action==1) updatePin(machineId,machinePins)
              else if (action==2) restockMachine(machineId,coffeeStocks, sugarStocks, milkStocks)
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
        Machine_ON=false}
      }

      else if (choixmenu == 1) {
        serveClient(machineId,coffeeStocks,sugarStocks,milkStocks)
      }
    }
  }
}