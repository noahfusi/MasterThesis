import scala.io.StdIn._

object Nospresso {
  val adminPin = "434343"
  val nbMachines = 5
  var coffeeStocks = Array(50, 50, 50, 50, 50)
  var sugarStocks = Array(30, 30, 30, 30, 30)
  var milkStocks = Array(500, 500, 500, 500, 500)
  var machinePins = Array.fill(nbMachines)(adminPin)

  def main(args: Array[String]): Unit = {
    var allumé = true // les machines commencent allumées
    while (allumé) {
      print("Machine sélectionnée (1-5) > ")
      var machineId = readInt() - 1
      while (machineId < 0 || machineId >= nbMachines){
        println("Option érronée")
        print("Machine sélectionnée (1-5) > ")
        machineId = readInt() - 1
      }
      if (machineId >= 0 && machineId < nbMachines) {
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        var mode = readInt()

        if (mode == 1) {
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        } else if (mode == 2) {
          if (validatePin(machineId, machinePins)) {
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            print("> ")
            var choix = readInt()
            while (choix != 1 && choix !=2){
              println("Option invalide.")
              print("> ")
              choix = readInt()
            }
            if (choix == 1) {
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            } else {
              updatePin(machineId, machinePins)
            }
          } else {
            allumé = false
          }
        } else if (mode == 3){
          println("Retour au choix de machine.")

        } else {
          println("Veuillez sélectionner votre mode :")
          println("1) Client")
          println("2) Admin")
          println("3) Quitter")
          print("> ")
          var mode = readInt()
        }
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    println("Entrez le code PIN : ")
    print("> ")
    while (tentatives > 0) { // on a 3 tentatives pour rentrer le bon pin
      var Pin = readLine()
      if (Pin == machinePins(machineId)) { // le pin est validé
        println("Accès accordé à la Machine " + (machineId +1) + ".")
        return true
      } else { // le pin n'est pas validé il s'est trompé
        tentatives = tentatives - 1
        if(tentatives > 1){
          println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
          print("> ")
        } else {
          println("Code PIN incorrect. " + tentatives + " tentative restante.")
          print("> ")
        }
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false // il a épuisé ses tentatives
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId+1) + ".")
    var nouveauPin = ""
    while (nouveauPin.length != 6) {
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machinePins(machineId) = nouveauPin
    println("Le code PIN pour la machine" + (machineId + 1) + " a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Mode Client pour la Machine " + (machineId+1))
    println("")
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val boisson = readInt()
    var typeboisson = ""
    var qtecafe = 0
    var qtelait = 0
    var prix = 0.0

    if (boisson == 1) {
      qtecafe = 8
      prix = 2.0
      typeboisson = "Expresso"
    } else if (boisson == 2) {
      qtecafe = 6
      qtelait = 10
      prix = 2.5
      typeboisson = "Cappuccino"
    } else if (boisson == 3) {
      println("Choisissez la taille :")
      println("1) Petit - 120ml")
      println("2) Moyen - 150ml")
      println("3) Grand - 200ml")
      print("> ")

      val taille = readInt()
      if (taille == 1) {
        qtecafe = 6
        qtelait = 12
        prix = 2.7
      } else if (taille == 2) {
        qtecafe = 8
        qtelait = 15
        prix = 3.2
      } else if (taille == 3) {
        qtecafe = 12
        qtelait = 2
        prix = 3.7
      } else {
        println("Choix invalide, retour au menu principal.")
        return false
      }
      typeboisson = "Latte"
    } else {
      println("Choix invalide, retour au menu principal.")
      return false
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val taillesucre = readInt()
    var qtesucre = 0
    var coutsucre = 0.0

    if (taillesucre == 2) {
      qtesucre = 5
      coutsucre = 0.10
    } else if (taillesucre == 3) {
      qtesucre = 10
      coutsucre = 0.20
    } else if (taillesucre == 4) {
      qtesucre = 15
      coutsucre = 0.30
    }

    val prixtotal = prix + coutsucre

    if (coffeeStocks(machineId) >= qtecafe && sugarStocks(machineId) >= qtesucre && milkStocks(machineId) >= qtelait) {
      println(f"Prix total : CHF $prixtotal%.2f")
      println("\nVeuillez payer en utilisant Twint.")
      val valeur = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var twintcode = ""
      var index = 0
      while (index < 5) {
        twintcode = twintcode + valeur(scala.util.Random.nextInt(valeur.length))
        index = index + 1
      }
      println("Votre code de paiement est : " + twintcode)
      var twintcodeentre = readLine("Veuillez entrer votre code : ")
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000) // Pause pour simuler le paiement

      if (twintcodeentre == twintcode) {
        println("\nMerci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson ...")
        Thread.sleep(2000)
        println("[...]")
        println("Votre " + typeboisson + " est prêt ! Bonne dégustation !")

        coffeeStocks(machineId) = coffeeStocks(machineId) - qtecafe
        sugarStocks(machineId) =sugarStocks(machineId) - qtesucre
        milkStocks(machineId) = milkStocks(machineId) - qtelait

      } else {
        println("Code Twint incorrect. Paiement annulé.")
        return false
      }
    } else {
      if (coffeeStocks(machineId) < qtecafe) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparér la boisson sélectionnée.")
      }
      if (sugarStocks(machineId) < qtesucre) {
        println("Erreur : Quantité de sucre insuffisante pour préparér la boisson sélectionnée.")
      }
      if (milkStocks(machineId) < qtelait) {
        println("Erreur : Quantité de lait insuffisante pour préparér la boisson sélectionnée.")
      }
      println("Veuillez choisir une autre boisson ou essayer une autre machine.")
      return false
    }
    true
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels :")
    println("    Poudre de café : " + coffeeStocks(machineId) + "g")
    println("    Sucre : " + sugarStocks(machineId) + "g")
    var laitconversion = 0.0
    laitconversion = laitconversion + milkStocks(machineId).toDouble/1000
    println("    Lait : " + laitconversion + "L")
    println("")
    println("Entrez les quantités à ajouter : ")
    print("Poudre de café > ")
    coffeeStocks(machineId) =coffeeStocks(machineId) + readInt()
    print("Sucre > ")
    sugarStocks(machineId) = sugarStocks(machineId) + readInt()
    print("Lait > ")
    laitconversion = readDouble()
    var laitstock = (laitconversion * 1000).toInt
    milkStocks(machineId) =milkStocks(machineId) + laitstock
    println("Les stocks ont été mis à jour avec succès.")
    println("Niveaux de stocks mis à jour :")
    println("")
    println("   Poudre de café : " + coffeeStocks(machineId) + "g")
    println("   Sucre          : " + sugarStocks(machineId) + "g")
    println("   Lait           : " + milkStocks(machineId)/1000 + "L")
    println("")
    println("Retour au menu principal...")
  }
}

