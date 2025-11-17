import scala.io.StdIn._
import scala.util.Random

object Main {

  val nbMachines = 5
  //val machineID = Array.fill(nbMachines)(1,2,3,4,5)
  var machinePins = Array.fill(nbMachines)("434343")
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  var consommationCafe = 0
  var consommationLait = 0
  var consommationLaitSupp = 0
  var consommationSucre = 0
  var prixBase = 0.00
  var prixLaitSupp = 0.00
  var prixSucre = 0.00
  var prixSupp = 0.00
  var prixFinal = 0.0
  var boisson = ""
  var taille = ""
  var niveausucre = ""
  var laitSupp = ""

  def entreeClavier (valeurMax: Int): Int = {
    var choix = readLine(">").toInt
    while (choix > valeurMax || choix <  1){
      println("Erreur. Veuillez saisir une valeur valide")
      choix = readLine(">").toInt
    }
    return choix
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("Mode Client")

    choisirCafe()
    choisirSucre()
    choisirLaitSupp()

    if (verifierStocks(machineId)) {
      afficherPaiment()

      coffeeStocks(machineId) -= consommationCafe
      milkStocks(machineId) -= consommationLait
      milkStocks(machineId) -= consommationLaitSupp
      sugarStocks(machineId) -= consommationSucre

      return true
    }
    else {
      println("Veuillez choisir une autre machine ou essazer une autre boisson...")
      return false
    }

  }

  def choisirCafe (): Unit = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var choixCafe = entreeClavier(3)

    if (choixCafe == 1) {
      boisson = "Expresso"
      taille = " "
      prixBase = 2.00
      prixSupp = 0.00
      prixLaitSupp = 0.00
      consommationCafe = 8
      consommationLait = 0
      consommationLaitSupp = 0
      laitSupp = "Non"
    }
    else if (choixCafe == 2) {
      boisson = "Cappuccino"
      taille = " "
      prixBase = 2.50
      consommationCafe = 6
      consommationLait = 100
    }
    else if (choixCafe == 3) {
      println("Veuillez sélectionner la taille:")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      var choixTaille = entreeClavier(3)

      if (choixTaille == 1) {
        boisson = "Latte"
        taille = " (Petit) "
        prixBase = 2.70
        consommationCafe = 6
        consommationLait = 120
      }
      else if (choixTaille == 2) {
        boisson = "Latte"
        taille = " (Moyen) "
        prixBase = 3.20
        consommationCafe = 8
        consommationLait = 150
      }
      else if (choixTaille == 3) {
        boisson = "Latte"
        taille = " (Grand) "
        prixBase = 3.70
        consommationCafe = 12
        consommationLait = 200
      }
    }
  }

  def choisirSucre (): Unit = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    var choixSucre = entreeClavier(4)

    if (choixSucre == 1) {
      prixSucre = 0.00
      niveausucre = "Sans sucre"
      consommationSucre = 0
    }
    else if (choixSucre == 2) {
      prixSucre = 0.10
      niveausucre = "Peu (5g)"
      consommationSucre = 5
    }
    else if (choixSucre == 3) {
      prixSucre = 0.20
      niveausucre = "Moyen (10g)"
      consommationSucre = 10
    }
    else if (choixSucre == 4) {
      prixSucre = 0.30
      niveausucre = "Beaucoup (15g)"
      consommationSucre = 15
    }
  }

  def choisirLaitSupp (): Unit = {
    if (boisson == "Cappuccino" || boisson == "Latte") {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")
      var choixLaitSupp = entreeClavier(2)

      if (choixLaitSupp == 1) {
        println("Combien de dose ? (3 doses maximale par boisson)")
        var doseLaitSupp = entreeClavier(3)
        prixLaitSupp = doseLaitSupp * 0.05
        consommationLaitSupp += (doseLaitSupp * 50)
        laitSupp = "Oui"
      }
      else if (choixLaitSupp == 2) {
        laitSupp = "Non"
        consommationLaitSupp = 0
        prixLaitSupp = 0.00
      }
    }
  }

  def verifierStocks (machineId: Int): Boolean = {
    if (milkStocks(machineId) < (consommationLait + consommationLaitSupp)) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sé́lectionnée.")
      return false
    }
    if (coffeeStocks(machineId) < consommationCafe) {
      println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      return false
    }
    if (sugarStocks(machineId) < consommationSucre) {
      println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.")
      return false
    }
    return true
  }

  def afficherPaiment(): Unit = {
    prixSupp = prixLaitSupp + prixSucre
    prixFinal = prixBase + prixSupp
    println()
    println("Boisson sélectionnée : " + boisson + taille)
    println("Niveau de sucre : " + niveausucre)
    println("Lait en supplément : " + laitSupp)
    printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBase, prixSupp, prixFinal)
    println()

    val codeTwint = Random.alphanumeric.take(5).mkString
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + codeTwint)
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été́ accepté.")

    println()

    println("Préparation de votre boisson...")
    println("[...]")
    Thread.sleep(2000)
    println("Votre " + boisson + " est prêt ! Bonne dégustation !")
  }


  def menuAdmin (machineId: Int): Unit = {
    if (validatePin(machineId, machinePins)) {
      println("1) Réapprovisionner les stocks")
      println("2) Mettre à jour le code PIN")
      val choix = entreeClavier(2)
      if (choix == 1){
        restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
      }
      else {
        updatePin(machineId, machinePins)
      }
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    println("Entrez le code PIN :")
    var numTentatives = 0
    while (numTentatives < 3){
      val pinEntree = readLine(">")
      if (pinEntree == machinePins(machineId)) {
        println("Accès accordé à la Machine " + (machineId+1))
        return true
      }
      numTentatives += 1
      println("Code Pin incorrect. " + (3-numTentatives) + " tentatives restantes.")
    }
    println()
    println("Trop de tentatives échouées. Fin du programme.")
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    var pinNew = readLine("Entrez un nouveau code PIN à 6 chiffres >").toString
    while (pinNew.length != 6 || !pinNew.forall(_.isDigit)) {
      println("Le code PIN doit comporter exactement 6 chiffres.")
      pinNew = readLine(">")
    }
    machinePins(machineId) = pinNew
    println("Le code PIN a été mis à jour avec succès.")
  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    //println("Machine sélectionnée : " + (machineId + 1))
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + coffeeStocks(machineId) + "g")
    println("Sucre : " + sugarStocks(machineId) + "g")
    printf("Lait : %.2fL\n", milkStocks(machineId) / 1000.0)
    println()

    println("Entrez les quantités à ajouter :")
    val coffee = readLine("Poudre de café > ").toInt
    val sugar = readLine("Sucre > ").toInt
    val milk = readLine("Lait > ").toDouble

    if (coffee >= 0) coffeeStocks(machineId) += coffee
    if (sugar >= 0) sugarStocks(machineId) += sugar
    if (milk >= 0) milkStocks(machineId) += (milk * 1000).toInt

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }


  def main(args: Array[String]): Unit = {

    var lancement = true
    while (lancement) {
      println("Nospresso Café")
      println()
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var mode = entreeClavier(3)

      /*println("Machine sélectionnée (1-5) >")
      var machineChoix = entreeClavier(5)
      var machineId = machineChoix - 1*/

      if (mode == 1) {
        var machineChoix = 0
        var machineId = 0

        do {
          println("Machine sélectionnée (1-5) >")
          machineChoix = entreeClavier(5)
          machineId = machineChoix - 1
        } while (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks))

        /*println("Machine sélectionnée (1-5) >")
        var machineChoix = entreeClavier(5)
        var machineId = machineChoix - 1
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)*/
      }

      else if (mode == 2) {
        println("Machine sélectionnée (1-5) >")
        var machineChoix = entreeClavier(5)
        var machineId = machineChoix - 1
        menuAdmin(machineId)
      }

      else if (mode == 3) {
        println("Fin du programme. Merci pour votre utilisation.")
        lancement = false
      }
    }
  }
}