
import scala.io.StdIn._
import scala.util.Random
import scala.io.Source._
import collection.mutable.ArrayBuffer
import java.io.{FileNotFoundException, FileWriter, IOException, PrintWriter}

object Main {

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


  class Machine ( var id: Int, var pincode:String, var milk:Int, var sugar:Int, var coffee:Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") milk += amount
      else if (ingredient == "sugar") sugar += amount
      else if (ingredient == "coffee") coffee += amount
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
      false
    }
  }


  def entreeClavier (valeurMax: Int): Int = {
    var choix = 0
    try{
      choix = readLine(">").toInt


      while (choix > valeurMax || choix <  1){
        println("Erreur. Veuillez saisir une valeur valide")
        choix = readLine(">").toInt
      }

      return choix
    }
    catch {
      case ex: java.lang.NumberFormatException =>
        println("Erreur. Veuillez saisir un nombre valide.")
        return entreeClavier(valeurMax)
    }
  }

  def serveClient(machineID : Int, machines: ArrayBuffer[Machine]): Boolean = {
    println("Mode Client")

    choisirCafe()
    choisirSucre()
    choisirLaitSupp()

    if (machineID >= 0 && machineID < machines.length) {
      if (machines(machineID).removeIngredient ("coffee", consommationCafe) && machines(machineID).removeIngredient ("milk", (consommationLait+consommationLaitSupp)) && machines(machineID).removeIngredient ("sugar", consommationSucre)) {
        afficherPaiment()
        return true
      }
      else {
        if (!machines(machineID).removeIngredient ("milk", (consommationLait+consommationLaitSupp))) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sé́lectionnée.")
          println("Veuillez choisir une autre machine ou essazer une autre boisson...")
          return false
        }
        if (machines(machineID).removeIngredient ("coffee", consommationCafe)) {
          println("Erreur : Quantité́ de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre machine ou essazer une autre boisson...")
          return false
        }
        if (machines(machineID).removeIngredient ("sugar", consommationSucre)) {
          println("Erreur : Quantité́ de sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre machine ou essazer une autre boisson...")
          return false
        }
        return false
      }
    }
    else {
      println("Machine ID incorrect.")
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
      println("Souhaitez-vous ajouter du lait en supplément ? CHF 0.05")
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
    Thread.sleep(1000)
    println("Votre " + boisson + " est prêt ! Bonne dégustation !")
  }



  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    println("Entrez le code PIN :")
    var numTentatives = 0
    while (numTentatives < 3){
      val pinEntree = readLine(">")
      if (pinEntree == machines(machineId).pincode) {
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

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1) + ".")
    var pinNew = readLine("Entrez un nouveau code PIN à 6 chiffres >").toString
    while (pinNew.length != 6 || !pinNew.forall(_.isDigit)) {
      println("Le code PIN doit comporter exactement 6 chiffres.")
      pinNew = readLine(">")
    }
    machines(machineId).pincode = pinNew
    println("Le code PIN a été mis à jour avec succès.")
  }


  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    //println("Machine sélectionnée : " + (machineId + 1))
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + machine.coffee + "g")
    println("Sucre : " + machine.sugar + "g")
    printf("Lait : %.2fL\n", machine.milk / 1000.0)
    println()

    println("Entrez les quantités à ajouter :")
    val coffee = readLine("Poudre de café > ").toInt
    val sugar = readLine("Sucre > ").toInt
    val milk = readLine("Lait > ").toDouble

    machine.addIngredient("coffee", coffee)
    machine.addIngredient("sugar", sugar)
    machine.addIngredient("milk", (milk * 1000).toInt)


    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def afficherMachine(id: Int, machine: Machine): Unit = {
    println("Machine " + id + " chargée :")
    println("ID: " + id)
    println("Code PIN: " + machine.pincode)
    println("Lait: " + f"${machine.milk.toDouble / 1000}%.3f L")
    println("Sucre: " + machine.sugar + "g")
    println("Café: " + machine.coffee + "g")
    println()
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    val machines = new ArrayBuffer[Machine]()
    try {
      println("Chargement des machines depuis " + filename)
      val fr = fromFile (filename)
      val ligneMF = fr.reset.getLines
      var i = 1
      if (!ligneMF.isEmpty) ligneMF.next
      while (!ligneMF.isEmpty) {
        var ligne = ligneMF.next
        var machine = ligne.split(",")
        val newMachine = new Machine(
          id = i,
          pincode = machine(0).toString,
          milk = machine(1).toInt,
          sugar = machine(2).toInt,
          coffee = machine(3).toInt
        )
        machines += newMachine
        afficherMachine(i, newMachine)
        i += 1

      }
    }
    catch {
      case ex : java.io.FileNotFoundException =>
        println("Erreur : Le fichier est introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(1)
      case ex: java.io.IOException =>
        println("Erreur d'entrée/sortie")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(1)
    }
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try{
      println("Sauvegarde des machines dans " + filename)
      val writer = new PrintWriter(new FileWriter(filename, false))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (i <- 0 until machines.length) {
        val machine = machines(i)
        val line = machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee
        writer.println(line)
      }
      writer.close()
      println("Les données ont été sauvegardées avec succès.")
    }
    catch {
      case ex : java.io.FileNotFoundException =>
        println("Erreur : Le fichier est introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(1)
      case ex: java.io.IOException =>
        println("Erreur d'entrée/sortie")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)
    var lancement = true
    while (lancement) {
      println("Nospresso Café")
      println()
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var mode = entreeClavier(3)


      if (mode == 1) {
        var machineChoix = 0
        var machineId = 0

          println("Machine sélectionnée (1-5) >")
          machineChoix = entreeClavier(5)
          machineId = machineChoix - 1
          serveClient( machineId, machines)
      }


      else if (mode == 2) {
        println("Machine sélectionnée (1-5) >")
        var machineChoix = entreeClavier(5)
        var machineId = machineChoix - 1

        if (validatePin(machineId, machines)) {
          println("1) Réapprovisionner les stocks")
          println("2) Mettre à jour le code PIN")
          val choix = entreeClavier(2)
          if (choix == 1){
            restockMachine(machineId, machines)
          }
          else {
            updatePin(machineId, machines)
          }
        }
      }

      else if (mode == 3) {
        println("Fin du programme. Merci pour votre utilisation.")
        savecsv(filename, machines)
        lancement = false
      }
    }
  }
}
