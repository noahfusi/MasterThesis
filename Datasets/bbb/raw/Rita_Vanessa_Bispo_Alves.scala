import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  //Déclaration des variables
  val nbMachines = 5
  var coffeeStocks = Array.fill(nbMachines)(50)
  var sugarStocks = Array.fill(nbMachines)(30)
  var milkStocks = Array.fill(nbMachines)(500)
  var machinePins = Array.fill(nbMachines)("434343")

  //Valider le code PIN de la machine sélectionnée
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentatives = 3
    while (tentatives > 0 && machineId > 0 && machineId <= 5) {
      println(s"Entrez le code PIN de la machine $machineId :\n> ")
      val PIN = readLine()
      if (PIN == machinePins(machineId-1)) {
        println(s"Accès accordé à la machine $machineId.")
        return true
      }
      else {
        tentatives -= 1
        println(s"Code PIN incorrect. $tentatives tentative(s) restante(s).")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }
  //Mettre à jour PIN de la machine sélectionnée
  def updatePIN(machineId: Int, machinePins: Array[String]): Unit = {
    var newPIN = ""
    println(s"Mise à jour du code PIN pour la machine $machineId.")
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :\n> ")
      newPIN = readLine()
    }
    while (newPIN.length != 6 || !newPIN.forall(_.isDigit))
    machinePins(machineId-1) = newPIN
    println(s"Le code PIN a été mis à jour avec succès pour la machine $machineId.\nRetour au menu principal...")
  }
  //Traiter une transaction client pour la machine sélectionnée
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    //Réinitialisation des variables à chaque nouvelle commande
    var prix_boisson = 0.00
    var cafe_requis = 0
    var sucre_requis = 0
    var prix_sucre = 0.00
    var lait_requis = 0
    var prix_dose_lait = 0.00
    var lait_suppl = "Non"
    var nom_boisson = ""
    var nom_sucre = ""

    //Sélection de la boisson
    println(s"Machine $machineId sélectionnée.\nChoisissez votre boisson : \n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    val choix = readLine().toInt

    if (choix >= 1 && choix <= 3) {
      if (choix == 1) {
        nom_boisson = "Expresso"
        prix_boisson += 2.00
        cafe_requis += 8
      }
      else if (choix == 2) {
        nom_boisson = "Cappuccino"
        prix_boisson += 2.50
        cafe_requis += 6
        lait_requis += 100
      }
      else if (choix == 3) {
        println("Quelle taille désirez-vous ?\n1) Petit\n2) Moyen\n3) Grand\n> ")
        val Latte_taille = readLine().toInt
        if (Latte_taille == 1){
          nom_boisson = "Latte petit"
          prix_boisson += 2.70
          cafe_requis += 6
          lait_requis += 120
        }
        else if (Latte_taille == 2){
          nom_boisson = "Latte moyen"
          prix_boisson += 3.20
          cafe_requis += 8
          lait_requis += 150
        }
        else if (Latte_taille == 3){
          nom_boisson = "Latte grand"
          prix_boisson += 3.70
          cafe_requis += 12
          lait_requis += 200
        }
        else {
          println("Choix invalide")
          return false
        }
      }
      else {
        println("Choix invalide")
        return false
      }
    }
    //Sélection du lait supplémentaire pour cappuccino et latte
    if (choix == 2 || choix == 3){
      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n> ")
      val lait = readLine().toInt
      if (lait == 1){
        lait_suppl = "Oui"
        println("Combien de dose (50ml)? (3 doses max par boisson)\n> ")
        val dose_lait = readLine().toInt

        if (dose_lait > 0 && dose_lait <= 3){
          prix_dose_lait += dose_lait * 0.05
          lait_requis += dose_lait * 50
        }
        else if(dose_lait > 3) {
          println("Il n'y a que 3 doses maximales par boisson\\nChoix invalide, retour au menu principal")
          return false
        }
        else {
          println("Choix invalide")
          return false
        }
      }
      else if (lait == 2){
      }
      else {
        println("Choix invalide")
        return false
      }
    }
    //Sélection du sucre
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ")
    val sucre = readLine().toInt
    if (sucre == 1) {
      nom_sucre = "Sans sucre"
      prix_sucre = 0.00
      sucre_requis += 0
    }
    else if (sucre == 2) {
      nom_sucre = "Peu (5g)"
      prix_sucre += 0.10
      sucre_requis += 5
    }
    else if (sucre == 3) {
      nom_sucre = "Moyen (10g)"
      prix_sucre += 0.20
      sucre_requis += 10
    }
    else if (sucre == 4) {
      nom_sucre = "Beaucoup (15g)"
      prix_sucre += 0.30
      sucre_requis += 15
    }
    else {
      println("Choix invalide, retour au menu principal")
      return false
    }
    //Vérifier les stocks
    val prix_Final = prix_boisson + prix_sucre + prix_dose_lait
    if (coffeeStocks(machineId-1) >= cafe_requis && sugarStocks(machineId-1) >= sucre_requis && milkStocks(machineId-1) >= lait_requis) {
      println(s"Boisson sélectionnée : $nom_boisson\nNiveau de sucre : $nom_sucre\nLait supplémentaire : $lait_suppl\nPrix total : CHF $prix_Final")

      //Code TWINT
      val code_Twint = Random.alphanumeric.take(5).mkString
      println("\nVeuillez payer en utilisant Twint\nVotre code de paiement est : " + code_Twint)
      println("(En attente de validation du paiement...)")

      //Attente de 3 secondes pour simuler validation paiement
      Thread.sleep(3000)

      //Mise à jour des stocks
      coffeeStocks(machineId-1) -= cafe_requis
      sugarStocks(machineId-1) -= sucre_requis
      milkStocks(machineId-1) -= lait_requis

      println("\nPaiement confirmé.\nPréparation de votre boisson...")

      //Attente de 5 secondes pour préparation boisson
      Thread.sleep(5000)
      println("Votre " + nom_boisson + " est prêt ! Bonne dégustation !")
      true
    }
    else if(milkStocks(machineId-1) < lait_requis){
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayez une autre boisson ou une autre machine.")
      false
    }
    else if(coffeeStocks(machineId-1) < cafe_requis){
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou essayez une autre machine ou vérifiez les stocks en mode Admin")
      false
    }
    else if(sugarStocks(machineId-1) < sucre_requis){
      println("Erreur : Qantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une quantité de sucre plus petite ou essayez sur une autre machine")
      false
    }
    else{
      false
    }
  }
  //Réapprovisionner les ingrédients pour la machine sélectionnée
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"Réapprovisionnement de la machine $machineId.\nEntrez les quantités à ajouter :")
    println("Poudre de café (en grammes):\n> ")
    val ajout_cafe = readLine().toInt
    println("Sucre (en grammes):\n> ")
    val ajout_sucre = readLine().toInt
    println("Lait (en litres):\n> ")
    val ajout_lait = (readDouble() * 1000).toInt

    //Validation des entrées
    if (ajout_cafe >= 0 && ajout_sucre >= 0 && ajout_lait >= 0) {
      coffeeStocks(machineId-1) += ajout_cafe
      sugarStocks(machineId-1) += ajout_sucre
      milkStocks(machineId-1) += ajout_lait
      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
    }
    else {
      println("Quantités invalides. Les stocks n'ont pas été mis à jour.")
    }
  }
  //Afficher le menu principal et gérer les choix
  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      //Menu principal
      println("\n     Nospresso Café \nVeuillez sélectionner votre mode :\n1. Client\n2. Admin\n3. Quitter\n> ")
      val mode = readLine().toInt

      //Mode client
      if (mode == 1) {
        println("Sélectionnez une machine (1-5):\n> ")
        val machineId = readLine().toInt
        if (machineId < 1 || machineId > nbMachines) {
          println("Machine invalide. Essayez encore.")
        }
        else if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
          println("Transaction échouée. Essayez à nouveau.")
        }
      }
      //Mode admin
      else if (mode == 2) {
        println("Sélectionnez une machine (1-5):\n> ")
        val machineId = readLine().toInt
        if (machineId == -1) {
          continuer = false
          println("Fin du programme.")
        }
        else if (machineId < 0 || machineId > nbMachines) {
          println("Machine invalide. Essayez encore.")
        }
        if (validatePin(machineId, machinePins)) {
          println("1. Réapprovisionner les ingrédients\n2. Mettre à jour le code PIN\n> ")
          val choix_admin = readLine().toInt
          if (choix_admin == 1) {
            println(s"Niveaux de stock actuels :\n   Poudre de café : " + coffeeStocks(machineId-1) + "g\n   Sucre : " + sugarStocks(machineId-1) + "g\n   Lait : " + (milkStocks(machineId-1).toDouble / 1000) + "L")
            restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          }
          else if (choix_admin == 2) {
            updatePIN(machineId, machinePins)
          }
          else {
            println("Choix invalide.")
          }
        }
      }
      //Quitter
      else if (mode == 3) {
        println("Merci et à bientôt !")
        continuer = false
      }
      //Mode "erreur"
      else {
        println("Retour au menu principal")
      }
    }
  }
}