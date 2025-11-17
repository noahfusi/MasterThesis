import scala.io.StdIn
import scala.util.Random

object Main {
  // Définition des stocks
  var StockCafe = 50.0
  var StockSucre = 30.0
  var StockLait = 500.0
  var continue = true
  val Pin = "434343"

  def main(args: Array[String]): Unit = {
    // Affichage du menu principal
    while (continue) {
      println("\nNospresso Café")
      println("Veuillez sélectionner un mode :")
      println("1) Client\n2) Admin\n3) Quitter")
      print("> ")

      val selection = StdIn.readLine() // Sélection du mode

      if (selection == "1") {
        clientMode()
      } else if (selection == "2") {
        adminMode()
      } else if (selection == "3") {
        println("Merci de votre visite. À bientôt !")
        continue = false
      } else {
        println("Sélection invalide, veuillez réessayer.")
      }
    }
  }

  def clientMode(): Unit = {
    // Affichage du mode client
    println("\nMode Client - Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val selectionBoisson = StdIn.readLine()
    var prixBoisson = 0.0
    var prixSucre = 0.0
    var prixLait = 0.0
    var prixTaille = 0.0

    if (selectionBoisson == "1") {
      // Sélection 1: Expresso
      if (StockCafe >= 8.0) {
        prixBoisson = 2.0
        StockCafe -= 8.0
        prixSucre = ajouterSucre()
      } else {
        println("Stock insuffisant pour un Expresso.")
        continue = false
      }
    } else if (selectionBoisson == "2") {
      // Sélection 2: Cappuccino
      if (StockCafe >= 6.0 && StockLait >= 100.0) {
        prixBoisson = 2.5
        StockCafe -= 6.0
        StockLait -= 100.0
        prixSucre = ajouterSucre()
        prixLait = ajouterLait()
      } else {
        println("Stock insuffisant pour un Cappuccino.")
        continue = false
      }
    } else if (selectionBoisson == "3") {
      // Sélection 3: Latte
      prixBoisson = 2.7
      println("Veuillez sélectionner la taille de votre boisson :") //Affichage de sélection de la taille du Latte
      println("1) Petit\n2) Moyen\n3) Grand")
      print("> ")

      val selectionTaille = StdIn.readLine()

      if (selectionTaille == "1" && StockCafe >= 6.0 && StockLait >= 120.0) {
        // Sélection 1: Petit
        prixTaille = 0.0
        StockCafe -= 6.0
        StockLait -= 120.0
      } else if (selectionTaille == "2" && StockCafe >= 8.0 && StockLait >= 150.0) {
        //Sélection 2: Moyen
        prixTaille = 0.5
        StockCafe -= 8.0
        StockLait -= 150.0
      } else if (selectionTaille == "3" && StockCafe >= 12.0 && StockLait >= 200.0) {
        //Sélection 3: Grand
        prixTaille = 1.0
        StockCafe -= 12.0
        StockLait -= 200.0
      } else {
        //Si la sélection n'est pas 1, 2 ou 3 pour le choix de la taille
        println("Stock insuffisant ou sélection invalide.")
        continue = false
      }

      prixSucre = ajouterSucre()
      prixLait = ajouterLait()
    } else {
      //Si la sélection n'est pas 1, 2 ou 3 pour le choix de la boisson
      println("Sélection invalide, veuillez réessayer.")
      continue = false
    }

    val prixTotal = prixBoisson + prixSucre + prixLait + prixTaille // Calcul du prix total
    println("Le prix total de votre boisson est : CHF " + prixTotal)

    val codeTwint = Random.alphanumeric.take(5).mkString // Génération du code Twint et validation du paiement
    println("Veuillez payer en utilisant Twint. Votre code de paiement est : " + codeTwint)
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000) // Simulation du délai

    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...")
    Thread.sleep(5000) // Simulation de la préparation
    println("Votre " + {boissonName(selectionBoisson)} + " est prêt ! Bonne dégustation !")
  }

  def ajouterSucre(): Double = { //Option d'ajout de sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val selectionSucre = StdIn.readLine()

    if (selectionSucre == "1") { //Sélection 1: sans sucre
      0.0
    } else if (selectionSucre == "2" && StockSucre >= 5.0) { //Sélection 2: peu de sucre
      StockSucre -= 5.0
      0.1
    } else if (selectionSucre == "3" && StockSucre >= 10.0) { //Sélection 3: sucre moyen
      StockSucre -= 10.0
      0.2
    } else if (selectionSucre == "4" && StockSucre >= 15.0) { //Sélection 4: beaucoup de sucre
      StockSucre -= 15.0
      0.3
    } else { //Si la saisie n'est pas 1, 2 ,3 ou 4 pour le choix du sucre
      println("Stock insuffisant de sucre ou sélection invalide.")
      0.0
    }
  }

  def ajouterLait(): Double = { //Option d'ajout de lait
    println("Souhaitez-vous ajouter du lait ?")
    println("1) Sans lait\n2) 1 dose (50mL) - CHF 0.05\n3) 2 doses (100mL) - CHF 0.10\n4) 3 doses (150mL) - CHF 0.15")
    print("> ")

    val selectionLait = StdIn.readLine()

    if (selectionLait == "1") { //Sélection 1: pas de lait
      0.0
    } else if (selectionLait == "2" && StockLait >= 50.0) { //Sélection 2: 1 dose
      StockLait -= 50.0
      0.05
    } else if (selectionLait == "3" && StockLait >= 100.0) { //Sélection 3: 2 doses
      StockLait -= 100.0
      0.1
    } else if (selectionLait == "4" && StockLait >= 150.0) { //Sélection 4: 3 doses
      StockLait -= 150.0
      0.15
    } else { //Si la saisie n'est pas 1, 2, 3 ou 4 pour l'ajout de lait
      println("Stock insuffisant de lait ou sélection invalide.")
      0.0
    }
  }

  def boissonName(selection: String): String = { //Affichage du nom de la boisson sélectionnée
    if (selection == "1") {
      "Expresso"
    } else if (selection == "2") {
      "Cappuccino"
    } else if (selection == "3") {
      "Latte"
    } else {
      "Boisson"
    }
  }

  def adminMode(): Unit = { //Mode admin
    println("\nVeuillez entrer le code PIN :")
    val pinSaisi = StdIn.readLine()

    if (pinSaisi == Pin) { //Vérification du pin
      println("PIN correct. Voici les stocks actuels :")
      println("Poudre de café : " + StockCafe + " g")
      println("Sucre : " + StockSucre + " g")
      println("Lait : " + StockLait + " ml")

      println("Souhaitez-vous réapprovisionner les stocks ? (oui/non)") //Affichage du choix de réapprovisionner les stocks
      val reapprovisionner = StdIn.readLine()

      if (reapprovisionner == "oui" || reapprovisionner == "Oui" || reapprovisionner == "OUI") { //Si le choix est oui
        println("Entrez la quantité de poudre de café à ajouter (en g) :") //Rajout de café
        StockCafe += StdIn.readDouble()

        println("Entrez la quantité de sucre à ajouter (en g) :") //Rajout de sucre
        StockSucre += StdIn.readDouble()

        println("Entrez la quantité de lait à ajouter (en ml) :") //Rajout de lait
        StockLait += StdIn.readDouble()

        println("Les stocks ont été mis à jour.") //Affchage de mise a jour des stocks
      } else if (reapprovisionner == "non" || reapprovisionner == "Non" || reapprovisionner == "NON") {
        println("Aucun réapprovisionnement effectué.")
      } else {
        println("Réponse invalide. Retour au menu principal.") //Si la saisie n'est pas oui ou non pour le choix de réapprovisionnement
      }
    } else {
      println("PIN incorrect. Retour au menu principal.") //Si le pin est incorrect
    }
  }
}