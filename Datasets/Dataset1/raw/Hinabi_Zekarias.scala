import scala.io.StdIn._
import scala.util.Random

object MainExercice1{

   var cafeStock: Int = 50
   var sucreStock: Int = 30
   var laitStock: Double = 0.5


   val boissons = Array(
    ("Expresso", 8, 0.0, 2.00),
    ("Cappuccino", 6, 0.1, 2.50),
    ("Latte", 6, 0.15, 3.00)
  )

  def main(args: Array[String]): Unit = {
    var continuer = true

    // Menu principal
    while (continuer) {
      println(" Veuillez sélectionner votre mode :")
      println("1. Client")
      println("2. Admin")
      println("3. Quitter")
      print(" > ")

      val input = readLine("Entrez votre choix (1, 2 ou 3): ")
      if (input == "1") {
        modeClient()
      } else if (input == "2") {
        modeAdmin()
      } else if (input == "3") {
        println("Merci et à bientôt!")
        continuer = false
      } else {
        println("Erreur: Veuillez entrer 1, 2 ou 3.")
      }
    }
  }

  // Mode Client - Commander une boisson
   def modeClient(): Unit = {
    println("Mode Client")
    println("Choisissez votre boisson:")
    for (i <- boissons.indices) {
      println((i + 1) + ". " + boissons(i)._1 + " (" + boissons(i)._2 + " CHF, " + boissons(i)._3 + "g café, " + (boissons(i)._4 * 1000).toInt + "ml lait)")
    }
    print("> ")

    val input = readLine("Quel est votre choix en chiffre (1, 2 ou 3): ")
    if (input == "1" || input == "2" || input == "3") {
      val choix = input.toInt - 1
      val (nom, cafeRequis, laitRequis, prix) = boissons(choix)

      // Options de sucre
      println("Souhaitez-vous du sucre?")
      println("1. Sans sucre")
      println("2. Peu de sucre (5g)")
      println("3. Beaucoup de sucre (10g)")
      print(" > ")

      val choixSucre = readLine()
      var sucreRequis = 0

      if (choixSucre == "1") {
        sucreRequis = 0
      } else if (choixSucre == "2") {
        sucreRequis = 5
      } else if (choixSucre == "3") {
        sucreRequis = 10
      } else {
        println("Choix invalide, pas de sucre ajouté.")
      }


      var laitSupplementaire = 0.0
      if (nom != "Expresso") {
        println("Souhaitez-vous du lait?")
        println("1. Sans lait")
        println("2. Peu de lait (50ml)")
        println("3. Beaucoup de lait (100ml)")
        print("> ")

        val choixLait = readLine()

        if (choixLait == "1") {
          laitSupplementaire = 0.0
        } else if (choixLait == "2") {
          laitSupplementaire = 0.05
        } else if (choixLait == "3") {
          laitSupplementaire = 0.1
        } else {
          println("Choix invalide, pas de lait ajouté.")
        }
      }


      if (laitStock < laitRequis + laitSupplementaire) {
        println("Désolé, il n'y a pas assez de lait.")
        return
      }
    }


    if (cafeStock >= cafeRequis && sucreStock >= sucreRequis && laitStock >= laitRequis + laitSupplementaire) {

      cafeStock -= cafeRequis
      sucreStock -= sucreRequis
      laitStock -= (laitRequis + laitSupplementaire)

      println("Préparation de votre " + nom + "...")
      println("Votre " + nom + " est prêt! Bonne dégustation.")
      effectuerPaiement(prix)
      afficherStocks()
    } else {
      println("Désolé, nous n'avons pas assez d'ingrédients pour préparer votre boisson.")
    }
    println("Erreur: Veuillez entrer un choix valide (1, 2 ou 3).")


    // Mode Admin - Réapprovisionner les stocks
    def modeAdmin(): Unit = {
      println("Mode Admin")
      println("Entrez le code PIN:")
      val codePin = readLine()


      if (codePin == "434343") {
        println("Accès autorisé.")
        println("Stocks actuels:")
        afficherStocks()

        println("Réapprovisionner le stock de café ? (1 = Oui, 2 = Non)")
        val reapprovisionnerCafe = readLine().toInt
        if (reapprovisionnerCafe == 1) {
          println("Combien de grammes de café souhaitez-vous ajouter ?")
          val ajoutCafe = readLine().toInt
          cafeStock += ajoutCafe
        }

        println("Réapprovisionner le stock de lait ? (1 = Oui, 2 = Non)")
        val reapprovisionnerLait = readLine().toInt
        if (reapprovisionnerLait == 1) {
          println("Combien de litres de lait souhaitez-vous ajouter ?")
          val ajoutLait = readLine().toDouble
          laitStock += ajoutLait
        }

        afficherStocks()
      } else {
        println("Code PIN incorrect. Accès refusé.")
      }
    }


    def afficherStocks(): Unit = {
      println("Stocks actuels - Café: " + cafeStock + "g, Sucre: " + sucreStock + "g, Lait: " + laitStock + "L")
    }

    if (cafeStock >= cafeRequis && sucreStock >= sucreRequis && laitStock >= laitRequis + laitSupplementaire) {

      cafeStock -= cafeRequis
      sucreStock -= sucreRequis
      laitStock -= (laitRequis + laitSupplementaire)

      println("Préparation de votre " + nom + "...")
      println("Votre " + nom + " est prêt! Bonne dégustation.")
      effectuerPaiement(prix)
      afficherStocks()
    } else {
      println("Désolé, nous n'avons pas assez d'ingrédients pour préparer votre boisson.")
    }
  }
}