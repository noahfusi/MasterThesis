import scala.io.StdIn.readLine
import scala.util.Random

object NospressoCafe {

  // Quantités initiales
  var poudreDeCafe = 50
  var sucre = 30
  var lait = 500

  val codePin = "434343"

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      readLine().trim match {
        case "1" => modeClient()
        case "2" => modeAdmin()
        case "3" =>
          println("Merci d'avoir utilisé Nospresso Café. Au revoir !")
          System.exit(0)
        case _ => println("Entrée invalide. Veuillez réessayer.")
      }
    }
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    val choice = readLine().trim
    choice match {
      case "1" => commandeBoisson("Expresso", 8, 0, 2.00)
      case "2" => commandeBoisson("Cappuccino", 6, 100, 2.50)
      case "3" => commandeLait()
      case _ => println("Entrée invalide. Retour au menu principal.")
    }
  }

  def commandeLait(): Unit = {
    println("Choisissez une taille :")
    println("1) Petit - CHF 2.70")
    println("2) Moyen - CHF 3.20")
    println("3) Grand - CHF 3.70")
    print("> ")
    val size = readLine().trim
    size match {
      case "1" => commandeBoisson("Latte Petit", 6, 120, 2.70)
      case "2" => commandeBoisson("Latte Moyen", 8, 150, 3.20)
      case "3" => commandeBoisson("Latte Grand", 12, 200, 3.70)
      case _ => println("Entrée invalide. Retour au menu principal.")
    }
  }

  def commandeBoisson(name: String, cafe: Int, laitNecessaire: Int, prixBase: Double): Unit = {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")
    val choixSucre = readLine().trim
    val coutSucre = choixSucre match {
      case "2" => 0.10
      case "3" => 0.20
      case "4" => 0.30
      case "1" => 0.00
      case _ => println("Entrée invalide. Sans sucre par défaut."); 0.00
    }
    val qteSucre = choixSucre match {
      case "2" => 5
      case "3" => 10
      case "4" => 15
      case _ => 0
    }

    val prixTotal = prixBase + coutSucre

    if (cafe > poudreDeCafe) {
      println(s"Erreur : Quantité de poudre de café insuffisante pour préparer $name.")
      return
    }
    if (laitNecessaire > lait) {
      println(s"Erreur : Quantité de lait insuffisante pour préparer $name.")
      return
    }
    if (qteSucre > sucre) {
      println(s"Erreur : Quantité de sucre insuffisante.")
      return
    }

    processusPaiement(prixTotal)

    // Déduction du stock
    poudreDeCafe -= cafe
    sucre -= qteSucre
    lait -= laitNecessaire

    println(s"Préparation de votre boisson...")
    Thread.sleep(5000)
    println(s"Votre $name est prêt ! Bonne dégustation !")
  }

  def processusPaiement(amount: Double): Unit = {
    val twintCode = Random.alphanumeric.take(5).mkString
    println(s"Veuillez payer CHF $amount en utilisant Twint.")
    println(s"Votre code de paiement est : $twintCode")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.")
  }

  def modeAdmin(): Unit = {
    println("Mode Admin")
    print("Entrez le code PIN : ")
    val pinEntre = readLine().trim
    if (pinEntre == codePin) {
      println("Accès autorisé.")
      println(s"Stocks actuels : Poudre de café : $poudreDeCafe g, Lait : $lait ml, Sucre : $sucre g")
      println("Entrez les quantités à ajouter :")
      print("Poudre de café (g) : ")
      poudreDeCafe += readLine().toIntOption.getOrElse(0)
      print("Lait (ml) : ")
      lait += readLine().toIntOption.getOrElse(0)
      print("Sucre (g) : ")
      sucre += readLine().toIntOption.getOrElse(0)
      println("Niveaux de stock mis à jour.")
    } else {
      println("Code PIN incorrect.")
    }
  }
}
