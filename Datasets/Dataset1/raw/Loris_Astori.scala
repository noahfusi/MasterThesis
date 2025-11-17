import scala.io.StdIn.readLine
import scala.util.Random

object Main {

  //Stock
  var cafestock = 50 //grammes
  var sucrestock = 30 //grammes
  var laitstock = 0.5 //litre
  // Pin
  val codePIN = "434343"

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      println("Veuillez séléctionner votre mode : \n 1) Client \n 2) Admin \n 3) Quitter")
      print("> ")
      val choixmode = readLine()

   //---------------------------------------------------------------------------------

      if (choixmode == "1") {
        var faisabiliteboisson = false
        while (!faisabiliteboisson) {

        println("Séléctionner votre boisson : \n 1) Expresso - CHF 2.00 \n 2) Cappucino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        val choixboisson = readLine()

        if (choixboisson == "1") {
          faireboisson("Expresso", 8, 0.0, 2.0)
        }
        else if (choixboisson == "2") {
          faireboisson("Cappuccino", 6, 0.1, 2.5)
        }
        else if (choixboisson == "3") {
          println("Choisissez la taille de votre Latte : \n 1) Petit - CHF 2.70 \n 2) Moyen - CHF 3.20 \n 3) Grand - CHF 3.70")
          print("> ")

          val choixtaille = readLine()
          if (choixtaille == "1") {
            faireboisson("Latte Petit", 6, 0.12, 2.7)
          }
          else if (choixtaille == "2") {
            faireboisson("Latte Moyen", 8, 0.15, 3.2)
          }
          else if (choixtaille == "3") {
            faireboisson("Latte Grand", 12, 0.2, 3.7)
          }
          else {
            println("Choix invalide")
          return}}}

        def faireboisson(nomboisson: String, cafe: Int, lait: Double, prixbase: Double): Unit = {

          // quantité sucre
          println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          val choixsucre = readLine()

          // sucre variable
          var sucrequantite = 0
          var prixsucre = 0.0
          if (choixsucre == "1") {
            sucrequantite = 0
          } else if (choixsucre == "2") {
            sucrequantite = 5
            prixsucre = 0.1
          } else if (choixsucre == "3") {
            sucrequantite = 10
            prixsucre = 0.2
          } else if (choixsucre == "4") {
            sucrequantite = 15
            prixsucre = 0.3
          } else {
            println("Choix de sucre invalide.")
            return
          }

          var prixlaitsupplementaire = 0.0
          var laitquantite = 0.0

          if (nomboisson == "Cappuccino" || nomboisson == "Latte Petit" || nomboisson == "Latte Moyen" || nomboisson == "Latte Grand") {
            println("Voulez-vous ajouter du lait supplémentaire ? (0.05 CHF par dose) \n 1) Oui \n 2) Non")
            print("> ")
            val choixlait = readLine()

            if (choixlait == "1") {
              println("Combien de dose (3 maximum)?")
              print("> ")
              val laitsupplementaire = readLine()

              if (laitsupplementaire == "1") {
                laitquantite = 0.05
                prixlaitsupplementaire = 0.05
              }
              else if (laitsupplementaire == "2") {
                laitquantite = 0.10
                prixlaitsupplementaire = 0.10
              }
              else if (laitsupplementaire == "3") {
                laitquantite = 0.15
                prixlaitsupplementaire = 0.15
              }
              else {
                println("Choix de lait supplémentaire invalide.")
                return false
              }}}

          //En fonction des stocks
          if (laitstock >= (lait + laitquantite) && sucrestock >= sucrequantite && cafestock >= cafe) {
            cafestock = cafestock - cafe
            sucrestock = sucrestock - sucrequantite
            laitstock = laitstock - (lait + laitquantite)

            val prixfinal = prixbase + prixsucre + prixlaitsupplementaire
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixbase, prixsucre, prixlaitsupplementaire, prixfinal)

            // TWINT
            val alphanumeric = "0123456789abcdefghijklmnopqrstuvwxyz"
            var code = ""
            for (_ <- 1 to 5) {
              code = code + alphanumeric(Random.nextInt(alphanumeric.length))}

            printf("Code Twint : %s\n", code)
            println("(Attende de validation du paiement)")
            Thread.sleep(3000)
            println("Merci! Votre paiement a été accepté.")
            println("Préparation de votre boisson... \n[...]")
            Thread.sleep(5000)
            printf("Votre %s est prête ! Bonne dégustation !\n", nomboisson)
            Thread.sleep(1000)
            faisabiliteboisson = true
            continuer = true}
          else {
            if (cafestock < cafe) println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            if (sucrestock < sucrequantite) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin. ")
            if (laitstock < (lait + laitquantite)) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          faisabiliteboisson = false}}}

//-------------------------------------------------------------------------------

      else if (choixmode == "2") {
        println("veuiller entrer le code PIN :")
        print("> ")
        val pin = readLine()
        if (pin == codePIN) {
          println("Accès autorisé")
          printf("Stock de café : %d g \n Stock de sucre : %d g \n Stock de lait : %.2f L \n", cafestock, sucrestock, laitstock)

          println("Combien de café ajouter ?")
          print("> ")
          cafestock = cafestock + readLine().toInt
          println("Combien de sucre ajouter ?")
          print("> ")
          sucrestock = sucrestock + readLine().toInt
          println("Combien de lait ajouter (en litres) ?")
          print("> ")
          laitstock = laitstock + readLine().toDouble
          printf("Stock de café : %d g \n Stock de sucre : %d g \n Stock de lait : %.2f L \n ", cafestock, sucrestock, laitstock)}
        else {
          println("Pin incorrect, accès refusé")}}

//--------------------------------------------------------------------
      else if (choixmode == "3") {
        continuer = false}
      else {println("Choix invalide, réessayer")}}}}
