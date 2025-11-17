import io.StdIn._
import scala.util.Random

 object Main {
  def main(args: Array[String]): Unit = {

//    — Les stocks initiaux sont définis comme suit : ´
    var cafe_stock  = 50
    var sucre_stock = 30
    var lait_stock  = 0.5

//    Choix du mode :
    var mode = 0
    while (mode != 3) {
      do mode = readLine("\nNospresso Café\nVeuillez sélectionner votre mode :\n  1) Client\n  2) Admin\n  3) Quitter\n  > ").toInt
      while (mode == 0 || mode > 3)

      if ( mode == 2 ) {  //  Si mode Admin Vérifier PIN puis réapprovisionner
        println("Mode Admin")
        var pin = 0
        do pin = readLine("Entrez le code PIN : ****** \n  > ").toInt
        while (pin != 434343)
        println(s"Accès autorisé.\n\nStocks:\n  Poudre de café: $cafe_stock" + f"g\n  Lait          : $lait_stock%.3f" + s"L\n  Sucre         : $sucre_stock" + "g")
        println("Réapprovisionnement des stocks...\nAjout :")
        cafe_stock  += readLine("  Poudre de café: ").toInt
        lait_stock  += readLine("  Lait          : ").toDouble
        sucre_stock += readLine("  Sucre         : ").toInt
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      }
      else if (mode == 1) { //  Si mode Client -> choisir la boisson
        var choix_boisson = 0
        var boisson = "rien"
        do choix_boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
        while (choix_boisson == 0 || choix_boisson > 3)

//        Initialiser le prix et les quantités nécessaires pour la boisson à préparer et vérifier suffisance des stocks
        var prix_base     = 0.00
        var cafe_boisson  = 0
        var sucre_boisson = 0
        var lait_boisson  = 0.0

        if      (choix_boisson == 1)  {boisson = "Expresso"; prix_base = 2.00;  cafe_boisson = 8}
        else if (choix_boisson == 2)  {boisson = "Cappucino"; prix_base = 2.50;  cafe_boisson= 6; lait_boisson = 0.1}
        else if (choix_boisson == 3)  {boisson = "Latte"  // Si boisson Latte choisir quelle taille
          var taille_latte = 0
          do taille_latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Latte Petit - CHF 2.70\n2) Latte Moyen - CHF 3.20\n3) Latte Grand - CHF 3.70\n> ").toInt
          while (taille_latte == 0 || taille_latte > 3)
          if      (taille_latte == 1)  {boisson += " (Petit)"; prix_base = 2.70; cafe_boisson = 6; lait_boisson = 0.120}
          else if (taille_latte == 2)  {boisson += " (Moyen)"; prix_base = 3.20; cafe_boisson = 8; lait_boisson = 0.150}
          else if (taille_latte == 3)  {boisson += " (Grand)"; prix_base = 3.70; cafe_boisson = 12; lait_boisson = 0.200}
        }

        //  Supplément sucre ?
        var supp_sucre = 0
        var prix_sucre = 0.0
        var niveau_sucre = "Sans sucre"
        do supp_sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while (supp_sucre == 0 || supp_sucre > 4)
        if      (supp_sucre == 2)  {niveau_sucre = "Peu (5g)"; sucre_boisson = 5; prix_sucre = 0.10}
        else if (supp_sucre == 3)  {niveau_sucre = "Moyen (10g)"; sucre_boisson = 10; prix_sucre = 0.20}
        else if (supp_sucre == 4)  {niveau_sucre = "Beaucoup (15g)"; sucre_boisson = 15; prix_sucre = 0.30}

        //  Supplément lait ?
        var str_lait = "Non"
        var prix_lait = 0.0
        if (choix_boisson == 2 || choix_boisson == 3) { //  (uniquement pour Cappuccino ou Latte)
          var supp_lait = 0
          do supp_lait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
          while (supp_lait != 1 && supp_lait != 2)
          if (supp_lait == 1)  {
            supp_lait = 0
            do supp_lait = readLine("Combien de doses ? (maximum 3)\n> ").toInt
            while (supp_lait == 0 || supp_lait > 3)
            str_lait = s"Oui - $supp_lait dose(s)"
            lait_boisson += supp_lait * 0.050
            prix_lait = supp_lait * 0.05
          }
        }

//        Résumé de la commande
        println(s"\nBoisson sélectionnée  : $boisson\nNiveau de sucre       : $niveau_sucre\nLait supplémentaire   : $str_lait")

//        Si Stock insuffisant pour la boisson  -> erreur
        var error = false
        var stock_insuf = ""
        if      (cafe_boisson > cafe_stock)   {error = true; stock_insuf += " -poudre de café-"}
        else if (lait_boisson > lait_stock)   {error = true; stock_insuf += " -lait-"}
        else if (sucre_boisson > sucre_stock) {error = true; stock_insuf += " -sucre-"}

        if (error) println(s"\nErreur : Quantité de$stock_insuf pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.\n")

//        Sinon si stock suffisant procéder au payement
        else {
          val code_paiement = Random.shuffle("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").take(5)  // générer code de paiement aléatoire
          println(f"Prix total : CHF $prix_base%.2f + CHF ${prix_sucre + prix_lait}%.2f = CHF ${prix_base + prix_sucre + prix_lait}%.2f\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : $code_paiement\n(En attente de validation du paiement...)\n[...]")
          Thread.sleep(5000)
          println("\nMerci ! Votre paiement a été accepté.\nPréparation de votre boisson...\n[...]")
          Thread.sleep(5000)
          cafe_stock  -= cafe_boisson
          sucre_stock -= sucre_boisson
          lait_stock  -= lait_boisson
          println(s"Votre $boisson est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
        }
      }

//        Si mode Quitter -> quitter sans rien faire
      else if (mode == 3) println("Nospresso vous remercie pour votre visite.\nAu revoir !")
    }
  }
}
