import io.StdIn._
import math._

object Main {
  def main(args: Array[String]): Unit = {
    // premiere partie interface utilisateur

    var poudrecafé = 50
    var sucreingrediant = 30
    var laitingrediant = 0.500
    println(" Nespresso café")
    println("Veuillez selectionner votre mode ")
    println("1)Client ")
    println("2)Admin")
    println ("3)Quitter ")
    var mode = readLine(">").toInt
    // mode Quitter
    if ( mode == 3 ){
      println("merci de votre visite au revoir et a la prochaine ")
     //deuxieme partie mode client
    }else if ( mode == 1){

      val symbol = "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN0123456789"
      var codeTwint = ""
      for ( i <-1 to  5){
        val choixsymboles = (math.random() * 62).toInt
        codeTwint += symbol(choixsymboles )
      }
      println("Veuillez sélectionner votre boisson :")
      println ("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println ("3) Latte - CHF 2.70 (petit), CHF 3.20 (moyen), CHF 3.70 (grand)")
      var boisson = readLine(">").toInt
      if ((boisson == 1 )&&(poudrecafé < 8)) {
        println("Erreur ingrédients insuffisant ")
      }
      println("souhaitez vous rajouter du sucre ?")
      println("1) sans sucre ")
        println("2) peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        var sucre = readLine(">") .toInt
      if ((boisson == 1 ) && (sucre == 1)){
        var prix = 2.00
        println("le prix de votre commande est :" + prix )
        println("veuillez payer en utilisant Twint.")
        println("votre code paiement est :" + codeTwint)
        Thread.sleep(5000)
        println("Merci ! votre paiement a été accepté")
        println(" préparation de votre Expresso ")
        Thread.sleep(5000)
        println("votre Expresso est prêt ! Bonne dégustation !")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("11&1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }else if ((boisson == 1 ) && (sucre == 2)){
        var prix =2.10
        println("le prix de votre commande est :" + prix )
        println("veuillez payer en utilisant Twint.")
          println("votre code paiement est :" + codeTwint)
        Thread.sleep(5000)
        println("Merci ! votre paiement a été accepté")
        println(" préparation de votre Expresso ")
        Thread.sleep(5000)
        println("votre Expresso est prêt ! Bonne dégustation !")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }else if ((boisson == 1 ) && (sucre == 3)){
        var prix = 2.20
        println("le prix de votre de commande est :" +prix )
        println("veuillez payer en utilisant Twint.")
          println("votre code paiement est :" + codeTwint)
        Thread.sleep(5000)
        println("Merci ! votre paiement a été accepté")
        println(" préparation de votre Expresso ")
        Thread.sleep(5000 )
        println("votre Expresso est prêt ! Bonne dégustation !")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }else if ((boisson == 1) && (sucre == 4)){
        var prix = 2.30
        println("le prix de votre commande est :" + prix )
        println("veuillez payer en utilisant Twint.")
          println("votre code paiement est :" + codeTwint)
        Thread.sleep(5000)
        println("Merci ! votre paiement a été accepté")
        println(" préparation de votre Expresso ")
        Thread.sleep(5000)
        println("votre Expresso est prêt ! Bonne dégustation !")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }
      if(boisson == 2){
        println ("Souhaitez vous ajouter du lait en supplément ? ")
        println("1) Oui")
        println("2) Non ")
        var lait = readLine(">").toInt
        if ( lait == 1 ){
          println("Combien de doses de lait supplémentaires souhaitez vous ?")
          println("1) Une dose (50ml) - CHF 0.5")
          println("2) Deux doses (100ml) - CHF 1.0")
          println("3) Trois doses (150ml)-CHF 1.5")
          var dose = readLine(">") .toInt
          // prix pour chaque situations
          // cas 1
          if (sucre == 1){
            var prix = 2.50
            println("le prix de votre commande est :" + prix)
            println("veuillez payer en utilisant Twint.")

              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappucino")
            Thread.sleep(5000)
            println("votre Cappucino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt

          }else if (sucre ==2 ) {
            var prix = 2.60
            println("le prix de votre commande est : " + prix )
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappuccino ")
            Thread.sleep(5000)
            println("votre Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }else if (sucre == 3 ){
            var prix = 2.70
            println("le prix de votre commande est :" + prix )
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Latte ")
            Thread.sleep(5000)
            println("votre Latte est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }else{
            var prix = 2.80
            println("le prix de votre commande est :" + prix)
            println("veuillez payer en utilisant Twint.")

              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappuccino ")
            Thread.sleep(5000)
            println("votre Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt

          }
        }else{
          if (sucre == 1) {
            var prix = 2.50
            println("le prix de votre commande est :" + prix)
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappucino ")
            Thread.sleep(5000)
            println("votre Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }else if ( sucre == 2 ){
            var prix = 2.60
            println ("le prix de votre commande est :" + prix )
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Latte ")
            Thread.sleep(5000)
            println("votre  Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }else if ( sucre == 3){
            var prix = 2.70
            println("le prix de votre commande est :" + prix )
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappucino ")
            Thread.sleep(5000)
            println("votre Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }else {
            var prix = 2.80
            println("le prix de votre commande est :" + prix)
            println("veuillez payer en utilisant Twint.")
              println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
            println(" préparation de votre Cappuccino ")
            Thread.sleep(5000)
            println("votre Cappuccino est prêt ! Bonne dégustation !")
            Thread.sleep(5000)
            println(" Nespresso café")
            println("Veuillez selectionner votre mode ")
            println("1)Client ")
            println("2)Admin")
            println ("3)Quitter ")
            var mode = readLine(">").toInt
          }
        }
      }else if (boisson == 3) {
        println("Souhaitez vous ajouter du lait en supplément ?")
        println("1) Oui")
        println ("2) Non ")
        var lait = readLine(">").toInt
        if (lait == 1 ){
        println("combien de doses souhaiter vous ajouter ?")
        println("1) Une dose (50ml)")
        println("2) Deux doses (100ml)")
        println("3) Trois doses (150ml)")
        var dose = readLine(">").toInt
        }
        println("Quelle taille souhaitez vous ?")
        println("1) Petit")
        println("2) Moyen ")
        println("3) Grand ")
        var taille = readLine(">") .toInt

        if ((sucre == 1 ) && (taille == 1)){
          var prix = 2.70
          println("le prix de votre commande est :" + prix)
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 1 ) &&  (taille == 2)){
          var prix = 3.20
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 1 ) && (taille == 3)){
          var prix = 3.70
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 2 ) && (taille == 1)){
          var prix = 2.80
          println("le prix de votre commande est :" + prix)
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 2 ) && (taille == 2 )){
          var prix = 3.30
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 2) && (taille == 3)){
          var prix = 3.80
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 3) && (taille == 1 )){
          var prix = 2.90
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 3 ) && (taille == 2)){
          var prix = 3.40
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 4) && (taille == 1 )){
          var prix = 3.00
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 4) && (taille == 2)){
          var prix = 3.50
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
          Thread.sleep(5000)
          println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
        }else if ((sucre == 4) && (taille == 3)){
          var prix = 4.00
          println("le prix de votre commande est :" + prix )
          println("veuillez payer en utilisant Twint.")
            println("votre code paiement est :" + codeTwint)
            Thread.sleep(5000)
            println("Merci ! votre paiement a été accepté")
          println(" préparation de votre Latte ")
          Thread.sleep(5000)
          println("votre Latte est prêt ! Bonne dégustation !")
          Thread.sleep(5000)
          println(" Nespresso café")
          println("Veuillez selectionner votre mode ")
          println("1)Client ")
          println("2)Admin")
          println ("3)Quitter ")
          var mode = readLine(">").toInt
          }
        }

      }else if (mode == 2){
      val codeadmin = 434343
      println("veuillez saisir le code PIN")
      var codesaisi = readLine(">").toInt
      if((codesaisi != codeadmin )){
        println("Erreur le code saisi le correspond pas ")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }else{
        println(" vous etes autorisé ")
       println ("le stock restant est: "   )
      println(" pourdre de café:" + poudrecafé )
      println(" sucre:" + sucreingrediant )
      println(" lait :" + laitingrediant )
      println(" Quel stock souhaitez vous ravitailler ?")
      println("1) pourdre de café")
      println("2) lait ")
      println("3) sucre")
      var ravitaillement = readLine (">").toInt
      println("restockage en cours veuillez patienter ")
        Thread.sleep(5000)
      println("stock plein restockage terminé")
        Thread.sleep(5000)
        println(" Nespresso café")
        println("Veuillez selectionner votre mode ")
        println("1)Client ")
        println("2)Admin")
        println ("3)Quitter ")
        var mode = readLine(">").toInt
      }

    }


  }

}