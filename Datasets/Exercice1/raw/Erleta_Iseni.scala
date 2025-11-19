import scala.util.Random
import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var codepin =434343 //déclaration des variables de bases
    var stockcafe = 50
    var stocksucre = 30
    var stocklait = 500
    var menu = 0
    while(menu != 3){ //tant que l'utilisateur de veux pas quitter, on continue à afficher le menu principal
      var supplement = 0 //déclaration des variables qui doivent être reset (le "s" à la fin des variables signifie string)("incr" signifie incrémental)
      var dose = 0
      var choixs = ""
      var sucreprix = 0.0
      var prix = 0.0
      var cafeincr = 0
      var laitincr = 0
      var sucres = ""
      var sucreincr = 0

      while(!(menu>= 1 && menu <= 3)){ //si le choix dans le menu est entre 1 et 3 compris
        menu = readLine ("\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      }
      if(menu == 1){ //cas menu client
        var laitprix = 0.0
        var choix = readLine ("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
        while(!(choix>= 1 && choix <= 3)){ //demande du choix de la boisson
          choix = readLine ("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
        }
        if(choix == 1){ //cas 1 et 2 sont simples, on déclare leurs valeurs propres à chaqu'une
          choixs = "Expresso"
          prix = 2
          cafeincr = cafeincr + 8
        }
        if(choix ==2){
          choixs = "Capuccino"
          cafeincr =cafeincr +6
          laitincr = 100
          prix = 2.5

        }
        if(choix == 3){ //cas latte, on demande la taille du latte
          choixs = "Latte"
          var taille = 0
          while(!(taille>=1 && taille <=3)){ //boucle pour un input juste
            taille = readLine("Quelle taille choisissez vous pour votre Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>").toInt
          }

          if(taille == 1){ //chaque taille a ses valeurs propres
            choixs = choixs + " (Petit)"
            prix = 2.7
            laitincr = 120
            cafeincr = 6
          }
          if (taille == 2){
            choixs = choixs + " (Moyen)"
            prix = 3.2
            laitincr = 150
            cafeincr = 8
          }
          if(taille == 3){
            choixs = choixs + " (Grand)"
            prix = 3.7
            laitincr = 200
            cafeincr = 12
          }
        }
        var nbrsucre = 0
        while(!(nbrsucre>=1 && nbrsucre<=4)){ //on demande la quantité de sucre
          nbrsucre = readLine("Souhaitez-vous ajouter du sucre ?  \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10  \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHF 0.30  \n>").toInt
        }
        if(nbrsucre == 1){//chaque quantité a ses valeurs propres
          sucreincr = 0
          sucreprix = 0
          sucres = "Sans sucre"
        }
        else if (nbrsucre==2){
          sucreincr = 5
          sucreprix = 0.1
          sucres = "Peu (5g)"
        }
        else if (nbrsucre==3){
          sucreincr = 10
          sucreprix = 0.2
          sucres = "Moyen (10g)"
        }
        else if (nbrsucre==4){
          sucreincr = 15
          sucreprix = 0.3
          sucres = "Beaucoup (15g)"
        }
        if(choix != 1){ //si c'est un latte ou un cappuccino

          while(!(supplement>=1 && supplement<=2)){ //on demande le lait supplémentaire
            supplement = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toInt
          }
          if(supplement!=2){

            while(!(dose>=1 && dose<=3)){ // on demande la dose
              dose = readLine("Combien de doses voulez vous ajouter ? (maximum 3)\n>").toInt
            }
            if(dose==1){ //on ajoute selon la dose
              laitprix = 0.05
              laitincr = laitincr + 50

            }
            if(dose==2){
              laitprix = 0.1
              laitincr = laitincr + 100
            }
            if(dose==3){
              laitprix = 0.15
              laitincr = laitincr + 150
            }
          }
        }
        println(s"Boisson sélectionnée : $choixs \nNiveau de sucre : $sucres") //on print les texts

        if(!(choix ==1 ) && supplement == 1){
          println(s"Lait en supplément: Oui, $dose dose(s)")
        }

        if(!(choix ==1 ) && supplement == 2){

          println("Lait en supplément: Non")

        }
        var camarche = true
        if (stockcafe<cafeincr){ //on regarde toutes les erreurs liés aux stock

          laitincr = 0
          cafeincr = 0
          sucreincr = 0
          choix = 1
          camarche = false
          println ("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
        } else if (stocksucre<sucreincr){
          println("Erreur : Quantité de poudre de sucre insuffisante pour\npr´eparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          laitincr = 0
          cafeincr = 0
          sucreincr = 0
          menu = 1
          camarche = false
        } else if(laitincr>stocklait && choix == 3){ // le texte est différent pour le lait selon si la boisson est un latte ou pas
          println ("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
          laitincr = 0
          cafeincr = 0
          sucreincr =0
          menu = 1
          camarche = false
        } else if(laitincr>stocklait && choix != 3){
          println ("Erreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          laitincr = 0
          cafeincr = 0
          sucreincr =0
          menu = 1
          camarche = false
        } else if(camarche==true) { // si il n'y a pas d'erreurs nous pouvons valider la commande
          var prixtot = prix +sucreprix +laitprix
          print ( f"Prix total : CHF $prix%.2f " + f"CHF $sucreprix%.2f + CHF $laitprix%.2f = CHF $prixtot%.2f\n" ) //print du prix
          var codetwint = " " //déclaration du code twint
          val chars_alphanum = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          for(x<-1 to 5){
            val obj =(math.random()*36).toInt
            codetwint+= chars_alphanum(obj)}

          println( s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : "+codetwint+"\n(En attente de validation du paiement...)")
          Thread.sleep( 3000 )

          print("Merci ! Votre paiement a été accepté.\npréparation de votre boisson...\n")
          Thread.sleep(5000)
          println( s"Votre $choixs est prêt ! Bonne dégustation !" )
          stockcafe = stockcafe - cafeincr //on enlève du stock les quantitées utilisées par la boisson
          stocklait = stocklait - laitincr
          stocksucre = stocksucre - sucreincr
          menu = 0
        }
      }
      if(menu == 2){ //menu admin
        var codeutil = 0
        while(codeutil!= codepin){ // on demande le code à l'utilisateur
          codeutil = readLine("Mode Admin\nEntrez le code PIN :").toInt
        } // dès qu'il a le bon code on affiche le stock
        print("Accès autorisé.\n" )
        print("Stocks:\n" )
        println( f"Poudre de café: $stockcafe g\nLait : ${stocklait*0.001}%.2f L\nSucre :"+stocksucre+"g")
        println( "Réapprovisionnement des stocks...\nAjout :")
        stockcafe = stockcafe + readLine("\tPoudre de café:").toInt //on lui demande les valeures à ajouter
        stocklait = stocklait + ((readLine("\tLait :").toFloat)*1000).toInt
        stocksucre = stocksucre + readLine("\tSucre :").toInt
        println( "Niveaux de stock mis à jour.\nRetour au menu principal...")
        menu = 0 //on reviens au menu principal
      }
    }
  }
}