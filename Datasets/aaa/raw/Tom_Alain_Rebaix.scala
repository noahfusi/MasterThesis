import io.StdIn._
import math._
object Main {
  def main(args: Array[String]): Unit = {
    print("\n\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
    var entre = readInt()
    var code = 0
    var choix = 0
    var lait = 0
    var sucre = 0
    var choixlatte = 0
    var poudrecafe = 50
    var stocksucre = 30
    var stocklait = 0.500
    var prix = 0.00
    var prixcafe = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00
    var aleatoire = 0
    var choixlait = 0
    var laitboucle = false

    while (entre != 3) {
      if (entre == 1) { // client


        sucre = 0
        lait= 0
        prix = 0
        choixlait = 0
        prixcafe = 0
        prixlait = 0
        prixsucre = 0
        laitboucle = false

        while (choix != 1 && choix != 2 && choix != 3) { // choix boisson
          print("Veuillez sélectionner une boisson: \n1) Expresso - 2.00 CHF \n2) Cappucino - 2.50 CHF \n3) Latte - Petit 2.70 CHF, Moyen 3.20 CHF, Grand 3.70 CHF \n >")
          choix = readInt()
           if (choix == 3) {
            print("1) petit - 2.70 CHF  \n2) moyen - 3.20 CHF \n3) grand - 3.70 CHF \n >") // choix des différents latte
            choixlatte = readInt()
          }
        }
        while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {  // choix sucre
          print("Quel quantité de sucre :\n 1) pas de sucre\n 2) peu de sucre, 5g - 0.10 CHF  \n 3) moyen, 10g - 0.20 CHF \n 4) Beaucoup, 15g - 0.30 CHF \n >")
          sucre = readInt()
          if ( choix == 1){
            laitboucle = true
          }}
        while (!laitboucle ){
        if (choix == 2 || choix ==3){
          print("Vous voulez ajouter une dose supplémentaire de lait ? \n1) oui \n2) non \n >")
          choixlait = readInt()}
          if (choixlait == 1) {
            while (lait != 1 && lait != 2 && lait != 3) { // choix lait
              laitboucle = true
              choixlait = 0
          print("Quel quantité de lait ? (maximun 3 doses)\n >")
          lait = readInt()}}
          else if (choixlait == 2){
            laitboucle = true
          }

        }
        if (choix == 1 && poudrecafe >= 8) { // choix expresso
          poudrecafe = poudrecafe - 8
          prixcafe = 2.00
          println("Boisson sélectionée: Expresso")}

          else if (poudrecafe < 8  && choix == 1) {
            println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n")
            choix =  0
            sucre = 0
            lait= 0
            prix = 0
            choixlait = 0
            prixcafe = 0
            prixlait = 0
            prixsucre = 0
            laitboucle = false
          }

        if (choix == 2  && (poudrecafe >= 6 && stocklait >=0.1 )) { // choix cappucino
          poudrecafe = poudrecafe - 6
          stocklait = stocklait - 0.1f
          println("Boisson sélectionée: Cappucino")
          prixcafe = 2.50 }

        else if (poudrecafe < 6 && choix == 2) {
          println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
          choix = 0
          sucre = 0
          choix = 0
          lait= 0
          prix = 0
          choixlait = 0
          prixcafe = 0
          prixlait = 0
          prixsucre = 0
          laitboucle = false
           }
            else if  (stocklait < 0.1 && choix == 2) {
          println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
          choix = 0
          sucre = 0
          choix = 0
          lait= 0
          prix = 0
          choixlait = 0
          prixcafe = 0
          prixlait = 0
          prixsucre = 0
          laitboucle = false
        }

          if (choixlatte == 1 && (poudrecafe >= 6 && stocklait >= 0.12)) { //lattes
            poudrecafe = poudrecafe - 6
            stocklait = stocklait - 0.12
            prixcafe =  2.70
            println("Boisson sélectionée: Petit latte")
          }

          if (choixlatte == 2 && (poudrecafe >= 8 && stocklait >= 0.15)) {
            poudrecafe = poudrecafe - 8
            stocklait = stocklait - 0.15
            println("Boisson sélectionée: Moyen latte")
            prixcafe =  3.20
            prix = prixcafe
          }
          else if (choixlatte == 3 && (poudrecafe >= 12 && stocklait >= 0.200 )) {
            poudrecafe = poudrecafe - 12
            stocklait = stocklait - 0.200
            prixcafe =  3.70
            println("Boisson sélectionée: Grand latte ")
          }
          else if ((choixlatte == 3 && poudrecafe < 12) || (choixlatte == 2 && poudrecafe < 8) || (choixlatte == 1 && poudrecafe < 6)) {
            println("Erreur: Quantité de poudre de café insuffisante\n Veuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
            sucre = 0
            choix = 0
            lait= 0
            prix = 0
            choixlait = 0
            prixcafe = 0
            prixlait = 0
            prixsucre = 0
            laitboucle = false
          }
          else if  ((choixlatte == 1  && stocklait < 0.12) || (choixlatte == 2  && stocklait < 0.15) ||  (choixlatte == 3  && stocklait < 0.200)){
          println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
          choix = 0
          sucre = 0
          choix = 0
          lait= 0
          prix = 0
          choixlait = 0
          prixcafe = 0
          prixlait = 0
          prixsucre = 0
          laitboucle = false
          choixlatte = 0

        }
        if (sucre == 1){ // sucre
            println ("Quantité de sucre : pas de sucre")
             }
            else if (sucre == 2 && stocksucre >= 5 ){
              stocksucre = stocksucre - 5
              prixsucre = 0.10
              println ("Quantité de sucre : peu (5g)")

            }
            else if (sucre == 3 && stocksucre >= 10){
              stocksucre = stocksucre - 10
              prixsucre = 0.20
              println ("Quantité de sucre : moyen (10g) ")
            }

            else if (sucre == 4 && stocksucre >= 15) {
              stocksucre = stocksucre - 15
              prixsucre = 0.30
              println ("Quantité de sucre : beaucoup (15g)")
            }
            else if (sucre == 4 && stocksucre < 15 || sucre == 3 && stocksucre < 10 ||sucre == 2 && stocksucre < 5 ) {
              println("Erreur: Quantité de sucre insuffisante\nVeuillez sélectionner une plus petite quantité de sucre ou contrôler le stock en mode admin\n ")
              choix = 0
              sucre = 0
              sucre = 0
              choix = 0
              lait= 0
              prix = 0
              choixlait = 0
              prixcafe = 0
              prixlait = 0
              prixsucre = 0
              laitboucle = false
              }
        if (choixlait == 2 ){ // lait
          println ("Lait: pas de dose")}

        else if ( lait == 1 && stocklait >= 0.05){
          stocklait = stocklait - 0.05
          prixlait = 0.05
          println ("Lait: Une dose ")}

        else if (lait == 2  && stocklait >= 0.1){
          stocklait = stocklait - 0.1
          prixlait = 0.1
          println("Lait: deux dose")
        }

        else if (lait == 3  && stocklait >= 0.15){
          println ("Lait: trois doses")
          prixlait =  0.15
          stocklait= stocklait - 0.15}

        else if  ((lait == 3 && stocklait < 0.1) || (lait == 2 && stocklait < 0.1) || (lait == 1 && stocklait < 0.01)  ) {
          println("Erreur: Quantité lait insuffisante pour la dose sélectionné \nVeuillez sélectionner une plus petite quantité de dose de lait ou contrôler le stock en mode admin\n ")
          print("Veuillez sélectionner une boisson: \n1) Expresso - 2.00 CHF \n2) Cappucino - 2.50 CHF \n3) Latte - Petit 2.70 CHF, Moyen 3.20 CHF, Grand 3.70 CHF \n >")
          choix = readInt()
          sucre = 0
          lait= 0
          prix = 0
          choixlait = 0
          prixcafe = 0
          prixlait = 0
          prixsucre = 0
          laitboucle = false
        }
        if (prixcafe >0.00 || prixlait > 0.00 || prixsucre > 0.00) { // le prix
          prix = prixcafe + prixlait + prixsucre
          if (prixsucre > 0.00 && prixlait > 0.00){
            printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) + %.2f CHF (lait)\n", prix, prixcafe, prixsucre, prixlait)}
          if (prixsucre > 0.00 && prixlait == 0.00){
            printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) \n", prix, prixcafe, prixsucre)}
          if (prixsucre == 0.00 && prixlait >0.00){
            printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (lait) \n", prix, prixcafe, prixlait)
          }
          if(prixsucre == 0.00 && prixlait == 0.00 ) {
            printf("Le prix de la boisson est de : %.2f CHF (café) \n",prixcafe)
          }

          println ("\nVeuillez procéder au paiement avec Twint")
          print( "Votre code pour le paiment est : "  )
          for ( i<- 0 to 4 ){
            while((aleatoire < 49) || (aleatoire  > 90) || ((aleatoire  < 65) && (aleatoire > 57))){
              aleatoire = (Math.random() * 90).toInt
            }
            print(aleatoire.toChar )
            aleatoire = 0
          }
          println("\nEn attente de la confirmation du paiement...")
          Thread.sleep(3000)
          println ("Paiement validé ! Merci pour votre commande")
          println ("\nVotre commande est cour de préparation...")
          if (choix== 1){
          println ("Voici votre expresso ! ")}
          else if (choix == 2 ){
            println ("Voici votre cappuccino !")
          }
          else if (choix == 3 ){
            println ("Voici votre latte !")
          }
        }
        if (prixcafe > 0){
        choix = 0
        sucre = 0
        lait = 0
        choixlatte = 0
        prix = 0
        choixlait = 0
        prixsucre = 0.00
        prixlait= 0.00
        prixcafe = 0.00
        laitboucle = false
        print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        entre = readInt()
       }}
      else if (entre == 2) {
        println("Mode admin \n Entrez le code = ******")
          code = readInt()
        if (code == 434343) { //stockage
          println ("accès autorisé")
          println ("Quantités de stock:\n1) Poudre de café: " + poudrecafe+"g \n2) Sucre: " + stocksucre+ "g\n3) Lait: " + stocklait + "L" )
          entre = 0
          var poudreajoute =  -1
            while (poudreajoute < 0){
              print(" Quel quantité de poudre à café à rajouter \n >")
              poudreajoute = readInt()
              }
          poudrecafe =  poudrecafe + poudreajoute

          var sucreajoute = -1
          while(sucreajoute < 0){
            print(" Quel quantité de sucre à rajouter \n >")
            sucreajoute = readInt()
          }
          stocksucre = sucreajoute + stocksucre

          var laitajoute = -1
          while ( laitajoute < 0 ){
            print(" Quel quantité de lait à rajouter \n >")
            laitajoute = readInt ()
          }
          stocklait = laitajoute + stocklait
          println ("réapprovisionnement des stocks ...")
          println ("Quantités de stock:\n1) Poudre de café: " + poudrecafe + "g \n2) Sucre: " + stocksucre+ "g\n3) Lait: " + stocklait + "L" )
          print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
          entre = readInt()

        } else {
          println ("Code invalide!")
          print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
          entre = readInt()
        }
      }
      else {
        while (entre != 1 && entre != 2 && entre != 3) {
          println("Sasie non-valide, veuillez réessayer \n ")
          print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n>")
          entre = readInt()
        }}}
  }}