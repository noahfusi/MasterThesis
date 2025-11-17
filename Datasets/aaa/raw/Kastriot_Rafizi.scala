import io.StdIn._
import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    ////////////////  MENU
    var choix_menu = 0 // on intialise à une valeur qu'on sait qu'on ne veut pas
    // Variables pour le mode Client
    var boisson = 0
    var nom_boisson = ""

    var prix_base = 0.0
    var prix_sucre = 0.0
    var prix_lait = 0.0 // selon le nombre de doses

    var prix_total = 0.0

    var sucre_ajout = 0.0 //dose de sucre en grammes rajoutée
    var quantite_sucre = 0.0 //1 sans sucre, 2 peu (5g), 3 moyen(10g) ou 4 beaucoup (15g)
    var quant_sucre_string = "" // sans sucre, peu, moyen ou beaucoup

    var ajouter_lait = 0 //1 pour oui, 2 pour non
    var lait_supp = "" // oui ou non
    var dose_lait = 0 //1,2 ou 3

    var cafe_necessaire = 0
    var lait_necessaire = 0

    var taille_latte = 0 //1 petit, 2 moyen, 3 grand
    var preparation = false //est ce qu'on est dans l'état de préparer la boisson ?

    // stock initial
    var poudre_cafe = 50.0 // 50 grammes
    var sucre = 30.0       //30 grammes
    var lait = 500.0       // 500 mL

    //code admin
    val code_pin_admin = "434343" // par défaut

    do {
      // choisir le menu "Client", "Admin" ou "Quitter" sotcker dans la variable choix_menu
      print("\t\tNospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      choix_menu = readInt()
      while (choix_menu != 1 && choix_menu != 2 && choix_menu != 3) {
        print("Erreur Choix\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
        choix_menu = readInt()
      }

      if(choix_menu==1){
        //mode client
        do{
          //le client choisi la boisson dans la variable boisson
          print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
          boisson = readInt()
          //on le force à choisir l'une des boisson sinon on lui repose la question
          while (boisson != 1 && boisson != 2 && boisson != 3) {
            println("Erreur Choix")
            print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
            boisson = readInt()
          }
          if(boisson == 1){ // cas expresso
            cafe_necessaire = 8
            nom_boisson = "Expresso"
            prix_base = 2.00
            // demande pour le sucre
            do {
              print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
              quantite_sucre = readInt()
              if (quantite_sucre == 1) {
                quant_sucre_string = "Sans sucre"
                sucre_ajout = 0.0
              }
              else if (quantite_sucre == 2) {
                quant_sucre_string = "Peu (5g)"
                sucre_ajout = 5
                prix_sucre = 0.10
              }
              else if (quantite_sucre == 3) {
                quant_sucre_string = "Moyen (10g)"
                sucre_ajout = 10
                prix_sucre = 0.20
              }
              else if (quantite_sucre == 4) {
                quant_sucre_string = "Beaucoup (15g)"
                sucre_ajout = 15
                prix_sucre = 0.30
              }
              else {
                println("Erreur choix")
              }
            } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)

            if(poudre_cafe<cafe_necessaire && sucre < sucre_ajout){ // on a 4 cas
              println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
            else if(poudre_cafe<cafe_necessaire && sucre>=sucre_ajout){
              println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
            else if(poudre_cafe>=cafe_necessaire && sucre<sucre_ajout){
              println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            }
            else{
              preparation = true
              poudre_cafe-=cafe_necessaire
              sucre-=sucre_ajout
            }
          }//fin cas expresso

          else{ // cas cappuccino et latte : boissons nécessitant du lait

            if (boisson == 2){ // cas cappuccino
              cafe_necessaire = 6
              lait_necessaire = 100
              nom_boisson = "Cappuccino"
              prix_base = 2.50

              // demande pour le sucre
              do {
                print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
                quantite_sucre = readInt()
                if (quantite_sucre == 1) {
                  quant_sucre_string = "Sans sucre"
                  sucre_ajout = 0.0
                }
                else if (quantite_sucre == 2) {
                  quant_sucre_string = "Peu (5g)"
                  sucre_ajout = 5
                  prix_sucre = 0.10
                }
                else if (quantite_sucre == 3) {
                  quant_sucre_string = "Moyen (10g)"
                  sucre_ajout = 10
                  prix_sucre = 0.20
                }
                else if (quantite_sucre == 4) {
                  quant_sucre_string = "Beaucoup (15g)"
                  sucre_ajout = 15
                  prix_sucre = 0.30
                }
                else {
                  println("Erreur choix")
                }
              } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)
              //demande de lait
              print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
              ajouter_lait = readInt()
              while (ajouter_lait != 1 && ajouter_lait != 2) {
                print("Erreur choix\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
                ajouter_lait = readInt()
              }
              if(ajouter_lait == 1){
                lait_supp = "Oui"
                print("Combien de dose ? (Max.3)\n> ")
                dose_lait = readInt()
                while (dose_lait != 1 && dose_lait != 2 && dose_lait != 3) {
                  print("Erreur choix !\nEntre 1 et 3 doses !\n> ")
                  dose_lait = readInt()
                }
                lait_necessaire += dose_lait * 50
                prix_lait = dose_lait * 0.05
              }
              else{
                lait_supp = "Non"
              }


              if (poudre_cafe<cafe_necessaire && sucre < sucre_ajout && lait < lait_necessaire){ // on a 8 cas
                println("Quantité de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe<cafe_necessaire && sucre < sucre_ajout && lait >= lait_necessaire){
                println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe<cafe_necessaire && sucre >= sucre_ajout && lait < lait_necessaire){
                println("Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe < cafe_necessaire && sucre >= sucre_ajout && lait >= lait_necessaire){
                println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre < sucre_ajout && lait < lait_necessaire){
                println("Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre < sucre_ajout && lait >= lait_necessaire){
                println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre >= sucre_ajout && lait < lait_necessaire){
                println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else{
                preparation = true
                poudre_cafe-=cafe_necessaire
                sucre-=sucre_ajout
                lait-=lait_necessaire
              }
            } //fin capuccino

            else{ // cas latte
              //demande taille
              print("Quelle taille pour le latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
              taille_latte = readInt()
              while (taille_latte != 1 && taille_latte != 2 && taille_latte != 3) {
                print("Erreur choix\nQuelle taille pour le latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
                taille_latte = readInt()
              }
              if(taille_latte == 1){
                cafe_necessaire = 6
                lait_necessaire = 120
                prix_base = 2.70
                nom_boisson = "Latte (Petit)"
              }
              else if(taille_latte == 2){
                cafe_necessaire = 8
                lait_necessaire = 150
                prix_base = 3.20
                nom_boisson = "Latte (Moyen)"
              }
              else{
                cafe_necessaire = 12
                lait_necessaire = 200
                prix_base = 3.70
                nom_boisson = "Latte (Grand)"
              }

              // demande pour le sucre
              do {
                print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
                quantite_sucre = readInt()
                if (quantite_sucre == 1) {
                  quant_sucre_string = "Sans sucre"
                  sucre_ajout = 0.0
                }
                else if (quantite_sucre == 2) {
                  quant_sucre_string = "Peu (5g)"
                  sucre_ajout = 5
                  prix_sucre = 0.10
                }
                else if (quantite_sucre == 3) {
                  quant_sucre_string = "Moyen (10g)"
                  sucre_ajout = 10
                  prix_sucre = 0.20
                }
                else if (quantite_sucre == 4) {
                  quant_sucre_string = "Beaucoup (15g)"
                  sucre_ajout = 15
                  prix_sucre = 0.30
                }
                else {
                  println("Erreur choix")
                }
              } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)
              //demande de lait
              print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
              ajouter_lait = readInt()
              while (ajouter_lait != 1 && ajouter_lait != 2) {
                print("Erreur choix\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
                ajouter_lait = readInt()
              }
              if(ajouter_lait == 1){
                lait_supp = "Oui"
                print("Combien de dose ? (Max.3)\n> ")
                dose_lait = readInt()
                while (dose_lait != 1 && dose_lait != 2 && dose_lait != 3) {
                  print("Erreur choix !\nEntre 1 et 3 doses !\n> ")
                  dose_lait = readInt()
                }
                lait_necessaire += dose_lait * 50
                prix_lait = dose_lait * 0.05
              }
              else{
                lait_supp = "Non"
              }

              if (poudre_cafe<cafe_necessaire && sucre < sucre_ajout && lait < lait_necessaire){
                println("Quantité de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe<cafe_necessaire && sucre < sucre_ajout && lait >= lait_necessaire){
                println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe<cafe_necessaire && sucre >= sucre_ajout && lait < lait_necessaire){
                println("Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe < cafe_necessaire && sucre >= sucre_ajout && lait >= lait_necessaire){
                println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre < sucre_ajout && lait < lait_necessaire){
                println("Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre < sucre_ajout && lait >= lait_necessaire){
                println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              }
              else if (poudre_cafe>=cafe_necessaire && sucre >= sucre_ajout && lait < lait_necessaire){ // on propose une autre taille que lorsque le café et le sucre sont dispo mais pas le lait (si pas de café alors ça ne sert à rien
                if (taille_latte==1){
                  println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
                }
                else {
                  println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin.\n")
                }
              }
              else{
                preparation = true
                poudre_cafe-=cafe_necessaire
                sucre-=sucre_ajout
                lait-=lait_necessaire
              }
            } // fin latte
          } // fin cas boissons lactées
          //paiement
          if(preparation==true){
            prix_lait = ((prix_lait*100).round)/100.0 // j'utilise cette méthode .round car pour 3 doses de lait, l'imprésicion des calculs donne 0.150000000..2
            if (boisson == 1){ // cas expresso : 2 cas : sucré ou non
              println("\nBoisson sélectionnée : " + nom_boisson)
              println("Niveau de sucre      : " + quant_sucre_string)
              if (quantite_sucre == 1){ // cas sans sucre
                println("Prix total : CHF " + f"$prix_base%.2f")
              }
              else{ // cas sucre
                prix_total = (prix_base+prix_sucre)
                println("Prix total : CHF " + f"$prix_base%.2f" + " + CHF " + f"$prix_sucre%.2f" + " = CHF " + f"$prix_total%.2f")
              }
            }

            else{ // cas des boisson avec lait

              println("\nBoisson sélectionnée : " + nom_boisson)
              println("Niveau de sucre      : " + quant_sucre_string)
              println("Lait supplémentaire  : " + lait_supp)
              if (quantite_sucre == 1){ // pas de sucre !
                if (lait_supp =="Non"){ // cas sans sucre et sans dose de lait supplémentaire
                  println("Prix total : CHF " + f"$prix_base%.2f")
                }
                else{ // si il y a du lait, on rajoute le prix du lait dans le print
                  prix_total = (prix_base+prix_lait)
                  println("Prix total : CHF " + f"$prix_base%.2f" + " + CHF " + f"$prix_lait%.2f" + " = CHF " + f"$prix_total%.2f")
                }
              }
              else{ // cas avec du sucre
                if (lait_supp =="Non"){ //cas sucre sans dose de lait supplémentaire
                  prix_total = (prix_base+prix_sucre)
                  println("Prix total : CHF " + f"$prix_base%.2f" + " + CHF " + f"$prix_sucre%.2f" + " = CHF " + f"$prix_total%.2f")
                }
                else{
                  prix_total = (prix_base+prix_lait+prix_sucre)
                  println("Prix total : CHF " + f"$prix_base%.2f" + " + CHF " + f"$prix_sucre%.2f" + " + CHF " + f"$prix_lait%.2f" + " = CHF " + f"$prix_total%.2f")
                }
              }
            }
            val alpha_num = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            var code_twint = ""
            var indice = (math.random()*36).toInt
            for (i <-1 to 5){
              code_twint += alpha_num(indice)
              indice = (math.random()*36).toInt
            }
            println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_twint)
            println("\n(En attente de paiement...)\n")

            //attente de 5 secondes pour le paiement
            Thread.sleep(3000)
            //////////
            println("Paiement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(5000)
            println("\nVotre " + nom_boisson + " est prêt ! Bonne dégustation !\n")

          }
          else{
            //cas où preparation n'est pas possible car il manque des ingrédients !!
            println("Pas de préparation\n")
          }
        }while(preparation!=true)
      }// fin mode client

      else if(choix_menu ==2){
        //mode admin
        print("Mode Admin\nEntrez le code PIN : ")
        var code_pin = readLine()
        while(code_pin!=code_pin_admin){    // code pin dans la variable code_pin_admin
          print("Erreur\nEntrez le code PIN : ")
          code_pin = readLine()
        }
        println("Accès autorisé.")
        println("\nStocks :")
        println("\tPoudre de café : " + poudre_cafe + " g")
        println("\tLait           : " + (lait/1000.0) + " L")
        println("\tSucre          : " + sucre + " g")

        println("\nRéapprovisionnement des stocks...")
        println("Ajout :")
        print("\tPoudre de café (g) : ")
        poudre_cafe+=readDouble()
        print("\tLait           (L) : ")
        lait += (readDouble()*1000.0)
        print("\tSucre          (g) : ")
        sucre += readDouble()

        //// On affiche les nouveaux stocks

        println("\nNouveau stocks :")
        println("\tPoudre de café : " + poudre_cafe + " g")
        println("\tLait           : " + (lait/1000.0) + " L")
        println("\tSucre          : " + sucre + " g")
        /////
        println("Niveaux de stocks mis à jour.\nRetour au menu principal...\n")
        //fin mode Admin
      }
      else{
        //quittez le programme
        println("Vous quittez le programme de la machine à café...")
      }
      preparation = false
    }while(choix_menu !=3)
    /////////////////////////fin du main\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  }
}