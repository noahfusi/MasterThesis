import scala.util.Random
import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    var continue = true //on déclare les variables de bases
    var sucrest = 30
    val pincode =  434343
    var cafest = 50
    var laitst = 500
    while(continue){ //tant qu'on continue (que le choixmenu n'est pas egal à 3)
      val laitenlitres = laitst*0.001
      var choixmenu = 0
      while(!(choixmenu ==1 || choixmenu ==2 ||choixmenu==3)){ //on demande le choix du menu
        print("\tNospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
        choixmenu =readInt()
      }
      if(choixmenu==2){ //cas menu admin
        println("Mode Admin")
        var pincodeuser = 0
        while(pincodeuser!=pincode){ //on demande à l'utilisateur de mettre un mot de passe
          print("Entrez le code PIN :")
          pincodeuser= readInt()
        } //si le code est le bon, il accède à l'inventaire du stock
        println(f"Stocks:\nPoudre de café: "+cafest+f"g\nLait : $laitenlitres%.2fL\nSucre : "+sucrest+"g")
        println("Réapprovisionnement des stocks...\nAjout :")
        print("Poudre de café: " ) // on demande ce que l'utilisateur veut ajouter au stock
        cafest = cafest + readInt()
        print("Lait : ")
        laitst = laitst +  (readFloat()*1000).toInt
        print("Sucre : ")
        sucrest = sucrest + readInt()
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      }
      if(choixmenu == 3){ //cas ou l'utilisateur quitte
        continue = false
      }
      while (choixmenu == 1){ //cas menu client
        var quantite_sucre = 0// variables de base du menu client à reset au début de chaque commande
        var sucre_lettre = ""
        var cout_cafe = 0 //cout en quantité
        var cout_lait = 0
        var cout_sucre = 0
        var nom_boisson = ""
        var prix_boisson = 0.0
        var prix_sucre = 0.0
        var prix_lait = 0.0
        var prix_tot = 0.0
        var choix_boisson = 0
        while(!(choix_boisson==1 || choix_boisson ==2 || choix_boisson ==3)){ //on demande un choix dans les limites imposées
          print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
          choix_boisson = readInt()
        }
        if(choix_boisson == 1){ //pour chaque boisson, on attribue les variables correspodantes

          prix_boisson = 2.00
          nom_boisson = "Expresso"
          cout_cafe = 8

        }
        else if(choix_boisson==2){
          nom_boisson = "Cappuccino"
          prix_boisson = 2.5
          cout_cafe =6
          cout_lait =100

        } else if(choix_boisson==3){ //pour le latte on demande la taille d'abord
          var taille_latte = 0
          while(!(taille_latte==1 || taille_latte ==2 || taille_latte == 3)) { //dans les limites
            print("Quelle taille souhaitez vous pour votre Latte ?  \n1) Petit - CHF 2.70  \n2) Moyen - CHF 3.20   \n3) Grand - CHF 3.70   \n>")
            taille_latte = readInt()
          }
          if(taille_latte==1){ //on applique pour chaque taille les valeurs correspondantes
            prix_boisson = 2.7
            cout_lait =120
            cout_cafe = 6
            nom_boisson = "Latte (Petit)"
          }
          if(taille_latte==2){
            cout_lait = 150
            prix_boisson = 3.2
            cout_cafe =8
            nom_boisson = "Latte (Moyen)"
          }
          if(taille_latte == 3){
            prix_boisson = 3.7
            cout_cafe =12
            cout_lait = 200
          }
        }
        while(!(quantite_sucre==1||quantite_sucre==2||quantite_sucre==3||quantite_sucre==4)){ //on demande la quantité de sucre que l'on veut
          print("Souhaitez-vous ajouter du sucre ?  \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n>")
          quantite_sucre = readInt()
        }
        prix_sucre = (quantite_sucre - 1)*0.1 //le calucl du prix du sucre
        if(quantite_sucre == 1){ // on donne le string correspondant à quantité
          sucre_lettre = "Sans sucre"
          cout_sucre = 0
        } else if(quantite_sucre == 2){
          cout_sucre = 5
          sucre_lettre = "Peu (5g)"
        } else if(quantite_sucre == 3){
          cout_sucre = 10
          sucre_lettre = "Moyen (10g)"
        } else if (quantite_sucre == 4){
          cout_sucre = 15
          sucre_lettre = "Beaucoup (15g)"
        }

        if(choix_boisson ==2 || choix_boisson == 3 ){ //si latte ou cappuccino
          var lait_plus = 0
          while (!(lait_plus==1||lait_plus==2)){ //on demande si il faut mettre du lait en +
            print("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui \n2) Non \n>")
            lait_plus = readInt()
          }
          if(lait_plus == 1){// si oui
            var dose_lait = 0
            while(!(dose_lait==1 ||dose_lait==2 || dose_lait==3)){ // on demande la dose à mettre
              dose_lait = readLine("Combien de doses ?(max 3) \n>").toInt
            }
            cout_lait =cout_lait + 50 * dose_lait // on calcul la quantité du lait
            prix_lait = dose_lait * 0.05 // et le prix
          }

        }
        if(cout_cafe>cafest){ // on test si il n'y a pas des erreures et on remet à zéro les variables qui sont propres à cette iteration
          cout_lait = 0

          cout_cafe = 0

          cout_sucre = 0
          choixmenu = 1
          print ("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.\n")
        } else if(sucrest<cout_sucre){
          cout_lait = 0

          cout_cafe = 0

          cout_sucre = 0
          choixmenu = 1
          println( "Erreur : Quantité de poudre de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")

        } else if(cout_lait > laitst && choix_boisson == 3){
          println ("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
          cout_lait = 0

          cout_cafe = 0

          cout_sucre = 0
          choixmenu = 1
        } else if(cout_lait > laitst && choix_boisson != 3){
          println("Erreur : Quantité  de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
          cout_lait = 0

          cout_cafe = 0

          cout_sucre = 0
          choixmenu = 1
        } else { // si tout vas bien on continue avec le payement
          prix_tot = prix_boisson + prix_sucre +prix_lait
          println( f"Prix total : CHF $prix_boisson%.2f + CHF $prix_sucre%.2f + CHF $prix_lait%.2f = CHF $prix_tot%.2f" )
          val alphabet_num : String = "ABCDEFGHIKLMNOPQRSTUVWXYZ0123456789"
          var i = 1
          var codetwint = ""
          while(i <= 5){
            val index = (Math.random() * 36).toInt
            codetwint += alphabet_num(index)
            i = i + 1
          }
          print(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codetwint\n(En attente de validation du paiement...)\n")
          Thread.sleep(3000)

          print("Merci ! Votre paiement a été accepté.\n")
          print( "préparation de votre boisson...\n")
          Thread.sleep(5000)

          print( s"Votre $nom_boisson est prêt ! Bonne dégustation !\n" )
          choixmenu= 0
          cafest = cafest-cout_cafe //on enlève du stock les quantitées de cette itération
          laitst = laitst-cout_lait
          sucrest = sucrest-cout_sucre
        }
      }
    }
  }
}
