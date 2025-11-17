import io.StdIn._
import util._

object Main {
  def main(args: Array[String]): Unit = {
    //Fins de boucles
    var fin : Boolean = false
    var success :Boolean = false
    var mode = 0
    var cafe = 0
    var sucre = 0
    var lait = 0
    var dose = 0
    var taille = 0
    //stock disponible
    var stockcafe = 50
    var stocklait = 500.0
    var laitL = stocklait/1000
    var stocksucre = 30
    //qte utilisée
    var varsucre = 0
    var varlait = 0.0
    var varcafe = 0
    //prix des éléments
    var prixsucre = 0.0
    var prixcafe = 0.0
    var prixlait = 0.0
    var prixdose = 0.05
    //boisson sélectionnée
    var boisson = ""
    // Quantité de sucre
    var qtesucre = ""
    //Supplément lait
    var laitsupp = ""
    //Twint
    var twint = ""
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    //Admin
    val PIN = 434343
    var code = 0


    while (!fin){
      do {
        //remise à zéro
        success = false
        //Menu principal
        print("        Nospresso Café\n" +
          "Veuillez sélectionner votre mode :\n" +
          "1) Client\n" +
          "2) Admin\n" +
          "3) Quitter\n" +
          ">")
        //entrée de l'utilisateur
        mode = readInt()
        //validation de l'entrée
        if ((mode < 1) || (mode > 3)) {
          println("Votre entrée ne correspond pas aux options.")
        }
      } while ((mode < 1) || (mode > 3))
      //Client
      if (mode == 1)
      {
        //Boucle en cas de problème de stock
        while(!success)
        {do {
          print("Veuillez sélectionner votre boisson :\n" +
            "1) Expresso - CHF 2.00\n" +
            "2) Cappuccino - CHF 2.50\n" +
            "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n" +
            ">")
          cafe = readInt()
          //validation des entrées
          if ((cafe < 1) || (cafe > 3)) {
            println("Votre entrée ne correspond pas aux options.")
          }
        } while ((cafe < 1) || (cafe > 3))
          //Expresso
          if (cafe == 1){
            boisson = "Expresso"
            varcafe = 8
            //vérification stockcafe. Attention, pas possible de retourner au mode Admin selon les instructions
            if (varcafe > stockcafe)
            {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n" +
              "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            else {
              //Choix sucre
              do {
                print("Souhaitez-vous ajouter du sucre ?\n" +
                  "1) Sans sucre\n" +
                  "2) Peu (5g) - CHF 0.10\n" +
                  "3) Moyen (10g) - CHF 0.20\n" +
                  "4) Beaucoup (15g) - CHF 0.30\n" +
                  ">")
                sucre = readInt()
                if ((sucre < 1) || (sucre > 4)){
                  println("Votre entrée ne correspond pas aux options.")
                }
              } while ((sucre < 1) || (sucre > 4))
              if (sucre == 1){
                success = true
                qtesucre = "Pas de sucre"
              }
              else if (sucre == 2){
                varsucre = 5
                if (varsucre > stocksucre){
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                }
                else {
                  qtesucre = "Peu (5g)"
                  prixsucre = 0.10
                  success = true
                }
              }

              else if (sucre == 3){
                varsucre = 10
                if (varsucre > stocksucre)
                {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                }
                else {
                  qtesucre = "Moyen (10g)"
                  prixsucre = 0.20
                  success = true
                }
              }
              else if (sucre == 4){
                varsucre = 15
                if (varsucre > stocksucre){
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                }
                else {
                  qtesucre = "Beaucoup (15g)"
                  prixsucre = 0.30
                  success = true
                }
              }
              if (success){
                if (sucre>1){
                  prixcafe = 2
                printf("Boisson sélectionnée : " + boisson + "\n" +
                  "Niveau de sucre : " + qtesucre + "\n" +
                  "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixcafe+prixsucre)
                println("\n")}
                else {
                  prixcafe = 2
                  printf("Boisson sélectionnée : " + boisson + "\n" +
                    "Niveau de sucre : " + qtesucre + "\n" +
                    "Prix total : CHF %.2f", prixcafe)
                  println("\n")}
              }

            }
          }
          else if (cafe == 2){
            boisson = "Cappuccino"
            varcafe = 6
            varlait = 100.0
            if (varcafe>stockcafe){println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n" +
              "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            varcafe = 0
            }
            else if (varlait>stocklait){
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
                "Veuillez choisir une autre boisson.")
              varlait = 0.0
            }
            else {
              do {
                print("Souhaitez-vous ajouter du sucre ?\n" +
                  "1) Sans sucre\n" +
                  "2) Peu (5g) - CHF 0.10\n" +
                  "3) Moyen (10g) - CHF 0.20\n" +
                  "4) Beaucoup (15g) - CHF 0.30\n" +
                  ">")
                sucre = readInt()
                if ((sucre < 1) || (sucre > 4)){
                  println("Votre entrée ne correspond pas aux options.")
                }
              } while ((sucre < 1) || (sucre > 4))
              if (sucre == 1){
                success = true
                qtesucre = "Pas de sucre"
              }
              else if (sucre == 2){
                varsucre = 5
                if (varsucre > stocksucre){
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
                }
                else {
                qtesucre = "Peu (5g)"
                prixsucre = 0.10
                  success = true
                }
              }

              else if (sucre == 3){
                varsucre = 10
                if (varsucre > stocksucre)
                {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
                }
                else {
                  qtesucre = "Moyen (10g)"
                  prixsucre = 0.20
                  success = true
                }
              }
              else if (sucre == 4){
                varsucre = 15
                if (varsucre > stocksucre){
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
              }
                else {
                  qtesucre = "Beaucoup (15g)"
                  prixsucre = 0.30
                  success = true
                }
            }
              if (success){
                do{
                  print("Souhaitez-vous ajouter du lait en supplément ?\n" +
                    "(Disponible uniquement pour Cappuccino et Latte)\n" +
                    "1) Oui\n" +
                    "2) Non\n" +
                    ">")
                  lait=readInt()
                  if ((lait<1)||(lait>2)){
                    println("Votre entrée ne correspond pas aux options.")
                  }
                } while ((lait<1)||(lait>2))
                if (lait == 1){
                  do {print("Combien de doses ?\n" +
                    ">")
                    dose = readInt()
                    if (dose < 1){println("minimum 1 dose")}
                    else if (dose > 3){println("maximum 3 doses")}
                } while((dose<1)||(dose>3))
                  prixdose *= dose
                  prixlait = prixdose
                  varlait = varlait + (50.0*dose)
                  laitsupp = "Oui"
                  if (varlait > stocklait){
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
                      "Veuillez choisir moins de lait supplémentaire.")
                    success = false
                    varlait = 0.0
                  }
                  }
                else{
                  laitsupp = "Non"
                }
                }
              if (success){
                prixcafe = 2.5
                //prix avec du lait supplémentaire
                if (lait  == 1 ) {
                  if (sucre > 1) {
                    printf("Boisson sélectionnée : " + boisson + "\n" +
                      "Niveau de sucre : " + qtesucre + "\n" +
                      "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
                      "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixlait, prixcafe + prixsucre + prixlait)
                    println("\n")
                  }
                  else {printf("Boisson sélectionnée : " + boisson + "\n" +
                    "Niveau de sucre : " + qtesucre + "\n" +
                    "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
                    "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixlait, prixcafe + prixlait)
                    println("\n")}
                }
                  //prix sans lait supplémentaire
                else {
                  if (sucre > 1){
                    printf("Boisson sélectionnée : " + boisson + "\n" +
                      "Niveau de sucre : " + qtesucre + "\n" +
                      "Lait en supplément : " + laitsupp + "\n" +
                      "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixcafe + prixsucre )
                  println("\n")}
                  else {
                    printf("Boisson sélectionnée : " + boisson + "\n" +
                      "Niveau de sucre : " + qtesucre + "\n" +
                      "Lait en supplément : " + laitsupp + "\n" +
                      "Prix total : CHF %.2f", prixcafe)
                    println("\n")
                  }}
              }
              }

          }
          else if (cafe == 3){
            do {
              boisson = "Latte"
              println("Quelle taille?\n" +
                "1) Petit\n" +
                "2) Moyen\n" +
                "3) Grand\n")
              taille = readInt()
              if ((taille<1)||(taille>3)) {
                println("Votre entrée ne correspond pas aux options.")
              }
            } while ((taille<1)||(taille>3))
            if (taille == 1){
              varcafe = 6
              varlait = 120.0
              prixcafe = 2.7
            }
            else if (taille == 2){
              varcafe = 8
              varlait = 150.0
              prixcafe = 3.2
            }
            else {
              varcafe = 12
              varlait = 200.0
              prixcafe = 3.7
            }
            if (varcafe > stockcafe){
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n" +
              "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              varcafe = 0
            }
            if ((varlait > stocklait)&&(taille > 1)){
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
                "Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              varlait = 0.0
            }
            else if ((varlait > stocklait)&&(taille == 1)){
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
                "Veuillez choisir une autre boisson.")
              success = false
              varlait = 0.0
            }
            else if ((varcafe<stockcafe)||(varlait>stocklait)) {
              do {
                print("Souhaitez-vous ajouter du sucre ?\n" +
                  "1) Sans sucre\n" +
                  "2) Peu (5g) - CHF 0.10\n" +
                  "3) Moyen (10g) - CHF 0.20\n" +
                  "4) Beaucoup (15g) - CHF 0.30\n" +
                  ">")
                sucre = readInt()
                if ((sucre < 1) || (sucre > 4)) {
                  println("Votre entrée ne correspond pas aux options.")
                }
              } while ((sucre < 1) || (sucre > 4))
              if (sucre == 1) {
                success = true
                qtesucre = "Pas de sucre"
              }
              else if (sucre == 2) {
                varsucre = 5
                if (varsucre > stocksucre) {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
                }
                else {
                  qtesucre = "Peu (5g)"
                  prixsucre = 0.10
                  success = true
                }
              }

              else if (sucre == 3) {
                varsucre = 10
                if (varsucre > stocksucre) {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
                }
                else {
                  qtesucre = "Moyen (10g)"
                  prixsucre = 0.20
                  success = true
                }
              }
              else if (sucre == 4) {
                varsucre = 15
                if (varsucre > stocksucre) {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.\n" +
                    "Veuillez choisir moins de sucre.")
                  varsucre = 0
                }
                else {
                  qtesucre = "Beaucoup (15g)"
                  prixsucre = 0.30
                  success = true
                }
              }
              if (success) {
                do {
                  print("Souhaitez-vous ajouter du lait en supplément ?\n" +
                    "(Disponible uniquement pour Cappuccino et Latte)\n" +
                    "1) Oui\n" +
                    "2) Non\n" +
                    ">")
                  lait = readInt()
                  if ((lait < 1) || (lait > 2)) {
                    println("Votre entrée ne correspond pas aux options.")
                  }
                } while ((lait < 1) || (lait > 2))
                if (lait == 1) {
                  do {
                    print("Combien de doses ?\n" +
                      ">")
                    dose = readInt()
                    if (dose < 1) {
                      println("minimum 1 dose")
                    }
                    else if (dose > 3) {
                      println("maximum 3 doses")
                    }
                  } while ((dose < 1) || (dose > 3))
                  prixdose *= dose
                  prixlait = prixdose
                  varlait = varlait + (50.0 * dose)
                  laitsupp = "Oui"
                  if (varlait > stocklait) {
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n" +
                      "Veuillez choisir moins de lait supplémentaire.")
                    success = false
                    varlait = 0.0
                  }
                }
                else {
                  laitsupp = "Non"
                }
              }
              if (success) {
                if (lait == 1) {
                  if (sucre > 1){
                  printf("Boisson sélectionnée : " + boisson + "\n" +
                    "Niveau de sucre : " + qtesucre + "\n" +
                    "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
                    "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixlait, prixcafe + prixsucre + prixlait)
                  println("\n")
                  }
                  else {
                    printf("Boisson sélectionnée : " + boisson + "\n" +
                      "Niveau de sucre : " + qtesucre + "\n" +
                      "Lait en supplément : " + laitsupp + " (" + dose + " doses)\n" +
                      "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixlait, prixcafe + prixlait)
                    println("\n")
                  }
                }
                //prix sans lait supplémentaire
                else {
                  if (sucre > 1){
                  printf("Boisson sélectionnée : " + boisson + "\n" +
                    "Niveau de sucre : " + qtesucre + "\n" +
                    "Lait en supplément : " + laitsupp + "\n" +
                    "Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixcafe, prixsucre, prixcafe + prixsucre)
                  println("\n")
                }
                  else {
                    printf("Boisson sélectionnée : " + boisson + "\n" +
                      "Niveau de sucre : " + qtesucre + "\n" +
                      "Lait en supplément : " + laitsupp + "\n" +
                      "Prix total : CHF %.2f", prixcafe)
                    println("\n")
                  }
                }
              }

            }
          }
        }
        if(success) {
          for (_ <- 1 to 5) {
            twint += caracteres(Random.nextInt(caracteres.length))
          }
          println("Veuillez payer en utilisant Twint.\n" +
            "Votre code de paiement est : " + twint + "\n" +
            "(En attente de validation du paiement...)\n")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Mise à jour des stocks
          stocksucre -= varsucre
          stockcafe -= varcafe
          stocklait -= varlait
          laitL = stocklait/1000
          println("Préparation de votre boisson...\n" +
            "[...]\n" +
            "Votre "+ boisson + " est prêt ! Bonne dégustation !\n")
          //Remise à zéro des valeurs
          varsucre = 0
          varcafe = 0
          varlait = 0.0
          twint = ""
          prixcafe = 0
          prixlait = 0
          prixsucre = 0
        }
      }
      //Admin
      else if (mode == 2){
        while(!success)
        {do {
          print("Mode Admin\n" +
            "Entrez le code PIN : ")
          code = readInt()
          //validation des entrées
          if (code != PIN) {
            println("Votre code est erroné.")
          }
          else {
            println("Accès autorisé.")
          }
        } while (code != PIN)
          println("Stocks:" +
            "Poudre de café: " + stockcafe + "g\n" +
            "Lait : " + laitL + "L\n" +
            "Sucre : " + stocksucre + "g\n")
          println("Réapprovisionnement des stocks...\n" +
            "Ajout :")
          print("Poudre de café: ")
          varcafe = readInt()
          print("Lait : ")
          varlait = readDouble()
          print("Sucre : ")
          varsucre = readInt()
          println("Niveaux de stock mis à jour.\n" +
            "Retour au menu principal...")
          success = true
          stockcafe += varcafe
          stocklait += (varlait*1000)
          stocksucre += varsucre
          varsucre = 0
          varlait = 0.0
          varcafe = 0
        }
      }
      //Quitter
      else {
        fin = true
      }

    }
    println("fin du programme")
  }
}
