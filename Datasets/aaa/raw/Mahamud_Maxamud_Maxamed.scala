import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {


    val codePin = 434343
    var code = 0
    var cafe = 0
    var loop = true
    var Scafe = 50
    var Ssucre = 30
    var Slait = 0.500
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val length = 5
    var i = 0
    var twint = ""
    var prixE =2.00
    var prixC =2.50
    var prixLp =2.70
    var prixLm =3.20
    var prixLg =3.70
    var prixSp =0.10
    var prixSm =0.20
    var prixSb =0.30
    var prixDl =0.05
    var laitBool = false
    var boisson =""
    var prixTot = 0.0
    var quantité = true

    while (loop) {
      prixTot = 0.0
      var choix = readLine("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte

    while (choix < 1 || choix > 3) {

      println("Erreur : Veuillez saisir un choix valide (1, 2 ou 3).")
      choix = readLine("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte
    }

      if (choix == 1) {
        var client = true
        while(client){
          prixTot = 0.0
          cafe = readLine("Veuillez saisir le numéro correspondant à votre café:\n1) Expresso - CHF "+prixE+"0\n2) Cappuccino - CHF "+prixC+"0\n"
            +"3) Latte - CHF "+prixLp+"0 (petit), CHF "+prixLm+"0 (moyen), CHF "+prixLg+"0 (grand)\n>").toByte
          if (cafe == 1) {
            if (Scafe >= 8) {
              Scafe -= 8
              laitBool = true
              boisson = "Expresso"
              prixTot += prixE
              client = false
            } else {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer l'Expresso.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              laitBool = false
              client = true
            }
          }
          if (cafe == 2) {
            if (Scafe >= 6 && Slait >= 0.100) {
              Scafe -= 6
              Slait -= 0.100
              laitBool = true
              boisson = "Cappuccino"
              prixTot += prixC
              client = false
            } else {
              println("Erreur : Quantité insuffisante pour préparer le Cappuccino.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
              laitBool = false
              client = true
            }
          }
          else if(cafe==3){
            var taille = readLine("Personalisation de la boisson:\nVeuillez saisir le numéro correspondant au format:\n1) Petit\n2) Moyen\n3) Grand\n>").toByte
            boisson = "Latte"
            while (taille < 1 || taille > 3) {
              println("Erreur : Choix de taille invalide. Veuillez entrer 1, 2 ou 3.")
              taille = readLine("Personalisation de la boisson:\nVeuillez saisir le numéro correspondant au format:\n1) Petit\n2) Moyen\n3) Grand\n>").toByte
            }
            if (taille == 1 && Scafe >= 6 && Slait >= 0.120) {
              Scafe -= 6
              Slait -= 0.120
              prixTot += prixLp
              laitBool = true
              client = false
            } else if (taille == 2 && Scafe >= 8 && Slait >= 0.150) {
              Scafe -= 8
              Slait -= 0.150
              prixTot += prixLm
              laitBool = true
              client = false
            } else if (taille == 3 && Scafe >= 12 && Slait >= 0.200) {
              Scafe -= 12
              Slait -= 0.200
              prixTot += prixLg
              laitBool = true
              client = false
            } else {
              println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
              laitBool = false
              client = true
            }
          }
          if (laitBool) {
          var sucre = readLine("Souhaitez-vous ajouter du sucre ?\n" +
            "1) Sans sucre \n2) Peu (5g) - CHF "+prixSp+"0\n3) Moyen (10g) - CHF "+prixSm+"0\n4) Beaucoup (15g) - CHF "+prixSb+"0\n>").toByte
            while (sucre < 1 || sucre > 4) {
              println("Erreur :  Veuillez saisir une valeur entre 1 et 4.")
              sucre = readLine("Souhaitez-vous ajouter du sucre ?\n" +
                "1) Sans sucre \n2) Peu (5g) - CHF "+prixSp+"0\n3) Moyen (10g) - CHF "+prixSm+"0\n4) Beaucoup (15g) - CHF "+prixSb+"0\n>").toByte
            }
          if (sucre == 2 && Ssucre >= 5) {
            Ssucre -= 5
            prixTot += prixSp
            client = false
          } else if (sucre == 3 && Ssucre >= 10) {
            Ssucre -= 10
            prixTot += prixSm
            client = false
          } else if (sucre == 4 && Ssucre >= 15) {
            Ssucre -= 15
            prixTot += prixSb
            client = false
          } else if (sucre == 2 || sucre == 3 || sucre == 4) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
            laitBool=false
            client = true
          }
        }

        if ((cafe == 2 || cafe == 3)&&(Ssucre>0)&&(laitBool==true)){
          var choixLait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toByte
          if (choixLait == 1){
            var lait = readLine("Combien de dose ?\n>").toByte
          while (lait < 1 || lait > 3) {
            println("Erreur : Vous pouvez ajouter jusqu'à 3 doses maximum. Veuillez saisir une valeur entre 1 et 3.")
            lait = readLine("Combien de dose ?\n>").toByte
          }
            quantité = true

            var dose = lait * 0.05
            if (Slait >= dose) {
              Slait -= dose
              laitBool = true
              prixTot += prixDl * lait
              client = false
            } else {
              quantité = false
              client = true
            }

            if (quantité==false){

              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
              laitBool=false
            }
          }
        }
        }
        if (laitBool){
          twint=""
          i = 0
          while (i < length) {
            val nombre = (math.random * 36).toInt // nombre de caractères dans mon string
            val alphaN = caracteres(nombre)
            twint += alphaN
            i += 1
          }
          printf("Prix total: %.2f CHF\nVeuillez payer en utilisant Twint.\nVotre code de paiement est :  ",prixTot)
          println(twint)
          println("(En attente de validation du paiement...)\n")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...\n[...]\nVotre "+boisson+" est prêt ! Bonne dégustation !\n")
        }
      }
      else if (choix == 2) {
        code = readLine("Mode Admin\nEntrez le code PIN :").toInt
        while (code != codePin) {
          println("Code incorrect\nVeuillez saisir à nouveau votre code pin")
          code = readLine("Mode Admin\nEntrez le code PIN :").toInt
        }
        println("Accès autorisé.\n\nStocks:\n   Poudre de café :" +Scafe+ "g\n   Lait : " +Slait+ "L\n   Sucre : "+Ssucre+"g\n\nRéapprovisionnement des stocks...\nAjout :")
        val caferestock = readLine("Quantité de poudre de café à ajouter (en g):\n>").toInt
        Scafe += caferestock
        val laitrestock = readLine("Quantité de lait à ajouter (en L):\n>").toDouble
        Slait += laitrestock
        val sucrerestock = readLine("Quantité de sucre à ajouter (en g):\n>").toInt
        Ssucre += sucrerestock
        println("Niveaux de stock mis à jour.\n")
        printf("Voici les nouveaux stocks:\n   Poudre de café :" +Scafe+ "g\n   Lait : %.2f L\n   Sucre : "+Ssucre+"g\n\nRetour au menu principal...", Slait)
      }
    if (choix == 3) {
      println("Vous quittez Nospresso...\n")
      loop = false
    }
  }
  }
}