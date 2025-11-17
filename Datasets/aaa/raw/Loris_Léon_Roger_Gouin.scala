
import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var cafePossible=true
    var sucreStock = 30
    var cafeStock = 50
    var laitStock = 500.0
    val codeAdmin = "434343"
    var mode = 0
    var sucre = 0
    var prixsucre = 0.0
    var supplementLait = 0
    var prixsucrepeu = 0.10
    var prixsucremoyen = 0.20
    var prixsucrebeaucoup = 0.30
    var prixexpresso = 2.00
    var prixcappuccino = 2.50
    var prixlattepetit = 2.70
    var prixlattemoyen = 3.20
    var nbdoses = 0
    var prixlattegrand = 3.70
    var tailleLatte = 0
    var cafe = 0
    var SupplementEnLait = 0
    var execution = 3
    var jeveuxdulait= 2
    var nombrededose=0
    var prixboisson = 0.0


    while (execution == 3) {
      cafePossible = true
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode = readLine("> ").toInt

      if (mode == 3 || mode == 1 || mode == 2) {
        execution = 4
      }

      while (mode != 1 && mode != 2 && mode != 3) {
        println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
        println("Bienvenue chez Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine("> ").toInt
      }

      while (mode == 1) {
        val caracteres = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var payementpartwint = ""
        for (i <- 1 to 5) {
          val choixcaracteres = (Math.random() * 62).toInt
          payementpartwint += caracteres(choixcaracteres)
        }

        println("Veuillez sélectionner un café")
        println("1) Expresso - CHF 2.00 ")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        cafe = readLine("> ").toInt

        if (cafe == 1) {
          prixboisson = 2.00
        }

        else if (cafe == 2) {
          prixboisson = 2.50
        }


        while (cafe != 1 && cafe != 2 && cafe != 3) {
          println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
          println("Veuillez sélectionner un café")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          cafe = readLine("> ").toInt
        }


        println("Souhaitez-vous ajouter du sucre ? ")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        sucre = readLine("> ").toInt

        while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Entrée invalide, veuillez saisir '1', '2', '3' ou '4'")
          println("Souhaitez-vous ajouter du sucre ? ")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          sucre = readLine("> ").toInt
        }


        if (cafe == 3) {
          println("Voulez-vous un Latte de quelle taille ?")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          tailleLatte = readLine("> ").toInt

          while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Entrée invalide, veuillez saisir '1', '2' ou '3'")
            println("Voulez-vous un Latte de quelle taille ?")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            tailleLatte = readLine("> ").toInt
          }
        }
        if (cafe == 2 || cafe == 3){
        println("Voulez vous un supplément de lait ? (0.05 CHF par dose) \n1)Oui\n2)Non")
        jeveuxdulait = readLine("> ").toInt
        while (jeveuxdulait !=1 && jeveuxdulait !=2 ){
          println("" +
            "Entrée invalide, veuillez saisir '1'ou '2'")
                  jeveuxdulait = readLine("> ").toInt

        }

        if (jeveuxdulait == 1){
          println("Combien de dose ? (maximum 3 doses)")
          nombrededose=readLine("> ").toInt
          while (nombrededose < 1 || nombrededose > 3){
            println("Entrée invalide, veuillez saisir '1', '2' ou '3")
            nombrededose=readLine("> ").toInt
          }
        }
        }

        laitStock-=nombrededose*0.05
        var prixlait = nombrededose * 0.05
        sucreStock-=(sucre-1)*5
        nombrededose=0


        if (cafe == 1){
          println ("Boisson sélectionnée : Expresso")
        }

        else if (cafe == 2){
          println ("Boisson sélectionnée : Cappucino")
        }

        else if (cafe == 3){
          if (tailleLatte == 1){
            println ("Boisson sélectionée : Latte (Petit)")
          }

          else if (tailleLatte == 2){
            println ("Boisson sélectionée : Latte (Moyen)")
          }

          else if (tailleLatte == 3){
            println ("Boisson séléctionée : Latte (Grand)")
          }
        }

        if (sucre == 1){
          println ("Niveau de sucre: Sans sucre ")
        }

        else if (sucre == 2){
          println("Niveau de sucre: Peu (5g) ")
        }

        else if (sucre == 3){
          println("Niveau de sucre: Moyen (10g) ")
        }

        else if ( sucre == 4){
          println("Niveau de sucre: Beaucoup (15g) ")
        }




        if( cafeStock<0){
          println("Erreur : Quantité de poudre de cafe insuffisante pour préparer la boisson sélectionnée. ")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          cafePossible=false
          mode = 1
          execution = 3
        }
        if( sucreStock<0){
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson selectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          cafePossible=false
          mode = 1
          execution = 3
        }

        if( laitStock<0){
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson selectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          cafePossible=false
          mode = 1
          execution = 3
        }

        if (cafePossible){
          if (cafe == 1) {
            cafeStock-=8
            // Expresso
            //prixtotal = prixexpresso + (sucre-1) * 0.1
            if (sucre == 1) {
            } else if (sucre == 2) {
              sucreStock -= 5
              prixsucre = 0.10
            } else if (sucre == 3) {
              prixsucre = 0.20
            } else if (sucre == 4) {
              prixsucre = 0.30
            }

          } else if (cafe == 2) {
            cafeStock -= 6
            laitStock -= 100
            // Cappuccino
            if (sucre == 1) {
            } else if (sucre == 2) {
              prixsucre = 0.10
            } else if (sucre == 3) {
              prixsucre = 0.20
            } else if (sucre == 4) {
              prixsucre = 0.30
            }

          } else if (cafe == 3) {
            // Latte
            if (tailleLatte == 1) {
              cafeStock -= 6
              laitStock -= 120
              prixboisson = 2.70
            } else if (tailleLatte == 2) {
              cafeStock -= 8
              laitStock -= 150
              prixboisson = 3.20
            } else if (tailleLatte == 3) {
              cafeStock -= 12
              laitStock -= 200
              prixboisson = 3.70
            }
          if (sucre == 1) {
          } else if (sucre == 2) {
            sucreStock -= 5
            prixsucre = 0.10
          } else if (sucre == 3) {
            sucreStock -= 10
            prixsucre = 0.20
          } else if (sucre == 4) {
            sucreStock -= 15
            prixsucre = 0.30
          }
        }


        if (jeveuxdulait == 1){
          println ("Lait supplémentaire: Oui")
        }

        else if (jeveuxdulait == 2){
          println("Lait supplémentaire: Non")
        }

        var prixTotal = prixboisson + prixsucre + prixlait

        printf ("prixTotal : + CHF " + f"$prixboisson%.2f" + " + CHF " + f"$prixsucre%.2f" + " + CHF " + f"$prixlait%.2f" + " = " + f"$prixTotal%.2f" + "CHF\n")
        println (" \nVeuillez payer en utilisant Twint.")
        println ("Votre code de paiement est : " + payementpartwint)
        println ("(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println ("Merci ! Votre paiement a été accepté.")
        println ("Préparation de votre boisson...")
        Thread.sleep(3000)
        mode = 0
        execution = 3


        if (cafe == 1){
        println("Votre Expresso est prêt ! Bonne dégustation !")
        }

        else if (cafe == 2) {
          println("Votre Cappuccino est prêt ! Bonne dégustation !")
        }

        else {
          println("Votre Latte est prêt ! Bonne dégustation !")
        }


        Thread.sleep(3000)
          }

      }

      if (mode == 2) {
        println("Mode Admin")

        println("Entrez le code PIN : ****** ")
        val codeSaisi = readLine("> ")
        if (codeSaisi == codeAdmin) {

          println("Stocks :")
          println("Poudre de café : " + cafeStock)
          println("Lait : " + laitStock)
          println("Sucre : " + sucreStock)


          println("Souhaitez-vous réapprovisionner les stocks ? (1 pour Oui, 2 pour Non)")
          var choix = readLine( "> ").toInt

          while (choix != 1 && choix !=2) {
            println("Entrée invalide, veuillez saisir '1'ou '2'")
            println("Souhaitez-vous réapprovisionner les stocks ? (1 pour Oui, 2 pour Non)")
            choix = readLine( "> ").toInt
          }

          if (choix == 1) {
            println("Combien de poudre à café souhaitez-vous ajouter (en gramme) ?")
            sucreStock += readLine("> ").toInt

            println("Combien de lait souhaitez-vous ajouter (en millilitre) ?")
            cafeStock += readLine("> ").toInt

            println("Combien de sucre souhaitez-vous ajouter ? (en gramme)")
            laitStock += readLine("> ").toDouble

            println("Stocks après réapprovisionnement :")
            println("Sucre : " + sucreStock)
            println("Café : " + cafeStock)
            println("Lait : " + laitStock)
          }

        } else {
          println("Code administrateur incorrect.")
        }
        println ("Retour au menu principal...")
        mode = 0
        execution = 3

    }
  }
}
}
