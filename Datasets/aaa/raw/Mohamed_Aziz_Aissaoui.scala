import scala.io.StdIn.{readDouble, readInt, readLine}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var running = true
    val PIN = "434343"
    var poudrecafe: Int = 50
    var sucre: Int = 30
    var lait:Double = 0.500
    val prixexp: Double = 2.0
    val prixcap: Double = 2.5
    val prixlatpetit: Double = 2.7
    val prixlatmoyen: Double = 3.2
    val prixlatgrand: Double = 3.7
    var quantitesucrerajoutee: String = ""
    var boisson :String = ""
    var laitsupp :String = ""
    var prixcafe:Double= 0
    var prixsucre:Double= 0
    var prixlait:Double=0

    while (running==true) {
      println("Nospresso Café")

      println("Veilleuz sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")

      var choix= readLine()
      while(choix != "1" && choix != "2" && choix != "3") {
        println("Entrée invalide. Veuillez sélectionner une option valide.")
        choix= readLine()}
      if (choix == "1") {
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
        var choix1= readLine()

        while(choix1!= "1" && choix1 != "2" && choix1 != "3") {
          println("Entrée invalide. Veuillez sélectionner une option valide.")
          choix1= readLine()}
        if (choix1 == "1") {
          boisson = "Expresso"
          prixcafe=prixexp
          println("Vous avez selectionnée un expresso")
          println("Souhaitez vous rajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          var sucrerajoutee = readLine()
          while (sucrerajoutee != "1" && sucrerajoutee != "2" && sucrerajoutee != "3" && sucrerajoutee != "4") {
            println("Entrée invalide. Veuillez sélectionner une option valide.")
            sucrerajoutee = readLine()}
          if (sucrerajoutee == "1") {
            quantitesucrerajoutee = "Sans sucre"
            sucre = sucre
            poudrecafe = poudrecafe - 8
          }


          if (sucrerajoutee == "2") {
            quantitesucrerajoutee = "Peu (5g)"
            sucre = sucre - 5
            poudrecafe = poudrecafe - 8


          }
          else if (sucrerajoutee == "3") {
            quantitesucrerajoutee = "Moyen (10g)"
            sucre = sucre - 10
            poudrecafe = poudrecafe - 8


          }
          else if (sucrerajoutee == "4"){
            quantitesucrerajoutee = "Beaucoup (15g)"
            poudrecafe = poudrecafe - 8
            sucre=sucre-15

          }


        }


        else if(choix1 == "2"){
          boisson="Cappucino"
          prixcafe=prixcap
          println("Vous avez choisi un Cappuccino.")
          lait=lait-0.100
          println("Souhaitez vous rajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
          var sucrerajoutee = readLine()
          while (sucrerajoutee != "1" && sucrerajoutee != "2" && sucrerajoutee != "3" && sucrerajoutee != "4") {
            println("Entrée invalide. Veuillez sélectionner une option valide.")
            sucrerajoutee= readLine()}
          if (sucrerajoutee == "1") {
            quantitesucrerajoutee = "Sans sucre"
            sucre = sucre
            poudrecafe=poudrecafe-6

          }

          if (sucrerajoutee == "2") {
            quantitesucrerajoutee = "Peu (5g)"
            sucre = sucre - 5
            poudrecafe=poudrecafe-6

          }
          else if (sucrerajoutee == "3") {
            quantitesucrerajoutee = "Moyen (10g)"
            sucre = sucre - 10
            poudrecafe=poudrecafe-6

          }
          else if (sucrerajoutee=="4"){
            quantitesucrerajoutee = "Beaucoup (15g)"
            poudrecafe=poudrecafe-6
            sucre=sucre-15

          }
          else {println("Entrée invalide. Veuillez sélectionner une option valide.")
          }
          println("Souhaitez-vous ajouter du lait en supplément ?\n 1)Oui\n 2)Non\n >")
          var laitajoute = readLine()
          while(laitajoute != "1" && laitajoute != "2" ) {
            println("Entrée invalide. Veuillez sélectionner une option valide.")
            laitajoute = readLine()}
          if (laitajoute== "1"){
            laitsupp= "Oui"
            println("Combien de dose?")

            var dose = readLine()
            while(dose != "1" && dose != "2" && dose!="3" ) {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              dose = readLine()
            }
            if(dose=="1"){
              lait=lait-0.050
              prixlait= 0.05
            }
            else if(dose=="2"){
              lait=lait-0.100
              prixlait= 0.10
            }
            else if(dose=="3"){
              lait=lait-0.150
              prixlait= 0.15
            }
            else {
              println("Entrée invalide. Veuillez sélectionner une option valide.")}
          }
          else if(laitajoute== "2"){
            laitsupp="Non"
            prixlait= 0
            lait=lait
          }






        }
        else if(choix1=="3"){
          println("Vous avez choisi un Latte.")
          println("Quelle taille voulez vous?\n\n1)Petit\n2)Moyen\n3)Grand")
          var taille= readLine()
          while(taille!= "1"&& taille!="2"&& taille!="3") {
            println("Entrée invalide. Veuillez sélectionner une option valide.")
            taille=readLine()
          }
          if (taille=="1"){
            boisson="Latte(Petit)"
            prixcafe=prixlatpetit
            poudrecafe=poudrecafe-6
            lait=lait-0.120
            println("Souhaitez vous rajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
            var sucrerajoutee = readLine()
            while (sucrerajoutee != "1" && sucrerajoutee != "2" && sucrerajoutee != "3" && sucrerajoutee != "4") {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              sucrerajoutee= readLine()}
            if (sucrerajoutee == "1") {
              quantitesucrerajoutee = "Sans sucre"
              sucre = sucre


            }


            if (sucrerajoutee == "2") {
              quantitesucrerajoutee = "Peu (5g)"
              sucre = sucre - 5


            }
            else if (sucrerajoutee == "3") {
              quantitesucrerajoutee = "Moyen (10g)"
              sucre = sucre - 10


            }
            else if (sucrerajoutee=="4"){
              quantitesucrerajoutee = "Beaucoup (15g)"
              sucre=sucre-15

            }
            println("Souhaitez-vous ajouter du lait en supplément ?\n 1)Oui\n 2)Non\n >")
            var laitajoute = readLine()
            while(laitajoute != "1" && laitajoute != "2" ) {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              laitajoute = readLine()}
            if (laitajoute== "1"){
              laitsupp="Oui"
              println("Combien de dose?")

              var dose = readLine()
              while(dose != "1" && dose != "2" && dose!="3" ) {
                println("Entrée invalide. Veuillez sélectionner une option valide.")
                dose = readLine()}
              if(dose=="1"){
                prixlait= 0.05
                lait=lait-0.050
              }
              else if(dose=="2"){
                lait=lait-0.100
                prixlait= 0.10
              }
              else if(dose=="3"){
                lait=lait-0.150
                prixlait= 0.15
              }
            }

            else if(laitajoute== "2"){
              laitsupp="Non"
              prixlait=0

              lait=lait
            }
          }
          else if(taille=="2"){
            boisson= "Latte(Moyen)"
            prixcafe=prixlatmoyen
            poudrecafe=poudrecafe-8
            lait=lait-0.150
            println("Souhaitez vous rajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
            var sucrerajoutee = readLine()
            while (sucrerajoutee != "1" && sucrerajoutee != "2" && sucrerajoutee != "3" && sucrerajoutee != "4") {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              sucrerajoutee= readLine()}
            if (sucrerajoutee == "1") {
              quantitesucrerajoutee = "Sans sucre"
              sucre = sucre
            }

            if (sucrerajoutee == "2") {
              quantitesucrerajoutee = "Peu (5g)"
              sucre = sucre - 5
            }
            else if (sucrerajoutee == "3") {
              quantitesucrerajoutee = "Moyen (10g)"
              sucre = sucre - 10
            }
            else if (sucrerajoutee=="4"){
              quantitesucrerajoutee = "Beaucoup (15g)"
              sucre=sucre-15
            }

            println("Souhaitez-vous ajouter du lait en supplément ?\n 1)Oui\n 2)Non\n >")
            var laitajoute = readLine()
            while(laitajoute != "1" && laitajoute != "2" ) {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              laitajoute = readLine()}
            if (laitajoute== "1"){
              laitsupp="Oui"
              println("Combien de dose?")

              var dose = readLine()
              while(dose != "1" && dose != "2" && dose!="3" ) {
                println("Entrée invalide. Veuillez sélectionner une option valide.")
                dose = readLine()}
              if(dose=="1"){
                lait=lait-0.050
                prixlait= 0.05
              }
              else if(dose=="2"){
                lait=lait-0.100
                prixlait=0.10
              }
              else if(dose=="3"){
                lait=lait-0.150
                prixlait= 0.15
              }

            }
            else if(laitajoute== "2"){
              laitsupp="Non"
              lait=lait
              prixlait= 0

            }}
          else if(taille=="3"){
            boisson="Latte(Grand)"
            prixcafe=prixlatgrand
            poudrecafe=poudrecafe-12
            lait=lait-0.200
            println("Souhaitez vous rajouter du sucre?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
            var sucrerajoutee = readLine()
            while (sucrerajoutee != "1" && sucrerajoutee != "2" && sucrerajoutee != "3" && sucrerajoutee != "4") {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              sucrerajoutee= readLine()}
            if (sucrerajoutee == "1") {
              quantitesucrerajoutee = "Sans sucre"
              sucre = sucre
            }


            if (sucrerajoutee == "2") {
              quantitesucrerajoutee = "Peu (5g)"
              sucre = sucre - 5
            }
            else if (sucrerajoutee == "3") {
              quantitesucrerajoutee = "Moyen (10g)"
              sucre = sucre - 10
            }
            else if (sucrerajoutee=="4"){
              quantitesucrerajoutee = "Beaucoup (15g)"
              sucre=sucre-15
            }
            println("Souhaitez-vous ajouter du lait en supplément ?\n 1)Oui\n 2)Non\n >")
            var laitajoute = readLine()
            while(laitajoute != "1" && laitajoute != "2" ) {
              println("Entrée invalide. Veuillez sélectionner une option valide.")
              laitajoute = readLine()}
            if (laitajoute== "1"){
              laitsupp="Oui"
              println("Combien de dose?")
              prixlait= 0.05

              var dose = readLine()
              while(dose != "1" && dose != "2" && dose!="3" ) {
                println("Entrée invalide. Veuillez sélectionner une option valide.")
                dose = readLine()}
              if(dose=="1"){
                lait=lait-0.050
              }
              else if(dose=="2"){
                lait=lait-0.100
                prixlait= 0.10
              }
              else if(dose=="3"){
                lait=lait-0.150
                prixlait= 0.15
              }
            }
            else if(laitajoute== "2"){
              laitsupp="Non"
              lait=lait
              prixlait= 0
            }
          }
          else{
            println("Choix de boisson invalide.")
          }}
        if (quantitesucrerajoutee=="Beaucoup (15g)") {
          prixsucre= 0.30
        }
        else if (quantitesucrerajoutee=="Moyen (10g)") {
          prixsucre= 0.20
        }
        else if (quantitesucrerajoutee=="Peu (5g)") {
          prixsucre= 0.10}
        else if (quantitesucrerajoutee=="Sans sucre") {
          prixsucre= 0
        }



        printf("Boisson sélectionnée: %s \n",boisson)
        printf("Niveau de sucre : %s \n" ,quantitesucrerajoutee)
        printf("Lait en supplément: %s \n", laitsupp)
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n\n" ,prixcafe,prixsucre,prixlait,prixcafe+prixsucre+prixlait)
        println("Veuillez payer en utilisant Twint.")
        val caracteresPossibles = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        val longueurCode = 5
        val codeTwint = (1 to longueurCode)
          .map(_ => caracteresPossibles(Random.nextInt(caracteresPossibles.length)))
          .mkString


        println(s"Votre code de paiement est : $codeTwint\n")
        println("(En attente de paiement...)\n\n")
        Thread.sleep(3000)
        println("Paiement confirmé.")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        printf("Votre %s est prêt ! Bonne dégustation !\n\n",boisson)}





      else if(choix=="2"){
        println("Mode Admin")
        println("Entrez le code PIN:")
        var PINsaisi= readLine()
        while(PINsaisi!=PIN)
        {println("Code PIN incorrect. Veuillez saisir le bon code")
          PINsaisi= readLine()}
        if (PINsaisi==PIN){
          println("Accés autorisé\n")}
        println("Stocks : ")
        printf("Poudre de café : %dg\n", poudrecafe)
        printf("Lait : %.3fL\n",lait)
        printf("Sucre : %dg\n",sucre)
        println("Réapprovisionnement des stocks...")
        println("Ajout :")
        println("Combien de poudre souhaitez vous rajouter?")
        val poudrerajoutee= readInt()
        poudrecafe=poudrecafe+poudrerajoutee
        printf("Poudre de café : %d\n",poudrerajoutee)
        println("Combien de lait souhaitez vous rajouter?")
        val laitajoutee= readDouble()
        lait=lait+laitajoutee
        printf("Lait : %.3f\n",laitajoutee)
        println("Combien de sucre souhaitez vous rajouter?")
        val sucrerajoutee= readInt()
        sucre=sucre+sucrerajoutee
        printf("Sucre : %d\n",poudrerajoutee)
        println("Niveaux de stock mis a jour.\nRetour au menu principal...")


      }





      else if(choix=="3"){
        println("Au revoir !")
        running = false
      }
    }}}
