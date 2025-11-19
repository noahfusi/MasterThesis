

object Main {
  def main(args: Array[String]): Unit = {
    import io.StdIn._
import scala.math._

    // x == qui
    // y == boisson
    // z == sucre
    // t == latte taille

    var x = 0
    var sslait = 0.500
    var sssucre = 30
    var sscafe = 50
    val PIN = 1234 // code pin ICI !!!!!!!!!!!!!!!!!!!!!!!!!
    var boissons = " "
    var laitsupl = " "
    var zucreutiliser = 0
    var laitutiliser = 0.0
    var cafeutiliser = 0
    var reaprovisation = 0
    var caferajouter = 0
    var laitrajouter = 0.0
    var sucrerajouter = 0
    var prix = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var prixtotal = 0.0



    do {

      x = 0
      prix= 0.0
      prixlait=0.0
      prixsucre=0.0
      prixtotal = 0.0
      var boissons = " "
      var laitsupl = " "

      if (sscafe < 0) {


        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionneé.")
        println("Veuillez vérifier les stocks en mode Admin ou choisir une autre boisson")


        sslait += laitutiliser
        sscafe += cafeutiliser
        sssucre += zucreutiliser

        laitutiliser = 0
        cafeutiliser = 0
        zucreutiliser = 0

      }


      if (sssucre < 0) {


        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionneé.")
        println("Veuillez vérifier les stocks en mode Admin ou choisir une autre boisson")


        sslait += laitutiliser
        sscafe += cafeutiliser
        sssucre += zucreutiliser

        laitutiliser = 0
        cafeutiliser = 0
        zucreutiliser = 0



      }



      laitutiliser = 0
      cafeutiliser = 0
      zucreutiliser = 0

      while (!(x == 1 || x == 2 || x == 3)) {

        println("Veuillez sélectionner votre mode :")
        println("1 pour Client")
        println("2 pour Admin")
        println("3 pour Quitter")
        print("> ")

        x = readLine().toInt

        if (!(x == 1) && !(x == 2) && !(x == 3)) println("Tapé 1, 2 ou 3")

      }

      if (x == 1) {
        var y = 0
        var t = 0

        while (!(y == 1 || y == 2 || y == 3)) {
          println("Veuillez sélectionner votre boisson :")
          println("1 pour Expresso - CHF 2.00")
          println("2 pour Cappuchino - CHF 2.50")
          println("3 pour Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")

          y = readLine().toInt

          if (!(y == 1) && !(y == 2) && !(y == 3)) println("Tapé 1, 2 ou 3")

          if (y == 1) {
            prix = 2
            sscafe -= 8
            cafeutiliser += 8
            boissons = "Expresso"
          }
          if (y == 2) {
            prix= 2.5
            boissons = "Capuccino"
            sscafe -= 6
            cafeutiliser += 6
            sslait -= 0.100
            laitutiliser += 0.100
          }


          if (y == 3) {
            boissons = "Late"

            while (!(t == 1 || t == 2 || t == 3)) {
              println("Taille ? :")
              println("1 pour petit")
              println("2 pour moyen")
              println("3 pour grand")
              print("> ")

              t = readLine().toInt


              if (!(t == 1) && !(t == 2) && !(t == 3)) println("Tapé 1, 2 ou 3")

            }

            if (t == 1) {
              sslait -= 0.120
              sscafe -= 6
              cafeutiliser += 6
              laitutiliser += 0.120
              prix= 2.7
            } else {
              if (t == 2) {
                sslait -= 0.150
                sscafe -= 8
                cafeutiliser += 8
                laitutiliser += 0.150

              } else {
                sslait -= 0.200
                sscafe -= 12
                cafeutiliser += 12
                laitutiliser += 0.200

              }
            }


          }
        }


        var z = 0
        while (!(z == 1 || z == 2 || z == 3 || z == 4)) {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")


          z = readLine().toInt

          if (!(z == 1) && !(z == 2) && !(z == 3) && !(z == 4)) println("Tapé 1, 2, 3 ou 4")

        }

        if (z == 2) {
          sssucre -= 5
          zucreutiliser += 5
        }
        if (z == 3) {
          sssucre -= 10
          zucreutiliser += 10
        }
        if (z == 4) {
          sssucre -= 15
          zucreutiliser += 15
        }


        //  3 doses maximale par boisson, une dose contiens 50ml de lait)
        var lait = 0
        var dose = 0

        if ((y == 2) || (y == 3)) {


          while (!(lait == 1 || lait == 2)) {
            println("Souhaitez-vous ajouter du lait ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) oui")
            println("2) non")

            print("> ")

            lait = readLine().toInt

            if (!(lait == 1) && !(lait == 2)) println("Tapé 1 ou 2")

            if (lait == 1) {
              while (!(dose == 1 || dose == 2 || dose == 3)) {
                println("nombre de dose : 1, 2 ou 3 : ")
                dose = readLine().toInt
                if (!(dose == 1) && !(dose == 2) && !(dose == 3)) println("Tapé 1, 2 ou 3")
              }
              sslait = sslait - (dose * 0.050)
              laitutiliser += (dose * 0.050)

            }
          }
        }
        //--------------------------------------------------------------------------------------------



        println("Boisson sélectionné : " + boissons)

        var sucreouinon = " "
        if (z == 1) sucreouinon = "pas de sucre"
        if (z == 2) sucreouinon = "faible (5g)"
        if (z == 3) sucreouinon = "Moyen (10g)"
        if (z == 4) sucreouinon = "Beaucoup (15g)"
        println("Niveau de sucre : " + sucreouinon)

        var laiouinon = " "
        if (lait == 1) laiouinon = "oui"
        if (lait == 2) laiouinon = "non"
        println("Lait en supplément: " + laiouinon)


        if(y==1)prix = 2
        if(y==2)prix = 2.5
        if(t==1) prix = 2.7
        if(t==2)prix = 3.2
        if(t==3)prix = 3.7
        if(z==1) prixsucre=0
        if(z==2) prixsucre=0.1
        if(z==3) prixsucre=0.2
        if(z==4) prixsucre=0.3
        prixlait= dose*0.05

        prixtotal = prix+prixlait+prixsucre




        println("Prix Total: CHF "+ prix+" + CHF "+prixlait+" + CHF "+ prixsucre+" = CHF " + prixtotal)



        //---   STOCK    -----------------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------







        if (sslait < 0) { //---   PAS DE LAIT    -----------------------------------------------------------------------------------------

          while (sslait < 0) {

            prix= 0.0
            prixlait=0.0
            prixsucre=0.0
            prixtotal = 0.0



            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez essayer une autre boisson ou choisir une taille plus petite.")
            sslait += laitutiliser
            sscafe += cafeutiliser
            sssucre += zucreutiliser

            laitutiliser = 0
            cafeutiliser = 0
            zucreutiliser = 0


            var y = 0
            var t = 0

            while (!(y == 1 || y == 2 || y == 3)) {
              println("Veuillez sélectionner votre boisson :")
              println("1 pour Expresso - CHF 2.00")
              println("2 pour Cappuchino - CHF 2.50")
              println("3 pour Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
              print("> ")

              y = readLine().toInt

              if (!(y == 1) && !(y == 2) && !(y == 3)) println("Tapé 1, 2 ou 3")

              if (y == 1) {
                sscafe -= 8
                cafeutiliser += 8
                boissons = "Expresso"

              }
              if (y == 2) {
                boissons = "Capuccino"
                sscafe -= 6
                cafeutiliser += 6
                sslait -= 0.100
                laitutiliser += 0.100
              }


              if (y == 3) {
                boissons = "Late"

                while (!(t == 1 || t == 2 || t == 3)) {
                  println("Taille ? :")
                  println("1 pour petit")
                  println("2 pour moyen")
                  println("3 pour grand")
                  print("> ")

                  t = readLine().toInt


                  if (!(t == 1) && !(t == 2) && !(t == 3)) println("Tapé 1, 2 ou 3")

                }

                if (t == 1) {
                  sslait -= 0.120
                  sscafe -= 6
                  cafeutiliser += 6
                  laitutiliser += 0.120
                } else {
                  if (t == 2) {
                    sslait -= 0.150
                    sscafe -= 8
                    cafeutiliser += 8
                    laitutiliser += 0.150
                  } else {
                    sslait -= 0.200
                    sscafe -= 12
                    cafeutiliser += 12
                    laitutiliser += 0.200
                  }
                }


              }
            }


            var z = 0
            while (!(z == 1 || z == 2 || z == 3 || z == 4)) {
              println("Souhaitez-vous ajouter du sucre ?")
              println("1) Sans sucre")
              println("2) Peu (5g) - CHF 0.10")
              println("3) Moyen (10g) - CHF 0.20")
              println("4) Beaucoup (15g) - CHF 0.30")
              print("> ")


              z = readLine().toInt

              if (!(z == 1) && !(z == 2) && !(z == 3) && !(z == 4)) println("Tapé 1, 2, 3 ou 4")

            }


            if (z == 2) {
              sssucre -= 5
              zucreutiliser += 5
            }
            if (z == 3) {
              sssucre -= 10
              zucreutiliser += 10
            }
            if (z == 4) {
              sssucre -= 15
              zucreutiliser += 15
            }

            //  3 doses maximale par boisson, une dose contiens 50ml de lait)
            var lait = 0
            var dose = 0

            if ((y == 2) || (y == 3)) {


              while (!(lait == 1 || lait == 2)) {
                println("Souhaitez-vous ajouter du lait ?")
                println("(Disponible uniquement pour Cappuccino et Latte)")
                println("1) oui")
                println("2) non")

                print("> ")

                lait = readLine().toInt

                if (!(lait == 1) && !(lait == 2)) println("Tapé 1 ou 2")

                if (lait == 1) {
                  while (!(dose == 1 || dose == 2 || dose == 3)) {
                    println("nombre de dose : 1, 2 ou 3 : ")
                    dose = readLine().toInt
                    if (!(dose == 1) && !(dose == 2) && !(dose == 3)) println("Tapé 1, 2 ou 3")
                  }
                  sslait = sslait - (dose * 0.050)
                  laitutiliser += (dose * 0.050)

                }
              }
            }
            //--------------------------------------------------------------------------------------------



            println("Boisson sélectionné : " + boissons)

            var sucreouinon = " "
            if (z == 1) sucreouinon = "pas de sucre"
            if (z == 2) sucreouinon = "faible (5g)"
            if (z == 3) sucreouinon = "Moyen (10g)"
            if (z == 4) sucreouinon = "Beaucoup (15g)"
            println("Niveau de sucre : " + sucreouinon)

            var laiouinon = " "
            if (lait == 1) laiouinon = "oui"
            if (lait == 2) laiouinon = "non"
            println("Lait en supplément: " + laiouinon)



            if(y==1)prix = 2
            if(y==2)prix = 2.5
            if(t==1) prix = 2.7
            if(t==2)prix = 3.2
            if(t==3)prix = 3.7
            if(z==1) prixsucre=0
            if(z==2) prixsucre=0.1
            if(z==3) prixsucre=0.2
            if(z==4) prixsucre=0.3
            prixlait= dose*0.05

            prixtotal = prix+prixlait+prixsucre




            println("Prix Total: CHF "+ prix+" + CHF "+prixlait+" + CHF "+ prixsucre+" = CHF " + prixtotal)

            //--


          }
        } //---   fin de pas DE LAIT//  ----------------------------------------------------------------------------------------
        //  ----------------------------------------------------------------------------------------


        //  ----------------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------
        //    -----------------------------------------------------------------------------------------

        //  -----------------------------------------------------------------------------------------









        //------------     PAYER    --------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------


        if(sscafe>=0 && sslait >=0 && sssucre>=0){
        println("Veuillez payer en utilisant Twint")

        var code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

        print("Votre code de paiement est : ")
        for (x <- 1 to 5) {
          print((code((math.random() * 61).toInt)))
        }
        println(" ")
        println("(En attente de validation du paiement...)")
        Thread.sleep(3000) // Attend pendant 5000 millisecondes (5 secondes)
        println("Merci ! Votre paiement a été́ accepte ́.")

        println("Préparation de votre boisson...")
        println("[...]")
        Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
        println("Votre " + boissons + " est prêt ! Bonne dégustation !")

        }



        //--------------      ADMIN      ------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------

      } else {

        if (x == 2) {
          println("Mode ADMIN")

          var PIN2 = 0
          while (!(PIN2 == PIN)) {
            println("entrer le Pin :")
            print("> ")
            PIN2 = readLine().toInt
            if (!(PIN2 == PIN)) println("faux")
            if ((PIN2 == PIN)) println("Admin confirmé")
          }



          println("Stocks:")

          println("poudre a café : " + sscafe + "g")
          println("Lait : " + sslait + "L")
          println("Sucre : " + sssucre + "g")


          while (!(reaprovisation == 1 || reaprovisation == 2 || reaprovisation == 3)) {

            reaprovisation = 0
            caferajouter = 0
            laitrajouter = 0.0
            sucrerajouter = 0


            println("Réapprovisionnement des stocks... Ajout :")
            println("1) café")
            println("2) Lait")
            println("3) Sucre")
            reaprovisation = readLine().toInt

            if (!(reaprovisation == 1) && !(reaprovisation == 2) && !(reaprovisation == 3)) println("Tapé 1, 2 ou 3")


          }

          if (reaprovisation == 1) {
            println("quantité de café rajouté : ")
            caferajouter = readLine().toInt
            sscafe += caferajouter
          } else {
            if (reaprovisation == 2) {
              println("quantité de Lait rajouté : ")
              laitrajouter = readLine().toDouble
              sslait += laitrajouter
            } else {
              if (reaprovisation == 3) {
                println("quantité de sucre rajouté : ")
                sucrerajouter = readLine().toInt
                sssucre += sucrerajouter
              }
            }
          }

          println("Stocks ajouté:")

          println("poudre a café : " + caferajouter + "g")
          println("Lait : " + laitrajouter + "L")
          println("Sucre : " + sucrerajouter + "g")

          println("Niveaux de stock mis a` jour.")
          println("Retour au menu principal...")


        }
      }

    }
    while( !(x==3))

    println("fin du programme")






















  }
}