import scala.io.StdIn.{readDouble, readInt, readLine}
import scala.util.Random


object main {


  def main(args: Array[String]): Unit = {

    // initialisation des variables

    var enMarche = true //variable pour le boucle principale
    val pin = 434343 //code admin
    var stockSucre = 30 //stock de sucre en grammes
    var stockCafe = 50 //stock de poudre de café en grammes
    var stockLait = 0.5 //stock de lait en litres


    //boucle principale
    while (enMarche){

      //interface utilisateur
      println("Nospresso Café \nVeuillez choisir votre mode :\n1) Client\n2) Admin\n3) Quitter")

      var choixUtilisateur = readInt() //choix de l'utilisateur

      if (choixUtilisateur == 1){ // MODE CLIENT -----------------------------------------------------------------------

        var prixTotal = 0.0
        var textPrixTotal = ""

        // on initialise le choix de la boisson à 0
        var choixBoisson : Int =  0
        // on initialise le texte de la boisson pour le résumé de la commande
        var boissonText = ""
        // on initialise la quantité de café pour le résumé de la commande
        var cafeQuantite = 0
        // on initialise la quantité de lait pour le résumé de la commande
        var laitQuantite = 0.0

        //tant que le choix de la boisson n'est pas valide, on redemande à l'utilisateur de choisir
        while (choixBoisson < 1 || choixBoisson > 3){
          println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte (Petit) - CHF 2.70" +
            ", CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          choixBoisson = readInt()
        }

        var choixTaille : Int = 0

        // on initialise le texte du latte pour le résumé de la commande
        var latteText = ""

        // on met à jour le prix et la quantité de café en fonction du choix de l'utilisateur

        if (choixBoisson == 1){
          boissonText = "Expresso"
          prixTotal += 2.0
          cafeQuantite = 8
          textPrixTotal = "CHF 2.00 "
        } else if (choixBoisson == 2){
          boissonText = "Cappuccino"
          prixTotal += 2.5
          cafeQuantite = 6
          laitQuantite = 0.1
          textPrixTotal = "CHF 2.50 "
        } else if (choixBoisson == 3){
          boissonText = "Latte"
          while (choixTaille < 1 || choixTaille > 3){
            println("Veuillez sélectionner la taille de votre Latte :\n1) Petit\n2) Moyen\n3) Grand")
            choixTaille = readInt()
          }
          if (choixTaille == 1){
            latteText = "(Petit)"
            prixTotal += 2.7
            cafeQuantite = 6
            laitQuantite = 0.12
            textPrixTotal = "CHF 2.70 "
          } else if (choixTaille == 2){
            latteText = "(Moyen)"
            prixTotal += 3.2
            cafeQuantite = 8
            laitQuantite = 0.18
            textPrixTotal = "CHF 3.20 "
          } else if (choixTaille == 3){
            latteText = "(Grand)"
            prixTotal += 3.7
            cafeQuantite = 12
            laitQuantite = 0.2
            textPrixTotal = "CHF 3.70 "
          }
        }


        // on demande à l'utilisateur s'il veut du sucre
        var sucre : Int = 0
        // on initialise le texte du sucre pour le résumé de la commande
        var sucreText = ""
        // on initialise la quantité de sucre pour le résumé de la commande
        var sucreQuantite = 0
        while (sucre < 1 || sucre > 4){
          println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)")
          sucre = readInt()
        }

        // on met à jour le prix, la quantité de sucre et le texte du sucre en fonction du choix de l'utilisateur

        if (sucre == 1){
          sucreText = "Sans sucre"
          prixTotal += 0.0
          sucreQuantite = 0
        } else if (sucre == 2){
          sucreText = "Peu"
          prixTotal += 0.1
          sucreQuantite = 5
          textPrixTotal += "+ CHF 0.10"
        } else if (sucre == 3){
          sucreText = "Moyen"
          prixTotal += 0.2
          sucreQuantite = 10
          textPrixTotal += "+ CHF 0.20"
        } else if (sucre == 4){
          sucreText = "Beaucoup"
          prixTotal += 0.3
          sucreQuantite = 15
          textPrixTotal += "+ CHF 0.30"
        }

        //ajouter du lait supplémentaire pour le latte et le cappuccino
        var choixAjoutLait : Int = 0
        // on initialise le texte du lait pour le résumé de la commande
        var laitText = ""


        if (choixBoisson == 2 || choixBoisson == 3){
          while (choixAjoutLait < 1 || choixAjoutLait > 2){
            println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
            choixAjoutLait = readInt()
          }
          if(choixAjoutLait == 1){
            laitText = "Oui"

            var doseLait = 0 //dose de lait max 3

            while (doseLait < 1 || doseLait > 3){
              println("Combien de dose ? (max 3)")
              doseLait = readInt()
            }

            // on met à jour le prix et la quantité de lait en fonction du choix de l'utilisateur
            if(doseLait == 1){
              prixTotal +=  0.05
              laitQuantite +=  0.05
              textPrixTotal +=  " + 0.5 CHF"
            }else if(doseLait == 2){
              prixTotal += 0.1
              laitQuantite += 0.1
              textPrixTotal += " + 0.10 CHF"
            }else if(doseLait == 3){
              prixTotal += 0.15
              laitQuantite += 0.15
              textPrixTotal += " + 0.15 CHF"
            }

          }else{
            laitText = "Non"
          }
        }

        // résumé de la commande
        println("\nBoisson sélectionnée : " + boissonText + " " + latteText)
        println("Niveau de sucre : " + sucreText)
        if(choixBoisson == 2 || choixBoisson == 3){
          println("Lait supplémentaire : " + laitText)
        }
        println(f"Prix total : ${textPrixTotal} = CHF ${prixTotal}\n")

        //on vérifie les quantités de stock
        if (stockSucre < sucreQuantite){
          println("\nQuantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
        } else if (stockCafe < cafeQuantite){
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
        } else if (stockLait < laitQuantite){

          //si le client a choisi le latte
          if(choixTaille == 2 || choixTaille == 3){
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")

          }else{
            println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
          }
        } else {

          //interface de paiement via twint

          var codeTwint = Random.alphanumeric.take(5).mkString

          println("\nVeuillez payen en utilisant Twint\nVotre code de paiement est : " + codeTwint)

          println("(En attente de validation du paiement")
          Thread.sleep(3000)

          println("\nMerci ! Votre paiement a été accepté")

          // on déduit les quantités de stock
          stockSucre -= sucreQuantite
          stockCafe -= cafeQuantite
          stockLait -= laitQuantite


          // on affiche la préparation de la boissons

          println("Préparation de votre boisson...")
          println("[...]")
          println("Votre + " + boissonText +" est prêt ! Bonne dégustation !\n")


          Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)

        }


      } else if (choixUtilisateur == 2){ // MODE ADMIN -----------------------------------------------------------------

        println("Mode Admin")
        // on initialise le code admin à 0
        var codeAdmin = 0

        //tant que le code admin n'est pas correct, on redemande à l'utilisateur de rentrer le code
        while (codeAdmin != pin){
          print("Entrez le code PIN : ")
          codeAdmin = readInt()
          if (codeAdmin != pin){
            println("Code PIN incorrect")
          }
        }

        //affichage des stocks actuels
        println("Stock actuel :")
        println("Poudre de café : " + stockCafe + "g")
        println("Lait : " + stockLait + "L")
        println("Sucre : " + stockSucre + "g")

        //ajout de stocks
        println("Ajout de stocks :\nEntrez la quantité de poudre de café à ajouter (g):")
        stockCafe += readInt()
        println("Entrez la quantité de sucre à ajouter (g):")
        stockSucre += readInt()
        println("Entrez la quantité de lait à ajouter (L):")
        stockLait += readDouble()


        //affichage des stocks mis à jour
        println("Ajout :")
        println("Poudre de café : " + stockCafe)
        println("Lait : " + stockLait)
        println("Sucre : " + stockSucre)
        println("Niveau de stock mis à jour")
        println("Retour au menu principal\n")


      } else if (choixUtilisateur == 3){
        enMarche = false
      }

    }

  }

}