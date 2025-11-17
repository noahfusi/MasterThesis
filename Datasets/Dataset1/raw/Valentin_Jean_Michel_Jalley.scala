import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    // Stocks initiaux
    var poudredecafe = 50
    var sucre = 30
    var lait = 0.5
    var panne_rester=true
    //variables de prix pour calculer dynamiquement après
    var prixducafe=0.0
    var prixdusucre=0.0
    var prixdulait=0.0

    //chaine de caractères qui comporte tous les caractères possibles du code alphanumérique à 5 caractères
    val possiblecarac = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val mdpadmin="434343"
    // quantité pour stock
    var quantitelaitselongrandeur = 0.0

    var choixlatte = 0
    var besoincafe = 0
    var besoinlait = 0.0
    var stocksuffisant = true
    var demandesucre = 0

    while(true){  //boucle générale qui permet de toujours revenir à cet endroit lorsque l'on sort des modes 1,2 et 3
      //On déclare ici nos vraiables sur le lait supplémentaire comme ça ils ne restent pas en mémoire

      //Démarrage du distributeur
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      // Demande à l'utilisateur ce qu'il veut faire entre les 3 choix.
      var selection = 0
      do {
        selection = readLine(">").toInt
      } while (!(selection == 1 || selection == 2 || selection == 3))

      // [Mode Client] (Commandes de boissons).
      if (selection == 1) {

        // Étape 1 : choix d'une boisson.
        var choixboisson = 0

        // Demande tant que les stocks sont insuffisants (donc on reste dans la boucle tant que stocksuffisant est faux)
          panne_rester=true
          while(panne_rester){
            var laitsupplementaire = 0.0
            var doselait = 0
            besoinlait=0.0
            besoincafe=0
            println("Veuillez sélectionner votre boisson :")
            println("1) Expresso - CHF 2.00 ")
            println("2) Cappuccino - CHF 2.50 ")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            panne_rester=false
            //Demande et vérification des entrées du client pour savoir si la boisson qu'il a envie
            do {
              choixboisson = readLine(">").toInt
            } while (!(choixboisson == 1 || choixboisson == 2 || choixboisson == 3))

            //Quelle taille pour le latte?
            if (choixboisson == 3) {
              println("Quelle taille voulez-vous pour votre Latte ?")
              println("1) Petit")
              println("2) Moyen")
              println("3) Grand")

              //Associer selon le choix dela taille du Latte une quantité de lait pour faciliter les calculs après
              do {
                choixlatte = readLine(">").toInt
              } while (!(choixlatte == 1 || choixlatte == 2 || choixlatte == 3))
              if (choixlatte == 1) {
                quantitelaitselongrandeur = 0.120
              } else if (choixlatte == 2) {
                quantitelaitselongrandeur = 0.150
              } else if (choixlatte == 3) {
                quantitelaitselongrandeur = 0.200
              }

            }
            // Étape 2 : Le client veut-il du sucre?
            var choixsucre = 0

            println("Quelle dose de sucre voulez-vous ?")
            println("1) Pas de sucre ")
            println("2) Peu de sucre 5[g] ")
            println("3) Moyen 10[g]")
            println("4) Beaucoup 15[g]")
            //Demande et vérification des entrées du client pour savoir si il veut du sucre
            do {
              choixsucre = readLine(">").toInt
            } while (!(choixsucre == 1 || choixsucre == 2 || choixsucre == 3 || choixsucre == 4))

            // Niveau de sucre demandé : si 1 alors 0[g] de sucre
            if (choixsucre == 1) {
              demandesucre = 0
              prixdusucre=0.0
              // Niveau de sucre demandé : si 2 alors 5[g] de sucre
            } else if (choixsucre == 2) {
              demandesucre = 5
              prixdusucre=0.10
              // Niveau de sucre demandé : si 3 alors 10[g] de sucre
            } else if (choixsucre == 3) {
              demandesucre = 10
              prixdusucre=0.20

              // Niveau de sucre demandé : si 3 alors 15[g] de sucre
            } else {
              demandesucre = 15
              prixdusucre=0.30
            }


            // Étape 3 : Le client veut-il du lait ? (pour le cappuccino et la latte) 3 doses maximum.
            var choixlait = 0
            if (choixboisson == 2 || choixboisson == 3) {
              println("Voulez-vous du lait supplémentaire (1 dose = 50[ml]) ?")
              println("1) Oui")
              println("2) Non")

              //Demande et vérification des entrées du client pour savoir si il veut du lait ou pas.
              do {
                choixlait = readLine(">").toInt
              } while (!(choixlait == 1 || choixlait == 2))
            }


            //Combien veut-il de lait ?

            if (choixlait == 1) {
              println("Combien de doses de lait voulez-vous?")
              println("1) 1 doses 50 [ml]")
              println("2) 2 doses 100 [ml]")
              println("3) 3 doses 150 [ml]")
              //Demande et vérification des entrées du client pour les doses de lait.
              do {
                doselait = readLine(">").toInt
              } while (!(doselait == 1 || doselait == 2 || doselait == 3))

              // Niveau de lait demandé : si 1 alors 50[ml] de lait
              if (doselait == 1) {
                laitsupplementaire = 0.050

                // Niveau de lait demandé : si 2 alors 100[ml] de lait
              } else if (doselait == 2) {
                laitsupplementaire = 0.100
                // Niveau de lait demandé : si 2 alors 100[ml] de lait
              } else {
                laitsupplementaire = 0.150
              }
            }
            //initialision des besoins des stocks en fonction de la boisson et du prix
            prixdulait = laitsupplementaire
            if (choixboisson == 1) {  //Espresso
              besoincafe = 8
              prixducafe=2.00
            } else if (choixboisson == 2) { //capuccino
              prixducafe=2.50
              besoincafe = 6
              besoinlait = 0.100 + laitsupplementaire
            } else if (choixboisson == 3 && choixlatte == 1) {  //petit latte
              prixducafe=2.70
              besoincafe = 6
              besoinlait = quantitelaitselongrandeur + laitsupplementaire
            } else if (choixboisson == 3 && choixlatte == 2) {  //moyen latte
              prixducafe=3.20
              besoincafe = 8
              besoinlait = quantitelaitselongrandeur + laitsupplementaire
            } else if (choixboisson == 3 && choixlatte == 3) {  //grand latte
              prixducafe=3.70
              besoincafe = 12
              besoinlait = quantitelaitselongrandeur + laitsupplementaire
            }
            //vérification du stock café insuffisant
            if (besoincafe > poudredecafe) {
              stocksuffisant = false
              println("Quantité de café insuffisante pour préparer la boisson sélectionnée.")
              println("veuillez essayer une autre boisson")
              println("....................................")
              panne_rester=true
              //vérification du stock lait insuffisant
            } else if (besoinlait > lait) {
              stocksuffisant = false
              println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("veuillez essayer une autre boisson")
              println("....................................")
              panne_rester=true
              //vérification du stock sucre insuffisant
            } else if (demandesucre > sucre) {
              stocksuffisant = false
              println("Quantité de sucre insuffisant pour préparer la boisson sélectionnée.")
              println("veuillez essayer avec moins de sucre")
              println("....................................")
              panne_rester=true
              //Stock suffisant
            } else {
              stocksuffisant = true
            }

            if(stocksuffisant){
              //si le stock est suffisant on passe à l'affichage de la boisson puis au paiement
              print("Boisson sélectionnée : ")
              //affichage de la boisson séléctionnée dynamiquement
              if(choixboisson==1){
                println("Espresso")
              }
              else if(choixboisson==2){
                println("Capuccino")
              }
              else if(choixboisson==3){
                print("Latte ")
                if(choixlatte==1)println("(Petit)")
                if(choixlatte==2)println("(Moyen)")
                if(choixlatte==3)println("(Grand)")
              }
              print("Niveau de sucre : ")
              if(choixsucre==1)println("Sans sucre")
              if(choixsucre==2)println("Peu (5g)")
              if(choixsucre==3)println("Moyen (10g)")
              if(choixsucre==4)println("Beaucoup (15g)")

              print("Lait en supplémentaire: ")
              if(doselait==0) println(" Non\n")
              else println(doselait+"\n")

              //Calcul du prix total en affichant selon si l'on a pris du sucre/lait plusieurs +
              printf("Prix total : CHF %.2f",prixducafe)
              if(choixsucre!=1){
                printf(" + CHF %.2f", prixdusucre)
              }
              if(choixlait!=2 && laitsupplementaire!=0.0){
                printf(" + CHF %.2f", prixdulait)
              }
              printf(" = CHF %.2f ", (prixdulait+prixducafe+prixdusucre))
              println("")
              println("Veuillez payer en utilisant Twint.")
              var chaine = ""
              var i = 0
              //tant qu'on a pas créé 5 caractères, on continue à boucler
              while (i < 5) {
                val alphanumerique = Random.nextInt(possiblecarac.length) //créer un indice entre 0 et le nombre de caractères qu'il y a dans la chaîne possible carac définit en haut du programme
                val carac = possiblecarac.charAt(alphanumerique)
                chaine = chaine + carac
                i = i + 1 //itération de i pour en sortir au bout de 5
              }

              println("Votre code de paiement est : "+chaine)
              println("(En attente de paiement...)")
              Thread.sleep(3000)  //attente de 3s
              println("\n Paiement confirmé")
              //le paiement est terminé et validé, on retire alors au stock de la machine nospresso les besoins du cient
              poudredecafe-=besoincafe
              lait-=besoinlait
              sucre-=demandesucre

              println("Préparation de votre boisson...")
              //écriture de la boisson prête selon le choix de la boisson de l'utilisateur
              if(choixboisson==1){
                println("Votre Espresso est prêt ! Bonne dégustation")
              }
              else if(choixboisson==2){
                println("Votre Capuccino est prêt ! Bonne dégustation")
              }
              else if(choixboisson==3) {
                println("Votre Latte est prêt ! Bonne dégustation")

              }
              println("")
            }
          }



      }
      else if(selection==3){
        //Mode EXIT, appel de la fonction exit
        System.exit(0)
      }
      else if(selection==2){
        //Nous entrons dans le mode administrateur pour gérer les stocks

        //D'abord, vérifier la légitimité de l'administrateur avec le code pin

        println("Mode Admin")
        var motdepasse="str"
        do{ //do while boucle pour rentrer le pin tant qu'il est mauvais
          motdepasse = readLine("Entrez le code PIN : ")
          if(motdepasse!=mdpadmin)println("Mot de passe incorrect")
        }while(motdepasse!=mdpadmin)
        println("\nAccès autorisé.\n")

        //Affichage des stocks actuel
        println("Stocks\n")
        println("Poudre de café : "+ poudredecafe+"g")
        printf("Lait           : %.2f L\n",lait)
        println("Sucre          : "+sucre+"g")

        println("Réapprovisionnement des stocks...")
        println("Ajout : \n")

        //Réajustement des stocks avec vérification de l'entrée utilisateur au cas où il mettrait des stocks négatifs
        print("Poudre de café: ")

        //utilisation des méthodes readInt et readDouble pour le lait
        var ajoutpoudrecafe=0
        do{
          ajoutpoudrecafe=readInt()
        }while(poudredecafe<0)

        printf("Lait          : ")
        var ajoutlait=0.0
        do{
          ajoutlait=readDouble()
        }while(ajoutlait<0.0)

        print("Sucre         :")
        var ajoutsucre=0
        do{
          ajoutsucre=readInt()
        }while(ajoutsucre<0)

        println("Niveaux de stock mis à jour.")
        //Addition pour mettre à jour les stocks
        poudredecafe+=ajoutpoudrecafe
        lait+=ajoutlait
        sucre+=ajoutsucre
        println("Retour au menu principal...")
      }
    }

  }
}