import scala.util.Random
import scala.io.StdIn.readDouble  //pour le lait qui utilise une quantité à virgule
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    //Assignation des valeurs des trois provisions de départ, 30g de sucre, 0.500L de lait et 50g de poudre de café
    var provision = 30
    var provisionlait = 0.5
    var provisiondecafe = 50

    val lettresMAJ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" //Alphabet
    val chiffres = "1234567890" //Chiffres
    val adminPIN = "434343" //Code PIN de l'administrateur

    while (true) {
      var breuvage = 0
      var sucre_quantite = 0
      var action = 0
      println("            Nospresso Café")
      println("Veuillez séléctionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println("> ")  //Indiquer au client qu'il a une saisie à effectuer
      action = readInt()  //Lire le chiffre qu'il entre
      while (action != 1 && action != 2 && action != 3) { //On va vérifier que le chiffre saisi correspond à une des options
        action = readLine("> ").toInt  //Si ça ne correspond pas, afficher > et lire la saisie + la convertir en entier
      }

      if (action == 3) {  //QUITTER
        println("A bientôt !")
        sys.exit(0) //Fonction permettant de quitter le programme
      }
      if (action == 1 ) { //Si le choix est de prendre le mode client, écriture du mode client
        var recommencer = 1 //pour se répérer et rentrer en boucle dans le mode client quand les stocks sont insuffisants
        while(recommencer==1){  //Rester en boucle insuffisance
          var vote = 0
          var laitdose = 0.0  //reinitialiser
          sucre_quantite=0  //on reinitialise au cas ou on n'en prend pas
          //Séléction de la boisson : affichage
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Capuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          while (vote<1 || vote>3) {
            println(" > ")
            vote = readInt()  //Récupération de la valeur
          }
          recommencer=0 //pour ne plus revenir dans cette boucle sauf en cas d'insuffisance
          var typedelatte = 0
          if (vote == 3) {  //Séléction d'un Latte
            println("Veuillez séléctionner la taille de votre Latte :")
            println("1) Petit (CHF 2.70)")
            println("2) Moyen (CHF 3.20)")
            println("3) Grand (CHF 3.70)")
            while (typedelatte !=1 && typedelatte !=2 && typedelatte !=3) {
              println(" > ")
              typedelatte = readInt() //Récupération de la valeur de la taille du Latte
            }
          }
          //Séléction du sucre : affichage
          var sucredec = 0
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          while (!(sucredec==1 || sucredec==2 || sucredec==3 || sucredec==4)) { //Tant que c'est pas vrai, retapper la saisie
            sucredec = readLine(">").toInt
          }
          //Quand la saisie est vraie, il faut associer les bons montants aux bons choix
          if (sucredec == 2){
            sucre_quantite = 5
          }
          if (sucredec == 3){
            sucre_quantite = 10
          }
          if (sucredec == 4){
            sucre_quantite = 15 //15g pour beaucoup
          }

          var laitSupp = 0
          var yes_lait = 0
          if (vote == 3 || vote == 2) { //Bloc de code qui ne concerne que les latte et les capuccino
            println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
            do{
              yes_lait = readLine(">").toInt
            }while (yes_lait!= 1 && yes_lait!= 2) //boucler a chaque réponse différente de 1 et 2

            if (yes_lait == 2) {  //CHOIX : NON
              laitSupp=0  //pas de lait donc 0mL
            }
            else{ //CHOIX : OUI
              println("Combien de dose ? (Maximum : 3)")
              while (laitSupp>3  || laitSupp <1) {
                println(">")
                laitSupp = readInt()  //lecture entre 1 et 3 compris
              }
            }
          }
          //Affichage de la commande avec la boisson, le sucre et le lait. Ca permet de rappeler à l'utilisateur ce qu'il a pris et justfier le prix qui s'affiche en bas après s'il y a du stock
          print("Boisson séléctionée : ")
          if(vote==1){
            breuvage = 8  //dose de poudre de café dans l'expresso
            println("Expresso")
          }
          if(vote==2){
            breuvage = 6
            laitdose = 0.10 + laitSupp / 20.0 //dose de lait du capuccino
            println("Capuccino")
          }
          if(typedelatte==1){
            laitdose = 0.12 + laitSupp / 20.0
            println("Latte (Petit)")
            breuvage = 6
          }
          if (typedelatte == 3) {
            laitdose = 0.2 + laitSupp / 20.0
            println("Latte (Grand)")
            breuvage = 12
          }
          if (typedelatte == 2) {
            laitdose = 0.15 + laitSupp / 20.0
            println("Latte (Moyen)")
            breuvage = 8
          }
          print("Niveau de sucre : ")
          if(sucre_quantite==0){
            println("Sans sucre")
          }
          else if(sucre_quantite==15){
            println("Beaucoup (15g)")
          }
          else if(sucre_quantite==10){
            println("Moyen (10g)")
          }
          else if(sucre_quantite==5){
            println("Peu (5g)")
          }
          print("Lait supplémentaire:")
          if(laitSupp==0) println(" Non ")
          else{
            println(" "+laitSupp)
          }

          //Soustractions pour vérifier si après soustraction des quantités demandées, il y aura encore du stock
          if (provisionlait - laitdose < 0.0){
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            recommencer=1 //insuffisance
          }
          if (provision - sucre_quantite  <0){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une dose plus faible dans votre boisson ou vérifier les stocks en mode Admin.")
            recommencer=1 //insuffisance
          }
          if (provisiondecafe - breuvage <0){
            recommencer=1 //rester dans la boucle d'insuffisance
            println("Erreur : Quantité de poudre de café insuffisante pour\n préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin.")
          }

          if (recommencer==0) { //si recommencer = 0 il y a du stock et on peut procéder au paiement twint de la boisson
            var prixsucre = (sucre_quantite*2)/100.0  //prix du sucre en ,
            var prixlait = (laitSupp*5)/100.0 //prix du lait
            var prixfin = prixsucre+prixlait  //prix du lait et du sucre combiné
            if(vote==1){
              print("Prix total: CHF 2,00")
              prixfin+=2.00 //ajouter la base café à l'addition
            }
            else if(vote==2){
              print("Prix total: CHF 2,50")
              prixfin+=2.50
            }
            if(typedelatte==1){
              print("Prix total: CHF 2,70")
              prixfin+=2.70 //ajouter 2,70 pour un petit latte
            }
            if(typedelatte==2){
              print("Prix total: CHF 3,20")
              prixfin+=3.20
            }
            if(typedelatte==3){
              print("Prix total: CHF 3,70")
              prixfin+=3.70
            }

            if(!(sucre_quantite==0)){ //Si il n'est pas "sans sucre", afficher l'addition du prix du sucre
              printf(" + CHF %.2f",prixsucre)
            }
            if(!(laitSupp==0)){ //même chose pour le lait, afficher addition
              printf(" + CHF %.2f",prixlait)
            }
            printf(" = CHF %.2f", prixfin)  //calcul final

            println("")
            println("\nVeuillez payer en utilisant Twint.") //mode de paiement

            var payer = ""  //code payer
            var parcours=0
            //Boucle de création de code aléatoire avec des caractères alphanumériques
            while(parcours<5) {  //tant que on a pas fait 5x la boucle pour obtenir 5 lettres, on recommence
              var pif = Random.nextInt(26+10)  //nombre aléatoire
              payer = payer + (lettresMAJ+chiffres).charAt(pif)  //séléction de la lettre associée
              parcours = parcours+1
            }
            //5 itérations finies
            print("Votre code de paiement est : ")
            println(payer)  //affichage du code
            println("(En attente de paiement...)")
            Thread.sleep(3000)  //attente 3 secondes
            println("\nPaiement confirmé. Préparation de votre boisson...")
            print("Votre")
            if(vote==3){
              println("Latte est prêt ! Bonne dégustation")
            }
            else if(vote==2){
              println("Capuccino est prêt ! Bonne dégustation")
            }
            else{
              println("Expresso est prêt ! Bonne dégustation")
            }
            //enlever les montants
            provisionlait = provisionlait-laitdose  //lait réduit
            provisiondecafe -= breuvage //cafe reduit
            provision -= sucre_quantite //sucre reduit
          }
        }
      } else if (action == 2) {
        //Mode administrateur, après avoir entré un bon mot de passe, il est possible de remettre à niveau les stocks
        var reapps=0
        var reappl:Double=0
        var reappc=0  //variables pour la mise a jour des stocks après le mot de passe
        println("Mode Admin")
        //Bloc concernant le mot de passe
        println("Entrez le code PIN :")
        var pinclient = readLine()  //Lecture PIN
        while (!(adminPIN==pinclient)) {  //Comparaison
          pinclient = readLine()
        }
        //Fin du bloc mot de passe
        println("\nAccès autorisé.")
        println("Stocks : \n")
        printf("     Poudre de café : %dg\n     Lait : %.2fL\n     Sucre : %dg\n",provisiondecafe,provisionlait,provision)  //Affichage des stocks disponibles avec printf et les formats d'affichage
        println("\nRéapprovisionnement des stocks...\nAjout : ")
        print("     Poudre de café : ")
        //Réapprovisionnement des trois stocks, impossible de mettre des valeurs <0, en revanche, pas de maximum mais il faut gérer la saisie donc utilisation de 3 do while
        do{
          reappc = readInt()
        }while(reappc<0)
        provisiondecafe += reappc //ajout au stock de café
        print("     Lait : ")
        do{
          reappl = readDouble() //lire en double
        }while(reappl<0.0)
        provisionlait += reappl //ajout au stock de lait
        print("     Sucre : ")
        do{
          reapps = readInt()
        }while(reapps<0)
        provision += reapps //ajout au stock de sucre
        println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")  //Dernier message avant de revenir au mode de séléction menu
      }
    }
  }
}
