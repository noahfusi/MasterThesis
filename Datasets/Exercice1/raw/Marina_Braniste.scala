import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {

    var volumecafe=50   //stock
    var volumedelait=0.50
    var volumesucre=30
    while(true)
    {
      //Grâce au while(true), on peut rester indéfiniment dans la boucle, à moins que l'on utilise des moyens de sortir de la boucle ou des fonctions de système comme pour l'option 3
      //Finalement, c'est très utile pour revenir systématiquement au menu de séléction du mode
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      val choixdumode = readInt() //lecture du mode dans une variable statique
      val chiffre = scala.util.Random.nextInt(10)
      if(choixdumode==3)  //test de la valeur de mode : Si c'est 3 alors quitter le programme
      {
        System.exit(0)
      }
      else if(choixdumode==2) //Si c'est 2 alors entrer dans le mode administrateur
      {
        var codepinentree=""
        val vraipin = "434343"  //code pin défini dans le sujet  Ce mode est protégé par un code PIN à 6 chiffres (par défaut : 434343).
        println("Mode Admin") //Accueil du mode admin
        do
        {
          codepinentree = readLine("Entrez le code PIN : ")
        }while(!(codepinentree==vraipin)) //Tant que les deux codes pin ne sont pas les mêmes et que cette condition n'est pas satisfaite alors, on ne peut pas quitter la boucle

        println("Accès autorisé") //Accueil du mode affichage de stocks
        println("Stocks")
        println("\nPoudre de café : " +volumecafe+"g")
        printf("Lait           : %.2fL\n",volumedelait)
        println("Sucre          : " +volumesucre+"g")

        println("Réapprovisionnement des stocks...")
        println("Ajout: ")

        var ajout=0         //pour stocker le café et le sucre (valeur int)
        var ajoutdouble=0.0 //pour stocker le lait             (valeur double)

        do{
          ajout=readLine("Poudre de café: ").toInt
          if(ajout<0) println("Veuillez entrer une valeur positive")  //Ecriture d'un message à l'utilisateur pour lui dire qu'il a commis une erreur
        }while(ajout<0)
        volumecafe +=ajout  //quand on est sorti de la boucle, on met à jour les stocks

        //Même procédé pour le lait mais en utilisant des comparaisons et des variables de type Double car la saisie doit être un nombre à virgules
        do{
          ajoutdouble=readLine("Lait: ").toDouble
          if(ajoutdouble<0.0) println("Veuillez entrer une valeur positive")
        }while(ajoutdouble<0.0)
        volumedelait +=ajoutdouble

        //Même procédé mais cette fois-ci pour le sucre alors on reprends la même chose que pour le café car c'est le même type
        var exit=true
        do{
          ajout=readLine("Sucre: ").toInt
          if(ajout<0){
            println("Veuillez entrer une valeur positive")
            exit=false
          }
        }while(!exit)
        volumesucre+= ajout
      }
      else if(choixdumode==1) //Si c'est 1 alors entrer dans le mode client
      {
        var restartClientMode = true
        while(restartClientMode){
          restartClientMode = false
          var boiss=0
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50 ")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

          var exit=false
          while(!exit)
          {
            boiss = readLine(" > ").toInt
            if(boiss==1 || boiss==2 || boiss==3) exit=true
          }
          var uniquementlatte = (boiss!=1 && boiss!=2)  //condition pour correspondre au Latte
          var boiss2=0
          if(uniquementlatte)
          {
            println("Veuillez sélectionner la taille de votre boisson :")
            println("1) Petit - CHF 2.70 ")
            println("2) Moyen - CHF 3.20 ")
            println("3) Grand - CHF 3.70 ")
            var exit2=false
            while(!exit2)
            {
              boiss2 = readLine(" > ").toInt
              if(boiss2==1 || boiss2==2 || boiss2==3) exit2=true
            }
          }
          //Cette partie concerne toutes les boissons : demande de sucre
          println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
          var sucre=0
          var supplement=0
          var nbsupplement=0
          do
          {
            sucre = readLine(" > ").toInt
          }while(sucre<1 || sucre>4)  //Sortir quand on est à l'opposé de cette condition
          var niv=""
          if(sucre==1){
            niv="Sans sucre"
          }
          else if(sucre==2){
            niv="Peu (5g)"
          }
          else if(sucre==3){
            niv="Moyen (10g)"
          }
          else if(sucre==4){
            niv="Beaucoup (15g)"
          }
          if(boiss2!=0 || boiss==2) //Si la boisson est UN Capuccino OU un latte
          {
            println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            supplement = readLine(" > ").toInt //saisie convertie en entier
            while (!(supplement == 1 || supplement == 2)) //Tant que la réponse n'est ni 1, ni 2, alors il faut afficher de nouveau le symbole > indiquant que la saisie doit être refaite
            {
              supplement = readLine(" > ").toInt //Saisie à refaire
            }
            if (supplement != 2) //Si la saisie n'était pas Non
            {
              println("Combien de dose ?")
              nbsupplement = readLine(" > ").toInt //saisie convertie en entier
              while (!(nbsupplement == 1 || nbsupplement == 2 || nbsupplement == 3 || nbsupplement == 0)) {
                nbsupplement = readLine(" > ").toInt //Saisie à refaire
              }
            }
          }
          print("Boisson séléctionnée : ")
          var tarif=0.0
          var differenceduLait = 0.0  //Montant qu'il va rester après soustraction de la quantité de lait dans la machine et celle de la boisson commandée
          if(boiss==2){ //Capuccino
            differenceduLait = volumedelait - (nbsupplement+0.10)
            tarif=2.50
            println("Capuccino")
          }
          if(boiss==1){
            differenceduLait = volumedelait
            tarif=2.00
            println("Expresso")
          }
          if(boiss2==1){  //Petit Latte
            differenceduLait = volumedelait - (nbsupplement*0.05 +0.12)
            tarif=2.70
            println("Latte (Petit)")
          }
          if(boiss2==2){  //Moyen
            differenceduLait = volumedelait - (nbsupplement*0.05 +0.15)
            tarif=3.20
            println("Latte (Moyen)")
          }
          if(boiss2==3){  //Grand
            differenceduLait = volumedelait - (nbsupplement*0.05 +0.20)
            tarif=3.70
            println("Latte (Grand)")
          }
          //Arrondir pour avoir un résultat à deux nb après la virgule
          differenceduLait=(differenceduLait*100).round
          differenceduLait=differenceduLait/100.0
          var differenceduCafe=0
          if(boiss==2 || boiss2==1){  //Expresso ou petit latte
            differenceduCafe = volumecafe - 6
          }
          if(boiss==1 || boiss2==2){  //capuccino ou moyen latte
            differenceduCafe = volumecafe - 8
          }
          if(boiss2==3){
            differenceduCafe = volumecafe - 12
          }
          var differenceduSucre = (volumesucre - ((sucre-1)*5))
          println("Niveau de sucre : "+niv) //Affichage du niveau de sucre

          print("Lait en supplément : ")
          if(supplement==1){
            println(nbsupplement) //Affichage du niveau de lait en supplément
          }
          else{
            println("Non")  //Si supplément est différent de 1, il vaut obligatoirement 2 soit un refus de l'utilisateur de prendre du lait en plus
          }
          if(differenceduSucre>=0 && differenceduLait>=0.0 && differenceduCafe>=0)
          {
            //Cela signifie que on a respectivement toutes les quantités nécessaires pour produire la boisson et encaisser le client
            print("Prix total: ")
            printf("CHF %.2f",tarif)
            if(sucre!=1){
              printf(" + CHF %.2f",(((volumesucre-differenceduSucre).toDouble)*2)/100.0)  //Afficher la dose de sucre
            }
            if(supplement==1){
              printf(" + CHF %.2f", (nbsupplement/20.0))  //Afficher le lait supplémentaire
            }
            var tt=(nbsupplement/20.0)+tarif+(((volumesucre-differenceduSucre).toDouble)*2)/100.0 //Calcul du tarif tout compris
            printf(" = CHF %.2f",tt)  //Affichage tarif tt
            println("\n")
            //Création d'un code aléatoire twint avec 2 lettres + 1 chiffre dans une partie et 2 lettres dans l'autre
            val partie1:String = (('A' + scala.util.Random.nextInt(26)).toChar).toString + (('A' + scala.util.Random.nextInt(26)).toChar).toString + chiffre.toString
            val partie2:String = (('A' + scala.util.Random.nextInt(26)).toChar).toString + (('A' + scala.util.Random.nextInt(26)).toChar).toString
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " +partie1+partie2)
            println("(En attente de paiement...)")
            Thread.sleep(3000) //On attend 3 secondes pour que la paiement aboutisse
            println("")
            println("Paiement confirmé\nPréparation de votre boisson...")
            print("Votre ")
            if(boiss==1) println("Expresso est prêt ! Bonne dégustation !")
            if(boiss==2) println("Capuccino est prêt ! Bonne dégustation !")
            if(boiss2!=0) println("Latte est prêt ! Bonne dégustation !")

            volumecafe=differenceduCafe
            volumesucre=differenceduSucre
            volumedelait=differenceduLait
          }
          else if(differenceduSucre<0)
          {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une plus faible quantité ou vérifier les stocks en mode Admin.")
            restartClientMode=true
          }
          else if(differenceduCafe<0)
          {
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            restartClientMode=true
          }
          else if(differenceduLait<0.0)
          {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            restartClientMode=true
          }
          println("")
        }

      }

    }
  }
}