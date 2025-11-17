import scala.util.Random
import scala.io.StdIn._

/*
Au cours du programme, tous les while() et les do while (sauf le premier) que nous allons utiliser
sont fait pour vérifier les saises utilisateur, pour éviter qu'elle soit négatives, ou qu'elles ne correspondent à aucune option
 */
object Main {
  def main(args: Array[String]): Unit = {
    val expresso=1
    val capuccino=2
    val latte=3

    var departsucre=30
    var departlait=0.5
    var departcafe=50

    var rester="oui"
    var bloque1=false
    var bloque2=false
    var bloque3=false
    val chiffre_et_alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val admin="434343"
    while(rester=="oui")  //tant que la variable rester vaut oui, on reste dedans, lorsqu'elle vaudra non, il faudra quitter le programme
    {
      var cafedesire=0
      var sucredesire=0
      var laitdesire=0.0
      var decision=0
      if(!bloque1 && !bloque2 && !bloque3) {
        //Affichage classique du menu nospresso
        println("            Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")

        decision = readLine(">").toInt //lecture de l'entrée utilisateur
        var bonnedecision = (decision == 1 || decision == 2 || decision == 3) //si l'utilisateur a fait un de ces choix, il s'agit d'une bonne décision
        while (!bonnedecision) { //tant que la décision n'est pas respecté, on répète le processus
          decision = readLine(">").toInt
          bonnedecision = (decision == 1 || decision == 2 || decision == 3)
        }
      }
        if(decision==1 || (bloque1 || bloque2 || bloque3)) //Soit il est en mode bloqué car il manque un ingrédient, soit il l'a choisi de lui-même
        { //Si l'utilisateur rentre en mode client, on affiche la séléction des boissons
          var montant:Double=0.0  //prix
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Capuccino - CHF 2.50")
          println("3) Latte- CHF 2.70 (Small), CHF 3.20 (Medium), CHF 3.70 (Large)")
          var decisionboisson=0
          do{//vérification des saisies
            decisionboisson = readLine(">").toInt
          }while(!(decisionboisson==expresso || decisionboisson==capuccino || decisionboisson==latte))
          var latteTaille=0
          if(decisionboisson==latte)
          {
            //Si le Latte est choisi, avant de demander la dose de sucre, il faut enregistrer la taille du latte
            println("\nVeuillez sélectionner la taille du Latte :")
            println("1) Petit Latte - CHF 2.70")
            println("2) Moyen Latte - CHF 3.20")
            println("3) Grand Latte - CHF 3.70")
            do{
              latteTaille = readLine(">").toInt
            }while(latteTaille<1 || latteTaille>3)
            //Valeurs par défaut pour un petit latte
            laitdesire = 0.12
            cafedesire = 6
            montant=2.70
            if(latteTaille==2){
              laitdesire=0.15 //Consomme 0.15L de lait pour un moyen
              cafedesire = 8
              montant += (latteTaille*0.25) //montant = 2.70 + 2*0.25 = 3.20
            }
            if(latteTaille==3){
              laitdesire=0.2
              cafedesire = cafedesire*2 //Café vaut 12g pour un grand latte donc 6*2
              montant += 1.00
            }
          }//Latte choix
          else{
            if(decisionboisson==expresso){
              cafedesire=8  //g de poudre café dans un expresso
              montant=2.00
            }
            else{ //CAPUCCINO
              cafedesire=6
              laitdesire=0.10 //L de lait dans capuccino
              montant=2.50
            }
          }//FIin de expresso & capuccino
          //Demande de sucre
          println("\nSouhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          var sucre_souhait=0
          sucre_souhait = readLine(">").toInt
          while(sucre_souhait!=1 && sucre_souhait!=2 && sucre_souhait!=3 && sucre_souhait!=4)
          {
            sucre_souhait = readLine(">").toInt
          }
          var stringNiveau = "Sans sucre"
          sucredesire=0
          if(sucre_souhait==2){
            stringNiveau="Peu (5g)"
            sucredesire=5
          }
          else if(sucre_souhait==3){
            stringNiveau="Moyen (10g)"
            sucredesire=10
          }
          else if(sucre_souhait==4){
            stringNiveau="Beaucoup (15g)"
            sucredesire=15
          }
          var laitcomp=0
          //La demande est validée, la saisie est bonne
          if(decisionboisson==capuccino || decisionboisson==latte)  //Vérification de la boisson pour proposer ou non du lait
          {
            println("\nSouhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            do{
              laitcomp = readLine(">").toInt
            }while(!(laitcomp==1 || laitcomp==2))
          }
          var portion=0
          if(laitcomp==1)
          {
            println("\nCombien de doses ?")
            portion = readLine(">").toInt
            while(portion<0 || portion>3){
              portion = readLine(">").toInt
            }
          }
          //Récap de la commande client
          print("Boisson séléctionnée : ")
          if(decisionboisson==capuccino){
            println("Capuccino")
          }
          else if(decisionboisson==expresso){
            println("Expresso")
          }
          else if(decisionboisson==latte && latteTaille==1){
            println("Latte (Petit)")
          }
          else if(decisionboisson==latte && latteTaille==2){
            println("Latte (Moyen)")
          }
          else if(decisionboisson==latte && latteTaille==3){
            println("Latte (Grand)")
          }
          val convlait:Double = (portion/20.0)
          laitdesire+=convlait  //total de lait désiré quand on y ajoute les doses de lait supplémentaires.

          println("Niveau de sucre : "+stringNiveau)
          if(portion==0){
            println("Lait en supplément : Non")
          }
          else{
            println("Lait en supplément : " + portion)
          }

          if(laitdesire>departlait)
          {
            bloque1=true
            println("")
            println("Erreur : Quantité de lait insuffisante pour préparer\n la boisson sélectionnée.")
            println("Veuillez choisir une dose plus petite ou essayer\n une autre boisson.")
          }
          else{
            bloque1=false
          }
          if(sucredesire>departsucre)
          {
            bloque2=true
            println("")
            println("Erreur : Quantité de sucre insuffisante pour préparer\n la boisson sélectionnée.")
            println("Veuillez choisir une quantité plus faible de sucre dans votre boisson ou vérifier les\n stocks en mode Admin.")
          }
          else{
            bloque2=false
          }
          if(cafedesire>departcafe)
          {
            println("")
            bloque3=true
            println("Erreur : Quantité de poudre de café insuffisante pour\n préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin.")
          }else{
            bloque3=false
          }
          if(!bloque1 && !bloque2 && !bloque3)
          {
            //Si bloque = false alors on peut tout à fait passer au paiement
            print("\nPrix total : CHF ")
            printf("%.2f",montant)
            if(stringNiveau!="Sans sucre"){
              print(" + CHF ")
              printf("%.2f",sucredesire/50.0)
            }
            if(portion>0){
              print(" + CHF ")
              printf("%.2f",portion/20.0)
            }
            print(" = CHF ")
            montant += sucredesire/50.0 + portion/20.0
            printf("%.2f\n\n",montant)

            println("Veuillez payer en utilisant Twint.")
            //On retire à la quantité de départ (50,0.5,30) la quantité des ingrédients consommés au cours de la commande
            departsucre = departsucre - sucredesire
            departcafe = departcafe - cafedesire
            departlait = departlait - laitdesire
            var codealeatoire=""
            //Génération aléatoire d'un code Twint composé de 5 caractères alphanumériques (chiffres et lettres mélangés).
            // On créer une boucle for qui va jusqu'à 5 (exclus) et qui commence à 0 (inclus) et à chaque tour de boucle
            //On créer un numéro aléatoire qui représente l'index d'un caractère alphanumérique, on tire ce caractère et on l'ajoute à une chaîne de caractères qui s'incrémente à chaque itération
            for(i<-0 until 5)
            {
              var nombrealea:Int = Random.nextInt(chiffre_et_alphabet.length)
              var resultat:Char = chiffre_et_alphabet.charAt(nombrealea)
              codealeatoire += resultat
            }
            print("Votre code de paiement est : ")
            println(codealeatoire)
            println("(En attente de paiement...)")

            Thread.sleep(3000)  //Fonction donnée dans le sujet pour attendre 3000ms

            println("")
            println("Paiement confirmé.")
            println("Préparation de votre boisson...")
            if(decisionboisson==capuccino){
              println("Votre Capuccino est prêt ! Bonne dégustation !\n")
            }
            else if(decisionboisson==latte)
            {
              println("Votre Latte est prêt ! Bonne dégustation !\n")
            }
            else if(decisionboisson==expresso)
            {
              println("Votre Expresso est prêt ! Bonne dégustation !\n")
            }
          }
        }//fin du mode client
        else if(decision==2)//début du mode admin
        {
          println("Mode Admin")
          var codepinpardefaut = readLine("Entrez le code PIN : ")
          while(admin!=codepinpardefaut){ //comparaison entre le code pin par défaut et celui rentré
            codepinpardefaut = readLine("Entrez le code PIN : ")
          }
          println("Accès autorisé\n")
          println("Stocks:")
          //Stocks disponibles
          println("Poudre de café: "+departcafe+"g")
          printf("Lait : %.2fL",departlait)
          print("\nSucre : "+departsucre)
          println("g")

          println("Réapprovisionnement des stocks..")
          println("Ajout : ")

          var poudreplus = 0
          print("   Poudre de café: ")
          poudreplus = readInt()  //lecture
          while(poudreplus<0){
            poudreplus = readLine("   Poudre de café: ").toInt
          }

          var laitplus = readLine("   Lait : ").toDouble
          while(!(laitplus>=0.0)){
            laitplus = readLine("   Lait: ").toDouble
          }

          print("   Sucre: ")
          var sucreplus = readInt()
          while(sucreplus<0){
            sucreplus = readLine("   Sucre: ").toInt
          }

          //Augmentation des stocks avec les ajouts
          departsucre += sucreplus
          departlait += laitplus
          departcafe += poudreplus
          println("Niveaux de stock mis à jour.") //indication
          println("Retour au menu principal...")
          println("")
          }//fin du mode admin
        else if(decision==3)
        {
          rester="non"
        }
        //Fin des modes
      }
    }
}