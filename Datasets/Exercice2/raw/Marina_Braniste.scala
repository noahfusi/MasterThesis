import scala.io.StdIn._

object Main {

  //Fonction updatePin. On envoie l'ID et le tableaux de PIN. Dans la fonction, on demande et stocke un nouveau mot de passe si il fait 6 de longueurs. Puis, on revient au menu après modification
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    print("Mise à jour du PIN pour la Machine ")
    println(""+ (machineId+1))
    var nouveau_code_pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ") //saisie
    if(nouveau_code_pin.length==6){ //si le code fait 6 chiffres, ne rien faire et reprendre après le else
    }
    else{ // < à 6 ou > à 6
      while(nouveau_code_pin.length!=6){
        nouveau_code_pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
    }
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")
    machinePins(machineId) = nouveau_code_pin //Mise à jour
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels :")
    println("\nPoudre de café : " +coffeeStocks(machineId)+"g")
    println("Sucre : " +sugarStocks(machineId)+"g")
    var conversion = milkStocks(machineId)/1000.0 //création d'une variable temporaire qui stock le lait sous forme de Litres et non milliLitres
    printf("Lait : %.2fL\n",conversion)

    println(" Entrez les quantités à ajouter :")

    var ajout=0         //pour stocker le café et le sucre (valeur int)
    var ajoutdouble=0.0 //pour stocker le lait             (valeur double)

    do{
      ajout=readLine("Poudre de café > ").toInt
      if(ajout<0) println("Veuillez entrer une valeur positive")  //Ecriture d'un message à l'utilisateur pour lui dire qu'il a commis une erreur
    }while(ajout<0)
    coffeeStocks(machineId) +=ajout  //quand on est sorti de la boucle, on met à jour les stocks



    //Même procédé mais cette fois-ci pour le sucre alors on reprends la même chose que pour le café car c'est le même type
    var exit=true
    do{
      ajout=readLine("Sucre > ").toInt
      if(ajout<0){
        println("Veuillez entrer une valeur positive")
        exit=false
      }
    }while(!exit)

    //Même procédé pour le lait mais en utilisant des comparaisons et des variables de type Double car la saisie doit être un nombre à virgules
    do{
      ajoutdouble=readLine("Lait > ").toDouble
      if(ajoutdouble<0.0) println("Veuillez entrer une valeur positive")
    }while(ajoutdouble<0.0)
    milkStocks(machineId) +=(ajoutdouble*1000).toInt  //Re-conversion sous format mL

    sugarStocks(machineId)+= ajout
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var valider:Boolean=false //variable à retourner
    print("Entrez le code PIN : ")
    var codepinentree = readLine()  //Stockage du codePIN
    if (machinePins(machineId)==codepinentree){ //vérification de similitude
      valider = true
    }
    else {
      valider=false
    }
    return valider  //Return bool
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var restartClientMode = true
    val chiffre = scala.util.Random.nextInt(10)
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
      var differenceduLait = 0  //Montant qu'il va rester après soustraction de la quantité de lait dans la machine et celle de la boisson commandée
      if(boiss==2){ //Capuccino
        differenceduLait = milkStocks(machineId) - ((nbsupplement/20.0 +0.10)*1000).toInt
        tarif=2.50
        println("Capuccino")
      }
      if(boiss==1){
        differenceduLait = milkStocks(machineId)
        tarif=2.00
        println("Expresso")
      }
      if(boiss2==1){  //Petit Latte
        differenceduLait = milkStocks(machineId) - ((nbsupplement/20.0 +0.12)*1000).toInt
        tarif=2.70
        println("Latte (Petit)")
      }
      if(boiss2==2){  //Moyen
        differenceduLait = milkStocks(machineId) - ((nbsupplement/20.0 +0.15)*1000).toInt
        tarif=3.20
        println("Latte (Moyen)")
      }
      if(boiss2==3){  //Grand
        differenceduLait = milkStocks(machineId) - ((nbsupplement/20.0 +0.20)*1000).toInt
        tarif=3.70
        println("Latte (Grand)")
      }
      //Arrondir pour avoir un résultat à deux nb après la virgule
      var differenceduCafe=0
      if(boiss==2 || boiss2==1){  //Expresso ou petit latte
        differenceduCafe = coffeeStocks(machineId) - 6
      }
      if(boiss==1 || boiss2==2){  //capuccino ou moyen latte
        differenceduCafe = coffeeStocks(machineId) - 8
      }
      if(boiss2==3){
        differenceduCafe = coffeeStocks(machineId) - 12
      }
      var differenceduSucre = (sugarStocks(machineId) - ((sucre-1)*5))
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
          printf(" + CHF %.2f",(((sugarStocks(machineId)-differenceduSucre).toDouble)*2)/100.0)  //Afficher la dose de sucre
        }
        if(supplement==1){
          printf(" + CHF %.2f", (nbsupplement/20.0))  //Afficher le lait supplémentaire
        }
        var tt=(nbsupplement/20.0)+tarif+(((sugarStocks(machineId)-differenceduSucre).toDouble)*2)/100.0 //Calcul du tarif tout compris
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

        coffeeStocks(machineId)=differenceduCafe
        sugarStocks(machineId)=differenceduSucre
        milkStocks(machineId)=differenceduLait
        restartClientMode=false
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
    return !restartClientMode
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var volumecafe= Array.fill(nbMachines)(50)   //stock
    var volumedelait=Array.fill(nbMachines)(500)
    var volumesucre=Array.fill(nbMachines)(30)

    var machinePins = Array.fill(nbMachines)("434343")  //pin par défaut pour les 5 machines
    while(true)
    {
      //Grâce au while(true), on peut rester indéfiniment dans la boucle, à moins que l'on utilise des moyens de sortir de la boucle ou des fonctions de système comme pour l'option 3
      //Finalement, c'est très utile pour revenir systématiquement au menu de séléction du mode
      println("         Nospresso Café")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      val choixdumode = readInt() //lecture du mode dans une variable statique
      if(choixdumode==3)  //test de la valeur de mode : Si c'est 3 alors quitter le programme
      {
        System.exit(0)
      }
      //L'option 3 est écartée, on peut donc demander la machine à séléctionner
      var machine_id=0
      while(machine_id<1 || machine_id>5){
        machine_id = readLine("Machine séléctionnée (1-5) > ").toInt
      }

      if(choixdumode==2) //Si c'est 2 alors entrer dans le mode administrateur
      {
        var maj=0
        var test=3
        while(!validatePin(machine_id-1,machinePins)){  //tant que l'utilisateur valide pas le code on lui redemande
          test = test - 1 //Chaque fois on enlève 1 et on affiche combien il en reste
          println("Code PIN Incorrect. "+test+" tentatives restantes.")
          if(test==0){  //Quand on atteint 0, on quitte le programme
            println("Trop de tentatives échouées. Fin du programme.")
            sys.exit(0)
          }
        }
        println("Accès autorisé à la machine "+machine_id) //Accueil du mode affichage de stocks
        println("Veuillez séléctionner une option du mode administrateur :")
        println("1) Mettre à jour le code PIN")
        println("2) Mettre à jour les ingrédients")
        while(!(maj==1 || maj==2)){
          print("> ")
          maj = readInt() //lecture entier
        }
        if(maj==1){
          //appeler la maj du code pin
          updatePin(machine_id-1,machinePins)
        }
        if(maj==2){
          //appeler la maj des stocks
          restockMachine(machine_id-1,volumecafe,volumesucre,volumedelait)
        }
      }
      else if(choixdumode==1) //Si c'est 1 alors entrer dans le mode client
      {
        if(serveClient(machine_id-1,volumecafe,volumesucre,volumedelait)==false){
          println("Erreur. Veuillez choisir une autre machine, celle-ci manque d'ingrédients.")
        }
      }

    }
  }
}