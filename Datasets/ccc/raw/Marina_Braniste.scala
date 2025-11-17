import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    //Fonction addIngredient. On envoie l'ingredient en string et le montant en entier. Dans la fonction, on regarde quel est l'ingrédient (café,sucre,lait) et quand ça correspond à l'un des trois on ajoute le montant
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if(ingredient=="cafe"){
        coffee+=amount
      }
      else if(ingredient=="sucre") {
        sugar+=amount
      }
      else if(ingredient=="lait") {
        milk+=amount
      }
    }
    //Fonction removeIngredient. On envoie l'ingredient en string et le montant en entier. Dans la fonction, on regarde quel est l'ingrédient (café,sucre,lait) et quand ça correspond à l'un des trois on enlève le montant
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      var bool=false  //Déclaration d'un booléen qui prendra la valeur true que si l'ingrédient est trouvé, sinon il reste tel qu'il est déclaré (false)
      if (ingredient=="cafe" && coffee>=amount) {
        coffee =amount
        bool=true
      }
      else if (ingredient=="sucre" && sugar>=amount) {
        sugar =amount
        bool=true
      }
      else if (ingredient=="lait" && milk>=amount) {
        milk =amount
        bool=true
      }
      return bool
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var iterateur=0
    var machinesv1 = ArrayBuffer[Machine]()
    try {
      //Bloc de code testé, si une exception est levée, elle sera envoyée au bloc catch qui traitera l'exception
      val fichier_lu = Source.fromFile(filename)
      val lignes = fichier_lu.reset.getLines
      lignes.next //On passe la première ligne
      while (!lignes.isEmpty) { //Tant qu'il reste des lignes dans le parcours du fichier, alors on continue de boucler, quand on arrivera à la dernière ligne et qu'il n'y aura plus de contenu, on sortira de la boucle
        val ligne = lignes.next
        val fragment = ligne.split(",")  //Chaque virgule est un attribut de la classe Machine à créer après
        machinesv1 += new Machine((iterateur+1), fragment(0), fragment(1).toInt, fragment(2).toInt, fragment(3).toInt)  //On utilise l'itérateur comme identifianten le faisant démarreer à 1 pour la première machine et non à 0
        iterateur = iterateur + 1 //on itère et passe à l'identifiant suivant
        println("Machine "+iterateur+" chargée:")
        println("   ID: "+iterateur)
        println("   Code PIN: "+fragment(0))
        printf("   Lait: %.3fL\n",fragment(1).toInt/1000.0)
        println("   Sucre: "+ fragment(2).toInt+"g")
        println("   Café: "+fragment(3).toInt+"g\n")
      }
      fichier_lu.close()
    } catch { //Traitement des exceptions levées et envoyées par le bloc try : 3 exceptions possibles
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule\n",filename)
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule\n",filename)
        println("Fermeture du programme.")
        System.exit(0)
    }
    return machinesv1  //on retourne le tableaux une fois rempli
  }


  //Fonction updatePin. On envoie l'ID et le tableaux de PIN. Dans la fonction, on demande et stocke un nouveau mot de passe si il fait 6 de longueurs. Puis, on revient au menu après modification
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
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
    machines(machineId).pincode = nouveau_code_pin //Mise à jour
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Niveaux de stock actuels :")
    println("\nPoudre de café : " +machines(machineId).coffee+"g")
    println("Sucre          : " +machines(machineId).sugar+"g")
    var conversion = machines(machineId).milk/1000.0 //création d'une variable temporaire qui stock le lait sous forme de Litres et non milliLitres
    printf("Lait           : %.2fL\n",conversion)

    println(" Entrez les quantités à ajouter :")

    var ajout=0         //pour stocker le café et le sucre (valeur int)
    var ajoutdouble=0.0 //pour stocker le lait             (valeur double)

    do{
      ajout=readLine("Poudre de café > ").toInt
      if(ajout<0) println("Veuillez entrer une valeur positive")  //Ecriture d'un message à l'utilisateur pour lui dire qu'il a commis une erreur
    }while(ajout<0)
    machines(machineId).addIngredient("cafe",ajout) //quand on est sorti de la boucle, on met à jour les stocks


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
    machines(machineId).addIngredient("lait",(ajoutdouble*1000).toInt)  //Re-conversion sous format mL

    machines(machineId).addIngredient("sucre",ajout)

  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var valider:Boolean=false //variable à retourner
    print("Entrez le code PIN : ")
    var codepinentree = readLine()  //Stockage du codePIN
    if (machines(machineId).pincode==codepinentree){ //vérification de similitude
      valider = true
    }
    else {
      valider=false
    }
    return valider  //Return bool
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
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
        differenceduLait = machines(machineId).milk - ((nbsupplement/20.0 +0.10)*1000).toInt
        tarif=2.50
        println("Capuccino")
      }
      if(boiss==1){
        differenceduLait = machines(machineId).milk
        tarif=2.00
        println("Expresso")
      }
      if(boiss2==1){  //Petit Latte
        differenceduLait = machines(machineId).milk - ((nbsupplement/20.0 +0.12)*1000).toInt
        tarif=2.70
        println("Latte (Petit)")
      }
      if(boiss2==2){  //Moyen
        differenceduLait = machines(machineId).milk - ((nbsupplement/20.0 +0.15)*1000).toInt
        tarif=3.20
        println("Latte (Moyen)")
      }
      if(boiss2==3){  //Grand
        differenceduLait = machines(machineId).milk - ((nbsupplement/20.0 +0.20)*1000).toInt
        tarif=3.70
        println("Latte (Grand)")
      }
      //Arrondir pour avoir un résultat à deux nb après la virgule
      var differenceduCafe=0
      if(boiss==2 || boiss2==1){  //Expresso ou petit latte
        differenceduCafe = machines(machineId).coffee - 6
      }
      if(boiss==1 || boiss2==2){  //capuccino ou moyen latte
        differenceduCafe = machines(machineId).coffee - 8
      }
      if(boiss2==3){
        differenceduCafe = machines(machineId).coffee - 12
      }
      var differenceduSucre = (machines(machineId).sugar - ((sucre-1)*5))
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
          printf(" + CHF %.2f",(((machines(machineId).sugar-differenceduSucre).toDouble)*2)/100.0)  //Afficher la dose de sucre
        }
        if(supplement==1){
          printf(" + CHF %.2f", (nbsupplement/20.0))  //Afficher le lait supplémentaire
        }
        var tt=(nbsupplement/20.0)+tarif+(((machines(machineId).sugar-differenceduSucre).toDouble)*2)/100.0 //Calcul du tarif tout compris
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
        machines(machineId).removeIngredient("cafe",differenceduCafe)
        machines(machineId).removeIngredient("lait",differenceduLait)
        machines(machineId).removeIngredient("sucre",differenceduSucre)

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

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println("Sauvegarde de "+machines.size+" machines dans "+filename)
    try{       //Bloc de code testé, si une exception est levée, elle sera envoyée au bloc catch qui traitera l'exception
      var fichier = new PrintWriter(filename)
      fichier.println("PINCODE,MILK,SUGAR,COFFEE")
      //On parcourt de j=0 à j=machines.size et à chaque itération, on rentre une ligne dans le fichier ouvert destiné à l'écriture.
      for(j<- machines.indices){
        //Création de 4 variables qui sont les composantes de la string finale (ligne)
        val cafe = (machines(j).coffee).toString
        val lait = (machines(j).milk).toString
        val sucre = (machines(j).sugar).toString
        val code = machines(j).pincode
        val ligne:String = code+","+lait+","+sucre+","+cafe+"\n" //Ajout de virgule au milieu pour respecter la charte du modèle dans l'énoncé
        fichier.print(ligne)  //On écrit et on va à la ligne
      }
      fichier.close() //Closing du fichier
      println("Fichier sauvegardé avec succès")
    }
    catch {
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule\n",filename)
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule\n",filename)
        println("Fermeture du programme.")
        System.exit(0)
    }
  }


  def main(args: Array[String]): Unit = {
    val filename="machines.csv"
    print("Chargement des machines depuis ")
    println(filename)
    var machines = loadcsv(filename)
    println(machines.size+" machine(s) chargée(s) avec succès.")
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
        savecsv(filename, machines)
        System.exit(0)
      }
      //L'option 3 est écartée, on peut donc demander la machine à séléctionner
      var machine_id=0


      if(choixdumode==2) //Si c'est 2 alors entrer dans le mode administrateur
      {
        while(machine_id<1 || machine_id>5){
          machine_id = readLine("Machine séléctionnée (1-5) > ").toInt
        }
        var maj=0
        var test=3
        while(!validatePin(machine_id-1,machines)){  //tant que l'utilisateur valide pas le code on lui redemande
          test = test - 1 //Chaque fois on enlève 1 et on affiche combien il en reste
          println("Code PIN Incorrect. "+test+" tentatives restantes.")
          if(test==0){  //Quand on atteint 0, on quitte lr programme
            println("Trop de tentatives échouées. Fin du programme.")
            savecsv(filename, machines)
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
          updatePin(machine_id-1,machines)
        }
        if(maj==2){
          //appeler la maj des stocks
          restockMachine(machine_id-1,machines)
        }
      }
      else if(choixdumode==1) //Si c'est 1 alors entrer dans le mode client
      {
        while(machine_id<1 || machine_id>5){
          machine_id = readLine("Machine séléctionnée (1-5) > ").toInt
        }
        if(serveClient(machine_id-1,machines)==false){
          println("Erreur. Veuillez choisir une autre machine, celle-ci manque d'ingrédients.")
        }
      }

    }
  }
}