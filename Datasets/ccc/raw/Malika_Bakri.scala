import scala.util.Random
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    //Méthode pour accéder aux ingrédients et les modifier
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient=="coffee" && amount<=coffee) {
        coffee = coffee - amount
        true
      }
      else if (ingredient=="sugar" && amount<=sugar) {
        sugar = sugar - amount
        true
      }
      else if (ingredient=="milk" && amount<=milk) {
        milk = milk - amount
        true
      }
      else false
    }
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if(ingredient=="coffee") coffee+=amount
      else if(ingredient=="sugar") sugar+=amount
      else if(ingredient=="milk") milk+=amount
    }

  }

  def main(args: Array[String]): Unit = {
    //Depart du stock initial

    var insuffisant=0
    var machines = ArrayBuffer[Machine]() //Déclaration du tableau de machines
    machines = loadcsv("machines.csv")  //Remplissage du tableau
    affichage(machines)
    while(true)
    { //rester
      var choix=0
      if(insuffisant!=0)choix=1
      else{
        //Affichage des choix des options
        println("\n             Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")

        choix = readLine(">").toInt //Déclaration et utilisation de la variable qui stocke le choix
        while((choix<1) || (choix>3)){  //test du choix dans une boucle
          choix = readLine(">").toInt
        }
      }
      var id_machine=0  //Id de la machine qui sera envoyée en paramètre aux fonctions
      if(choix==1)
      {
        //Choix de la machine
        do{
          id_machine = readLine("Machine séléctionnée (1-5) > ").toInt
        }while(!(id_machine>=1 && id_machine<=5)) //Condition pour passer outre et commander
        if(!serveClient(id_machine,machines)) println("Veuillez séléctionner une autre machine.\n")  //on appelle la fonction et on réagit en cas d'erreur, c'est-à-dire en cas de return false
      }
      if(choix==2)
      {
        //Sous-menu administrateur :
        println("Quelle action souhaitez-vous réaliser ?")
        println("1) Mettre à jour le code PIN")
        println("2) Procéder au restock des ingrédients")
        var action=0
        while(action!=1 && action!=2){
          action = readLine(">").toInt
        }
        //Choix ultérieur de la machine
        do{
          id_machine = readLine("Machine séléctionnée (1-5) > ").toInt
        }while(!(id_machine>=1 && id_machine<=5))
        //Saisie du Code PIN administrateur avec appel de la fonction validatePIN
        var essais=0
        var diffessais=0
        while(!validatePin(id_machine,machines)){  //tant que on ne résusit pas le bon code
          essais+=1 //augmentation du nombre de tentatives échouées
          diffessais = 3 - essais //calcul du nombre d'essais restants
          printf("Code PIN incorrect. %d tentatives restantes. \n",diffessais)  //Affichage de ce nombre en printf
          if(diffessais==0){
            println("Trop de tentatives échouées. Fin du programme.")
            savecsv("machines.csv",machines)
            System.exit(0)  //Sortie de programme
          }
        }
        //Orchestration des différentes actions selon le choix réalisé dans le sous-menu administrateur
        if(action==1) updatePin(id_machine-1,machines)
        else if(action==2) restockMachine(id_machine,machines)
      }
      else if(choix==3)
      {
        //Cas de l'option 3 -> Quitter
        savecsv("machines.csv",machines)
        System.exit(0)
      }
    }
  }

  def affichage(machines: ArrayBuffer[Machine]): Unit = {
    for(l<-0 until machines.size){
      println("Machine "+machines(l).id+" chargée:")
      println("   ID: "+machines(l).id)
      println("   Code PIN: "+machines(l).pincode)
      printf("   Lait: %.3fL",machines(l).milk/1000.0)
      println("\n   Sucre: "+ machines(l).sugar+"g")
      println("   Café: "+machines(l).coffee+"g")
      println("")
    }
    println(machines.size+" machine(s) chargée(s) avec succès.")  //nombre de machines sauvegardées
  }
  //Fonction restockMachine appelée par l'administrateur
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    //On affiche les quantités actuelles avec %.2f pour la valeur Double : le lait
    println("Niveaux de stock actuels :")
    //Changement d'ordre entre le sucre et le lait et changement d'unité de conversion pour le lait
    val milkstockenmilli = machines(machineId-1).milk/1000.0 //conversion
    println("Poudre de café: " + machines(machineId-1).coffee + "g")
    println("Sucre         : " + machines(machineId-1).sugar + "g")
    printf("Lait          : %.2fL\n", milkstockenmilli) //Affichage

    println("Entrez les quantités à ajouter : ")

    //nouvelles variables de nouveaux stocks
    var poudre=0
    var sucre2 = 0
    var lait2=0.0
    do{
      poudre = readLine("Poudre de café > ").toInt
    }while (poudre < 0)
    machines(machineId-1).addIngredient("coffee",poudre)
    do {
      sucre2 = readLine("Sucre > ").toInt
    }while (sucre2 < 0)
    do{
      lait2 = readLine("Lait > ").toDouble
    }while (lait2 < 0.0)
    machines(machineId-1).addIngredient("milk",(lait2*1000).toInt)

    machines(machineId-1).addIngredient("sugar",sucre2)

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
    //Retour menu sans return
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    var dernieremachine = machines.size
    println("Sauvegarde de "+dernieremachine+" machines dans "+filename)
    try{
      var fichier_write = new PrintWriter(filename)
      fichier_write.println("PINCODE,MILK,SUGAR,COFFEE")
      for(i<-0 until dernieremachine){
        fichier_write.println((machines(i).pincode)+","+(machines(i).milk).toString+","+(machines(i).sugar).toString+","+(machines(i).coffee).toString)
      }
      fichier_write.close()
      println("Fichier sauvegardé avec succès")
    }
    catch {
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var totalmachines = ArrayBuffer[Machine]()
    printf("Chargement des machines depuis %s\n",filename)
    try {
      val fr = Source.fromFile(filename) //source ouvre fichier
      var i = 0
      val ligne = fr.reset.getLines
      while (!ligne.isEmpty) {
        if(i==0) {  //première fois qu'on arrive, on doit sauter les titres PINCODE,MILK, ...
          var saut = ligne.next
        }
        var recuperation = ligne.next
        val splitting = recuperation.split(",")  //Séparation de la ligne dès qu'on croise une virgule
        i+=1
        totalmachines += new Machine(i, splitting(0), splitting(1).toInt, splitting(2).toInt, splitting(3).toInt) //ajout d'une machine
      }
      fr.close()//fermeture du fichier
    } catch {
      case e: java.io.FileNotFoundException =>  //exception de fichier introuvable
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    return totalmachines  //on retourne le tableaux une fois rempli
  }


  //Fonction validatePin appelée par l'administrateur pour vérifier qu'il a accès à l'admin
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var codecorrect = false
    var code = readLine("Entrez le code PIN : ")
    codecorrect = (code == machines(machineId-1).pincode)  //true si c'est le même, false sinon
    return codecorrect  //return de la ligne précédente
  }

  //Fonction serveClient appelée par le Client pour commander une boisson
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    //Choix n°1 <=> Entrer dans le mode Client
    /*
    Etape 1 : Demander la boisson
    Etape 2 : Demander la taille de la boisson si c'est un Latte
    Etape 3 : Demander le niveau de sucre
    Etape 4 : Demander si il y a du lait supplémentaire (pour Capuccino et Latte)
    Etape 5 : Vérifier les stocks
    Etape 6 : Si stock : Affichage récapitulatif + prix + paiement / Sinon : message d'erreur
    Etape 7 : Mise à jour des stocks
     */
    val longueurchaine=36 //Taille de l'alphabet + 10 chiffres


    //Etape n°1
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Small), CHF 3.20 (Medium), CHF 3.70 (Large)")

    var choix_boisson = readLine(">").toInt
    var choix_boisson_end = 0.0
    while(choix_boisson<1 || choix_boisson>3){  //vérification du choix de la boisson
      choix_boisson = readLine(">").toInt
    }
    var temp=0
    if(choix_boisson==3){ //Cas du latte
      //Etape n°2
      println("Veuillez sélectionner la taille de votre Latte :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      temp= readLine(">").toInt
      while(temp<1 || temp>3){  //boucle pour s'assurer de la qualité de la réponse
        temp = readLine(">").toInt
      }
    }
    var affichagedrink:String="Boisson X"
    var cout=0.0  //variable pour associer un cout à chaque boisson

    //Personnalisation de l'affichage de la boisson pour l'après prise de commande dans l'affichage + ajouter le cout de chaque boisson
    if(choix_boisson==1){
      affichagedrink="Expresso"
      cout=2.00
    }
    if(choix_boisson==2){
      affichagedrink="Capuccino"
      cout=2.50
    }
    if(choix_boisson==3 && temp==1){
      affichagedrink="Latte (Petit)"
      cout=2.70
    }
    if(choix_boisson==3 && temp==2){
      affichagedrink="Latte (Moyen)"
      cout=3.20
    }
    if(choix_boisson==3 && temp==3){
      affichagedrink="Latte (Grand)"
      cout=3.70
    }
    var caferequis =0
    var laitrequis=0.0
    /*
    Calcul mathématiques pour optimisation :
    Si temp=1 -> petit latte, caferequis = 6 + 1*0 = 6
    Si temp=2 -> moyen latte, caferequis = 6 + 2*1 = 8
    Si temp=3 -> grand latte, caferequis = 6 + 3*2 = 12
     */
    if(temp!=0) caferequis = 6 + temp*(temp-1)
    else caferequis = 8 - choix_boisson*(choix_boisson-1)
    /*
    Calcul mathématiques à nouveau :
    0.11 + 0.01*1 = 0.12
    0.11 + 0.02*2 = 0.15
    0.11 + 0.03*3 = 0.20
     */
    if(temp!=0) laitrequis=0.11 + ((temp/100.0)*(temp))
    if(choix_boisson==2) laitrequis=0.1
    //Etape n°3
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    var dosesucre = readLine(">").toInt
    while(dosesucre!=1 && dosesucre!=2 && dosesucre!=3 && dosesucre!=4) {
      dosesucre = readLine(">").toInt
    }
    //Calcul mathématiques pour avoir la dose de sucre exacte selon le choix du niveau
    dosesucre = 5*(dosesucre-1)
    var dosesucrechaine = ""
    //Personnalisation de la chaine de caractres de sucre
    if(dosesucre==0) {
      dosesucrechaine = "Sans sucre"
    }
    if(dosesucre==5) {
      dosesucrechaine = "Peu ("+dosesucre+"g)"  //On rajoute au milieu de la chaine le nombre de grammes de sucre dans la dose choisie
    }
    if(dosesucre==10) {
      dosesucrechaine = "Moyen ("+dosesucre+"g)"
    }
    if(dosesucre==15) {
      dosesucrechaine = "Beaucoup ("+dosesucre+"g)"
    }

    var boissonChoisie = (choix_boisson == 2 || choix_boisson == 3)
    var choixlait=0
    var doses=0
    if (boissonChoisie) { //si la boisson choisie respecte certaines conditions (ligne 125)
      //Etape n°4
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")
      while (choixlait != 1 && choixlait != 2) {
        choixlait = readLine(">").toInt
      }
      if (choixlait == 1) {
        while (doses < 1 || doses > 3) {
          doses = readLine("Combien de doses ?\n>").toInt
        }
      }
      laitrequis = laitrequis + doses*0.05  //Penser à ajouter les doses extra de lait au montant du lait requis pour la commande
      laitrequis = Math.round(laitrequis * 100) / 100.0 //arrondir pour éviter les .00000002
    }

    //Etape n°5
    //Déclaration de 3 variables booléennes qui permettent de savoir (true/false) de manière biaire s'il reste assez
    // de café ou de lait ou même de sucre pour commander une boisson
    var laitsuffisant = ((laitrequis*1000).toInt)<=machines(machineId-1).milk
    var cafesuffisant = caferequis<=machines(machineId-1).coffee
    var sucresuffisant = dosesucre<=machines(machineId-1).sugar

    println("Boisson sélectionnée : "+affichagedrink)
    println("Niveau de sucre : "+dosesucrechaine)


    if(doses==0 && (choix_boisson==2 || choix_boisson==3))
      println("Lait en supplément : Non")
    else if(doses!=0 && (choix_boisson==2 || choix_boisson==3))
      println("Lait en supplément : "+doses)
    if(laitsuffisant && cafesuffisant && sucresuffisant){          //Etape 6
      var totalcout = cout + dosesucre/50.0 + 0.05*doses
      var strplus = " + CHF "
      /*var str_sucre = strplus + (dosesucre/2.0)
      var str_lait = strplus + 0.05*doses*/
      print("Prix total: CHF ") //Affichage de la base de la string du prix total
      printf("%.2f",cout)
      if(dosesucre>0){  //En cas de séléction de sucre l'ajouter à la note pour mettre en avant la somme des options
        print(strplus)
        printf("%.2f",dosesucre/50.0)
      }
      if(doses>0){  //pareil avec les doses de lait
        print(strplus)
        printf("%.2f",0.05*doses)
      }
      printf(" = CHF %.2f",totalcout) //Affichage final

      println("\n\nVeuillez payer en utilisant Twint.")
      var linktwint=""
      //Création d'un code twint de taille de 5 caractères entre 0123456789 et l'alphabet
      for(i<-0 until 5){//faire 5x la ligne suivante
        linktwint += ("0123456789"+"ABCDEFGHIJKLMNOPQRSTUVWXYZ")(Random.nextInt(longueurchaine))  //tirage au soir d'un nombre entre 0 et 35. Puis séléction d'un caractre parmi ceux-là à l'index (entre 0 et 35)
      }
      println("Votre code de paiement est : "+linktwint)
      println("(En attente de paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé.")
      println("")
      //affichage de la bonne boisson
      println("Préparation de votre boisson...\nVotre "+ affichagedrink +" est prêt ! Bonne dégustation !\n")

      //Etape 7 : Mise à jour des stocks
      machines(machineId-1).removeIngredient("milk",(laitrequis*1000).toInt)
      machines(machineId-1).removeIngredient("sugar",dosesucre)
      machines(machineId-1).removeIngredient("coffee",caferequis)
      return true
    }
    else if(!laitsuffisant){//Etape 6
      println("Erreur : Quantité de lait insuffisante pour préparer\n la boisson sélectionnée.")
      println("Veuillez choisir une taille plus petite ou essayer\n une autre boisson.")

    }else if(!cafesuffisant){//Etape 6
      println("Erreur : Quantité de poudre de café insuffisante pour\n préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin.")

    }else if(!sucresuffisant){//Etape 6
      println("Erreur : Quantité de sucre insuffisante pour préparer\n la boisson sélectionnée.")
      println("Veuillez choisir une une dose plus faible de sucre ou vérifier les\n stocks en mode Admin.")
    }
    if(!laitsuffisant || !sucresuffisant || !cafesuffisant){
      return false
    }
    else true
  }

  //Fonction updatePIn appelée par l'administrateur pour modifier le code PIN d'une machine séléctionnée
  def updatePin(machineId: Int,machines: ArrayBuffer[Machine]): Unit = {
    //Comme ici on a envoyé machineId et pas machineId-1, on fait l'affichage en ajoutant 1 et on fait le changement de pin sans rien ajouter
    println("Mise à jour du code PIN pour la Machine " + (machineId+1))
    var test = ""
    while(test.length!=6){  //tant que le code n'est pas un code bon (6 caractères)
      test=readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      if(test.length!=6){ //message d'erreur au client
        println("Le code que vous avez rentré n'est pas composé de 6 chiffres.")
      }
    }
    machines(machineId).pincode = test //nouveau code changé
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")

  }
}