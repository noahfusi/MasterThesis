import scala.io.Source
import scala.util.Random
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._

/*
Au cours du programme, tous les while() et les do while (sauf le premier) que nous allons utiliser
sont fait pour vérifier les saises utilisateur, pour éviter qu'elle soit négatives, ou qu'elles ne correspondent à aucune option
 */
object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) { //Classe définie comme Machine avec cinq variables : attributs de classe : ingrédients, ID & Codepin.
    //Classe définie avec deux méthodes qui manipulent les ingrédients en ajoutant ou soustrayant des montants
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if(ingredient=="cafe") {
        coffee+=amount
      }
      else if(ingredient=="sucre"){
        sugar+=amount
      }
      else if(ingredient=="lait"){
        milk+=amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (ingredient=="cafe" && coffee>=amount) {
        coffee = (coffee - amount)
        return true
      }
      else if (ingredient=="sucre" && sugar>=amount){
        sugar = (sugar - amount)
        return true
      }
      else if (ingredient=="lait" && milk>=amount) {
        milk = (milk - amount)
        return true
      }
      else return false
    }
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var listmachine = ArrayBuffer[Machine]()
    println("Chargement des machines depuis "+filename)
    try {
      val filesrc = Source.fromFile(filename) //Source qui ouvre le fichier
      val lines = filesrc.reset.getLines
      lines.next  //on passe le début sans virgule
      while (lines.hasNext){
        var id = 1  //on définit une variable d'identifiant qui prend 1 à chaque passage pour avoir un id différent
        for(l<- lines) {  //l = objet de lines
          val decoupage = l.split(",") // Diviser la ligne en utilisant la virgule
          var mach1= new Machine(id, decoupage(0), decoupage(1).toInt, decoupage(2).toInt, decoupage(3).toInt)  //Créer un nouvel objet de Machines avec des conversions qui initialise les attributs de ce nouvel objet qui sera ensuite ajouté à l'arraybuffer de Machine
          listmachine += mach1  //ajout array
          println("Machine "+mach1.id+" chargée:")
          println("   ID: "+mach1.id)
          println("   Code PIN: "+mach1.pincode)
          printf("   Lait: %.3fL",mach1.milk/1000.0)
          println("\n   Sucre: "+ mach1.sugar+"g")
          println("   Café: "+mach1.coffee+"g")
          id += 1 //augmenter
        }
      }
      filesrc.close() // Fermer
    } catch {
      case e: java.io.FileNotFoundException => // Exception pour fichier introuvable
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>            //Exception pour écriture
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)

    }

    listmachine // Retourner la liste des machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    //Définition d'une taille pour l'arrayBuffer. Parcours de l'arraybuffer jusqu'à ce compteur, à chaque tour de boucle, on écrit dans le fichier avec un fw.println les composantes de une machine que l'on sépare avec des virgules comme dans l'en-tête proposé dans le sujet
    val compteur_machine = machines.size
    println(s"Sauvegarde de $compteur_machine machines dans $filename")
    try{
      var fw = new PrintWriter(filename)  //Ouverture du fichier
      fw.println("PINCODE,MILK,SUGAR,COFFEE") //En-tête proposé par le sujet en majuscule
      for(unemachine<- machines) fw.println(s"${unemachine.pincode},${unemachine.milk},${unemachine.sugar},${unemachine.coffee}")
      fw.close()  //fermeture du fichier
      println("Fichier sauvegardé avec succès")
    }
    catch {
      //Gestion des exceptions possibles sur le fichier
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
    }
  }



  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var codepinpardefaut = readLine("Entrez le code PIN : ") //récupérer le code pin entré
    if (machines(machineId-1).pincode==codepinpardefaut){  //vérifier qu'il correspond à celui de la machine séléctionnée
      println("Accès accordé à la machine "+machineId)
      return true
    }
    else false  //retourner faux : mauvais pin
  }
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    //On a déplacé les variables du main qui servait pour le mode client directement ici
    val expresso=1
    val capuccino=2
    val latte=3
    var bloque1=false
    var bloque2=false
    var bloque3=false
    val chiffre_et_alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    var montant:Double=0.0  //prix

    var cafedesire=0
    var sucredesire=0
    var laitdesire=0
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
      laitdesire = 120
      cafedesire = 6
      montant=2.70
      if(latteTaille==2){
        laitdesire=150 //Consomme 0.15L de lait pour un moyen
        cafedesire = 8
        montant += (latteTaille*0.25) //montant = 2.70 + 2*0.25 = 3.20
      }
      if(latteTaille==3){
        laitdesire=200
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
        laitdesire=100 //L de lait dans capuccino
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
    val convlait = ((portion/20.0)*1000).toInt
    laitdesire+=convlait //total de lait désiré quand on y ajoute les doses de lait supplémentaires.

    println("Niveau de sucre : "+stringNiveau)
    if(portion==0){
      println("Lait en supplément : Non")
    }
    else{
      println("Lait en supplément : " + portion)
    }
    //Pour les 3 prochains if, on retourne faux car il manque un ingrédient
    if(laitdesire>machines(machineId-1).milk)
    {
      bloque1=true
      println("")
      println("Erreur : Quantité de lait insuffisante pour préparer\n la boisson sélectionnée.")
      println("Veuillez choisir une dose plus petite ou essayer\n une autre boisson.")
      return false
    }
    else{
      bloque1=false
    }
    if(sucredesire>machines(machineId-1).sugar)
    {
      bloque2=true
      println("")
      println("Erreur : Quantité de sucre insuffisante pour préparer\n la boisson sélectionnée.")
      println("Veuillez choisir une quantité plus faible de sucre dans votre boisson ou vérifier les\n stocks en mode Admin.")
      return false
    }
    else{
      bloque2=false
    }
    if(cafedesire>machines(machineId-1).coffee)
    {
      println("")
      bloque3=true
      println("Erreur : Quantité de poudre de café insuffisante pour\n préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin.")
      return false
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
      machines(machineId-1).removeIngredient("sucre",sucredesire)
      machines(machineId-1).removeIngredient("cafe",cafedesire)
      machines(machineId-1).removeIngredient("lait",laitdesire)

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
      return true //bon cas de figure, on retourne true
    }
    return true
  }

  def updatePin(machineId: Int,machines: ArrayBuffer[Machine]): Unit = {
    var changementdepin = ""  //variable du nouveau code
    println("Mise à jour du code PIN pour la Machine "+machineId) //pas de -1 car pour l'utilisateur on parle de la machine en cours, pas de son ID
    do
    {
      changementdepin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")  //stockage du nouveau code
    } while (changementdepin.length!=6) //boucle qui revient à la demande chaque fois que le code est <6 chiffres

    machines(machineId-1).pincode = changementdepin  //Stockage du code
    //Affichage d'informations basiques
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Niveaux de stocks actuels :")
    //Stocks disponibles
    println("Poudre de café: "+machines(machineId-1).coffee+"g")
    print("Sucre : "+machines(machineId-1).sugar)
    println("g")
    var affichagelait=machines(machineId-1).milk/1000.0  //création d'une variable qui stocke le lait actuel mais divisé par 1000
    printf("Lait : %.2fL", affichagelait)

    println("\nRéapprovisionnement des stocks...")
    println("Entrez les quantités à ajouter : ")

    var poudreplus = 0
    print("   Poudre de café: ")
    poudreplus = readInt()  //lecture
    while(poudreplus<0){
      poudreplus = readLine("   Poudre de café: ").toInt
    }
    print("   Sucre: ")
    var sucreplus = readInt()
    while(sucreplus<0){
      sucreplus = readLine("   Sucre: ").toInt
    }

    var laitplus = readLine("   Lait : ").toDouble
    while(!(laitplus>=0.0)){
      laitplus = readLine("   Lait: ").toDouble
    }
    laitplus=laitplus*1000  //conversion pour rebasculer en mL

    //Augmentation des stocks avec les ajouts
    machines(machineId-1).addIngredient("sucre",sucreplus)
    machines(machineId-1).addIngredient("lait",laitplus.toInt) //conversion en entier
    machines(machineId-1).addIngredient("cafe",poudreplus)
    println("Niveaux de stock mis à jour.") //indication
    println("Retour au menu principal...")
    println("")
  }

  def main(args: Array[String]): Unit = {
    var rester="oui"
    val filename="machines.csv"
    var machines = loadcsv(filename)
    while(rester=="oui")  //tant que la variable rester vaut oui, on reste dedans, lorsqu'elle vaudra non, il faudra quitter le programme
    {
      var decision=0
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
      var machine_saisie:Int=0 //pour stocker l'id de la machine
      if(decision==1) //Soit il est en mode bloqué car il manque un ingrédient, soit il l'a choisi de lui-même
      { //Si l'utilisateur rentre en mode client, on affiche la séléction des boissons
        while(machine_saisie!=1 && machine_saisie!=2 && machine_saisie!=3 && machine_saisie!=4 && machine_saisie!=5){
          print("Machine sélectionnée (1-5) > ")
          machine_saisie = readInt()
        }
        //CALL SUR LA FONCTION SERVECLIENT ET ENREGISTREMENT DU RETURN
        var reussit = serveClient(machine_saisie,machines)
        if(reussit){//Return true
        }
        else{ //REturn false
          println("Erreur liée au stock d'ingrédients de la machine "+machine_saisie+". Veuillez séléctionner une autre machine")
        }
      }//fin du mode client
      else if(decision==2)//début du mode admin
      {
        println("Mode Admin")
        while(machine_saisie<1 || machine_saisie>machines.size){
          print("Machine sélectionnée (1-"+machines.size+") > ")
          machine_saisie = readInt()
        }
        var tentatives=3  //Commencer avec 3 essais
        while(validatePin(machine_saisie,machines)==false){  //tant que la fonction retourne faux (mauvais pin), on boucle
          tentatives = tentatives - 1 //on enlève 1 tentative
          println("Code PIN incorrect. "+ tentatives +" tentatives restantes.")
          if(tentatives<1){  //quand il n'y a plus de tentatives
            println("Trop de tentatives échouées. Fin du programme.")
            savecsv(filename,machines)
            rester="non"  //sortir
            sys.exit(0)
          }
        } //Reboucler
        if(rester!="non"){
          var option_selectionnee =0
          println("Veuillez séléctionner une option : ")  //choix à faire entre le restock ou le changement de code
          println("1) Restock les ingrédients")
          println("2) Mise à jour du code PIN")
          while((option_selectionnee!=1 && option_selectionnee!=2)){
            option_selectionnee = readLine("> ").toInt
          } //boucle jusqu'à bonne séléction
          //On appelle les fonctions selon le choix de l'admin
          if(option_selectionnee==1){
            restockMachine(machine_saisie,machines)
          }
          else{
            updatePin(machine_saisie,machines)
          }
        }
      }//fin du mode admin
      else if(decision==3)
      {
        savecsv(filename,machines)
        rester="non"
      }
      //Fin des modes
    }
  }
}