import scala.io.StdIn._
import scala.util.Random
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {

  //Changements ex2: on divise par 1000 ou multiplie par 1000 le stock de lait par rapport à l'exercice 1 car dans l'énoncé le lait est un entier donc doit être traité comme des millilitres.

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      //Pour cette fonction, on vérifie la chaine de caractères envoyé, soit café, soit lait soit sucre et on enlève le montant (amount) envoyé selon la chaïne
      if (ingredient=="cafe" && coffee>=amount) { //si c'est cafe et queon en a assez, on soustrait
        coffee -= amount
        true
      }
      else if (ingredient=="sucre" && sugar>=amount) {
        sugar -= amount
        true
      }
      else if (ingredient=="lait" && milk>=amount) {
        milk -= amount
        true
      }
      else {
        false
      }
    }
    def addIngredient(ingredient: String, amount: Int): Unit = {
      //Même fonction mais cette fois-ci en ajoutant
      if(ingredient=="cafe"){
        coffee+=amount
      }
      else if(ingredient=="sucre"){
        sugar+=amount
      }
      else if(ingredient=="lait"){
        milk+=amount
      }
    }

  }
  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var codesaisi = readLine("Entrez le code PIN : ") //demander à l'utilisateur de saisir le code
    if (machines(machineId-1).pincode==codesaisi) true //si c'est le même entre l'id de la machine donc idmachine -1 et celui rentré, on retourne true
    else false //sinon false
  }
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var gestionmachines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis "+filename)
    println("")
    try {
      val fichier = Source.fromFile(filename) //ouverture du fichier
      var encours = 0 //va permettre de parcourir le fichier et savoir à quelle ligne on se situe
      val ligne = fichier.reset.getLines  //contenu du fichier
      val entete = ligne.next() //on dépasse la première ligne inutile (renseignement)
      while (!ligne.isEmpty) {  //tant que le fichier n'est pas vide, on continue le parcours
        var entite = ligne.next //on enregistre la chaine (ligne)
        val att = entite.split(",")  //on la sépare
        var unique = new Machine((encours + 1), att(0), att(1).toInt, att(2).toInt, att(3).toInt) //on créer une machine
        encours+=1
        gestionmachines+=unique //on l'ajoute au tableau
        println("Machine "+encours+" chargée:")
        println("   ID: "+encours)
        println("   Code PIN: "+att(0).toString)
        printf("   Lait: %.3fL\n",att(1).toInt/1000.0)
        println("   Sucre: "+ att(2).toInt+"g")
        println("   Café: "+att(3).toInt+"g")
        println("")
      }
      println(encours+" machine(s) chargée(s) avec succès.")
      fichier.close() //fermeture
    } catch { //exception attrappée
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    return gestionmachines
  }


  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    val caracterescode = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" // Caractères pour générer le code de paiement
    val chiffrescode = "1234567890"

    var taillelatte=0
    var nombredoses=0

    var coutboisson = 0.0 // Prix de la boisson choisie, ajusté en fonction des options
    var cafenecessaire = 0 // Quantité de café requise pour la boisson en grammes
    var sucrenecessaire = 0 // Quantité de sucre requise pour la boisson en grammes
    var laitcommande = 0.0 // Quantité de lait requise en litres
    var supplait = 0.0 // Quantité de lait supplémentaire sélectionnée par le client en litres

    // Génération de 5 caractères aléatoires pour le code de paiement après la prise de commande
    val c1 = caracterescode(Random.nextInt(26))
    val c2 = caracterescode(Random.nextInt(26))
    val n = chiffrescode(Random.nextInt(10))
    val c4 = caracterescode(Random.nextInt(26))
    val c5 = caracterescode(Random.nextInt(26))

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00 ")
    println("2) Cappuccino - CHF 2.50 ")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    var typeboisson = readInt()
    while(!(typeboisson>=1 && typeboisson<=3)){
      println("Veuillez choisir une option comprise entre 1 et 3")
      typeboisson = readLine(">").toInt
    }

    // Détermine les ingrédients et le prix de base selon le choix de la boisson

    if (typeboisson == 1) {
      coutboisson = 2.00
      cafenecessaire = 8
    } else if (typeboisson == 2) {
      coutboisson = 2.50
      cafenecessaire = 6 // Moins de café pour un cappuccino
      laitcommande = 0.100 // Ajout de lait pour le cappuccino
    } else if (typeboisson == 3) {
      println("Quelle taille souhaitez-vous pour votre Latte ?")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      taillelatte = readLine(">").toInt
      while(!(taillelatte>=1 && taillelatte<=3)){
        println("Veuillez choisir une option comprise entre 1 et 3")
        taillelatte = readLine(">").toInt
      }
      if (taillelatte == 1) {
        coutboisson = 2.70
        cafenecessaire = 6
        laitcommande = 0.120
      } else if (taillelatte == 2) {
        coutboisson = 3.20
        cafenecessaire = 8
        laitcommande = 0.150
      } else if (taillelatte == 3) {
        coutboisson = 3.70
        cafenecessaire = 12
        laitcommande = 0.200
      }
    }

    // Option de sucre : demande la quantité et ajuste le prix en conséquence

    println("Quelle dose de sucre voulez-vous ?")
    println("1) Sans sucre ")
    println("2) Peu (5g) - CHF 0.10 ")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30\n>")
    var niveausucre = readInt()
    while(!(niveausucre>=1 && niveausucre<=4)){
      println("Veuillez choisir une option comprise entre 1 et 4")
      niveausucre = readLine(">").toInt
    }
    if (niveausucre == 1) {
      sucrenecessaire = 0
    } else if (niveausucre == 2) {
      sucrenecessaire = 5
    } else if (niveausucre == 3) {
      sucrenecessaire = 10
    } else if (niveausucre == 4) {
      sucrenecessaire = 15
    }

    // Lait supplémentaire pour les boissons concernées (capuccino et latte)
    if (typeboisson!=1) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non\n>")
      var ajoutLait = readInt()
      while(!(ajoutLait>=1 && ajoutLait<=2)){
        println("Veuillez choisir une option comprise entre 1 et 2")
        ajoutLait = readLine(">").toInt
      }
      if (ajoutLait == 1) {
        println("Combien de dose ?\n>")

        nombredoses = readInt()
        while(!(nombredoses>=0 && nombredoses<=3)){
          println("Veuillez choisir une option comprise entre 0 et 3")
          nombredoses = readLine(">").toInt
        }
        // Ajustement de la quantité de lait supplémentaire selon la demande client
        if (nombredoses == 1) {
          supplait = 0.050
        } else if (nombredoses == 2) {
          supplait = 0.100
        } else if (nombredoses == 3) {
          supplait = 0.150
        }
        laitcommande = laitcommande+supplait

      }
    }
    laitcommande = laitcommande*1000
    // Vérification des stocks pour chaque ingrédient avant la préparation
    if (cafenecessaire > machines(machineId-1).coffee) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
      false
    }  else if (sucrenecessaire > machines(machineId-1).sugar) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre dose ou vérifier les stocks en mode Admin\n")
      false
    }
    else if (laitcommande.toInt > machines(machineId-1).milk) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin\n")
      false
    }else {

      //Affichage confort utilisateur pour afficher tous les détails de la commande : Boisson/Sucre/Lait extra/Prix/Paiement/Message de remerciement

      //Détail : Boisson
      if(typeboisson==1){
        println("Boisson sélectionnée : Expresso")
      }
      else if(typeboisson==2){
        println("Boisson sélectionnée : Capuccino")
      }
      //Détail : Boisson avec taille en cas de commande d'un Latte
      else if(typeboisson==3){
        println("Boisson sélectionnée : Latte")
        if(taillelatte==1){
          println("(Petit)")
        }
        else if(taillelatte==3){
          println("(Grand)")
        }
        else if(taillelatte==2){
          println("(Moyen)")
        }
      }
      //Détail : Sucre
      print("Niveau de sucre : ")
      if(niveausucre==1){
        println("Sans sucre")
      }
      else if(niveausucre==4){
        println("Beaucoup (15g)")
      }
      else if(niveausucre==3){
        println("Moyen (10g)")
      }
      else if(niveausucre==2){
        println("Peu (5g)")
      }

      //Détail : Lait additionnel
      print("Lait en supplémentaire: ")
      if(nombredoses==0){
        println("Non")
      }
      else{ //Sinon afficher seulement le nombre de doses séléctionnées par l'utilisateur
        println(nombredoses)
      }

      //Calcul du prix total avec affichage de l'addition qui mène au prix final.
      printf("\nPrix total : CHF %.2f",coutboisson)
      //Augmentation des coûts affichés selon le niveau de sucre séléctionné
      if(niveausucre==2){
        print(" + CHF 0,10")
        coutboisson+=0.1
      }
      if(niveausucre==3){
        print(" + CHF 0,20")
        coutboisson+=0.2
      }
      if(niveausucre==4){
        print(" + CHF 0,30")
        coutboisson+=0.3
      }
      //En cas d'ajout de lait, affichage de l'addition avec le lait
      if(supplait>0.0){
        printf(" + CHF %.2f", supplait)
      }
      printf(" = CHF %.2f \n", (coutboisson+supplait))

      println("Votre code de paiement est : " + (c1.toString+c2.toString+n.toString+c4+c5)) //Génaration du code final de paiement Twint en ajoutant (concaténation) les lettres et les chiffres à la suite
      println("(En attente de paiement...)\n")
      Thread.sleep(3000) // Simule le délai de paiement
      println("Paiement confirmé")

      // Mise à jour des stocks après confirmation de la commande
      machines(machineId-1).removeIngredient("cafe",cafenecessaire)
      machines(machineId-1).removeIngredient("lait",laitcommande.toInt)
      machines(machineId-1).removeIngredient("sucre",sucrenecessaire)

      // Message final pour indiquer que la boisson est prête
      println("Préparation de votre boisson...")
      if (typeboisson == 1) {
        println("Votre Espresso est prêt ! Bonne dégustation\n")
        true
      } else if (typeboisson == 3) {
        println("Votre Latte est prêt ! Bonne dégustation\n")
        true
      }
      else if (typeboisson == 2) {
        println("Votre Cappuccino est prêt ! Bonne dégustation\n")
        true
      }
      true
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    //Fonction qui parcours le tableau de machines et a chaque machine il
    println("Sauvegarde de "+machines.size+" machines dans "+filename)
    try{
      var ecriture = new PrintWriter(filename)
      ecriture.println("PINCODE,MILK,SUGAR,COFFEE") //écriture de la première ligne basique
      for(m<- machines){  //parcours de toutes les machines, chaque machine est enregistré comme m
        ecriture.println((m.pincode)+","+(m.milk).toString+","+(m.sugar).toString+","+(m.coffee).toString)  //on accède a tous les attributs de m et on les écrit
      }
      println("Fichier sauvegardé avec succès")
      //succès puis fermeture
      ecriture.close()
    }
    catch {
      //Traitement de deux cas d'exceptions : fichier introuvable + problème d'écriture
      case e: java.io.IOException =>  //pb écriture / denied access
        println("Erreur : Echec de l’écriture dans "+filename+"\nLe fichier peut être verrouillé ou en lecture seule")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.FileNotFoundException =>  //fichier introuvable
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      //dans les 2 cas on quitte le programme
    }
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Mise à jour du code PIN pour la Machine " + machineId)
    var nouveau = "pin"
    var taille=0
    do {
      nouveau = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      taille = nouveau.length
    } while (!(taille==6))

    println("Le code PIN a été mis à jour avec succès.")
    machines(machineId-1).pincode = nouveau

    println("Retour au menu principal...\n")

  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Niveaux de stock actuels :")
    println("Poudre de café : " + machines(machineId-1).coffee + "g")
    println("Sucre          : " + machines(machineId-1).sugar + "g")
    printf("Lait           : %.2fL\n", machines(machineId-1).milk/1000.0)

    // Interface de réapprovisionnement pour ajuster les stocks
    println("\nEntrez les quantités à ajouter :")

    //Réapprovisionnement avec tests sur les valeur
    print("Poudre de café > ")
    var stock2 = readLine().toInt
    while(!(stock2>=0)){
      stock2 = readLine("Poudre de café > ").toInt
    }
    print("Sucre > ")
    var stock4 = readLine().toInt
    while(!(stock4>=0)){
      stock4= readLine("Sucre > ").toInt
    }
    print("Lait > ")
    var stock3 = readDouble()
    while(!(stock3>=0.0)){
      stock3 = readLine("Lait > ").toDouble
    }
    stock3 = stock3*1000

    //Mise à jour des stocks en appelant la méthode de la classeMachine add ingredient
    machines(machineId-1).addIngredient("lait",stock3.toInt)
    machines(machineId-1).addIngredient("sucre",stock4)
    machines(machineId-1).addIngredient("cafe",stock2)
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  def main(args: Array[String]): Unit = { //Fonction main du programme
    var machines = loadcsv("machines.csv")

    var manque=true
    // Code admin et variables générales

    var actif = true  //// Contrôle de la boucle principale de fonctionnement du distributeur
    var choix=0
    println("       Nospreso Café")

    while (actif) {
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      choix = readLine().toInt
      var machine=0
      if (choix == 1){ // Mode Client : interface principale pour commander une boisson
        while(machine!=1 && machine!=2 && machine!=3 && machine!=4 && machine!=5){
          print("Machine sélectionnée (1-5) > ")
          machine = readInt()
        }
        var retour = serveClient(machine,machines)  //appel de la fonction serveClient et récupération du booléen retourné pour savoir s'il reste du stock
        if(retour==false){
          println("Machine défectueuse ou stock épuisé sur la machine "+machine)  //Affichage d'erreur en cas d'insuffisance : false
        }
      }
      else if (choix == 2) { // Mode Admin : permet l'accès aux stocks et réapprovisionnement
        while(machine!=1 && machine!=2 && machine!=3 && machine!=4 && machine!=5){
          print("Machine sélectionnée (1-5) > ")
          machine = readInt()
        }
        var valider=false //condition pour sortir de la demande de mot de passe
        var tentatives=3  //nombre de tentatives initialement attribué à l'admin
        while(!valider){
          if(validatePin(machine,machines)){  //retour true signifie bon mot de passe
            println("Accès accordé à la machine "+machine)
            valider=true  //on sort
          }
          else{ //REtour false
            tentatives = tentatives - 1 //on décrémente
            println("Code PIN incorrect. "+tentatives+" tentatives restantes.")
            if(tentatives==0){  //plus de tentatives
              println("Trop de tentatives échouées. Fin du programme.")
              savecsv("machines.csv",machines)
              valider=true
              actif=false //pour finir le programme
            }
          }
        }
        if(actif){  //si actif est true cela veut dire que il nous reste des tentatives
          println("Quelle option souhaitez vous séléctionner ? ") //formulation d'un choix pour update ou restock
          println("1) Mettre à jour le code PIN")
          println("2) Mettre à jour le stock d'ingrédients")
          var souhait =0
          while(!(souhait==1 || souhait==2)){
            souhait = readLine("> ").toInt
          }
          if(souhait==1){
            updatePin(machine,machines)
          }
          else{
            restockMachine(machine,machines)
          }
        }



      } else if (choix == 3) {
        savecsv("machies.csv",machines) //on appelle save avant de quitter pour écrire dans le fichier
        actif = false // Arrête la boucle principale, mettant fin au programme
      }
    }
  }
}
