import java.io.PrintWriter
import io.StdIn._
import math._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

object Main {

  //Déclaration d'une nouvelle classe Machine qui va gérer une Machine, ses ingrédients, son code pin et la variation des stocks
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int)
  {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if(ingredient=="coffee")
        coffee+=amount
      else if(ingredient=="milk")
        milk+=amount
      else if(ingredient=="sugar")
        sugar+=amount
    }
    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if(ingredient=="sugar" && sugar>=amount){
        sugar-=amount
        return true
      }
      if(ingredient=="coffee" && coffee>=amount){
        coffee-=amount
        return true
      }
      if(ingredient=="milk" && milk>=amount){
        milk-=amount
        return true
      }

     return false
    }
  }

  def main(args: Array[String]): Unit = {

    val nbMachines=5
    // Stocks initiaux sous forme de tableaux de taille nbmachines, toutes les cases des tableaux sont initialisés par les valeurs entre parenthèses

    var machines = loadcsv("machines.csv") //On déclare machines, l'arraybuffer qui va gérer toutes les machines et machines est = à la valeur de retour de la fonction loadcsv après chargement dans le fichier envoyé

    while(true){  //boucle générale qui permet de toujours revenir à cet endroit lorsque l'on sort des modes 1,2 et 3
      //On déclare ici nos vraiables sur le lait supplémentaire comme ça ils ne restent pas en mémoire

      //Démarrage du distributeur
      println("Veuillez sélectionner votre mode : ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      // Demande à l'utilisateur ce qu'il veut faire entre les 3 choix.
      var selection = 0
      var machineId=0 //id de la machine
      do {
        selection = readLine(">").toInt
      } while (!(selection == 1 || selection == 2 || selection == 3))

      if(selection==3){
        //Mode EXIT, appel de la fonction exit
        savecsv("machines.csv",machines)
        System.exit(0)
      }
      while(machineId<1 ||  machineId>5){
        machineId = readLine("Machine sélectionnée (1-5) > ").toInt
      }
      if (selection == 1) {      // [Mode Client] (Commandes de boissons).
        serveClient(machineId-1,machines)  //fonction cliente
      }
      else if(selection==2){
        //Nous entrons dans le mode administrateur pour gérer les stocks
        //D'abord, vérifier la légitimité de l'administrateur avec le code pin
        println("Mode Admin")
        var essais=0
        var quitter=false //indicateur d'échec
        while(essais!=3){ //tant que l'utilisateur n'a pas épuisé ses 3 essais, répéter
          var mdp = validatePin(machineId-1,machines) //tester le code PIN
          if(mdp){  //si il est bon
            essais=3  //aller directement à 3 essais pour sortir de la boucle
            quitter=false
          }
          else{
            essais+=1 //augmenter un essai (perdre une tentative)
            print("Code PIN incorrect. ")
            print(3-essais)
            println(" tentatives restantes.")
            quitter=true  //
          }
        }
        if(quitter){
          println("Trop de tentatives échouées. Fin du programme.")
          savecsv("machines.csv",machines)
          System.exit(0)  //quitter le programme comme dans l'option 3
        }
        else{ //si tout s'est bien passé et que le code pin est valide
          var option=0
          var optionvalide=false  //Rester au cas
          do{
            println("Veuillez choisir une option administrateur : ")
            println("1) Réapprovisioner les ingrédients")
            println("2) Mettre à jour le code PIN")
            option = readLine("> ").toInt
            optionvalide = (option==1 || option==2)
            if(!optionvalide) println("Veuillez séléctionner une option comprise entre 1 et 2. ") //message d'erreur
            else{
              if(option==1){
                restockMachine(machineId-1,machines)
              }
              else if(option==2){
                updatePin(machineId-1,machines) //toujours enlever 1 à machineId pour éviter l'erreur out of bounds
              }
            }
          }while(!optionvalide) //revenir à la demande tant que ona  pas les bonnes conditions
        }
      }
    }

  }
  //mettre à jour le pin
  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    print("Mise à jour du code PIN pour la Machine")  //phrase à afficher imposée dans le sujet
    println(" "+(machineId+1))
    var changementDePin = ""  //variable qui stcok le nouveau mot de passe
    while (changementDePin.length!=6){  //tant qu'il ne fait pas 6 caractères, on redemande
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      changementDePin = readLine()  //stocker
    } //il fait ici 6 caractères pour sortir du while
    machines(machineId).pincode = changementDePin  //changement
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...\n")
  }

  //réapprovisionner la machine
  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    //Affichage des stocks actuel
    var laitiner = machines(machineId).milk/1000.0 //diviser par 1000 avant d'afficher
    println("Niveaux de stock actuels :")
    println("Poudre de café : "+ machines(machineId).coffee+"g")
    println("Sucre          : "+machines(machineId).sugar+"g")
    printf("Lait           : %.2f L\n",laitiner)

    println("Entrez les quantités à ajouter :")

    //Réajustement des stocks avec vérification de l'entrée utilisateur au cas où il mettrait des stocks négatifs
    print("Poudre de café: ")

    //utilisation des méthodes readInt et readDouble pour le lait
    var ajoutpoudrecafe=0
    do{
      ajoutpoudrecafe=readInt()
    }while(ajoutpoudrecafe<0)

    print("Sucre         :")
    var ajoutsucre=0
    do{
      ajoutsucre=readInt()
    }while(ajoutsucre<0)

    printf("Lait          : ")
    var ajoutlait=0.0
    do{
      ajoutlait=readDouble()
    }while(ajoutlait<0.0)

    println("Niveaux de stock mis à jour.")
    //Addition pour mettre à jour les stocks
    machines(machineId).addIngredient("coffee",ajoutpoudrecafe)
    machines(machineId).addIngredient("milk",(ajoutlait*1000).toInt)
    machines(machineId).addIngredient("sugar",ajoutsucre)

    println("Retour au menu principal...")
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    //machines arraybuffer que on va utiliser dans la fonction puis retourner au Main
    println("Chargement des machines depuis "+filename)
    val machines = ArrayBuffer[Machine]()
    var id=0
    try {
      val file = Source.fromFile(filename)
      val lines = file.reset.getLines
      lines.next  //ignorer le header
      for (line <- lines) { //parcours de ligne par ligne
        val stockage_ligne = line.split(",")
        machines += new Machine((id + 1), stockage_ligne(0), stockage_ligne(1).toInt, stockage_ligne(2).toInt, stockage_ligne(3).toInt)
        println("Machine "+machines(id).id+" chargée:")
        println("   ID: "+machines(id).id)
        println("   Code PIN: "+machines(id).pincode)
        printf("   Lait: %.3fL",machines(id).milk/1000.0)
        println("")
        println("   Sucre: "+ machines(id).sugar+"g")
        println("   Café: "+machines(id).coffee+"g\n")
        id=id+1
      }
      file.close()  //fermer le fichier ouvert en lecture
      println(machines.size+" machine(s) chargée(s) avec succès.")

    } catch {
      //Catch les exceptions possibles
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
    return machines
  }


  //Servir le client mode 1
  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var prixducafe=0.0
    var prixdusucre=0.0
    var prixdulait=0.0
    var panne_rester=true

    var choixlatte = 0
    var besoincafe = 0
    var besoinlait: Int = 0
    var stocksuffisant = true
    var demandesucre = 0


    //chaine de caractères qui comporte tous les caractères possibles du code alphanumérique à 5 caractères
    val possiblecarac = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    // Étape 1 : choix d'une boisson.
    var choixboisson = 0
    // quantité pour stock
    var quantitelaitselongrandeur = 0

    // Demande tant que les stocks sont insuffisants (donc on reste dans la boucle tant que stocksuffisant est faux)
    panne_rester=true
    while(panne_rester){
      var laitsupplementaire:Int = 0
      var doselait = 0
      besoinlait=0
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
          quantitelaitselongrandeur = 120
        } else if (choixlatte == 2) {
          quantitelaitselongrandeur = 150
        } else if (choixlatte == 3) {
          quantitelaitselongrandeur = 200
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
          laitsupplementaire = 50

          // Niveau de lait demandé : si 2 alors 100[ml] de lait
        } else if (doselait == 2) {
          laitsupplementaire = 100
          // Niveau de lait demandé : si 2 alors 100[ml] de lait
        } else {
          laitsupplementaire = 150
        }
      }
      //initialision des besoins des stocks en fonction de la boisson et du prix
      prixdulait = laitsupplementaire/1000.0
      if (choixboisson == 1) {  //Espresso
        besoincafe = 8
        prixducafe=2.00
      } else if (choixboisson == 2) { //capuccino
        prixducafe=2.50
        besoincafe = 6
        besoinlait = 100 + laitsupplementaire
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
      if (besoincafe > machines(machineId).coffee) {
        stocksuffisant = false
        println("Quantité de café insuffisante pour préparer la boisson sélectionnée.")
        println("veuillez essayer une autre boisson")
        println("....................................")
        panne_rester=true
        //vérification du stock lait insuffisant
      } else if (besoinlait > machines(machineId).milk) {
        stocksuffisant = false
        println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("veuillez essayer une autre boisson")
        println("....................................")
        panne_rester=true
        //vérification du stock sucre insuffisant
      } else if (demandesucre > machines(machineId).sugar) {
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
        if(choixlait!=2 && laitsupplementaire!=0){
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
        machines(machineId).removeIngredient("coffee",besoincafe)
        machines(machineId).removeIngredient("milk",besoinlait)
        machines(machineId).removeIngredient("sugar",demandesucre)


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
    true
  }

    def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var motdepasse="str"
    motdepasse = readLine("Entrez le code PIN : ")
    if(motdepasse!=machines(machineId).pincode){
      println("Mot de passe incorrect")
      false
    }
    else{
      println("Accès accordé à la machine "+(machineId+1))
      true
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    //Affichage d'informations avant sauvegarde
    print("Sauvegarde de ")
    print(machines.size)
    print(" machines dans ")
    println(filename)
    try{
      val fw = new PrintWriter(filename)  //Fichier
      fw.println("PINCODE,MILK,SUGAR,COFFEE") //ecrire le header
      for(unit <-machines){ //parcours des machines
        val concat = (unit.pincode)+"," //récupération du codepin concaténé avec une virgule
        fw.println(concat+(unit.milk).toString+","+(unit.sugar).toString+","+(unit.coffee).toString)
      }
      fw.close()  //fichier fermé
      //Affichage d'informations après sauvegarde
      println("Fichier sauvegardé avec succès")
    }
    catch { //exceptions levées
      case e: java.io.FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.io.IOException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule",filename)
        println("Fermeture du programme.")
        System.exit(0)
      case e: java.nio.file.AccessDeniedException =>
        printf("Erreur : Echec de l’écriture dans %s\nLe fichier peut être verrouillé ou en lecture seule",filename)
        println("Fermeture du programme.")
        System.exit(0)
    }
  }

  }