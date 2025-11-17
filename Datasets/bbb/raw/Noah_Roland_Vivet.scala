import scala.io.StdIn.{readDouble, readInt, readLine} //librairie pour lire les double, int et string
import scala.util.Random  //pour générer le code twint

/*Objectif :
  L'objectif est de développer un programme qui simule le fonctionnement de 5 distributeurs automatiques de
  boissons (machine à café). Le programme doit permettre à l’utilisateur-trice de
 sélectionner une boisson, personnaliser sa commande, gérer les stocks d’ingrédients, traiter les
 paiements, et mettre à jour les stocks après chaque transaction. Il doit également permettre un
 mode administration protéger par un code PIN pour réapprovisionner les stocks.
 Comme il y a 5 machines, on peut commander sur une machine souhaitée, réapprovisionner les stocks ou modifier le code PIN
 de la machine souhaitée
 */

object Main {

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels :")
    var id_moins = machineId - 1
    println("   Poudre de café: "+coffeeStocks(id_moins)+"g")
    printf("   Lait          : %.2fL\n",milkStocks(id_moins)/1000.0)  //Affichage avec %.2f pour avoir 2 chiffres après la virgule
    println("   Sucre         : "+sugarStocks(id_moins)+"g")

    //Proposition de réapprovisionnement des stocks sous forme d'entrée INT
    println("Souhaitez-vous réapprovisionner les stocks ?")
    println("1) Oui\n2) Non")
    var nouveaustock=0
    do{
      println(">")
      nouveaustock = readInt()
    }while(nouveaustock!=1 && nouveaustock!=2)
    if(nouveaustock==1){
      println("Entrez les quantités à ajouter :")

      print("   Poudre de café: ")
      var ajoutcafe = readInt()
      while(ajoutcafe<0)ajoutcafe = readLine("   Poudre de café: ").toInt

      //Utilisation de readDouble car on lit un nombre a virgule
      print("   Lait: ")
      var ajoutlait = readDouble()
      while(ajoutlait<0.0)ajoutlait = readLine("   Lait: ").toDouble

      print("   Sucre: ")
      var ajoutsucre = readInt()
      while(ajoutsucre<0)ajoutsucre = readLine("   Sucre: ").toInt

      //Affichage du retour au menu avant de sortir de l'option mode Admin et de l'option de réapprovisionnement de stocks
      println("Niveaux de stock mis à jour.")
      println("Retour au menu principal...")
      ajoutlait = ajoutlait*1000
      coffeeStocks(id_moins)+=ajoutcafe
      milkStocks(id_moins)+=ajoutlait.toInt
      sugarStocks(id_moins)+=ajoutsucre
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var code = readLine("Enter PIN: ")  //saisie du mot de passe
    var retour=false
    if (code==machinePins(machineId - 1)) { //comparaison des deux code PIN
      retour=true
    }
    else {
      retour=false
    }
    return retour //REturn du booléen assigné
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = { //Modifier le code PIN
    var neufPIN = ""
    val taillesouhaitee:Int=6 //Taille du mot de passe imposée par le sujet de l'exercice 2. Si jamais on doit le modifier, il faut changer la valeur ici
    printf("Updating for Machine %d\n",machineId)
    do {
      neufPIN = readLine("Enter new 6-digit PIN > ")
      machinePins(machineId-1) = neufPIN  //Changer l'ancienne valeur
    } while (taillesouhaitee!=neufPIN.length) //Tant qu'on a pas la longueur voulu, on redemande et on ré-associe
    println("PIN updated successfully.")
    println("Returning to the main menu...")
    println("")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    //On déclare ici les variables qui nous seront essentielles pour le mode client, donc les quantités demandées par le client etc...
    var chaineboisson:String="void"
    var quantiteDeLait = 0
    var quantiteDeCafe = 0
    var quantiteDeSucre = 0
    var id_moins_un = machineId - 1
    var plusdeLait=0
    var dosesLait=0
    var prix=0.0  //Initialisation du prix
    var prixsucresupp=0.0
    val alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val tempsPaiement = 3000


    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte- CHF 2.70 (Small), CHF 3.20 (Medium), CHF 3.70 (Large)")
    var boisson=0

    //Même procédé que plus haut, nous allons répéter ce procédé plusieurs fois lors de ce programme, nous déclarons une variable, puis nous lisons une entrée utilisateur, si cette entrée ne convient pas par rapport aux valeurs attendus, nous restons dans une boucle de demande à l'utilisateur jusqu'à ce que la donnéee soit celle attendue
    do{
      println(">")
      boisson = readInt()
    }while((boisson!=1) && (boisson!=2) && (boisson!=3))

    //En fonction du choix de l'utilisateur, on définit une chaîne de caractères qui correspond à la boisson que celui-ci a demandé
    if(boisson==1) chaineboisson="Expresso"
    if(boisson==2) chaineboisson="Capuccino"
    if(boisson==1) prix=2.00
    if(boisson==2) prix=2.50
    if(boisson==1) quantiteDeCafe=8
    if(boisson==2) quantiteDeCafe=6
    if(boisson==2) quantiteDeLait=100
    if(boisson==3){
      chaineboisson = "Latte"
      println("Veuillez sélectionner la taille de votre boisson :")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      var tailleboisson=0
      while(tailleboisson<1 || tailleboisson>3)
      {
        println(">")
        tailleboisson = readInt()
      }
      //Une fois que l'utilisateur a choisi la taille de son Latte, il faut garder en mémoire les quantités que requiert la boisson en terme de lait et de café. Pour gagner en optimisation, on peut également garder en mémoire le prix et ajouter à notre chaîne de caractères boissons la taille
      if(tailleboisson==3)
      {
        prix=3.70
        quantiteDeLait=200        //lait consommé par un grand latte (en L) soit 20 cl ou 200mL
        quantiteDeCafe=12         //poudre de café consommée par un Grand Latte
        chaineboisson+=" (Grand)" //ajout a la chaîne la sépcification de la taille du latte
      }
      if(tailleboisson==1)
      {
        prix=2.70
        quantiteDeLait=120
        quantiteDeCafe=6
        chaineboisson+=" (Petit)"
      }
      if(tailleboisson==2)
      {
        prix=3.20
        quantiteDeLait=150
        quantiteDeCafe=8
        chaineboisson+=" (Moyen)"
      }
    }
    //Le cas spécial des tailles pour le Latte a été traité donc on sort de la condition boisson=3 pour proposer du sucre au client
    var propositionSucre=0
    var chainedeSucre="Sans sucre"
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    do{
      println(">")
      propositionSucre = readInt()
    }while(propositionSucre<1 || propositionSucre>4)

    if(boisson==2 || boisson==3)
    {
      //Si la boisson est soit capuccino ou soit Latte, alors on a le droit d'avoir du lait en supplément, donc on peut poser la question au client s'il souhaite du lait en supplément
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui\n2) Non")
      do{
        println(">")
        plusdeLait = readInt()
      }while((plusdeLait!=1)&&(plusdeLait!=2))

      if(plusdeLait!=2)
      {
        //Si l'utilisateur n'a pas choisi l'option 2)Non alors on lui demande combien de doses il souhaite
        println("Combien de doses ?")
        do{
          println(">")
          dosesLait = readInt()
        }while((dosesLait<0)||(dosesLait>3))

        quantiteDeLait = ((quantiteDeLait + ((dosesLait*0.05)*1000).toInt))
      }
    }
    //Ici, on va calculer stratégiquement la quantité de sucre et le prix de manière proportionnelle à ce que l'utilisateur a choisi. 1,2,3 ou 4. Si l'utilisateur a choisi 1, pas de sucre, donc on ne traite pas ce cas. EN revanche, s'il a choisi 2 par exemple, cela représente Peu de sucre (5g) et 0.10 CHF. Si on soustrait 1, 2-1 = 1. A partir de ce 1 on peut avoir le prix (en divisant /10) et avoir la quantité en faisant *5 (5g)
    quantiteDeSucre = ((propositionSucre-1)*5)
    prixsucresupp = (propositionSucre-1)/10.0
    //La dernière chose que on ne peut pas optimiser est la quantité en terme de chaîne de caractères donc on l'écrit en "dur"
    if(propositionSucre==4) chainedeSucre="Beaucoup ("+quantiteDeSucre.toString+"g)"
    if(propositionSucre==3) chainedeSucre="Moyen ("+quantiteDeSucre.toString+"g)"
    if(propositionSucre==2) chainedeSucre="Peu ("+quantiteDeSucre.toString+"g)"
    println("Boisson sélectionnée : "+chaineboisson)
    println("Niveau de sucre : "+chainedeSucre)
    if(coffeeStocks(id_moins_un)>=quantiteDeCafe){
      if(milkStocks(id_moins_un)>=quantiteDeLait){
        if(sugarStocks(id_moins_un)>=quantiteDeSucre){
          //Si ces 3 conditions sont réunies, cela veut dire que on peut se servir dans la machine car les stocks des trois produits sont disponibles en quantié suffisante
          milkStocks(id_moins_un) = (milkStocks(id_moins_un) - quantiteDeLait)
          coffeeStocks(id_moins_un) -= quantiteDeCafe
          sugarStocks(id_moins_un) = sugarStocks(id_moins_un) - quantiteDeSucre

          var prixTotal = prix + prixsucresupp + dosesLait*0.05
          if(dosesLait>0)println("Lait supplémentaire : "+dosesLait)
          else println("Lait supplémentaire : Non")
          printf("Prix total : CHF %.2f",prix)
          if(quantiteDeSucre>0)printf(" + CHF %.2f",prixsucresupp)
          if(dosesLait>0)printf(" + CHF %.2f", dosesLait*0.05)
          printf(" = CHF %.2f\n\n", prixTotal)

          println("Veuillez payer en utilisant Twint.")
          var i=0
          var codeDePaiement=""
          while(i<5){
            codeDePaiement += alpha(Random.nextInt(36))
            i+=1
          }
          println("Votre code de paiement est : "+codeDePaiement)
          println("(En attente de paiement...)")

          Thread.sleep(tempsPaiement)

          println("")
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")
          println("Votre "+ chaineboisson +" est prêt ! Bonne dégustation !\n")
          return false
        }
        //Maintenant on doit traiter les cas où les 3 conditions ne sont pas réunies, on les traite un par un
        else{
          //Premier cas, manque de sucre
          println("Erreur : Quantité de sucre insuffisante pour préparer\n la boisson sélectionnée.")
          println("Veuillez choisir une quantité plus faible de sucre dans votre boisson ou vérifier les\n stocks en mode Admin.")
          return true
        }
      }
      else{
        //Deuxième cas, manque de lait
        println("Erreur : Quantité de lait insuffisante pour préparer\n la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer\n une autre boisson.")
        return true
      }
    }
    else{
      //Troisième cas, manque de café
      println("Erreur : Quantité de poudre de café insuffisante pour\n préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les\n stocks en mode Admin.")
      return true
    }
  }
    def main(args: Array[String]): Unit = {
    val nbmachines=5  //Nombre de machines
    //Variables qui initialisent les stocks de nos trois produits à leurs valeurs de base. On le fait ici car si on le faisait plus bas, par exemple dans le while(true), les valeurs reviendraient à ces données à chaque tour de boucle
    var cafenospresso= Array.fill(nbmachines)(50)
    var laitnospresso=Array.fill(nbmachines)(500)
    var sucrenospresso=Array.fill(nbmachines)(30)
    var pannetechnique=false
    val motdepasse=Array.fill(nbmachines)("434343")
    var modeDeSelection = 0
    while(true){  //tant que true est vrai (ce qui est tout le temps le cas), on affiche le menu
      //Affichage du menu
      modeDeSelection = 1
      if(!pannetechnique)
      {
        println("             Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        //modeDeSelection enregistre le mode que l'utilisateur a choisi à travers son clavier
        modeDeSelection = readLine(">").toInt
        //Tant que ce mode n'est pas 1 ou 2 ou 3, demander à l'utilisateur de saisir encore
        while((modeDeSelection!=1) && (modeDeSelection!=2) && (modeDeSelection!=3)){
          modeDeSelection = readLine(">").toInt
        }
      }
      var selection_m=0
      if(modeDeSelection!=3){
        print("Machine séléctionnée (1-5) > ")  //Affichage
        selection_m = readInt() //Lecture de la machine (ID)
        while(selection_m<=0 || selection_m>=6){  //Vérification de l'entrée
          selection_m = readLine("> ").toInt
        }
        //Machine entre 1 et 5 qui correspond à 0 et 4
      }

      //Ecriture de 3 parties avec la condition posé sur le modeDeSelection : traitement du mode client, puis admin et exit
      if(modeDeSelection==1)
      {
        var resultat = serveClient(selection_m,cafenospresso,sucrenospresso,laitnospresso)
      }
      else if(modeDeSelection==2) {
        //Panneau de configuration Admin pour observer et réajuster les stocks
        println("Mode Admin")
        var tentatives=3
        while(tentatives>0){
          if(validatePin(selection_m,motdepasse)){
            println("Veuillez séléctionner un mode: ")
            println("1) Mettre à jour les stocks d'ingrédients\n2) Mettre à jour le code PIN")
            var modde=0
            do{
              modde = readLine(">").toInt
            }while(modde<1 || modde>2)
            if(modde==1) restockMachine(selection_m,cafenospresso,sucrenospresso,laitnospresso)
            else updatePin(selection_m,motdepasse)
            tentatives= -10
          }
          else{
            tentatives-= 1
            print("Code PIN incorrect. ")
            println(tentatives+" tentatives restantes.")
          }
        }
        if(tentatives==0){
          println("Trop de tentatives échouées. Fin du programme.")
          sys.exit(0)
        }
      }
      else if(modeDeSelection==3)
      {
        //Fonction qui va nous permettre de sortir de toutes les conditions, boucles etc et de provoquer l'arrêt définitif du programme avec un code succès 0
        sys.exit(0)
      }
    }
  }
}//FIN DU PROGRAMME

