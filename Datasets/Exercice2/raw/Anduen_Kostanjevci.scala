import scala.util.Random
import scala.io.StdIn.readDouble  //pour le lait qui utilise une quantité à virgule
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object Main {

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    print("Entrez le code PIN : ")
    var pinclient = readLine("")
    if (machinePins(machineId-1)!=pinclient){ //Code pin différents
      return false  //Retorunez faux
    }
    else{ //bon code pin
      println("Accès accordé à la machine "+ machineId)
      return true
    }
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var recommencer = 1 //pour se répérer et rentrer en boucle dans le mode client quand les stocks sont insuffisants
    val lettresMAJ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" //Alphabet
    val chiffres = "1234567890" //Chiffres
    var breuvage = 0
    var machine_id_2=machineId - 1 // soustraire 1 pour avoir un ID cohérent
    var sucre_quantite = 0
    while(recommencer==1){  //Rester en boucle insuffisance
      var vote = 0
      var laitdose = 0  //reinitialiser
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
        laitdose = 100 + laitSupp*50 //dose de lait du capuccino
        println("Capuccino")
      }
      if(typedelatte==1){
        laitdose = 120 + laitSupp*50
        println("Latte (Petit)")
        breuvage = 6
      }
      if (typedelatte == 3) {
        laitdose = 200 + laitSupp*50
        println("Latte (Grand)")
        breuvage = 12
      }
      if (typedelatte == 2) {
        laitdose = 150 + laitSupp*50
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
      if (milkStocks(machine_id_2) - laitdose < 0.0){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        recommencer=1 //insuffisance
      }
      if (sugarStocks(machine_id_2) - sucre_quantite  <0){
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une dose plus faible dans votre boisson ou vérifier les stocks en mode Admin.")
        recommencer=1 //insuffisance
      }
      if (coffeeStocks(machine_id_2) - breuvage <0){
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
        milkStocks(machine_id_2) = milkStocks(machine_id_2)-laitdose  //lait réduit
        coffeeStocks(machine_id_2) -= breuvage //cafe reduit
        sugarStocks(machine_id_2) -= sucre_quantite //sucre reduit
        return true
      }
      else{
        return false
      }
    }
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    print("Mise à jour du code PIN pour la Machine ")
    println(machineId)
    var maj= readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    if(maj.length==6){
      //ne rien faire car c'est bon
    }
    else{ //sinon
      while(maj.length!=6){ //on fait répéter l'action
        maj= readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      } //on sort quand c'est 6
    }//on sort
    println("Le code PIN a été mis à jour avec succès.")
    machinePins(machineId-1) = maj  //changement de code pin sur la machineId - 1 pour que machinePins(5) n'existe pas car c'est une erreur
    println("Retour au menu principal...")
    println("")

  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels : \n")
    var reapps=0
    var reappl:Double=0
    var reappc=0  //variables pour la mise a jour des stocks après le mot de passe
    printf("     Poudre de café : %dg\n     Lait : %.2fL\n     Sucre : %dg\n",coffeeStocks(machineId-1),milkStocks(machineId-1)/1000.0,sugarStocks(machineId-1))  //Affichage des stocks disponibles avec printf et les formats d'affichage
    println("\nEntrez les quantités à ajouter : ")
    print("     Poudre de café : ")
    //Réapprovisionnement des trois stocks, impossible de mettre des valeurs <0, en revanche, pas de maximum mais il faut gérer la saisie donc utilisation de 3 do while
    do{
      reappc = readInt()
    }while(reappc<0)
    coffeeStocks(machineId-1) += reappc //ajout au stock de café
    print("     Lait : ")
    do{
      reappl = readDouble() //lire en double
    }while(reappl<0.0)
    var reappl_entier = (reappl*1000).toInt //nouvelle variable de conversion
    milkStocks(machineId-1) += reappl_entier //ajout au stock de lait mais avec la nouvelle variable
    print("     Sucre : ")
    do{
      reapps = readInt()
    }while(reapps<0)
    sugarStocks(machineId-1) += reapps //ajout au stock de sucre
    println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")  //Dernier message avant de revenir au mode de séléction menu
  }


  def main(args: Array[String]): Unit = {
    //Assignation des valeurs des trois provisions de départ, 30g de sucre, 0.500L de lait et 50g de poudre de café. Exercice 2 : Changement en array pour utiliser 5 machines
    val nbMachines=5
    var provision = Array.fill(nbMachines)(30)
    var provisionlait = Array.fill(nbMachines)(500)
    var provisiondecafe = Array.fill(nbMachines)(50)


    val adminPIN = Array.fill(nbMachines)("434343") //Code PIN de l'administrateur pour chaque machine

    while (true) {

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
      var id=0
      do{
        id = readLine("Machine sélectionnée (1-5) > ").toInt
      }while(id<=0 || id>5)

      if (action == 1 ) { //Si le choix est de prendre le mode client, écriture du mode client
        if(serveClient(id,provisiondecafe,provision,provisionlait)==false){
          println("La machine "+id+ "subit une insuffisance de stock. Veuillez en séléctionner une autre. ")
        }
      } else if (action == 2) {
        //Mode administrateur, après avoir entré un bon mot de passe, il est possible de remettre à niveau les stocks
        var boncode=0  // Indique si l'utilisateur a fourni le bon code
        var essaisRestants = 3    // Nombre maximum de tentatives pour entrer le code
        while(boncode==0){
          if(validatePin(id,adminPIN)){   // La fonction retourne true si le code est correct, sinon false
            boncode+=1 //quitter la boucle ==0
          }
          else{
            // Si le code est incorrect
            essaisRestants -= 1 // On diminue le nombre d'essais restants
            printf("Code PIN incorrect. %d tentatives restantes.\n",essaisRestants) //affichage préventif du nombre d'essais restants
            if(essaisRestants<=0)
            { // Si l'utilisateur n'a plus d'essais
              println("Trop de tentatives échouées. Fin du programme.") //Information quitter
              sys.exit(0) //sortie immédiate
            }
          }
        }
        //On va créer un sous-menu pour pouvoir déterminer ce que l'administrateur souhaite faire. Il a le choix entre appeler deux méthodes : restock ou update
        var methode =0
        println("Séléctionnez une méthode : ")
        println("1) Changer le code PIN de la machine "+id) //choix 1
        println("2) Restocker les ingrédients de la machine "+id) //choix 2
        do{
          methode = readLine("> ").toInt  //Demander le choix
        }while(methode<1 || methode>2)  //Tant qu'il n'est pas bon
        if(methode==1){
          updatePin(id,adminPIN)  //choix 1
        }
        else{
          restockMachine(id,provisiondecafe,provision,provisionlait)  //2
        }


      }
    }
  }
}
