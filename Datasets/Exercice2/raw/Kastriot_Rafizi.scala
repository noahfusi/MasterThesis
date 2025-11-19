import io.StdIn._



object Main {

  ////////////// les variables de l'exo 1 pour le mode client

  var boisson = 0
  var nom_boisson = ""

  var prix_base = 0.0
  var prix_sucre = 0.0
  var prix_lait = 0.0 // selon le nombre de doses

  var prix_total = 0.0

  var sucre_ajout = 0 //dose de sucre en grammes rajoutée
  var quantite_sucre = 0.0 //1 sans sucre, 2 peu (5g), 3 moyen(10g) ou 4 beaucoup (15g)
  var quant_sucre_string = "" // sans sucre, peu, moyen ou beaucoup

  var ajouter_lait = 0 //1 pour oui, 2 pour non
  var lait_supp = "" // oui ou non
  var dose_lait = 0 //1,2 ou 3

  var cafe_necessaire = 0
  var lait_necessaire = 0

  var taille_latte = 0 //1 petit, 2 moyen, 3 grand

  //////////////////// fin variables exo 1 pour le mode client


  var preparation = false

  val nbMachines = 5 // par défaut


  ///////////// stocks \\\\\\\\\\\
  var stock_cafe = Array.fill[Int](nbMachines)(50)   // 50 grammes
  var stock_sucre = Array.fill[Int](nbMachines)(30)  // 30 grammes
  var stock_lait = Array.fill[Int](nbMachines)(500) // 500 ml



  val pinMachines = Array.fill(nbMachines)("434343") // par défaut
  var tentativePIN = ""
  var tentative = 3
  var code_valid = false

  var nouveauPIN = ""

  var menu = 0

  var machineChoisie = 0

  var choixAdmin = 0 // 1 si pour refaire le stock ou 2 pour changer le PIN


  // val numero afin de vérifier que le nouveau code pin ne contient que des chiffres
  val numero = Array[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')


  /////////////////////////        Methodes

  // vérification du code pin
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    print("Entrez le code PIN : > ")
    tentativePIN = readLine()
    if (tentativePIN == machinePins(machineId)){
      return true
    }
    else{
      println("Erreur")
      return false
    }
  } /////////// fin vérification code pin

  // update du code PIN
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var etat_pin = 1 // en gros, cette variable restera à 1 tant que le nouveau pin proposé reste à 1
    println(s"\nMise à jour du code PIN de la machine ${machineId+1}: ")


    do{
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      nouveauPIN=readLine()
      etat_pin = 1 // commencé par ça pour que si le pin n'est pas bon car il y a des caractères alors on revient à 1
      // on vérifie si le pin n'a que des numéros
      for(i<-0 to nouveauPIN.length-1){
        if(numero.contains(nouveauPIN(i))){
          etat_pin *= 1 // si le caractère est un chiffre on multiplie par 1
        }
        else{
          etat_pin *= 0 // si caractère autre que chiffre alors etat_pin devient 0
        }
      }
      
      if(etat_pin == 1 && nouveauPIN.length == 6){
        println("Le code a été mis à jour avec succès.")
        machinePins(machineId) = nouveauPIN
      }
      else{
        println("PIN invalide.")

      }


    }while(etat_pin != 1 || nouveauPIN.length != 6)
  } // fin de la fonction updatePIN



  //// Refaire le stock de la machine
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit={

    var ajout_cafe = 0
    var ajout_sucre = 0
    var ajout_lait = 0.0


    println(s"Niveaux des stocks actuels de la machine ${machineId+1}: ")
    println("\tPoudre de café : " + coffeeStocks(machineId) + "g")
    println("\tSucre          : " + sugarStocks(machineId) + "g")
    println("\tLait           : " + (milkStocks(machineId))/1000.0 + "L") // pour afficher en litre

    println("\nEntrez les quantités à ajouter : ")
    print("Poudre de café (g): ")
    ajout_cafe = readInt()
    while(ajout_cafe<0){
      println("Erreur !\nVeuillez entrer une quantité valide !")
      print("Poudre de café (g): ")
      ajout_cafe = readInt()
    }

    print("Sucre (g)         : ")
    ajout_sucre = readInt()
    while(ajout_sucre<0){
      println("Erreur !\nVeuillez entrer une quantité valide !")
      print("Sucre (g)         : ")
      ajout_sucre = readInt()
    }

    print("Lait (L)          : ")
    ajout_lait = readDouble()
    while(ajout_lait<0){
      println("Erreur !\nVeuillez entrer une quantité valide !")
      print("Lait (L)          : ")
      ajout_lait = readDouble()
    }

    coffeeStocks(machineId) += ajout_cafe
    sugarStocks(machineId) += ajout_sucre
    milkStocks(machineId) += (ajout_lait*1000).toInt

    println("Les stocks ont été mis à jour avec succès.")

  }// fin de la fonction de réapprovisionnement du stock


  //// méthode client
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    //le client choisi la boisson dans la variable boisson
    print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
    boisson = readInt()
    //on le force à choisir l'une des boisson sinon on lui repose la question
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      println("Erreur Choix")
      print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ")
      boisson = readInt()
    }
    if(boisson == 1){ // cas expresso
      cafe_necessaire = 8
      nom_boisson = "Expresso"
      prix_base = 2.00
      // demande pour le sucre
      do {
        print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
        quantite_sucre = readInt()
        if (quantite_sucre == 1) {
          quant_sucre_string = "Sans sucre"
          sucre_ajout = 0
        }
        else if (quantite_sucre == 2) {
          quant_sucre_string = "Peu (5g)"
          sucre_ajout = 5
          prix_sucre = 0.10
        }
        else if (quantite_sucre == 3) {
          quant_sucre_string = "Moyen (10g)"
          sucre_ajout = 10
          prix_sucre = 0.20
        }
        else if (quantite_sucre == 4) {
          quant_sucre_string = "Beaucoup (15g)"
          sucre_ajout = 15
          prix_sucre = 0.30
        }
        else {
          println("Erreur choix")
        }
      } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)

      if(coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) < sucre_ajout){ // on a 4 cas
        println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
      }
      else if(coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId)>=sucre_ajout){
        println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
      }
      else if(coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId)<sucre_ajout){
        println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
      }
      else{
        preparation = true
        coffeeStocks(machineId)-=cafe_necessaire
        sugarStocks(machineId)-=sucre_ajout
      }
    }//fin cas expresso

    else{ // cas cappuccino et latte : boissons nécessitant du lait

      if (boisson == 2){ // cas cappuccino
        cafe_necessaire = 6
        lait_necessaire = 100
        nom_boisson = "Cappuccino"
        prix_base = 2.50

        // demande pour le sucre
        do {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
          quantite_sucre = readInt()
          if (quantite_sucre == 1) {
            quant_sucre_string = "Sans sucre"
            sucre_ajout = 0
          }
          else if (quantite_sucre == 2) {
            quant_sucre_string = "Peu (5g)"
            sucre_ajout = 5
            prix_sucre = 0.10
          }
          else if (quantite_sucre == 3) {
            quant_sucre_string = "Moyen (10g)"
            sucre_ajout = 10
            prix_sucre = 0.20
          }
          else if (quantite_sucre == 4) {
            quant_sucre_string = "Beaucoup (15g)"
            sucre_ajout = 15
            prix_sucre = 0.30
          }
          else {
            println("Erreur choix")
          }
        } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)
        //demande de lait
        print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
        ajouter_lait = readInt()
        while (ajouter_lait != 1 && ajouter_lait != 2) {
          print("Erreur choix\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
          ajouter_lait = readInt()
        }
        if(ajouter_lait == 1){
          lait_supp = "Oui"
          print("Combien de dose ? (Max.3)\n> ")
          dose_lait = readInt()
          while (dose_lait != 1 && dose_lait != 2 && dose_lait != 3) {
            print("Erreur choix !\nEntre 1 et 3 doses !\n> ")
            dose_lait = readInt()
          }
          lait_necessaire += dose_lait * 50
          prix_lait = dose_lait * 0.05
        }
        else{
          lait_supp = "Non"
        }


        if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) < lait_necessaire){ // on a 8 cas
          println("Quantité de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId) < cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else{
          preparation = true
          coffeeStocks(machineId)-=cafe_necessaire
          sugarStocks(machineId)-=sucre_ajout
          milkStocks(machineId)-=lait_necessaire
        }
      } //fin capuccino

      else{ // cas latte
        //demande taille
        print("Quelle taille pour le latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
        taille_latte = readInt()
        while (taille_latte != 1 && taille_latte != 2 && taille_latte != 3) {
          print("Erreur choix\nQuelle taille pour le latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ")
          taille_latte = readInt()
        }
        if(taille_latte == 1){
          cafe_necessaire = 6
          lait_necessaire = 120
          prix_base = 2.70
          nom_boisson = "Latte (Petit)"
        }
        else if(taille_latte == 2){
          cafe_necessaire = 8
          lait_necessaire = 150
          prix_base = 3.20
          nom_boisson = "Latte (Moyen)"
        }
        else{
          cafe_necessaire = 12
          lait_necessaire = 200
          prix_base = 3.70
          nom_boisson = "Latte (Grand)"
        }

        // demande pour le sucre
        do {
          print("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g)\n3) Moyen (10g)\n4) Beaucoup (15g)\n> ")
          quantite_sucre = readInt()
          if (quantite_sucre == 1) {
            quant_sucre_string = "Sans sucre"
            sucre_ajout = 0
          }
          else if (quantite_sucre == 2) {
            quant_sucre_string = "Peu (5g)"
            sucre_ajout = 5
            prix_sucre = 0.10
          }
          else if (quantite_sucre == 3) {
            quant_sucre_string = "Moyen (10g)"
            sucre_ajout = 10
            prix_sucre = 0.20
          }
          else if (quantite_sucre == 4) {
            quant_sucre_string = "Beaucoup (15g)"
            sucre_ajout = 15
            prix_sucre = 0.30
          }
          else {
            println("Erreur choix")
          }
        } while (quantite_sucre != 1 && quantite_sucre != 2 && quantite_sucre != 3 && quantite_sucre != 4)
        //demande de lait
        print("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
        ajouter_lait = readInt()
        while (ajouter_lait != 1 && ajouter_lait != 2) {
          print("Erreur choix\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ")
          ajouter_lait = readInt()
        }
        if(ajouter_lait == 1){
          lait_supp = "Oui"
          print("Combien de dose ? (Max.3)\n> ")
          dose_lait = readInt()
          while (dose_lait != 1 && dose_lait != 2 && dose_lait != 3) {
            print("Erreur choix !\nEntre 1 et 3 doses !\n> ")
            dose_lait = readInt()
          }
          lait_necessaire += dose_lait * 50
          prix_lait = dose_lait * 0.05
        }
        else{
          lait_supp = "Non"
        }

        if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de café, de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de café et de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)<cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId) < cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) < lait_necessaire){
          println("Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) < sucre_ajout && milkStocks(machineId) >= lait_necessaire){
          println("Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        }
        else if (coffeeStocks(machineId)>=cafe_necessaire && sugarStocks(machineId) >= sucre_ajout && milkStocks(machineId) < lait_necessaire){ // on propose une autre taille que lorsque le café et le sucre sont dispo mais pas le lait (si pas de café alors ça ne sert à rien
          if (taille_latte==1){
            println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
          }
          else {
            println("Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou une autre boisson ou vérifier les stocks en mode Admin.\n")
          }
        }
        else{
          preparation = true
          coffeeStocks(machineId)-=cafe_necessaire
          sugarStocks(machineId)-=sucre_ajout
          milkStocks(machineId)-=lait_necessaire
        }
      } // fin latte
    } // fin cas boissons lactées
    //paiement
    if(preparation==true){
      prix_lait = ((prix_lait*100).round)/100.0 // j'utilise cette méthode .round car pour 3 doses de lait, l'imprésicion des calculs donne 0.150000000..2
      if (boisson == 1){ // cas expresso : 2 cas : sucré ou non
        println("\nBoisson sélectionnée : " + nom_boisson)
        println("Niveau de sucre      : " + quant_sucre_string)
        if (quantite_sucre == 1){ // cas sans sucre
          printf("Prix total : CHF %.2f\n", prix_base)
        }
        else{ // cas sucre
          prix_total = (prix_base+prix_sucre)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prix_base, prix_sucre, prix_total)

        }
      }

      else{ // cas des boisson avec lait

        println("\nBoisson sélectionnée : " + nom_boisson)
        println("Niveau de sucre      : " + quant_sucre_string)
        println("Lait supplémentaire  : " + lait_supp)
        if (quantite_sucre == 1){ // pas de sucre !
          if (lait_supp =="Non"){ // cas sans sucre et sans dose de lait supplémentaire
            printf("Prix total : CHF %.2f\n", prix_base)
          }
          else{ // si il y a du lait, on rajoute le prix du lait dans le print
            prix_total = (prix_base+prix_lait)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prix_base, prix_lait, prix_total)
          }
        }
        else{ // cas avec du sucre
          if (lait_supp =="Non"){ //cas sucre sans dose de lait supplémentaire
            prix_total = (prix_base+prix_sucre)
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prix_base, prix_sucre, prix_total)
          }
          else{
            prix_total = (prix_base+prix_lait+prix_sucre)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prix_base, prix_sucre, prix_lait, prix_total)
          }
        }
      }
      val alpha_num = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var code_twint = ""
      var indice = (math.random()*36).toInt
      for (i <-1 to 5){
        code_twint += alpha_num(indice)
        indice = (math.random()*36).toInt
      }
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_twint)
      println("\n(En attente de paiement...)\n")

      //attente de 5 secondes pour le paiement
      Thread.sleep(3000)
      //////////
      println("Paiement confirmé.\nPréparation de votre boisson...")
      Thread.sleep(5000)
      println("\nVotre " + nom_boisson + " est prêt ! Bonne dégustation !\n")

    }
    else{
      //cas où preparation n'est pas possible car il manque des ingrédients !!
      println("Pas de préparation\nSélectionnez une autre machine.\n")
    }
    return preparation
  } /////7 fin méthode client
  
  ///////////////////////////////// Fin Méthodes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\





  def main(args: Array[String]): Unit = {




    do{
      // on choisit le menu et on le force à choisir entre 1 et 3
      print("\t\tNospresso\nVeuillez choisir un mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
      menu = readInt()
      while(menu != 1 && menu != 2 && menu != 3){
        print("Erreur\nVeuillez choisir un mode :\n1) Client\n2) Admin\n3) Quitter\n> ")
        menu = readInt()
      }

      if(menu == 2){
        println("Mode Admin")
        print(s"Machine sélectionnée (1-$nbMachines) > ")
        machineChoisie = readInt()
        while(machineChoisie<1 || machineChoisie>nbMachines){
          println("Erreur")
          print(s"Machine sélectionnée (1-$nbMachines) > ")
          machineChoisie=readInt()
        }
        /////// choix de la machine faite entre 1 et nbre de machines


        while(code_valid==false && tentative>0){
          code_valid = validatePin(machineChoisie-1, pinMachines)
          tentative-=1




          if(code_valid== false){
            if(tentative>1){
              println(s"Code PIN incorrect. ${tentative} tentatives restantes.")
            }
            else if(tentative== 1){
              println("Code PIN incorrect. 1 tentative restante.")
            }
            else{
              println("Trop de tentatives échouées. Fin du programme.")
            }
          }
        }







        if(code_valid==false){
          menu = 3
        }
        else{ // cas où ça marche
          println(s"\nAccès accordé à la machine ${machineChoisie}\n")
          // on demande si il veut update le code pin de la machine ou restocker
          print(s"Que voulez-vous faire :\n1) Réapprovisionner les ingrédients pour la machine ${machineChoisie}\n2) Changer le code PIN de la machine ${machineChoisie}\n> ")

          choixAdmin = readInt()
          while(choixAdmin!=1 && choixAdmin!=2){
            println("Erreur")
            print(s"Que voulez-vous faire :\n1) Réapprovisionner les ingrédients pour la machine ${machineChoisie}\n2) Changer le code PIN de la machine ${machineChoisie}\n> ")
            choixAdmin = readInt()
          }
          if(choixAdmin == 1){
            // cas où on refait le stock
            restockMachine(machineChoisie-1, stock_cafe, stock_sucre, stock_lait)
          }
          else{
            //cas où on change le code PIN
            updatePin(machineChoisie-1, pinMachines)
          }
          println("Retour au menu principal...\n")
        }

        tentative = 3
        code_valid = false

        // on réinitialise le nbre de tentatives pour le code pin et le test de la validité du pin


      }// fin mode admin


      else if (menu == 1){ // mode client

        println("Mode Client")

        do {
          print(s"Machine sélectionnée (1-$nbMachines) > ")
          machineChoisie = readInt()
          while(machineChoisie<1 || machineChoisie>nbMachines){
            println("Erreur")
            print(s"Machine sélectionnée (1-$nbMachines) > ")
            machineChoisie=readInt()
          }
        }while(!serveClient(machineChoisie-1, stock_cafe, stock_sucre, stock_lait))

        // la boisson est préparée
        println("Retour au menu principal...\n")
        preparation = false // on réinitialise à l'état false
      }// fin mode client

      else{
        println("Vous quittez le programme...")
      }


    //println("Retour au menu principal...\n")

    }while(menu !=3)


  }
}




