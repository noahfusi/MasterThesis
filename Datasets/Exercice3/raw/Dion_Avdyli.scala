import scala.io.StdIn._
import java.io.PrintWriter
import scala.io.Source
import scala.collection.mutable.ArrayBuffer


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  val idMachine = id
  var codepin = pincode
  var lait = milk
  var sucre = sugar
  var cafe = coffee


  def addIngredient(ingredient: String, amount: Int): Unit = {
    if(ingredient=="sugar"){
      sucre += amount
    }
    else if(ingredient=="milk"){
      lait += amount
    }
    else {
      cafe += amount
    }


  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if(ingredient=="sugar" && sucre>=amount){
      sucre=sucre-amount
      return true
    }
    else if(ingredient=="milk" && lait>=amount){
      lait=lait-amount
      return true
    }
    else if(ingredient=="coffee" && cafe>=amount){
      cafe = cafe-amount
      return true
    }
    else{
      return false
    }
  }
}


object Main {

  var menu = 0
  var boucle = true
  var sauv = false

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit={
    try{
      val file_writter = new PrintWriter(filename)
      file_writter.println("PIN" + "," + "MILK" + "," + "SUGAR" + "," + "COFFEE")
      for (i <- 0 to machines.size - 1) {
        file_writter.println(machines(i).codepin + "," + machines(i).lait + "," + machines(i).sucre + "," + machines(i).cafe)
      }
      file_writter.close()
      sauv = true
    }
    catch{
      case _=> sauv = false
    }
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    try {
      var arrayMachine = new ArrayBuffer[Machine]()
      var id = 1
      val r = Source.fromFile(filename)
      var ligne_r = r.reset.getLines
      var l = ligne_r.next

      while (!ligne_r.isEmpty) {
        l = ligne_r.next

        var machine = l.split(",")

        arrayMachine += new Machine(id, machine(0), machine(1).toInt, machine(2).toInt, machine(3).toInt)
        id+=1
      }
      return arrayMachine
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        return null
    }
  }


  // Nomination de toutes les  variables du programme
  val prixE = 2.0
  val prixC = 2.5
  val prixLP = 2.7
  val prixLM = 3.2
  val prixLG = 3.7
  var prixTotal = 0.0
  val prixsucre = 0.1
  val prixlait = 0.05
  var tentatives = 3


  //////// Déclaration des méthodes
  var validation = false
  def validatePin(mach: Machine): Boolean = {
    validation = false
    for(x<- 1 to tentatives){
      var CODEPIN = readLine("\nEntrez le code PIN : \n> ")
      if (CODEPIN == mach.codepin) {
        println("Accès accordé à la Machine " + (mach.idMachine))
        return true
      }
      else if(CODEPIN != mach.codepin){
        tentatives = tentatives - 1
        print("Code PIN incorrect. " + tentatives + " tentatives restantes. \n> ")}}

    if(tentatives == 0) {
      print("\nTrop de tentatives échouées. Fin du programme.")
    }
    false
  }

  def updatePin(mach: Machine): Unit ={
    var nvPin = ""
    print("\nMise à jour du code PIN pour la Machine " + (mach.idMachine))
    while((nvPin.length != 6) || (!nvPin.forall(_.isDigit))) {
      nvPin = readLine("\nEntrez un nouveau code PIN à 6 chiffres > ")
      if((nvPin.length == 6)&&(nvPin.forall(_.isDigit))){}
    }
    mach.codepin = nvPin
    print("\nLe code PIN a été mis à jour avec succès. \nRetour au menu principal...")
  }


  def restockMachine(mach: Machine): Unit = {
    println("\nNiveaux de stock actuels : ")
    printf("   Poudre de café :  " + mach.cafe + "g\n"  )

    printf("   Sucre          :  " + mach.sucre + "g\n" )
    printf("   Lait           :  %.2f L\n" , mach.lait/1000.0)

    //ajout de stock ou quitte le mode admin
    println("Entrez les quantitées à ajouter :")
    print("   Poudre de café : > ")
    var cafSupp = readInt()
    mach.addIngredient("coffee", cafSupp)

    print("   Sucre          : > ")
    var sucreSupp = readInt()
    mach.addIngredient("sugar", sucreSupp)
    print("   Lait           : > ")
    var laitSupp = readDouble()
    var laitSuppInt = (laitSupp*1000).toInt
    mach.addIngredient("milk",laitSuppInt)
    println("Les stocks ont été mis à jour avec succès. \nRetour au menu principal...")}


  def serveClient(mach: Machine): Boolean = {
    var boisson = false
    //construction du code aléatoire pour le paiement par code twint
    var codeTwint = ""
    for (_ <- 1 to 5) {
      val alphanumerique = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      val caractere = (math.random() * 36).toInt
      codeTwint += alphanumerique(caractere)
    }
    // println("Mode sélectionné : Client ")
    var choix = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) : \n> ").toInt
    while ((choix != 1) && (choix != 2) && (choix != 3)) {
      choix = readLine("Oups... Vous vous êtes trompés, veuillez taper : " + "\n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) : \n> ").toInt}

    // sélection = expresso
    if (choix == 1) {
      println("Boisson sélectionnée : Expresso ")
      //nombre de sucre dans le café
      var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      val sucre = (nombredesucre - 1) * 5
      while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
        nombredesucre = readLine("Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

      if (nombredesucre == 1.0) println("Niveau de sucre : Sans sucre (0g) ")
      else if (nombredesucre == 2.0) println("Niveau de sucre : Peu de sucre (5g) ")
      else if (nombredesucre == 3.0) println("Niveau de sucre : Moyen (10g) ")
      else if (nombredesucre == 4.0) println("Niveau de sucre : Beaucoup de sucre (15g) ")
      //préparation du café
      if ((mach.cafe >= 8) && (mach.sucre >= sucre)) {
        //println("Stock : Suffisant")
        boucle = true
        boisson = mach.removeIngredient("coffee", 8) && mach.removeIngredient("sugar", sucre)
       //println(boisson)
        val prixsucrefinal = prixsucre * (nombredesucre - 1)
        prixTotal = prixE + prixsucrefinal
        if (nombredesucre == 1) { //prix sans le sucre
          printf("Prix total : CHF %.2f = CHF %.2f\n", prixE, prixTotal)
        } else { //prix avec le sucre
          printf("Prix total : CHF %.2f +  CHF %.2f = CHF %.2f\n", prixE, prixsucrefinal, prixTotal)}
        println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
        println("(En attente de paiement... )")
        Thread.sleep(3000)
        println("\nPaiement confirmé. ")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre expresso est prêt ! Bonne dégustation ! ")
        }

      // cas de stocks insuffisants
      else if(mach.cafe < 8) {
        println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
        boucle = false}
      else if (mach.sucre < sucre) {
        println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
        boucle = false
      }}

    if (choix == 2) {
      println("Boisson sélectionnée : Cappuccino")
      var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      val sucre = (nombredesucre - 1) * 5
      while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
        nombredesucre = readLine("Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

      if (nombredesucre == 1.0) {
        println("Niveau de sucre : Sans sucre (0g) ")
      } else if (nombredesucre == 2.0) {
        println("Niveau de sucre : Peu de sucre (5g) ")
      } else if (nombredesucre == 3.0) {
        println("Niveau de sucre : Moyen (10g) ")
      } else if (nombredesucre == 4.0) {
        println("Niveau de sucre : Beaucoup de sucre (15g) ")}

      var nombredelait = 0.0
      var lait = (0.1 + 0.05 * nombredelait)*1000
      var laitInt = lait.toInt
      var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
      while ((laitsuppl != 1) && (laitsuppl != 2)) {
        laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}
      if (laitsuppl == 1) {
        nombredelait = readLine("Combien de doses ? \n> ").toInt
        lait = (0.1 + 0.05 * nombredelait)*1000
        laitInt = lait.toInt
        while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
          nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
        if (nombredelait == 1.0) {
          println("Lait en supplément : Oui (50ml) ")
        } else if (nombredelait == 2.0) {
          println("Lait en supplément : Oui (100ml) ")
        } else if (nombredelait == 3.0) {
          println("Lait en supplément : Oui (150ml) ")}

      } else if (laitsuppl == 2) {
        println("Lait supplémentaire : Non ")}

      if ((mach.cafe > 6) && (mach.sucre >= sucre) && (mach.lait >= lait)) {
        //println("Stock : Suffisant")
        boucle = true
        boisson = mach.removeIngredient("coffee", 6) && mach.removeIngredient("sugar", sucre) && mach.removeIngredient("milk", laitInt)
        val prixsucrefinal = prixsucre * (nombredesucre - 1)
        val prixlaitfinal = prixlait * nombredelait
        prixTotal = prixC + prixsucrefinal + prixlaitfinal
        if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
          printf("Prix total : CHF %.2f = CHF %.2f\n", prixC, prixTotal)
        } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixlaitfinal, prixTotal)
        } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixsucrefinal, prixTotal)
        } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixC, prixsucrefinal, prixlaitfinal, prixTotal)}

        println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
        println("(En attente de paiement... )")
        Thread.sleep(3000)
        println("\nPaiement confirmé. ")
        println("Préparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre Cappuccino est prêt ! Bonne dégustation ! ")}

      // cas de stocks insuffisants
      else if (mach.cafe < 6) {
        println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
        boucle = false
      } else if (mach.sucre < sucre) {
        println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
        boucle = false
      } else if (mach.lait < lait) {
        println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
        boucle = false}}

    if (choix == 3) {
      var taille = readLine("\nVeuillez sélectionnez 1 pour Latte (Petit) - 2.7CHF, 2 pour Latte (Moyen), 3.2CHF, 3 pour Latte (Grand), 3.7 \n> ").toInt
      while ((taille != 1) && (taille != 2) && (taille != 3)) {
        taille = readLine("\nOups... vous vous êtes trompés, sélectionnez 1 pour Latte (Petit) - 2.7CHF, 2 pour Latte (Moyen), 3.2CHF, 3 pour Latte (Grand), 3.7 \n> ").toInt}

      if (taille == 1) {
        println("Boisson sélectionnée : Latte (Petit) ")
        var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
        val sucre = (nombredesucre - 1) * 5
        while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
          nombredesucre = readLine("Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

        if (nombredesucre == 1.0) {
          println("Niveau de sucre : Sans sucre (0g) ")
        } else if (nombredesucre == 2.0) {
          println("Niveau de sucre : Peu de sucre (5g) ")
        } else if (nombredesucre == 3.0) {
          println("Niveau de sucre : Moyen (10g) ")
        } else if (nombredesucre == 4.0) {
          println("Niveau de sucre : Beaucoup de sucre (15g) \"")}

        var nombredelait = 0.0
        var lait = (0.12 + 0.05 * nombredelait)*1000
        var laitInt = lait.toInt

        var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
        while ((laitsuppl != 1) && (laitsuppl != 2)) {
          laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

        if (laitsuppl == 1) {
          nombredelait = readLine("Combien de doses ? \n> ").toInt
          lait = (0.12 + 0.05 * nombredelait)*1000
          while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
            nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
          if (nombredelait == 1.0) {
            println("Lait en supplément : Oui (50ml) ")
          } else if (nombredelait == 2.0) {
            println("Lait en supplément : Oui (100ml) ")
          } else if (nombredelait == 3.0) {
            println("Lait en supplément : Oui (150ml) ")}

        } else if (laitsuppl == 2) {
          println("Lait supplémentaire : Non ")}

        if ((mach.cafe > 6) && (mach.sucre >= sucre) && (mach.lait >= lait)) {
          //println("Stock : Suffisant")
          boucle = true
          boisson = mach.removeIngredient("coffee", 6) && mach.removeIngredient("sugar", sucre) && mach.removeIngredient("milk", laitInt)
          val prixsucrefinal = prixsucre * (nombredesucre - 1)
          val prixlaitfinal = prixlait * nombredelait
          prixTotal = prixLP + prixsucrefinal + prixlaitfinal
          if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
            printf("Prix total : CHF %.2f = CHF %.2f\n", prixLP, prixTotal)
          } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixlaitfinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixsucrefinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLP, prixsucrefinal, prixlaitfinal, prixTotal)}
          println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
          println("(En attente de paiement... )")
          Thread.sleep(3000)
          println("\nPaiement confirmé. ")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre Latte est prêt ! Bonne dégustation ! ")}

        // cas de stocks insuffisants
        else if (mach.cafe < 6) {
          println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
          boucle = false

        } else if (mach.sucre < sucre) {
          println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
          boucle = false

        } else if (mach.lait < lait) {
          println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
          boucle = false
        }}

      if (taille == 2) {
        println("Boisson sélectionnée : Latte (Moyen) ")
        var nombredesucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
        val sucre = (nombredesucre - 1) * 5
        while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
          nombredesucre = readLine("Oups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

        if (nombredesucre == 1.0) {
          println("Niveau de sucre : Sans sucre (0g) ")
        } else if (nombredesucre == 2.0) {
          println("Niveau de sucre : Peu de sucre (5g) ")
        } else if (nombredesucre == 3.0) {
          println("Niveau de sucre : Moyen (10g) ")
        } else if (nombredesucre == 4.0) {
          println("Niveau de sucre : Beaucoup de sucre (15g) ")}
        var nombredelait = 0.0
        var lait = (0.15 + 0.05 * nombredelait)*1000
        var laitInt = lait.toInt
        var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
        while ((laitsuppl != 1) && (laitsuppl != 2)) {
          laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

        if (laitsuppl == 1) {
          nombredelait = readLine("Combien de doses ? \n> ").toInt
          lait = (0.15 + 0.05 * nombredelait)*1000
          laitInt = laitInt
          while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
            nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
          if (nombredelait == 1.0) {
            println("Lait en supplément : Oui (50ml) ")
          } else if (nombredelait == 2.0) {
            println("Lait en supplément : Oui (100ml) ")
          } else if (nombredelait == 3.0) {
            println("Lait en supplément : Oui (150ml) ")}

        } else if (laitsuppl == 2) {
          println("\nLait supplémentaire : Non ")}

        if ((mach.cafe > 8) && (mach.sucre >= sucre) && (mach.lait >= lait)) {
          //println("Stock : Suffisant")
          boucle = true

          boisson = mach.removeIngredient("coffee", 8)&&mach.removeIngredient("sugar", sucre) && mach.removeIngredient("milk", laitInt)
          val prixsucrefinal = prixsucre * (nombredesucre - 1)
          val prixlaitfinal = prixlait * nombredelait
          prixTotal = prixLM + prixsucrefinal + prixlaitfinal
          if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
            printf("Prix total : CHF %.2f = CHF %.2f\n", prixLM, prixTotal)
          } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixlaitfinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixsucrefinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLM, prixsucrefinal, prixlaitfinal, prixTotal)}
          println("\nVeuillet utilisant Twint. \nVotre code de paiement est : " + codeTwint)
          println("(En attente de paiement... )")
          Thread.sleep(3000)
          println("\nPaiement confirmé. ")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre Latte est prêt ! Bonne dégustation ! ")}

        // cas de stocks insuffisants
        else if (mach.cafe < 8) {
          println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
          boucle = false

        } else if (mach.sucre < sucre) {
          println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
          boucle = false

        } else if (mach.lait < lait) {
          println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
          boucle = false
        }}

      if (taille == 3) {
        println("Boisson sélectionnée : Latte (Grand) ")
        var nombredesucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
        val sucre = (nombredesucre - 1) * 5
        while ((nombredesucre != 1) && (nombredesucre != 2) && (nombredesucre != 3) && (nombredesucre != 4)) {
          nombredesucre = readLine("\nOups... Vous vous êtes trompés, veuillez taper : \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt}

        if (nombredesucre == 1.0) {
          println("Niveau de sucre : Sans sucre (0g) ")
        } else if (nombredesucre == 2.0) {
          println("Niveau de sucre : Peu de sucre (5g) ")
        } else if (nombredesucre == 3.0) {
          println("Niveau de sucre : Moyen (10g) ")
        } else if (nombredesucre == 4.0) {
          println("Niveau de sucre : Beaucoup de sucre (15g) \n")}
        var nombredelait = 0.0
        var lait = (0.2 + 0.05 * nombredelait)*1000
        var laitInt = lait.toInt
        var laitsuppl = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt
        while ((laitsuppl != 1) && (laitsuppl != 2)) {
          laitsuppl = readLine("Oups... vous vous êtes trompés, souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non \n> ").toInt}

        if (laitsuppl == 1) {
          nombredelait = readLine("Combien de doses ? \n> ").toInt
          lait = (0.2 + 0.05 * nombredelait)*1000
          laitInt = lait.toInt
          while ((nombredelait != 1) && (nombredelait != 2) && (nombredelait != 3)) {
            nombredelait = readLine("Oups... Vous vous êtes trompés, veuillez choisir entre 1 et 3 doses au maximum : ").toInt}
          if (nombredelait == 1.0) {
            println("Lait en supplément : Oui (50ml) ")
          } else if (nombredelait == 2.0) {
            println("Lait en supplément : Oui (100ml) ")
          } else if (nombredelait == 3.0) {
            println("Lait en supplément : Oui (150ml) ")}

        } else if (laitsuppl == 2) {
          println("Lait supplémentaire : Non ")}

        if ((mach.cafe > 12) && (mach.sucre >= sucre) && (mach.lait >= lait)) {
          //println("Stock : Suffisant")
          boucle = true

          boisson = mach.removeIngredient("coffee", 12)&&mach.removeIngredient("sugar", sucre) && mach.removeIngredient("milk", laitInt)
          val prixsucrefinal = prixsucre * (nombredesucre - 1)
          val prixlaitfinal = prixlait * nombredelait
          prixTotal = prixLG + prixsucrefinal + prixlaitfinal
          if ((nombredesucre == 1) && (laitsuppl == 2)) { //prix sans le sucre ni lait suppl
            printf("Prix total : CHF %.2f = CHF %.2f\n", prixLG, prixTotal)
          } else if ((nombredesucre == 1) && (laitsuppl == 1)) { //prix sans le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixlaitfinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 2)) { //prix avec le sucre, sans lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixsucrefinal, prixTotal)
          } else if ((nombredesucre != 1) && (laitsuppl == 1)) { //prix avec le sucre, avec lait suppl
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixLG, prixsucrefinal, prixlaitfinal, prixTotal)}
          println("\nVeuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
          println("(En attente de paiement... )")
          Thread.sleep(3000)
          println("\nPaiement confirmé.")
          println("Préparation de votre boisson...")
          Thread.sleep(5000)
          println("Votre Latte est prêt ! Bonne dégustation ! ")}

        // cas de stocks insuffisants
        else if (mach.cafe < 12) {
          println("\nERREUR : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée. \nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin. \n")
          boucle = false

        } else if (mach.sucre < sucre) {
          println("\nERREUR : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \nVeuillez prendre moins de sucre ou choisissez une autre boisson. \n")
          boucle = false

        } else if (mach.lait < lait) {
          println("\nERREUR : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \nVeuillez choisir une taille plus petite ou essayer une autre boisson. \n")
          boucle = false
        }}}
    return boisson
  }



  def main(args: Array[String]): Unit = {

    var f = "machines.csv"

    println("Chargement des machines depuis machines.csv...\n")
    var machines = loadcsv(f)
    if(machines != null) {
      for (i <- 0 to machines.size - 1) {
        //printf("Machine ")
        println("\nMachine " + {i + 1} + " chargée : ")
        println("ID : " + machines(i).idMachine)
        println("PIN : " + machines(i).codepin)
        println("Café : " + machines(i).cafe)
        println("Sucre : " + machines(i).sucre)
        println("Lait : " + machines(i).lait)
      }
      println("\n"+ machines.size + " machine(s) chargée(s) avec succès.\n")


      while (boucle == true) {
        tentatives = 3

        //construction du code aléatoire pour le paiement par code twint
        var codeTwint = ""
        for (_ <- 1 to 5) {
          val alphanumerique = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          val caractere = (math.random() * 36).toInt
          codeTwint += alphanumerique(caractere)
        }

        // Interface de la machine
        println("\nNospresso Café")
        println("Bonjour, veuillez sélectionner votre mode : ")
        //menu principal
        menu = readLine("\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
        while ((menu != 1) && (menu != 2) && (menu != 3)) {
          print("Oups... Vous vous êtes trompés, veuillez sélectionner votre mode : \n>")
          menu = readLine("1) Client \n2) Admin \n3) Quitter \n> ").toInt
        }

        var boissonservie = false
        if (menu == 1) {
          do {
            var machineId = readLine("Machine sélectionnée (1-"+machines.size+") : > ").toInt
            while ((machineId > machines.size) || (machineId < 1)) {
              println("Erreur... Machine sélectionnée (1-"+machines.size +") : \n> ")
              machineId = readInt()
            }
            boissonservie = serveClient(machines(machineId-1))
          }while(boissonservie == false)
        savecsv("machines.csv", machines)
        }

        else if (menu == 2){
          var machineId = readLine("Machine sélectionnée (1-"+machines.size+") : \n> ").toInt
          while(machineId > machines.size || machineId<1){
            println("Erreur... Machine sélectionnée (1-"+machines.size+") : \n> ")
            machineId = readInt()}

          if(validatePin(machines(machineId-1))){
            var choixAdmin = readLine("Veuillez taper 1 pour changer votre code PIN ou 2 pour accéder aux stocks : " ).toInt
            while((choixAdmin != 1) && (choixAdmin != 2)){
              choixAdmin = readLine("Veuillez taper 1 pour changer votre code PIN ou 2 pour accéder aux stocks : " ).toInt}
            if(choixAdmin == 1){
              updatePin(machines(machineId-1))}
            else restockMachine(machines(machineId-1))
          } else {
            boucle = false
            menu = 3
          }
          savecsv("machines.csv", machines)
        }

        else if(menu == 3){
          savecsv("machines.csv", machines)
          println("\nSauvegarde des machines dans machines.csv...\n")
          if(sauv){
          println("Fichier sauvegardé avec succès.")
          println("Le programme a été quitté.")
    }else{
      println("Erreur : Echec du chargement ou de la sauvegarde des machines. \nFermeture du programme.")
          }
        boucle = false
        }
      sauv = false
      }
  }else{
  println("Erreur : Echec du chargement ou de la sauvegarde des machines. \nFermeture du programme. ")
    }
  }
}
