import scala.io.StdIn._
import scala.util.Random


object Main {

  def validatePin(machineId:Int, machinePins : Array[String]):Boolean = {
    println(" Entrez le code :")
    val cPin = readLine(">")
    if(cPin != machinePins(machineId-1)) { //-1 pour atteindre le bon index
      false
    }
    else {
      true
    }
  }

  def updatePin(machineId:Int, machinePins:Array[String]):Unit = {
    var nouveau_cPin = ""
    println("Entrez un nouveau code Pin à 6 chiffres")
    while(nouveau_cPin.length > 6 || nouveau_cPin.length<6){
      nouveau_cPin = readLine("")
      if(nouveau_cPin.length > 6 || nouveau_cPin.length<6){
        println("Le nouveau code doit être de 6 chiffres !!")
      }
    }
    machinePins(machineId-1)= nouveau_cPin
    println("Le code Pin a été mis à jour avec succès" + "\nRetour au menu principal...")
  }

  def serveClient(machineId:Int,pdc:Array[Int],stocksucre:Array[Int],stocklait:Array[Int]):Boolean = {
    var boissons = 0
    var nboisson = "" //nom boisson
    var sucre = 0
    var qts = 0 //quantité de sucre à utiliser
    var qtc = 0 //quantité de café
    var qtl = 0 //quantité de lait
    var choixddl = 0
    var ddl = 0
    var tdl = 0 //taille du latte
    var atdl = "" //affichage du latte
    var addl = "" //affichage dose de lait
    var prixddl = "" //prix des doses de lait pour afficher
    var prixddl2 = "" //éviter les arrondis
    var prixsucre = ""
    var ptt = 0.0 //prix total,  *1000 pour éviter les erreurs d'arrondis
    val twint = Random.alphanumeric.take(5).mkString
    println("Veuillez sélectionner votre boisson : ")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    while (boissons != 1 && boissons != 2 && boissons != 3) {
      boissons = readLine(">").toInt
      if (boissons != 1 && boissons != 2 && boissons != 3) {
        println("Veuillez saisir une boisson autorisée : 1 ; 2 ou 3")
      }
    }
    if (boissons == 1) { //espresso
      qtc = 8
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        sucre = readLine(">").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez saisir une quantité autorisée : 1 ; 2 ou 3")
        }
      }
      println("Boisson sélectionnée : Expresso")
      if (sucre == 1) {
        qts = 0
        ptt = 2000.0
        println("Niveau de sucre : Sans sucre")

      }
      if (sucre == 2) {
        qts = 5
        prixsucre = " + CHF 0.10"
        ptt = 2100.0
        println("Niveau de sucre : Peu (5g)")
      }
      if (sucre == 3) {
        qts = 10
        ptt = 2200.0
        prixsucre = " + CHF 0.20"
        println("Niveau de sucre : Moyen (10g)")

      }
      if (sucre == 4) {
        qts = 15
        ptt = 2300.0
        prixsucre = " + CHF 0.30"
        println("Niveau de sucre : Beaucoup (15g)")
      }

      if (pdc(machineId-1) >= qtc) {
        if (stocksucre(machineId-1) >= qts) {
          pdc(machineId-1) -= qtc
          stocksucre(machineId-1) -= qts
          if (sucre == 1) {
            println("Prix total : CHF 2.00")
          }
          else {
            println("Prix total : CHF 2.00" + prixsucre + " = " + "CHF " + ptt/1000 + "0") //+0 pour afficher 2.50 par ex.
          }
          Thread.sleep(1500)
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + twint)
          println("(En attente de paiement...)" + "\n")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté." + "\n")
          Thread.sleep(1500)
          println("Préparation de votre boisson...")
          println("[...]")
          Thread.sleep(2000)
          println("Votre Expresso est prêt ! Bonne dégustation ! ")

          Thread.sleep(2000)
          true
        }
        else {
          println("Erreur : Quantité de sucre insuffisante pour préparer")
          println("la boisson sélectionnée.")
          Thread.sleep(1500)
          false
        }
      }
      else {
        println("Erreur : Quantité de café insuffisante pour préparer")
        println("la boisson sélectionnée.")
        Thread.sleep(1500)
        false
      }
    }
    else if (boissons == 2) { //cappuccino
      qtc = 6
      qtl = 100
      //dose de sucre
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        sucre = readLine(">").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez saisir une quantité autorisée : 1 ; 2 ou 3")
        }
      }
      //dose de lait
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      while (choixddl != 1 && choixddl != 2) {
        choixddl = readLine(">").toInt
        if (choixddl != 1 && choixddl != 2) {
          println("Veuillez choisir entre 1 = Oui et 2 = Non")
        }
      }
      if (choixddl == 1) {
        println("Combien de dose ?")
        while (ddl != 1 && ddl != 2 && ddl != 3) {
          ddl = readLine(">").toInt
          if (ddl != 1 && ddl != 2 && ddl != 3) {
            println("Veuillez choisir une dose autorisée : 1 ; 2 ou 3")
          }
        }
      }
      println("Boisson sélectionnée : Cappuccino")
      if (sucre == 1) {
        qts = 0
        ptt = 2500.0
        println("Niveau de sucre : Sans sucre")

      }
      else if (sucre == 2) {
        qts = 5
        prixsucre = " + CHF 0.10"
        ptt = 2600.0
        println("Niveau de sucre : Peu (5g)")
      }
      else if (sucre == 3) {
        qts = 10
        ptt = 2700.0
        prixsucre = " + CHF 0.20"
        println("Niveau de sucre : Moyen (10g)")

      }
      else {
        qts = 15
        ptt = 2800.0
        prixsucre = " + CHF 0.30"
        println("Niveau de sucre : Beaucoup (15g)")
      }
      if (choixddl == 1) {
        qtl += (ddl * 50)
        addl = (ddl * 50).toString + " mL" //pour mettre en mL
        prixddl = " + CHF " + (ddl * 0.05).toString
        for (i <- prixddl) { //pour chaque char de prixddl, on rajoute i a prixddl2 tant que la longueur de prixddl2 <11, càd que l'on va pas afficher plus loin que 2 décimales
          if (prixddl2.length < 11) {
            prixddl2 += i
          }
        }

        ptt += (ddl * 0.05)*1000
        println("Lait en supplément : " + addl)
      }
      else {
        println("Lait en supplément : Non")
      }

      if (pdc(machineId-1) >= qtc) {
        if (stocksucre(machineId-1) >= qts){
          if(stocklait(machineId-1)>=qtl){
            if (sucre == 1) {
              if (choixddl == 1) {
                if (ddl == 2) { //comme la valeur si le sucre ==1 sera toujours une dizaine, on ajoute 0 pour bien afficher 2.50 et non 2.5
                  println("Prix total : CHF 2.50 " + prixddl + "0" + " = " + "CHF " + ptt/1000 + "0")
                }
                else {
                  println("Prix total : CHF 2.50 " + prixddl2  + " = " + "CHF " + ptt/1000)
                }
              }
              else {
                println("Prix total : CHF 2.50 ")
              }
            }
            else {
              if (choixddl == 1) {
                if (ddl == 2) {
                  println("Prix total : CHF 2.50" + prixsucre + prixddl+"0" + " = " + "CHF " + ptt/1000 + "0")
                }
                else {
                  println("Prix total : CHF 2.50" + prixsucre + prixddl2 + " = " + "CHF " + ptt/1000)
                }
              }
              else {
                println("Prix total : CHF 2.50" + prixsucre + " = " + "CHF " + ptt/1000 + "0")
              }
            }
            pdc(machineId-1) -= qtc
            stocksucre(machineId-1) -= qts
            stocklait(machineId-1) -= qtl
            Thread.sleep(1500)
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + twint)
            println("(En attente de paiement...)" + "\n")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté." + "\n")
            Thread.sleep(1500)
            println("Préparation de votre boisson...")
            println("[...]")
            Thread.sleep(2000)
            println("Votre Cappuccino est prêt ! Bonne dégustation ! ")

            Thread.sleep(2000)
            true
          }
          else{
            println("Erreur : Quantité de lait insuffisante pour préparer")
            println("la boisson sélectionnée")
            Thread.sleep(1500)
            false
          }
        }
        else {
          println("Erreur : Quantité de sucre insuffisante pour préparer")
          println("la boisson sélectionnée.")
          Thread.sleep(1500)
          false
        }
      }
      else {
        println("Erreur : Quantité de café insuffisante pour préparer")
        println("la boisson sélectionnée.")
        Thread.sleep(1500)
        false
      }
    }

    else { //latte
      while (tdl != 1 && tdl != 2 && tdl != 3) {
        println("Veuillez sélectionner la taille du Latte : ")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        tdl = readLine(">").toInt
        if (tdl != 1 && tdl != 2 && tdl != 3) {
          println("Veuillez saisir une taille autorisée : 1 (Petit) ; 2 (Moyen) ; 3 (Grand)")
          Thread.sleep(1000)
        }
      }
      //dose de sucre
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        sucre = readLine(">").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez saisir une quantité autorisée : 1 ; 2 ou 3")
        }
      }
      //dose de lait
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("1) Oui")
      println("2) Non")
      while (choixddl != 1 && choixddl != 2) {
        choixddl = readLine(">").toInt
        if (choixddl != 1 && choixddl != 2) {
          println("Veuillez choisir entre 1 = Oui et 2 = Non")
        }
      }
      if (choixddl == 1) {
        println("Combien de dose ?")
        while (ddl != 1 && ddl != 2 && ddl != 3) {
          ddl = readLine(">").toInt
          if (ddl != 1 && ddl != 2 && ddl != 3) {
            println("Veuillez choisir une dose autorisée : 1 ; 2 ou 3")
          }
        }
      }

      if (tdl == 1) {
        qtc = 6
        qtl = 120
        ptt = 2700.0
        atdl = "CHF 2.70"
        nboisson = " Latte (Petit) "
        println("Boisson sélectionnée : Latte (Petit)")
      }
      else if (tdl == 2) {
        qtc = 8
        qtl = 150
        ptt = 3200.0
        atdl = "CHF 3.20"
        nboisson = " Latte (Moyen) "
        println("Boisson sélectionnée : Latte (Moyen)")
      }
      else {
        qtc = 12
        qtl = 200
        ptt = 3700.0
        atdl = "CHF 3.70"
        nboisson = " Latte (Grand) "
        println("Boisson sélectionnée : Latte (Grand)")
      }
      if (sucre == 1) {
        qts = 0
        println("Niveau de sucre : Sans sucre")

      }
      else if (sucre == 2) {
        qts = 5
        prixsucre = " + CHF 0.10"
        ptt += 100
        println("Niveau de sucre : Peu (5g)")
      }
      else if (sucre == 3) {
        qts = 10
        ptt += 200
        prixsucre = " + CHF 0.20"
        println("Niveau de sucre : Moyen (10g)")

      }
      else {
        qts = 15
        ptt += 300
        prixsucre = " + CHF 0.30"
        println("Niveau de sucre : Beaucoup (15g)")
      }

      if (choixddl == 1) {
        qtl += ddl * 50
        addl = (ddl * 50).toString + " mL" //pour mettre en mL
        prixddl = " + CHF " + (ddl * 0.05).toString
        for (i <- prixddl) { //pour chaque char de prixddl, on ajoute i à prixddl2 tant que la longueur de prixddl2 <11, càd que l'on va pas afficher plus loin que 2 décimales
          if (prixddl2.length < 11) {
            prixddl2 += i
          }
        }

        ptt += (ddl * 0.05)*1000
        println("Lait en supplément : " + addl)
      }
      else {
        println("Lait en supplément : Non")
      }

      if (pdc(machineId-1) >= qtc) {
        if (stocksucre(machineId-1) >= qts) {
          if (stocklait(machineId-1) >= qtl) {
            if (sucre == 1) {
              if (choixddl == 1) {
                if (ddl == 2) { //comme la valeur si le sucre ==1 sera toujours une dizaine, on ajoute 0 pour bien afficher 2.50 et non 2.5
                  println("Prix total : " + atdl + prixddl + "0" + " = " + "CHF " + ptt/1000 + "0")
                }
                else {
                  println("Prix total : " + atdl + prixddl2  + " = " + "CHF " + ptt/1000)
                }
              }
              else {
                println("Prix total : " + atdl)
              }
            }
            else {
              if (choixddl == 1) {
                if (ddl == 2) {
                  println("Prix total : " + atdl + prixsucre + prixddl+ "0" + " = " + "CHF " + ptt/1000 + "0")
                }
                else {
                  println("Prix total : " + atdl + prixsucre + prixddl2 + " = " + "CHF " + ptt/1000)
                }
              }
              else {
                println("Prix total : " + atdl + prixsucre + " = " + "CHF " + ptt/1000 + "0")
              }
            }
            pdc(machineId-1) -= qtc
            stocksucre(machineId-1) -= qts
            stocklait(machineId-1) -= qtl
            Thread.sleep(1500)
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + twint)
            println("(En attente de paiement...)" + "\n")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté." + "\n")
            Thread.sleep(1500)
            println("Préparation de votre boisson...")
            println("[...]")
            Thread.sleep(2000)
            println("Votre" + nboisson + " est prêt ! Bonne dégustation ! ")
            Thread.sleep(2000)
            true

          }
          else {
            println("Erreur : Quantité de lait insuffisante pour préparer")
            println("la boisson sélectionnée.")
            Thread.sleep(1500)
            false
          }
        }
        else {
          println("Erreur : Quantité de sucre insuffisante pour préparer")
          println("la boisson sélectionnée.")
          Thread.sleep(1500)
          false
        }
      }

      else {
        println("Erreur : Quantité de café insuffisante pour préparer")
        println("la boisson sélectionnée.")
        Thread.sleep(1500)
        false
      }
    }
  }

  def restockMachine(machineId:Int,pdc:Array[Int],stocksucre:Array[Int],stocklait:Array[Int]): Unit = {
    var ajtstock = 0//choisir d'ajouter ou non
    val stocklait2 = stocklait(machineId-1).toDouble //conversion en double pour l'affichage
    print("Poudre de café : " + pdc(machineId-1) + "g")
    print("\nSucre : " + stocksucre(machineId-1) + "g")
    printf("\nLait : %.2f",  stocklait2/1000) //conversion en Litres
    print(" L")
    Thread.sleep(1500)
    while(ajtstock!=1 && ajtstock!=2){
      println("\nVoulez-vous ajouter du stock ?")
      println("1) Oui")
      println("2) Non")
      ajtstock = readLine(">").toInt
      if(ajtstock!=1 && ajtstock!=2){
        println("Veuillez saisir 1 si Oui et 2 si Non")
      }
    }
    if(ajtstock==1){
      var ajtcafe= -5
      var ajtsucre = -5
      var ajtlait = -5
      do{
        ajtcafe = readLine("Quelle quantité de café voulez-vous ajouter ? ").toInt
        if(ajtcafe<0){
          println("Veuillez saisir une quantité positive")
        }
      }while(ajtcafe<0)

      do{
        ajtsucre = readLine("Quelle quantité de sucre voulez-vous ajouter ? ").toInt
        if(ajtsucre<0){
          println("Veuillez saisir une quantité positive")
        }
      }while(ajtsucre<0)

      do{
        ajtlait = readLine("Quelle quantité de lait voulez-vous ajouter ? ").toInt
        if(ajtlait<0){
          println("Veuillez saisir une quantité positive")
        }
      }while(ajtlait<0)
      val ajtlait2 = ajtlait.toDouble //afficher l'ajout
      pdc(machineId-1) += ajtcafe
      stocklait(machineId-1)+= ajtlait
      stocksucre(machineId-1)+=ajtsucre

      print("\nVous avez ajouté : " )
      print("\nPoudre de café : " + ajtcafe + "g")
      print("\nSucre : " + ajtsucre + "g")
      printf("\nLait : %.3f",  ajtlait2/1000)
      print("L")
      println("\nLes stocks ont été mis à jour avec succès")
      println("Retour au menu...")
      Thread.sleep(1500)
    }
    else{
      println("Retour au menu...")
    }
  }

  def main(args: Array[String]): Unit = {
    //variables
    var mode = 0
    var machineId = 0
    val NbMachines = 5

    //stocks

    val stocksucre = Array.fill(NbMachines)(30)
    val pdc = Array.fill(NbMachines)(50) //poudre de café, stock
    val stocklait = Array.fill(NbMachines)(500)//mL
    val machinePins = Array.fill(NbMachines)("434343")
    while (mode != 3) {
      println("   Nospresso Café")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      mode=0

      while (mode != 1 && mode != 2 && mode != 3) {
        mode = readLine(">").toInt
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez saisir un mode autorisé : 1 ; 2 ou 3")
        }
      }
      // si mode Client

      if(mode==1){
        while(machineId-1<0 || machineId-1>NbMachines-1) {
          println("Veuillez indiquer le numéro de la machine (1 à 5)")
          machineId = readLine(">").toInt
          if(machineId-1<0 || machineId-1>NbMachines-1){
            println("Veuillez saisir un identifiant entre 1 et 5")
          }
        }
        println("Machine sélectionnée > " + machineId)
        val Client = serveClient(machineId, pdc, stocksucre, stocklait)
        if(!Client){
          println("Essayez une autre machine.")
          Thread.sleep(2000)
        }
      }

      else if (mode == 2) {
        var test = 0
        while(machineId-1<0 || machineId-1>NbMachines-1) {
          println("Veuillez indiquer le numéro de la machine (1 à 5)")
          machineId = readLine(">").toInt
          if(machineId-1<0 || machineId-1>NbMachines-1){
            println("Veuillez saisir un identifiant entre 1 et 5")
          }
        }
        println("Machine sélectionnée > " + machineId)
        while(test<3){
          val validationPin = validatePin(machineId,machinePins)
          if(!validationPin){
            test+=1
            print("Code Pin incorrect : " + (3-test) + " tentatives restantes.")
            if(test==3){
              println("\nTrop de tentatives échouées." + "\nFin du programme")
              mode=3
              Thread.sleep(1500)
            }
          }
          else{
            println("Accès accordé à la machine " + machineId)
            var choix = 0 //soit ajouter du stock ou changer le code Pin
            println("Voulez-vous changer de code Pin ou bien ajouter du stock ?")
            println("1) Code " + "\n2) Stocks ")
            while(choix!=1 && choix!=2){
              choix = readLine(">").toInt
              if(choix!=1 && choix!=2){
                println("Choisissez 1 ou 2")
              }
            }
            if(choix==1){
              updatePin(machineId,machinePins)
              Thread.sleep(1500)
              test=3
            }
            else{
              restockMachine(machineId,pdc,stocksucre, stocklait)
              Thread.sleep(1500)
              test=3
            }
          }
        }


      }
      machineId = 0
    }

    println("Merci, et à bientôt !")
  }
}