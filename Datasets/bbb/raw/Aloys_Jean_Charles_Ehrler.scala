import scala.io.StdIn._
import scala.util.Random
object Main {
  def validatePin(machineId:Int, machinePins: Array[String]):Boolean = {
    println("Entrez le code Pin : ")
    val PIN = readLine(">")
    if(PIN== machinePins(machineId-1)){
      true
    }
    else{
      false
    }
  }
  def updatePin(machineId:Int,machinePins: Array[String]): Unit = {
    var nouveau_PIN = ""
    println("Entrez un nouveau code PIN à 6 chiffres")
    while(nouveau_PIN.length != 6){
      nouveau_PIN = readLine(">")
      if(nouveau_PIN.length!=6){
        println("Veuillez donner un code à exactement 6 chiffres")
      }
      if(nouveau_PIN.length == 6){
        machinePins(machineId-1) = nouveau_PIN
        println("Le code PIN a été mis à jour avec succès")
        println("Retour au menu princpal...")
      }
    }
  }

  def serveClient(machineId: Int, Spoudre:Array[Int], Ssucre:Array[Int],Slait:Array[Int]):Boolean = {
    val pass = Random.alphanumeric.take(5).mkString
    var cboisson = "" //choix de la boisson, pour le nom
    var boisson = 0
    var sucre = 0
    var prix: Double = 0
    var prixS: Double = 0
    var prixL: Double = 0
    var prixT: Double = 0
    var poudre = 0
    var lait = 0
    var dose = 0
    var taille = 0
    var psucre = 0
    var llait = 0


    while (boisson != 1 && boisson != 2 && boisson != 3) {
      boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
    }

    if (boisson == 1) {
      cboisson = "Expresso"
      println("Vous avez choisi un expresso")
      while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4) {
        psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
      }
      if (psucre == 1) {
        prix = 2.0
        prixS = 0.0
        prixL = 0.0
        poudre += 8
      }
      if (psucre == 2) {
        prix = 2.0
        poudre += 8
        sucre += 5
        prixS = 0.10
        prixL = 0.0
      }
      if (psucre == 3) {
        prix = 2.0
        prixS = 0.20
        prixL = 0.0
        poudre += 8
        sucre += 10
      }
      if (psucre == 4) {
        prix = 2.0
        prixS = 0.30
        prixL = 0.0
        poudre += 8
        sucre += 15
      }
    }


    else if (boisson == 2) {
      cboisson = "Cappuccino"
      println("Vous avez choisi un capuccino")
      while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4) {
        psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
      }
      while (llait != 1 && llait != 2) {
        llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
      }
      if (llait == 2 && psucre == 1) {
        prix = 2.50
        prixS = 0.0
        prixL = 0.0
        poudre += 6
        lait += 100
      }
      if (llait == 2 && psucre == 2) {
        prix = 2.50
        prixS = 0.10
        prixL = 0.0
        poudre += 6
        lait += 100
        sucre += 5
      }
      if (llait == 2 && psucre == 3) {
        prix = 2.50
        prixS = 0.20
        prixL = 0.0
        poudre += 6
        lait += 100
        sucre += 10
      }
      if (llait == 2 && psucre == 4) {
        prix = 2.50
        prixS = 0.30
        prixL = 0.0
        poudre += 6
        lait += 100
        sucre += 15
      }
      if (llait == 1) {
        while (dose != 1 && dose != 2 && dose != 3) {
          dose = readLine("Combien de dose ?\n>").toInt
        }
        if (dose == 1 && psucre == 1) {
          prix = 2.50
          prixS = 0.0
          prixL = 0.05
          poudre += 6
          lait += 150
        }
        if (dose == 1 && psucre == 2) {
          prix = 2.50
          prixS = 0.10
          prixL = 0.05
          poudre += 6
          lait += 150
          sucre += 5
        }
        if (dose == 1 && psucre == 3) {
          prix = 2.50
          prixS = 0.20
          prixL = 0.05
          poudre += 6
          lait += 150
          sucre += 10
        }
        if (dose == 1 && psucre == 4) {
          prix = 2.50
          prixS = 0.30
          prixL = 0.05
          poudre += 6
          lait += 150
          sucre += 15
        }
        if (dose == 2 && psucre == 1) {
          prix = 2.50
          prixS = 0.0
          prixL = 0.10
          poudre += 6
          lait += 200
        }
        if (dose == 2 && psucre == 2) {
          prix = 2.50
          prixS = 0.10
          prixL = 0.10
          poudre += 6
          lait += 200
          sucre += 5
        }
        if (dose == 2 && psucre == 3) {
          prix = 2.50
          prixS = 0.20
          prixL = 0.10
          poudre += 6
          lait += 200
          sucre += 10
        }
        if (dose == 2 && psucre == 4) {
          prix = 2.50
          prixS = 0.30
          prixL = 0.10
          poudre += 6
          lait += 200
          sucre += 15
        }
        if (dose == 3 && psucre == 1) {
          prix = 2.50
          prixS = 0.0
          prixL = 0.15
          poudre += 6
          lait += 250
        }
        if (dose == 3 && psucre == 2) {
          prix = 2.50
          prixS = 0.10
          prixL = 0.15
          poudre += 6
          lait += 250
          sucre += 5
        }
        if (dose == 3 && psucre == 3) {
          prix = 2.50
          prixS = 0.20
          prixL = 0.15
          poudre += 6
          lait += 250
          sucre += 10
        }
        if (dose == 3 && psucre == 4) {
          prix = 2.50
          prixS = 0.30
          prixL = 0.15
          poudre += 6
          lait += 250
          sucre += 15
        }
      }
    }

    else {
      println("Vous avez choisi un latte")
      cboisson = "Latte"
      while (taille != 1 && taille != 2 && taille != 3) {
        taille = readLine("Quelle taille souhaitez-vous ?\n1) Petit \n2) Moyen \n3) Grand \n>").toInt
      }
      while(psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4){
        psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
      }
      while(llait != 1 && llait != 2){
        llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
      }

      if (llait == 2 && psucre == 1 && taille == 1) {
        prix = 2.70
        prixS = 0.0
        prixL = 0.0
        poudre += 6
        lait += 120
      }
      if (llait == 2 && psucre == 2 && taille == 1) {
        prix = 2.70
        prixS = 0.10
        prixL = 0.0
        poudre += 6
        lait += 120
        sucre += 5
      }
      if (llait == 2 && psucre == 3 && taille == 1) {
        prix = 2.70
        prixS = 0.20
        prixL = 0.0
        poudre += 6
        lait += 120
        sucre += 10
      }
      if (llait == 2 && psucre == 4 && taille == 1) {
        prix = 2.70
        prixS = 0.30
        prixL = 0.0
        poudre += 6
        lait += 120
        sucre += 15
      }
      if (llait == 2 && psucre == 1 && taille == 2) {
        prix = 3.20
        prixS = 0.0
        prixL = 0.0
        poudre += 8
        lait += 150
      }
      if (llait == 2 && psucre == 2 && taille == 2) {
        prix = 3.20
        prixS = 0.10
        prixL = 0.0
        poudre += 8
        lait += 150
        sucre += 5
      }
      if (llait == 2 && psucre == 3 && taille == 2) {
        prix = 3.20
        prixS = 0.20
        prixL = 0.0
        poudre += 8
        lait += 150
        sucre += 10
      }
      if (llait == 2 && psucre == 4 && taille == 2) {
        prix = 3.20
        prixS = 0.30
        prixL = 0.0
        poudre += 8
        lait += 150
        sucre += 15
      }
      if (llait == 2 && psucre == 1 && taille == 3) {
        prix = 3.70
        prixS = 0.0
        prixL = 0.0
        poudre += 12
        lait += 200
      }
      if (llait == 2 && psucre == 2 && taille == 3) {
        prix = 3.70
        prixS = 0.10
        prixL = 0.0
        poudre += 12
        lait += 200
        sucre += 5
      }
      if (llait == 2 && psucre == 3 && taille == 3) {
        prix = 3.70
        prixS = 0.20
        prixL = 0.0
        poudre += 12
        lait += 200
        sucre += 10
      }
      if (llait == 2 && psucre == 4 && taille == 3) {
        prix = 3.70
        prixS = 0.30
        prixL = 0.0
        poudre += 12
        lait += 200
        sucre += 15

      }
      if (llait == 1) {
        while (dose != 1 && dose != 2 && dose != 3) {
          dose = readLine("Combien de dose ?\n>").toInt
        }
        if (dose == 1 && taille == 1 && psucre == 1) {
          prix = 2.70
          prixS = 0.0
          prixL = 0.05
          poudre += 6
          lait += 170
        }
        if (dose == 1 && taille == 1 && psucre == 2) {
          prix = 2.70
          prixS = 0.10
          prixL = 0.05
          poudre += 6
          lait += 170
          sucre += 5
        }
        if (dose == 1 && taille == 1 && psucre == 3) {
          prix = 2.70
          prixS = 0.20
          prixL = 0.05
          poudre += 6
          lait += 170
          sucre += 10
        }
        if (dose == 1 && taille == 1 && psucre == 4) {
          prix = 2.70
          prixS = 0.30
          prixL = 0.05
          poudre += 6
          lait += 170
          sucre += 15
        }
        if (dose == 2 && taille == 1 && psucre == 1) {
          prix = 2.70
          prixS = 0.0
          prixL = 0.10
          poudre += 6
          lait += 220
        }
        if (dose == 2 && taille == 1 && psucre == 2) {
          prix = 2.70
          prixS = 0.10
          prixL = 0.10
          poudre += 6
          lait += 220
          sucre += 5
        }
        if (dose == 2 && taille == 1 && psucre == 3) {
          prix = 2.70
          prixS = 0.20
          prixL = 0.10
          poudre += 6
          lait += 220
          sucre += 10
        }
        if (dose == 2 && taille == 1 && psucre == 4) {
          prix = 2.70
          prixS = 0.30
          prixL = 0.10
          poudre += 6
          lait += 220
          sucre += 15
        }
        if (dose == 3 && taille == 1 && psucre == 1) {
          prix = 2.70
          prixS = 0.0
          prixL = 0.15
          poudre += 6
          lait += 270
        }
        if (dose == 3 && taille == 1 && psucre == 2) {
          prix = 2.70
          prixS = 0.10
          prixL = 0.15
          poudre += 6
          lait += 270
          sucre += 5
        }
        if (dose == 3 && taille == 1 && psucre == 3) {
          prix = 2.70
          prixS = 0.20
          prixL = 0.15
          poudre += 6
          lait += 270
          sucre += 10
        }
        if (dose == 3 && taille == 1 && psucre == 4) {
          prix = 2.70
          prixS = 0.30
          prixL = 0.15
          poudre += 6
          lait += 270
          sucre += 15
        }
        if (dose == 1 && taille == 2 && psucre == 1) {
          prix = 3.20
          prixS = 0.0
          prixL = 0.05
          poudre += 8
          lait += 200
        }
        if (dose == 1 && taille == 2 && psucre == 2) {
          prix = 3.20
          prixS = 0.10
          prixL = 0.05
          poudre += 8
          lait += 200
          sucre += 5
        }
        if (dose == 1 && taille == 2 && psucre == 3) {
          prix = 3.20
          prixS = 0.20
          prixL = 0.05
          poudre += 8
          lait += 200
          sucre += 10
        }
        if (dose == 1 && taille == 2 && psucre == 4) {
          prix = 3.20
          prixS = 0.30
          prixL = 0.05
          poudre += 8
          lait += 200
          sucre += 15
        }
        if (dose == 2 && taille == 2 && psucre == 1) {
          prix = 3.20
          prixS = 0.0
          prixL = 0.10
          poudre += 8
          lait += 250
        }
        if (dose == 2 && taille == 2 && psucre == 2) {
          prix = 3.20
          prixS = 0.10
          prixL = 0.10
          poudre += 8
          lait += 250
          sucre += 5
        }
        if (dose == 2 && taille == 2 && psucre == 3) {
          prix = 3.20
          prixS = 0.20
          prixL = 0.10
          poudre += 8
          lait += 250
          sucre += 10
        }
        if (dose == 2 && taille == 2 && psucre == 4) {
          prix = 3.20
          prixS = 0.30
          prixL = 0.10
          poudre += 8
          lait += 250
          sucre += 15
        }
        if (dose == 3 && taille == 2 && psucre == 1) {
          prix = 3.20
          prixS = 0.0
          prixL = 0.15
          poudre += 8
          lait += 300
        }
        if (dose == 3 && taille == 2 && psucre == 2) {
          prix = 3.20
          prixS = 0.10
          prixL = 0.15
          poudre += 8
          lait += 300
          sucre += 5
        }
        if (dose == 3 && taille == 2 && psucre == 3) {
          prix = 3.20
          prixS = 0.20
          prixL = 0.15
          poudre += 8
          lait += 300
          sucre += 10
        }
        if (dose == 3 && taille == 2 && psucre == 4) {
          prix = 3.20
          prixS = 0.30
          prixL = 0.15
          poudre += 8
          lait += 300
          sucre += 15
        }
        if (dose == 1 && taille == 3 && psucre == 1) {
          prix = 3.70
          prixS = 0.0
          prixL = 0.05
          poudre += 12
          lait += 250
        }
        if (dose == 1 && taille == 3 && psucre == 2) {
          prix = 3.70
          prixS = 0.10
          prixL = 0.05
          poudre += 12
          lait += 250
          sucre += 5
        }
        if (dose == 1 && taille == 3 && psucre == 3) {
          prix = 3.70
          prixS = 0.20
          prixL = 0.05
          poudre += 12
          lait += 250
          sucre += 10
        }
        if (dose == 1 && taille == 3 && psucre == 4) {
          prix = 3.70
          prixS = 0.30
          prixL = 0.05
          poudre += 12
          lait += 250
          sucre += 15
        }
        if (dose == 2 && taille == 3 && psucre == 1) {
          prix = 3.70
          prixS = 0.0
          prixL = 0.10
          poudre += 12
          lait += 300
        }
        if (dose == 2 && taille == 3 && psucre == 2) {
          prix = 3.70
          prixS = 0.10
          prixL = 0.10
          poudre += 12
          lait += 300
          sucre += 5
        }
        if (dose == 2 && taille == 3 && psucre == 3) {
          prix = 3.70
          prixS = 0.20
          prixL = 0.10
          poudre += 12
          lait += 300
          sucre += 10
        }
        if (dose == 2 && taille == 3 && psucre == 4) {
          prix = 3.70
          prixS = 0.30
          prixL = 0.10
          poudre += 12
          lait += 300
          sucre += 15
        }
        if (dose == 3 && taille == 3 && psucre == 1) {
          prix = 3.70
          prixS = 0.0
          prixL = 0.15
          poudre += 12
          lait += 350
        }
        if (dose == 3 && taille == 3 && psucre == 2) {
          prix = 3.70
          prixS = 0.10
          prixL = 0.15
          poudre += 12
          lait += 350
          sucre += 5
        }
        if (dose == 3 && taille == 3 && psucre == 3) {
          prix = 3.70
          prixS = 0.20
          prixL = 0.15
          poudre += 12
          lait += 350
          sucre += 10
        }
        if (dose == 3 && taille == 3 && psucre == 4) {
          prix = 3.70
          prixS = 0.30
          prixL = 0.15
          poudre += 12
          lait += 350
          sucre += 15
        }
      }
    }
    //-1 pour atteindre le bon index
    if (Spoudre(machineId-1) >= poudre && Ssucre(machineId-1) >= sucre && Slait(machineId-1) >= lait) {
      Spoudre(machineId-1) -= poudre
      Slait(machineId-1) -= lait
      Ssucre(machineId-1)-= sucre
      poudre -= poudre
      lait -= lait
      sucre -= sucre
      prixT = prix + prixS + prixL
      println("Le prix est de : " + f"$prix%.2f" + "CHF" + " + " +  f"$prixS%.2f" + "CHF"+ " + " +  f"$prixL%.2f" + "CHF" + " = " + f"$prixT%.2f" + "CHF")
      Thread.sleep(1000)
      println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + pass)
      Thread.sleep(1000)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      Thread.sleep(3000)
      println("Votre " + cboisson + " est prêt! Bonne dégustation !")
      Thread.sleep(1500)
      true
    }
    else {
      if (poudre > Spoudre(machineId-1)) {
        println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)

      }
      else if (sucre>Ssucre(machineId-1)){
        println("Erreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)

      }
      else if(lait>Slait(machineId-1)){
        println("Erreur : Quantité de lait insuffisante pour \npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)

      }
      false
    }
  }


  def restockMachine(machineId: Int, Spoudre:Array[Int], Ssucre:Array[Int],Slait:Array[Int]): Unit = {
    var choixajout = 0
    val lait_en_L = Slait(machineId-1).toDouble
    println("Voici les stocks :")
    print("\nPoudre de café : " + Spoudre(machineId-1) + "g" + "\nSucre : " + Ssucre(machineId-1) + "g" + "\nLait : ")
    printf("%.2f", lait_en_L/1000)
    print("L\n")
    while(choixajout!=1 && choixajout !=2){
      println("Voulez-vous ajouter du stock ? " + "\n1) Oui" + "\n2) Non")
      choixajout= readLine(">").toInt
      if(choixajout!=1 && choixajout !=2){
        println("Veuillez choisir 1 ou 2")
      }
    }
    if(choixajout==1){
      println("Indiquez la quantité à ajouter : ")
      var ajoutC = -2
      var ajoutS = -1
      var ajoutL = -2
      while(ajoutC<0){
        ajoutC = readLine("\nPoudre de café > ").toInt
        if(ajoutC<0){
          println("Il faut saisir des quantités positives")
          Thread.sleep(1000)
        }
      }
      while(ajoutS<0){
        ajoutS = readLine("\nSucre > ").toInt
        if(ajoutS<0){
          println("Il faut saisir des quantités positives")
          Thread.sleep(1000)
        }
      }
      while(ajoutL<0){
        ajoutL = readLine("\nLait > ").toInt
        if(ajoutL<0){
          println("Il faut saisir des quantités positives")
          Thread.sleep(1000)
        }
      }
      Spoudre(machineId-1) += ajoutC
      Ssucre(machineId-1) += ajoutS
      Slait(machineId-1)+= ajoutL
      println("Les stocks ont été mis à jour avec succès" + "\nRetour au menu...")
    }
    else{
      println("Retour au menu...")
    }

  }
  def main(args: Array[String]): Unit = {
    val machinePins = Array("434343","434343","434343","434343","434343")
    var machineId = 0
    val nbMachines = 5
    var modif = 0
    val Spoudre = Array.fill(nbMachines)(50)
    val Ssucre = Array.fill(nbMachines)(30)
    val Slait = Array.fill(nbMachines)(500)
    var stockM = 0
    var choix = 0
    var essai = 0
    var exit = false

    while (!exit) {
      while (choix != 1 && choix != 2 && choix != 3) {
        choix = readLine("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      }

      if(choix==1){
        while(machineId<=nbMachines-5 || machineId> nbMachines){
          println("Veuiller choisir une machine (1-5) : ")
          machineId = readLine(">").toInt


        }
        println("Machine sélectionnée (1-5) > " + machineId)
        serveClient(machineId, Spoudre, Ssucre, Slait)
      }

      else if (choix == 2) {
        while(machineId<=nbMachines-5 || machineId> nbMachines){
          println("Veuiller choisir une machine (1-5) : ")
          machineId = readLine(">").toInt
        }
        println("Machine sélectionnée (1-5) > " + machineId)
        while(essai<3){
          if(validatePin(machineId, machinePins)){
            println("Accès accordé à la machine " + machineId)
            essai = 3
            while(stockM !=1 && stockM!= 2){
              println("Voulez vous changer le code PIN de la machine ou accéder à ses stock?")
              println("1) Changer le code PIN " + "\n2) Accéder aux stocks")
              stockM=readLine(">").toInt
              if(stockM !=1 && stockM!=2){
                println("Veuillez choisir 1 ou 2")
              }
            }
            if(stockM==1){
              println("Mise à jour du code PIN à 6 chiffres pour la machine " + machineId)
              updatePin(machineId,machinePins)
            }
            else{
              restockMachine(machineId,Spoudre, Ssucre, Slait)
            }
          }

          else{
            essai+=1
            println("Code PIN incorrect. " + (3-essai) + " tentatives restantes.")
            if(essai==3){
              println("\nTrop de tentatives échouées. Fin du programme.")
              exit = true
            }
          }

        }
      }

      else {
        exit=true
        println("Vous avez quitté le programme")
      }

      choix = 0
      modif = 0
      essai = 0
      stockM = 0
      machineId = 0
    }
  }
}