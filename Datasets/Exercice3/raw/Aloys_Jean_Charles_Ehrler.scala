import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileNotFoundException, FileWriter, PrintWriter}
object Main {

  class Machine(val id: Int, var codepin: String, var lait: Int,
                var sucre: Int, var cafe: Int){

    def addIngredient(ingredient: String, quantite: Int): Unit={
      if(ingredient=="cafe"){
        cafe+=quantite
      }
      else if(ingredient == "sucre"){
        sucre+=quantite
      }
      else{
        lait+=quantite
      }
    }

    def removeIngredient(ingredient:String, quantite:Int):Boolean={
      if(ingredient=="cafe" && cafe>=quantite){
        cafe-=quantite
        true
      }
      else if(ingredient=="sucre" && sucre>=quantite){
        sucre-=quantite
        true
      }
      else if(ingredient=="lait" && lait>=quantite){
        lait-=quantite
        true
      }
      else if (ingredient=="cafe" && cafe<quantite){
        false
      }
      else if(ingredient=="sucre" && sucre<quantite){
        false
      }
      else if(ingredient=="lait" && lait<quantite){
        false
      }
      else{
        false
      }
    }



  }

  //gestion fichier
  def loadcsv(filename: String): ArrayBuffer[Machine]={

    try{
      val fichier = Source.fromFile(filename)//source du fichier
      val lignef = fichier.reset.getLines//en haut du fichier
      lignef.next()//saute l'entête
      val machines = new ArrayBuffer[Machine]//objet machine
      var ligne_fichier = 1

      while(lignef.nonEmpty){
        val ligne = lignef.next //lis la ligne suivante
        val parts = ligne.split(",")//sépare la ligne =>stock dans le tableau parts
        val id = ligne_fichier
        val codepin = parts(0) //chaque index de part est un attribut de Machine
        val lait = parts(1).toInt
        val sucre = parts(2).toInt
        val cafe = parts(3).toInt
        machines += new Machine(id,codepin,lait,sucre,cafe)
        ligne_fichier+=1
      }
      machines
    }
    catch{
      case e:FileNotFoundException=>println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez")
        null//machine sera vide
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit={
    //réecriture
    val fichier2 = new PrintWriter(new FileWriter(filename,false))
    //l'entête
    fichier2.println("PINCODE,MILK,SUGAR,COFFEE")
    for(ligne<-machines){
      fichier2.println(ligne.codepin + "," + ligne.lait + "," + ligne.sucre + "," + ligne.cafe)
    }
    fichier2.close()
  }

  def validatePin(machineId:Int, machines : ArrayBuffer[Machine]):Boolean = {
    println("Entrez le code Pin : ")
    val PIN = readLine(">")
    if(PIN == machines(machineId-1).codepin){
      true
    }
    else{
      false
    }
  }
  def updatePin(machineId:Int,machines:ArrayBuffer[Machine]): Unit = {
    var nouveau_PIN = ""
    var i = 0
    println("Entrez un nouveau code PIN à 6 chiffres")
    while(nouveau_PIN.length != 6){
      nouveau_PIN = readLine(">")
      try{
        i = nouveau_PIN.toInt
      }
      catch{
        case e:NumberFormatException=>println("Veuillez saisir un nombre valide.")
          nouveau_PIN = ""
      }

      if(nouveau_PIN.length!=6){
        println("Veuillez donner un code à exactement 6 chiffres")
      }
      if(nouveau_PIN.length == 6){
        machines(machineId-1).codepin = nouveau_PIN
        println("Le code PIN a été mis à jour avec succès")
        println("Retour au menu princpal...")
      }
    }
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]):Boolean = {
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



    println("Vous avez choisi le mode Client")
    while (boisson != 1 && boisson != 2 && boisson != 3) {
      try{
        boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
      }
      catch{
        case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
      }
    }

    if (boisson == 1) {
      cboisson = "Expresso"
      println("Vous avez choisi un expresso")
      while (psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4) {
        try{
          psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }

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
        try{
          psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
      }

      while (llait != 1 && llait != 2) {
        try{
          llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
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
          try{
            dose = readLine("Combien de dose ?\n>").toInt
          }
          catch{
            case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
          }
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
        try{
          taille = readLine("Quelle taille souhaitez-vous ?\n1) Petit \n2) Moyen \n3) Grand \n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
      }
      while(psucre != 1 && psucre != 2 && psucre != 3 && psucre != 4){
        try{
          psucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
      }
      while(llait != 1 && llait != 2){
        try{
          llait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
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
          try{
            dose = readLine("Combien de dose ?\n>").toInt
          }
          catch{
            case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
          }
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
    if (machines(machineId-1).cafe >= poudre && machines(machineId-1).sucre >= sucre && machines(machineId-1).lait >= lait) {
      machines(machineId-1).removeIngredient("cafe",poudre)
      machines(machineId-1).removeIngredient("lait",lait)
      machines(machineId-1).removeIngredient("sucre",sucre)
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
      if (poudre > machines(machineId-1).cafe) {
        println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)

      }
      else if (sucre>machines(machineId-1).sucre){
        println("Erreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)

      }
      else if(lait>machines(machineId-1).lait){
        println("Erreur : Quantité de lait insuffisante pour \npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
        Thread.sleep(1500)
      }
      false
    }
  }

  def restockMachine(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    var choixajout = 0
    val lait_en_L = machines(machineId-1).lait.toDouble
    println("Voici les stocks :")
    print("\nPoudre de café : " + machines(machineId-1).cafe + "g" + "\nSucre : " + machines(machineId-1).sucre + "g" + "\nLait : ")
    printf("%.3f", lait_en_L/1000)
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
      machines(machineId-1).addIngredient("cafe",ajoutC)
      machines(machineId-1).addIngredient("sucre",ajoutS)
      machines(machineId-1).addIngredient("lait",ajoutL)
      println("Les stocks ont été mis à jour avec succès" + "\nRetour au menu...")
    }
    else{
      println("Retour au menu...")
    }

  }

  def main(args: Array[String]): Unit = {
    var machineId = 0
    val nbMachines = 5
    var modif = 0
    var stockM = 0
    var essai = 0
    var exit = false
    var choix = 0
    val machines = loadcsv("machines.csv")
    if(machines!=null){
      var i = 0
      while(i<5){
        println("\nMachine " + machines(i).id + " chargée :")
        print("ID: " + machines(i).id)
        print("\nCode PIN: " + machines(i).codepin + "\n")
        val lait2 = machines(i).lait.toDouble //afficher en double (litres) et 3f
        printf("Lait: %.3f", lait2/1000)
        print("L")
        print("\nSucre: " + machines(i).sucre + "g")
        print("\nCafé: " + machines(i).cafe + "g" + "\n")
        Thread.sleep(1200)
        i+=1
      }
      println("5 machines chargées avec succès !")
    }
    else{
      exit=true
      choix=3
      println("Erreur du chargement ou de la sauvegarde des machines. \nFermeture du programme")
      Thread.sleep(600)

    }
    while (!exit) {
      while(machineId<=nbMachines-5 || machineId> nbMachines){
        println("Veuiller choisir une machine (1-5) : ")
        try{
          machineId = readLine(">").toInt
        }
        catch{
          case e: NumberFormatException=>println("Veuillez choisir un identifiant valide (1-5)")
        }
      }
      println("Machine sélectionnée (1-5) > " + machineId)

      while (choix != 1 && choix != 2 && choix != 3) {
        try{
          choix = readLine("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
        }
        catch{
          case e:NumberFormatException=>println("Veuillez séléctionner un chiffre entre 1 et 3")
        }
      }

      if(choix==1){
        serveClient(machineId, machines)
      }

      else if (choix == 2) {
        while(essai<3){
          if(validatePin(machineId,machines)){
            println("Accès accordé à la machine " + machineId)
            essai = 3
            while(stockM !=1 && stockM!= 2){
              println("Voulez vous changer le code PIN de la machine ou accéder à ses stock?")
              println("1) Changer le code PIN " + "\n2) Accéder aux stocks")
              try{
                stockM=readLine(">").toInt
              }
              catch{
                case e:NumberFormatException=>println("Veuillez choisir 1 ou 2")
              }
            }
            if(stockM==1){
              println("Mise à jour du code PIN à 6 chiffres pour la machine " + machineId)
              updatePin(machineId,machines)
            }
            else{
              restockMachine(machineId,machines)
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
        println("Sauvegarde des machines...")
        Thread.sleep(1000)
        try{
          savecsv("machines.csv",machines)
          println("Fichier sauvegardé avec succès !")
          Thread.sleep(1000)
          println("Vous avez quitté le programme.")
        }
        catch{
          case e:FileNotFoundException=>println("Erreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
            println("Erreur du chargement ou de la sauvegarde des machines. \nFermeture du programme")
        }
      }
      if(machines==null){
        println("Erreur du chargement ou de la sauvegarde des machines. \nFermeture du programme")
      }
      choix = 0
      modif = 0
      essai = 0
      stockM = 0
      machineId = 0
    }
  }
}

