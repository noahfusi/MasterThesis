import scala.io.StdIn._
import scala.util.Random

object Main {

  def selectMachine():Int = {
    val nbMachines = 5
    var machineId = 0
    println("Veuillez sélectionner une machine (1-5)")
    while(machineId>nbMachines || machineId<=0){

      machineId = readLine(">").toInt
      if(machineId>nbMachines || machineId<=0){
        println("Veuillez sélectionner un identifiant valide (1-5)")
      }
    }
    machineId-1 //-1 pour obtenir l'index de 0 à 4
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    println("Entrez le code Pin : ")
    val codepin = readLine(">")
    if(codepin!= machinePins(machineId)){
      false
    }
    else{

      true
    }
  }

  def updatePin(machineId:Int, machinePins:Array[String]):Unit = {
    var new_PIN = ""
while(new_PIN.length!=6) {
  new_PIN = readLine(">")
    if(new_PIN.length != 6){
      println("Veuillez entrer un nouveau code PIN à 6 chiffres")
    }
    else{
      machinePins(machineId) = new_PIN
      println("Le code PIN a été mis à jour avec succès")
      println("Retour au menu princpal...")
      Thread.sleep(2000)
    }
  }
}
  def restockMachine(machineId:Int, stock_cafe:Array[Int],stock_sucre:Array[Int],stock_lait:Array[Int]):Unit = {
    var ajoutcafe = -1
    var ajoutlait = -1
    var ajoutsucre = -1
    val affichagelait = stock_lait(machineId).toDouble //pour afficher en L
    println("Niveaux de stock actuels : " + "\nPoudre de café : " + stock_cafe(machineId) + "g" + "\nSucre : " + stock_sucre(machineId) + "g" + "\nLait : "  + affichagelait/1000 + "L")
    println("Saisissez le montant à ajouter : ")
    while(ajoutcafe<0||ajoutlait<0||ajoutsucre<0) {
      ajoutcafe = readLine("Poudre de café: ").toInt
      ajoutlait = readLine("Lait (mL)         : ").toInt
      ajoutsucre = readLine("Sucre         : ").toInt
      if(ajoutcafe<0||ajoutlait<0||ajoutsucre<0){
        println("Veuillez saisir uniquement des valeurs positives")
        Thread.sleep(1500)
      }
    }

    println("Réapprovisionnement des stocks..." + "\nAjout :")
    Thread.sleep(2000)
    println("Poudre de café: " + ajoutcafe + "\nLait          : " + ajoutlait + "mL" + "\nSucre         : " + ajoutsucre)
    stock_cafe(machineId)+= ajoutcafe
    stock_lait(machineId)+=ajoutlait
    stock_sucre(machineId)+=ajoutsucre
    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
    Thread.sleep(3000)
  }


  def serveClient (machineId:Int, stock_cafe:Array[Int],stock_sucre:Array[Int],stock_lait:Array[Int]) : Boolean = {
    val code_paiement = Random.alphanumeric.take(5)
    var prix_boisson = ""
    var prix_final = 0.0 //*100 pour éviter les erreurs d'arrondis
    var prix_sucre = ""
    var prix_lait = ""
    var nom_boisson = ""
    var boisson = ""
    var espace = ""
    var choix_sucre = ""
    var choix_taille = ""
    var choix_lait = ""
    var dose_lait = ""
    var qt_cafe = 0
    var qt_lait = 0
    var qt_sucre = 0
    while (boisson != "1" && boisson != "2" && boisson != "3"){
      println("Veuillez sélectionner votre boisson :" + "\n1) Expresso - CHF 2.00" + "\n2) Cappuccino - CHF 2.50" + "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

      /*choix mode*/
      boisson = readLine(">")
      /*réinitialisation de espace*/
      espace = ""
      /* sans espace*/
      for(i<-boisson){
        if(i!=' '){
          espace += i
        }
      }
      boisson = espace
      if (boisson!= "1" && boisson != "2" && boisson!= "3")  {
        println(" Veuillez sélectionner la bonne boisson comme indiqué : 1,2 ou 3" + "\n")
        Thread.sleep(1000)
      }
    }
    if(boisson=="1" || boisson=="2"){
    while(choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre !="4"){
      println("Souhaitez-vous ajouter du sucre ? " + "\n1) Sans sucre" + "\n2) Peu (5g) - CHF 0.10" + "\n3) Moyen (10g) - CHF 0.20" + "\n4) Beaucoup (15g) - CHF 0.30")
      /*choix sucre*/
      choix_sucre = readLine(">")
      /*sans espace*/
      espace = ""
      for(i<-choix_sucre){
        if(i!=' '){
          espace += i
        }
      }
      choix_sucre = espace
      if (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre != "4"){
        println(" Veuillez sélectionner la bonne quantité comme indiqué : 1,2, 3 ou 4" + "\n")
        Thread.sleep(1000)
      }
    }

    if(boisson=="1"){
      /*données boisson*/
      nom_boisson = "Expresso"
      prix_boisson = "CHF 2.00"
      prix_final+= 200
      qt_cafe = 8
      }
    else if(boisson=="2") {
      nom_boisson="Cappuccino"
      prix_boisson = "CHF 2.50"
      prix_final+=250
      qt_cafe = 6
      qt_lait = 100

    }
    }
    else {

      while(choix_taille != "1" && choix_taille!="2" && choix_taille != "3"){
        println("Veuillez sélectionner la taille du Latte : " + "\n1) Petit" + "\n2) Moyen" + "\n3) Grand")
        choix_taille = readLine(">")
        /*sans espace*/
        espace = ""
        for(i<-choix_taille){
          if(i!=' '){
            espace += i
          }
        }
        choix_taille = espace
        if(choix_taille != "1" && choix_taille!="2" && choix_taille != "3"){
          println("Veuillez sélectionner la bonne taille comme indiqué : 1, 2, ou 3")
          Thread.sleep(1000)
        }
      }
      while(choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre !="4"){
        println("Souhaitez-vous ajouter du sucre ? " + "\n1) Sans sucre" + "\n2) Peu (5g) - CHF 0.10" + "\n3) Moyen (10g) - CHF 0.20" + "\n4) Beaucoup (15g) - CHF 0.30")
        /*choix sucre*/
        choix_sucre = readLine(">")
        /*sans espace*/
        espace = ""
        for(i<-choix_sucre){
          if(i!=' '){
            espace += i
          }
        }
        choix_sucre = espace
        if (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre != "4"){
          println(" Veuillez sélectionner la bonne quantité comme indiqué : 1,2, 3 ou 4" + "\n")
          Thread.sleep(1000)
        }
      }
      if (choix_taille == "1"){
        prix_boisson = "CHF 2.70"
        qt_cafe = 6
        qt_lait = 120
        nom_boisson = "Latte (Petit)"
        prix_final+=270
      }
      else if (choix_taille =="2"){
        prix_boisson = "CHF 3.20"
        qt_cafe = 8
        qt_lait = 150
        nom_boisson = "Latte (Moyen)"
        prix_final+=320
      }
      else {
        prix_boisson = "CHF 3.70"
        qt_cafe = 12
        qt_lait = 200
        nom_boisson = "Latte (Grand)"
        prix_final+=370
      }
    }
    if(boisson=="2"||boisson=="3") {
      while(choix_lait != "1" && choix_lait != "2"){
        println("Souhaitez-vous ajouter du lait en supplément ?" + "\n(disponible que pour Cappucino ou Latte)" + "\n1) Oui " + "\n2) Non " )

        /*choix lait*/
        choix_lait = readLine(">")
        espace = ""
        for(i<-choix_lait){
          if(i!=' '){
            espace += i
          }
        }
        choix_lait= espace
        if(choix_lait != "1" && choix_lait != "2"){
          println(" Veuillez sélectionner Oui ou Non, comme indiqué : 1 ou 2" + "\n")
        }
      }
    }
    /*si oui*/
    if (choix_lait == "1"){
      while(dose_lait!="1" && dose_lait!="2" && dose_lait!= "3"){
        println("Combien de dose ? ")
        /*choix dose*/
        dose_lait = readLine(">")
        espace = ""
        for(i<-dose_lait){
          if(i!=' '){
            espace += i
          }
        }
        dose_lait= espace
        if(dose_lait != "1" && dose_lait != "2" && dose_lait!="3"){
          println(" Veuillez sélectionner 1 à 3 dose de 50mL de lait, comme indiqué : 1, 2 ou 3" + "\n")
          Thread.sleep(1000)
        }
      }
    }
      println("Boisson sélectionnée : " + nom_boisson)

    /*montrer qt sucre + ajustement prix*/
      if (choix_sucre=="1"){
        qt_sucre=0
        println("Niveau de sucre : sans sucre")

      }
      else if (choix_sucre=="2"){
        qt_sucre = 5
        println("Niveau de sucre : Peu (5g)")
        prix_sucre = "CHF 0.10"
        prix_final +=  10
      }
      else if (choix_sucre=="3"){
        qt_sucre = 10
        println("Niveau de sucre : Moyen (10g)")
        prix_sucre = "CHF 0.20"
        prix_final +=  20
      }
      else {
        qt_sucre = 15
        println("Niveau de sucre : Beaucoup (15g)")
        prix_sucre = "CHF 0.30"
        prix_final+= 30
      }

      if (choix_lait != "1" ){
        println("Lait en supplément : Non")
      }
      else{

        if (dose_lait=="1"){
          println("Lait en supplément : 50 mL")
          prix_lait = "CHF 0.05"
          prix_final+=5
          qt_lait+=5
        }
        else if(dose_lait=="2"){
          println("Lait en supplément : 100mL")
          prix_lait = "CHF 0.10"
          prix_final+=10
          qt_lait+=10
        }
        else{
          println("Lait en supplément : 150mL")
          prix_lait = "CHF 0.15"
          prix_final+=15
          qt_lait+=15
        }
      }

    if(stock_cafe(machineId)>=qt_cafe){
      if(stock_sucre(machineId)>=qt_sucre){
        if(boisson=="2" || boisson=="3"){
          if(stock_lait(machineId)>=qt_lait){
            stock_cafe(machineId) -= qt_cafe
            stock_sucre (machineId)-= qt_sucre
            stock_lait(machineId)-= qt_lait
            print("\nPrix total : " + prix_boisson)
            if(choix_sucre!="1"){
              print(" + " + prix_sucre)
              if(choix_lait=="2"){
                print(" = ")
                printf("CHF %.2f",prix_final/100)
              }
            }
            if(choix_lait=="1"){
              print(" + " + prix_lait + " = ")
              printf("CHF %.2f",prix_final/100)
            }

            Thread.sleep(2000)
            println("\nVeuillez payer en utilisant Twint." + "\nVotre code de paiement est : " + code_paiement.mkString + "\n(En attente de validation du paiement...)" + "\n")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté. " + "\n")
            Thread.sleep(2000)
            println("Préparation de votre boisson... " + "\n[...]")
            Thread.sleep(3000)
            println("Votre " + nom_boisson + " est prêt ! Bonne dégustation ! " + "\n")
            Thread.sleep(2000)
             true
          }
          else{
            println("Erreur : Quantité de lait insuffisante pour préparer "+ "\nla boisson sélectionnée. ")
            Thread.sleep(3000)
            false
          }
        }

        else{
          stock_cafe(machineId) -= qt_cafe
          stock_sucre(machineId)-= qt_sucre
          if(choix_sucre=="1"){
            print("Prix total : " + prix_boisson)
          }
          else{
            print("Prix total : " + prix_boisson + " + " + prix_sucre + " = " )
            printf("CHF %.2f",prix_final/100)
          }

          Thread.sleep(2000)
          println("\nVeuillez payer en utilisant Twint." + "\nVotre code de paiement est : " + code_paiement.mkString + "\n(En attente de validation du paiement...)" + "\n")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté. " + "\n")
          Thread.sleep(2000)
          println("Préparation de votre boisson... " + "\n[...]")
          Thread.sleep(3000)
          println("Votre " + nom_boisson + " est prêt ! Bonne dégustation ! " + "\n")
          Thread.sleep(2000)
          true
        }
    }

      else{
        println("Erreur : Quantité de sucre insuffisante pour préparer " + "\nla boisson sélectionnée. ")
        Thread.sleep(3000)
        false
      }
    }
    else{
      println("Erreur : Quantité de café insuffisante pour préparer " + "\nla boisson sélectionnée. ")
      Thread.sleep(3000)
      false
    }

  }


  def main(args: Array[String]): Unit = {


    //exo2//
    val nbMachines = 5
    val machinePins = Array.fill(nbMachines)("434343")

    /*pour enlever les espaces dans les inputs*/
    var espace = ""
    var prix_sucre = ""
    var prix_final= 0.0

    /*choix*/
    var mode = ""
    var boisson=""
    var choix_sucre=""
    var choix_lait = ""
    var dose_lait = ""
    var choix_taille = ""
    /*stocks*/
    val stock_cafe = Array.fill(nbMachines)(50)
    val stock_sucre = Array.fill(nbMachines)(30)
    val stock_lait = Array.fill(nbMachines)(500) /*en mL*/
    var choix_stock = "" //pour le mode admin

    var fin = false

    while(!fin){
      while(mode != "1" && mode != "2" && mode != "3"){
        espace=""
        println("    Nospresso Café" + "\nVeuillez sélectionner votre mode :" + "\n1) Client" + "\n2) Admin" + "\n3) Quitter")
        /*choix mode*/
        mode = readLine(">")
        for(i<-mode){
          if(i!=' '){/*pour chaque caractère de 'mode', on ajoute à 'espace' le caractère, s'il n'y a pas d'espace*/
            espace += i
          }
        }
        mode = espace /*mode devient espace => il n'y a plus d'espace*/
        if (mode != "1" && mode != "2" && mode != "3")  {
          println(" Veuillez sélectionner le bon mode comme indiqué : 1,2 ou 3" + "\n")
          Thread.sleep(1000)
        }
      }

      /*suite après choix du mode*/

      if (mode=="1"){
        val machineId = selectMachine()
        val Client = serveClient(machineId,stock_cafe,stock_sucre,stock_lait)

          if(!Client) {
          println("Veuillez choisir une autre machine.")
            println("Retour au menu...")
            Thread.sleep(2000)
        }
          else {
            println("Retour au menu...")
            Thread.sleep(2000)
          }
        /*réinitialisation des variables choix*/
          boisson = ""
          mode = ""
          choix_lait = ""
          choix_sucre = ""
          dose_lait = ""
          choix_taille = ""
          choix_stock = ""
          /*variables prix*/
          prix_sucre  = ""
          prix_final = 0

          /*var espace*/
          espace =""
        }
      if (mode=="2"){
        val machineId = selectMachine()
        var tentative = 3 //pour valider le pin
        println("Machine sélectionnée (1-5) > " + (machineId+1))
        while(tentative>0){
          if(validatePin(machineId, machinePins)){
            println("Accès autorisé à la machine " + machineId+1)
            tentative = 0
            //choix stock ou code PIN
            println("Voulez vous accéder aux stocks ou modifier le code PIN ?" + "\n1) Stocks " + "\n2) Code PIN")
            var choix_admin = 0
            while(choix_admin!=1 && choix_admin!=2){
              choix_admin= readLine().toInt
              if(choix_admin!=1 && choix_admin!=2){
                println("Veuillez choisir une des deux options, 1 ou 2")
              }
            }
            if(choix_admin==1){
              restockMachine(machineId, stock_cafe, stock_sucre, stock_lait)


            }
            else{
              println("Mise à jour du code PIN à 6 chiffres pour la machine " + (machineId+1) + "\nEntrez un nouveau code PIN à 6 chiffres ")
              updatePin(machineId,machinePins)

            }
          }
          else{
            tentative-=1
            println("Code PIN incorrect. " + tentative + " tentatives restantes.")
            if(tentative==0){
              println("\nTrop de tentatives échouées. Fin du programme.")
              mode = "3"
            }
          }
        }
      }
      if(mode=="3"){
        fin = true
      }

mode=""
  }
    println(" Au revoir !")
  }
}












