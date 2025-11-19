import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileNotFoundException, FileWriter, PrintWriter}



object Main {
  class Machine(val id:Int, var pincode:String, var milk:Int,var sugar:Int, var coffee:Int){
    def addIngredient(ingredient:String, amount:Int):Unit={
        if(ingredient == "lait"){
          milk+=amount
        }
        else if (ingredient=="sucre"){
          sugar+=amount
        }
        else if (ingredient=="cafe"){
          coffee+=amount
        }
        else{
       println("ingredient invalide")
            }
    }
    def removeIngredient(ingredient:String, amount:Int):Boolean={
      if(ingredient == "lait"){
        if(amount<=milk){
          milk-=amount
          true
        }
        else{
          false
        }
      }
      else if (ingredient=="sucre"){
        if(amount<=sugar){
          sugar-=amount
          true
        }
        else{
          false
        }
      }

      else if (ingredient=="cafe"){
        if(amount<=coffee){
          coffee-=amount
          true
        }
        else{
          false
        }
      }

      else{

        false
      }
  }
  }

  def loadCSV(filename:String):ArrayBuffer[Machine]={
    try{
      //source + sauter la première ligne
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines
      lignefr.next()

      val machines = new ArrayBuffer[Machine]
      var i = 1
      while(lignefr.nonEmpty){ //pour chaque ligne, créer un objet Machine
        val ligne = lignefr.next
        val parties = ligne.split(",") //séparer chaque ligne =>stockage dans le tableau "ligne"
        val id = i
        val pincode = parties(0)
        val milk = parties(1).toInt
        val sugar = parties(2).toInt
        val coffee = parties(3).toInt
        machines += new Machine(id,pincode,milk,sugar,coffee)
        i+=1
      }
      machines
    }
    catch {
      case ex: java.io.FileNotFoundException=>println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez") //fichier introuvable
        null
    }
  }

def saveCSV(filename:String, machines: ArrayBuffer[Machine]):Unit={
  //on réecrit le fichier
  val pw = new PrintWriter(new FileWriter(filename,false))
  //le titre
  pw.println("PINCODE,MILK,SUGAR,COFFEE")
  //les machines

  //pour chaque objet Machine dans la collection machines
  for(i<-machines){
    pw.println(i.pincode + "," + i.milk + "," + i.sugar + "," + i.coffee)
  }
  pw.close()

}

  def selectMachine():Int = {
    val nbMachines = 5
    var machineId = 0
    println("Veuillez sélectionner une machine (1-5)")
    while(machineId>nbMachines || machineId<=0){
      try{
        machineId = readLine(">").toInt
        if(machineId>nbMachines || machineId<=0){
          println("Veuillez sélectionner un identifiant valide (1-5)")
        }
      }
      catch{
        case ex:java.lang.NumberFormatException=>println("Veuillez saisir un chiffre entre 1 et 5")
      }
    }
    machineId-1 //-1 pour obtenir l'index de 0 à 4
  }

  def validatePin(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {
    println("Entrez le code Pin : ")
    val codepin = readLine(">")
    if(codepin!= machines(machineId).pincode)
      false
    else{

      true
    }
  }

  def updatePin(machineId:Int, machines: ArrayBuffer[Machine]):Unit = {
    var new_PIN = ""
    var new_PIN2 = 0 //pour vérifier s'il s'agit bien de nombres et pas de lettres
while(new_PIN.length!=6) {
  try{
    new_PIN = readLine(">")
    new_PIN2 = new_PIN.toInt //si la conversion rate == c'est qu'il y a des lettres dans la réponse=> exception
    if(new_PIN.length != 6){
      println("Veuillez entrer un nouveau code PIN à 6 chiffres")
    }
    else{
      machines(machineId).pincode = new_PIN
      println("Le code PIN a été mis à jour avec succès")
      println("Retour au menu princpal...")
      Thread.sleep(2000)
    }
  }
  catch{
    case ex:NumberFormatException=>println("Veuillez saisir des nombres !")
      new_PIN=""
  }
  }
}

  def serveClient (machineId:Int,machines: ArrayBuffer[Machine]) : Boolean = {
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

    if(machines(machineId).coffee>=qt_cafe){
      if(machines(machineId).sugar>=qt_sucre){
        if(boisson=="2" || boisson=="3"){
          if(machines(machineId).milk>=qt_lait){
            machines(machineId).removeIngredient("cafe",qt_cafe)
            machines(machineId).removeIngredient("sucre",qt_sucre)
            machines(machineId).removeIngredient("lait",qt_lait)
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
          machines(machineId).removeIngredient("cafe",qt_cafe)
          machines(machineId).removeIngredient("sucre",qt_sucre)
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
    var fin = false
    var modif_machines =0 //pour savoir le nombre de machines utilisées
    //exo3//
    println("Chargement des machines dans machines.csv...")
    Thread.sleep(1500)

    val machines = loadCSV("machines.csv")
    if(machines==null){ //si erreur de chargement
      fin = true
      println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
    else{
      for(i<- 0 to 4){
        println("Machine " + (i+1) + " chargée :")
        println("ID: " + machines(i).id)
        print("Code PIN: " + machines(i).pincode + "\n")
        val affichage_lait = machines(i).milk.toDouble
        printf("Lait: %.3f", affichage_lait/1000)
        print("L" + "\n")
        println("Sucre: " + machines(i).sugar + "g")
        println("Café: " + machines(i).coffee + "g" + "\n")
        Thread.sleep(1000)
      }
    }

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
    var choix_stock = "" //pour le mode admin

    while(!fin){
      val machineId = selectMachine()
      if(modif_machines!=(machineId)+1){
        modif_machines+=1
      }

      //message de chargement de la machine

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
        val Client = serveClient(machineId,machines:ArrayBuffer[Machine])
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
        var tentative = 3 //pour valider le pin
        println("Machine sélectionnée (1-5) > " + (machineId+1))
        while(tentative>0){
          if(validatePin(machineId, machines)){
            println("Accès autorisé à la machine " + (machineId+1))
            tentative = 0
            //choix stock ou code PIN
            println("Voulez vous changer les stocks ou modifier le code PIN ?" + "\n1) Stocks " + "\n2) Code PIN")
            var choix_admin = 0
            while(choix_admin!=1 && choix_admin!=2){
            try{
                choix_admin= readLine().toInt
                if(choix_admin!=1 && choix_admin!=2){
                  println("Veuillez choisir une des deux options, 1 ou 2")
                }
            }
            catch{
              case ex:java.lang.NumberFormatException=>println("Veuillez choisir entre le chiffre 1 ou 2")
            }
            }

            if(choix_admin==1){
              var choix_ingredient = 0
              println("Voulez-vous ajouter (1) ou enlever du stock (2) ?")//remove ou addIngredient
              while(choix_ingredient!=1 && choix_ingredient!=2){
                try{
                  choix_ingredient = readLine(">").toInt
                  if(choix_ingredient!=1 && choix_ingredient!=2){
                    println("Veuillez choisir un chiffre entre 1 et 2")
                  }
                  }
                  catch{
                    case ex:java.lang.NumberFormatException=> println("Veuillez choisir un chiffre entre 1 et 2")
                  }
                }
              if(choix_ingredient==1){
                var ingredient = ""
                while(ingredient!="lait"&& ingredient!="sucre" && ingredient!="cafe"){
                  println("Quel ingrédient voulez-vous ajouter ?")
                  ingredient = readLine(">")
                  if(ingredient!="lait"&& ingredient!="sucre" && ingredient!="cafe"){
                    println("Veuillez choisir entre 'lait','sucre' et 'cafe'")
                  }
                }
                var amount = -1
                var f = 0 //pour avoir une demande à l'infinie si erreur
                while(f!=1) {
                    try{
                      while(amount<0){
                      amount= readLine("Quelle quantité voulez-vous ajouter?").toInt
                        println("Veuille saisir une valeur positive")
                    }
                      machines(machineId).addIngredient(ingredient,amount)
                      f=1
                      println("Ajout effectué." + "\n")
                    }
                    catch{
                      case ex:java.lang.NumberFormatException=>println("Veuillez saisir une quantité valide")

                    }
                }
              }
              else{ //retirer une quantité
                var ingredient = ""
                while(ingredient!="lait"&& ingredient!="sucre" && ingredient!="cafe"){
                  println("Quel ingrédient voulez-vous retirer ?")
                  ingredient = readLine(">")
                  if(ingredient!="lait"&& ingredient!="sucre" && ingredient!="cafe"){
                    println("Veuillez choisir entre 'lait','sucre' et 'cafe'")
                  }
                }
                var g = 0 //pour avoir une demande à l'infinie si erreur
                var amount = -1
                while(g!=1) {
                  try{
                    while(amount<0){
                    amount= readLine("Quelle quantité voulez-vous retirer?").toInt
                      if(amount<0){
                        println("Veuille saisir une valeur positive")
                      }
                  }
                    if(machines(machineId).removeIngredient(ingredient,amount)){
                      println("Retrait effectué.")
                      Thread.sleep(1000)
                    }
                    else{
                      println("Stock insuffisant. Veuillez ajouter du stock dans le mode Admin ou changer de machine.")
                    }
                    g=1
                  }
                  catch{
                    case ex:java.lang.NumberFormatException=>println("Veuillez saisir une quantité valide")

                    }

                }
              }
            }
            else{
              println("Mise à jour du code PIN à 6 chiffres pour la machine " + (machineId+1) + "\nEntrez un nouveau code PIN à 6 chiffres ")
              updatePin(machineId,machines)
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
        try{
          println("Sauvegarde de " + modif_machines + " machines dans machines.csv...")
          saveCSV("machines.csv",machines)
          Thread.sleep(1000)
          println("Sauvegarde effectuée. \nFermeture du programme")
          Thread.sleep(1000)
        }
        catch{
          case ex: FileNotFoundException=>println("Erreur : Échec de l'écriture dans machines.csv. Le fichier est verouillé ou en lecture seule.")
            Thread.sleep(1500)
            println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        }
        fin = true

      }

mode=""
  }

    println("Au revoir !")

  }
}













