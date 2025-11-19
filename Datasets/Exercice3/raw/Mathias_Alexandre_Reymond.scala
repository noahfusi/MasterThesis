

import scala.io.StdIn.readLine
import scala.math._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.{FileNotFoundException,FileWriter,PrintWriter}
import scala.io.Source
import java.nio.file.AccessDeniedException



object Main {
  var id=0
  var pincode =""
  var milk=0
  var coffee=0
  var sugar=0

  class Machine(val id: Int, var pincode: String, var milk: Int, var coffee: Int, var sugar: Int ) {


    def addIngredient(ingredient: String, amount: Int):Unit = {

      if((ingredient=="lait")||(ingredient=="Lait")){
        milk += amount
      }
      if((ingredient=="café")||(ingredient=="Café")){
        coffee+=amount
      }
      if((ingredient=="sucre")||(ingredient=="Sucre")){
        sugar+=amount
      }
    }


    def removeIngredient(ingredient: String, amount: Int):Boolean = {
      if((ingredient=="lait")||(ingredient=="Lait")){
        if(milk>=amount){
          milk -= amount
          return true
        }
        else {
          return false
        }
      }
      else if((ingredient=="café")||(ingredient=="Café")){
        if(coffee>=amount){
          coffee-=amount
          return true
        }
        else{
          return false
        }
      }
      else {
        if(sugar>=amount){
          sugar-=amount
          return true
        }
        else {
          return false
        }
      }
    }
  }

  def loadcsv(filename: String) : ArrayBuffer[Machine] = {
    try{
      val fr = Source.fromFile(filename)
      val frG = fr.reset.getLines()
      var machines = new ArrayBuffer[Machine]
      var ligne = frG.next
      var i = 0
      while (!frG.isEmpty)  {
        ligne = frG.next
        var machine = ligne.split(",")
        val id= i + 1
        machines += new Machine(id, pincode, milk, coffee, sugar )
        machines(i).pincode = machine(0)
        machines(i).milk = machine(1).toInt
        machines(i).sugar = machine(2).toInt
        machines(i).coffee = machine(3).toInt



        i += 1
      }
      return machines
    }
    catch{
      case x : FileNotFoundException=>println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez!")
        Thread.sleep(2000)
        println("Erreur: Echec du chargement ou de la sauvegarde. Fermeture du programme!")
        null
    }

  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
val PW = new PrintWriter(new FileWriter(filename,false))
PW.println("PINCODE,MILK,SUGAR,COFFEE")
    var p = 0
    while(p<5){
      PW.println(machines(p).pincode+"," + machines(p).milk + ","+ machines(p).sugar + ","+ machines(p).coffee)
      p+=1
    }
    PW.close()
  }



  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    import scala.math._
    import scala.util.Random
    import scala.collection.mutable.ArrayBuffer
    import java.io.{FileWriter,PrintWriter}
    import scala.io.Source

    val nbMachines = 5
    var end=false
    var end2=false
    var choix=0
    var end3 = false
    var choixAdmin=""
    var mcodePin=0
    var nvcodePin = ""
    var essaiPin=0
    var caféIns=false
    var sucreIns=false
    var laitIns=false
    var choixmachine=0


    val machines = loadcsv("machines.csv")

println("Chargement des machines depuis machine.csv...")
    if(machines==null){
      end=true
      end3=true
    }
    else{

      try{
        for(i<-0 until 5){
          println("\nMachine " + machines(i).id + " chargée : ")
          println("ID : " + machines(i).id)
          println("CODE PIN : "+ machines(i).pincode)
          var laitL = machines(i).milk.toDouble
          printf("LAIT : %.3f", laitL/1000)
          print("L")
          println("\nSUCRE : " + machines(i).sugar + "g")
          println("CAFE : " + machines(i).coffee + "g\n")
          Thread.sleep(800)

        }
        println("5 machines chargées avec succès...")
      }
      catch{
        case x : AccessDeniedException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
      }
    }




    def validatePin(choixmachine: Int, machines: ArrayBuffer[Machine] ): Boolean = {
      choixAdmin=readLine("\nEntrez le code Pin : ****** \n>")
      if(choixAdmin==machines(choixmachine-1).pincode) {
        true
      }
      else {
        false
      }
    }

    def updatePin(choixmachine: Int, machines: ArrayBuffer[Machine] ) : Unit = {
      println("Mise à jour du code PIN pour la Machine " + choixmachine + ".")
      nvcodePin=readLine("Entrez un nouveau code Pin à 6 chiffres \n>")
      if (nvcodePin.length !=6) {
        do {
          nvcodePin=readLine("Entrez un nouveau code Pin à 6 chiffres \n>")
        }
        while(nvcodePin.length!=6)
      }

      machines(choixmachine-1).pincode = nvcodePin
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")
    }


    def serveClient(choixmachine: Int, machines:ArrayBuffer[Machine]) : Boolean = {
      end2=false
      var choixC = 0
      var choixC2 = 0
      var choixC3=0
      var choixLatte=0
      var choixC4=0
      var caféIns=false
      var sucreIns=false
      var laitIns=false
      var prixboisson=0.0f
      var prixsucre=0.0f
      var prixlait=0.0f
      var prixTotal=0.0f
      var typeboisson=""
      var typesucre=""
      var typelait=""
      var codeTwint=""
      var admin1=0
      var admin2=0
      var admin3=0
      var choixAdmin=0
      var choixStock=0
      var choixStock2=0
      var quantitéstock1=0.0
      var quantitéstock2=0.0
      var quantitéstock3=0.0
      var laitcafé=false
      var cafésucre=false
      var laitsucre=false
      var double=false
      try{
        choixC =readLine("Veuillez sélectionner votre boisson :" +
          "\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
        if((choixC!=1)&&(choixC!=2)&&(choixC!=3)) {
          do {
            choixC =readLine("Veuillez sélectionner votre boisson : " +
              "\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
          }
          while((choixC!=1)&&(choixC!=2)&&(choixC!=3))
        }
      }
      catch {
        case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
      }

      if(choixC==3){
        try{
          choixLatte=readLine("Quelle taille désirez-vous ?: " +
            "\n1) Petit \n2) Moyen \n3) Grand\n>").toInt
          if((choixLatte!=1)&&(choixLatte!=2)&&(choixLatte!=3)) {
            do {
              choixLatte=readLine("Quelle taille désirez-vous ?: " +
                "\n1) Petit \n2) Moyen \n3) Grand\n>").toInt
            }
            while((choixLatte!=1)&&(choixLatte!=2)&&(choixLatte!=3))
          }
        }
        catch{
          case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
        }

      }
      try{
        choixC2= readLine("Souhaitez-vous ajouter du sucre ?: " +
          "\n1) Pas de sucre \n2) Peu de sucre(5g) - CHF 0.10 \n3) Moyen(10g) - CHF 0.20 \n4) Beaucoup(15g) - CHF 0.30\n>").toInt
        if((choixC2!=1)&&(choixC2!=2)&&(choixC2!=3)&&(choixC2!=4)) {
          do {
            choixC2= readLine("Souhaitez-vous ajouter du sucre ?: " +
              "\n1) Pas de sucre " +
              "\n2) Peu de sucre(5g) - CHF 0.10 " +
              "\n3) Moyen(10g) - CHF 0.20 " +
              "\n4) Beaucoup(15g) - CHF 0.30\n>").toInt
          }
          while((choixC2!=1)&&(choixC2!=2)&&(choixC2!=3)&&(choixC2!=4))
        }
      }
      catch{
        case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
      }

      if((choixC==2)||(choixC==3)){
        try{
          choixC3=readLine("Voulez-vous ajouter du lait ?: " +
            "\n1) Oui " +
            "\n2) Non\n>").toInt
          if((choixC3!=1)&&(choixC3!=2)) {
            do {
              choixC3=readLine("Voulez-vous ajouter du lait ?: " +
                "\n1) Oui " +
                "\n2) Non\n>").toInt
            }
            while((choixC3!=1)&&(choixC3!=2))
          }
        }
        catch{
          case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
        }

        if(choixC3==1) {
          try{
            choixC4= readLine("\n1) Une dose (50ml) - CHF 0.05 " +
              "\n2) Deux doses (100ml) - CHF 0.10  " +
              "\n3) Trois doses (150ml) - CHF 0.15\n>").toInt
            if((choixC4!=1)&&(choixC4!=2)&&(choixC4!=3)) {
              do {
                choixC4= readLine("\n1) Une dose (50ml) - CHF 0.05 " +
                  "\n2) Deux doses (100ml) - CHF 0.10  " +
                  "\n3) Trois doses (150ml) - CHF 0.15\n>").toInt
              }
              while((choixC4!=1)&&(choixC4!=2)&&(choixC4!=3))
            }
          }
          catch{
            case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
          }

        }
      }

      if(choixC==1) {
        if(machines(choixmachine-1).coffee<8) {
          caféIns=true
          typeboisson="Expresso"
          machines(choixmachine-1).coffee +=0
        }
        else {
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee - 8
          prixboisson=2.00f
          typeboisson="Expresso"
        }
      }
      if(choixC==2) {
        if(machines(choixmachine-1).coffee<6) {
          caféIns=true
          typeboisson="Cappuccino"
          machines(choixmachine-1).coffee+=0
        }
        if(machines(choixmachine-1).milk<100) {
          laitIns=true
          typeboisson="Cappuccino"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee - 6
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 100
          prixboisson=2.50f
          typeboisson="Cappuccino"
        }
      }
      if((choixC==3)&&(choixLatte==1)) {
        if(machines(choixmachine-1).coffee<6) {
          caféIns=true
          typeboisson="Latte (Petit)"
          machines(choixmachine-1).coffee+=0
        }
        if(machines(choixmachine-1).milk<120) {
          laitIns=true
          typeboisson="Latte (Petit)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee - 6
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 120
          prixboisson=2.70f
          typeboisson="Latte (Petit)"
        }
      }
      if((choixC==3)&&(choixLatte==2)) {
        if(machines(choixmachine-1).coffee<8) {
          caféIns=true
          typeboisson="Latte (Moyen)"
          machines(choixmachine-1).coffee+=0
        }
        if(machines(choixmachine-1).milk<150) {
          laitIns=true
          typeboisson="Latte (Moyen)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee - 8
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 150
          prixboisson=3.20f
          typeboisson="Latte (Moyen)"
        }
      }
      if((choixC==3)&&(choixLatte==3)) {
        if(machines(choixmachine-1).coffee<12) {
          caféIns=true
          typeboisson="Latte (Grand)"
          machines(choixmachine-1).coffee+=0
        }
        if(machines(choixmachine-1).milk<200) {
          laitIns=true
          typeboisson="Latte (Grand)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee - 12
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 200
          prixboisson=3.70f
          typeboisson="Latte (Grand)"
        }
      }
      if(choixC4==1) {
        if(machines(choixmachine-1).milk<50) {
          laitIns=true
          typelait="une dose de lait (50ml)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 50
          prixlait=0.05f
          typelait="une dose de lait (50ml)"
        }
      }
      if(choixC4==2) {
        if(machines(choixmachine-1).milk<100) {
          laitIns=true
          typelait="deux doses de lait (100ml)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 100
          prixlait=0.10f
          typelait="deux doses de lait (100ml)"
        }
      }
      if(choixC4==3) {
        if(machines(choixmachine-1).milk<150) {
          laitIns=true
          typelait="trois doses de lait (150ml)"
          machines(choixmachine-1).milk+=0
        }
        else {
          machines(choixmachine-1).milk = machines(choixmachine-1).milk - 150
          prixlait=0.15f
          typelait="trois doses de lait (150ml)"
        }
      }
      if(choixC2==1) {
        machines(choixmachine-1).sugar = machines(choixmachine-1).sugar
        prixsucre=0.0f
        typesucre="Pas de sucre"

      }
      if(choixC2==2) {
        if(machines(choixmachine-1).sugar<5) {
          sucreIns=true
          typesucre="Peu"
          machines(choixmachine-1).sugar+=0
        }
        else {
          machines(choixmachine-1).sugar = machines(choixmachine-1).sugar - 5
          prixsucre=0.10f
          typesucre="Peu"
        }
      }
      if(choixC2==3) {
        if(machines(choixmachine-1).sugar<10) {
          sucreIns=true
          typesucre="Moyen"
          machines(choixmachine-1).sugar+=0
        }
        else {
          machines(choixmachine-1).sugar = machines(choixmachine-1).sugar - 10
          prixsucre=0.20f
          typesucre="Moyen"
        }
      }
      if(choixC2==4) {
        if(machines(choixmachine-1).sugar<15) {
          sucreIns=true
          typesucre="Beaucoup"
          machines(choixmachine-1).sugar+=0
        }
        else {
          machines(choixmachine-1).sugar = machines(choixmachine-1).sugar - 15
          prixsucre=0.30f
          typesucre="Beaucoup"
        }
      }
      prixTotal= prixboisson + prixsucre + prixlait
      println("Boisson sélectionnée : " + typeboisson)
      println("Niveau de sucre : " + typesucre)
      if(choixC3==1) {
        println("Lait supplémentaire : " + typelait)
      }
      else {
        println("Lait supplémentaire : Non")
      }
      if((caféIns==true)||(sucreIns==true)||(laitIns==true)) {
        end=false
        if ((caféIns==true)&&(laitIns==true)) {
          double=true
          println("Erreur : Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }
        else {
          laitcafé=false
        }
        if ((caféIns==true)&&(sucreIns==true)) {
          double=true
          println("Erreur : Quantité de sucre et de café insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }
        else {
          cafésucre=false
        }
        if ((sucreIns==true)&&(laitIns==true)) {
          double=true
          println("Erreur : Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }

        else {
          laitsucre=false
        }
        while ((laitsucre!=false)&&(laitcafé!=false)&&(cafésucre!=false)) {
          if(laitIns==true) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            laitsucre= true
          }

          if(caféIns==true) {
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
            laitcafé=true
          }
          if(sucreIns==true){

            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            cafésucre=true
          }
        }
        while (double!=true) {
          if(laitIns==true) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }

          if(caféIns==true) {
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }
          if(sucreIns==true){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }
          machines(choixmachine-1).coffee = machines(choixmachine-1).coffee
          machines(choixmachine-1).milk = machines(choixmachine-1).milk
          machines(choixmachine-1).sugar = machines(choixmachine-1).sugar
          end2=false
          admin1=0
        }
        return false
      }
      else {
        machines(choixmachine-1).coffee = machines(choixmachine-1).coffee
        machines(choixmachine-1).milk = machines(choixmachine-1).milk
        machines(choixmachine-1).sugar = machines(choixmachine-1).sugar
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ",prixboisson,prixsucre,prixlait,prixTotal)
        codeTwint=Random.alphanumeric.take(5).mkString

        println("\nVeuillez payer en utilisant TWINT.")
        println("Votre code de paiement est : " + codeTwint)
        println("En attente de validation du paiement...")

        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("...")
        println("Votre " +typeboisson+ " est prêt ! Bonne dégustation !")
        Thread.sleep(5000)
        end2=true
      }

      true
    }


    while(end!=true) {

      try{
        choixmachine = readLine("Quelle machine désirez-vous utiliser : \n1) machine 1 \n2) machine 2 \n3) machine 3 \n4) machine 4 \n5) machine 5 \n>").toInt
        if((choixmachine!=1)&&(choixmachine!=2)&&(choixmachine!=3)&&(choixmachine!=4)&&(choixmachine!=5)){
          do {
            choixmachine = readLine("Quelle machine désirez-vous utiliser : \n1) machine 1 \n2) machine 2 \n3) machine 3 \n4) machine 4 \n5) machine 5 \n>").toInt
          }
          while((choixmachine!=1)&&(choixmachine!=2)&&(choixmachine!=3)&&(choixmachine!=4)&&(choixmachine!=5))
        }
      }
      catch{
        case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
      }
      println("Machine "+ machines(choixmachine-1).id+ " chargée...")

      try{
        choix = readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
        if ((choix!=1)&(choix!=2)&(choix!=3)){
          do {
            choix= readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
          }
          while ((choix!=1)&(choix!=2)&(choix!=3))
        }
      }
      catch {
        case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
      }

      if(choix==3){

      }
      else {

        if((choix!=1)&&(choix!=2)&&(choix!=3)) {
          try{
            do {
              choix=readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
            }
            while((choix!=1)&&(choix!=2)&&(choix!=3))
          }
          catch{
            case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
          }

        }
        var choixC = 0
        var choixC2 = 0
        var choixC3=0
        var choixLatte=0
        var choixC4=0
        var caféIns=false
        var sucreIns=false
        var laitIns=false
        var prixboisson=0.0f
        var prixsucre=0.0f
        var prixlait=0.0f
        var prixTotal=0.0f
        var typeboisson=""
        var typesucre=""
        var typelait=""
        var codeTwint=""
        var end2=false
        var admin1=0
        var admin2=0
        var admin3=0
        var choixAdmin=""
        var choixStock=0
        var choixStock2=0
        var quantitéstock1=0.0
        var quantitéstock2=0.0
        var quantitéstock3=0.0
        var laitcafé=false
        var cafésucre=false
        var laitsucre=false
        var double=false
        var essaiPin = 0
        var end3 = false
        if (choix==1) {
            val erreurstocks = serveClient(choixmachine: Int, machines: ArrayBuffer[Machine])
            if (erreurstocks) {
              end2=true

            }
            else {
              println("Veuillez sélectionner une autre machine !")
              Thread.sleep(3000)
              end2=true

            }

        }

        if(choix==2) {
          print("Machine sélectionnée (1-5) >  " + choixmachine)
          while(!end3){
            while(essaiPin<3){
              val validPin = validatePin(choixmachine,machines)
              if(validPin) {
                essaiPin = 3
                println("Accès accordé à la machine "+ choixmachine)
                try{
                  mcodePin=readLine("Voulez-vous mettre à jour le code PIN? \n1)Oui \n2)Non \n>").toInt
                  if((mcodePin!=1)&&(mcodePin!=2)) {
                    do {
                      mcodePin=readLine("Voulez-vous mettre à jour le code PIN? \n1)Oui \n2)Non \n>").toInt
                    }
                    while((mcodePin!=1)&&(mcodePin!=2))
                  }
                }
                catch{
                  case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
                }

                if(mcodePin==1) {
                  updatePin(choixmachine,machines)
                  essaiPin=3
                  Thread.sleep(3000)
                  end3=true
                }

                else {
                  var AR=0
                  try{
                    do { AR=readLine("Voulez-vous ajouter ou enlever du stock? \n1) ajouter \n2) enlever \n>").toInt}
                    while((AR!=1)&&(AR!=2))
                  }
                  catch{
                    case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci!")
                  }
                  if(AR==1){
                    var ingredient= ""
                    while((ingredient!="lait")&&(ingredient!="café")&&(ingredient!="sucre")){
                      ingredient = readLine("Quel est l'ingrédient (en minuscules) dont vous souhaitez augmenter la quantité?  \n> ")
                      if((ingredient!="lait")&&(ingredient!="café")&&(ingredient!="sucre")){
                        ingredient=readLine("Cet ingrédient n'existe pas! Veuillez choisir lait, café ou sucre \n>")
                      }
                    }
                   var amount=0
                    try{
                      if(ingredient=="lait"){
                        amount=readLine("Combien voulez vous rajouter de lait (en mL) \n>").toInt
                      }
                      if(ingredient=="café"){
                        amount=readLine("Combien voulez vous rajouter de café (en grammes) \n>").toInt
                      }
                      if(ingredient=="sucre"){
                        amount=readLine("Combien voulez vous rajouter de sucre (en grammes) \n>").toInt
                      }
                    }
                    catch{
                      case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci! \n>")
                    }

                    machines(choixmachine-1).addIngredient(ingredient,amount)
                    println("Les quantités ont été ajoutées")
                    Thread.sleep(700)
                  }
                  else if(AR==2){
                    var ingredient= ""
                    while((ingredient!="lait")&&(ingredient!="café")&&(ingredient!="sucre")){
                      ingredient = readLine("Quel est l'ingrédient(en minuscules) dont vous souhaitez diminuer la quantité? \n>")
                      if((ingredient!="lait")&&(ingredient!="café")&&(ingredient!="sucre")){
                        ingredient=readLine("Cet ingrédient n'existe pas! Veuillez choisir lait, café ou sucre \n>")
                      }
                    }
                    var amount=0
                    try{
                      if(ingredient=="lait"){
                        amount=readLine("Combien voulez vous retirer de lait (en mL) \n>").toInt
                      }
                      if(ingredient=="café"){
                        amount=readLine("Combien voulez vous retirer de café (en grammes) \n>").toInt
                      }
                      if(ingredient=="sucre"){
                        amount=readLine("Combien voulez vous retirer de sucre (en grammes) \n>").toInt
                      }
                    }
                    catch{
                      case x : NumberFormatException=>println("Erreur, veuillez choisir une valeur adéquate, merci! \n>")
                    }

                    machines(choixmachine-1).removeIngredient(ingredient,amount)
                    println("Les quantités ont été retirées")
                    Thread.sleep(700)
                  }
                  end2=true
                  end3=true
                }
              }
              else{
                essaiPin+=1
                println("Code PIN incorrect. " + (3-essaiPin) + " tentatives restantes.")
                if(essaiPin==3){
                  println("Trop de tentatives échouées. Fin du programme.")
                  end3 = true
                  end = true
                }
              }
            }
          }
        }
      }
      if(choix==3) {
        println("Au revoir..")
        end = true

      }
    }

    try {
      println("Sauvegarde de 5 machines dans machines.csv...")
      Thread.sleep(2000)
      savecsv("machines.csv",machines)
      println("Fichier sauvegardé avec succès!")
    }
    catch {
      case x : FileNotFoundException =>println("Erreur : échec de l'écriture dans machine.csv \nLe fichier peut être verrouillé ou en lecture seule.")
        Thread.sleep(2000)
        println("Erreur: Echec du chargement ou de la sauvegarde. Fermeture du programme!")
    }


  }
}
