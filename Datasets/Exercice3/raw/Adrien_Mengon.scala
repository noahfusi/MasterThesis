import java.io.{FileNotFoundException, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.io.StdIn.readLine
import scala.reflect.io.File
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //////////////////////commande en cours///////////
    var coffeeNeeded = 0
    var sugarNeeded = 0
    var milkNeeded = 0
    var com1 = ""
    var com2 = ""
    var com3 = ""
    var ad1 = 0
    var ad2 = 0
    var ad3 = 0
    var isLatte = false
    var isEspresso = false
    var choix = ""
    var isRightFormat=true
    val file="machines.csv"
    var machineId=0
    var machines=ArrayBuffer[machine]()
    ///////////////////////////////////////////////////
    class machine(Id:Int,PIN:String,Milk:Int,Sugar:Int,Coffee:Int){
      val id = Id
      var pincode= PIN
      var milk= Milk
      var sugar=Sugar
      var coffee=Coffee
      def restockMachine(): Unit = {
        var tpoudre = 0
        var tsucre = 0
        var tlait = 0
        printf("\nStock:\n    Poudre de café:%5d  g\n    Lait          :%5d mL\n    Sucre         :%5d  g\nRéapprovisionnement des stocks...\nAjout :\n", coffee, milk, sugar)
        tpoudre = readLine("    Poudre de café (g):").toInt
        tlait = readLine("    Lait          (mL):").toInt
        tsucre = readLine("    Sucre          (g):").toInt
        coffee += tpoudre
        sugar += tsucre
        milk += tlait
        println("Niveaux de stock mis à jour.\nRetour au menu principal...")
      }
      def validatePin(): Boolean = {
        var CodeIn = ""
        var isValid = false
        var count = 3
        println("Entrez votre code PIN:")
        do {
          CodeIn = readLine(">")
          if (pincode == CodeIn) {
            println("Accès autorisé à la machine "+id+"\n")
            isValid = true
            count = 0
          } else {
            count -= 1
            printf("Code PIN incorrect, %d essais restants.\n", count)
          }
        } while (count > 0)
        if (!isValid) println("Trop de tentatives échouées. Fin du programme")
        choix = "end"
        return isValid
      }
      def updatePin(): Unit = {
        var input = ""
        var isValid = true
        var max=6
        println("Mise à jour du code PIN pour la machine "+ id)
        do {
          isValid=true
          input = readLine(s"Entrez votre nouveau Code PIN à $max chiffres > ")
          if (input.length == max) {
            for (i <- 0 until max) {
              if (input(i) < '0' || input(i) > '9') isValid = false
            }
          }
          else isValid = false
          if (isValid) {
            pincode = input
            println("Code PIN mis à jour.\nRetour au menu principal...")
          }
        }while (!isValid)
      }
      def addIngredients(ingredient:String,amount:Int): Unit = {
        if(ingredient.toLowerCase=="milk"){
          milk+=amount
        }
        else if(ingredient.toLowerCase=="sugar"){
          sugar+=amount
        }
        else if(ingredient.toLowerCase=="coffee"){
          coffee+=amount
        }
        else {
          println("Ingredient "+ingredient+" does not exist")
        }
      }
      def removeIngredient(ingredient:String,amount:Int):Boolean={
        var isStocked=true
        if(milk<milkNeeded){
          isStocked=false
          println("Erreur, lait insuffisant")
        }
        else if(sugar<sugarNeeded){
          isStocked=false
          println("Erreur, sucre insuffisant")
        }
        else if(coffee<coffeeNeeded){
          isStocked=false
          println("Erreur, poudre de café insuffisante")
        }
        if(isStocked) {
          if (ingredient.toLowerCase == "milk") {
              milk -= amount
              milkNeeded=0
              return true
          }
          else if (ingredient.toLowerCase == "sugar") {
              sugar -= amount
              sugarNeeded=0
              return true
          }
          else {
              coffee -= amount
              coffeeNeeded=0
              return true
          }
        }
        else {
          return false
        }
      }
    }
    def loadcsv(filename:String): ArrayBuffer[machine] = {
      val Tbuffer=ArrayBuffer[machine]()
      val file=File(filename)
      var stream:BufferedSource=null
      if(file.exists) {
        println("Chargement des machines depuis"+file)
        try{
          stream = Source.fromFile(filename)
          var temp=new Array[Int](4)
          var ID = 0

          for (i <- stream.getLines()) {
            if (isRightFormat) {
              val line = i.split(',')
              if (ID == 0) {
                if (i != "PINCODE,MILK,SUGAR,COFFEE") {
                  isRightFormat = false
                  println("Erreur: En-tête incorrecte pour le fichier " + file + ".\nVeuillez vérifier le contenu du fichier")
                }
              }
              else if (isRightFormat) {
                if (line.length == 4) {
                  for (j <- line.indices) {
                    temp(j) = line(j).toInt
                  }
                }
                else {
                  isRightFormat = false
                  println("Erreur: une ligne ou plus ne contient pas le bon nombre de colonnes.\nVérifiez que chaque ligne contient 4 colonnes.")
                }
              }
              if (isRightFormat && ID != 0) {
                Tbuffer += new machine(ID, line(0), temp(1), temp(2), temp(3))
              }
              ID += 1
            }
          }
        }
        catch{
          case a: FileNotFoundException => println("Erreur: fichier "+file+" inaccessible.\nVérifiez que vous bénéficiez des droits d'accès ou que le fichier n'est pas déjà en cours d'utilisation par un autre programme.")
                                        isRightFormat=false
          case b: NumberFormatException => println("Erreur: le fichier "+file+" ne contient pas le bon type de données.\nVérifiez que les nombres du fichier sont tous des entiers.")
                                        isRightFormat=false
          case _ =>println("Erreur: une erreur s'est produite pendant le chargement.")
                    isRightFormat=false
        }
        if (stream!=null) stream.close()
        if(isRightFormat) {
          println(Tbuffer.size+" machines chargées avec succès.")
        }
      }
      else println("Erreur: le fichier "+file+" est introuvable, veuillez vérifier que le fichier existe et est au bon endroit.")
      return Tbuffer
    }
    def savecsv(filename:String,machines:ArrayBuffer[machine]): Unit = {
      var stream:PrintWriter=null
      try {
        stream = new PrintWriter(filename)
        stream.print("PINCODE,MILK,SUGAR,COFFEE")
        for (i <- machines.indices) {
          stream.print("\n" + machines(i).pincode + "," + machines(i).milk + "," + machines(i).sugar + "," + machines(i).coffee)
        }
        printf("Sauvegarde de %s machines dans le fichier:\"machines.csv\".\n",machines.length)
      }
      catch{
        case _ => println("Échec de la sauvegarde, le fichier est peut-être vérouillé ou en mode read-only.")
      }
      if (stream!=null) stream.close()
      choix=""
    }
    def read(min: Int, max: Int): String = {
      var choix = ""
      do {
        choix = readLine(">")
        if(choix.length!=1) choix=""
      } while (choix < min.toString || max.toString < choix)
      return choix
    }
    def serveClient(): Boolean = {
      if (machines(machineId).removeIngredient("milk",milkNeeded)&&machines(machineId).removeIngredient("sugar",sugarNeeded)&&machines(machineId).removeIngredient("coffee",coffeeNeeded)) {
        //addition début
        println("\nBoisson choisie   : " + com1 + "\nNiveau de sucre   : " + com2)
        if (!isEspresso) println("Lait en supplément: " + com3)
        print("Prix total: ")
        if (ad1 > 0) printf("CHF %.2f", ad1.toDouble / 100) //boisson
        if (ad2 > 0) printf(" + CHF %.2f", ad2.toDouble / 100) //supplément sucre
        if (ad3 > 0) printf(" + CHF %.2f", ad3.toDouble / 100) //supplément lait
        printf(" = CHF %.2f", {
          ad1 + ad2 + ad3
        }.toDouble / 100)
        //addition fin
        //paiement début
        if (isLatte) com1 = "Latte" //correction du nom
        val twint = Random.alphanumeric.take(5).mkString.toUpperCase //génère "code twint"
        println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + twint + "\n(En attente de paiement...)")
        Thread.sleep(3000) //délai obligatoire de 3000 milisecondes
        println("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + com1 + " est prêt ! Bonne dégustation !")
        //paiement fin
        return true
      }
      //stock suffisant fin
      //Erreurs stock début
      else {
        println("Veuillez essayer une autre machine, une boisson différente ou vérifier les stocks en mode admin")
        return false
      }
      //Erreur stock fin
      //contrôle des stocks fin
    }
    def Client(): Unit = {
      coffeeNeeded=0
      milkNeeded=0
      sugarNeeded=0
      println("\nMachine sélectionnée 1-"+machines.length)
      machineId = read(1, machines.length).toInt - 1
      println("\nVeuillez sélectionner votre boisson :\n1) Espresso   - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte      - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      choix = read(1, 3)
      //sélection type de boisson fin
      //Latte début
      if (choix == "3") {
        isLatte = true
        println("\nVeuillez sélectionner la taille :")
        println("1) Petit\n2) Moyen\n3) Grand")
        choix = read(1, 3)
        if (choix == "1") {
          ad1 = 270
          milkNeeded = 120
          coffeeNeeded = 6
          com1 = "Latte (Petit)"
        }
        else if (choix == "2") {
          ad1 = 320
          milkNeeded = 150
          coffeeNeeded = 8
          com1 = "Latte (Moyen)"
        }
        else if (choix == "3") {
          ad1 = 370
          milkNeeded = 200
          coffeeNeeded = 12
          com1 = "Latte (Grand)"
        }
      }
      //Latte fin
      //esp+capp début
      else if (choix == "1") {
        ad1 = 200
        coffeeNeeded = 8
        com1 = "Espresso"
        isEspresso = true
      }
      else if (choix == "2") {
        ad1 = 250
        coffeeNeeded = 6
        milkNeeded = 100
        com1 = "Cappuccino"
      }
      //esp+capp fin
      //supplément sucre début
      println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu      (5g)  - CHF 0.10\n3) Moyen    (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      choix = read(1, 4)
      sugarNeeded = (choix.toInt - 1) * 5 ////ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
      ad2 = (choix.toInt - 1) * 10
      if (choix == "1") com2 = "Sans Sucre"
      else if (choix == "2") com2 = "Peu (5g)"
      else if (choix == "3") com2 = "Moyen (10g)"
      else com2 = "Beaucoup (15g)"
      //supplément sucre fin
      //supplément lait début
      if (!isEspresso) {
        println("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
        choix = read(0, 2)
        if (choix == "1") {
          println("\nCombien de doses ? (0,05CHF par dose. 3 Doses maximum)")
          do {
            choix = readLine(">")
          }
          while (choix < "0" || choix > "3")
          ad3 = choix.toInt * 5 //ces formules ont pour résultat la valeur correspondant au choix. pour le prix ou l'ingrédient
          milkNeeded += (choix.toInt * 100)
          if (choix == "1") com3 = "1 Dose"
          else if (choix == "2") com3 = "2 Doses"
          else if (choix == "3") com3 = "3 Dose"
          else com3 = "Non"
        }
        else com3 = "Non"
      }
    }
    def Admin(): Unit = {
      println("\nMachine sélectionnée 1-"+machines.length)
      machineId = read(1, machines.length).toInt - 1
      if (machines(machineId).validatePin()) {
        println("Que voulez-vous faire?\n1)Changer le code PIN\n2)Mettre à jour les stocks")
        choix = read(1, 2)
        if (choix == "1") machines(machineId).updatePin()
        else if (choix == "2") machines(machineId).restockMachine()
      }
    }
    def printmachines(): Unit = {//imprime les machines par ligne de 8 pour la lisibilité
      var start=0
      var end=math.min(machines.length-1,start+7)
      var max=machines.length
      do {
        print("\n,")
        for (i <- start to end) {
            print(f"${"-" * 3}machine ${i + 1}".padTo(15, '-')+",")
        }
        print("\n¦")
        for (i <- start to end) {
            print(f" id:${machines(i).id}".padTo(15, ' ') + "¦")
        }
        print("\n¦")
        for (i <- start to end) {
            print(f" PIN:${machines(i).pincode}".padTo(15, ' ') + "¦")
        }
        print("\n¦")
        for (i <-start to end) {
            print(f" lait:${machines(i).milk}mL".padTo(15, ' ') + "¦")
        }
        print("\n¦")
        for (i <- start to end) {
            print(f" café:${machines(i).coffee}g".padTo(15, ' ') + "¦")

        }
        print("\n¦")
        for (i <- start to end) {
            print(f" sucre:${machines(i).sugar}g".padTo(15, ' ') + "¦")
        }
        print("\n'")
        for (i <- start to end) {
            print("-" * 15+"'")

        }
        print("\n")
        max-=8
        start+=8
        end=math.min(machines.length-1,start+7)
      }while(max>0)
    }
///////////////////////////////////////////////////////Début Programme////////////////////////////////////////////////////////////////////////////////
    machines=loadcsv(file)
    if (isRightFormat) {
      do {
        Thread.sleep(1000)  //donne le temps à l'utilisateur de lire les messages
        printmachines()
        Thread.sleep(1500) //donne le temps à l'utilisateur de lire les messages
        println("\nVeuillez sélectionner le mode")
        println("1)Client\n2)Admin\n3)Quitter")
        choix = read(1, 3)
        if (machines.isEmpty) choix = "end"
        if (choix == "1") {
          do {
            Client()
          } while (!serveClient())
          choix = ""
        }
        if (choix == "2") {
          Admin()
        }
      } while (choix != "3" && choix != "end")
      savecsv(file, machines)
      println("Fin du programme.")
    }
  }
}