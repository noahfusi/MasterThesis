import io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import java.io._

object Main {
  var mode = 0
  var erreur = false

  class Machine(val id:Int,var pincode:String,var milk:Int,var sugar:Int, var coffee:Int){
    def removeIngredient(ingredient:String,amount:Int): Boolean = {
      if(ingredient=="Lait" && milk>=amount){
        milk-=amount
        true
      }
      else if(ingredient=="Sucre" && sugar>=amount){
        sugar-=amount
        true
      }
      else if(ingredient=="Café" && coffee>=amount){
        coffee-=amount
        true
      }
      else{
        false
      }
    }


    def addIngredient(ingredient:String,amount:Int):Unit={
      if(ingredient=="Lait"){
        milk+=amount
      }
      else if(ingredient=="Sucre"){
        sugar+=amount
      }
      else{
        coffee+=amount
      }
    }
  }



  def loadcsv(filename:String):ArrayBuffer[Machine]={
    try{
      val fr = fromFile(filename)  //source
      val ligne_1 = fr.reset.getLines()
      val machines = new ArrayBuffer[Machine]
      var id = 1
      ligne_1.next()   //saute la ligne PINCODE,MILK,SUGAR,COFFEE
      while(!ligne_1.isEmpty){
        val ligne_2 = ligne_1.next()
        val colonne = ligne_2.split(",")  //chaque colonne est stocké
        val pincode = colonne(0)
        val milk = colonne(1).toInt
        val sugar = colonne(2).toInt
        val coffee = colonne(3).toInt
        machines += new Machine(id,pincode,milk,sugar,coffee)
        id+= 1
      }
      return machines
    }
    catch{
      case ex:java.io.FileNotFoundException =>println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez !")
        mode = 3
        erreur = true
        return null
    }
  }


  def savecsv(filename:String,machines:ArrayBuffer[Machine]):Unit={
    val reecriture_fichier = new PrintWriter(filename)
    reecriture_fichier.println("PINCODE,MILK,SUGAR,COFFEE") //en-tête
    var id = 0
    while(id<5){
      reecriture_fichier.println(machines(id).pincode + "," + machines(id).milk + "," + machines(id).sugar + "," + machines(id).coffee)
      id+=1
    }
    reecriture_fichier.close()
  }
  def validatePin(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {
    val codeUtil = readLine("Saisissez le code PIN: ")
    if (codeUtil == machines(machineId).pincode) {
      true
    }
    else {
      false
    }
  }

  def updatePin(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    var newcode = readLine("Saisissez un nouveau code PIN: ")
    while (newcode.length != 6){
      println("Le code PIN doit être composer de 6 chiffres.")
      newcode = readLine(">")
    }
    if (newcode.length == 6) {
      machines(machineId).pincode = newcode
      println("Le nouveau code est: " + machines(machineId).pincode)
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")
      Thread.sleep(1000)
    }
  }



  def serveClient(machineId: Int, machines:ArrayBuffer[Machine]): Boolean = {
    var prixboi: Double = 0.0
    var prixlait: Double = 0.0
    var prixsucre: Double = 0.0
    var taille = 0
    var sucre = 0
    var lait = 0

    var qcafe = 0
    var qmilk = 0
    var qsugar = 0
    taille = 0
    lait = 0
    sucre = 0
    var boisson = 0
    var nom = ""
    var grandeur = ""
    var nivsucre = ""
    var laitier = ""

    var service = false


    taille = 0
    sucre = 0
    lait = 0
    boisson = 0
    println("Veuillez sélectionner votre boisson:")

    println("1) Expresso - CHF 2.00")
    println("2) Capuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

    while (boisson != 1 && boisson != 2 && boisson != 3 ) {
      boisson = readLine(">").toInt
      if (boisson != 1 && boisson != 2 && boisson != 3 ) {
        println("Veuillez sélectionner 1,2 ou 3")
      }
    }
    if (boisson == 1) {
      qcafe = 8
      prixboi = prixboi + 2.00
      nom = "Expresso"
    }
    if (boisson == 2) {
      qmilk = 100
      qcafe = 6
      prixboi = prixboi + 2.50
      nom = "Capuccino"
    }
    if (boisson == 3) {
      println("Vous avez choisi le Latte, choisissez une taille")
      println("1) Petit - CHF 2.70")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70")
      while (taille != 1 && taille != 2 && taille != 3) {
        taille = readLine(">").toInt
        if (taille != 1 && taille != 2 && taille != 3) {
          println("Veuillez sélectionner 1,2 ou 3")
        }
      }
      if (taille == 1) {
        qmilk = 120
        qcafe = 6
        prixboi = prixboi + 2.70
        grandeur = "petit"
      }
      if (taille == 2) {
        qcafe = 8
        qmilk = 150
        prixboi = prixboi + 3.20
        grandeur = "moyen"
      }
      if (taille == 3) {
        qcafe = 12
        qmilk = 200
        prixboi = prixboi + 3.70
        grandeur = "grand"
      }
      nom = "Latte"
    }

    if (boisson == 1 || boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du sucre?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
        sucre = readLine(">").toInt
        if (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
          println("Veuillez sélectionner 1,2,3 ou 4")
        }
      }
      if (sucre == 1) {
        qsugar = 0
        prixsucre = prixsucre + 0.0
        nivsucre = "Sans"
      }
      if (sucre == 2) {
        qsugar = 5
        prixsucre = prixsucre + 0.10
        nivsucre = "Peu (5g)"
      }
      if (sucre == 3) {
        qsugar = 10
        prixsucre = prixsucre + 0.20
        nivsucre = "Moyen (10g)"
      }
      if (sucre == 4) {
        qsugar = 15
        prixsucre = prixsucre + 0.30
        nivsucre = "Beaucoup (15g)"
      }
    }
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément?")
      println("1) oui")
      println("2) non")
      while (lait != 1 && lait != 2) {
        lait = readLine(">").toInt
        if (lait != 1 && lait != 2) {
          println("Veuillez sélectionner 1 ou 2")
        }
      }
      if (lait == 1) {
        println("Combien de dose?")
        laitier = "Oui"
        var dose = 0
        println("1) 50 ml - CHF 0.05")
        println("2) 100 ml - CHF 0.10")
        println("3) 150 ml - CHF 0.15")
        while (dose != 1 && dose != 2 && dose != 3) {
          dose = readLine(">").toInt
          if (dose != 1 && dose != 2 && dose != 3) {
            println("Il y a uniquement 3 doses maximales")
          }
        }
        if (dose == 1) {
          qmilk += 50
          prixlait = prixlait + 0.05
        }
        if (dose == 2) {
          qmilk += 100
          prixlait = prixlait + 0.10
        }
        if (dose == 3) {
          qmilk += 150
          prixlait = prixlait + 0.15
        }
      }
      if (lait == 2) {
        laitier = "Non"
      }
    }
    println("Boisson séléctionnée : " + nom + " " + grandeur)
    println("Niveau de sucre : " + nivsucre)
    if (lait == 1 || lait == 2) {
      println("Lait supplémentaire : " + laitier)
    }
    Thread.sleep(2000)
    if (machines(machineId).coffee < qcafe) {
      println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson séléctionnée.")
      println("Veuillez remplir les stocks en mode Admin ou choisir une autre machine.")
      Thread.sleep(1500)

      service = false
    }
    else if (machines(machineId).milk < qmilk) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée.")
      println("Veuillez remplir les stocks en mode Admin ou choisir une autre machine.")
      Thread.sleep(1500)

      service = false
    }
    else if (machines(machineId).sugar < qsugar) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
      println("Veuillez remplir les stocks en mode Admin ou choisir une autre machine.")
      Thread.sleep(1500)

      service = false
    }
    else {
      machines(machineId).removeIngredient("Café",qcafe)
      machines(machineId).removeIngredient("Sucre",qsugar)
      machines(machineId).removeIngredient("Lait",qmilk)
      printf("Prix total : CHF %.2f ", prixboi)
      printf("+ CHF %.2f ", prixsucre)
      printf("+ CHF %.2f ", prixlait)
      printf("= CHF %.2f ", prixlait + prixsucre + prixboi)
      Thread.sleep(2000)
      println("\nVeuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString)
      println("(En attente de de validation du paiement) ")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      Thread.sleep(1000)
      println("Préparation de votre boisson...")
      println("[...]")
      Thread.sleep(3000)
      println("Votre " + nom + " est prêt ! Bonne dégustation ! ")
      Thread.sleep(1500)

      service = true
    }

    service
  }

  def restockMachine(machineId: Int, machines:ArrayBuffer[Machine]): Unit = {
    println("Niveau de stock actuel: ")
    println("Poudre à café: " +  machines(machineId).coffee + "g")
    println("Sucre: " + machines(machineId).sugar + "g")
    println("Lait: " + machines(machineId).milk + "ml")
    println("Entrez les quantités à ajouter: ")
    val ajoutcoffee = readLine("Poudre à café: ").toInt
    machines(machineId).addIngredient("Café",ajoutcoffee)
    val ajoutsugar = readLine("Sucre: ").toInt
    machines(machineId).addIngredient("Sucre",ajoutsugar)
    val ajoutmilk = readLine("Lait: ").toInt
    machines(machineId).addIngredient("Lait",ajoutmilk)
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(1500)
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    var machine = 0

    println("Chargement des machines...")
    val machines=loadcsv("machines.csv")
    if(machines!=null){
      var id = 0
      while(id < nbMachines){
        val affichage_milk = machines(id).milk.toDouble / 1000
        println("Machine " + machines(id).id + " chargée: ")
        println("ID: " + machines(id).id)
        println("Pincode: " + machines(id).pincode)
        printf("Lait: %.3f",affichage_milk)
        print("L\n")
        println("Sucre: " + machines(id).sugar + "g")
        println("Café: " + machines(id).coffee + "g\n")
        Thread.sleep(1000)
        id+=1
      }
      println("5 machines chargées avec succès !")
      Thread.sleep(500)


    }

    while (mode != 3) {
      mode = 0
      machine = 0
      println("Veuillez choisir votre machine entre 1 et 5")
      while (machine != 1 && machine != 2 && machine != 3 && machine != 4 && machine != nbMachines) {
        machine = readLine(">").toInt
        if (machine != 1 && machine != 2 && machine != 3 && machine != 4 && machine != nbMachines) {
          println("Veuillez sélectionner 1, 2, 3, 4 ou 5")
        }
      }
      val machineId = machine - 1


      println("Nospresso cafe")
      println("Veuillez choisir votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      while (mode != 1 && mode != 2 && mode != 3) {
        mode = readLine(">").toInt
        if (mode != 1 && mode != 2 && mode != 3) {
          println("Veuillez sélectionner 1, 2 ou 3")
        }
      }
      if (mode == 1) {
        if (machine == 1 || machine == 2 || machine == 3 || machine == 4 || machine == nbMachines) {
          val commande = serveClient(machineId, machines)
          if (commande){
            println("")
          }
          else{
            println("")
          }
        }
      }

      if (mode == 2) {
        var essai = 3
        var choix = 0
        val access = validatePin(machineId, machines)
        while (essai > 0) {
          if (access) {
            println("Accès autorisé")
            essai = 0
            choix = 0
            println("1) Changez le code PIN")
            println("2) Remplir les stocks")
            while (choix != 1 && choix != 2){
              choix = readLine(">").toInt
              if (choix != 1 && choix != 2){
                println("Veuillez choisir entre 1 ou 2")
              }
            }
            if (choix == 1){
              updatePin(machineId, machines)
            }
            if (choix == 2) {
              restockMachine(machineId, machines)
            }
          }
          else {
            essai = essai - 1
            if (essai == 2) {
              essai = essai - 1
              println("Code PIN incorrect. 2 tentatives restantes.")
              val deuxieme = validatePin(machineId, machines)
              if (deuxieme) {
                println("Accès autorisé")
                essai = 0
                choix = 0
                println("1) Changez le code PIN")
                println("2) Remplir les stocks")
                while (choix != 1 && choix != 2){
                  choix = readLine(">").toInt
                  if (choix != 1 && choix != 2){
                    println("Veuillez choisir entre 1 ou 2")
                  }
                }
                if (choix == 1){
                  updatePin(machineId, machines)
                }
                if (choix == 2) {
                  restockMachine(machineId, machines)
                }
              }
            }
            if (essai == 1){
              essai = essai - 2
              println("Code PIN incorrect. 1 tentatives restantes")
              val troisieme = validatePin(machineId, machines)
              if (troisieme) {
                println("Accès autorisé")
                essai = 0
                choix = 0
                println("1) Changez le code PIN")
                println("2) Remplir les stocks")
                while (choix != 1 && choix != 2){
                  choix = readLine(">").toInt
                  if (choix != 1 && choix != 2){
                    println("Veuillez choisir entre 1 ou 2")
                  }
                }
                if (choix == 1){
                  updatePin(machineId, machines)
                }
                if (choix == 2) {
                  restockMachine(machineId, machines)
                }
              }
            }
            if (essai < 0) {
              println("Trop de tentatives échouées. Fin du Programme.")
              mode = 3
            }
          }
        }
      }
      if (mode == 3){

        println("Bonne journée, aurevoir!")
      }
    }
    if(!erreur){
      try{
        println("Sauvegarde de 5 machines dans machines.csv...")
        Thread.sleep(1000)
        savecsv("machines.csv",machines)
        Thread.sleep(1000)
        println("Fichier sauvegardé avec succès !")
      }
      catch{
        case ex: java.io.FileNotFoundException=>println("Erreur : Échec de l'écriture dans machines.csv. Le fichier est verrouilé ou en lecture seule.")
          erreur = true
      }
    }
    if(erreur){
      Thread.sleep(1000)
      println("Erreur : Échec du chargement ou de la sauvegarde de la machine.\nFermeture du programme.")
    }
  }
}