import scala.io.StdIn._
import scala.io.Source
import java.io.{PrintWriter,FileWriter}
import scala.collection.mutable.ArrayBuffer

object Main {

    var code = 0
    var loop = true
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val length = 5
    var i = 0
    var twint = ""
    var prixE =2.00
    var prixC =2.50
    var prixLp =2.70
    var prixLm =3.20
    var prixLg =3.70
    var prixSp =0.10
    var prixSm =0.20
    var prixSb =0.30
    var prixDl =0.05
    var laitBool = false
    var boisson =""
    var prixTot = 0.0
    var tentative = 3
    var loaded = true

    var index = 1
    val machines = ArrayBuffer[Machine]()

    object CSV {
      var total = 0

      def loadcsv(filename: String): ArrayBuffer[Machine] = {
        try {
          val file = Source.fromFile(filename)
          val lignes = file.getLines()
          var ignorerentete = 0
          total = 0
          for (ligne <- lignes) {
            if (ignorerentete > 0){
            val cellulle = ligne.split(",")
            val pincode = cellulle(0)
            val milk = cellulle(1).toInt
            val sugar = cellulle(2).toInt
            val coffee = cellulle(3).toInt
            machines.append(new Machine(index, pincode, milk, sugar, coffee))
            index += 1
            total += 1
            }
          ignorerentete += 1
        }
          file.close()
        } catch {
          case _: Exception => println("Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
            machines+=new Machine(0,"error",0,0,0)
        }
        machines
      }

      def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
//        if (machines.isEmpty) {
//          println("Erreur : Aucune donnée à sauvegarder. Le fichier existant ne sera pas écrasé.")
//          return
//        }
        try {
          val writer = new PrintWriter(new FileWriter(filename))
          writer.println("PINCODE,MILK,SUGAR,COFFEE")
          for (machine <- machines) {
            val line = machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee
            writer.println(line)
          }
          writer.close()
          println("Fichier sauvegardé avec succès.")
        } catch {
          case _: Exception =>
            println("Erreur : Impossible d'écrire dans le fichier. Vérifiez les autorisations.")
        }
      }
    }

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") {
        milk += amount
      }  else if (ingredient == "coffee") {
        coffee += amount
      }  else if (ingredient == "sugar") {
        sugar += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if (milk >= amount && ingredient == "milk" ) {
        milk -= amount
        true
      } else if (sugar >= amount  && ingredient == "sugar") {
        sugar -= amount
        true
      } else if (coffee >= amount && ingredient == "coffee") {
        coffee -= amount
        true
      } else {
        false
      }
    }
  }



  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
      code = readLine("Mode Admin\nEntrez le code PIN :\n>").toInt
      val machine = machines(machineId-1)
      while (tentative != 0) {
        if (code == machine.pincode.toInt) {
          println("Accès accordé à la Machine "+machineId+".")
          return true
        } else {
          tentative -= 1
          if (tentative > 0) {
            println("Code PIN incorrect. "+tentative+" tentatives restantes.")
            code = readLine(">").toInt
          } else {
            println("Code PIN incorrect. "+tentative+" tentatives restantes.")
            println("\nTrop de tentatives échouées. Fin du programme.")
            loop = false
          }
        }
      }
    false
  }



  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit ={
    var valide = false
    val machine = machines(machineId-1)
    println("Mise à jour du code PIN pour la Machine "+machineId+".")
    while (!valide) {
      var newCode = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
      if  (newCode.length !=6 ){
        newCode = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
      }
      else {
        println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
        valide = true
        machine.pincode = newCode
      }
    }
  }


  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var cafe = 0
    while (cafe < 1 || cafe > 3) {
      cafe = readLine("Veuillez saisir le numéro correspondant à votre café:\n1) Expresso - CHF "+prixE+"0\n2) Cappuccino - CHF "+prixC+"0\n"
        +"3) Latte - CHF "+prixLp+"0 (petit), CHF "+prixLm+"0 (moyen), CHF "+prixLg+"0 (grand)\n>").toByte
      if (cafe < 1 || cafe  > 3) {
        println("Erreur : Veuillez saisir un choix valide (1, 2 ou 3).")
      }
    }
    val machine = machines(machineId - 1)
    if (cafe == 1) {
      if (machine.coffee >= 8) {
        boisson = "Expresso"
        machine.removeIngredient("coffee",8)
        prixTot += 2.00
      } else {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer l'Expresso.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        return false
      }
    }

    if (cafe == 2) {
      if (machine.coffee >= 6 && machine.milk >= 100) {
        boisson = "Cappuccino"
        machine.removeIngredient("coffee",8)
        machine.removeIngredient("milk",100)
        prixTot += 2.50
      } else {
        println("Erreur : Quantité insuffisante pour préparer le Cappuccino.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        return false
      }
    }

    if (cafe == 3) {
      var taille = 0
      while (taille < 1 || taille > 3) {
        taille = readLine("Personalisation de la boisson:\nVeuillez saisir le numéro correspondant au format:\n1) Petit\n2) Moyen\n3) Grand\n>").toByte
        if (taille < 1 || taille > 3) {
          println("Erreur : Choix de taille invalide. Veuillez entrer 1, 2 ou 3.")
        }
      }

      if (taille == 1 && machine.coffee >= 6 && machine.milk >= 120) {
        boisson = "Latte (Petit)"
        machine.removeIngredient("coffee",6)
        machine.removeIngredient("milk",120)
        prixTot += 2.70
      } else if (taille == 2 && machine.coffee >= 8 && machine.milk >= 150) {
        boisson = "Latte (Moyen)"
        machine.removeIngredient("coffee",8)
        machine.removeIngredient("milk",150)
        prixTot += 3.20

      } else if (taille == 3 && machine.coffee >= 12 && machine.milk >= 200) {
        boisson = "Latte (Grand)"
        machine.removeIngredient("coffee",12)
        machine.removeIngredient("milk",200)
        prixTot += 3.70

      } else {
        println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        return false
      }
    }

    var sucre = 0
    while (sucre < 1 || sucre > 4) {
      sucre = readLine("Souhaitez-vous ajouter du sucre ?\n" +
        "1) Sans sucre \n2) Peu (5g) - CHF "+prixSp+"0\n3) Moyen (10g) - CHF "+prixSm+"0\n4) Beaucoup (15g) - CHF "+prixSb+"0\n>").toByte
      if (sucre < 1 || sucre > 4) {
        println("Erreur :  Veuillez saisir une valeur entre 1 et 4.")
      }
    }

    if (sucre == 2 && machine.sugar >= 5) {
      machine.removeIngredient("sugar",5)
      prixTot += 0.10
    } else if (sucre == 3 && machine.sugar >= 10) {
      machine.removeIngredient("suger",10)
      prixTot += 0.20
    } else if (sucre == 4 && machine.sugar >= 15) {
      machine.removeIngredient("sugar",15)
      prixTot += 0.30
    } else if (sucre == 2 || sucre == 3 || sucre == 4 ) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
      return false
    }

    if ((cafe == 2 || cafe == 3) && (machine.sugar > 0) && laitBool){
      var choixLait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toByte
      var lait = 0
      while (lait < 1 || lait > 3) {
        lait = readLine("Combien de dose ?\n>").toByte
        if (choixLait == 1){
          println("Erreur : Vous pouvez ajouter jusqu'à 3 doses maximum. Veuillez saisir une valeur entre 1 et 3.")
        }
      }
      val dose = lait * 50
      if (machine.milk >= dose) {
        machine.removeIngredient("milk",dose.toInt)
        laitBool = true
        prixTot += prixDl * lait
      } else {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        laitBool=false
        return false
      }
    }
    var i = 0
    while (i < length) {
      val nombre = (math.random * 36).toInt // nombre de caractères dans mon string
      val alphaN = caracteres(nombre)
      twint += alphaN
      i += 1
    }
    printf("Prix total : %.2f CHF\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : ", prixTot)
    println(twint)
    println("(En attente de validation du paiement...)\n")
    Thread.sleep(3000)
    println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...\n[...]\nVotre "+boisson+" est prêt ! Bonne dégustation !\n")
    true
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    println("Niveaux de stock actuels :")
    println("Poudre de café : "+ machines(machineId).coffee + "g")
    println("Sucre : "+machines(machineId).sugar+" g")
    printf("Milk: " + (machines(machineId).milk.toDouble * 0.001) + "L")

    println("\nEntrez les quantités à ajouter :")
    machines(machineId).addIngredient("milk", readLine("Quantité de poudre de café à ajouter (en g):\n>").toInt)
    machines(machineId).addIngredient("sugar", readLine("Quantité de sucre à ajouter (en g):\n>").toInt)
    machines(machineId).addIngredient("coffee", (readLine("Quantité de lait à ajouter (en mL):\n>").toDouble * 1000).toInt)

    println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
  }


  def main(args: Array[String]): Unit = {
    CSV.loadcsv("machines.csv")
    if(machines(0).pincode!="error") {
      println("Chargement des machines depuis machines.csv...")
      for (i <- machines.indices) {
        val machine = machines(i)
        println("Machine " + (i+1) + " chargée :\n   ID: " + (i+1) + "\n   Code Pin: " + machine.pincode + "\n   Lait: " + machine.milk + "L")
        println("   Sucre: " + machine.sugar + "g\n   Café: " + machine.coffee + "g")
      }
      println(CSV.total + " machine(s) chargée(s) avec succès")
      while(loop){

        var choix = readLine("\n        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte
        while (choix < 1 || choix > 3) {
          println("Erreur : Veuillez saisir un choix valide (1, 2 ou 3).")
          choix = readLine("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte
        }
        var machineId = 0
        if (choix!=3) {
          machineId = readLine("Veuillez sélectionner une machine (1 à " + machines.size + ") :\n>").toInt
          while (machineId < 1 || machineId > CSV.total) {
            println("La machine sélectionnée n'existe pas, sélectionnez une machine existante (entre 1 "+machines.size+").")
            machineId = readLine("Veuillez sélectionner une machine (1 à " + machines.size + ") :\n>").toInt
          }
        }

        if (choix==1){
          serveClient(machineId,machines)
        }
        else if (choix ==2){
          if (validatePin(machineId, machines)) {
            val choixAdmin = readLine("1) Mise à jour du code PIN \n2) Réapprovisionnement\n>").toInt
            if (choixAdmin == 1) {
              updatePin(machineId, machines)
            } else if (choixAdmin == 2) {
              println("Réapprovisionnement...")
              restockMachine(machineId-1, machines)
            }
          }
        }
        else{
          println("Sauvegarde de "+CSV.total+" machines dans machines.csv...")
          loop = false
        }
      }
      CSV.savecsv("machines.csv",machines)
    }
    else {
      println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }
}