import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {


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

    val coffeeStocks = Array.fill(5)(50)
    val sugarStocks = Array.fill(5)(30)
    val milkStocks = Array.fill(5)(0.500)

    var caferestock = 0
    var sucrerestock = 0
    var laitrestock = 0.0

    val nbMachines = 5
    val machineIds: Array[Int] = Array.tabulate(nbMachines)(i=>i+1)
    var machinePins: Array[Int]= Array.fill(nbMachines)(434343)

    def validatePin(machineId: Int, machinePins: Array[Int]): Boolean = {
      code = readLine("Mode Admin\nEntrez le code PIN :\n>").toInt
      while (tentative != 0) {
        if (code == machinePins(machineId-1)) {
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



    def updatePin(machineId: Int, machinePins: Array[Int]): Unit ={
      var valide = false
      println("Mise à jour du code PIN pour la Machine "+machineId+".")
      while (!valide) {
        var newCode = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
        if  (newCode.length !=6 ){
          newCode = readLine("Entrez un nouveau code PIN à 6 chiffres\n>")
        }
        else {
          println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
          valide = true
          machinePins(machineId-1) = newCode.toInt
        }
      }
    }

    def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
      var cafe = 0
      while (cafe < 1 || cafe > 3) {
        cafe = readLine("Veuillez saisir le numéro correspondant à votre café:\n1) Expresso - CHF "+prixE+"0\n2) Cappuccino - CHF "+prixC+"0\n"
          +"3) Latte - CHF "+prixLp+"0 (petit), CHF "+prixLm+"0 (moyen), CHF "+prixLg+"0 (grand)\n>").toByte
        if (cafe < 1 || cafe  > 3) {
          println("Erreur : Veuillez saisir un choix valide (1, 2 ou 3).")
        }
      }

      if (cafe == 1) {
        if (coffeeStocks(machineId-1) >= 8) {
          boisson = "Expresso"
          coffeeStocks(machineId-1) -= 8
          prixTot += 2.00
        } else {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer l'Expresso.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
          return false
        }
      }

      if (cafe == 2) {
        if (coffeeStocks(machineId-1) >= 6 && milkStocks(machineId-1) >= 0.100) {
          boisson = "Cappuccino"
          coffeeStocks(machineId-1) -= 6
          milkStocks(machineId-1) -= 0.100
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

        if (taille == 1 && coffeeStocks(machineId-1) >= 6 && milkStocks(machineId-1) >= 0.120) {
          boisson = "Latte (Petit)"
          coffeeStocks(machineId-1) -= 6
          milkStocks(machineId-1) -= 0.120
          prixTot += 2.70
        } else if (taille == 2 && coffeeStocks(machineId-1) >= 8 && milkStocks(machineId-1) >= 0.150) {
          boisson = "Latte (Moyen)"
          coffeeStocks(machineId-1) -= 8
          milkStocks(machineId-1) -= 0.150
          prixTot += 3.20

        } else if (taille == 3 && coffeeStocks(machineId-1) >= 12 && milkStocks(machineId-1) >= 0.200) {
          boisson = "Latte (Grand)"
          coffeeStocks(machineId-1) -= 12
          milkStocks(machineId-1) -= 0.200
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

      if (sucre == 2 && sugarStocks(machineId-1) >= 5) {
        sugarStocks(machineId-1) -= 5
        prixTot += 0.10
      } else if (sucre == 3 && sugarStocks(machineId-1) >= 10) {
        sugarStocks(machineId-1) -= 10
        prixTot += 0.20
      } else if (sucre == 4 && sugarStocks(machineId-1) >= 15) {
        sugarStocks(machineId-1) -= 15
        prixTot += 0.30
      } else if (sucre == 2 || sucre == 3 || sucre == 4 ) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
        return false
      }

      if ((cafe == 2 || cafe == 3) && (sugarStocks(machineId-1) > 0) && laitBool){
        var choixLait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toByte
        var lait = 0
        while (lait < 1 || lait > 3) {
          lait = readLine("Combien de dose ?\n>").toByte
          if (choixLait == 1){
            println("Erreur : Vous pouvez ajouter jusqu'à 3 doses maximum. Veuillez saisir une valeur entre 1 et 3.")
          }
        }
        val dose = lait * 0.05
        if (milkStocks(machineId-1) >= dose) {
          milkStocks(machineId-1) -= dose.toInt
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

    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Unit={
      println("Niveaux de stock actuels :")
      println("Poudre de café : "+coffeeStocks(machineId-1)+ "g")
      println("Sucre : "+sugarStocks(machineId-1)+" g")
      printf("Lait : %.2f L",milkStocks(machineId-1))

      println("\nEntrez les quantités à ajouter :")
      caferestock = readLine("Quantité de poudre de café à ajouter (en g):\n>").toInt
      coffeeStocks(machineId-1) += caferestock

      sucrerestock = readLine("Quantité de sucre à ajouter (en g):\n>").toInt
      sugarStocks(machineId-1) += sucrerestock

      laitrestock = readLine("Quantité de lait à ajouter (en L):\n>").toDouble
      milkStocks(machineId-1) += laitrestock

      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
    }


    while(loop){

      var machineId = readLine("Veuillez sélectionner une machine (1 à 5) :\n>").toInt
      while (machineId < 1 || machineId > nbMachines) {
        println("La machine sélectionnée n'existe pas, sélectionnez une machine existante (entre 1 et 5).")
        machineId = readLine("Veuillez sélectionner une machine (1 à 5) :\n>").toInt
      }

      var choix = readLine("        Nospresso Café num°"+machineId+"\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte
      while (choix < 1 || choix > 3) {
        println("Erreur : Veuillez saisir un choix valide (1, 2 ou 3).")
        choix = readLine("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toByte
      }

      if (choix==1){
        serveClient(machineId,coffeeStocks,sugarStocks, milkStocks)
      }
      else if (choix ==2){
        if (validatePin(machineId, machinePins)) {
          val choixAdmin = readLine("1) Mise à jour du code PIN \n2) Réapprovisionnement\n>").toInt
          if (choixAdmin==1) {
            updatePin(machineId, machinePins)
          }else if (choixAdmin==2){
            restockMachine(machineId-1, coffeeStocks, sugarStocks, milkStocks)
          }
        }
      }
      else{
        println("Vous quittez Nospresso Café. Merci et à bientôt !")
        loop = false
      }
    }
  }
}