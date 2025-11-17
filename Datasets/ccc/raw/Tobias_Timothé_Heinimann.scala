import scala.io.StdIn._
import java.io.{FileWriter,PrintWriter}
import collection.mutable.ArrayBuffer
import scala.io.Source._
object Main {
  def main(args: Array[String]): Unit = {
    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
      var numero = id
      var pin = pincode
      var lait = milk
      var sucre = sugar
      var cafe = coffee

      def addIngredient(ingredient: String, amount: Int) : Unit = {
        if(ingredient == "cafe"){
          cafe += amount
        }
        if(ingredient == "sucre"){
          sucre += amount
        }
        if(ingredient == "lait"){
          lait += amount
        }
      }//fin addingredient
      def removeIngredient(ingredient : String, amount: Int) : Boolean = {
        var validation = 1
        if(ingredient == "cafe"){
          if(amount<= cafe){
            cafe -= amount
            validation = 1
          }else{
            validation = 0
          }
        }
        if(ingredient == "sucre") {
          if (amount <= sucre) {
            sucre -= amount
            validation = 1
          } else {
            validation = 0
          }
        }
        if(ingredient == "lait") {
          if (amount <= lait) {
            lait -= amount
            validation = 1
          } else {
            validation = 0
          }
        }
        if(validation == 1){
          return true
        }else{
          return false
        }
      }

    }//fin classe

    //VALIDATE PIN-------------------------------------------------------------------------------
    def validatePin(id: Int, machines: ArrayBuffer[Machine]): Boolean = {
      var entreepin = readLine("Entrez le code pin > ")
      var essai = 2
      if (entreepin != machines(id).pin) {
        while ((essai != 0) ){
          if(entreepin == machines(id).pin){
            essai = 0
          }else{
            essai = essai - 1
            entreepin = readLine("Code PIN incorrect. " + (essai + 1) + " tentatives restantes.\n> ")
          }
        }
      }
      entreepin == machines(id).pin
    }
    //UPDATEPIN-------------------------------------------------------------------------------------
    def updatePin(id: Int, machines: ArrayBuffer[Machine]): Unit = {
      println("Mise à jour du code PIN pour la Machine "+ id)
      var NouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      var NouveauPinTaille = NouveauPin.toCharArray
      while (NouveauPinTaille.length != 6) {
        println("Votre code PIN ne contient pas exactement 6 chiffres")
        NouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        NouveauPinTaille = NouveauPin.toCharArray
      }
      machines(id).pin = NouveauPin
      println("Votre mot de passe a été mis à jour\nRetour au menu principal...")
    }

    def serveClient(id: Int, machines: ArrayBuffer[Machine]) : Boolean = {
      //Variable du paiement + TWINT
      var alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var code_twint = ""
      for (i <- 1 to 5) {
        var numero = (math.random() * alphanumerique.length).toInt
        code_twint = code_twint + alphanumerique(numero)
      }
      var choix_boisson = readLine("Quelle boisson souhaitez vous commander ?\n1) Espresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
      while ((choix_boisson < 1) || (choix_boisson > 3)) {
        choix_boisson = readLine("Option indisponible, veuillez réessayer...\nQuelle boisson souhaitez vous commander ?\n1) Espresso - CHF 2.00\n2) Cappucino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
      }
      var q_sucre = 0
      var a_sucre = "test"
      var a_lait = "test"
      var prix_sucre = 0.00
      var prix_lait = 0.00
      var prix = 0.00
      var prix_b = 0.00
      var sucre = 0
      var lait = 0
      var d_lait = 0
      var pdc = 0
      var q_lait = 0
      var coffee_avant = 0
      var lait_avant = 0
      var sugar_avant = 0
      if (choix_boisson == 1) {
        sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 >").toInt
        while ((sucre < 1) || (sucre > 4)) {
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        pdc = 8
        prix_b = 2.00
        prix = prix_b
        if ((sucre == 1)) {
          q_sucre = 0
          a_sucre = "Sans sucre"
        } else if (sucre == 2) {
          q_sucre = 5
          prix_sucre = 0.10
          prix = prix + prix_sucre
          a_sucre = "Peu (5g)"
        } else if (sucre == 3) {
          q_sucre = 10
          prix_sucre = 0.20
          prix = prix + prix_sucre
          a_sucre = "Moyen (10g)"
        } else if (sucre == 4) {
          q_sucre = 15
          prix_sucre = 0.30
          prix = prix + prix_sucre
          a_sucre = "Beaucoup (15g)"
        }
        if ((machines(id).cafe >= pdc) && (machines(id).sucre >= q_sucre)) {
          //Resumer Commande
          println("\nBoisson selectionnée : Expresso\nNiveau de sucre :" + a_sucre)
          if (sucre == 1) {
            printf("Prix total : CHF %.2f = CHF %.2f \n", prix_b, prix)
          } else {
            printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix)
          }
          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Espresso est prêt ! Bonne dégustation !")
          coffee_avant = machines(id).cafe
          sugar_avant = machines(id).sucre
          machines(id).cafe = machines(id).cafe - pdc
          machines(id).sucre = machines(id).sucre - q_sucre
        } else {
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
        //CAPPUCCINO--------------------------------------------------------------------------------------------------------
      } else if (choix_boisson == 2) {
        pdc = 6
        prix_b = 2.50
        prix = prix_b
        sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while ((sucre < 1) || (sucre > 4)) {
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        while ((lait < 1) || (lait > 2)) {
          lait = readLine("Option indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        }
        if (lait == 1) {
          d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
          if (d_lait <= 3) {
            q_lait = 100 + (d_lait * 50)
            prix = prix + (d_lait * 0.05)
            if (d_lait == 1) {
              a_lait = "1 dose (0.05L)"
              prix_lait = 0.05
            } else if (d_lait == 2) {
              a_lait = "2 doses (0.1L)"
              prix_lait = 0.10
            } else {
              a_lait = "3 doses (0.15L)"
              prix_lait = 0.15
            }
          } else {
            println("Erreur: Le nombre de doses demandés est trop élevés.")
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
          }
        } else if (lait == 2) {
          q_lait = 100
          a_lait = "Non"
        }
        if ((sucre == 1)) {
          q_sucre = 0
          a_sucre = "Sans sucre"
        } else if (sucre == 2) {
          q_sucre = 5
          prix_sucre = 0.10
          prix = prix + prix_sucre
          a_sucre = "Peu (5g)"
        } else if (sucre == 3) {
          q_sucre = 10
          prix_sucre = 0.20
          prix = prix + prix_sucre
          a_sucre = "Moyen (10g)"
        } else if (sucre == 4) {
          q_sucre = 15
          prix_sucre = 0.30
          prix = prix + prix_sucre
          a_sucre = "Beaucoup (15g)"
        }
        if ((machines(id).cafe >= pdc) && (machines(id).sucre >= q_sucre) && (machines(id).lait >= q_lait)) {
          //Resumer Commande
          if ((lait == 1) && (sucre != 1)) {
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix_lait, prix)
          } else if ((lait == 2) && (sucre != 1)) {
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_sucre, prix)
          } else if ((lait == 1) && (sucre == 1)) {
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_lait, prix)
          } else {
            println("\nBoisson selectionnée : Cappuccino\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f \n", prix)
          }

          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Cappuccino est prêt ! Bonne dégustation !")
          coffee_avant = machines(id).cafe
          sugar_avant = machines(id).sucre
          lait_avant = machines(id).lait
          machines(id).cafe = machines(id).cafe - pdc
          machines(id).sucre = machines(id).sucre - q_sucre
          machines(id).lait = machines(id).lait - q_lait
        } else {
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
      } else if (choix_boisson == 3) {
        var nom_latte = "Latte (xxx)"
        var taille = readLine("Quelle taille souahitez vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
        while ((taille < 1) || (taille > 3)) {
          taille = readLine("Taille indisponible, veuillez réessayer...\nQuelle taille souahitez vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n> ").toInt
        }
        sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        while ((sucre < 1) || (sucre > 4)) {
          sucre = readLine("Quantité indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du sucre ? \n1) sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
        }
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        while ((lait < 1) || (lait > 2)) {
          lait = readLine("Option indisponible, veuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ? \n1) Oui \n2) Non\n> ").toInt
        }
        //PETIT-----------------------------------------------------------------------------------------
        if (taille == 1) {
          pdc = 6
          prix_b = 2.70
          if (lait == 1) {
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            if (d_lait <= 3) {
              q_lait = 120 + (d_lait * 50)
              prix_b = prix_b + (d_lait * 0.05)
              if (d_lait == 1) {
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              } else if (d_lait == 2) {
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              } else {
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            } else {
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            }
          } else if (lait == 2) {
            q_lait = 120
            a_lait = "Non"
          }
          if ((sucre == 1)) {
            q_sucre = 0
            a_sucre = "Sans sucre"
          } else if (sucre == 2) {
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix_b + prix_sucre
            a_sucre = "Peu (5g)"
          } else if (sucre == 3) {
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix_b + prix_sucre
            a_sucre = "Moyen (10g)"
          } else if (sucre == 4) {
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix_b + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        } else if (taille == 2) {
          pdc = 8
          prix_b = 3.20
          prix = prix_b
          if (lait == 1) {
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            if (d_lait <= 3) {
              q_lait = 150 + (d_lait * 50)
              prix = prix_b + (d_lait * 0.05)
              if (d_lait == 1) {
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              } else if (d_lait == 2) {
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              } else {
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            } else {
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n >").toInt
            }
          } else if (lait == 2) {
            q_lait = 150
            a_lait = "Non"
          }
          if ((sucre == 1)) {
            q_sucre = 0
            a_sucre = "Sans sucre"
          } else if (sucre == 2) {
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix + prix_sucre
            a_sucre = "Peu (5g)"
          } else if (sucre == 3) {
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix + prix_sucre
            a_sucre = "Moyen (10g)"
          } else if (sucre == 4) {
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        } else if (taille == 3) {
          pdc = 6
          prix_b = 3.70
          prix = prix_b
          if (lait == 1) {
            d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toInt
            if (d_lait <= 3) {
              q_lait = 200 + (d_lait * 50)
              prix = prix + (d_lait * 0.05)
              if (d_lait == 1) {
                a_lait = "1 dose (0.05L)"
                prix_lait = 0.05
              } else if (d_lait == 2) {
                a_lait = "2 doses (0.1L)"
                prix_lait = 0.10
              } else {
                a_lait = "3 doses (0.15L)"
                prix_lait = 0.15
              }
            } else {
              println("Erreur: Le nombre de doses demandés est trop élevés.")
              d_lait = readLine("Combien de dose ? (3 doses maximum)\n> ").toInt
            }
          } else if (lait == 2) {
            q_lait = 200
            a_lait = "Non"
          }
          if ((sucre == 1)) {
            q_sucre = 0
            a_sucre = "Sans sucre"
          } else if (sucre == 2) {
            q_sucre = 5
            prix_sucre = 0.10
            prix = prix + prix_sucre
            a_sucre = "Peu (5g)"
          } else if (sucre == 3) {
            q_sucre = 10
            prix_sucre = 0.20
            prix = prix + prix_sucre
            a_sucre = "Moyen (10g)"
          } else if (sucre == 4) {
            q_sucre = 15
            prix_sucre = 0.30
            prix = prix + prix_sucre
            a_sucre = "Beaucoup (15g)"
          }
        }
        if ((machines(id).cafe >= pdc) && (machines(id).sucre >= q_sucre) && (machines(id).lait >= q_lait)) {
          if (taille == 1) {
            nom_latte = "Latte (Petit)"
          } else if (taille == 2) {
            nom_latte = "Latte (Moyen)"
          } else if (taille == 3) {
            nom_latte = "Latte (Grand)"
          }
          //Resumer Commande
          if ((lait == 1) && (sucre != 1)) {
            println("\nBoisson selectionnée : " + nom_latte + "\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", prix_b, prix_sucre, prix_lait, prix)
          } else if ((lait == 2) && (sucre != 1)) {
            println("\nBoisson selectionnée : " + nom_latte + "\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_sucre, prix)
          } else if ((lait == 1) && (sucre == 1)) {
            println("\nBoisson selectionnée : " + nom_latte + "\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prix_b, prix_lait, prix)
          } else {
            println("\nBoisson selectionnée : " + nom_latte + "\nNiveau de sucre : " + a_sucre + "\nLait en supplément : " + a_lait + "\nPrix total : CHF" + prix_b)
            printf("Prix total : CHF %.2f \n", prix)
          }

          //Paiement
          println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + code_twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("Merci ! Votre paiement a été accepté.\n")
          //Apres paiement
          println("Préparation de votre boisson...\n[...]\nVotre Cappuccino est prêt ! Bonne dégustation !")
          coffee_avant = machines(id).cafe
          sugar_avant = machines(id).sucre
          lait_avant = machines(id).lait
          machines(id).cafe = machines(id).cafe - pdc
          machines(id).sucre = machines(id).sucre - q_sucre
          machines(id).lait = machines(id).lait - q_lait
        } else {
          println("Erreur: stocks insuffisant...\nVeuillez sélectionner une autre machine")
        }
      }
      if (choix_boisson == 1) {
        (coffee_avant >= pdc) && (sugar_avant >= q_sucre)
      } else {
        (coffee_avant >= pdc) && (sugar_avant >= q_sucre) && (lait_avant >= q_lait)
      }
    }
    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val fr = fromFile(filename)
      val lignefr = fr.reset.getLines()
      val systeme = new ArrayBuffer[Machine]()
      var i = 0
      var ligne = lignefr.next()
      while(!lignefr.isEmpty){
        ligne = lignefr.next()
        var machine = ligne.split(",")
        systeme += new Machine(i, machine(0), machine(1).toDouble.toInt , machine(2).toDouble.toInt, machine(3).toDouble.toInt )
      }
      return systeme
    }
    def savecsv(filename: String, machines: ArrayBuffer[Machine]) : Unit = {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for(i<- 0 until machines.length){
        fw.println(machines(i).pin+","+machines(i).lait+","+machines(i).sucre+","+machines(i).cafe)
      }
      fw.close
    }
    try{
      val machines = loadcsv("src/machines.csv")
      println("Chargement des machines depuis machines.csv...")
      for(i<- 0 to machines.length-1){
        println("\nMachine "+ (i+1)+" chargée :\n ID: "+(i+1)+"\n Code PIN: "+machines(i).pin+"\n Lait: "+((machines(i).lait).toDouble/1000.0)+"L\n Sucre: "+machines(i).sucre+"g\n Café: "+machines(i).cafe+"g")
      }
      println("\n"+machines.length +" machine(s) chargée(s) avec succès.")
      //---------------------------------------------------------------------
      var programme = 0
      var panne = 0
      var machineId = 0
      while(programme == 0){
        panne = 0
        print("\nNospresso Café")
        var choix = readLine(" Veuillez sélectionner votre mode : \n1) Client \n2) Admin\n3) Quitter\n> ").toInt
        while((choix < 1)||(choix > 3)){
          choix = readLine("Action impossible, veuillez réessayer...\nVeuillez sélectionner votre mode : \n1) Client \n2) Admin\n3) Quitter\n> ").toInt
        }
        if(choix == 1){
          while(panne == 0){
            machineId = readLine("Machine sélectionée (1-5) >  ").toInt
            while((machineId < 1)||(machineId > 5)){
              machineId = readLine("Numéro de Machine introuvable, veuillez réessayer...\nMachine sélectionée (1-5) >  ").toInt
            }
            machineId = machineId -1
            if(serveClient(machineId, machines) == false){
            }else{
              panne = 1
            }
          }
        }else if(choix == 2){
          machineId = readLine("Machine sélectionée (1-5) >  ").toInt
          while((machineId < 1)||(machineId > 5)){
            machineId = readLine("Numéro de Machine introuvable, veuillez réessayer...\nMachine sélectionée (1-5) >  ").toInt
          }
          machineId = machineId -1
          if(validatePin(machineId, machines)== true){
            println("Accès autorisé")
            var choix_admin = readLine("1) Changer de pin\n2) Réapprovisionner le stock\n3) Débiter le stock\n > ").toInt
            while((choix_admin < 1)||(choix_admin > 3)){
              choix_admin = readLine("Action impossible, veuillez réessayer...\n1) Changer de pin\n2) Réapprovisionner le stock\n > ").toInt
            }
            if(choix_admin == 1){
              updatePin(machineId, machines)
            }else if(choix_admin == 2){
              var ingredient = readLine("Quel ingrédient souhaitez vous réapprovisionner ?\ncafe\nsucre\nlait\n> ")
              var amount = readLine("Combien de g (ml) souhaitez vous ajouter ?\n> ").toInt
              machines(machineId).addIngredient(ingredient, amount)
            }else if(choix_admin == 3){
              var ingredient = readLine("Quel ingrédient souhaitez vous réapprovisionner ?\ncafe\nsucre\nlait\n> ")
              var amount = readLine("Combien de g (ml) souhaitez vous ajouter ?\n> ").toInt
              machines(machineId).removeIngredient(ingredient, amount)
            }
          }else{
            println("Trop de tentatives échouées. Fin du programme.")
            programme = 1
          }
        }else if(choix == 3){
          programme = 1
          savecsv("machines.csv", machines)
          println("Sauvegarde de "+(machines.length) +" machines dans machines.csv...\nFichier sauvegardé avec succès.")
        }
      }
    }//try
    catch{
      case ex : java.io.FileNotFoundException => println("Erreur: Fichier introuvable. Vérifier la chemin d'accès et réessayez.")
      case ex : java.io.IOException => println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      case ex : java.nio.file.AccessDeniedException => println("Erreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule")
    }
  }
}