import io.StdIn._
import scala.io.Source._
import collection.mutable.ArrayBuffer
import java.io.{FileWriter, PrintWriter}



object Main {
  def main(args: Array[String]): Unit = {

  // class machine avec def addIngredient et def removeIngredient
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    var machineId = id
    var PIN = pincode
    var Milk = milk
    var Sugar = sugar
    var Coffee = coffee

    //def pour ajouter les ingrédients
    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "Café") {
        Coffee += amount
      } else if (ingredient == "Lait") {
        Milk += amount
      } else if (ingredient == "Sucre") {
        Sugar += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if ((ingredient == "Café") && (amount <= Coffee) ) {
      Coffee -= amount
      true
    } else if ((ingredient == "Lait") && (amount <= Milk) ) {
      Milk -= amount
      true
    } else if ((ingredient == "Sucre") && (amount <= Sugar) ) {
      Sugar -= amount
      true
    } else {
      false
    }

    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var codepin = readLine("Entrez le code PIN :\n> ")
    var tentatives = 0
    if (codepin == machines(machineId).PIN) {
    } else {
      while (tentatives != 2) {
        if (codepin != machines(machineId).PIN) {
          tentatives = tentatives + 1
          codepin = readLine("Code PIN incorrect. " + (3 - tentatives) + " tentatives restantes.\n > ")
        } else {
          tentatives = 3
        }
      }
    }
    codepin == machines(machineId).PIN
  }

    def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
      println("Mise à jour du code PIN pour la machine " + (machineId) + "\nVeuillez entrer un nouveau code PIN à 6 chiffres.")
      var newpin = readLine("> ")
      while (newpin.length != 6) {
        println("Veuillez entre un nouveau code PIN d'une longueur de 6 chiffres !!")
        newpin = readLine("> ")
      }
      machines(machineId).PIN = newpin
      println("Le nouveau code PIN a été mis à jour avec succès !")
      println("Retour au menu principal...")
    }



    def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var machine = 0
    var choixcafe = 0
    var affichage = 1
    var mode = 0

    var boisson = "café"
    var ajsucre = "avec"
    var ajlait = "combien de doses"

    var boissonprix = 0.00
    var prixfinal = 0.00
    var ajsucreprix = 0.00
    var ajlaitprix = 0.00
    var choixsucre = 0
    var choixlait = 0
    var quantitesucre = 0
    var doselait = 0
    var quantitelait = 0
    var PCcafe = 0
    var laitcafe = 0
    var pc0 = 0
    var lait0 = 0
    var sucre0 = 0
    var indispo = 0

    choixcafe = readLine("\nMachine séléctionnée (1-5) > " + (machineId +1) + "\nVeuillez sélectionné votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n > ").toInt
    do {
      if ((choixcafe == 1) || (choixcafe == 2) || (choixcafe == 3)) {
        indispo = 1
      } else {
        choixcafe = readLine("Option de café indisponible.\nVeuillez réessayer...\n Veuillez sélectionné votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n >").toInt
      }
    } while (indispo == 0)

    if(choixcafe == 1) {
      PCcafe = 8
      indispo = 0
      boissonprix = 2.00
      prixfinal = boissonprix
      boisson = "Expresso"
      choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
      do {
        if ((choixsucre == 1) || (choixsucre == 2) || (choixsucre == 3) || (choixsucre == 4)) {
          indispo = 1
        } else {
          choixsucre = readLine("Quantité de sucre demandé indisponible.\nVeuillez réessayer...\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
        }
      } while (indispo == 0)
      if (choixsucre == 1) {
        quantitesucre = 0
        ajsucre = "Sans sucre"

      } else if (choixsucre == 2) {
        quantitesucre = 5
        ajsucreprix = 0.10
        ajsucre = "Peu (5g)"
        prixfinal += ajsucreprix

      } else if (choixsucre == 3) {
        quantitesucre = 10
        ajsucreprix = 0.20
        ajsucre = "Moyen (10g)"
        prixfinal += ajsucreprix

      } else if (choixsucre == 4) {
        quantitesucre = 15
        ajsucreprix = 0.30
        ajsucre = "Beaucoup (15g)"
        prixfinal += ajsucreprix
      }

      //Si client choisi Cappuccino
    } else if(choixcafe == 2) {
      PCcafe = 6
      indispo = 0
      var laitcapuccino = 100
      var boissonprix = 2.50
      prixfinal = boissonprix
      boisson = "Capuccino"
      choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
      do {
        if ((choixsucre == 1) || (choixsucre == 2) || (choixsucre == 3) || (choixsucre == 4)) {
          indispo = 1
        } else {
          choixsucre = readLine("Quantité de sucre demandé indisponible.\nVeuillez réessayer...\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
        }
      } while (indispo == 0)
      indispo = 0
      choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt
    do {
      if ((choixlait == 1) || (choixlait == 2)) {
        indispo = 1
      } else {
        choixlait = readLine("Quantité de lait indisponible.\nVeuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt
      }
    }while (indispo == 0)
        indispo = 0

      //Choix du lait à ajouter (même chose uniquement Cappucino et Latte)
      if (choixlait == 1) {
        doselait = readLine("Combien de dose ?\n > ").toInt

        if (doselait == 1) {
          quantitelait = 100 + 50
          ajlait = "1 dose de lait (0.05L)"
          ajlaitprix = 0.05 //CHF
          prixfinal += ajlaitprix

        } else if (doselait == 2) {
          quantitelait = 100 + 100
          ajlait = "2 dose de lait (0.1L)"
          ajlaitprix = 0.10 //CHF
          prixfinal += ajlaitprix

        } else if (doselait == 3) {
          quantitelait = 100 + 150
          ajlait = "3 dose de lait (0.15L)"
          ajlaitprix = 0.15 //CHF
          prixfinal += ajlaitprix

        } else {
          println("Erreur : La quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
          println("Retour au menu principal...")
          return false
        }

      } else {
        quantitelait = 100
        ajlait = "Non"
      }
      //Choix de sucre
      if (choixsucre == 1) {
        quantitesucre = 0
        ajsucre = "Sans sucre"

      } else if (choixsucre == 2) {
        quantitesucre = 5
        ajsucreprix = 0.10
        ajsucre = "Peu (5g)"
        prixfinal += ajsucreprix

      } else if (choixsucre == 3) {
        quantitesucre = 10
        ajsucreprix = 0.20
        ajsucre = "Moyen (10g)"
        prixfinal += ajsucreprix

      } else if (choixsucre == 4) {
        quantitesucre = 15
        ajsucreprix = 0.30
        ajsucre = "Beaucoup (15g)"
        prixfinal += ajsucreprix
      }

      //Si client choisi Latte
    } else if (choixcafe == 3) {
         var taille = readLine("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n > ").toInt

         do {
           if ((taille == 1) || (taille == 2) || (taille == 3)) {
             indispo = 1
           } else {
             taille = readLine("Taille choisie est indisponible.\nVeuillez réessayer...\nVeuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n > ").toInt
           }
         } while (indispo == 0)
         indispo = 0
         choixsucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt

         do {
           if ((choixsucre == 1) || (choixsucre == 2) || (choixsucre == 3) || (choixsucre == 4)) {
             indispo = 1
           } else {
             choixsucre = readLine("Quantité de sucre demandé indisponible.\nVeuillez réessayer...\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n > ").toInt
           }
         } while (indispo == 0)
         indispo = 0
         choixlait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt

         do {
           if ((choixlait == 1) || (choixlait == 2)) {
             indispo = 1
           } else {
             choixlait = readLine("Quantité de lait indisponible.\nVeuillez réessayer...\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n > ").toInt
           }
         }while (indispo == 0)
             indispo = 0


           //Choix des différentes tailles pour le Latte (petit, moyen ou grand)
           //Si taille du Latte --> Petit
           if (taille == 1) {
             var PCcafe = 6
             var laitpetitlatte = 0.120
             boisson = "Latte (Petit)"
             boissonprix = 2.70
             prixfinal = boissonprix

             //Choix du lait à ajouter (même chose uniquement pour Capuccino et Latte)
             if (choixlait == 1) {
               doselait = readLine("Combien de dose ?\n > ").toInt
               if (doselait == 1) {
                 quantitelait = 120 + 50
                 ajlait = "1 dose de lait (0.05L)"
                 ajlaitprix = 0.05
                 prixfinal += ajlaitprix

               } else if (doselait == 2) {
                 quantitelait = 120 + 100
                 ajlait = "2 dose de lait (0.1L)"
                 ajlaitprix = 0.10
                 prixfinal += ajlaitprix

               } else if (doselait == 3) {
                 quantitelait = 120 + 150
                 ajlait = "3 dose de lait (0.15L)"
                 ajlaitprix = 0.15
                 prixfinal += ajlaitprix

               } else {
                 println("Erreur : Le nombre de dose demandées est trop élevé.")
                 doselait = readLine("Combien de dose ? (3 doses maximum)\n > ").toInt
               }

             } else {
               quantitelait = 120
               ajlait = "Non"
             }
             if (choixsucre == 1) {
               quantitesucre = 0
               ajsucre = "Sans sucre"

             } else if (choixsucre == 2) {
               quantitesucre = 5
               ajsucreprix = 0.10
               ajsucre = "Peu (5g)"
               prixfinal += ajsucreprix

             } else if (choixsucre == 3) {
               quantitesucre = 10
               ajsucreprix = 0.20
               ajsucre = "Moyen (10g)"
               prixfinal += ajsucreprix

             } else if (choixsucre == 4) {
               quantitesucre = 15
               ajsucreprix = 0.30
               ajsucre = "Beaucoup (15g)"
               prixfinal += ajsucreprix
             }
           }

           //Si client choisi taille 2
           else if (taille == 2) {
             var PCcafe = 8
             var laitmoyenlatte = 150
             boisson = "Latte (Moyen)"
             boissonprix = 3.20
             prixfinal = boissonprix
             if (choixlait == 1) {
               doselait = readLine("Combien de dose ?\n > ").toInt
               if (doselait == 1) {
                 quantitelait = 150 + 50
                 ajlait = "1 dose de lait (0.05L)"
                 ajlaitprix = 0.05
                 prixfinal += ajlaitprix

               } else if (doselait == 2) {
                 quantitelait = 150 + 100
                 ajlait = "2 dose de lait (0.1L)"
                 ajlaitprix = 0.10
                 prixfinal += ajlaitprix

               } else if (doselait == 3) {
                 quantitelait = 150 + 150
                 ajlait = "3 dose de lait (0.15L)"
                 ajlaitprix = 0.15
                 prixfinal += ajlaitprix

               } else {
                 println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                 println("Combien de dose ? (3 doses maximum)\n > ")
               }
             } else {
               quantitelait = 150
               ajlait = "Non"
             }
             if (choixsucre == 1) {
               quantitesucre = 0
               ajsucre = "Sans sucre"

             } else if (choixsucre == 2) {
               quantitesucre = 5
               ajsucreprix = 0.10
               ajsucre = "Peu (5g)"
               prixfinal += ajsucreprix

             } else if (choixsucre == 3) {
               quantitesucre = 10
               ajsucreprix = 0.20
               ajsucre = "Moyen (10g)"
               prixfinal += ajsucreprix

             } else if (choixsucre == 4) {
               quantitesucre = 15
               ajsucreprix = 0.30
               ajsucre = "Beaucoup (15g)"
               prixfinal += ajsucreprix
             }

             //Si taille du Latte --> Grand (même chose encore une fois)
           } else if (taille == 3) {
             var PCcafe = 12
             var laitgrandlatte = 200
             boisson = "Latte (Grand)"
             boissonprix = 3.70
             prixfinal = boissonprix
             if (choixlait == 1) {
               doselait = readLine("Combien de dose ?\n > ").toInt

               if (doselait == 1) {
                 quantitelait = 200 + 50
                 ajlait = "1 dose de lait (0.05L)"
                 ajlaitprix = 0.05
                 prixfinal += ajlaitprix

               } else if (doselait == 2) {
                 quantitelait = 200 + 100
                 ajlait = "2 dose de lait (0.1L)"
                 ajlaitprix = 0.10
                 prixfinal += ajlaitprix

               } else if (doselait == 3) {
                 quantitelait = 200 + 150
                 ajlait = "3 dose de lait (0.15L)"
                 ajlaitprix = 0.15
                 prixfinal += ajlaitprix

               } else {
                 println("Erreur : Quantité de lait disponible n'est pas suffisante pour le nombre de dose demandés.")
                 println("Combien de doses ? (3 doses maximum)\n > ")
               }
             } else {
               quantitelait = 200
               ajlait = "Non"
             }
             if (choixsucre == 1) {
               quantitesucre = 0
               ajsucre = "Sans sucre"
             } else if (choixsucre == 2) {
               quantitesucre = 5
               ajsucreprix = 0.10
               ajsucre = "Peu (5g)"
               prixfinal += ajsucreprix
             } else if (choixsucre == 3) {
               quantitesucre = 10
               ajsucreprix = 0.20
               ajsucre = "Moyen (10g)"
               prixfinal += ajsucreprix
             } else if (choixsucre == 4) {
               quantitesucre = 15
               ajsucreprix = 0.30
               ajsucre = "Beaucoup (15g)"
               prixfinal += ajsucreprix
             }
           }
         }

    //Résumé de la commande pour chaque scénario de possible de commande
      //Si choix du café est Esspresso
    if (choixcafe == 1) {
      if ((machines(machineId).Coffee >= PCcafe) && (machines(machineId).Sugar >= quantitesucre)) {

        affichage = 0

        if (choixsucre == 1) {
          println("\nBoisson sélectionnée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
        } else {
          println("\nBoisson sélectionnée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
        }
        pc0 = machines(machineId).Coffee
        sucre0 = machines(machineId).Sugar
        machines(machineId).Coffee = machines(machineId).Coffee - PCcafe
        machines(machineId).Sugar = machines(machineId).Sugar - quantitesucre
      } else {
        println("Erreur : Stocks insuffisant...\nVeuillez séléctionner une autre machine (1-5)")
        affichage = 1
      }

    } else {
      //Si le choix du café est pour Capuccino ou Latte (tout sauf Esspresso)
      if ((machines(machineId).Coffee >= PCcafe) && (machines(machineId).Sugar >= quantitesucre) && (machines(machineId).Milk >= quantitelait)) {

        affichage = 0

        if ((choixlait == 1) && (choixsucre != 1)) {
          println("\nBoisson sélectionée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          println("Lait en supplément : " + ajlait)
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, ajlaitprix, prixfinal)
        } else if ((choixlait == 2) && (choixsucre != 1)) {
          println("\nBoisson sélectionée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          println("Lait en supplément : " + ajlait)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajsucreprix, prixfinal)
        } else if ((choixlait == 1) && (choixsucre == 1)) {
          println("\nBoisson sélectionée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          println("Lait en supplément : " + ajlait)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", boissonprix, ajlaitprix, prixfinal)
        } else {
          println("\nBoisson sélectionée : " + boisson)
          println("Niveau de sucre : " + ajsucre)
          println("Lait en supplément : " + ajlait)
          printf("Prix total : CHF %.2f = CHF %.2f \n", boissonprix, prixfinal)
        }

        pc0 = machines(machineId).Coffee
        sucre0 = machines(machineId).Sugar
        lait0 = machines(machineId).Milk
        machines(machineId).Coffee = machines(machineId).Coffee - PCcafe
        machines(machineId).Sugar = machines(machineId).Sugar - quantitesucre
        machines(machineId).Milk = machines(machineId).Milk - quantitelait
      } else {
        println("Erreur : Stocks insuffisant...\nVeuillez séléctionner une autre machine (1-5)")
        affichage = 1
      }
    }

      //Paiement de la boisson avec Twint et génération du code alphanumérique
      var alphanumérique = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      var codetwint = ""
      for (i <- 1 to 5) {
        var index = (math.random() * 62).toInt
        codetwint += alphanumérique(index)
      }

      //Affichage correcte paiement Twint
      if (affichage == 0) {
      println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.\n")
      println("Préparation de votre boisson...\n[...]\nVotre " + boisson + " est prêt ! Bonne dégustation !")
    }
    if (choixcafe == 1) {
      (pc0 >= PCcafe) && (sucre0 >= quantitesucre)
    } else {
      (pc0 >= PCcafe) && (sucre0 >= quantitesucre) && (lait0 >= quantitelait)
    }

  }


    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
      val pw = new PrintWriter(filename)
      pw.println("PINCODE,MILK,SUGAR,COFFEE")
      for(i <- 0  until  machines.length) {
        pw.println(machines(i).PIN + "," + machines(i).Milk + "," + machines(i).Sugar + "," + machines(i).Coffee)
      }
      pw.close
    }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val ff = fromFile(filename)
    val ffline = ff.reset.getLines()
    val actif = new ArrayBuffer[Machine]()
    var i = 0
    var line = ffline.next()
    while(!ffline.isEmpty) {
      line = ffline.next()
      var machine = line.split(",")
      actif += new Machine(i, machine(0).toString, machine(1).toInt, machine(2).toInt, machine(3).toInt)
    }
    return actif
  }

    //Chargement des machines du fichier csv au démarrage
  try {
    println("Chargements des machines depuis machines.csv...")
    println("")
    val fichier = loadcsv("machines.csv")
    for (i <- 0 to fichier.length - 1) {
      println("Machine " + (i + 1) + " chargée : ")
      println(" ID: " + (i + 1))
      println(" Code PIN: " + fichier(i).PIN)
      println(" Lait: " + ((fichier(i).Milk).toDouble / 1000.0) + "L")
      println(" Sucre: " + fichier(i).Sugar + "g")
      println(" Café: " + fichier(i).Coffee + "g")
      println("")
      println((i + 1) + " machine(s) chargée(s) avec succès.")
      println("")
    }



    //Affichage d'acceuil de la machine
    var mode = 0
    var machineId = 0
    var mistake = 0
    var machine = 0
    var indispo = 0
    while (machine == 0) {
      mistake = 0
      indispo = 0
      println("\nNospresso café")
      var mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n > ").toInt

      do {
        if ((mode == 1) || (mode == 2) || (mode == 3)) {
          indispo = 1
        } else {
          mode = readLine("Action impossible.\nVeuillez réessayer...\nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n > ").toInt
        }
      } while (indispo == 0)

      //Choix des modes par l'utilisateur

      //Si utilisateur choisi mode client
      if (mode == 1) {
        while (mistake == 0) {
          machineId = readLine("Machine sélctionnée (1-5) > ").toInt
          while ((machineId < 1) || (machineId > fichier.length)) {
            machineId = readLine("Machine sélectionnée introuvable.\nVeuillez réessayer avec un machine existante...\nMachine sélectionnée (1-5) > ").toInt
          }
          machineId -= 1
          if (serveClient(machineId, fichier) == true) {
            mistake = 1
          } else {
          }
        }


        //Si utilisateur choisi mode admin
      } else if (mode == 2) {
        machineId = readLine("Machine sélctionnée (1-5) > ").toInt
        while ((machineId < 1) || (machineId > fichier.length)) {
          machineId = readLine("Machine sélectionnée introuvable.\nVeuillez réessayer avec un machine existante...\nMachine sélectionnée (1-5) > ").toInt
        }
        machineId -= 1
        if (validatePin(machineId, fichier) == false) {
          println("Trop de tentatives échouées.\nFin du programme.")
          machine = 1
        } else {
          println("Accès autorisé !")
          indispo = 0
          var adminchoix = readLine("1) Modifier le PIN de la machine " + (machineId) + "\n2) Réapprovisionner les stocks de la machine " + (machineId) + "\n3) Retirer un stock de la machine " + (machineId) + "\n > ").toInt
          do {
            if ((adminchoix == 1) || (adminchoix == 2) || (adminchoix == 3)) {
              indispo = 1
            } else {
              adminchoix = readLine("Action impossible.\nVeuillez réessayer...\n1) Modifier le PIN de la machine" + (machineId) + "\n2) Réapprovisionner le stock de la machine " + (machineId) + "\n3) Retirer un stock de la machine " + (machineId) + "\n > ").toInt
            }
          } while (indispo == 0)
          if (adminchoix == 1) {
            updatePin(machineId, fichier)
          } else if (adminchoix == 2) {
              println("Veuillez sélectionner quel ingrédient vous désirez réapprovisionner : ")
              println("Lait")
              println("Sucre")
              println("Café")
              var ingredient = readLine("> ")
              println("Combien de g/ml souhaitez vous ajouter ?")
              var quantite = readLine("> ").toInt
              fichier(machineId).addIngredient(ingredient, quantite)
          } else if(adminchoix == 3) {
              println("Veuillez sélectionner quel ingrédient du stock vous désirez retirer : ")
              println("Lait")
              println("Sucre")
              println("Café")
              var ingredient = readLine("> ")
              println("Combien de g/ml souhaitez vous retier ?")
              var quantite = readLine("> ").toInt
              fichier(machineId).removeIngredient(ingredient, quantite)
          }
        }


        //Si utilisateur choisi mode quitter
      } else if (mode == 3) {
        println("Sauvegarde de " + (fichier.length) + " machines dans machines.csv...")
        savecsv("machines.csv", fichier)
        println("Fichier sauvegardé avec succès.")
        machine = 1
      }
      if ((mode != 1) && (mode != 2) && (mode != 3)) {
        println("Erreur : Veuillez séléctionner parmi les 3 modes disponibles.")
      }
    }
  }
    //Messages de gestion des exceptions
    catch {
      case e: java.io.FileNotFoundException => println("Erreur : Fichier introuvable.\nVérifier le chemin d'accès et réessayez.")
      case e: java.nio.file.AccessDeniedException => println("Erreur : Echec de l'écriture dans machines.csv.\nLe fichier peut être verouillé ou en lecture seule.")
      case e: java.io.IOException => println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme...")
    }

  }
}