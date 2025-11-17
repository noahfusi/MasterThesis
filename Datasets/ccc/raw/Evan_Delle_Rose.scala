import scala.io.StdIn._
import scala.io.Source
import java.io.{FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ArrayBuffer

object Main {
  //déclaration des méthodes

  def findmachineId(machines:ArrayBuffer[Machine]): Int = {
    val nbmachine = machines.length
    println("Sélectionnez la machine (1-" + nbmachine + ") ")
    var machineId = readInt()
    while ((machineId < 1) || (machineId > nbmachine)) {
      println("Entrée invalide, veuillez entrer une valeur entre 1 et " + nbmachine)
      machineId = readInt()
    }
    machineId - 1
  }

  def validatePin(machines:ArrayBuffer[Machine],machineId:Int): Boolean = {
    var essais = 3
    while ((essais <= 3) && (essais > 0)) {
      println("Entrez le code PIN : ")
      var PINentre = readLine().toString
      if (PINentre == machines(machineId).pincode) {
        println("Accès autorisé à la machine " + (machineId + 1))
        return true
      }
      //essais > 1 sinon ça fait 4 tentatives
      else {
        essais -= 1
        println("PIN incorrect " + essais + " tentatives restantes")
      }
    }
    if (essais == 0) {
      println("Trop de tentatives échouées. Fin du programme")
    }
    return false
  }

  //modification du code PIN
  def updatePin(machines:ArrayBuffer[Machine],machineId:Int): Unit = {

    println("Mise à jour du code PIN de la machine " + (machineId + 1))
    var nouveauPIN = readLine("Entrez le nouveau code PIN à 6 chiffres: ").toString

    while ((nouveauPIN.length != 6) || (!nouveauPIN.forall(_.isDigit))) {
      println("Le nouveau PIN doit contenir exactement 6 chiffres et uniquement des chiffres")
      nouveauPIN = readLine("> ").toString
    }
    println("Le code PIN a été mis à jour avec succès.")
    machines(machineId).pincode = nouveauPIN.toString
    println(machines(machineId).pincode.mkString(" , "))
  }

  def restockMachine(machines:ArrayBuffer[Machine],machineId:Int): Unit = {
    println("Niveaux de stocks actuels: ")
    println("Poudre de café:    " + machines(machineId).coffee + "g")
    println("Sucre:    " + machines(machineId).sugar + "g")
    println("Lait:    " + (machines(machineId).milk.toDouble / 1000) + "L")

    println("Entrez les quantités à ajouter: ")

    println("Poudre de café : ")
    var ajoutpoudreacafe = readInt()
    while (ajoutpoudreacafe < 0) {
      println("Veuillez entrer une valeur positive ")
      ajoutpoudreacafe = readInt()
    }
    machines(machineId).addIngredient("coffee",ajoutpoudreacafe)


    println("Sucre : ")
    var ajoutsucre = readInt()
    while (ajoutsucre < 0) {
      println("Veuillez entrer une valeur positive ")
      ajoutsucre = readInt()
    }
    machines(machineId).addIngredient("sugar",ajoutsucre)

    println("Lait : ")
    var ajoutlait = readDouble()
    while (ajoutlait < 0) {
      println("Veuillez entrer une valeur positive ")
      ajoutlait = readDouble()
    }
    var ajoutlaitenml = (ajoutlait * 1000).toInt
    machines(machineId).addIngredient("milk",ajoutlaitenml)

    println("Les Stocks ont été mis à jour avec succès.")

  }


  def serveClient(machines : ArrayBuffer[Machine],machineId: Int): Boolean = {
    var choixducafe = 0
    var choixtaillelatte = 0
    var transactionterminee = false
    var choixdusucre = 0
    var choixouiounonlait = 0
    var choixnbdosedelait = 0
    var prixcafe = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var prixtotal = 0.0
    var retouraumenu = false
    var cafechoisi = "Expresso".toString
    var sucrechoisi = "Peu (5g)"
    var doselaitchoisie = " 50ml"
    var laitensupplement = " Oui "
    //code TWINT
    val caractères = "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789" //chiffres à double pour avoir moins de chances de tomber sur un code composé uniquement de lettres
    var codeTWINT = " "
    // fin code Twint





    while (transactionterminee == false) {
      retouraumenu = false

      while ((retouraumenu == false) && (transactionterminee == false)) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Capuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        choixducafe = readInt()
        //validation de l'entrée
        while ((choixducafe != 1) && (choixducafe != 2) && (choixducafe != 3)) {
          println("Entrée non-valide")
          println("Veuillez sélectionner votre café")
          choixducafe = readInt()
        }

        // expresso
        if (choixducafe == 1) {


          prixcafe = 2.00
          cafechoisi = "Expresso".toString
          println("Voulez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          choixdusucre = readInt()

          if (choixdusucre == 1) {
            sucrechoisi = " sans sucre "
          }
          if (choixdusucre == 2) {
            sucrechoisi = " Peu (5g)"
          }
          if (choixdusucre == 3) {
            sucrechoisi = " Moyen (10g)"
          }
          if (choixdusucre == 4) {
            sucrechoisi = " Beaucoup (15g)"
          }


          while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
            println("Entrée non-valide")
            println("Veuillez sélectionner le niveau de sucre")
            choixdusucre = readInt()
          }
          if (machines(machineId).coffee < 8) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Erreur : stock de poudre à café insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if ((choixdusucre == 2) && (machines(machineId).sugar < 5)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if ((choixdusucre == 3) && (machines(machineId).sugar < 10)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 4) && (machines(machineId).sugar < 15)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if (choixdusucre == 1) {
            prixsucre = 0.00
          }
          if (choixdusucre == 2) {
            prixsucre = 0.10
          }
          if (choixdusucre == 3) {
            prixsucre = 0.20
          }
          if (choixdusucre == 4) {
            prixsucre = 0.30
          }

          // diminution des stocks de sucre
          {
            if (choixdusucre == 2) {
              machines(machineId).removeIngredient("sugar",5)
            }
            if (choixdusucre == 3) {
              machines(machineId).removeIngredient("sugar",10)
            }
            if (choixdusucre == 4) {
              machines(machineId).removeIngredient("sugar",15)
            }
          }

          //fin de la sélection + paiement
          if (retouraumenu == false) {
            transactionterminee = true
            if (choixducafe == 1) {
              machines(machineId).removeIngredient("coffee",8)

            }

            var codeTWINT = ""
            for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génere un nouveau code pour ne pas avoir toujours le même code twint

            prixtotal = prixcafe + prixsucre
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            printf("Prix total : CHF %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre, prixtotal)
            println(" ")
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTWINT)
            println("(En attente de paiement...)")
            println(" ")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Paiement confirmé")
            println("préparation de votre boisson...")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Votre Expresso est prêt ! Bonne dégustation !")
            println(" ")
            println(" ")
            transactionterminee = true
          }
        } // fin expresso 1

        //Capucino 2
        if (choixducafe == 2) {
          cafechoisi = "Capucino".toString
          println("Voulez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")

          choixdusucre = readInt()
          while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
            println("Entrée non-valide")
            println("Veuillez sélectionner le niveau de sucre")
            choixdusucre = readInt()
          }
          if (choixdusucre == 1) {
            sucrechoisi = " Sans sucre"
          }
          if (choixdusucre == 2) {
            sucrechoisi = " Peu (5g)"
          }
          if (choixdusucre == 3) {
            sucrechoisi = " Moyen (10g)"
          }
          if (choixdusucre == 4) {
            sucrechoisi = " Beaucoup (15g)"
          }
          // Choix du lait
          println("Souhaitez-vous ajouter du lait en supplément?")
          println("1) Oui")
          println("2) Non")
          choixouiounonlait = readInt()

          while ((choixouiounonlait != 1) && (choixouiounonlait != 2)) {
            println("Entrée non-valide")
            println("Veuillez entrer 1 ou 2")
            choixouiounonlait = readInt()
          }

          if (choixouiounonlait == 1) {
            println("Combien de doses? (1 dose = 50 ml) 3 doses maximum")
            println("1) 1 dose (50 ml) - CHF 0.05")
            println("2) 2 doses (100 ml) - CHF 0.10")
            println("3) 3 doses (150 ml) - CHF 0.15")
            choixnbdosedelait = readInt()
            while ((choixnbdosedelait != 1) && (choixnbdosedelait != 2) && (choixnbdosedelait != 3)) {
              println("Entrée non-valide. Combien de doses de lait?")
              choixnbdosedelait = readInt()
            }
          }
          if (choixouiounonlait == 2) {
            laitensupplement = " Non"
          }
          // gestion des erreurs de stock
          if ((machines(machineId).coffee < 6) && (machines(machineId).milk < 100)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println(" ")
            println("Erreur : stock insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          // gestion stock lait avant supplément pour voir si assez lait en stock
          machines(machineId).removeIngredient("milk",100)

          if ((choixdusucre == 2) && (machines(machineId).sugar < 5)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println(" ")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 3) && (machines(machineId).sugar < 10)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 4) && (machines(machineId).sugar < 15)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }


          if ((choixnbdosedelait == 1) && (machines(machineId).milk < 50)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            machines(machineId).milk += 100 //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait

            retouraumenu = true
            return false
          }
          if ((choixnbdosedelait == 2) && (machines(machineId).milk < 100)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            machines(machineId).milk += 100 //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait

            retouraumenu = true
            return false
          }
          if ((choixnbdosedelait == 3) && (machines(machineId).milk < 150)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            machines(machineId).milk += 100 //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait

            retouraumenu = true
            return false
          }
          if (retouraumenu == false) {
            transactionterminee = true
            if (choixducafe == 2) {
              cafechoisi = "Capucino".toString
              machines(machineId).removeIngredient("coffee",6)

            }
            if (choixdusucre == 2) {
              sucrechoisi = " Peu (5g)"
              machines(machineId).removeIngredient("sugar",5)
              prixsucre = 0.10
            }
            if (choixdusucre == 3) {
              sucrechoisi = " Moyen (10g)"
              machines(machineId).removeIngredient("sugar",10)
              prixsucre = 0.20
            }
            if (choixdusucre == 4) {
              sucrechoisi = " Beaucoup (15g)"
              machines(machineId).removeIngredient("sugar",15)
              prixsucre = 0.30
            }


            if (choixnbdosedelait == 1) {
              machines(machineId).removeIngredient("milk",50)
              prixlait = 0.05
            }
            if (choixnbdosedelait == 2) {
              machines(machineId).removeIngredient("milk",100)
              prixlait = 0.10
            }
            if (choixnbdosedelait == 3) {
              machines(machineId).removeIngredient("milk",150)
              prixlait = 0.15
            }
            if (choixouiounonlait == 1) {
              laitensupplement = " Oui "
            }
            if (choixouiounonlait == 2) {
              laitensupplement = " Non "
            }
            if (choixducafe == 2) {
              prixcafe = 2.50
            }
            var codeTWINT = ""
            for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génère un nouveau code pour ne pas avoir toujours le même code twint
            prixtotal = prixcafe + prixsucre + prixlait
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre, prixlait, prixtotal)
            println(" ")
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTWINT)
            println("(En attente de paiement...)")
            println(" ")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Paiement confirmé")
            println("préparation de votre boisson...")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Votre Capucino est prêt ! Bonne dégustation !")
            println(" ")
            println(" ")
          }

        } //fin café 2

        //Latte
        if (choixducafe == 3) {
          println("Veuillez choisir la taille de votre Latte : ")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3-70")
          choixtaillelatte = readInt()
          while ((choixtaillelatte != 1) && (choixtaillelatte != 2) && (choixtaillelatte != 3)) {
            println("Entrée invalide, veuillez entrer une valeur valide")
            choixtaillelatte = readInt()
          }
          if (choixtaillelatte == 1) {
            cafechoisi = " Latte (Petit)"
          }
          if (choixtaillelatte == 2) {
            cafechoisi = " Latte (Moyen)"
          }
          if (choixtaillelatte == 3) {
            cafechoisi = " Latte (Grand)"
          }
          //sucre

          println("Voulez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")

          choixdusucre = readInt()
          while ((choixdusucre != 1) && (choixdusucre != 2) && (choixdusucre != 3) && (choixdusucre != 4)) {
            println("Entrée non-valide")
            println("Veuillez sélectionner le niveau de sucre")
            choixdusucre = readInt()
          }
          if (choixdusucre == 1) {
            sucrechoisi = " Sans sucre"
          }
          if (choixdusucre == 2) {
            sucrechoisi = " Peu (5g)"
          }
          if (choixdusucre == 3) {
            sucrechoisi = " Moyen (10g)"
          }
          if (choixdusucre == 4) {
            sucrechoisi = " Beaucoup (15g)"
          }

          // Choix du lait
          println("Souhaitez-vous ajouter du lait en supplément?")
          println("1) Oui")
          println("2) Non")
          choixouiounonlait = readInt()

          while ((choixouiounonlait != 1) && (choixouiounonlait != 2)) {
            println("Entrée non-valide")
            println("Veuillez entrer 1 ou 2")
            choixouiounonlait = readInt()
          }
          if (choixouiounonlait == 2) {
            laitensupplement = " Non"
          }
          if (choixouiounonlait == 1) {
            println("Combien de doses? (1 dose = 50 ml) 3 doses maximum")
            println("1) 1 dose (50 ml) - CHF 0.05")
            println("2) 2 doses (100 ml) - CHF 0.10")
            println("3) 3 doses (150 ml) - CHF 0.15")
            choixnbdosedelait = readInt()
            while ((choixnbdosedelait != 1) && (choixnbdosedelait != 2) && (choixnbdosedelait != 3)) {
              println("Entrée non-valide. Combien de doses de lait?")
              choixnbdosedelait = readInt()
            }
          }
          if (choixouiounonlait == 2) {
            laitensupplement = " Non"
          }
          //gestion des insuffisances de stock


          if ((choixtaillelatte == 1) && (machines(machineId).coffee < 6)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de poudre de café insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixtaillelatte == 1) && (machines(machineId).milk < 120)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if ((choixtaillelatte == 2) && (machines(machineId).coffee < 8)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de poudre de café insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixtaillelatte == 2) && (machines(machineId).milk < 150)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if ((choixtaillelatte == 3) && (machines(machineId).coffee < 12)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de poudre de café insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixtaillelatte == 3) && (machines(machineId).milk < 200)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          // gestion stock lait avant supplément pour voir si assez lait en stock
          if (choixtaillelatte == 1) {
            machines(machineId).removeIngredient("milk",120)
          }
          if (choixtaillelatte == 2) {
            machines(machineId).removeIngredient("milk",150)
          }
          if (choixtaillelatte == 3) {
            machines(machineId).removeIngredient("milk",200)
          }

          if ((choixnbdosedelait == 1) && (machines(machineId).milk < 50)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            if (choixtaillelatte == 1) {
              machines(machineId).milk += 120
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 2) {
              machines(machineId).milk += 150
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 3) {
              machines(machineId).milk += 200
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            retouraumenu = true
            return false
          }

          if ((choixnbdosedelait == 2) && (machines(machineId).milk < 100)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            if (choixtaillelatte == 1) {
              machines(machineId).milk += 120
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 2) {
              machines(machineId).milk += 150
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 3) {
              machines(machineId).milk += 200
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            retouraumenu = true
            return false
          }
          if ((choixnbdosedelait == 3) && (machines(machineId).milk) < 150) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de lait insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            if (choixtaillelatte == 1) {
              machines(machineId).milk += 120
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 2) {
              machines(machineId).milk += 150
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            if (choixtaillelatte == 3) {
              machines(machineId).milk += 200
            } //rajout du stock de lait enlevé plus haut car boisson non préparable avec suppléement lait
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 2) && (machines(machineId).sugar < 5)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 3) && (machines(machineId).sugar < 10)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }
          if ((choixdusucre == 4) && (machines(machineId).sugar < 15)) {
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            println("")
            println("Erreur : stock de sucre insuffisant. ")
            println("Veuillez sélectionner une autre machine.")
            retouraumenu = true
            return false
          }

          if (retouraumenu == false) {
            transactionterminee = true
            if ((choixducafe == 3) && (choixtaillelatte == 1)) {
              cafechoisi = "Latte (Petit)".toString
              machines(machineId).removeIngredient("coffee",6)

            }
            if ((choixducafe == 3) && (choixtaillelatte == 2)) {
              cafechoisi = "Latte (Moyen)".toString
              machines(machineId).removeIngredient("coffee",8)

            }
            if ((choixducafe == 3) && (choixtaillelatte == 3)) {
              cafechoisi = "Latte (Grand)".toString
              machines(machineId).removeIngredient("coffee",12)

            }


            if (choixdusucre == 2) {
              sucrechoisi = " Peu (5g)"
              machines(machineId).removeIngredient("sugar",5)
              prixsucre = 0.10
            }
            if (choixdusucre == 3) {
              sucrechoisi = " Moyen (10g)"
              machines(machineId).removeIngredient("sugar",10)
              prixsucre = 0.20
            }
            if (choixdusucre == 4) {
              sucrechoisi = " Beaucoup (15g)"
              machines(machineId).removeIngredient("sugar",15)
              prixsucre = 0.30
            }


            if (choixnbdosedelait == 1) {
              machines(machineId).removeIngredient("milk",50)
              prixlait = 0.05
            }
            if (choixnbdosedelait == 2) {
              machines(machineId).removeIngredient("milk",100)
              prixlait = 0.10
            }
            if (choixnbdosedelait == 3) {
              machines(machineId).removeIngredient("milk",150)
              prixlait = 0.15
            }
            if (choixouiounonlait == 1) {
              laitensupplement = " Oui "
            }
            if (choixouiounonlait == 2) {
              laitensupplement = " Non "
            }
            if ((choixducafe == 3) && (choixtaillelatte == 1)) {
              prixcafe = 2.70
            }
            if ((choixducafe == 3) && (choixtaillelatte == 2)) {
              prixcafe = 3.20
            }
            if ((choixducafe == 3) && (choixtaillelatte == 3)) {
              prixcafe = 3.70
            }
            var codeTWINT = ""
            for (_ <- 1 to 5) codeTWINT += caractères((math.random * caractères.length).toInt) //génere un nouveau code pour ne pas avoir toujours le même code twint
            prixtotal = prixcafe + prixsucre + prixlait
            println("Boisson sélectionnée : " + cafechoisi)
            println("Niveau de sucre : " + sucrechoisi)
            println("Lait en supplément : " + laitensupplement)
            printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f  = CHF %.2f \n", prixcafe, prixsucre, prixlait, prixtotal)
            println(" ")

            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : " + codeTWINT)
            println("(En attente de paiement...)")
            println(" ")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Paiement confirmé")
            println("préparation de votre boisson...")
            Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
            println("Votre Latte est prêt ! Bonne dégustation !")
            println(" ")
            println(" ")
          }
        } //fin café 3


      } //accolade de fin de while transaction terminee false et quitter false
    }


    return true


  } //fin servclient()


  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)

    //déclaration des variables
    //affichage des stocks des machines

    for (a <- machines) {
      println(s"Machine " + (a.id) + " chargée :" )
      println("ID: " + (a.id) )
      println("Code PIN: " + (a.pincode) )
      println("Lait: " + (a.milk) )
      println("Sucre: " + (a.sugar) )
      println("Café: " + a.coffee)
    }


    var transactionterminee = false
    var retouraumenu = false
    var choixdumode = 0
    var quitter = false


    while (quitter == false) {
      println("        Nospresso café")
      println(" ") // juste pour faire joli
      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      choixdumode = readInt()

      while ((choixdumode != 1) && (choixdumode != 2) && (choixdumode != 3)) {
        println("Entrée non-valide")
        println("Veuillez sélectionner votre mode")
        choixdumode = readInt()
      }
      if (choixdumode == 3) {
        println(" Machines éteintes")
        quitter = true
      }


      if (quitter == false) {
        val machineId= findmachineId(machines).toInt
        if (choixdumode == 1) {
          serveClient (machines,machineId)
        }

        if (choixdumode == 2) {
          if (validatePin(machines,machineId) == true) {
            println(" Veuillez sélectionner l'action à réaliser: ")
            println("1) Modifier le code PIN")
            println("2) Réapprovisionnement des stocks")
            var choixdelaction = readInt()
            while ((choixdelaction != 1) && (choixdelaction != 2)) {
              println("Entrée invalide. Veuillez entrer 1 ou 2")
              choixdelaction = readInt()
            }
            if (choixdelaction == 1) {
              updatePin(machines, machineId)
            }
            if (choixdelaction == 2) {
              restockMachine(machines, machineId)
            }
          }
          else {
            quitter = true
          }
        } // accolade mode admin


      } // if quitter == false
    } // while quitter false
    savecsv(filename, machines)
  }


  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    val machines = ArrayBuffer[Machine]()

    try {

      val File = Source.fromFile(filename)
      val Line = File.getLines().drop(1)

      var idmachine = 1

      for (nbligne <- Line) {
        val valeur = nbligne.split(",")
        if (valeur.length == 4) {
          val id = idmachine

          val pincode = valeur(0).toString
          val milk = valeur(1).toInt
          val sugar = valeur(2).toInt
          val coffee = valeur(3).toInt

          machines += new Machine(id, pincode, milk, sugar, coffee)

          idmachine = idmachine + 1
        }
      }
    } catch {

      case _: FileNotFoundException =>

        println("Erreur : Fichier introuvable. Vérifiez le chemin d’acc`es et réessayez.")
        println("---")
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.: ")

        sys.exit(1)
    }
    machines
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    try {
      val newcsv = new PrintWriter((filename))

      newcsv.println("PINCODE,MILK,SUGAR,COFFEE")

      for (i <- machines) {

        val newline = i.pincode + "," + i.milk + "," + i.sugar + "," + i.coffee
        newcsv.println(newline)
      }
      newcsv.close

      println("Sauvegarde des machines dans machines.csv...")
      println("Fichier sauvegardé avec succès.")

    } catch {

      case _: IOException =>
        println("Erreur : Echec de l’écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")

    }

  }
}
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    //vérifier les stocks
    if (amount<=0){return false}
    //
    if (ingredient=="milk") {
      if (milk>=amount) {milk -=amount
        true
      } else {
        println("Stock insuffisant de lait.")
        false
      }
    } else if (ingredient=="sugar") {
      if (sugar>=amount) {sugar -=amount
        true
      } else {
        println("Stock insuffisant de sucre.")
        false
      }
    } else if (ingredient=="coffee") {
      if (coffee>=amount) {coffee -=amount
        true
      } else {
        println("Stock insuffisant de café.")
        false
      }
    } else {
      false
    }
  }

  def addIngredient(ingredient: String, amount: Int): Unit = {

    if (ingredient=="milk") {milk +=amount

    } else if (ingredient=="sugar") {sugar +=amount

    } else if (ingredient=="coffee") {coffee +=amount
    }

  }


}