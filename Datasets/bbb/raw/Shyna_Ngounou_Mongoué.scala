import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    var Mode = 0
    // Définir le nombre de machines
    val nbmachines = 5
    var machineId = 0


    // Initialisation des tableaux pour les codes PIN et les stocks
    val machinePins = Array.fill(nbmachines)("434343")
    val coffeeStocks = Array.fill(nbmachines)(50)
    val sugarStocks = Array.fill(nbmachines)(30)
    val milkStocks = Array.fill(nbmachines)(500)

    // Choix du mode
    var continuer = true
    while (continuer) {
     Mode = 0




      while (Mode != 1 && Mode != 2 && Mode!= 3) {
        println("Nospresso Café \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        Mode= readInt()
        if (Mode != 1 && Mode != 2 && Mode != 3) {
          println("Veuillez choisir un nombre valide")
        }

        while(machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5){
          println ("Veuillez choisir une machine un nombre entre 1 et 5>")
          machineId = readInt()
          if (machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4 && machineId != 5) {
            println("Veuillez choisir un nombre valide")
          }
        }

        while (Mode==1){
          Mode = 0
          serviceClient(machineId,coffeeStocks, sugarStocks,milkStocks)

          }

    // Méthode pour valider le code PIN
    def validePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var tentative = 0
      val maxTentatives = 3
      while (tentative < maxTentatives) {
        println(" Machine sélectionnée (1-5) > " + machineId)
        println("Entrez le code PIN :")
        val PIN_entre = readLine

        if (PIN_entre == machinePins(machineId)) {
          println("Accès accordé à la machine " + machineId)
          return true
        } else {
          println("Code PIN incorrect." + (maxTentatives -1)+ "tentatives")
          tentative += 1

        }}
      if (tentative == maxTentatives) {
        println("Trop de tentatives échouées.Fin du programme")

      }
      false

    }
    // Méthode pour mettre à jour le code PIN
    def updatePin(machineId: Int, machinePins: Array[String]) : Unit ={
      println("Machine sélectionnée (1-5) >" + machineId)
      println("Entrez le code PIN :")
      val PIN_entre = readLine
      while(PIN_entre == machinePins(machineId)) {
        println ("Accès accordé.")
        println("Mise à jour du code PIN pour la machine" + machineId)
        println ("Entrez un nouveau code PIN à 6 chiffres >")
        val nouveauPin = readLine()

        // Vérification que le code PIN comporte 6 chiffres
        if (nouveauPin.length == 6 && nouveauPin.forall(_.isDigit)){
          machinePins(machineId) = nouveauPin
          println(" Le code PIN a été mis à jour avec succès. ")
          println ("Retour au menu principal ...")

        }
        else {
          println("Entrez un nouveau code PIN à 6 chiffres >")
        }
      }

    }
        def restockMachine(machineId : Int, coffeeStocks: Array [Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit ={

          println("Réapprovisionnement de la machine" +machineId)
          println("Niveaux de stocks actuels:")
          println("Poudre de café:" + coffeeStocks(machineId) + "g")
          println("Lait :" +  milkStocks(machineId)+ "mL")
          println("Sucre:" + sugarStocks(machineId) + "g")
          println("\n")
          println("Entrez les quantité à ajouter")
          println("Poudre de café : ")
          coffeeStocks(machineId) = coffeeStocks(machineId)+ readInt
          println("Lait:" )
          milkStocks(machineId)= milkStocks(machineId) + readInt
          println("Sucre:")
          sugarStocks(machineId)= sugarStocks(machineId) + readInt
          println("Les stocks ont été mis à jour avec succès.")
          println("Retour au menu principal...")

              }





    // Méthode pour service client
    def serviceClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]) : Boolean ={


      // Quantité pour Boisson + Suppléments
      var TypeBoisson = 0
      var Boisson_Selectionnee = ""
      var Niveau_Sucre = ""
      var Lait_Supp = ""
      var TailleLatte = 0
      var Sucre = 0
      var DoseSucre = 5.0
      var Quantite_Sucre_Add = 0.0
      var lait = 0
      var DoseLait = 0
      var Quantite_Lait_Cappuccino = 0.0
      var Quantite_Lait_Petit_Latte = 0.0
      var Quantite_Lait_Moyen_Latte = 0.0
      var Quantite_Lait_Grand_Latte = 0.0

      // Prix
      var PrixTotal = 0.0
      var PrixBaseExpresso = 2.00
      var PrixSucre = 0.10
      var PrixLait = 0.05
      var PrixBaseCappuccino = 2.50
      var PrixPetitLatte = 2.70
      var PrixMoyenLatte = 3.20
      var PrixGrandLatte = 3.70

      // quantité de Café utilisé
      val cafeCapuccino = 6
      val cafeExpresso = 8
      val cafePetitLatte = 6
      val cafeMoyenLatte = 8
      val cafeGrandLatte = 12



            TypeBoisson = 0


            while (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
              println(" Veuillez sélectionner votre boisson : \n 1) Expresso - CHF 2.00\n 2) Cappucino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit) , CHF 3.20 (Moyen) , CHF 3.70 (Grand) \n >")
              TypeBoisson = readInt()
              if (TypeBoisson != 1 && TypeBoisson != 2 && TypeBoisson != 3) {
                println("Veuillez choisir un nombre valide")
              }
            }
            // Gestion des boissons Latte
            if (TypeBoisson == 3) {
              TailleLatte = 0
              while (TailleLatte!= 1 && TailleLatte != 2 && TailleLatte!= 3  ) {
                println("Quelle taille de Latte voulez-vous ? \n 1) Petit \n 2) Moyen \n 3)Grand\n > ")
                TailleLatte = readInt()
                if (TailleLatte!= 1 && TailleLatte != 2 && TailleLatte != 3) {
                  println("Veuillez choisir un nombre valide")
                }
              }
            }
            Sucre = 0
            // Gestion du sucre
            while (Sucre!= 1 && Sucre != 2 && Sucre != 3 && Sucre != 4) {
              Sucre=0
              println(" Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30 \n >")
              Sucre = readInt()
              if (Sucre!= 1 && Sucre != 2 && Sucre != 3 && Sucre != 4) {
                println("Veuillez choisir un nombre valide")
              }
            }
            if (Sucre == 1) {
              Niveau_Sucre = "Sans Sucre"
            } else if (Sucre == 2) {
              Niveau_Sucre = "Peu (5g)"
            } else if (Sucre == 3) {
              Niveau_Sucre = " Moyen (10g)"
            } else if (Sucre == 4) {
              Niveau_Sucre = "Beaucoup (15g)"
            }

            // Gestion quantité sucre ajouté
            if (Sucre == 1) { // Si l'utilisateur chosit "Sans sucre", aucun sucre n'est ajouté . Quantité_Sucre_add = 0
              Quantite_Sucre_Add = 0

            }
            else {
              Quantite_Sucre_Add = (Sucre - 1) * DoseSucre

            }

      // Validation des stocks avant de préparer un expresso
      def suffisantExpresso(cafeExpresso: Int,Quantite_Sucre_Add: Double): Boolean = {
        if (coffeeStocks(machineId) < cafeExpresso) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (sugarStocks(machineId) < Quantite_Sucre_Add) {
          println("Erreur : Quantité de sucre insuffisante  pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        }
        return true
      }


            //Gestion des boissons Expresso
            if (TypeBoisson == 1) {


              Boisson_Selectionnee = "Expresso"
              if (suffisantExpresso(cafeExpresso,Quantite_Sucre_Add)) {
                coffeeStocks(machineId) -= 8
                sugarStocks(machineId) -= Quantite_Sucre_Add
                println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre)
                PrixSucre *= (Sucre - 1)
                PrixTotal = PrixBaseExpresso + PrixSucre
                printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseExpresso, PrixSucre, PrixTotal)
                // Interface paiement
                println("Veuillez payer en utilsant Twint.")
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                val charsSize = chars.length
                var pin = ""
                for (i <- 1 to 5) {
                  val twint = (Math.random() * charsSize).toInt
                  pin += chars(twint)
                }
                println("Votre code de paiement est :" + pin)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                println("Merci ! Votre paiement a été accepté.")

                // Préparation de la boisson
                println("Préparation de votre boisson ... \n [...] \n Votre" + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")
                PrixSucre = 0.10
                return true

              }

            }

            // Cas si boisson est un cappuccino ou un latte : Demande supp lait
            if (TypeBoisson == 2 || TypeBoisson == 3) {

              lait = 0
              DoseLait = 0

              while (lait!= 1 && lait != 2 ) {
                println(" Souhaitez-vous ajouter du lait en supplément ? \n ( Disponible uniquemnt pour Cappucino et Latte)\n 1) Oui \n 2) Non\n >")
                lait = readInt()
                if (lait!= 1 && lait != 2 ) {
                  println("Veuillez choisir un nombre valide")
                }
              }
            }
            if (lait == 1) {
              lait = 0
              Lait_Supp = "Oui"
              while (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {

                println("Combien de dose ? \n >")
                DoseLait = readInt()
                if (DoseLait != 1 && DoseLait != 2 && DoseLait != 3) {
                  println("Veuillez choisir une dose de lait supplémentaire valide")

                }
              }




              // Gestion quantité lait ajouté

              if (DoseLait == 1 && TypeBoisson == 2) {
                Quantite_Lait_Cappuccino = 150

              }
              else if (DoseLait == 2 && TypeBoisson == 2) {
                Quantite_Lait_Cappuccino = 200

              }
              else if (DoseLait == 3 && TypeBoisson == 2) {
                Quantite_Lait_Cappuccino = 250

              }
              else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 1) {
                Quantite_Lait_Petit_Latte = 170

              }
              else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 2) {
                Quantite_Lait_Moyen_Latte = 200

              }
              else if (DoseLait == 1 && TypeBoisson == 3 && TailleLatte == 3) {
                Quantite_Lait_Grand_Latte = 250

              }
              else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 1) {
                Quantite_Lait_Grand_Latte = 220

              }
              else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 2) {
                Quantite_Lait_Moyen_Latte = 250

              }
              else if (DoseLait == 2 && TypeBoisson == 3 && TailleLatte == 3) {
                Quantite_Lait_Grand_Latte = 300

              }
              else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 1) {
                Quantite_Lait_Grand_Latte = 270

              }
              else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 2) {
                Quantite_Lait_Moyen_Latte = 300

              }
              else if (DoseLait == 3 && TypeBoisson == 3 && TailleLatte == 3) {
                Quantite_Lait_Grand_Latte = 350

              }
            }

            if (lait == 2) {
              Lait_Supp = "Non"
              if (TypeBoisson == 2) {
                Quantite_Lait_Cappuccino = 100

              }
              else if (TypeBoisson == 3 && TailleLatte == 1) {
                Quantite_Lait_Petit_Latte = 120

              }
              else if (TypeBoisson == 3 && TailleLatte == 2) {
                Quantite_Lait_Moyen_Latte = 150

              }
              else if (TypeBoisson == 3 && TailleLatte == 3) {
                Quantite_Lait_Grand_Latte = 200

              }

            }
      // Validation des stocks avant de préparer d'un capuccino
      def suffisantCappucino(cafeCappucino: Int,Quantite_Sucre_Add: Double, Quantite_Lait_Cappuccino: Double): Boolean = {
        if (coffeeStocks(machineId) < cafeCappucino) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (sugarStocks(machineId) < Quantite_Sucre_Add) {
          println("Erreur : Quantité de sucre insuffisante  pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (milkStocks(machineId) < Quantite_Lait_Cappuccino) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        }
        return true
      }

      // Validation des stocks avant de préparer d'un petit Latte
      def suffisantPetit_Latte(cafePetitLatte: Int,Quantite_Sucre_Add: Double, Quantite_Lait_Petit_Latte: Double): Boolean = {
        if (coffeeStocks(machineId) < cafePetitLatte) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (sugarStocks(machineId) < Quantite_Sucre_Add) {
          println("Erreur : Quantité de sucre insuffisante  pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (milkStocks(machineId) < Quantite_Lait_Petit_Latte) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        }
        return true
      }
     // Validation des stocks avant de préparer d'un moyen Latte
      def suffisantMoyen_Latte(cafeMoyenLatte: Int,Quantite_Sucre_Add: Double, Quantite_Lait_Moyen_Latte: Double): Boolean = {
        if (coffeeStocks(machineId) < cafeMoyenLatte) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (sugarStocks(machineId) < Quantite_Sucre_Add) {
          println("Erreur : Quantité de sucre insuffisante  pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (milkStocks(machineId) < Quantite_Lait_Moyen_Latte) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        }
        return true
      }


      // Validation des stocks avant de préparer d'un grand Latte
      def suffisantGrand_Latte(cafeGarndLatte: Int,Quantite_Sucre_Add: Double, Quantite_Lait_Garnd_Latte: Double): Boolean = {
        if (coffeeStocks(machineId) < cafeGrandLatte) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (sugarStocks(machineId) < Quantite_Sucre_Add) {
          println("Erreur : Quantité de sucre insuffisante  pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        } else if (milkStocks(machineId) < Quantite_Lait_Grand_Latte) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n Veuillez choisir une autre machine.")
          return false
        }
        return true
      }

            // Gestion des boissons Cappuccino
            if (TypeBoisson == 2) {

              Boisson_Selectionnee = "Cappuccino"
              if (suffisantCappucino(cafeCapuccino,Quantite_Sucre_Add,Quantite_Lait_Cappuccino)) {
                coffeeStocks(machineId)-= 6
                milkStocks(machineId)-= Quantite_Lait_Cappuccino
                sugarStocks(machineId)-= Quantite_Sucre_Add
                println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                PrixSucre *= (Sucre - 1)
                PrixLait *= DoseLait
                PrixTotal = PrixBaseCappuccino + PrixSucre + PrixLait
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixBaseCappuccino, PrixSucre, PrixLait, PrixTotal)
                // Interface paiement
                println("Veuillez payer en utilsant Twint.")
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                val charsSize = chars.length
                var pin = ""
                for (i <- 1 to 5) {
                  val twint = (Math.random() * charsSize).toInt
                  pin += chars(twint)
                }
                println("Votre code de paiement est :" + pin)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                println("Merci ! Votre paiement a été accepté.")

                // Préparation de la boisson
                println("Préparation de votre boisson ... \n [...] \n Votre" + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

                PrixSucre = 0.10
                PrixLait = 0.05
                return true
              }



            }


            // Gestion commande petit Latte
            if (TailleLatte == 1 && TypeBoisson == 3) {

              Boisson_Selectionnee = "Latte (petit)"
              if (suffisantPetit_Latte(cafePetitLatte, Quantite_Sucre_Add, Quantite_Lait_Petit_Latte)) {
                TypeBoisson = 0
                coffeeStocks(machineId)-= 6
                milkStocks(machineId) -= Quantite_Lait_Petit_Latte
                sugarStocks(machineId) -= Quantite_Sucre_Add
                println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                PrixSucre *= (Sucre - 1)
                PrixLait *= DoseLait
                PrixTotal = PrixPetitLatte + PrixSucre + PrixLait
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixPetitLatte, PrixSucre, PrixLait, PrixTotal)
                // Interface paiement
                println("Veuillez payer en utilsant Twint.")
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                val charsSize = chars.length
                var pin = ""
                for (i <- 1 to 5) {
                  val twint = (Math.random() * charsSize).toInt
                  pin += chars(twint)
                }
                println("Votre code de paiement est :" + pin)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                println("Merci ! Votre paiement a été accepté.")

                // Préparation de la boisson
                println("Préparation de votre boisson ... \n [...] \n Votre" + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

                PrixSucre = 0.10
                PrixLait = 0.05
                return true
              }


            }

            // Gestion commande moyen Latte
            if (TailleLatte == 2 && TypeBoisson == 3) {

              Boisson_Selectionnee = "Latte (moyen)"
              if (suffisantMoyen_Latte (cafeMoyenLatte,Quantite_Sucre_Add,Quantite_Lait_Moyen_Latte)){
                TypeBoisson = 0
                coffeeStocks(machineId)-= 8
                milkStocks(machineId)-= Quantite_Lait_Moyen_Latte
                sugarStocks(machineId) -= Quantite_Sucre_Add
                println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                PrixSucre *= (Sucre - 1)
                PrixLait *= DoseLait
                PrixTotal = PrixMoyenLatte + PrixSucre + PrixLait
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixMoyenLatte, PrixSucre, PrixLait, PrixTotal)
                // Interface paiement
                println("Veuillez payer en utilsant Twint.")
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                val charsSize = chars.length
                var pin = ""
                for (i <- 1 to 5) {
                  val twint = (Math.random() * charsSize).toInt
                  pin += chars(twint)
                }
                println("Votre code de paiement est :" + pin)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                println("Merci ! Votre paiement a été accepté.")

                // Préparation de la boisson
                println("Préparation de votre boisson ... \n [...] \n Votre" + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

                PrixSucre = 0.10
                PrixLait = 0.05
                return true
              }


            }

            // Gestion commande Grand Latte
            if (TailleLatte == 3 && TypeBoisson == 3) {
              Boisson_Selectionnee = "Latte (grand)"

              if (suffisantGrand_Latte (cafeGrandLatte, Quantite_Sucre_Add, Quantite_Lait_Grand_Latte)){
                TypeBoisson = 0
                coffeeStocks(machineId) -= 12
                milkStocks(machineId) -= Quantite_Lait_Grand_Latte
                sugarStocks(machineId) -= Quantite_Sucre_Add
                println("Boisson sélectionnée :" + Boisson_Selectionnee + "\n Niveau de sucre:" + Niveau_Sucre + "\n Lait supplémentaire :" + Lait_Supp)
                PrixSucre *= (Sucre - 1)
                PrixLait *= DoseLait
                PrixTotal = PrixGrandLatte + PrixSucre + PrixLait
                printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n", PrixGrandLatte, PrixSucre, PrixLait, PrixTotal)
                // Interface paiement
                println("Veuillez payer en utilsant Twint.")
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWZ0123456789"
                val charsSize = chars.length
                var pin = ""
                for (i <- 1 to 5) {
                  val twint = (Math.random() * charsSize).toInt
                  pin += chars(twint)
                }
                println("Votre code de paiement est :" + pin)
                println("(En attente de validation du paiement...)")
                Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes) avant de valider le paiement
                println("Merci ! Votre paiement a été accepté.")

                // Préparation de la boisson
                println("Préparation de votre boisson ... \n [...] \n Votre" + Boisson_Selectionnee + " est prêt ! Bonne dégustation ! ")

                PrixSucre = 0.10
                PrixLait = 0.05
                return true
              }




          }



    var mode = 0



        // Mode Admin
        while (mode == 2) {
          mode = 0
          validePin(machineId,machinePins)
          println("Mode Admin")
          val PIN_Defaut = "434343"
          var Statut_acces = false
          println("Entrer le code PIN : ******")
          val PIN_entre = readLine()
          if (PIN_entre == PIN_Defaut) {
            Statut_acces = true
            println("Accès autorisé.")
            println("\n")

            restockMachine(machineId,coffeeStocks,sugarStocks,milkStocks)
            updatePin(machineId, machinePins)
          }

          mode = 0
        }

        // Mode Quitter
        if (mode == 3) {
          continuer = false
        }

      }

    }













}
}}